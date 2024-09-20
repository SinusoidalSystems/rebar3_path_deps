-module(rebar_path_resource).

-export([init/2,
         lock/2,
         download/4, download/3,
         needs_update/2,
         make_vsn/1]).

-include_lib("kernel/include/file.hrl").

init(Type, _State) ->
   Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
   {ok, Resource}.

lock(Dir, Source) when is_tuple(Source) ->
  lock_(Dir, Source);

lock(AppInfo, _) ->
  lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

lock_(Dir, {path, Path, _}) ->
  lock_(Dir, {path, Path});

lock_(_Dir, {path, Path}) ->
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  Checksum = dep_checksum(Source),
  {path, Path, {md5, Checksum}}.

download(TmpDir, {path, Path, _}, State) ->
  download(TmpDir, {path, Path}, State);
download(TmpDir, {path, Path}, State) ->
  case download_(TmpDir, {path, Path}, State) of
    ok -> {ok, State};
    Error -> Error
  end.

download(TmpDir, AppInfo, State, _) ->
  case rebar_app_info:source(AppInfo) of
    {path, Path} ->
      download_(TmpDir, {path, Path}, State);
    {path, Path, _} ->
      download_(TmpDir, {path, Path}, State)
  end.

download_(Dir, {path, Path}, _State) ->
  ok = filelib:ensure_dir(Dir),
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),

  Files = files(Source),

  lists:foreach(fun(File) -> copy(File, Source, Dir) end, Files),
  rebar_api:debug("copied source from=~p, to=~p ~n", [Path, Dir]),
  ok.

make_vsn(_Dir) ->
  {error, "Replacing version of type `path` not supported."}.

%% Let's only target rebar3 version > 3.7.0 when the resource API was defined.
needs_update(AppInfo, _CustomState) ->
  needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

needs_update_(_Dir, {path, Path, {md5, OldChecksum}}) ->
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  Checksum = dep_checksum(Source),
  %% rebar_api:info("Checksum ~p =?= ~200p", [Checksum, OldChecksum]),
  OldChecksum /= Checksum;
needs_update_(_Dir, Path) ->
  rebar_api:debug("Unexpected path format: ~p - expected {path, <Path>, {md5, <Checksum>}}", Path),
  true.

copy(File, Source, Target) ->
  SourceFile = filename:join(Source, File),
  TargetFile = filename:join(Target, File),
  ok = filelib:ensure_dir(TargetFile),
  {ok, _} = file:copy(SourceFile, TargetFile).

dep_checksum(Source) ->
  MD5s = [ md5_file(filename:join(Source, File)) || File <- files(Source) ],
  Checksum = erlang:md5(<< MD5 || <<MD5/binary>> <- MD5s>>),
  binary:encode_hex(Checksum, lowercase).

md5_file(File) ->
  case file:read_file(File) of
    {ok, Bin} -> erlang:md5(Bin);
    _         -> <<>>
  end.

-define(EXCLUDE_PATHS, [".git", "_build", "examples", ".eqc_info"]).
-define(EXCLUDE_FILES, ["^\\.", "~$", "current_counterexample.eqc"]).

%% Return the list of files - either for copy, or update check/checksum
files(RootDir) ->
  files(RootDir, ?EXCLUDE_PATHS, ?EXCLUDE_FILES, ".").

files(Root, IgnorePaths, IgnoreFiles, Path) ->
  File = filename:join(Root, Path),
  case filelib:is_dir(File) of
    true  ->
      case file:list_dir(File) of
        {ok, List} ->
          Dirs = [filename:join(Path, X) || X <- List, not lists:member(X, IgnorePaths)],
          lists:flatmap(fun(X) -> files(Root, IgnorePaths, IgnoreFiles, X) end, Dirs);
        {error, _Reason} ->
          []
      end;
    false ->
      [ Path || not is_excluded(filename:basename(File), IgnoreFiles) ]
  end.

is_excluded(Path, Excludes) ->
  lists:foldl(fun(_, true)   -> true;
                 (RE, false) -> re:run(Path, RE, [unicode]) =/= nomatch
              end, false, Excludes).
