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
  {path, Path, {mtime, to_iso8601(last_modified(Source))}}.

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
  rebar_api:info("Download files: ~p" , [Files]),
  lists:foreach(fun(File) -> copy(File, Source, Dir) end, Files),
  rebar_log:log(debug, "copied source from=~p, to=~p ~n", [Path, Dir]),
  LastModified = last_modified(Source, Files),
  {ok, A} = file:read_file_info(Dir),
  file:write_file_info(Path, A#file_info{mtime = LastModified, atime = LastModified}).

make_vsn(_Dir) ->
  {error, "Replacing version of type path not supported."}.

%% Let's only target rebar3 version > 3.7.0 when the resource API was defined.
needs_update(AppInfo, _CustomState) ->
  needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

needs_update_(Dir, {path, Path, _}) ->
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  LastModified = last_modified(Source),
  Old = filelib:last_modified(Dir),
  rebar_log:log(debug, "compare dir=~p, path=~p last modified=~p, old=~p~n", [Dir, Path, LastModified, Old]),
  (Old < LastModified).

last_modified(Source) ->
  Files = files(Source),
  last_modified(Source, Files).

last_modified(_, []) -> calendar:local_time();
last_modified(Root, Files) ->
  lists:max([ filelib:last_modified(filename:join(Root, File)) || File <- Files ]).

to_iso8601({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).

copy(File, Source, Target) ->
    SourceFile = filename:join(Source, File),
    TargetFile = filename:join(Target, File),
    ok = filelib:ensure_dir(TargetFile),
    {ok, _} = file:copy(SourceFile, TargetFile).

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
            case is_excluded(filename:basename(File), IgnoreFiles) of
                true -> [];
                false -> [Path]
            end
    end.

is_excluded(Path, Excludes) ->
    lists:foldl(fun(_, true) -> true;
                   (RE, false) -> re:run(Path, RE, [unicode]) =/= nomatch
                end, false, Excludes).
