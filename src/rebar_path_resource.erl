-module(rebar_path_resource).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/1]).

-include_lib("kernel/include/file.hrl").

init(Type, _State) ->
   Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
   {ok, Resource}.

lock(AppInfo, _) ->
  Path = extract_path(AppInfo),
  {path, Path, no_lock}.

download(Dir, AppInfo, _RebarState, _ResourceState) ->
  Path = extract_path(AppInfo),
  ok = filelib:ensure_dir(Dir),
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),

  Files = files(Source),

  lists:foreach(fun(File) -> copy(File, Source, Dir) end, Files),
  rebar_api:debug("copied source from=~p, to=~p ~n", [Path, Dir]),
  ok.

extract_path(AppInfo) ->
  case rebar_app_info:source(AppInfo) of
    {path, Path, _} ->
      Path;
    {path, Path} ->
      Path
  end.

make_vsn(_Dir) ->
  {error, "Replacing version of type `path` not supported."}.

needs_update(_AppInfo, _CustomState) ->
  true.

copy(File, Source, Target) ->
  SourceFile = filename:join(Source, File),
  TargetFile = filename:join(Target, File),
  ok = filelib:ensure_dir(TargetFile),
  {ok, _} = file:copy(SourceFile, TargetFile).

-define(EXCLUDE_PATHS, [".git", "_build", "examples"]).
-define(EXCLUDE_FILES, ["^\\.", "~$"]).

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
