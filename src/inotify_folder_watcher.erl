-module(inotify_folder_watcher).
-behaviour(gen_server).

-compile(export_all).

-define(MASK, 16#4000410A). %ISDIR, CREATE, CLOSE_WRITE, Q_OVERFLOW, MODIFY

watch_file(Pid, Path) -> Pid ! {watch_file, Path}.
watch_folders(Pid, Folders) -> Pid ! {watch_folders, Folders}.


start_link({Folders, Parent}) -> gen_server:start_link(?MODULE, {Folders, Parent}, []).

init({Folders, Parent}) ->
    {ok, Fd} = inotify:init(),

    self() ! {watch_folders, Folders},

    {ok, #{inotify_fd=> Fd, parent=> Parent, wd_lookup=> #{}}}.


recurse_folders(Fd, []) -> done;
recurse_folders(Fd, [Folder|T]) ->
    AbsFolder = filename:absname(unicode:characters_to_binary(Folder)),
    IsDir = filelib:is_dir(AbsFolder),

    %io:format("abstocheck ~p \n", [AbsFolder]),

    case file:list_dir(AbsFolder) of
        {error, enotdir} -> 
            recurse_folders(Fd, T);

        {error, enoent} when IsDir -> 
            self() ! {watch_file, AbsFolder},
            recurse_folders(Fd, T);

        {error, _} -> 
            recurse_folders(Fd, T);

        {ok, Files} ->
            self() ! {watch_file, AbsFolder},
            Files2 = [filename:join(AbsFolder, unicode:characters_to_binary(X)) || X <- Files],
            recurse_folders(Fd, T++Files2)
    end
    .

wd_lookup_absname(WdLookup, Wd, Filename) -> 
    case maps:get(Wd, WdLookup, undefined) of
        undefined -> {error, notfound};
        Abspath -> filename:join(Abspath, Filename)
    end.

wd_rev_lookup(WdLookupIter, Absname) ->
    case maps:next(WdLookupIter) of
        {Wd, Absname, _} -> Wd;
        {_, _, NextIter} -> wd_rev_lookup(NextIter, Absname);
        none -> {error, notfound}
    end.


%Add watched folders and recurse them
handle_info({watch_folders, Folders}, S) ->
    Fd = maps:get(inotify_fd, S),
    recurse_folders(Fd, Folders),
    {noreply, S};


%watch single file/folder
handle_info({watch_file, Path}, S) ->
    Fd = maps:get(inotify_fd, S),
    WdLookup = maps:get(wd_lookup, S),

    Absname = filename:absname(Path),
    FileUnicode = filename:absname(Absname),

    {ok, Wd} = inotify:add_watch(Fd, ?MASK, FileUnicode),

    {noreply, S#{wd_lookup=> maps:merge(WdLookup, #{Wd=> FileUnicode})}};

%Remove a watched file/folder
handle_info({rm_watch, Path}, S) ->
    Fd = maps:get(inotify_fd, S),
    WdLookup = maps:get(wd_lookup, S),

    Absname = filename:absname(Path),
    Wd = wd_rev_lookup(maps:iterator(WdLookup), Absname),

    ok = inotify:rm_watch(Fd, Wd),

    {noreply, S#{wd_lookup=> maps:remove(Wd, WdLookup)}};

handle_info(tick, S) ->
    Fd = maps:get(inotify_fd, S),
    Parent = maps:get(parent, S),
    WdLookup = maps:get(wd_lookup, S),

    case inotify:read(Fd) of
        {error, 11} -> 'EAGAIN', S;
        {error, ErrCode} -> throw({"inotify:read failed with", ErrCode}), S;

        {ok, Events} ->
            lists:foreach(fun(E) ->
                    case E of
                        {inotify, invalid_event} -> pass;
                        {inotify, Wd, Mask, Cookie, Filename} ->
                            IsWritten = lists:member(close_write, Mask),
                            IsDir = lists:member(isdir, Mask),
                            Create = lists:member(create, Mask),

                            %File written to and changed
                            case {IsDir, IsWritten} of
                                {false, true} -> 
                                    AbsName1 = wd_lookup_absname(WdLookup, Wd, Filename),
                                    Parent ! {inotify, changed, AbsName1};
                                _ -> pass
                            end,

                            %New directory created, start monitoring it
                            case {IsDir, Create} of
                                {true, true} -> 
                                    AbsName2 = wd_lookup_absname(WdLookup, Wd, Filename),
                                    self() ! {watch_folders, [AbsName2]};
                                _ -> pass
                            end 
                    end
                end, Events
            )
    end,

    {noreply, S};

handle_info(Message, S) -> {noreply, S}.

handle_call(Message, From, S) -> {reply, ok, S}.
handle_cast(Message, S) -> {noreply, S}.

terminate(_Reason, S) -> ok.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 