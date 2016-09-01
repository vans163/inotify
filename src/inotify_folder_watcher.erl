-module(inotify_folder_watcher).
-behaviour(gen_server).

-compile(export_all).

-define(POLL_DELAY, 10).
-define(MASK, 16#4000410A). %ISDIR, CREATE, CLOSE_WRITE, Q_OVERFLOW, MODIFY

start_link({Folders, Parent}) -> gen_server:start_link(?MODULE, {Folders, Parent}, []).

init({Folders, Parent}) ->
    {ok, Fd} = inotify:init(),

    self() ! {watch_folders, Folders},
    self() ! tick,

    {ok, #{inotify_fd=> Fd, parent=> Parent, wd_lookup=> #{}}}.


watch_file(Fd, File) ->
    Absname = filename:absname(File),
    FileUnicode = unicode:characters_to_binary(Absname),

    %io:format("Watching ~p\n", [FileUnicode]),

    {ok, Wd} = inotify:add_watch(Fd, ?MASK, FileUnicode),
    {FileUnicode, Wd}.

recurse_folders(Fd, Folders) -> recurse_folders(Fd, Folders, #{}).
recurse_folders(Fd, [], Acc) -> Acc;
recurse_folders(Fd, [Folder|T], Acc) ->
    AbsFolder = filename:absname(unicode:characters_to_binary(Folder)),
    IsDir = filelib:is_dir(AbsFolder),

    %io:format("abstocheck ~p \n", [AbsFolder]),

    case file:list_dir(AbsFolder) of
        {error, enotdir} -> 
            recurse_folders(Fd, T, Acc);

        {error, enoent} when IsDir -> 
            {UnicodeName, Wd} = watch_file(Fd, AbsFolder),
            recurse_folders(Fd, T, Acc#{Wd=> UnicodeName});

        {error, _} -> 
            recurse_folders(Fd, T, Acc);

        {ok, Files} ->
            {UnicodeName, Wd} = watch_file(Fd, AbsFolder),
            Files2 = [filename:join(AbsFolder, unicode:characters_to_binary(X)) || X <- Files],
            recurse_folders(Fd, T++Files2, Acc#{Wd=> UnicodeName})
    end
    .

wd_lookup_absname(WdLookup, Wd, Filename) -> 
    case maps:get(Wd, WdLookup, undefined) of
        undefined -> {error, notfound};
        Abspath -> filename:join(Abspath, Filename)
    end.


%Add watched folders and recurse them
handle_info({watch_folders, Folders}, S) ->
    Fd = maps:get(inotify_fd, S),
    WdLookup = maps:get(wd_lookup, S),

    MoreWdLookup = recurse_folders(Fd, Folders),
    {noreply, S#{wd_lookup=> maps:merge(WdLookup, MoreWdLookup)}};

%Remove a watched file descriptor
handle_info({rm_watch, Fd, Wd}, S) -> ok = inotify:rm_watch(Fd, Wd);

handle_info(tick, S) ->
    Fd = maps:get(inotify_fd, S),
    Parent = maps:get(parent, S),
    WdLookup = maps:get(wd_lookup, S),

    case inotify:read(Fd) of
        {error, 11} -> 'EAGAIN', S;
        {error, ErrCode} -> throw({"inotify:read failed with", ErrCode}), S;

        {ok, Events} ->
            (fun Event([]) -> done;
                 Event([ {inotify, invalid_event} |T]) -> Event(T);
                 Event([ {inotify, Wd, Mask, Cookie, Filename} |T]) ->

                    %io:format("inotify ~p ~p ~p ~p\n", [Wd, Mask, Cookie, Filename]),

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
                    end,

                    Event(T)
            end)(Events)
    end,

    erlang:send_after(?POLL_DELAY, self(), tick),
    {noreply, S};

handle_info(Message, S) -> {noreply, S}.

handle_call(Message, From, S) -> {reply, ok, S}.
handle_cast(Message, S) -> {noreply, S}.

terminate(_Reason, S) -> ok.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 