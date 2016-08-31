# inotify
Erlang inotify  

### Prerequisite
Linux kernel >= 2.6.13  

### Note
If you are passing unicode paths make sure to use unicode:characters_to_binary/1 or
unicde:characters_to_list/1.  
Example: "말말" in shell is [47568,47568]. But we need [235,167,144,235,167,144]
to pass to the NIF.

http://man7.org/linux/man-pages/man7/inotify.7.html  
  
To check inotify system limits:  
ls /proc/sys/fs/inotify/  
cat $_/*  

### Usage
```erlang

-module(folder_watcher).
-behaviour(gen_server).

-compile(export_all).

start_link(Folder) -> gen_server:start(?MODULE, Folder, []).

init(Folder) ->
    {ok, Fd} = inotify:init(),
    Mask = 0, %all events
    FolderUnicode = unicode:characters_to_binary(Folder),

    {ok, Wd} = inotify:add_watch(Fd, Mask, FolderUnicode),

    self() ! tick,
    {ok, #{inotify_fd=> Fd, folder_fd=> Wd, dirPath=> FolderUnicode}}.


handle_info(tick, S=#{inotify_fd:= Fd}) ->
    case inotify:read(Fd) of
        {error, 11} -> 'EAGAIN';
        {error, ErrCode} -> throw({"inotify:read failed with", ErrCode});

        {ok, Events} ->
            (fun Event([]) -> done;
                 Event([ {inotify, invalid_event} |T]) -> Event(T);
                 Event([ {inotify, Wd, Mask, Cookie, Filename} |T]) ->

                    io:format("inotify ~p ~p ~p ~p\n", [Wd, Mask, Cookie, Filename]),

                    CloseWrite = lists:member(close_write, Mask),
                    %File written to and changed

                    IsDir = lists:member(isdir, Mask),
                    Create = lists:member(create, Mask),
                    %New directory created, start monitoring it

                    Event(T)
            end)(Events)
            
    end,

    erlang:send_after(100, self(), tick),
    {noreply, S};

handle_info(Message, S) -> {noreply, S}.

handle_call(Message, From, S) -> {reply, ok, S}.
handle_cast(Message, S) -> {noreply, S}.

terminate(_Reason, S) -> ok.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

```
