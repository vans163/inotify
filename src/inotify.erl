-module(inotify).
-on_load(load_nif/0).

-export([init/0, add_watch/3, rm_watch/2, read/1]).

load_nif() -> 
    Path = case code:priv_dir(scv) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(scv)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Pathe ->
            Pathe
    end,
    FullPath = filename:join([Path, "inotify"]),
    erlang:load_nif(FullPath, 0).
    
init() -> "NIF library not loaded".
add_watch(_,_,_) -> "NIF library not loaded".
rm_watch(_,_) -> "NIF library not loaded".
read(_) -> "NIF library not loaded".