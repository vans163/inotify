# inotify
Erlang inotify  

### Prerequisite
Linux kernel >= 2.6.13  

### Note
If you are passing unicode paths make sure to use unicode:characters_to_binary/1 or
unicode:characters_to_list/1.  
Example: "말말" in shell is [47568,47568]. But we need [235,167,144,235,167,144]
to pass to the NIF.  
  
Make sure to use absolute paths filename:absname/1. 

http://man7.org/linux/man-pages/man7/inotify.7.html  
https://www.kernel.org/pub/linux/kernel/people/rml/inotify/headers/inotify.h  
  
To check inotify system limits:  
ls /proc/sys/fs/inotify/  
cat $_/*  

### Usage
```erlang

%Look at inotify_folder_watcher.erl to implement custom watcher.
%Default only sends message on a file changed.


inotify_folder_watcher:start_link({["/tmp", "/home/user"], self()}).
receive
    {inotify, changed, FileName} -> pass
end.

```
