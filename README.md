haskell-dirWatcher - directory watcher app written in Haskell
=======

Simple application to watch some directory for file changes & perform some
action (written in Haskell of course).

I used it to send files via Dropbox to my printer server, but there are many
more possibilities. ;)

Of course cron would do the job, but it's not the point ;)

Moreover file size monitoring is implemented so the action will be called only
after file size stays still for a while (and not immediately when the file
appears in the directory).
