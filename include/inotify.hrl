%% Inotifywait event record definition

-record(inotify, { event, isdir, file, watched }).
