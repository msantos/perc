perc is an Erlang interface for controlling Unix processes.

## COMPILING

    make


## EXPORTS

    kill(Pid, Signal) -> ok | {error, posix()}
    
        Types   Pid = integer()
                Signal = integer()

        Send a signal to a Unix process.

        Process IDs and signals are signed 32-bit integers.

        Sending a signal to PID 0 will send the signal to the Erlang VM.

    prlimit(Pid, Resource, NewLimit, OldLimit) -> {ok, NewLimit1, OldLimit1} | {error, Error}

        Types   Pid = integer()
                Resource = integer()
                NewLimit = NewLimit1 = <<>> | binary()
                OldLimit = OldLimit1 = <<>> | binary()
                Error = unsupported | posix()

        Linux only: on other platforms, {error, unsupported} will be
        returned to the caller.

        Set or retrieve process limits for a process. Passing in an
        empty binary for NewLimit or OldLimit indicates the caller is
        not interested in these values.

        The binary size of NewLimit/OldLimit must otherwise match the
        size of a struct rlimit for the platform. struct rlimit is
        usually composed of two 8 byte values in native format.


## EXAMPLES

### kill(2)

    $ sleep 1000000 &
    [1] 2947

    $ erl -pa ebin

    1> perc:kill(2947, 9).
    ok

    2> perc:kill(1, 1).
    {error,eperm}

    3> perc:kill(31337, 1).
    {error,esrch}

### prlimit(2)

    % Set RLIMIT_NOFILE (7) on the Erlang VM. Sets the current and max file descriptors
    % to 8.
    1> perc:prlimit(0, 7, <<8:8/native-unsigned-integer-unit:8, 8:8/native-unsigned-integer-unit:8>>, <<>>).
    {ok,<<8,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0>>,<<>>}

    2> file:open("/etc/passwd", [read]).
    {ok,<0.39.0>}

    3> file:open("/etc/passwd", [read]).
    {error,emfile}


## TODO

* process lookup

* Linux: add support for prctl(2)

* add support for getrlimit(2)/setrlimit(2)
