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
                    | rlimit_cpu
                    | rlimit_fsize
                    | rlimit_data
                    | rlimit_stack
                    | rlimit_core
                    | rlimit_rss
                    | rlimit_nproc
                    | rlimit_nofile
                    | rlimit_ofile
                    | rlimit_memlock
                    | rlimit_as
                    | rlimit_locks
                    | rlimit_sigpending
                    | rlimit_msgqueue
                    | rlimit_nice
                    | rlimit_rtprio
                    | rlimit_rttime
                    | rlimit_nlimits
                    | rlim_infinity
                NewLimit = NewLimit1 = <<>> | binary()
                OldLimit = OldLimit1 = <<>> | binary()
                Error = unsupported | posix()

        Linux only: on other platforms, {error, unsupported} will be
        returned to the caller.

        Set or retrieve process limits for a process. Passing in an
        empty binary for NewLimit or OldLimit indicates the caller is
        not interested in these values.

        The binary size of NewLimit/OldLimit must otherwise match the size
        of a struct rlimit for the platform. struct rlimit is usually
        composed of two 8 byte values in native format. To retrieve the
        current settings, pass in a zero'ed 16 byte value.


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

    % Get the current value for rlimit_cpu
    1> perc:prlimit(0, rlimit_cpu, <<>>, <<0:128>>).
    {ok,<<>>,<<"ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ">>}

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

* atom rlimit\_\* constants only supported for 64-bit values

* Linux: add support for prctl(2)

* add support for getrlimit(2)/setrlimit(2)
