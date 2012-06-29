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


## EXAMPLES

    $ sleep 1000000 &
    [1] 2947

    $ erl -pa ebin

    1> perc:kill(2947, 9).
    ok

    2> perc:kill(1, 1).
    {error,eperm}

    3> perc:kill(31337, 1).
    {error,esrch}


## TODO

* process lookup
