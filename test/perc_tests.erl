%% Copyright (c) 2012-2021, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(perc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("perc/include/perc_signal.hrl").

-define(PRIO_PROCESS, perc_prio:define(prio_process)).
-define(UINT(N, S), N:S / native - unsigned - integer - unit:8).

kill_test() ->
    Res = os:cmd("sleep 60 & echo $!"),
    Pid = as_int(Res),
    ok = perc:kill(Pid, 9),
    % sleep for the process to be reaped
    timer:sleep(1),
    {error, esrch} = perc:kill(Pid, 9).

kill_errno_test() ->
    {error, einval} = perc:kill(1, 1000),
    {error, eperm} = perc:kill(1, 1).

prlimit_test() ->
    Fd = <<128:8/native-unsigned-integer-unit:8, 128:8/native-unsigned-integer-unit:8>>,

    case perc:prlimit(0, rlimit_nofile, Fd, <<>>) of
        {error, unsupported} -> ok;
        {ok, Fd, <<>>} -> {ok, <<>>, Fd} = perc:prlimit(0, rlimit_nofile, <<>>, <<0:128>>)
    end.

prio_test() ->
    Res = os:cmd("sleep 60 & echo $!"),
    Pid = as_int(Res),

    {ok, 0} = perc:getpriority(?PRIO_PROCESS, Pid),
    ok = perc:setpriority(?PRIO_PROCESS, Pid, 10),
    {ok, 10} = perc:getpriority(?PRIO_PROCESS, Pid),

    {error, eacces} = perc:setpriority(?PRIO_PROCESS, Pid, -10),

    % User may not be allowed to decrease process priority
    {ok, 15} =
        case perc:renice({pid, Pid}, "-5") of
            {ok, 5} ->
                ok = perc:renice({pid, Pid}, "+10"),
                perc:getpriority(?PRIO_PROCESS, Pid);
            {error, eacces} ->
                ok = perc:renice({pid, Pid}, "+5"),
                perc:getpriority(?PRIO_PROCESS, Pid)
        end,

    ok = perc:kill(Pid, 9).

signalfd_test() ->
    case perc:sigaddset([]) of
        {ok, _} ->
            signalfd_test_create();
        {error, unsupported} ->
            ok
    end.

signalfd_test_create() ->
    Signals = [sigusr1, sigterm, sigusr2, sighup],
    {ok, Ref} = perc_signal:start(Signals),

    spawn(fun() ->
        [perc:kill(0, Signal) || Signal <- Signals]
    end),

    signalfd_test_poll(Ref, Signals).

signalfd_test_poll(Ref, []) ->
    perc_signal:stop(Ref);
signalfd_test_poll(Ref, Signals) ->
    receive
        {signal, Ref, #signalfd_siginfo{ssi_signo = ?SIGHUP}} ->
            signalfd_test_poll(Ref, Signals -- [sighup]);
        {signal, Ref, #signalfd_siginfo{ssi_signo = ?SIGTERM}} ->
            signalfd_test_poll(Ref, Signals -- [sigterm]);
        {signal, Ref, #signalfd_siginfo{ssi_signo = ?SIGUSR1}} ->
            signalfd_test_poll(Ref, Signals -- [sigusr1]);
        {signal, Ref, #signalfd_siginfo{ssi_signo = ?SIGUSR2}} ->
            signalfd_test_poll(Ref, Signals -- [sigusr2]);
        Error ->
            error_logger:error_report([{error, Error}])
    end.

as_int(Res) ->
    "\n" ++ N = lists:reverse(Res),
    list_to_integer(lists:reverse(N)).

umask_test() ->
    Mask = 8#077,
    MaskStr = "077",

    Cur = perc:umask(),
    Cur = perc:umask(MaskStr),
    Mask = perc:umask(Mask),
    Mask = perc:umask(Cur),

    ok.

getumask_test() ->
    Mask = perc:umask(),
    Mask = perc:getumask().

umask_stress_test() ->
    Self = self(),
    N = 100,
    [spawn(fun() -> set_mask(Self, N) end) || _ <- lists:seq(1, 100)],

    ok = umask_stress_test_1(N).

umask_stress_test_1(0) ->
    ok;
umask_stress_test_1(N) ->
    receive
        ok -> umask_stress_test_1(N - 1);
        error -> umask_race
    after 1000 -> timeout
    end.

set_mask(Pid, 0) ->
    Pid ! ok;
set_mask(Pid, N) ->
    case perc:umask() of
        0 -> Pid ! error;
        _ -> set_mask(Pid, N - 1)
    end.

rlimit_test() ->
    Size = erlang:system_info({wordsize, external}),

    % Get the current soft and hard file descriptor limit for the process
    {ok, <<?UINT(_Soft, Size), ?UINT(Hard, Size)>>} = perc:getrlimit(rlimit_nofile),

    % Set the soft limit to the hard limit
    Limit1 = <<?UINT(Hard, Size), ?UINT(Hard, Size)>>,
    {ok, Limit1} = perc:setrlimit(rlimit_nofile, Limit1),

    % Set the limits beyond the hard limit
    Invalid = Hard * 2,
    Limit2 = <<?UINT(Invalid, Size), ?UINT(Invalid, Size)>>,
    {error, eperm} = perc:setrlimit(rlimit_nofile, Limit2),

    ok.

getuid_test() ->
    true = is_integer(perc:getuid()).

getgid_test() ->
    true = is_integer(perc:getgid()).
