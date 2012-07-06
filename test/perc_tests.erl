%% Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
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

-define(PRIO_PROCESS, perc_prio:define(prio_process)).

kill_test() ->
    Res = os:cmd("sleep 60 & echo $!"),
    Pid = as_int(Res),
    ok = perc:kill(Pid, 9),
    timer:sleep(1), % sleep for the process to be reaped
    {error, esrch} = perc:kill(Pid, 9).

kill_errno_test() ->
    {error, einval} = perc:kill(1, 1000),
    {error, eperm} = perc:kill(1, 1).

prlimit_test() ->
    Fd = <<128:8/native-unsigned-integer-unit:8,
           128:8/native-unsigned-integer-unit:8>>,

    case perc:prlimit(0, rlimit_nofile, Fd, <<>>) of
        {error, unsupported} -> ok;
        {ok, Fd, <<>>} ->
            {ok, <<>>, Fd} = perc:prlimit(0, rlimit_nofile, <<>>, <<0:128>>)
    end.

prio_test() ->
    Res = os:cmd("sleep 60 & echo $!"),
    Pid = as_int(Res),

    {ok, 0} = perc:getpriority(?PRIO_PROCESS, Pid),
    ok = perc:setpriority(?PRIO_PROCESS, Pid, 10),
    {ok, 10} = perc:getpriority(?PRIO_PROCESS, Pid),

    {error, eacces} = perc:setpriority(?PRIO_PROCESS, Pid, -10),

    % User may not be allowed to decrease process priority
    {ok, 15} = case perc:renice({pid, Pid}, "-5") of
        {ok, 5} ->
            perc:renice({pid, Pid}, "+10");
        {error, eacces} ->
            perc:renice({pid, Pid}, "+5")
    end,

    ok = perc:kill(Pid, 9).

as_int(Res) ->
    "\n" ++ N = lists:reverse(Res),
    list_to_integer(lists:reverse(N)).
