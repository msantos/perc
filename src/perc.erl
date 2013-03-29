%% Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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
-module(perc).

-export([
    kill/2,

    renice/2,
    getpriority/2,
    setpriority/3,

    umask/0, umask/1,

    getrlimit/1,
    setrlimit/2,

    % Linux only
    prctl/3,
    prlimit/4,
    signalfd/1, signalfd/2,
    sigaddset/1, close/1
    ]).

-on_load(on_load/0).


on_load() ->
    erlang:load_nif(progname(), []).

kill(Pid, Signal) when is_integer(Signal) ->
    kill_nif(Pid, Signal);
kill(Pid, Signal) when is_atom(Signal) ->
    kill_nif(Pid, perc_signal:define(Signal)).
kill_nif(_,_) ->
    erlang:error(not_implemented).

getpriority(_,_) ->
    erlang:error(not_implemented).

setpriority(_,_,_) ->
    erlang:error(not_implemented).

renice(Type, Priority) ->
    {Which, Who} = prio(Type),
    case renice(Which, Who, Priority) of
        ok ->
            getpriority(Which, Who);
        Error ->
            Error
    end.

renice(Which, Who, "+" ++ Priority) ->
    case getpriority(Which, Who) of
        {ok, OldPrio} ->
            N = list_to_integer(Priority),
            setpriority(Which, Who, OldPrio + N);
        Error ->
            Error
    end;
renice(Which, Who, "-" ++ Priority) ->
    case getpriority(Which, Who) of
        {ok, OldPrio} ->
            N = list_to_integer(Priority),
            setpriority(Which, Who, OldPrio - N);
        Error ->
            Error
    end;
renice(Which, Who, Priority) when is_list(Priority) ->
    setpriority(Which, Who, list_to_integer(Priority));
renice(Which, Who, Priority) when is_integer(Priority) ->
    setpriority(Which, Who, Priority).

umask() ->
    % Use process registration as a lock, to prevent a race condition
    % in successive umasks.
    %
    % If more than one process sets the umask, it's possible that the
    % process will be swapped out after the umask(0). This will leave beam
    % with a umask of 0.
    %
    % There is a possible DoS if something else registers 'perc_umask'
    % and never releases it.
    %
    % In the event something else changes the umask between calls,
    % umask/0 will crash, releasing the lock.
    %
    % Probably simpler to add another NIF for umask/0 to make the
    % sequence atomic.
    try erlang:register(perc_umask, self()) of
        true ->
            Mask = umask(0),
            0 = umask(Mask),
            erlang:unregister(perc_umask),
            Mask
    catch
        error:badarg ->
            timer:sleep(1),
            umask()
    end.

umask(Mask) when is_list(Mask) ->
    umask(list_to_integer(Mask, 8));
umask(Mask) when is_integer(Mask) ->
    umask_nif(Mask).

umask_nif(_) ->
    erlang:error(not_implemented).

getrlimit(Resource) when is_atom(Resource) ->
    getrlimit(perc_rlimit:define(Resource));
getrlimit(Resource) ->
    % struct rlimit {
    %   rlim_t rlim_cur;  /* Soft limit */
    %   rlim_t rlim_max;  /* Hard limit (ceiling for rlim_cur) */
    % };
    getrlimit_nif(Resource, <<0:(16*8)>>).

getrlimit_nif(_,_) ->
    erlang:error(not_implemented).

setrlimit(Resource, Limit) when is_atom(Resource) ->
    setrlimit(perc_rlimit:define(Resource), Limit);
setrlimit(Resource, Limit) ->
    setrlimit_nif(Resource, Limit).

setrlimit_nif(_,_) ->
    erlang:error(not_implemented).


prctl(_,_,_) ->
    erlang:error(not_implemented).

prlimit(Pid, Resource, New, Old) when is_atom(Resource) ->
    prlimit(Pid, perc_rlimit:define(Resource), New, Old);
prlimit(Pid, Resource, New, Old) ->
    prlimit_nif(Pid, Resource, New, Old).

prlimit_nif(_,_,_,_) ->
    erlang:error(not_implemented).

sigaddset(Signals) when is_list(Signals) ->
    Signals1 = lists:map(
            fun(N) when is_integer(N) -> N;
               (N) when is_atom(N) ->
                    perc_signal:define(N)
            end,
            Signals),
    sigaddset_nif(Signals1).

sigaddset_nif(_) ->
    erlang:error(not_implemented).

close(_) ->
    erlang:error(not_implemented).

signalfd(Mask) ->
    signalfd(-1, Mask).

signalfd(Fd, Mask) ->
    signalfd_nif(Fd, Mask).

signalfd_nif(_,_) ->
    erlang:error(not_implemented).

prio({pid, N}) ->
    {perc_prio:define(prio_process), N};
prio({pgrp, N}) ->
    {perc_prio:define(prio_pgrp), N};
prio({user, N}) ->
    {perc_prio:define(prio_user), N};
prio(N) when is_integer(N) ->
    {perc_prio:define(prio_process), N}.

progname() ->
    case code:priv_dir(?MODULE) of
        {error,bad_name} ->
            filename:join([
                filename:dirname(code:which(?MODULE)),
                    "..",
                    "priv",
                    ?MODULE
                ]);
        Dir ->
            filename:join([Dir,?MODULE])
    end.
