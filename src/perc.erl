%%% @copyright 2012-2021 Michael Santos <michael.santos@gmail.com>

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(perc).

-export([
    kill/2,

    getuid/0,
    geteuid/0,
    getgid/0,
    getegid/0,

    renice/2,
    getpriority/2,
    setpriority/3,

    umask/0, umask/1,
    getumask/0,

    status/0,

    getrlimit/1,
    setrlimit/2,

    % Linux only
    prctl/3,
    prlimit/4,
    signalfd/1, signalfd/2,
    sigaddset/1,
    close/1
]).

-export_type([
    int32_t/0,
    pid_t/0,
    posix/0,
    priority/0,
    uint32_t/0,
    uint64_t/0
]).

-type posix() ::
    'e2big'
    | 'eacces'
    | 'eaddrinuse'
    | 'eaddrnotavail'
    | 'eadv'
    | 'eafnosupport'
    | 'eagain'
    | 'ealign'
    | 'ealready'
    | 'ebade'
    | 'ebadf'
    | 'ebadfd'
    | 'ebadmsg'
    | 'ebadr'
    | 'ebadrpc'
    | 'ebadrqc'
    | 'ebadslt'
    | 'ebfont'
    | 'ebusy'
    | 'ecapmode'
    | 'echild'
    | 'echrng'
    | 'ecomm'
    | 'econnaborted'
    | 'econnrefused'
    | 'econnreset'
    | 'edeadlk'
    | 'edeadlock'
    | 'edestaddrreq'
    | 'edirty'
    | 'edom'
    | 'edotdot'
    | 'edquot'
    | 'eduppkg'
    | 'eexist'
    | 'efault'
    | 'efbig'
    | 'ehostdown'
    | 'ehostunreach'
    | 'eidrm'
    | 'einit'
    | 'einprogress'
    | 'eintr'
    | 'einval'
    | 'eio'
    | 'eisconn'
    | 'eisdir'
    | 'eisnam'
    | 'el2hlt'
    | 'el2nsync'
    | 'el3hlt'
    | 'el3rst'
    | 'elbin'
    | 'elibacc'
    | 'elibbad'
    | 'elibexec'
    | 'elibmax'
    | 'elibscn'
    | 'elnrng'
    | 'eloop'
    | 'emfile'
    | 'emlink'
    | 'emsgsize'
    | 'emultihop'
    | 'enametoolong'
    | 'enavail'
    | 'enet'
    | 'enetdown'
    | 'enetreset'
    | 'enetunreach'
    | 'enfile'
    | 'enoano'
    | 'enobufs'
    | 'enocsi'
    | 'enodata'
    | 'enodev'
    | 'enoent'
    | 'enoexec'
    | 'enolck'
    | 'enolink'
    | 'enomem'
    | 'enomsg'
    | 'enonet'
    | 'enopkg'
    | 'enoprotoopt'
    | 'enospc'
    | 'enosr'
    | 'enostr'
    | 'enosym'
    | 'enosys'
    | 'enotblk'
    | 'enotcapable'
    | 'enotconn'
    | 'enotdir'
    | 'enotempty'
    | 'enotnam'
    | 'enotrecoverable'
    | 'enotsock'
    | 'enotsup'
    | 'enotty'
    | 'enotuniq'
    | 'enxio'
    | 'eopnotsupp'
    | 'eoverflow'
    | 'eownerdead'
    | 'eperm'
    | 'epfnosupport'
    | 'epipe'
    | 'eproclim'
    | 'eprocunavail'
    | 'eprogmismatch'
    | 'eprogunavail'
    | 'eproto'
    | 'eprotonosupport'
    | 'eprototype'
    | 'erange'
    | 'erefused'
    | 'eremchg'
    | 'eremdev'
    | 'eremote'
    | 'eremoteio'
    | 'eremoterelease'
    | 'erofs'
    | 'erpcmismatch'
    | 'erremote'
    | 'eshutdown'
    | 'esocktnosupport'
    | 'espipe'
    | 'esrch'
    | 'esrmnt'
    | 'estale'
    | 'esuccess'
    | 'etime'
    | 'etimedout'
    | 'etoomanyrefs'
    | 'etxtbsy'
    | 'euclean'
    | 'eunatch'
    | 'eusers'
    | 'eversion'
    | 'ewouldblock'
    | 'exdev'
    | 'exfull'.

-type int32_t() :: -16#7fffffff..16#7fffffff.
-type uint32_t() :: 0..16#ffffffff.
-type uint64_t() :: 0..16#ffffffffffffffff.
-type pid_t() :: int32_t().

-on_load(on_load/0).

on_load() ->
    erlang:load_nif(progname(), []).

%% @doc kill(2): send a signal to a Unix process
%%
%% Process IDs and signals are signed 32-bit integers. A signal
%% can also be the lower cased signal name as an atom.
%%
%% Sending a signal to PID 0 will send the signal to the Erlang VM.
-spec kill(pid_t(), atom() | integer()) -> ok | {error, posix()}.
kill(Pid, Signal) when is_integer(Signal) ->
    kill_nif(Pid, Signal);
kill(Pid, Signal) when is_atom(Signal) ->
    kill_nif(Pid, perc_signal:define(Signal)).

kill_nif(_, _) ->
    erlang:nif_error(not_implemented).

%% @doc getpriority(2): get the process priority
%%
%% Get the priority (the "nice" value) of processes by pid, process
%% group or user.
-spec getpriority(int32_t(), int32_t()) -> {ok, int32_t()} | {error, posix()}.
getpriority(_, _) ->
    erlang:nif_error(not_implemented).

%% @doc setpriority(2): set the process priority
%%
%% Set the priority (the "nice" value) of processes by pid, process
%% group or user.
-spec setpriority(int32_t(), int32_t(), int32_t()) -> ok | {error, posix()}.
setpriority(_, _, _) ->
    erlang:nif_error(not_implemented).

%% @doc getuid(2): get user identity
-spec getuid() -> uint32_t().
getuid() ->
    erlang:nif_error(not_implemented).

%% @doc geteuid(2): get effective user id
-spec geteuid() -> uint32_t().
geteuid() ->
    erlang:nif_error(not_implemented).

%% @doc getgid(2): get group identity
-spec getgid() -> uint32_t().
getgid() ->
    erlang:nif_error(not_implemented).

%% @doc getegid(2): get effective group id
-spec getegid() -> uint32_t().
getegid() ->
    erlang:nif_error(not_implemented).

%% @doc renice: reset the process priority
%%
%% Convenience wrapper around getpriority/2 and setpriority/3,
%% similar to renice(1).
%%
%% WARNING: renice/2 makes successive calls to getpriority/2 and
%% setpriority/3. Since this sequence is not atomic, the priority
%% may change between calls or the process may have been terminated.
%%
%% Sets the priority of a process or processes by pid, pgroup or
%% user. The new priority may be an integer or a list containing a
%% relative priority, indicated by using a "+" or "-". For example,
%% using "+10" will increase the niceness of the process by 10.
-spec renice(priority(), string() | int32_t()) -> ok | {error, posix()}.
renice(Type, Priority) ->
    {Which, Who} = prio(Type),
    renice(Which, Who, Priority).

-spec renice(int32_t(), int32_t(), string() | int32_t()) -> ok | {error, posix()}.
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

%% @doc status: retrieve process status
%%
%% Reads /proc/self/status as a map.
-spec status() -> {ok, #{binary() => binary()}} | {error, atom()}.
status() ->
    case file:read_file("/proc/self/status") of
        {error, _} = Error ->
            Error;
        {ok, Status} ->
            {ok,
                maps:from_list(
                    [
                        list_to_tuple(binary:split(N, <<":\t">>))
                        || N <- binary:split(Status, <<"\n">>, [global, trim]),
                           N /= <<>>
                    ]
                )}
    end.

%% @doc getumask(3): safer version of umask/0
%%
%% Obtain the current process mask by parsing /proc/self/status,
%% avoiding the race condition in umask/0.
%%
%% If /proc/self/status does not exist or is not parsable, getumask/0
%% fails back to umask/0.
-spec getumask() -> int32_t().
getumask() ->
    case status() of
        {error, _} ->
            umask();
        {ok, Status} ->
            case maps:find(<<"Umask">>, Status) of
                error ->
                    umask();
                {ok, Mask} ->
                    binary_to_integer(Mask, 8)
            end
    end.

%% @doc umask(2): get the process mask
%%
%% WARNING: umask/0 is destructive: the umask is retrieved by setting
%% the process mask to 0, then re-setting it back to the original
%% mask. Between the successive calls to umask(2), the process
%% mask is 0. An erlang process calling umask/0 concurrently with
%% a process creating a file may have unexpected results.
-spec umask() -> int32_t().
umask() ->
    umask_nif().

umask_nif() ->
    erlang:nif_error(not_implemented).

%% @doc umask(2): set the process mask
%%
%% Sets the file creation mask for beam. The mask may be either
%% an integer or a list representing an octal number, e.g., either
%% 8#022 or "022".
%%
%% The old mask value is returned. To retrieve the current umask,
%% use umask/0 or getumask/0.
-spec umask(string() | int32_t()) -> int32_t().
umask(Mask) when is_list(Mask) ->
    umask(list_to_integer(Mask, 8));
umask(Mask) when is_integer(Mask) ->
    umask_nif(Mask).

umask_nif(_) ->
    erlang:nif_error(not_implemented).

%% @doc getrlimit(2): get process resource limits
%%
%% Get process limits for beam. See prlimit/4 for a list of the
%% resource atoms.
%%
%% The value returned is a struct rlimit:
%%
%% ```
%% 1> {ok, <<Soft:8/native-unsigned-integer-unit:8, Hard:8/native-unsigned-integer-unit:8>>} = perc:getrlimit(rlimit_nofile).
%% 2> Soft.
%% 1024
%% 3> Hard.
%% 4096
%% '''
-spec getrlimit(atom() | int32_t()) -> {ok, binary()} | {error, posix()}.
getrlimit(Resource) when is_atom(Resource) ->
    getrlimit(perc_rlimit:define(Resource));
getrlimit(Resource) ->
    % struct rlimit {
    %   rlim_t rlim_cur;  /* Soft limit */
    %   rlim_t rlim_max;  /* Hard limit (ceiling for rlim_cur) */
    % };
    Size = erlang:system_info({wordsize, external}),
    getrlimit_nif(Resource, <<0:(16 * Size)>>).

getrlimit_nif(_, _) ->
    erlang:nif_error(not_implemented).

%% @doc setrlimit(2): set process resource limits
%%
%% Set process limits for beam. See prlimit/4 for a list of the
%% resource atoms.
-spec setrlimit(atom() | int32_t(), atom() | int32_t()) -> {ok, binary()} | {error, posix()}.
setrlimit(Resource, Limit) when is_atom(Resource) ->
    setrlimit(perc_rlimit:define(Resource), Limit);
setrlimit(Resource, Limit) ->
    setrlimit_nif(Resource, Limit).

setrlimit_nif(_, _) ->
    erlang:nif_error(not_implemented).

-spec prctl(int32_t(), uint32_t() | uint64_t(), binary()) -> ok | {error, posix()}.
prctl(_, _, _) ->
    erlang:nif_error(not_implemented).

%% @doc prlimit(2): set resource limits for a specified process
%%
%% Linux only: on other platforms, {error, unsupported} will be
%% returned to the caller.
%%
%% Set or retrieve process limits for a process. Passing in an
%% empty binary for NewLimit or OldLimit indicates the caller is
%% not interested in these values.
%%
%% The binary size of NewLimit/OldLimit must otherwise match the size
%% of a struct rlimit for the platform. struct rlimit is usually
%% composed of two 8 byte values in native format. To retrieve the
%% current settings, pass in a zeroed 16 byte value.
-spec prlimit(pid_t(), atom() | int32_t(), binary(), binary()) ->
    {ok, binary(), binary()} | {error, posix()}.
prlimit(Pid, Resource, New, Old) when is_atom(Resource) ->
    prlimit(Pid, perc_rlimit:define(Resource), New, Old);
prlimit(Pid, Resource, New, Old) ->
    prlimit_nif(Pid, Resource, New, Old).

prlimit_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).

-spec sigaddset([atom() | int32_t()]) -> {ok, binary()} | {error, posix()}.
sigaddset(Signals) when is_list(Signals) ->
    Signals1 = lists:map(
        fun
            (N) when is_integer(N) ->
                N;
            (N) when is_atom(N) ->
                perc_signal:define(N)
        end,
        Signals
    ),
    sigaddset_nif(Signals1).

sigaddset_nif(_) ->
    erlang:nif_error(not_implemented).

-spec close(int32_t()) -> ok | {error, posix()}.
close(_) ->
    erlang:nif_error(not_implemented).

-spec signalfd(binary()) -> {ok, int32_t()} | {error, posix()}.
signalfd(Mask) ->
    signalfd(-1, Mask).

-spec signalfd(int32_t(), binary()) -> {ok, int32_t()} | {error, posix()}.
signalfd(Fd, Mask) ->
    signalfd_nif(Fd, Mask).

signalfd_nif(_, _) ->
    erlang:nif_error(not_implemented).

-type priority() ::
    int32_t()
    | {pid, int32_t()}
    | {pgrp, int32_t()}
    | {user, int32_t()}.

-spec prio(priority()) -> {int32_t(), int32_t()}.
prio({pid, N}) ->
    {perc_prio:define(prio_process), N};
prio({pgrp, N}) ->
    {perc_prio:define(prio_pgrp), N};
prio({user, N}) ->
    {perc_prio:define(prio_user), N};
prio(N) when is_integer(N) ->
    {perc_prio:define(prio_process), N}.

-spec progname() -> binary() | string().
progname() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            filename:join([
                filename:dirname(code:which(?MODULE)),
                "..",
                "priv",
                ?MODULE
            ]);
        Dir ->
            filename:join([Dir, ?MODULE])
    end.
