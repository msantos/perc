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
-module(perc_signal).
-behaviour(gen_server).

-include_lib("perc/include/perc.hrl").
-include_lib("perc/include/perc_signal.hrl").

-export([
    define/1,
    signalfd_siginfo/1
    ]).
-export([
    start/1,
    stop/1,
    getfd/1,

    start_link/1
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {
        port,
        pid,
        fd
        }).



%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
start(Signals) when is_list(Signals) ->
    start_link(Signals).

stop(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, stop).

getfd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getfd).

start_link(Signals) when is_list(Signals) ->
    case perc:sigaddset(Signals) of
        {ok, Mask} ->
            Pid = self(),
            gen_server:start_link(?MODULE, [Pid, Mask], []);
        Err ->
            Err
    end.


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Mask]) ->
    process_flag(trap_exit, true),

    case perc:signalfd(Mask) of
        {ok, FD} ->
            Port = erlang:open_port({fd, FD, FD}, [stream, binary]),
            {ok, #state{
                port = Port,
                pid = Pid,
                fd = FD
            }};
        Err ->
            Err
    end.


handle_call(getfd, _From, #state{fd = FD} = State) ->
    {reply, FD, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Signal received
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    [ Pid ! {signal, self(), signalfd_siginfo(Signal)} ||
        <<Signal:128/binary>> <= Data ],
    {noreply, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = FD, port = Port}) ->
    catch erlang:port_close(Port),
    perc:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Utility functions
%%--------------------------------------------------------------------

% Signal definitions: linux
define(sighup) ->  1;
define(sigint) ->  2;
define(sigquit) ->  3;
define(sigill) ->  4;
define(sigtrap) ->  5;
define(sigabrt) ->  6;
define(sigbus) ->  7;
define(sigfpe) ->  8;
define(sigkill) ->  9;
define(sigusr1) ->  10;
define(sigsegv) ->  11;
define(sigusr2) ->  12;
define(sigpipe) ->  13;
define(sigalrm) ->  14;
define(sigterm) ->  15;
define(sigstkflt) ->  16;
define(sigchld) ->  17;
define(sigcont) ->  18;
define(sigstop) ->  19;
define(sigtstp) ->  20;
define(sigttin) ->  21;
define(sigttou) ->  22;
define(sigurg) ->  23;
define(sigxcpu) ->  24;
define(sigxfsz) ->  25;
define(sigvtalrm) ->  26;
define(sigprof) ->  27;
define(sigwinch) ->  28;
define(sigio) ->  29;
define(sigpwr) ->  30;
define(sigsys) ->  31;
define(sigrtmin) ->  34;
define(sigrtmax) ->  64.

signalfd_siginfo(<<
        ?UINT32(Signo),     % Signal number
        ?INT32(Errno),      % Error number (unused)
        ?INT32(Code),       % Signal code
        ?UINT32(Pid),       % PID of sender
        ?UINT32(Uid),       % Real UID of sender
        ?INT32(Fd),         % File descriptor (SIGIO)
        ?UINT32(Tid),       % Kernel timer ID (POSIX timers)
        ?UINT32(Band),      % Band event (SIGIO)
        ?UINT32(Overrun),   % POSIX timer overrun count
        ?UINT32(Trapno),    % Trap number that caused signal
        ?INT32(Status),     % Exit status or signal (SIGCHLD)
        ?INT32(Int),        % Integer sent by sigqueue(2)
        ?UINT64(Ptr),       % Pointer sent by sigqueue(2)
        ?UINT64(Utime),     % User CPU time consumed (SIGCHLD)
        ?UINT64(Stime),     % System CPU time consumed (SIGCHLD)
        ?UINT64(Addr),      % Address that generated signal (for hardware-generated signals)
        Pad/binary          % Pad size to 128 bytes (allow for additional fields in the future)
    >> = Data) when byte_size(Data) =:= 128 ->
    #signalfd_siginfo{
        ssi_signo = Signo,
        ssi_errno = Errno,
        ssi_code = Code,
        ssi_pid = Pid,
        ssi_uid = Uid,
        ssi_fd = Fd,
        ssi_tid = Tid,
        ssi_band = Band,
        ssi_overrun = Overrun,
        ssi_trapno = Trapno,
        ssi_status = Status,
        ssi_int = Int,
        ssi_ptr = Ptr,
        ssi_utime = Utime,
        ssi_stime = Stime,
        ssi_addr = Addr,
        pad = Pad
        };
signalfd_siginfo(#signalfd_siginfo{
        ssi_signo = Signo,
        ssi_errno = Errno,
        ssi_code = Code,
        ssi_pid = Pid,
        ssi_uid = Uid,
        ssi_fd = Fd,
        ssi_tid = Tid,
        ssi_band = Band,
        ssi_overrun = Overrun,
        ssi_trapno = Trapno,
        ssi_status = Status,
        ssi_int = Int,
        ssi_ptr = Ptr,
        ssi_utime = Utime,
        ssi_stime = Stime,
        ssi_addr = Addr,
        pad = Pad
    }) ->
    <<
        ?UINT32(Signo),     % Signal number
        ?INT32(Errno),      % Error number (unused)
        ?INT32(Code),       % Signal code
        ?UINT32(Pid),       % PID of sender
        ?UINT32(Uid),       % Real UID of sender
        ?INT32(Fd),         % File descriptor (SIGIO)
        ?UINT32(Tid),       % Kernel timer ID (POSIX timers)
        ?UINT32(Band),      % Band event (SIGIO)
        ?UINT32(Overrun),   % POSIX timer overrun count
        ?UINT32(Trapno),    % Trap number that caused signal
        ?INT32(Status),     % Exit status or signal (SIGCHLD)
        ?INT32(Int),        % Integer sent by sigqueue(2)
        ?UINT64(Ptr),       % Pointer sent by sigqueue(2)
        ?UINT64(Utime),     % User CPU time consumed (SIGCHLD)
        ?UINT64(Stime),     % System CPU time consumed (SIGCHLD)
        ?UINT64(Addr),      % Address that generated signal (for hardware-generated signals)
        Pad/binary          % Pad size to 128 bytes (allow for additional fields in the future)
    >>.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
