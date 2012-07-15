%%, Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
%%, All rights reserved.
%%, 
%%, Redistribution and use in source and binary forms, with or without
%%, modification, are permitted provided that the following conditions
%%, are met:
%%, 
%%, Redistributions of source code must retain the above copyright
%%, notice, this list of conditions and the following disclaimer.
%%, 
%%, Redistributions in binary form must reproduce the above copyright
%%, notice, this list of conditions and the following disclaimer in the
%%, documentation and/or other materials provided with the distribution.
%%, 
%%, Neither the name of the author nor the names of its contributors
%%, may be used to endorse or promote products derived from this software
%%, without specific prior written permission.
%%, 
%%, THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%, "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%, LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%, FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%, COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%, LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%, CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%, LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%, ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%, POSSIBILITY OF SUCH DAMAGE.
-define(SIGHUP, 1).
-define(SIGINT, 2).
-define(SIGQUIT, 3).
-define(SIGILL, 4).
-define(SIGTRAP, 5).
-define(SIGABRT, 6).
-define(SIGBUS, 7).
-define(SIGFPE, 8).
-define(SIGKILL, 9).
-define(SIGUSR1, 10).
-define(SIGSEGV, 11).
-define(SIGUSR2, 12).
-define(SIGPIPE, 13).
-define(SIGALRM, 14).
-define(SIGTERM, 15).
-define(SIGSTKFLT, 16).
-define(SIGCHLD, 17).
-define(SIGCONT, 18).
-define(SIGSTOP, 19).
-define(SIGTSTP, 20).
-define(SIGTTIN, 21).
-define(SIGTTOU, 22).
-define(SIGURG, 23).
-define(SIGXCPU, 24).
-define(SIGXFSZ, 25).
-define(SIGVTALRM, 26).
-define(SIGPROF, 27).
-define(SIGWINCH, 28).
-define(SIGIO, 29).
-define(SIGPWR, 30).
-define(SIGSYS, 31).
-define(SIGRTMIN, 34).
-define(SIGRTMAX, 64).


-record(signalfd_siginfo, {
    ssi_signo = 0,      % Signal number
    ssi_errno = 0,      % Error number (unused)
    ssi_code = 0,       % Signal code
    ssi_pid = 0,        % PID of sender
    ssi_uid = 0,        % Real UID of sender
    ssi_fd = 0,         % File descriptor (SIGIO)
    ssi_tid = 0,        % Kernel timer ID (POSIX timers)
    ssi_band = 0,       % Band event (SIGIO)
    ssi_overrun = 0,    % POSIX timer overrun count
    ssi_trapno = 0,     % Trap number that caused signal
    ssi_status = 0,     % Exit status or signal (SIGCHLD)
    ssi_int = 0,        % Integer sent by sigqueue(2)
    ssi_ptr = 0,        % Pointer sent by sigqueue(2)
    ssi_utime = 0,      % User CPU time consumed (SIGCHLD)
    ssi_stime = 0,      % System CPU time consumed (SIGCHLD)
    ssi_addr = 0,       % Address that generated signal (for hardware-generated signals)
    pad = 0             % Pad size to 128 bytes (allow for additional fields in the future)
}).
