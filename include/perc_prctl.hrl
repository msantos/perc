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
-define(PR_GET_SECCOMP, 21).
-define(PR_SET_SECCOMP, 22).

-define(SECCOMP_MODE_DISABLED, 0).          % seccomp is not in use
-define(SECCOMP_MODE_STRICT, 1).            % uses hard-coded filter
-define(SECCOMP_MODE_FILTER, 2).            % uses user-supplied filter

-define(SECCOMP_RET_KILL, 16#00000000).     % kill the task immediately
-define(SECCOMP_RET_TRAP, 16#00030000).     % disallow and force a SIGSYS
-define(SECCOMP_RET_ERRNO, 16#00050000).    % returns an errno
-define(SECCOMP_RET_TRACE, 16#7ff00000).    % pass to a tracer or disallow
-define(SECCOMP_RET_ALLOW, 16#7fff0000).    % allow

-define(SECCOMP_RET_ACTION, 16#ffff0000).
-define(SECCOMP_RET_DATA, 16#0000ffff).

-record(seccomp_data, {
    nr = 0,
    arch = 0,
    instruction_pointer = 0,
    args = <<0:(64*6)>>
}).
