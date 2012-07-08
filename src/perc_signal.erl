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
-module(perc_signal).

-export([
    define/1
    ]).


% Linux
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
