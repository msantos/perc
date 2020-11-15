%%% @copyright 2012-2020 Michael Santos <michael.santos@gmail.com>

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
-module(perc_rlimit).

-export([
    define/1
    ]).


% XXX 64 bit values only supported
define(rlimit_cpu) -> 0;
define(rlimit_fsize) -> 1;
define(rlimit_data) -> 2;
define(rlimit_stack) -> 3;
define(rlimit_core) -> 4;
define(rlimit_rss) -> 5;
define(rlimit_nproc) -> 6;
define(rlimit_nofile) -> 7;
define(rlimit_ofile) -> define(rlimit_nofile);
define(rlimit_memlock) -> 8;
define(rlimit_as) -> 9;
define(rlimit_locks) -> 10;
define(rlimit_sigpending) -> 11;
define(rlimit_msgqueue) -> 12;
define(rlimit_nice) -> 13;
define(rlimit_rtprio) -> 14;
define(rlimit_rttime) -> 15;
define(rlimit_nlimits) -> 16;
define(rlim_infinity) -> 16#ffffffffffffffff;
define(rlim64_infinity) -> define(rlim_infinity).
