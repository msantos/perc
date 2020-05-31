%%% Copyright (c) 2012-2020 Michael Santos <michael.santos@gmail.com>. All
%%% rights reserved.
%%%
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
-module(perc_prctl).

-include("perc.hrl").
-include("perc_prctl.hrl").

-export([
    arch/0, arch/1,
    define/1,
    seccomp_data/1
    ]).

arch() ->
    case erlang:system_info(system_architecture) of
        "x86_64" ++ _ ->
            arch(x86_64);
        "i386" ++ _ ->
            arch(i386);
        _ ->
            {error, unsupported}
    end.

% linux/audit.h
arch(x86_64) -> 16#C000003E;
arch(ii386) -> 16#40000003.

define(pr_get_seccomp) -> ?PR_GET_SECCOMP;
define(pr_set_seccomp) -> ?PR_SET_SECCOMP.

% struct seccomp_data {
%     int nr;
%     __u32 arch;
%     __u64 instruction_pointer;
%     __u64 args[6];
% };
seccomp_data(#seccomp_data{
    nr = NR,
    arch = Arch,
    instruction_pointer = Ptr,
    args = Args
    }) ->
    <<?INT32(NR),
      ?UINT32(Arch),
      ?UINT64(Ptr),
      Args/binary>>;
seccomp_data(<<?INT32(NR),
    ?UINT32(Arch),
    ?UINT64(Ptr),
    Args/binary>>) ->
    #seccomp_data{
        nr = NR,
        arch = Arch,
        instruction_pointer = Ptr,
        args = Args
    }.
