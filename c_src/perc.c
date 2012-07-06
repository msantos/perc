/* Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#include <sys/time.h>
#include <sys/resource.h>

#ifdef HAVE_PRLIMIT
#pragma message "Enabling support for prlimit(2)"
#endif

#include "erl_nif.h"
#include "erl_driver.h"


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_unsupported;


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_unsupported = enif_make_atom(env, "unsupported");

    return 0;
}

    static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

    static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

    void
unload(ErlNifEnv* env, void* priv_data)
{
}

    static ERL_NIF_TERM
nif_kill(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    pid_t pid = 0;
    int signal = 0;


    if (!enif_get_int(env, argv[0], &pid))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &signal))
        return enif_make_badarg(env);

    if (kill(pid, signal) != 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return atom_ok;
}

    static ERL_NIF_TERM
nif_getpriority(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int which = 0;
    int who = 0;

    int prio = 0;


    if (!enif_get_int(env, argv[0], &which))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &who))
        return enif_make_badarg(env);

    errno = 0;
    if ( ((prio = getpriority(which, who)) == -1) && (errno != 0))
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return enif_make_tuple2(env, atom_ok, enif_make_int(env, prio));
}

    static ERL_NIF_TERM
nif_setpriority(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int which = 0;
    int who = 0;
    int prio = 0;


    if (!enif_get_int(env, argv[0], &which))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &who))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[2], &prio))
        return enif_make_badarg(env);

    if (setpriority(which, who, prio) == -1)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return atom_ok;
}

    static ERL_NIF_TERM
nif_prlimit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_PRLIMIT
    pid_t pid = 0;
    int resource = 0;
    ErlNifBinary new_limit = {0};
    ErlNifBinary old_limit = {0};


    if (!enif_get_int(env, argv[0], &pid))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &resource))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &new_limit))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[3], &old_limit))
        return enif_make_badarg(env);

    if ( !(new_limit.size == 0 || new_limit.size == sizeof(struct rlimit)) ||
         !(old_limit.size == 0 || old_limit.size == sizeof(struct rlimit)))
        return enif_make_badarg(env);

    if (prlimit(pid, resource,
                (new_limit.size == 0 ? NULL : (struct rlimit *)new_limit.data),
                (old_limit.size == 0 ? NULL : (struct rlimit *)old_limit.data)) != 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return enif_make_tuple3(env, atom_ok,
            enif_make_binary(env, &new_limit),
            enif_make_binary(env, &old_limit));
#else
    return enif_make_tuple2(env, atom_error, atom_unsupported);
#endif
}


static ErlNifFunc nif_funcs[] = {
    {"kill", 2, nif_kill},

    {"getpriority", 2, nif_getpriority},
    {"setpriority", 3, nif_setpriority},

    {"prlimit_nif", 4, nif_prlimit}
};

ERL_NIF_INIT(perc, nif_funcs, load, reload, upgrade, unload)
