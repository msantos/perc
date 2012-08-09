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
#include <unistd.h>

#include <sys/time.h>
#include <sys/resource.h>

#ifdef HAVE_SIGNALFD
#pragma message "Enabling support for signalfd(2)"
#include <sys/signalfd.h>
#endif

#ifdef HAVE_PRCTL
#include <sys/prctl.h>
#pragma message "Enabling support for prctl(2)"
#endif

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
nif_signalfd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_SIGNALFD
    int fd = 0;
    ErlNifBinary mask = {0};
    int rv = 0;


    if (!enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &mask) || mask.size != sizeof(sigset_t))
        return enif_make_badarg(env);

    /* According to sigprocmask(2):
     *
     *  "The use of sigprocmask() is unspecified in a multithreaded
     *   process; see pthread_sigmask(3)."
     *
     * And pthread_sigmask(3):
     *
     *  "The  pthread_sigmask()  function is just like sigprocmask(2),
     *   with the difference that its use in multithreaded programs is
     *   explicitly specified by POSIX.1-2001."
     *
     */
    if (pthread_sigmask(SIG_BLOCK, (sigset_t *)mask.data, NULL) < 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    if ( (rv = signalfd(fd, (sigset_t *)mask.data, SFD_NONBLOCK|SFD_CLOEXEC)) < 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return enif_make_tuple2(env, atom_ok, enif_make_int(env, rv));
#else
    return enif_make_tuple2(env, atom_error, atom_unsupported);
#endif
}

    static ERL_NIF_TERM
nif_sigaddset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_SIGNALFD
    sigset_t mask = {{0}};
    ERL_NIF_TERM head = {0};
    ERL_NIF_TERM tail = {0};
    ErlNifBinary bin = {0};


    if (!enif_is_list(env, argv[0]))
        return enif_make_badarg(env);

    sigemptyset(&mask);
    tail = argv[0];

    while (enif_get_list_cell(env, tail, &head, &tail)) {
        int sig = 0;

        if (!enif_get_int(env, head, &sig))
            return enif_make_badarg(env);

        sigaddset(&mask, sig);
    }

    if (!enif_alloc_binary(sizeof(mask), &bin))
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(ENOMEM)));

    (void)memcpy(bin.data, &mask, sizeof(mask));

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &bin));
#else
    return enif_make_tuple2(env, atom_error, atom_unsupported);
#endif
}

    static ERL_NIF_TERM
nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_SIGNALFD
    int fd;


    if (!enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    if (close(fd) < 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return atom_ok;
#else
    return enif_make_tuple2(env, atom_error, atom_unsupported);
#endif
}

    static ERL_NIF_TERM
nif_prctl(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_PRCTL
    int option = 0;
    unsigned long arg2 = 0;
    ErlNifBinary arg3 = {0};


    if (!enif_get_int(env, argv[0], &option))
        return enif_make_badarg(env);

    if (!enif_get_ulong(env, argv[1], &arg2))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &arg3))
        return enif_make_badarg(env);

    if (prctl(option, arg2, arg3.data) < 0)
        return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errno)));

    return atom_ok;
#else
    return enif_make_tuple2(env, atom_error, atom_unsupported);
#endif
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
    {"kill_nif", 2, nif_kill},

    {"getpriority", 2, nif_getpriority},
    {"setpriority", 3, nif_setpriority},

    {"prlimit_nif", 4, nif_prlimit},

    {"prctl", 3, nif_prctl},

    /* signal handling */
    {"close", 1, nif_close},
    {"sigaddset_nif", 1, nif_sigaddset},
    {"signalfd_nif", 2, nif_signalfd}
};

ERL_NIF_INIT(perc, nif_funcs, load, reload, upgrade, unload)
