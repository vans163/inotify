#include "erl_nif.h"

#include <sys/inotify.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

#include <unistd.h>
#include <stdio.h>

#define INOTIFY_BUF_LEN (10 * (sizeof(struct inotify_event) + NAME_MAX + 1))

ERL_NIF_TERM mk_atom(ErlNifEnv*,const char*);
ERL_NIF_TERM mk_error(ErlNifEnv*, const char*);
ERL_NIF_TERM mk_errno(ErlNifEnv*);

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int res = inotify_init1(IN_NONBLOCK);
    if (res < 0)
        return mk_errno(env);

    return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_int(env, res));
}

static ERL_NIF_TERM add_watch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;
    if (!enif_get_int(env, argv[0], &fd))
        return mk_error(env, "argv0 not_a_number");

    uint32_t mask = -1;
    if (!enif_get_uint(env, argv[1], &mask))
        return mk_error(env, "argv1 not_a_number");

    if (mask == 0)
        mask = IN_ALL_EVENTS;

    ErlNifBinary in;
    char path[PATH_MAX] = {0};
    int wd = -1;
    if (enif_is_list(env, argv[2])) {
        enif_get_string(env, argv[2], path, PATH_MAX, ERL_NIF_LATIN1);
        wd = inotify_add_watch(fd, path, mask);
    } else if (enif_inspect_binary(env, argv[2], &in)) {
        in.data[in.size] = 0;
        wd = inotify_add_watch(fd, in.data, mask);
    } else {
        return mk_error(env, "filename not_a_list or binary");
    }

    if (wd < 0)
        return mk_errno(env);

    return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_int(env, wd));
}

static ERL_NIF_TERM rm_watch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;
    if (!enif_get_int(env, argv[0], &fd))
        return mk_error(env, "argv0 not_a_number");

    int wd = -1;
    if (!enif_get_int(env, argv[1], &wd))
        return mk_error(env, "argv1 not_a_number");

    if (inotify_rm_watch(fd, wd) != 0) 
        return mk_errno(env);

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM inotify_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;
    if (!enif_get_int(env, argv[0], &fd))
        return mk_error(env, "argv0 not_a_number");

    char buf[INOTIFY_BUF_LEN] __attribute__ ((aligned(8)));

    ssize_t n = read(fd, buf, INOTIFY_BUF_LEN);
    if (n == -1 && errno != EAGAIN)
        return mk_errno(env);

    //We got EAGAIN most likely
    if (n <= 0) 
        return mk_errno(env);

    ERL_NIF_TERM retList = enif_make_list(env, 0);

    int i = 0;
    while (i < n) {
        struct inotify_event *ev;
        ERL_NIF_TERM ret;

        ev = (struct inotify_event *) &buf[i];
        if (!ev->len) {
            goto invalid_event;
        }

        int wd = ev->wd;
        uint32_t mask = ev->mask;
        uint32_t cookie = ev->cookie;

        //inotify copies in blocks for the filename with \0 padding
        //TODO: Ensure this works for all cases of unicode
        uint32_t zeroByteO = ev->len-1;
        for (;zeroByteO >= 0; zeroByteO--) {
            if (ev->name[zeroByteO] != 0) {
                zeroByteO++;
                break;
            }
        }
        ErlNifBinary nameBin;
        enif_alloc_binary(zeroByteO, &nameBin);
        memcpy(nameBin.data, ev->name, zeroByteO);
        ERL_NIF_TERM nameTerm = enif_make_binary(env, &nameBin);
        //--

        ERL_NIF_TERM maskList = enif_make_list(env, 0);

        if (mask & IN_ACCESS)        maskList = enif_make_list_cell(env, mk_atom(env, "access"), maskList);
        if (mask & IN_ATTRIB)        maskList = enif_make_list_cell(env, mk_atom(env, "attrib"), maskList);
        if (mask & IN_CLOSE_NOWRITE) maskList = enif_make_list_cell(env, mk_atom(env, "close_nowrite"), maskList);
        if (mask & IN_CLOSE_WRITE)   maskList = enif_make_list_cell(env, mk_atom(env, "close_write"), maskList);
        if (mask & IN_CREATE)        maskList = enif_make_list_cell(env, mk_atom(env, "create"), maskList);
        if (mask & IN_DELETE)        maskList = enif_make_list_cell(env, mk_atom(env, "delete"), maskList);
        if (mask & IN_DELETE_SELF)   maskList = enif_make_list_cell(env, mk_atom(env, "delete_self"), maskList);
        if (mask & IN_IGNORED)       maskList = enif_make_list_cell(env, mk_atom(env, "ignored"), maskList);
        if (mask & IN_ISDIR)         maskList = enif_make_list_cell(env, mk_atom(env, "isdir"), maskList);
        if (mask & IN_MODIFY)        maskList = enif_make_list_cell(env, mk_atom(env, "modify"), maskList);
        if (mask & IN_MOVE_SELF)     maskList = enif_make_list_cell(env, mk_atom(env, "move_self"), maskList);
        if (mask & IN_MOVED_FROM)    maskList = enif_make_list_cell(env, mk_atom(env, "moved_from"), maskList);
        if (mask & IN_MOVED_TO)      maskList = enif_make_list_cell(env, mk_atom(env, "moved_to"), maskList);
        if (mask & IN_OPEN)          maskList = enif_make_list_cell(env, mk_atom(env, "open"), maskList);
        if (mask & IN_Q_OVERFLOW)    maskList = enif_make_list_cell(env, mk_atom(env, "q_overflow"), maskList);
        if (mask & IN_UNMOUNT)       maskList = enif_make_list_cell(env, mk_atom(env, "unmount"), maskList);
        
        ret = enif_make_tuple5(env,
            mk_atom(env, "inotify"),
            enif_make_int(env, wd),
            maskList,

            enif_make_uint(env, cookie),

            nameTerm
        );

next_event:
        retList = enif_make_list_cell(env, ret, retList);
        i += sizeof(struct inotify_event) + ev->len;
        continue;

invalid_event:
        ret = enif_make_tuple2(env,
            mk_atom(env, "inotify"),
            mk_atom(env, "invalid_event")
        );
        goto next_event;
    }

    ERL_NIF_TERM retListInOrder;
    enif_make_reverse_list(env, retList, &retListInOrder);

    return enif_make_tuple2(env, mk_atom(env, "ok"), retListInOrder);
}



ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

ERL_NIF_TERM mk_errno(ErlNifEnv* env)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), enif_make_int(env, errno));
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"init", 0, init},
    {"add_watch", 3, add_watch},
    {"rm_watch", 2, rm_watch},
    {"read", 1, inotify_read},
};

ERL_NIF_INIT(inotify,nif_funcs,NULL,NULL,&upgrade,NULL)