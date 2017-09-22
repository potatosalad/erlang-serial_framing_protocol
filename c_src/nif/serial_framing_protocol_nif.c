// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "serial_framing_protocol_nif.h"
#include "serial_framing_protocol.h"

static ERL_NIF_TERM ATOM_false;
static ERL_NIF_TERM ATOM_ok;
static ERL_NIF_TERM ATOM_read;
static ERL_NIF_TERM ATOM_sfp;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_write;
static ErlNifResourceType *serial_framing_protocol_resource_type = NULL;

typedef struct sfp_socket_s {
    ErlNifEnv *env;
    ERL_NIF_TERM pid_term;
    ErlNifPid pid;
    ErlNifMonitor monitor;
    SFPcontext ctx;
} sfp_socket_t;

// extern int erts_fprintf(FILE *, const char *, ...);

/*
 * NIF Functions
 */

static ERL_NIF_TERM
serial_framing_protocol_nif_getsizeof_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_ulong(env, (unsigned long)sfpGetSizeof());
}

static void
sfp_socket_read(uint8_t *buf, size_t len, void *userdata)
{
    sfp_socket_t *s = (void *)userdata;
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM data;
    unsigned char *rbuf = enif_make_new_binary(msg_env, len, &data);
    (void)memcpy(rbuf, buf, len);
    (void)enif_send(NULL, &s->pid, msg_env, enif_make_tuple3(msg_env, ATOM_sfp, ATOM_read, data));
    (void)enif_free_env(msg_env);
    return;
}

static int
sfp_socket_write(uint8_t *octets, size_t len, size_t *outlen, void *userdata)
{
    sfp_socket_t *s = (void *)userdata;
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM data;
    unsigned char *wbuf = enif_make_new_binary(msg_env, len, &data);
    (void)memcpy(wbuf, octets, len);
    (void)enif_send(NULL, &s->pid, msg_env, enif_make_tuple3(msg_env, ATOM_sfp, ATOM_write, data));
    (void)enif_free_env(msg_env);
    if (outlen != NULL) {
        *outlen = len;
    }
    return 0;
}

static ERL_NIF_TERM
serial_framing_protocol_nif_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid self;
    enif_self(env, &self);
    sfp_socket_t *s = (void *)enif_alloc_resource(serial_framing_protocol_resource_type, sizeof(sfp_socket_t));
    s->env = enif_alloc_env();
    s->pid_term = enif_make_copy(s->env, enif_make_pid(env, &self));
    if (!enif_get_local_pid(s->env, s->pid_term, &s->pid) || enif_monitor_process(env, (void *)s, &self, &s->monitor) != 0) {
        (void)enif_release_resource((void *)s);
        return enif_make_badarg(env);
    }
    (void)sfpInit(&s->ctx);
    (void)sfpSetDeliverCallback(&s->ctx, sfp_socket_read, (void *)s);
    (void)sfpSetWriteCallback(&s->ctx, sfp_socket_write, (void *)s);
    ERL_NIF_TERM out = enif_make_resource(env, (void *)s);
    (void)enif_release_resource((void *)s);
    return out;
}

static ERL_NIF_TERM
serial_framing_protocol_nif_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sfp_socket_t *s = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], serial_framing_protocol_resource_type, (void **)&s)) {
        return enif_make_badarg(env);
    }
    (void)sfpInit(&s->ctx);
    (void)sfpSetDeliverCallback(&s->ctx, sfp_socket_read, (void *)s);
    (void)sfpSetWriteCallback(&s->ctx, sfp_socket_write, (void *)s);
    return ATOM_ok;
}

static ERL_NIF_TERM
serial_framing_protocol_nif_connect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sfp_socket_t *s = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], serial_framing_protocol_resource_type, (void **)&s)) {
        return enif_make_badarg(env);
    }
    (void)sfpConnect(&s->ctx);
    return ATOM_ok;
}

static ERL_NIF_TERM
serial_framing_protocol_nif_is_connected_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sfp_socket_t *s = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], serial_framing_protocol_resource_type, (void **)&s)) {
        return enif_make_badarg(env);
    }
    if (sfpIsConnected(&s->ctx)) {
        return ATOM_true;
    } else {
        return ATOM_false;
    }
}

static ERL_NIF_TERM
serial_framing_protocol_nif_read_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sfp_socket_t *s = NULL;
    ErlNifBinary data;
    if (argc != 2 || !enif_get_resource(env, argv[0], serial_framing_protocol_resource_type, (void **)&s) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }
    uint8_t *octets = (uint8_t *)data.data;
    size_t len = (size_t)data.size;
    while ((len--) > 0) {
        (void)sfpDeliverOctet(&s->ctx, *octets, NULL, 0, NULL);
        octets++;
    }
    return ATOM_ok;
}

static ERL_NIF_TERM
serial_framing_protocol_nif_write_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sfp_socket_t *s = NULL;
    ErlNifBinary data;
    if (argc != 2 || !enif_get_resource(env, argv[0], serial_framing_protocol_resource_type, (void **)&s) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }
    uint8_t *buf = (uint8_t *)data.data;
    size_t len = (size_t)data.size;
    (void)sfpWritePacket(&s->ctx, buf, len, NULL);
    return ATOM_ok;
}

/*
 * NIF Callbacks
 */

static void
serial_framing_protocol_resource_dtor(ErlNifEnv *env, void *obj)
{
    sfp_socket_t *s = (void *)obj;
    if (s) {
        if (s->env) {
            (void)enif_free_env(s->env);
            s->env = NULL;
        }
    }
    return;
}

static void
serial_framing_protocol_resource_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *monitor)
{
    return;
}

static ErlNifResourceTypeInit serial_framing_protocol_resource_init = {serial_framing_protocol_resource_dtor, NULL,
                                                                       serial_framing_protocol_resource_down};

static int
serial_framing_protocol_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (serial_framing_protocol_resource_type == NULL) {
        serial_framing_protocol_resource_type =
            enif_open_resource_type_x(env, "serial_framing_protocol_resource", &serial_framing_protocol_resource_init,
                                      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if (serial_framing_protocol_resource_type == NULL) {
            return ENOMEM;
        }
    }
    ATOM_false = enif_make_atom(env, "false");
    ATOM_ok = enif_make_atom(env, "ok");
    ATOM_read = enif_make_atom(env, "read");
    ATOM_sfp = enif_make_atom(env, "sfp");
    ATOM_true = enif_make_atom(env, "true");
    ATOM_write = enif_make_atom(env, "write");
    return 0;
}

static int
serial_framing_protocol_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    if (serial_framing_protocol_resource_type == NULL) {
        return serial_framing_protocol_nif_load(env, priv_data, load_info);
    }
    return 0;
}

static void
serial_framing_protocol_nif_unload(ErlNifEnv *env, void *priv_data)
{
    serial_framing_protocol_resource_type = NULL;
    return;
}

static ErlNifFunc serial_framing_protocol_nif_funcs[] = {{"getsizeof", 0, serial_framing_protocol_nif_getsizeof_0},
                                                         {"open", 0, serial_framing_protocol_nif_open_0},
                                                         {"init", 1, serial_framing_protocol_nif_init_1},
                                                         {"connect", 1, serial_framing_protocol_nif_connect_1},
                                                         {"is_connected", 1, serial_framing_protocol_nif_is_connected_1},
                                                         {"read", 2, serial_framing_protocol_nif_read_2},
                                                         {"write", 2, serial_framing_protocol_nif_write_2}};

ERL_NIF_INIT(serial_framing_protocol_nif, serial_framing_protocol_nif_funcs, serial_framing_protocol_nif_load, NULL,
             serial_framing_protocol_nif_upgrade, serial_framing_protocol_nif_unload);
