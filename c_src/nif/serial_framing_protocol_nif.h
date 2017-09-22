// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef SERIAL_FRAMING_PROTOCOL_NIF_H
#define SERIAL_FRAMING_PROTOCOL_NIF_H

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

/* NIF Functions */

static ERL_NIF_TERM serial_framing_protocol_nif_getsizeof_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_connect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_is_connected_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_read_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM serial_framing_protocol_nif_write_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Callbacks */

static int serial_framing_protocol_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int serial_framing_protocol_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void serial_framing_protocol_nif_unload(ErlNifEnv *env, void *priv_data);

#endif
