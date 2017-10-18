%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  19 September 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(serial_framing_protocol_nif).

%% NIF
-export([getsizeof/0]).
-export([open/0]).
-export([init/1]).
-export([connect/1]).
-export([is_connected/1]).
-export([read/2]).
-export([write/2]).

-on_load(init/0).

%%%===================================================================
%%% NIF Functions
%%%===================================================================

getsizeof() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

open() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

init(_Socket) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

connect(_Socket) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

is_connected(_Socket) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

read(_Socket, _Iodata) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

write(_Socket, _Iodata) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(serial_framing_protocol:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
