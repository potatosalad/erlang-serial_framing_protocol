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
-module(serial_framing_protocol).

%% Public API
-export([getsizeof/0]).
-export([open/0]).
-export([init/1]).
-export([connect/1]).
-export([is_connected/1]).
-export([read/2]).
-export([write/2]).
%% Internal API
-export([priv_dir/0]).

%% Types
-type socket() :: reference().

-export_type([socket/0]).

%%%===================================================================
%%% Public API Functions
%%%===================================================================

-spec getsizeof() -> non_neg_integer().
getsizeof() ->
	serial_framing_protocol_nif:getsizeof().

-spec open() -> socket().
open() ->
	serial_framing_protocol_nif:open().

-spec init(socket()) -> ok.
init(Socket) ->
	serial_framing_protocol_nif:init(Socket).

-spec connect(socket()) -> ok.
connect(Socket) ->
	serial_framing_protocol_nif:connect(Socket).

-spec is_connected(socket()) -> boolean().
is_connected(Socket) ->
	serial_framing_protocol_nif:is_connected(Socket).

-spec read(socket(), iodata()) -> ok.
read(Socket, Iodata) ->
	serial_framing_protocol_nif:read(Socket, Iodata).

-spec write(socket(), iodata()) -> ok.
write(Socket, Iodata) ->
	serial_framing_protocol_nif:write(Socket, Iodata).

%%%===================================================================
%%% Internal API Functions
%%%===================================================================

-spec priv_dir() -> file:filename_all().
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
