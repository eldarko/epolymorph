-module(udp_connection).

%
% 'connection' interface callbacks
%
-behaviour(connection).
-export([send/2]).

%
% 'epolymorph_instance_spec' callbacks
%
-behaviour(epolymorph_instance_spec).
-export([epolymorph_create/1]).
-export([epolymorph_delete/1]).

epolymorph_create({Host,Port}) ->
  case gen_udp:open(0) of
    {ok, Socket} ->
      {ok, {state, Socket, Host, Port}};
    {error, Reason} ->
      {error, Reason}
  end.

epolymorph_delete({state, Socket, _Host, _Port}) ->
  gen_udp:close(Socket).

send({state, Socket, Host, Port}, Data) ->
  gen_udp:send(Socket, Host, Port, Data).
