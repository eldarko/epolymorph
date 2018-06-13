-module(tcp_connection).

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
  gen_tcp:connect(Host, Port, []).

epolymorph_delete(Socket) ->
  gen_tcp:close(Socket).

send(Socket, Data) ->
  gen_tcp:send(Socket, Data).

