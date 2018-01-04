-module(tcp_connection).

%
% 'connection' interface callbacks
%
-behaviour(connection).
-export([send/2, close/1]).

%
% 'epolymorph_instance_spec' callbacks
%
-behaviour(epolymorph_instance_spec).
-export([epolymorph_create/1]).

epolymorph_create({Host,Port}) ->
  gen_tcp:connect(Host, Port, []).

send(Socket, Data) ->
  gen_tcp:send(Socket, Data).

close(Socket) ->
  gen_tcp:close(Socket).
