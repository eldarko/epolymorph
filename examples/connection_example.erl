-module(connection_example).

-export([main/0]).

send_data_and_close(Conn) ->
  connection:send(Conn, "data"),
  connection:close(Conn).

main() ->
  % Create UDP-based implementation of connection
  {ok, Conn1} = connection:create(udp_connection, {"8.8.8.8", 53}),

  % Create TCP-based implementation of connection
  {ok, Conn2} = connection:create(tcp_connection, {"rnd-zabbix.avp.ru", 80}),

  % Use connection in an abstract way without knowing the underlying details
  send_data_and_close(Conn1),
  send_data_and_close(Conn2).
