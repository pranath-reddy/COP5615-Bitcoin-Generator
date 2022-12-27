%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-1

-module(client).
-export([main/2]).
-import(helpers, [gen_rand_str/1, hash_str/1, get_leading_zeros/2]).
-define(PORT, 0476).

% The main client function
main(ID, IP_address) ->
  % Args:
  % ID: [1,9] - Set Unique ID for the Client
  % IP_address - Address of the Server

  % Connect to server
  {_, Address} = inet:parse_address(IP_address),
  {_, Socket} = gen_tcp:connect(Address, ?PORT, [binary, {packet, 0}, {active, false}]),
  gen_tcp:send(Socket, "Connecting to Server"),
  io:fwrite("Client ID: ~p\n", [ID]),
  {ok,<<W, K>>} = gen_tcp:recv(Socket, 0),

  % Start the master actor
  spawn(fun() -> client_master(W, K, ID, Socket) end).

% The master actor of client
client_master(W, K, ID, Socket) ->
  % Args:
  % W: No of workers, each worker will keep mining until a bitcoin is found
  % K: No of leading zeroes required
  % ID: [1,9] - Set Unique ID for the Client
  % Socket

  io:fwrite("Client Started Mining\n"),
  io:fwrite("Number of workers: ~p\n", [W]),
  io:fwrite("Number of Leading Zeroes: ~p\n", [K]),
  io:fwrite("Prefix: ~p\n", ["kumbam.pranath"]),
  spawner(W, K, ID, Socket).

% The mining actor of client
client_worker(K, ID, Socket) ->
    % Args:
    % K: No of leading zeroes required
    % ID: [1,9] - Set Unique ID for the Client
    % Socket

    S = gen_rand_str(8),
    H = hash_str(S),
    [Z | H2] = get_leading_zeros(H, K),
    if
      H2 == Z ->
        gen_tcp:send(Socket, [ID, S, H]);
      true ->
        client_worker(K, ID, Socket)
    end.

% Spawns the required number of client workers
spawner(0, _K, _ID, _Socket) -> io:fwrite("Spawned All Client Workers\n");
spawner(W, K, ID, Socket) ->
    % Args:
    % W: No of workers, each worker will keep mining until a bitcoin is found
    % K: No of leading zeroes required
    % ID: [1,9] - Set Unique ID for the Client
    % Socket

    spawn(fun() -> client_worker(K, ID, Socket) end),
    spawner(W-1, K, ID, Socket).
