%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-1

-module(server).
-export([main/2]).
-import(helpers, [gen_rand_str/1, hash_str/1, get_leading_zeros/2]).
-define(PORT, 0476).

% The main server function
main(W, K) ->
  % Args:
  % W: No of workers, each worker will keep mining until a bitcoin is found
  % K: No of leading zeroes required

  % Benchmarking
  statistics(runtime),
	statistics(wall_clock),

  % Get the IP Address of the server
  {ok, L} = inet:getif(),
  S_IP = element(1, hd(L)),
  io:fwrite("Server IP: ~p\n", [S_IP]),

  % Start the master actor
  spawn(fun() -> master(W, K) end),

  % Receive connection from Client
  {_, LSocket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
  {_, Socket} = gen_tcp:accept(LSocket),
  {_, _Bin} = do_recv(Socket, W, K, []).

% Recieves messages from Client
do_recv(Socket, W, K, Bs) ->
    % Args:
    % Socket
    % W: No of workers, each worker will keep mining until a bitcoin is found
    % K: No of leading zeroes required

    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
          if
            (B =:= <<"Connecting to Server">>) ->
              io:fwrite("Recieved Client Signal ~p\n", [B]),
              gen_tcp:send(Socket, [W, K]),
              do_recv(Socket, W, K, [Bs, B]);
            true ->
              {S1, H} = split_binary(B, 24),
              {ID, S} = split_binary(S1, 1),
              io:fwrite("Client ~p Found ~p ~p\n", [binary_to_list(ID), binary_to_list(S), binary_to_list(H)]),
              do_recv(Socket, W, K, [Bs, B])
            end;
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

% The master actor
master(W, K) ->
  % Args:
  % W: No of workers, each worker will keep mining until a bitcoin is found
  % K: No of leading zeroes required

  io:fwrite("Started Mining\n"),
  io:fwrite("Number of workers: ~p\n", [W]),
  io:fwrite("Number of Leading Zeroes: ~p\n", [K]),
  io:fwrite("Prefix: ~p\n", ["kumbam.pranath"]),

  % PID of the bitcoin logger
  LogID = spawn(fun() -> bitcoinLogger(W) end),
  spawner(W, K, LogID).

% The mining actor
worker(K, LogID) ->
  % Args:
  % K: No of leading zeroes required
  % LogID: PID of logger

  S = gen_rand_str(8),
  H = hash_str(S),
  [Z | H2] = get_leading_zeros(H, K),
  if
    H2 == Z ->
      LogID ! {S, H};
    true ->
      worker(K, LogID)
  end.

% Spawns the required number of workers
spawner(0, _K, _LogID) -> io:fwrite("Spawned All workers\n");
spawner(W, K, LogID) ->
  % Args:
  % W: No of workers, each worker will keep mining until a bitcoin is found
  % K: No of leading zeroes required
  % LogID: PID of logger

  spawn(fun() -> worker(K, LogID) end),
  spawner(W-1, K, LogID).

% Bitcoin logger used to display the coins mined by the server and benchmark
bitcoinLogger(0) ->
  io:fwrite("Completed Mining\n"),
  {_, CPUTime} = statistics(runtime),
	{_, RunTime} = statistics(wall_clock),

  % Benchmark
	io:format("CPU time: ~p s\n", [CPUTime/1000]),
	io:format("real time: ~p s\n", [RunTime/1000]),
	io:format("Ratio: ~p \n", [CPUTime/RunTime]),
	exit(self(),kill);

bitcoinLogger(W) ->
  % Args:
  % W: No of workers, each worker will keep mining until a bitcoin is found

  receive
    {S, H} ->
      io:fwrite("Server Found: ~p ~p\n", [S, H]),
      bitcoinLogger(W-1);

    {S, H, ID} ->
      io:fwrite("Client ~p Found: ~p ~p\n", [ID, S, H]),
      bitcoinLogger(W-1)
    end.
