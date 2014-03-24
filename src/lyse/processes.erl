%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2014 2:49 PM
%%%-------------------------------------------------------------------
-module(processes).
-author("matt_fitz").

%% API
-compile(export_all).

go() ->
  Pid2 = spawn(processes, loop, []),
  Pid2 ! {self(), hello},
  receive
    {Pid2, Msg} ->
      io:format("P1 ~w~n", [Msg])
  end,
  Pid2 ! stop.

loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true
  end.

%% PingPong
ping_pong() ->
  Pong = spawn(processes, pong, []),
  Ping = spawn(processes, ping, []),
  Ping ! {start, Pong, 10},
  io:format("I'm already done, rest of this shit is async~n").

ping() ->
  receive
    {start, Pong, N} ->
      io:format("The PingPong War Hath Begun, It shall last for ~p cycles~n", [N]),
      Pong ! {ping, self(), N - 1},
      ping();
    {pong, Pong, 0} ->
      io:format("Too...many....Pongs....bllearrgghhhh~n");
    {pong, Pong, N} ->
      io:format("The Pinger got Ponged! Time for Revenge!~n"),
      Pong ! {ping, self(), N - 1},
      ping()
  end.

pong() ->
  receive
    {ping, Ping, N} ->
      io:format("Damn that Pinger! I'll show him what for!~n"),
      Ping ! {pong, self(), N},
      pong()
  after 5000 ->
    io:format("I have won, bwhuahahah!~n")
  end.


%% Telephone Ring
start_telephone(N) ->
  Players = [spawn(processes, telephone_player, []) || _ <- lists:seq(1, N)],
  hd(Players) ! {send_along, Players, 1}.

telephone_player() ->
  receive
    {send_along, Players, N} when length(Players) =/= N ->
      SendTo = N + 1,
      io:format("~p got the message, sending it along to ~p~n", [N, SendTo]),
      lists:nth(SendTo, Players) ! {send_along, Players, SendTo},
      telephone_player();
    {send_along, Players, N} when length(Players) =:= N ->
      SendTo = 1,
      io:format("Last Player got the message~n"),
      lists:nth(SendTo, Players) ! {done, Players, SendTo},
      telephone_player();
    {done, Players, N} when length(Players) =/= N ->
      SendTo = N + 1,
      io:format("~p is done now, letting ~p know that they should be done~n", [N, SendTo]),
      lists:nth(SendTo, Players) ! {done, Players, SendTo};
    {done, Players, N} when length(Players) =:= N ->
      io:format("All done!~n")
  end.
