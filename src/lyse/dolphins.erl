%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% Created : 03. Mar 2014 2:02 PM
%%%-------------------------------------------------------------------
-module(dolphins).
-author("matt_fitz").

%% API
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("How about no?~n");
    fish ->
      io:format("So long and thanks for all the fish!~n");
    _ ->
      io:format("Heh~n")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?~n";
    {From, fish} ->
      From ! "So long and thanks for all the fish!~n";
    _ ->
      io:format("Heh~n")
  end.

dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?~n",
      dolphin3();
    {From, fish} ->
      From ! "So long and thanks for all the fish!~n";
    _ ->
      io:format("Heh~n"),
      dolphin3()
  end.
