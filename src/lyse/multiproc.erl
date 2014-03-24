%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 2:49 PM
%%%-------------------------------------------------------------------
-module(multiproc).
-author("matt_fitz").

%% API
important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
    after 0 ->
      normal()
  end.

normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
    after 0 ->
      []
  end.
