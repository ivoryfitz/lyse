%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2014 1:11 PM
%%%-------------------------------------------------------------------
-module(cat_fsm).
-author("matt_fitz").

%% API
-compile(export_all).

start() ->
  spawn(fun() -> dont_give_crap() end).



event(Pid, Event) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Event},
  receive
    {Ref, Msg} -> {ok, Msg}
  after 5000 ->
    {error, timeout}
  end.

dont_give_crap() ->
  receive
    {Pid, Ref, _Msg} -> Pid ! {Ref, meh};
    _ -> ok
  end,
  io:format("Switching to `dont_give_crap` state~n"),
  dont_give_crap().
