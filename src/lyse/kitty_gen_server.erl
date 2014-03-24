%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2014 12:48 PM
%%%-------------------------------------------------------------------
-module(kitty_gen_server).
-behaviour(my_gen_server).
-author("matt_fitz").

%% API
-compile(export_all).
-record(cat, {name, color = green, description}).

start_link() -> my_gen_server:start_link(?MODULE, [], []).

%% synchronous
order_cat(Pid, Name, Color, Description) ->
  my_gen_server:call(Pid, {order, Name, Color, Description}).

%% asynchrnous
return_cat(Pid, Cat = #cat{}) ->
  my_gen_server:cast(Pid, {return, Cat}).

%% synchronous
close_shop(Pid) ->
  my_gen_server:call(Pid, terminate).

init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if
    Cats =:= [] -> {reply, make_cat(Name, Color, Description), Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

make_cat(Name, Col, Desc) ->
  #cat{name = Name, color = Col, description = Desc}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
