%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2014 10:31 AM
%%%-------------------------------------------------------------------
-module(curling_feed).
-behaviour(gen_event).
-author("matt_fitz").

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init([Pid]) -> {ok, Pid}.

handle_event(Event, Pid) ->
  Pid ! {curling_feed, Event},
  {ok, Pid}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
