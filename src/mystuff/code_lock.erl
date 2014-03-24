%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2014 4:30 PM
%%%-------------------------------------------------------------------
-module(code_lock).
-author("matt_fitz").
-behaviour(gen_fsm).

%% API
-export([start_link/1, button/1]).

%% gen_fsm Callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state callbacks
-export([locked/3, unlocked/3, relock/0]).

%% API
start_link(Code) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, lists:reverse(Code), []).

button(Digit) ->
  gen_fsm:sync_send_event(?MODULE, {button, Digit}).

relock() ->
  gen_fsm:sync_send_event(?MODULE, lock).

%% Sync States
locked({button, Digit}, _From, {SoFar, Code}) ->
  case [Digit | SoFar] of
    Code ->
      {reply, {ok, unlocked}, unlocked, {[], Code}};
    Incomplete when length(Incomplete) < length(Code) ->
      io:format("SoFar: ~p~n Code: ~p~n", [Incomplete, Code]),
      {reply, {ok, incomplete}, locked, {Incomplete, Code}};
    _Wrong ->
      {reply, {error, wrong_code}, locked, {[], Code}}
  end;
locked(_Event, _From, State) ->
  io:format("Unrecognized Message while locked~n"),
  {reply, {error, invalid_message}, locked, State}.

unlocked(lock, _From, State) ->
  {reply, {ok, locking_same_code}, locked, State};
unlocked(_Event, _From, State) ->
  {reply, {error, invalid_message}, unlocked, State}.


%% Callbacks
init(Code) ->
  {ok, locked, {[], Code}}.

handle_event(Event, StateName, StateData) ->
  erlang:error(not_implemented).

handle_sync_event(Event, From, StateName, StateData) ->
  erlang:error(not_implemented).

handle_info(Info, StateName, StateData) ->
  erlang:error(not_implemented).

terminate(Reason, StateName, StateData) ->
  io:format("Terminating...~n"),
  io:format("Reason: ~p~n StateName: ~p~n StateData: ~p~n", [Reason, StateName, StateData]),
  ok.

code_change(OldVsn, StateName, StateData, Extra) ->
  erlang:error(not_implemented).

