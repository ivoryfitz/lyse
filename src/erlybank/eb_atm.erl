%%%-------------------------------------------------------------------
%%% File    : eb_atm.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ATM backend for ErlyBank
%%%
%%% Created :  6 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(eb_atm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, unauthorized/3, authorized/2, thank_you/2, authorize/2, deposit/1, withdraw/1, cancel/0, balance/0, authorized/3, thank_you/3, unauthorized/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

authorize(Name, PIN) ->
  gen_fsm:sync_send_event(?SERVER, {authorize, Name, PIN}).

deposit(Amount) ->
  gen_fsm:send_event(?SERVER, {deposit, Amount}).

withdraw(Amount) ->
  gen_fsm:send_event(?SERVER, {withdraw, Amount}).

cancel() ->
  gen_fsm:send_all_state_event(?SERVER, cancel).

balance() ->
  gen_fsm:sync_send_event(?SERVER, balance).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([]) ->
  {ok, unauthorized, {nobody, nopin}}.

%% States - state/2 = async, state/3 = sync
%% Really good idea for both async/sync to provide default's for each state
%% This will prevent dumb crashes
authorized({deposit, Amount}, {Name, PIN} = State) ->
  eb_server:deposit(Name, PIN, Amount),
  {next_state, thank_you, State, 5000};
authorized({withdraw, Amount}, {Name, PIN} = State) ->
  eb_server:withdraw(Name, PIN, Amount),
  {next_state, thank_you, State, 5000};
authorized(_Event, State) ->
  {next_state, authorized, State}.

authorized(balance, _From, {Name, PIN} = State) ->
  Reply = eb_server:balance(Name, PIN),
  {reply, Reply, thank_you, State};
authorized(_Event, _From, State) ->
  {reply, {error, invalid_message}, authorized, State}.

thank_you(timeout, _State) ->
  {next_state, unauthorized, {nobody, nopin}};
thank_you(_Event, _State) ->
  {next_state, unauthorized, {nobody, nopin}}.

thank_you(_Event, _From, _State) ->
  {reply, {error, invalid_message}, unauthorized, {nobody, nopin}}.

unauthorized(_Event, State) ->
  {next_state, unauthorized, State}.

unauthorized({authorize, Name, PIN}, _From, State) ->
  case eb_server:authorize(Name, PIN) of
    ok ->
      {reply, ok, authorized, {Name, PIN}};
    {error, Reason} ->
      {reply, {error, Reason}, unauthorized, State}
  end;
unauthorized(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, unauthorized, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(cancel, _StateName, _State) ->
  {next_state, unauthorized, {nobody, nopin}};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
