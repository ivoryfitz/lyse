%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(eb_server).

-behaviour(gen_server).

%% API
-export([start_link/0, create_account/2, deposit/3, cancel_account/2, withdraw/3, balance/2, authorize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_account(Name, PIN) ->
  gen_server:cast(?SERVER, {create, Name, PIN}).

deposit(Name, PIN, Amount) ->
  gen_server:call(?SERVER, {deposit, Name, PIN, Amount}).

cancel_account(Name, PIN) ->
  gen_server:call(?SERVER, {cancel, Name, PIN}).

withdraw(Name, PIN, Amount) ->
  gen_server:call(?SERVER, {withdraw, Name, PIN, Amount}).

balance(Name, PIN) ->
  gen_server:call(?SERVER, {balance, Name, PIN}).

authorize(Name, PIN) ->
  gen_server:call(?SERVER, {authorize, Name, PIN}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
  eb_event_manager:add_handler(eb_withdrawal_handler),
  {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({authorize, Name, PIN}, _From, State) ->
  case access_balance(Name, PIN, State) of
    {ok, _Balance} ->
      {reply, ok, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call({deposit, Name, PIN, Amount}, _From, State) ->
  case access_balance(Name, PIN, State) of
    {ok, Balance} ->
      NewBalance = Balance + Amount,
      Response = {ok, NewBalance},
      NewState = dict:store(Name, {PIN, NewBalance}, State),
      {reply, Response, NewState};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call({withdraw, Name, PIN, Amount}, _From, State) ->
  case access_balance(Name, PIN, State) of
    {ok, Balance} ->
      NewBalance = Balance - Amount,
      if
        NewBalance < 0 ->
          {reply, {error, account_balance_too_low}, State};
        true ->
          eb_event_manager:notify({withdraw, Name, Amount, NewBalance}),
          {reply, {ok, NewBalance}, dict:store(Name, {PIN, NewBalance}, State)}
      end;
    {error, Reason} ->
      {reply, {error, Reason}}
  end;
handle_call({cancel, Name, PIN}, _From, State) ->
  case access_balance(Name, PIN, State) of
    {ok, Balance} ->
      {reply, {ok, Balance}, dict:erase(Name, State)};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call({balance, Name, PIN}, _From, State) ->
  case access_balance(Name, PIN, State) of
    {ok, Balance} ->
      {reply, {ok, Balance}, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, Name, PIN}, State) ->
  {noreply, dict:store(Name, {PIN, 0}, State)};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
access_balance(Name, PIN, Accounts) ->
  case dict:find(Name, Accounts) of
    {ok, {PIN, Balance}} ->
      {ok, Balance};
    {ok, _Value} ->
      {error, invalid_pin};
    error ->
      {error, account_not_found}
  end.
