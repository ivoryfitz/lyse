%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2014 2:07 PM
%%%-------------------------------------------------------------------
-module(time).
-author("matt_fitz").

%% API
-export([swedish_date/0]).

%% Return string containing date YYMMDD

swedish_date() ->
  swedish_date(lists:map(fun(X) -> integer_to_list(X) end, tuple_to_list(date()))).

swedish_date(DateList) ->
  swedish_date(DateList, "").

swedish_date([H | T], []) ->
  swedish_date(T, parse_value(H));
swedish_date([H | T], Acc) ->
  swedish_date(T, Acc ++ parse_value(H));
swedish_date([], Acc) -> Acc.

parse_value(Val) when length(Val) =:= 2 ->
  Val;
parse_value(Val) when length(Val) < 2 ->
  "0" ++ Val;
parse_value(Val) when length(Val) > 2 ->
  lists:sublist(Val, 3, 2).


