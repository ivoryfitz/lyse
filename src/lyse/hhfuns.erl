%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 9:19 AM
%%%-------------------------------------------------------------------
-module(hhfuns).
-author("matt_fitz").

%% API
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H - 1 | decrement(T)].

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

base(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().

base() ->
  A = 1,
  fun(A) -> A = 2 end(2).

even(L) -> lists:reverse(even(L, [])).
even([], Acc) -> Acc;
even([H | T], Acc) when H rem 2 == 0 ->
  even(T, [H | Acc]);
even([_ | T], Acc) ->
  even(T, Acc).

filter(F, L) ->
  lists:reverse(filter(F, L, [])).
filter(_, [], Acc) -> Acc;
filter(F, [H | T], Acc) ->
  case F(H) of
    true -> filter(F, T, [H | Acc]);
    false -> filter(F, T, Acc)
  end.

max([H | T]) -> max2(T, H).
max2([H | T], Max) when H > Max -> max2(T, H);
max2([], Max) -> Max;
max2([_ | T], Max) -> max2(T, Max).

sum(L) -> sum(L, 0).
sum([H|T], Acc) -> sum(T, Acc + H);
sum([], Acc) -> Acc.
