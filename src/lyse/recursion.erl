%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% Created : 03. Mar 2014 11:33 AM
%%%-------------------------------------------------------------------
-module(recursion).
-author("matt_fitz").

%% API
-compile(export_all).

duplicate(N, Term) ->
  duplicate(N, Term, []).
duplicate(0, _, List) ->
  List;
duplicate(N, Term, List) ->
  duplicate(N - 1, Term, [Term | List]).

tailduplicate(N, Term) ->
  tailduplicate(N, Term, []).
tailduplicate(0, _, Acc) ->
  Acc;
tailduplicate(N, Term, Acc) ->
  tailduplicate(N - 1, Term, [Term | Acc]).

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

tailfac(N) -> tailfac(N, 1).
tailfac(0, Acc) -> Acc;
tailfac(N, Acc) -> tailfac(N - 1, N * Acc).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

taillen([], Acc) -> Acc;
taillen([_ | T], Acc) -> taillen(T, Acc + 1).
taillen(List) -> taillen(List, 0).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

tailreverse(List) ->
  tailreverse(List, []).
tailreverse([], ReversedList) ->
  ReversedList;
tailreverse([H | T], ReversedList) ->
  tailreverse(T, [H | ReversedList]).

sublist([], _) -> [];
sublist(_, 0) -> [];
sublist([H | T], N) when N > 0 ->
  [H | sublist(T, N - 1)].


tailsublist(L, N) ->
  tailsublist(L, N, []).
tailsublist(_, 0, Acc) -> Acc;
tailsublist([], _, Acc) -> Acc;
tailsublist([H | T], N, Acc) when N > 0 ->
  tailsublist(T, N - 1, Acc ++ [H]).

strictzip(List1, List2) when length(List1) =/= length(List2) ->
  io:format("Please Zip Two Lists Of Same Length");
strictzip([], []) -> [];
strictzip(List1, List2) when length(List1) =:= length(List2) ->
  [H1 | T1] = List1,
  [H2 | T2] = List2,
  [{H1, H2} | strictzip(T1, T2)].

tailstrictzip(List1, List2) when length(List1) =/= length(List2) ->
  io:format("Please Zip Two Lists Of Same Length");
tailstrictzip(List1, List2) when length(List1) =:= length(List2) ->
  reverse(tailstrictzip(List1, List2, [])).
tailstrictzip([], [], ZippedList) -> ZippedList;
tailstrictzip([X | Xs], [Y | Ys], ZippedList) ->
  tailstrictzip(Xs, Ys, [{X, Y} | ZippedList]).

lazyzip([], _) -> [];
lazyzip(_, []) -> [];
lazyzip(List1, List2) ->
  [H1 | T1] = List1,
  [H2 | T2] = List2,
  [{H1, H2} | lazyzip(T1, T2)].

taillazyzip(ListX, ListY) ->
  reverse(taillazyzip(ListX, ListY, [])).
taillazyzip([], _, Zipped) -> Zipped;
taillazyzip(_, [], Zipped) -> Zipped;
taillazyzip([X | Xs], [Y | Ys], Zipped) ->
  taillazyzip(Xs, Ys, [{X, Y} | Zipped]).

quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
  {Smaller, Larger} = partition(Pivot, Rest, [], []),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};

partition(Pivot, [H | T], Smaller, Larger) when H > Pivot ->
  partition(Pivot, T, [H | Smaller], Larger);

partition(Pivot, [H | T], Smaller, Larger) when H =< Pivot ->
  partition(Pivot, T, Smaller, [H | Larger]).

%% Uses listcomprehension. Readability++, Performance--
lc_quicksort([]) -> [];
lc_quicksort([Pivot | Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
    lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
