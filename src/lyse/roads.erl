%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% Created : 03. Mar 2014 1:28 PM
%%%-------------------------------------------------------------------
-module(roads).
-author("matt_fitz").

%% API
-compile(export_all).

main() ->
  File = "road.txt",
  {ok, Bin} = file:read_file(File),
  RoadTuples = lists:reverse(group_vals(parse_map(Bin))).

parse_map(Bin) when is_binary(Bin) ->
  parse_map(binary_to_list(Bin));
parse_map(List) when is_list(List) ->
  [list_to_integer(X) || X <- string:tokens(List, "\r\t\n ")].

group_vals(RoadVals) -> group_vals(RoadVals, []).
group_vals([], Acc) -> Acc;
group_vals([A, B, X | Rest], Acc) ->
  group_vals(Rest, [{A, B, X} | Acc]).


%% Fastest to A1 from A = [B, X] 40 {{40, [B, X]}, {10, [B]}}
%% Fastest to B2 from B = [B] 10

shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
  OptA1 = {}

