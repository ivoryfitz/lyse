%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2014 4:16 PM
%%%-------------------------------------------------------------------
-module(curling_scoreboard_hw).
-author("matt_fitz").

%% API
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).


set_teams(TeamA, TeamB) ->
  io:format("Scorebard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
  io:format("Scoreboard: round over~n").

add_point(Team) ->
  io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).

reset_board() ->
  io:format("Scoreboard: All teams are undefined and all scores are 0~n").
