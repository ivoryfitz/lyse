%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% Created : 03. Mar 2014 12:40 PM
%%%-------------------------------------------------------------------
-module(exceptions).
-author("matt_fitz").

%% API
-export([throws/1, exits/1, errors/1, sword/1, black_knight/1, catcher/2]).


throws(F) ->
  try F() of
    _ -> ok
  catch
    Throw -> {throw, caught, Throw}
  end.


errors(F) ->
  try F() of
    _ -> ok
  catch
    error:Error -> {error, caught, Error}
  end.

exits(F) ->
  try F() of
    _ -> ok
  catch
    exit:Exit -> {exit, caught, Exit}
  end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
  try Attack() of
    _ -> "None shall pass."
  catch
    throw:slice -> "It is but a scratch.";
    error:cut_arm -> "I've had worse.";
    exit:cut_leg -> "Come on you pansy!";
    _:_ -> "Just a flesh wound."
  end.


catcher(X, Y) ->
  case catch X/Y of
    {'EXIT', {badarith, _}} -> "uh oh";
    N -> N
  end.
