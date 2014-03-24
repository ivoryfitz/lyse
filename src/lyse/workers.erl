%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 2:02 PM
%%%-------------------------------------------------------------------
-module(workers).
-author("matt_fitz").

%% API
-compile(export_all).

worker(Number) ->
  receive
    {Master, Task} ->
      Result = Task(),
      Master ! {workDone, Number, Result};
    _ ->
      throw(unrecognizedMessage)
  end.
