%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 1:23 PM
%%%-------------------------------------------------------------------
-module(records).
-author("matt_fitz").

%% API
-compile(export_all).

-include("records.hrl").

-record(robot, {
  name,
  type = industrial,
  hobbies,
  details = []}).

-record(user, {id, name, group, age}).

admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
  Name ++ " is not allowed!".

adult_section(U) when U#user.age >= 18 ->
  allowed;
adult_section(_) ->
  forbidden.

first_robot() ->
  #robot{
    name = "Mechatron",
    type = handmade,
    details = ["Moved bya small man inside"]
  }.

car_factory(CorpName) ->
  #robot{name=CorpName, hobbies="building cars"}.

repairman(Rob) ->
  {repaired, Rob#robot{details=["Repaired by repairman" | Rob#robot.details]}}.

included() -> #included{some_field = "SomeValue"}.
