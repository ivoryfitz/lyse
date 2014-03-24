%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2014 11:49 AM
%%%-------------------------------------------------------------------
{application, erlybank, [
  {description, "Erlybank system."},
  {vsn, "1.0"},
  {modules, [eb_app, eb_sup, eb_server, eb_atm, eb_event_manager, eb_withdrawal_handler]},
  {registered, [eb_sup, eb_server, eb_atm, eb_event_manager]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {eb_app, []}}
]}.
