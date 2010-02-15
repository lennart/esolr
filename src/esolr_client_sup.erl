-module(esolr_client_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).


start_link(Options) ->
  supervisor:start_link(esolr_client_sup, Options).

init(Options) ->
  {ok,{{one_for_one, 5, 60},
      [{esolr_client, {esolr_client, start_link, [Options]},
          permanent, 10, worker, [esolr_client]}]}}. 
