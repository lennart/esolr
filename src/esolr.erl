%% esolr -  Erlang client library for the apache Solr search server
%%---------------------------------------------------------------------------
%% Copyright (c) 2002 Pablo Polvorin <ppolv@yahoo.com.ar>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
% @author Pablo Polvorin <ppolv@yahoo.com.ar>
% @version alpha, really
% @doc <b>esolr</b> , a http/xml client library to the apache Solr search server.
-module(esolr).
-behaviour(application).

-include("esolr.hrl").

%%Application Callbacks
-export([start/2,stop/1]).

%%API
-export([add/1,search/2,delete/1,commit/0,optimize/0,set_auto_optimize/1,set_auto_commit/1,escape_solr_query_fragment/1]).




%%-----------------API---------------------------------------
% @doc same as start([])
start(_Type, Options) ->
  esolr_client_sup:start_link(Options).

stop(_State) ->
  ok.

%start() ->
%	start([]).
%
%
%% @doc start the esolr process
%% 
%%      If not specified, the default url used for search is "http://localhost:8983/solr/select", 
%%      and for updates http://localhost:8983/solr/update 
%% @spec start(Options::[Option]) -> Result
%%       Option = {search_url,URL}|{update_url,URL}| {add_timeout,integer()} | {search_timeout,integer()} 
%%                |{delete_timeout,integer()} | {commit_timeout,integer()} | {optimize_timeout,integer()}
%%       URL = string()
%start(Options) ->
%	esolr_client_sup:start_link({local,?MODULE},?MODULE,Options).


%init({module, Module}) ->
%    {ok,
%        {_SupFlags = {simple_one_for_one, ?ESOLR_MAX_RESTART, ?ESOLR_MAX_TIME},
%            [
%              % HTTP Client
%              {   undefined,                               % Id       = internal id
%                  {Module,start_link,[]},                  % StartFun = {M, F, A}
%                  temporary,                               % Restart  = permanent | transient | temporary
%                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
%                  worker,                                  % Type     = worker | supervisor
%                  []                                       % Modules  = [Module] | dynamic
%              }
%            ]
%        }
%    };
%	
%init(Options) ->
%  {ok,
%        {_SupFlags = {one_for_one, ?ESOLR_MAX_RESTART, ?ESOLR_MAX_TIME},
%            [
%              % eCouch Listener
%              {   es_client_sup,                         % Id       = internal id
%                {supervisor,start_link,[{local,es_client_sup},?MODULE,[{module, esolr_client}]]},   % StartFun = {M, F, A}
%                  permanent,                               % Restart  = permanent | transient | temporary
%                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
%                  supervisor,                                  % Type     = worker | supervisor
%                  [esolr_client]                            % Modules  = [Module] | dynamic
%              }
%            ]
%        }
%    }.
%
%
%	
%	


% @doc add the given documents to the index
%	
% @spec add(Docs::Documents) -> Result
% Documents = [Doc]
% Doc = {doc,Fields}
% Fields = [Field]	
% Field = {Name,Value}
% Name = atom()
% Value = IOString
add(Docs) ->
	gen_server:call(esolr_client,{add,Docs},?ESOLR_MAX_ADD_TIMEOUT).
	
% @doc search the index
%
%      AdditionalInfo contains additional information present in the response 
%      not directly parsed by esolr, like highlight info
%
% @spec search(Query::Query,Options::SearchOptions) -> SearchResult 	
% Query = string()
% SearchOptions = [SearchOption]
% SearchOption = {fields,SearchFields} | {start,StartRow} | {count,Count} | {sort,[SortSpecification]} | {highlight,HihhligthFields}
% SearchFields = string()
% HihhligthFields = string()
% StartRow = integer()
% Count = integer()
% SortSpecification = {Name,Sort}
% Sort = asc | desc
% SearchResult = {ok,RespAttrs,Docs,AdditionalInfo} | {error,Reason}
% RespAttrs = [{Name,Value}]
% Docs = [{doc,Fields}]
% Fields = [{Name,Value}]
% Name = atom()
% Value = IOString
% AdditionalInfo = term()  
search(Query,Options) ->
	gen_server:call(esolr_client,{search,Query,Options},?ESOLR_MAX_SEARCH_TIMEOUT).
	
	
% @doc  delete one or more documents. 
% @spec delete(Del::Delete) -> Response
%       Delete = {id,Id} | {q,Query}
% 		Id = string()
% 		Query = string()
%		Response = ok | {error,Reason}
delete(Del) ->	
	gen_server:call(esolr_client,{delete,Del},?ESOLR_MAX_DELETE_TIMEOUT).


% @doc  send a "commit" command to the server
% @see set_auto_commit/1
commit() ->
	gen_server:call(esolr_client,commit,?ESOLR_MAX_COMMIT_TIMEOUT).

% @doc  send a "optimize" command to the server
%       
% @see set_auto_optimize/1
optimize() ->
	gen_server:call(esolr_client,optimize,infinity).

% @doc  stop the esolr process
stop() ->
	gen_server:call(esolr_client,stop,infinity).

% @doc  sets the autocommit behavior of the library. 
%
%       {time,N} to do an automatic commit every N miliseconds, if there are uncommited updates <br/>
% 		always :   esolr will automatically commit after each update.     <br/>
% 		false :    esolr won't commit automatically. 
% @spec set_auto_commit(AutoCommitMode::Mode) ->ok
%       Mode = false | always | {time,integer()}
set_auto_commit(AutoCommitMode)  ->
	if 
		AutoCommitMode == false;
		AutoCommitMode == always; 
		is_tuple(AutoCommitMode) ->
			gen_server:call(esolr_client,{set_auto_commit,AutoCommitMode});
		true -> throw(bad_option)
	end.


escape_solr_query_fragment([H|T]) ->
  if
    H =< 32; H == $\\; H == $[; H == $]; H == $+ ; H == $- ; H == $!  ; H == $( ; H == $) ; H == $: ; H == $^ ; H == $[ ; H == $] ; H == '\"' ; H == ${ ; H == $} ; H == $~; H == $*; H == $?; H == $|; H == $&; H ==$; ->
      [$\\,$\\,H| escape_solr_query_fragment(T)];
    true ->
      [H | escape_solr_query_fragment(T)]

  end;
escape_solr_query_fragment([]) ->
  [].
	
	
	
% @doc  sets the auto optimize behavior of the library.
%
%		Similar to set_auto_commit/1, esolr can periodically send "optimize" commands to the server.
%
%       {time,N}: to do an automatic optimizet every N miliseconds <br/>
% 		false :    esolr won't send optimize commands automatically. <br/>
% @spec set_auto_optimize(AutoOptimizeMode::Mode) ->ok
%       Mode = false | {time,integer()}
set_auto_optimize(AutoOptimizetMode)->
	if 
		AutoOptimizetMode == false;
	    is_tuple(AutoOptimizetMode) ->
	    	gen_server:call(esolr_client,{set_auto_optimize,AutoOptimizetMode});
	    true -> throw(bad_option)
	end.
	    	



	
	
	
	
