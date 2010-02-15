-module(esolr_client).

-behaviour(gen_server).
-include("esolr.hrl").


-export([start_link/1]).

%%gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


%% @doc same as start_link([]) 
%start_link() ->
%	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

% @see start/1
start_link(Options) ->
	gen_server:start_link({local,?MODULE},esolr_client,Options,[]).

%%----------------------gen_server callbacks ------------------------


code_change(_OldVsn,State,_Extra)	->
	{ok,State}.

% @hidden
timeout_value(Key,Max,Default,Options) ->
	case lists:keysearch(Key,1,Options) of
		{value,{Key,N}} when N =< Max -> 
			N;
		{value,{Key,N}} when N >  Max -> 
			throw({invalid_timeout,{Key,too_big,N}});
		false -> 
			Default
	end.

% @hidden
init(Options) ->
	SelectUrl = case lists:keysearch(select_url,1,Options) of
					{value,{select_url,S}} -> S;
					false -> ?ESOLR_DEFAULT_SELECTURL
				end,
	UpdateUrl = case lists:keysearch(update_url,1,Options) of
					{value,{update_url,U}} -> U;
					false -> ?ESOLR_DEFAULT_UPDATEURL
				end,
	AddTimeout = timeout_value(add_timeout,?ESOLR_MAX_ADD_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	CommitTimeout = timeout_value(commit_timeout,?ESOLR_MAX_COMMIT_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	OptimizeTimeout = timeout_value(optimize_timeout,?ESOLR_MAX_OPTIMIZE_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	SearchTimeout = timeout_value(search_timeout,?ESOLR_MAX_SEARCH_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
				
	inets:start(),
	{ok,#esolr{update_url=UpdateUrl,search_url = SelectUrl, 
		add_timeout=AddTimeout,
		commit_timeout=CommitTimeout,
		optimize_timeout=OptimizeTimeout,
		search_timeout=SearchTimeout,
		pending=gb_trees:empty()}}.


% @hidden
%%Cancel previous timer if exists
handle_call(R={set_auto_commit,_Mode},From,State=#esolr{auto_commit={time,TRef}}) ->
	timer:cancel(TRef),
	handle_call(R,From,State#esolr{auto_commit=false});
%%create new timer if neccesary
handle_call({set_auto_commit,{time,N}},_From,State) ->
	{ok,TRef} = timer:send_interval(N,auto_commit),
	{reply,ok,State#esolr{auto_commit={time,TRef}}};
%%no need to setup timer	
handle_call({set_auto_commit,Mode},_From,State) ->
	{reply,ok,State#esolr{auto_commit=Mode}};
	
handle_call(R={set_auto_optimize,_Mode},From,State=#esolr{auto_optimize={time,TRef}}) ->
	timer:cancel(TRef),
	handle_call(R,From,State#esolr{auto_optimize=false});
%%create new timer if neccesary
handle_call({set_auto_optimize,{time,N}},_From,State) ->
	{ok,TRef} = timer:send_interval(N,auto_optimize),
	{reply,ok,State#esolr{auto_optimize={time,TRef}}};
%%no need to setup timer	
handle_call({set_auto_optimize,Mode},_From,State) ->
	{reply,ok,State#esolr{auto_optimize=Mode}};
		


handle_call({add,Docs},From,State=#esolr{add_timeout=T}) ->
	Request = encode_add(Docs),
	make_post_request(Request,{From,add},State#esolr{dirty=true},T);
	
handle_call(commit,From,State=#esolr{commit_timeout=T}) ->	
	Request = encode_commit(),
	make_post_request(Request,{From,commit},State#esolr{dirty=false},T);
	

handle_call({delete,Del},From,State=#esolr{delete_timeout=T}) ->	
	Request = encode_delete(Del),
	make_post_request(Request,{From,delete},State#esolr{dirty=true},T);

handle_call(optimize,From,State=#esolr{optimize_timeout=T}) ->	
	Request = encode_optimize(),
	make_post_request(Request,{From,optimize},State,T);
	
handle_call({search,Query,Options},From,State=#esolr{search_url=URL,pending=P,search_timeout=Timeout}) ->	
	RequestParams = encode_search(Query,Options),
	SearchURL = lists:flatten([URL,"?wt=json&"|RequestParams]),
	{ok,RequestId} = http:request(get,{SearchURL,[]},[{timeout,Timeout}],[{sync,false}]),
	Pendings = gb_trees:insert(RequestId,{From,search},P),
	{noreply,State#esolr{pending=Pendings}};

handle_call(stop,_From,State) ->	
	{stop,normal,ok,State}.
	

make_post_request(Request,PendingInfo,State=#esolr{update_url=URL,pending=P,auto_commit=AC,dirty=Dirty},Timeout) ->
	{ok,RequestId} = http:request(post,{URL,[],"text/xml",Request},[{timeout,Timeout}],[{sync,false}]),
	Pendings = gb_trees:insert(RequestId,PendingInfo,P),
	if 
		(AC == always) and Dirty ->  
				  CommitRequest = encode_commit(),
				  {ok,C_RequestId} = http:request(post,{URL,[],"text/xml",CommitRequest},
				  					     [{timeout,State#esolr.commit_timeout}],[{sync,false}]),
				  Pendings2 = gb_trees:insert(C_RequestId,{auto,auto_commit},Pendings),
				  error_logger:info_report([{auto_commit,send}]),
			  	  {noreply,State#esolr{pending=Pendings2,dirty=false}};
		
		true -> {noreply,State#esolr{pending=Pendings}}
	end.



% @hidden
handle_cast(_Request,State) ->
	{noreply,State}.


 
 
% @hidden
handle_info({http,{RequestId,HttpResponse}},State = #esolr{pending=P}) ->
	case gb_trees:lookup(RequestId,P) of
		{value,{Client,RequestOp}} -> handle_http_response(HttpResponse,RequestOp,Client),
						 {noreply,State#esolr{pending=gb_trees:delete(RequestId,P)}};
		none -> {noreply,State}
				%% the requestid isn't here, probably the request was deleted after a timeout
	end;


handle_info(auto_commit,State = #esolr{dirty=true,commit_timeout=T}) ->
	Request = encode_commit(),
	R = make_post_request(Request,{auto,auto_commit},State#esolr{dirty=false},T),
	error_logger:info_report([{auto_commit,send}]),
	R;
	
	
	
handle_info(auto_commit,State = #esolr{dirty=false}) ->	
	{noreply,State};
	
handle_info(auto_optimize,State) ->
	Request = encode_optimize(),
	R = make_post_request(Request,{auto,auto_optimize},State,State#esolr.optimize_timeout),
	error_logger:info_report([{auto_optimize,send}]),
	R.

	

 
% @hidden
terminate(_Reason,_State) ->
	ok.
% @hidden	

%%----------------------internal functions ------------------------	

handle_http_response({error,HttpError},RequestOp,Client) ->
	response_error(RequestOp,Client,HttpError);
	
%%search response are in json format
handle_http_response({{_HttpV,200,_Reason},_Headers,Data},search,Client) ->
	{ok,{obj,Response},[]} = rfc4627:decode(Data),
	{value,{"responseHeader",{obj,Headers}},RestResponse} = lists:keytake("responseHeader",1,Response),
	{value,{"status",Status}} = lists:keysearch("status",1,Headers),
	case Status of
		0 -> parse_search_response(RestResponse,Client); 
		N -> response_error(search,Client,N)
	end;
 	
handle_http_response({{_HttpV,200,_Reason},_Headers,Data},Op,Client) ->
	{Response,[]} = xmerl_scan:string(binary_to_list(Data)),
	[Header] = xmerl_xpath:string("/response/lst[@name='responseHeader']",Response),
	case parse_xml_response_header(Header) of
		{ok,QTime} ->  parse_xml_response(Op,Response,QTime,Client);
		{error,Error} ->  response_error(Op,Client,Error)
	end;
	
 	
	
handle_http_response({{_HttpV,StatusCode,Reason},_Headers,_Data},_Op,Client) ->	
	error_logger:error_report({"unrecognized response status",StatusCode,Reason}),
	gen_server:reply(Client,{error,{status_code,StatusCode,Reason}}).	
	
	
response_error(auto_commit,auto,Error) ->
	error_logger:error_report([{auto_commit_error,Error}]);
	
 
response_error(auto_optimize,auto,Error) ->
	error_logger:error_report([{auto_optimize_error,Error}]);
	

response_error(_Op,Client,Error) ->
 	gen_server:reply(Client,{error,Error}).
	
	
	
parse_search_response(Response,Client) ->
	{value,{"response",{obj,SearchRespFields}},RestResponse} = lists:keytake("response",1, Response),
	{value,{"docs",Docs},RespFields} =  lists:keytake("docs",1,SearchRespFields),
	gen_server:reply(Client,{ok,RespFields,[{doc,DocFields} || {obj,DocFields}<-Docs],RestResponse}).
	
	
	
parse_xml_response(Op,_Response,_QTime,Client) when Op == add ;
											   Op == commit;
											   Op == optimize;
											   Op == delete	->
	gen_server:reply(Client,ok);
	
	
parse_xml_response(auto_commit,_Response,QTime,auto) ->
	error_logger:info_report([{auto_commit,QTime}]);
	
parse_xml_response(auto_optimize,_Response,QTime,auto) ->
	error_logger:info_report([{auto_optimize,QTime}]).
	
parse_xml_response_header(Header) ->
	[#xmlText{value=V}] = xmerl_xpath:string("/lst/int[@name='status']/text()",Header),
	case list_to_integer(V) of
		?ESOLR_STATUS_OK ->  [#xmlText{value=V1}] = xmerl_xpath:string("/lst/int[@name='QTime']/text()",Header),
					         {ok,list_to_integer(V1)};
		Other -> {error,Other}
		
	end.
		
		
	
	
encode_search(Query,Options) ->
	S = [["q=",url_encode(Query)] | lists:map(fun encode_search_option/1,Options)],
	string:join(S,"&").
	
	
encode_search_option({fields,Fields}) ->
	["fl=",url_encode(Fields)];
	
encode_search_option({start,Start}) ->
	["start=",integer_to_list(Start)];

encode_search_option({count,Count}) ->	
	["count=",integer_to_list(Count)];
	
encode_search_option({sort,SortFields}) ->		
	S = [ [atom_to_list(Name), "+", atom_to_list(Order)] || {Name,Order} <- SortFields],
	["sort=",string:join(S,",")];


encode_search_option({highlight,Highlight}) ->
	["hl=on&hl.fl=",url_encode(Highlight)].


encode_delete({id,Id})->
	iolist_to_binary(xmerl:export_simple([{delete,[],[{id,[],[Id]}]}],xmerl_xml));

encode_delete({q,Query})->
	iolist_to_binary(xmerl:export_simple([{delete,[],[{'query',[],[Query]}]}],xmerl_xml)).
	
encode_commit() ->
	iolist_to_binary(xmerl:export_simple([{commit,[]}],xmerl_xml)).
	
encode_optimize() ->
	iolist_to_binary(xmerl:export_simple([{optimize,[]}],xmerl_xml)).
	
	
encode_add(Docs) ->
	Doc = {add,[],lists:map(fun encode_doc/1,Docs)},
	iolist_to_binary(xmerl:export_simple([Doc],xmerl_xml)).
	
encode_doc({doc,Fields}) ->
	{doc,[],lists:map(fun encode_field/1,Fields)};

encode_doc({doc,Boost,Fields}) ->
	{doc,[{boost,Boost}],lists:map(fun encode_field/1,Fields)}.
	
	
encode_field({Name,Value}) when is_binary(Value)->
	{field,[{name,Name}],[[Value]]};

encode_field({Name,Value}) ->
	{field,[{name,Name}],[Value]};
	
encode_field({Name,Value,Boost}) when is_binary(Value)->
	{field,[{name,Name},{boost,Boost}],[[Value]]};

encode_field({Name,Value,Boost}) ->
	{field,[{name,Name},{boost,Boost}],[Value]}.



  	
%%%
% URL encode - borrowed from CouchDB
% borrowed again from http://weblog.plexobject.com/?p=1594
%%%
url_encode([H|T]) ->
    if
         H >= $a, $z >= H ->
             [H|url_encode(T)];
         H >= $A, $Z >= H ->
             [H|url_encode(T)];
         H >= $0, $9 >= H ->
             [H|url_encode(T)];
         H == $_; H == $.; H == $-; H == $: ->
             [H|url_encode(T)];
         true ->
             case lists:flatten(io_lib:format("~.16.0B", [H])) of
                 [X, Y] ->
                     [$%, X, Y | url_encode(T)];
                 [X] ->
                     [$%, $0, X | url_encode(T)]
             end
     end;
url_encode([]) ->
     [].
 
