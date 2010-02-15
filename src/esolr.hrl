-define(ESOLR_DEFAULT_SELECTURL,"http://localhost:8983/solr/select").
-define(ESOLR_DEFAULT_UPDATEURL,"http://localhost:8983/solr/update").
-define(ESOLR_STATUS_OK,0).

-define(ESOLR_DEFAULT_TIMEOUT,10*1000).  %10sec
-define(ESOLR_MAX_RESTART, 5).
-define(ESOLR_MAX_TIME, 60).
-define(ESOLR_MAX_ADD_TIMEOUT,60*1000).   %1 min
-define(ESOLR_MAX_DELETE_TIMEOUT,60*1000).
-define(ESOLR_MAX_SEARCH_TIMEOUT,60*1000).
-define(ESOLR_MAX_COMMIT_TIMEOUT,120*1000). %2min
-define(ESOLR_MAX_OPTIMIZE_TIMEOUT,120*1000).

-include_lib("xmerl/include/xmerl.hrl").

-record(esolr,
	{update_url,   
	 search_url,
	 add_timeout,	
	 delete_timeout,
	 commit_timeout,
	 search_timeout,
	 optimize_timeout,
	 pending,		%% gb_tree mapping with pending request, keyed by RequestId (as return by async http:request)
	 auto_commit = false,   %% false | always | {time,TRef}
	 auto_optimize = false, %% false | {time,TRef}
	 dirty = false		   %%true if there are uncommited updates
	 }).


