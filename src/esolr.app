{application, esolr,
  [
  {description, "An Erlang Solr Client"},
  {vsn, "0.0"},
  {modules, [esolr, esolr_client_sup, rfc4627, esolr_client]},
  {registered, [esolr_client]},
  {applications,[kernel, stdlib]},
  {env, []},
  {mod, {esolr, []}}
  ]
}.

