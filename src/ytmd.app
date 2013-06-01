{application, ytmd,
 [{description, "YouTube Monitor"},
  {vsn, "1"},
  {modules, [time, conf, netw, rssf, text, html, ytmd]},
  {registered, [ytmd]},
  {applications, [kernel, stdlib]},
  {mod, {ytmd, []}},
  {env, [{cfgfile, "config.xml"}]}
 ]}.
