{application, zvz,
    [{description, "ZVZ Server"},
     {vsn, "0.1"},
     {modules, [zvz_app]},
     {registered, []},
     {applications, [kernel, stdlib]},
     {mod, {zvz_app, []}},
     {env, []},
     {start_phases, []}
]}.