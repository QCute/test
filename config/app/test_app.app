{
    application, main,
    [
        {description, "This is test application."},
        {vsn, "1.0.0"},
        {mod, {test_app, []}},
        {modules, [test_app]},
        {registered, [test_app]},
        {applications, [kernel, stdlib, sasl]},
        {env, []}
    ]
}.
