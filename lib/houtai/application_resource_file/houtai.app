%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 20:13
%%%-------------------------------------------------------------------
{application, houtai, [
    {description, ""},
    {vsn, "4.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {modules,[houtai,houtai_handler,houtai_sup]},
    {mod, {houtai, []}},
    {env, [
        {logFile,"D:/houtai4/houtai/log.txt"}
    ]}
]}.