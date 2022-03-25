%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 七月 2018 10:02
%%%-------------------------------------------------------------------
-author("Administrator").

-record(shang_state, {
    worker                  %% 工作进程
}).

%% mysql配置
-define(MysqlHost, "localhost").
-define(MysqlUser, "sy_mmo").
-define(MysqlPassword, "sy_mmo123456").
-define(MysqlDatabase, "shang_kaifa").

%% 游戏节点配置
-define(NodeName, "houtai_shang@192.168.1.99").
-define(AtomNodeName,'houtai_shang@192.168.1.99').
-define(NodeIp, "192.168.1.99").
-define(NodePort, 8000).
%% 通过确定该模块(服务进程名)检查游戏节点是否开启,
-define(CheckStartServerName, timer_day).
-define(OpenDate, {2018, 1, 1}).
%% 游戏路径配置
-define(LocalServerRootDir, "D:/Work/shang/local").
-define(SvnServerRootDir, "D:/Work/shang/server").

%% 网页配置
-define(HttpPort, 11111).
