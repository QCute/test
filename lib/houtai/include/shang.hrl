%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. ���� 2018 10:02
%%%-------------------------------------------------------------------
-author("Administrator").

-record(shang_state, {
    worker                  %% ��������
}).

%% mysql����
-define(MysqlHost, "localhost").
-define(MysqlUser, "sy_mmo").
-define(MysqlPassword, "sy_mmo123456").
-define(MysqlDatabase, "shang_kaifa").

%% ��Ϸ�ڵ�����
-define(NodeName, "houtai_shang@192.168.1.99").
-define(AtomNodeName,'houtai_shang@192.168.1.99').
-define(NodeIp, "192.168.1.99").
-define(NodePort, 8000).
%% ͨ��ȷ����ģ��(���������)�����Ϸ�ڵ��Ƿ���,
-define(CheckStartServerName, timer_day).
-define(OpenDate, {2018, 1, 1}).
%% ��Ϸ·������
-define(LocalServerRootDir, "D:/Work/shang/local").
-define(SvnServerRootDir, "D:/Work/shang/server").

%% ��ҳ����
-define(HttpPort, 11111).
