%%%------------------------------------------------------------------
%%% @doc
%%% common define
%%% @end
%%%------------------------------------------------------------------
%% 布尔值定义
-define(TRUE,                                         1).
-define(FALSE,                                        0).

%% 时间相关
%% 毫秒 定时器使用
-define(MILLISECONDS(Seconds),                        ((Seconds) * 1000)).       %% 一秒的时间（毫秒）
-define(MINUTE_MILLISECONDS(Minute),                  ((Minute) * 60 * 1000)).   %% 一分钟的时间（毫秒）
-define(HOUR_MILLISECONDS(Hour),                      ((Hour) * 60 * 1000)).     %% 一分钟的时间（毫秒）
-define(DAY_MILLISECONDS(Day),                        ((Day) * 86400 * 1000)).   %% 一天的时间（毫秒）
-define(WEEK_MILLISECONDS(Week),                      ((Week) * 604800 * 1000)). %% 一周的时间（毫秒）

%% 秒 一般记录使用
-define(MINUTE_SECONDS(Minute),                       ((Minute) * 60)).          %% 一分钟的时间（秒）
-define(HOUR_SECONDS(Hour),                           ((Hour) * 60)).            %% 一分钟的时间（秒）
-define(DAY_SECONDS(Day),                             ((Day) * 86400)).          %% 一天的时间（秒）
-define(WEEK_SECONDS(Week),                           ((Week) * 604800)).        %% 一周的时间（秒）

%% 毫秒 定时器使用
-define(MILLISECONDS,                                 1000).                     %% 一秒的时间（毫秒）
-define(MINUTE_MILLISECONDS,                          60 * 1000).                %% 一分钟的时间（毫秒）
-define(HOUR_MILLISECONDS,                            3600 * 1000).              %% 一小时的时间（毫秒）
-define(DAY_MILLISECONDS,                             86400 * 1000).             %% 一天的时间（毫秒）
-define(WEEK_MILLISECONDS,                            604800 * 1000).            %% 一周的时间（毫秒）

%% 秒 一般记录使用
-define(MINUTE_SECONDS,                               60).                       %% 一分钟的时间（秒）
-define(HOUR_SECONDS,                                 3600).                     %% 一小时的时间（秒）
-define(DAY_SECONDS,                                  86400).                    %% 一天的时间（秒）
-define(WEEK_SECONDS,                                 604800).                   %% 一周的时间（秒）

-define(CALL_TIMEOUT,                                 5000).                     %% call默认超时

%% 通用错误类型
-type error() :: error | {error, term()}.
