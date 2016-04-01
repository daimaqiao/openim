%%% File    : mod_logger.hrl
%%% Author  : daimaqiao <daimaqiao@126.com>
%%% Purpose : 定义独立的日志工具
%%% Created : 2016.3.22
%%% Version : 1/2016.0322
%%% Dependencies:
%%%     log4erl     独立的日志工具
%%%     mochiweb    log4erl依赖项，通常用于实现HTTP服务功能
%%%		mod_logger	在ejabberd中初始化log4erl
%%%
%%%--------------------------------------------------------

-define(LOGD(Text),
	log4erl:debug(Text)).
-define(LOGD(Format, Args),
	log4erl:debug(Format, Args)).

-define(LOGI(Text),
	log4erl:info(Text)).
-define(LOGI(Format, Args),
	log4erl:info(Format, Args)).

-define(LOGW(Text),
	log4erl:warn(Text)).
-define(LOGW(Format, Args),
	log4erl:warn(Format, Args)).

-define(LOGE(Text),
	log4erl:error(Text)).
-define(LOGE(Format, Args),
	log4erl:error(Format, Args)).

-define(LOGF(Text),
	log4erl:fatal(Text)).
-define(LOGF(Format, Args),
	log4erl:fatal(Format, Args)).


