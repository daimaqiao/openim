%%%--------------------------------------------------------
%%% File	: mod_logger.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 实现独立的日志工具
%%% Created	: 2016.3.22
%%% Version	: 1/2016.0322
%%% Dependencies:
%%%		log4erl		独立的日志工具
%%%		mochiweb	log4erl依赖项
%%%	Input	: $EJABBERD_HOME/etc/ejabberd/mod_logger.conf
%%%	Output	: $EJABBERD_HOME/var/log/ejabberd/mod_logger.log
%%%
%%%--------------------------------------------------------

-module(mod_logger).
-author(daimaqiao).
-vsn(1).

-behavior(gen_mod).

-include("logger.hrl").
-include("mod_logger.hrl").
-define(LOGGER, log4erl).

-export([start/2, stop/1]).


start(Host, Opts) ->
	?INFO_MSG("Start module ~p!~n"
			  "Host= ~p~n"
			  "Opts= ~p~n"
			  "Conf= ~p~n"
			  "Log= ~p~n"
			  ,[?MODULE, Host, Opts, get_conf(), get_log()]),
	application:start(?LOGGER),
	?LOGGER:conf(get_conf()),
	?LOGI(" ===== mod_logger is here. ===== "),
	ok.

stop(Host) ->
	?INFO_MSG("Stop module ~p!~n"
			  "Host= ~p~n"
			  ,[?MODULE, Host]),
	ok.


%% get_conf()
%% 返回模块对应的配置文件
get_conf() ->
	Confile= atom_to_list(?MODULE)++".conf",
	get_env_path("EJABBERD_CONFIG_PATH")++Confile.


%% get_log()
%% 返回模块对应的日志文件
get_log() ->
	Logfile= atom_to_list(?MODULE)++".log",
	get_env_path("EJABBERD_LOG_PATH")++Logfile.


%% get_env_path(Key)
%% 返回以“/”结束的目录路径
%% Key: EJABBERD_CONFIG_PATH, EJABBERD_LOG_PATH
%%
get_env_path(Key) ->
	case os:getenv(Key) of
		false-> "./";
		Path ->
			case filename:dirname(Path) of
				"/"-> "/";
				Dir-> Dir++"/"
			end
	end.


