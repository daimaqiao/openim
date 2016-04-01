%%%--------------------------------------------------------
%%% File	: mod_dump_packet.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 输出XMPP包内容到文件
%%% Created	: 2016.3.22
%%% Version	: 5/2016.0330
%%% Dependencies:
%%%		mod_logger			log4erl的模块封装
%%%		log4erl				独立的日志工具
%%%		mochiweb			log4erl依赖项
%%%		xmlel_indent.erl	为#xmlel添加缩进空格等信息
%%%
%%%--------------------------------------------------------

-module(mod_dump_packet).
-author(daimaqiao).
-vsn(5).

-behavior(gen_mod).

-include("logger.hrl").
-include("jlib.hrl").
-include("mod_logger.hrl").

-define(OPT_STANZA, dump_packet_stanza).

-export([start/2, stop/1]).
-export([install_dump_packet/0, dump_packet/1]).
-export([mod_opt_type/1]).

start(Host, Opts) ->
	?INFO_MSG("Start module ~p!~n"
			  "Host= ~p~n"
			  "Opts= ~p~n"
			  ,[?MODULE, Host, Opts]),
	cache_opts(Opts),
	install_dump_packet(),
	ok.

stop(Host) ->
	?INFO_MSG("Stop module ~p!~n"
			  "Host= ~p~n"
			  ,[?MODULE, Host]),
	ok.


%% install_dump_packet()
%% 设置filter_packet事件
%%
install_dump_packet() ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, dump_packet, 20).


%% dump_packet(From, To, Packet)
%% 返回drop表示丢弃，否则就原样返回
%%
dump_packet(All= {From, To, Packet}) ->
	case select_packet(From#jid.user, To#jid.user, Packet#xmlel.name) of
		true ->
			dump_to_text(All);
		_ ->
			ok
	end,
	All.

%% select_packet(User1, User2, Stanza)
%% 选择匹配的数据包，返回true表示选中
%%
select_packet(User1, User2, Stanza) ->
	{OptUser, OptStanza}= fetch_opts_user_stanza(),

	((OptUser== []) or lists:member(User1, OptUser) or lists:member(User2, OptUser)) and
	((OptStanza== []) or lists:member(Stanza, OptStanza)).


%% do_dump_packet(From, To, Packet)
%% 原样输出包内容
%%
do_dump_packet({From, To, Packet}) ->
	?LOGD(" *** dumping ***~n"
		  "  From  = ~ts~n"
		  "  To    = ~ts~n"
		  "  Packet= ~ts~n"
		  "~n"
		  ,[From, To, Packet]),
	ok.

%% dump_to_text(From#jid{}, To#jid{}, Packet#xmlel{})
%% 将内容转换成字符串再输出
%%
dump_to_text({From= #jid{}, To= #jid{}, Packet= #xmlel{}}) ->
	Id1= jlib:jid_to_string(From),
	Id2= jlib:jid_to_string(To),
	Xmlel1= xmlel_indent:deflate_xmlel(Packet),
	Xmlel2= xmlel_indent:inflate_xmlel(Xmlel1),
	Xml= xml:element_to_binary(Xmlel2),
	do_dump_packet({binary_to_list(Id1), binary_to_list(Id2),
					binary_to_list(Xml)});
dump_to_text(All) ->
	do_dump_packet(All).


%%
%% mod_opt_type
%% 识别并转换模块参数
%% user:   [string(), ...]
%% stanza: [string(), ...]
%%
mod_opt_type(user) ->
	fun(A) -> A end;
mod_opt_type(stanza) ->
	fun(A) -> A end;
mod_opt_type(_) ->
    [user, stanza].


%% cache_opts(Opts)
%% 将必要参数保存到ETS中
%% 返回ok
%%
cache_opts(Opts) ->
	ets:new(?MODULE, [set, protected, named_table]),
	User= opt_of(user, Opts, []),
	Stanza= opt_of(stanza, Opts, []),
	ets:insert(?MODULE, {opts_user_stanza, {User, Stanza}}),
	ok.

%% opt_of(Key, Opts, Def)
%% 从列表Opts中找到Key，返回Key对应的Val，或者Def
%%
opt_of(Key, Opts, Def) ->
	case lists:keyfind(Key, 1, Opts) of
		{Key, Val} -> Val;
		false -> Def
	end.

%% fetch_opts_user_stanza()
%% 从ETS中取回参数
%% 返回{User::list(), Stanza::list()}
%%
fetch_opts_user_stanza() ->
	case ets:lookup(?MODULE, opts_user_stanza) of
		[{opts_user_stanza, {User, Stanza}}] ->
			{User, Stanza};
		_ ->
			{[], []}
	end.

