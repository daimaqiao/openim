%%%--------------------------------------------------------
%%% File	: mod_receipts_ack.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 实现XEP-0184协议的服务端应答
%%% Created	: 2016.3.29
%%% Version	: 2/2016.0330
%%% Dependencies:
%%%		log4erl		独立的日志工具
%%%		mochiweb	log4erl依赖项
%%%	Input	: $EJABBERD_HOME/etc/ejabberd/mod_logger.conf
%%%	Output	: $EJABBERD_HOME/var/log/ejabberd/mod_logger.log
%%%
%%%	References:
%%%		https://github.com/Mingism/ejabberd-stanza-ack
%%%
%%%--------------------------------------------------------

-module(mod_receipts_ack).
-author(daimaqiao).
-vsn(2).

-behavior(gen_mod).

-include("logger.hrl").
-include("jlib.hrl").
-include("mod_logger.hrl").

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
-define(ACK_PREFIX, <<"ack.">>).


-export([start/2, stop/1]).
-export([install_receipts_ack/1, receipts_ack/4]).


start(Host, Opts) ->
	?INFO_MSG("Start module ~p!~n"
			  "Host= ~p~n"
			  "Opts= ~p~n"
			  ,[?MODULE, Host, Opts]),
	install_receipts_ack(Host),
	ok.

stop(Host) ->
	?INFO_MSG("Stop module ~p!~n"
			  "Host= ~p~n"
			  ,[?MODULE, Host]),
	ok.


%% install_receipts_ack()
%% 设置filter_packet事件
%%
install_receipts_ack(Host) ->
	mod_disco:register_feature(Host, ?NS_RECEIPTS),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, receipts_ack, 20).


%% receipts_ack(Packet, C2SStage, From, To)
%% 返回Packet
%%
receipts_ack(Packet= #xmlel{name= <<"message">>, attrs= Attrs}, _C2SStage, From, To) ->
	case xml:get_attr_s(<<"type">>, Attrs) of
		<<"chat">> ->
			case xml:get_subtag_with_xmlns(Packet, <<"request">>, ?NS_RECEIPTS) of
				false ->
					%% no receipts request
					ok;
				_ ->
					do_receipts_ack(From, To,
									xml:get_attr_s(<<"id">>, Attrs),
									xml:get_subtag_cdata(Packet, <<"body">>))
			end;
		_ ->
			%% ignore non-chat-type
			ok
	end,
	Packet;
receipts_ack(Packet, _, _, _) ->
	Packet.

%% do_receipts_ack(From, To, Id, Body)
%% 参照XEP-0184协议，向From回应服务器端的Ack
%% 返回ok
do_receipts_ack(From, To, Id, Body) when size(Id)>0; size(Body)>0 ->
	?LOGD("Send receipts/Ack to ~ts~n"
		  "for(~ts): [~ts]~ts~n",
		  [binary_to_list(jlib:jid_to_string(From)),
		   binary_to_list(jlib:jid_to_string(To)),
		   binary_to_list(Id),
		   binary_to_list(Body)]),
	Jid= make_jid_ack(From),
	Xmlel= make_xmlel_ack(jlib:jid_to_string(From), Id),
    ejabberd_router:route(Jid, From, Xmlel);
do_receipts_ack(_, _, _, _) ->
	ok.


%% make_jid_ack(To, Id)
%% 生成ack的jid数据
%% 返回#jid
%%
make_jid_ack(#jid{luser= User, lserver= Server}) ->
	jid:make(User, <<?ACK_PREFIX/binary, Server/binary>>, <<"">>).


%% make_xmlel_ack(To, Id)
%% 生成ack的xmlel数据
%% 返回#xmlel
%%
make_xmlel_ack(To, Id) ->
	#xmlel{name= <<"message">>, attrs= [{<<"to">>, To}],
		   children= [
					  #xmlel{name= <<"received">>,
							 attrs= [{<<"xmlns">>, ?NS_RECEIPTS},
									 {<<"id">>, Id}
									], children= []}
					 ]}.

