%%%--------------------------------------------------------
%%% File	: mod_receipts_ack.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 实现XEP-0184协议的服务端应答
%%% Created	: 2016.3.29
%%% Version	: 6/2016.0623
%%% Dependencies:
%%%		log4erl		独立的日志工具
%%%		mochiweb	log4erl依赖项
%%%		fast_xml	原p1_xml，xml工具
%%%		p1_utils	fast_xml依赖项
%%%	Input	: $EJABBERD_HOME/etc/ejabberd/mod_logger.conf
%%%	Output	: $EJABBERD_HOME/var/log/ejabberd/mod_logger.log
%%%
%%%	References:
%%%		https://github.com/Mingism/ejabberd-stanza-ack
%%%		https://github.com/mrDoctorWho/ejabberd_mod_apns
%%%
%%%--------------------------------------------------------

-module(mod_receipts_ack).
-author(daimaqiao).
-vsn(6).
-date({2016,6,23}).

-behavior(gen_mod).

-include("logger.hrl").
-include("jlib.hrl").
-include("mod_logger.hrl").

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
-define(ACK_PREFIX, <<"ack.">>).

-define(TIMER_GO_OFFLINE, 180000).%% 3min
-define(POS_KEY, 1).
-define(POS_STATE, 2).
-define(POS_TIMER, 3).
-define(POS_TS, 4).
-define(POS_ORIG, 5).

-export([start/2, stop/1]).
-export([install_receipts_ack/1, receipts_ack/4, offline_ack/3]).
-export([mod_opt_type/1]).
-export([go_offline/4]).


start(Host, Opts) ->
	?INFO_MSG("Start module ~p!~n"
			  "Host= ~p~n"
			  "Opts= ~p~n"
			  ,[?MODULE, Host, Opts]),
	cache_init(),
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
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_ack, 20),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, receipts_ack, 20).


offline_ack(From, To, Packet= #xmlel{name= <<"message">>, attrs= Attrs}) ->
	case fxml:get_attr_s(<<"type">>, Attrs) of
		<<"chat">> ->
			offline_ack_request(From, To, Packet, Attrs);
		_ ->
			%% ignore non-chat-type
			false
	end,
	ok;
offline_ack(_,_,_) ->
	ok.

%% 处理完成返回ok，不匹配未处理返回false
offline_ack_request(From, To, Message, Attrs) ->
	case fxml:get_subtag_with_xmlns(Message, <<"request">>, ?NS_RECEIPTS) of
		false ->
			%% mismatching
			false;
		_ ->
			Id= fxml:get_attr_s(<<"id">>, Attrs),
			?LOGD("offline receipts: ~s, ~s",
				  [binary_to_list(jid:to_string(From)), binary_to_list(Id)]),
			cache_remove_chat(cache_key(From, Id)),
			do_receipts_ack(To, From, Id),
			ok
	end.



%% receipts_ack(#xmlel(), C2SStage?, #jid(), #jid()) ->
%% #xmlel()
receipts_ack(Packet= #xmlel{name= <<"message">>, attrs= Attrs}, _C2SStage, From, To) ->
	case fxml:get_attr_s(<<"type">>, Attrs) of
		<<"chat">> ->
			case receipts_ack_request(From, To, Packet, Attrs) of
				false ->
					case receipts_ack_received(From, To, Packet) of
						ok ->
							ok;
						_ ->
							false
					end;%% received
				_ ->
					ok
			end;%% request
		_ ->
			%% ignore non-chat-type
			false
	end,
	Packet;
receipts_ack(Packet, _, _, _) ->
	Packet.

%% receipts_ack_request(#jid(), #jid(), #xmlel(), list()) ->
%% ok | false
%% 处理完成返回ok，不匹配未处理返回false
receipts_ack_request(From, To, Message, Attrs) ->
	case fxml:get_subtag_with_xmlns(Message, <<"request">>, ?NS_RECEIPTS) of
		false ->
			%% mismatching
			false;
		_ ->
			Id= fxml:get_attr_s(<<"id">>, Attrs),
			%%Body= fxml:get_subtag_cdata(Message, <<"body">>),
			?LOGD("request receipts: ~s, ~s",
				  [binary_to_list(jid:to_string(From)), binary_to_list(Id)]),
			cache_save_chat(From, To, Message, Id)
	end.

%% receipts_ack_received(#jid(), #jid(), #xmlel()) ->
%% ok | false
%% 处理完成返回ok，不匹配未处理返回false
receipts_ack_received(_From, To, Message) ->
  	case fxml:get_subtag_with_xmlns(Message, <<"received">>, ?NS_RECEIPTS) of
  		#xmlel{attrs= ReceivedAttrs} ->
  			case fxml:get_attr(<<"id">>, ReceivedAttrs) of
  				{value, Id} ->
					?LOGD("received receipts: ~s, ~s",
						  [binary_to_list(jid:to_string(To)), binary_to_list(Id)]),
					cache_remove_chat(cache_key(To, Id)),
  					ok;
  				_ ->
					false
  			end;
  		_ ->
			false
  	end.


%% cache_key(#jid(), binary()) ->
%% {binary(), binary()}
cache_key(Jid, Id) ->
	{jid:to_string(Jid), Id}.


cache_init() ->
	case lists:member(?MODULE, ets:all()) of
		true ->
			%% 初始化缓存一次
			ok;
		_ ->
			timer_init(),
			ets:new(?MODULE, [set, public, named_table]),
			ok
	end.

%% cache_save_chat
%%	(#jid(), #jid(), #xmlel(), binary()) ->
%% true
cache_save_chat(From, To, Packet, Id) ->
	Ts= erlang:system_time(seconds),
	Key= cache_key(From, Id),
	Timer= case timer_reset(Key, ?TIMER_GO_OFFLINE, go_offline, [Key, From, To, Packet]) of
			   {ok, T} ->
					?LOGD("set timer go_offline ~p for ~p", [T, Key]),
				   T;
			   {error, Reason} ->
				   ?LOGW("ON cache_save_chat, timer_reset fault: ~p", [Reason]),
				   false
		   end,
	ets:insert(?MODULE, {Key, {state, save_chat}, {timer, Timer}, {ts, Ts}, {orig, {From, To, Packet}}}).

%% cache_remove_chat(tuple())->
%% true
cache_remove_chat(Key) ->
	timer_unset(Key),
	ets:delete(?MODULE, Key).


%% go_offline(tuple(), #jid(), #jid(), #xmlel()) ->
%% ok | false
%% clear timer, save into offline, and kick bad recipient
go_offline(Key, From, To, Packet) ->
	%% clear timer
	?LOGD("timer of go_offline timeout: ~p", [Key]),
	Ts= erlang:system_time(seconds),
	try ets:update_element(?MODULE, Key, [{?POS_STATE, {state, go_offline}},
										  {?POS_TIMER, {timer, timeout}},
										  {?POS_TS, {ts, Ts}}])
	catch
		_:Reason ->
			%% may not exist
			?LOGW("ON go_offline, update_element fault: ~p, for ~p", [Key, Reason]),
			false
	end,
	%% save into offline
	save_into_offline(From, To, Packet),
	%% kick bad recipient
	kick_user(To).


timer_init() ->
	timer:start().

%% timer_unset(tuple()) ->
%% ok | false
%% clear last timer if exists
timer_unset(Key) ->
	try ets:lookup_element(?MODULE, Key, ?POS_TIMER) of
	{timer, false} ->
		%% nothing
		ok;
	{timer, T} ->
		%% cancel the timer
		?LOGD("cancel timer ~p for ~p", [T, Key]),
		timer:cancel(T),
		ok
	catch
		_:_ ->
			%% may not exist
			false
	end.

%% timer_reset(tuple(), integer(), atom(), list()) ->
%% {ok, TRef} | {error, Reason}
%% clear last timer if exists, and then create a new timer
timer_reset(Key, Timeout, Action, Args) ->
	timer_unset(Key),
	timer:apply_after(Timeout, ?MODULE, Action, Args).


%% (#jid())->
%% ok | false
kick_user(Jid=#jid{luser=User,lserver=Server,lresource= <<"">>}) ->
	N= ejabberd_sm:kick_user(User, Server),
	if
		N > 0 ->
			?LOGI("KICK recipient ~s with ~p resources", [binary_to_list(jid:to_string(Jid)),N]),
			ok;
		true ->
			false
	end;
kick_user(Jid=#jid{luser=User,lserver=Server,lresource=Resource}) ->
	case ejabberd_sm:get_session_pid(User, Server, Resource) of
		none ->
			false;
		Pid ->
			?LOGI("KICK nonresponse recipient: ~s", [binary_to_list(jid:to_string(Jid))]),
			Pid! kick,
			ok
	end.

%% (#jid(), #jid(), #xmlel()) ->
%% ok
save_into_offline(From, To, Packet) ->
	?LOGD("PUT message into offline: ~s --> ~s~n~s",
		  [binary_to_list(jid:to_string(From)), binary_to_list(jid:to_string(To)),
		   binary_to_list(fxml:element_to_binary(Packet))]),
	ejabberd_hooks:run(offline_message_hook, From#jid.lserver, [From, To, Packet]).


%% 仅当消息进入离线队列，无法收到接收方receipts时
%% 服务器回应ack receipts
%% 参照XEP-0184协议，向To回应服务器端代From发的Ack
%% 返回ok
do_receipts_ack(#jid{luser=User,lserver=Server,lresource=Resource}, To, Id) ->
	Jid= jid:make(User, <<?ACK_PREFIX/binary, Server/binary>>, Resource),
	?LOGD("send ack ~s, ~s --> ~s",
		  [binary_to_list(Id), binary_to_list(jid:to_string(Jid)), binary_to_list(jid:to_string(To))]),
	Xmlel= #xmlel{name= <<"message">>, attrs= [{<<"to">>, To}],
				  children= [#xmlel{name= <<"received">>,
									attrs= [{<<"xmlns">>, ?NS_RECEIPTS},
											{<<"id">>, Id}],
									children= []}]},
    ejabberd_router:route(Jid, To, Xmlel).


%% 不提供模块参数
mod_opt_type(_) ->
	[].

