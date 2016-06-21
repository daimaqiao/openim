%%%--------------------------------------------------------
%%% File	: mod_receipts_ack.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 实现XEP-0184协议的服务端应答
%%% Created	: 2016.3.29
%%% Version	: 5/2016.0616
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
-vsn(5).
-date({2016,6,16}).

-behavior(gen_mod).

-include("logger.hrl").
-include("jlib.hrl").
-include("mod_logger.hrl").

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
-define(ACK_PREFIX, <<"ack.">>).

-define(TIMER_TIME,	15000).%% 15s

-export([start/2, stop/1]).
-export([install_receipts_ack/1, receipts_ack/4]).
-export([mod_opt_type/1]).
-export([timer_act/2]).


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
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, receipts_ack, 20).


%% receipts_ack(Packet, C2SStage, From, To)
%% 返回Packet
%%
receipts_ack(Packet= #xmlel{name= <<"message">>, attrs= Attrs}, _C2SStage, From, To) ->
	case fxml:get_attr_s(<<"type">>, Attrs) of
		<<"chat">> ->
			case receipts_ack_request(From, To, Packet, Attrs) of
				false ->
					case receipts_ack_received(From, To, Packet, Attrs) of
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

%% 处理完成返回ok，不匹配未处理返回false
receipts_ack_request(From, To, Message, Attrs) ->
	case fxml:get_subtag_with_xmlns(Message, <<"request">>, ?NS_RECEIPTS) of
		false ->
			%% mismatching
			false;
		_ ->
			Id= fxml:get_attr_s(<<"id">>, Attrs),
			%%Body= fxml:get_subtag_cdata(Message, <<"body">>),
			?LOGD("request receipts id: ~s", [binary_to_list(Id)]),
			cache_save_chat(From, Id, wait, {From, To, Message})
	end.

%% 处理完成返回ok，不匹配未处理返回false
receipts_ack_received(_From, To, Message, _Attrs) ->
  	case fxml:get_subtag_with_xmlns(Message, <<"received">>, ?NS_RECEIPTS) of
  		#xmlel{attrs= ReceivedAttrs} ->
  			case fxml:get_attr(<<"id">>, ReceivedAttrs) of
  				{value, Id} ->
					?LOGD("received receipts id: ~s", [binary_to_list(Id)]),
					cache_remove_chat(To, Id),
  					ok;
  				_ ->
					false
  			end;
  		_ ->
			false
  	end.



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
cache_save_chat(From, Id, State, Orig) when is_record(From, jid) ->
	Jid= jid:to_string(From),
	cache_save_chat(Jid, Id, State, Orig);
cache_save_chat(Jid, Id, State, Orig) when is_binary(Jid) ->
	Timer =
	case timer_reset({Jid, Id}, wait, ?TIMER_TIME) of
		{ok, T} ->
			T;
		{error, Reason} ->
			?LOGW("timer_reset (wait) error: ~p", [Reason]),
			false
	end,
	Now= erlang:system_time(seconds),
	ets:insert(?MODULE, {{Jid, Id}, {state, State}, {timer, Timer}, {t, Now}, {orig, Orig}}).


cache_remove_chat(From, Id) when is_record(From, jid) ->
	Jid= jid:to_string(From),
	cache_remove_chat(Jid, Id);
cache_remove_chat(Jid, Id) when is_binary(Jid) ->
	timer_unset({Jid, Id}),
	ets:delete(?MODULE, {Jid, Id}).

cache_update_state(To, Id, State) when is_record(To, jid) ->
	Jid= jid:to_string(To),
	cache_update_state(Jid, Id, State);
cache_update_state(Jid, Id, State) when is_binary(Jid) ->
	Timer =
	case timer_reset({Jid, Id}, State, ?TIMER_TIME) of
		{ok, T} ->
			T;
		{error, Reason} ->
			?LOGW("timer_reset (~p) error: ~p", [State, Reason]),
			false
	end,
	Now= erlang:system_time(seconds),
	ets:update_element(?MODULE, {Jid, Id}, [{2, {state, State}},
											{3, {timer, Timer}},
											{4, {t, Now}}]).


cache_dump_chat(From, Id) when is_record(From, jid) ->
	Jid= jid:to_string(From),
	cache_dump_chat(Jid, Id);
cache_dump_chat(Jid, Id) when is_binary(Jid) ->
	Found= ets:lookup(?MODULE, {Jid, Id}),
	?LOGD("cache dump: ~p", [Found]).
cache_dump_chat({Jid, Id}) ->
	cache_dump_chat(Jid, Id).


timer_init() ->
	timer:start().

%% clear last timer if exists
%% ->
%% {ok | false}
timer_unset(Key) ->
	try ets:lookup_element(?MODULE, Key, 3) of
	{timer, false} ->
		%% nothing
		ok;
	{timer, T} ->
		%% cancel the timer
		timer:cancel(T),
		ok
	catch
		_:_ ->
			%% may not exist
			false
	end.

%% create timer
%% ->
%% {ok, TRef} | {error, Reason}
timer_set(Key, State, Timeout) ->
	timer:apply_after(Timeout, ?MODULE, timer_act, [Key, State]).

%% clear last timer if exists
%% and then create a new timer
%% ->
%% {ok, TRef} | {error, Reason}
timer_reset(Key, State, Timeout) ->
	timer_unset(Key),
	timer_set(Key, State, Timeout).

%% actions of timer
timer_act(Key, State) ->
	?LOGD("TIMEOUT: (~p) ~p", [State, Key]),
	Now= erlang:system_time(seconds),
	try ets:update_element(?MODULE, Key, [{2, {state, timeout_state(State)}},
										  {3, {timer, false}},
										  {4, {t, Now}}])
	catch
		_:Reason ->
			%% may not exist
			?LOGW("element may not exist on timer_act: ~p, for ~p~n~p", [State, Key, Reason]),
			false
	end,
	cache_dump_chat(Key).

%% ->
%% atom()
timeout_state(wait) ->
	wait_timeout;
timeout_state(ack) ->
	ack_timeout;
timeout_state(_) ->
	timeout.



%% 仅当消息进入离线队列，无法收到接收方receipts时
%% 服务器回应ack receipts
%% 参照XEP-0184协议，向From回应服务器端的Ack
%% 返回ok
do_receipts_ack(From, To, Id) ->
	?LOGD("ack receipts id: ~s", [binary_to_list(Id)]),
	Jid =
	jid:make(From#jid.user,
			 <<?ACK_PREFIX/binary, (From#jid.server)/binary>>,
			 From#jid.resource),
	Xmlel =
	#xmlel{name= <<"message">>, attrs= [{<<"to">>, To}],
		   children= [
					  #xmlel{name= <<"received">>,
							 attrs= [{<<"xmlns">>, ?NS_RECEIPTS},
									 {<<"id">>, Id}
									], children= []}
					 ]},
    ejabberd_router:route(Jid, To, Xmlel).

%%%% 生成ack的jid数据
%%%% 返回#jid
%%make_jid_ack(#jid{luser= User, lserver= Server, lresource= Resource}) ->
%%	jid:make(User, <<?ACK_PREFIX/binary, Server/binary>>, Resource).
%%
%%%% 生成ack的xmlel数据
%%%% 返回#xmlel
%%make_xmlel_ack(To, Id) ->
%%	#xmlel{name= <<"message">>, attrs= [{<<"to">>, To}],
%%		   children= [
%%					  #xmlel{name= <<"received">>,
%%							 attrs= [{<<"xmlns">>, ?NS_RECEIPTS},
%%									 {<<"id">>, Id}
%%									], children= []}
%%					 ]}.


%% 不提供模块参数
mod_opt_type(_) ->
	[].

