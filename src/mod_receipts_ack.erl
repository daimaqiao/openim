%%%--------------------------------------------------------
%%% File	: mod_receipts_ack.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 实现XEP-0184协议的服务端应答
%%% Created	: 2016.3.29
%%% Version	: 7/2016.0627
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
-vsn(7).
-date({2016,6,27}).

-behavior(gen_mod).

-include("logger.hrl").
-include("jlib.hrl").
-include("mod_logger.hrl").

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
-define(ACK_PREFIX, <<"ack.">>).

%%-define(MODULE_RESEND, list_to_atom(atom_to_list(?MODULE)++"resend")).
-define(MODULE_RESEND, mod_receipts_ack_resend).


-define(TIMER_GO_OFFLINE, (10*1000)).%% 10s
-define(POS_KEY, 1).
-define(POS_STATE, 2).
-define(POS_TIMER, 3).
-define(POS_TS, 4).
-define(POS_ORIG, 5).


-export([start/2, stop/1]).
-export([install_receipts_ack/1, receipts_ack/4, offline_ack/3, logon_ack/3]).
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
	uninstall_receipts_ack(Host),
	ok.


%% install_receipts_ack()
%% 设置filter_packet事件
%%
install_receipts_ack(Host) ->
	mod_disco:register_feature(Host, ?NS_RECEIPTS),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_ack, 20),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, receipts_ack, 20),
	ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, logon_ack, 20).
%%
uninstall_receipts_ack(Host) ->
	mod_disco:unregister_feature(Host, ?NS_RECEIPTS),
	ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_ack, 20),
	ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, receipts_ack, 20),
	ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, logon_ack, 20).


logon_ack(_Sid, Jid, _Info) ->
	KeyResend= cache_key(Jid),
	Found= ets:take(?MODULE_RESEND, KeyResend),
	?LOGD("check receipts are waiting for on logon: ~p", [Found]),
    lists:foreach(
		fun({_, Key}) ->
			cancel_timer_and_resend(Key)
		end, Found),
	ok.


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
			?LOGD("get offline message, and receipts is required: key= ~p", [cache_key(From, Id)]),
			cache_remove_chat(From, To, Id),
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
			?LOGD("get chat message, and receipts is required: key= ~p", [cache_key(From, Id)]),
			cache_save_chat(From, To, Message, Id)
	end.

%% receipts_ack_received(#jid(), #jid(), #xmlel()) ->
%% ok | false
%% 处理完成返回ok，不匹配未处理返回false
receipts_ack_received(From, To, Message) ->
  	case fxml:get_subtag_with_xmlns(Message, <<"received">>, ?NS_RECEIPTS) of
  		#xmlel{attrs= ReceivedAttrs} ->
  			case fxml:get_attr(<<"id">>, ReceivedAttrs) of
  				{value, Id} ->
					?LOGD("receipts is received: key= ~p", [cache_key(To, Id)]),
					cache_remove_chat(To, From, Id),
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
%% cache_key(#jid()) ->
%% {binary()}
cache_key(Jid) ->
	jid:to_string(jid:remove_resource(Jid)).



cache_init() ->
	case lists:member(?MODULE, ets:all()) of
		true ->
			%% 初始化缓存一次
			ok;
		_ ->
			timer_init(),
			ets:new(?MODULE, [set, public, named_table]),
			ets:new(?MODULE_RESEND, [bag, public, named_table]),
			ok
	end.

%% cache_save_chat
%%	(#jid(), #jid(), #xmlel(), binary()) ->
%% true
cache_save_chat(From, To, Packet, Id) ->
	Ts= erlang:system_time(seconds),
	Key= cache_key(From, Id),
	KeyResend= cache_key(To),
	Timer= case timer_reset(Key, ?TIMER_GO_OFFLINE, go_offline, [Key, From, To, Packet]) of
			   {ok, T} ->
					?LOGD("chat message is being cached, waiting for receipts: timer= ~p, key= ~p", [T, Key]),
				   T;
			   {error, Reason} ->
				   ?LOGW("FAILED to set timer, when caching chat message: ~p", [Reason]),
				   false
		   end,
	ets:insert(?MODULE, {Key, {state, save_chat}, {timer, Timer}, {ts, Ts}, {orig, {From, To, Packet}}}),
	ets:insert(?MODULE_RESEND, {KeyResend, Key}).

%% cache_remove_chat(#jid(), #jid(), binary())->
%% true
cache_remove_chat(From, To, Id) ->
	Key= cache_key(From, Id),
	KeyResend= cache_key(To),
	timer_unset(Key),
	ets:delete(?MODULE, Key),
	ets:delete_object(?MODULE_RESEND, {KeyResend, Key}).


%% go_offline(tuple(), #jid(), #jid(), #xmlel()) ->
%% ok | false
%% clear timer, save into offline, and drop bad recipient
go_offline(Key, From, To, Packet) ->
	%% clear timer
	?LOGD("time up! the recipient may be not online, because of none receipts: key= ~p", [Key]),
	Ts= erlang:system_time(seconds),
	try ets:update_element(?MODULE, Key, [{?POS_STATE, {state, go_offline}},
										  {?POS_TIMER, {timer, timeout}},
										  {?POS_TS, {ts, Ts}}])
	catch
		_:Reason ->
			%% may not exist
			?LOGW("FAILED to clear the timer waiting for receipts after timeout: key= ~p~nerror= ~p", [Key, Reason]),
			false
	end,
	%% save into offline
	save_into_offline(From, To, Packet),
	%% drop bad recipient
	drop_recipient(To).


timer_init() ->
	timer:start().

%% timer_unset(tuple()) ->
%% ok | false
%% clear last timer if exists
timer_unset(Key) ->
	try ets:lookup_element(?MODULE, Key, ?POS_TIMER) of
	{timer, Timer} when is_tuple(Timer) ->
		%% cancel the timer
		?LOGD("unset timer ~p for ~p", [Timer, Key]),
		timer:cancel(Timer),
		ok;
	_ ->
		%% nothing
		false
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


%% cancel_timer_and_resend(tuple()) ->
%% ok | false
cancel_timer_and_resend(Key) ->
	case ets:lookup(?MODULE, Key) of
		[{Key, _, {timer, Timer}, _, {orig, {From, To, Packet}}}] when is_tuple(Timer) ->
			%% cancel the timer
			?LOGD("cancel timer ~p for ~p", [Timer, Key]),
			timer:cancel(Timer),
			%% update ets
			Ts= erlang:system_time(seconds),
			try ets:update_element(?MODULE, Key, [{?POS_STATE, {state, to_resend}},
												  {?POS_TIMER, {timer, cancel}},
												  {?POS_TS, {ts, Ts}}])
			catch
				_:Reason ->
					%% may not exist
					?LOGW("FAILED to clear the timer that is canceled: key= ~p~nerror= ~p", [Key, Reason]),
					false
			end,
			%% resend the message
			?LOGD("resend message: ~p --> ~p", [jid:to_string(From), jid:to_string(To)]),
			ejabberd_router:route(From, To, Packet);
		Found ->
			?LOGD("timer waiting for receipts may be already cleared, resending is not required!~nfound= ~p", [Found]),
			false
	end.



%% (#jid())->
%% ok | false
drop_recipient(Jid=#jid{luser=User,lserver=Server,lresource= <<"">>}) ->
    Resources= ejabberd_sm:get_user_resources(User, Server),
    lists:foreach(
		fun(Resource) ->
			case ejabberd_sm:get_session_pid(User, Server, Resource) of
				none ->
					false;
				Pid ->
					close_pid(Pid)
			end
		end, Resources),
	if 
		length(Resources) > 0 ->
			?LOGI("DROP multiple recipients for nonresponse: jid= ~p, resources= ~p",
				  [jid:to_string(Jid), Resources]),
			ok;
		true ->
			false
	end;
drop_recipient(Jid=#jid{luser=User,lserver=Server,lresource=Resource}) ->
	case ejabberd_sm:get_session_pid(User, Server, Resource) of
		none ->
			false;
		Pid ->
			?LOGI("DROP recipient for nonresponse: jid= ~p", [jid:to_string(Jid)]),
			close_pid(Pid)
	end.

close_pid(Pid) ->
	ejabberd_c2s:close(Pid),
	ok.


%% (#jid(), #jid(), #xmlel()) ->
%% ok
save_into_offline(From, To, Packet) ->
	?LOGD("message is going into the offline storage: ~p --> ~p",
		  [jid:to_string(From), jid:to_string(To)]),
	ejabberd_hooks:run(offline_message_hook, From#jid.server, [From, To, Packet]).


%% 仅当消息进入离线队列，无法收到接收方receipts时
%% 服务器回应ack receipts
%% 参照XEP-0184协议，向To回应服务器端代From发的Ack
%% 返回ok
do_receipts_ack(#jid{luser=User,lserver=Server,lresource=Resource}, To, Id) ->
	Jid= jid:make(User, <<?ACK_PREFIX/binary, Server/binary>>, Resource),
	?LOGD("make ack of receipts locally: ~p --> ~p, id= ~p",
		  [jid:to_string(Jid), jid:to_string(To), Id]),
	Xmlel= #xmlel{name= <<"message">>, attrs= [{<<"to">>, To}],
				  children= [#xmlel{name= <<"received">>,
									attrs= [{<<"xmlns">>, ?NS_RECEIPTS},
											{<<"id">>, Id}],
									children= []}]},
    ejabberd_router:route(Jid, To, Xmlel).


%% 不提供模块参数
mod_opt_type(_) ->
	[].

