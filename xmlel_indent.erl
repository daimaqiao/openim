%%%--------------------------------------------------------
%%% File	: xmlel_indent.erl
%%% Author	: daimaqiao <daimaqiao@126.com>
%%% Purpose	: 为#xmlel补充/清除缩进空格等信息（换行或空格）
%%% Created	: 2016.3.24
%%% Version	: 1/2016.0324
%%% Dependencies:
%%%		p1_xml		process-one出品的xml工具
%%%		p1-utils	p1_xml依赖项
%%%
%%%--------------------------------------------------------

-module(xmlel_indent).
-author(daimaqiao).
-vsn(1).

-include("jlib.hrl").

-export([inflate_xmlel/1, deflate_xmlel/1]).

-define(INDENT_LEVEL, 4).
-define(MAKE_INDENT(N),{xmlcdata,list_to_binary("\n"++lists:duplicate(N," "))}).
-define(INLINE_MAX, 24).


%% inflate_xml(Xml)
%% 为Xml补充缩进信息
%% 返回扩充之后的#xmlel
%%
inflate_xmlel(Xml) when is_record(Xml, xmlel) ->
	do_inflate_xmlel([], 0, Xml#xmlel.name, Xml#xmlel.attrs, Xml#xmlel.children);
inflate_xmlel(Any) ->
	Any.


%% deflate_xml(Xml)
%% 为Xml清除缩进信息
%% 返回清除缩进之后的#xmlel
%%
deflate_xmlel(Xml) when is_record(Xml, xmlel) ->
	do_deflate_xmlel([], Xml#xmlel.name, Xml#xmlel.attrs, Xml#xmlel.children);
deflate_xmlel(Any) ->
	Any.


%% do_deflate_xmlel(Acc, Name, Attrs, Children)
%% 在Children中清除缩进信息（去除非可见文字信息）
%% Acc是累加器
%% 返回不含缩进信息的#xmlel{name=Name, attrs=Attrs, children=NewChildren}
%%
do_deflate_xmlel([], Name, Attrs, []) ->
	#xmlel{name= Name, attrs= Attrs, children= []};
do_deflate_xmlel(Acc, Name, Attrs, []) ->
	Children= lists:reverse(Acc),
	#xmlel{name= Name, attrs= Attrs, children= Children};
do_deflate_xmlel(Acc, Name, Attrs, [{xmlcdata, B}| Children]) ->
	case re:replace(B, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]) of
		<<"">> ->
			do_deflate_xmlel(Acc, Name, Attrs, Children);
		X ->
			Child= {xmlcdata, X},
			do_deflate_xmlel([Child| Acc], Name, Attrs, Children)
	end;
do_deflate_xmlel(Acc, Name, Attrs, [Child| Children]) ->
	NewChild= deflate_xmlel(Child),
	do_deflate_xmlel([NewChild| Acc], Name, Attrs, Children).


%% do_inflate_xmlel(Acc, N, Name, Attrs, Children)
%% 在Children中补充缩进信息
%% Acc是累加器，N表示缩进量（空格数量）
%% 返回包含缩进信息的#xmlel{name=Name, attrs=Attrs, children=NewChildren}
%%
do_inflate_xmlel([], _, Name, Attrs, []) ->
	#xmlel{name= Name, attrs= Attrs, children= []};
do_inflate_xmlel([], _, Name, Attrs, Children= [{xmlcdata, B}])
  when size(B)=< ?INLINE_MAX ->
	#xmlel{name= Name, attrs= Attrs, children= Children};
do_inflate_xmlel(Acc, N, Name, Attrs, []) ->
	Children= lists:reverse([?MAKE_INDENT(N)| Acc]),
	#xmlel{name= Name, attrs= Attrs, children= Children};
do_inflate_xmlel(Acc, N, Name, Attrs, [Child | Children]) ->
	Indent= N+ ?INDENT_LEVEL,
	NewChild= do_inflate_xmlel2(Indent, Child),
	Acc1= [?MAKE_INDENT(Indent)| Acc],
	NewAcc= [NewChild| Acc1],
	do_inflate_xmlel(NewAcc, N, Name, Attrs, Children).

%% do_inflate_xmlel2(N, X)
%% 根据X的类型决定是否为X补充缩进信息
%% 返回更新后的X
%%
do_inflate_xmlel2(N, X) when is_record(X, xmlel) ->
	do_inflate_xmlel([], N, X#xmlel.name, X#xmlel.attrs, X#xmlel.children);
do_inflate_xmlel2(_, X) ->
	X.

