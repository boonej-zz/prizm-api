%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2014 9:17 PM
%%%-------------------------------------------------------------------
-module(callback_handler).
-author("joeconway").

-behaviour(cowboy_http_handler).

%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handle_request({<<"GET">>, Req}) ->
    {Mode, Req} = cowboy_req:qs_val(<<"hub.mode">>, Req),
    {Challenge, Req} = cowboy_req:qs_val(<<"hub.challenge">>, Req),
    {VerifyToken, Req} = cowboy_req:qs_val(<<"hub.verify_token">>, Req),

    io:format("Did receive GET ~p ~p ~p~n", [Mode, Challenge, VerifyToken]),

    cowboy_req:reply(200, [], Challenge, Req);
handle_request({<<"POST">>, Req}) ->
    {ok, Body, _Req} = cowboy_req:body(Req),
    Payload = jsx:decode(Body),
    io:format("~p~n", [Payload]),

    cowboy_req:reply(200, Req).

handle(Req, _State) ->
    handle_request(cowboy_req:method(Req)).



terminate(_Reason, _Req, _State) ->
    ok.