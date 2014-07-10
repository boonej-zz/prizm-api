%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2014 1:43 PM
%%%-------------------------------------------------------------------
-module(prism_auth).
-author("joeconway").

-behaviour(gen_server).

%% API
-export([start_link/0, obtain_token/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {token, refresh_token, expiration, last_fetch_time}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

obtain_token() ->
    gen_server:call(?SERVER, obtain_token).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.


handle_call(obtain_token, _From, State) ->
    case State#state.token of
        undefined ->
            NewState = fetch_token(),
            {reply, NewState#state.token, NewState};
        Token ->
            case token_is_valid(State#state.expiration, State#state.last_fetch_time) of
                true ->
                    {reply, Token, State};
                false ->
                    NewState = refresh_token(State#state.refresh_token),
                    {reply, NewState#state.token, NewState}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

token_is_valid(Expiration, LastFetchTime) ->
    Diff = timer:now_diff(erlang:now(), LastFetchTime) / 1000000,
    case Diff > Expiration - 5 of
        true ->
            false;
        false ->
            true
    end.

refresh_token(PrevRefreshToken) ->
    URL = prism_config:base_url_string() ++ "/oauth2/token",
    Headers = [auth_header()],
    Body = jsx:encode([{<<"code">>, PrevRefreshToken}, {<<"grant_type">>, <<"refresh_token">>}, {<<"redirect_uri">>, list_to_binary(prism_config:redirect_uri())}]),

    Request = {URL, Headers, "application/json", Body},

    {ok, {{_Version, 200, _Reason}, _Headers, ResponseBody}} = httpc:request(post, Request, [], []),
    JSON = jsx:decode(list_to_binary(ResponseBody)),

    [Data] = proplists:get_value(<<"data">>, JSON),

    Token = proplists:get_value(<<"access_token">>, Data),
    Expires = proplists:get_value(<<"expires_in">>, Data),
    RefreshToken = proplists:get_value(<<"refresh_token">>, Data),

    #state{token = Token,
    expiration = Expires,
    refresh_token = RefreshToken,
    last_fetch_time = erlang:now()}.

fetch_token() ->
    AuthCode = fetch_auth_code(),

    URL = prism_config:base_url_string() ++ "/oauth2/token",
    Headers = [auth_header()],
    Body = jsx:encode([{<<"code">>, AuthCode}, {<<"grant_type">>, <<"authorization_code">>}, {<<"redirect_uri">>, list_to_binary(prism_config:redirect_uri())}]),

    Request = {URL, Headers, "application/json", Body},

    {ok, {{_Version, 200, _Reason}, _Headers, ResponseBody}} = httpc:request(post, Request, [], []),
    JSON = jsx:decode(list_to_binary(ResponseBody)),

    [Data] = proplists:get_value(<<"data">>, JSON),

    Token = proplists:get_value(<<"access_token">>, Data),
    Expires = proplists:get_value(<<"expires_in">>, Data),
    RefreshToken = proplists:get_value(<<"refresh_token">>, Data),


    #state{token = Token,
            expiration = Expires,
            refresh_token = RefreshToken,
            last_fetch_time = erlang:now() }.


auth_header() ->
    Concat = prism_config:client_id() ++ ":" ++ prism_config:client_secret(),
    Base64 = base64:encode_to_string(Concat),
    {"Authorization", "Basic " ++ Base64}.

fetch_auth_code() ->
    URL = prism_config:base_url_string() ++ "/oauth2/authorize?" ++ "response_type=code" ++ "&client_id=" ++ prism_config:client_id() ++ "&redirect_uri=" ++ prism_config:redirect_uri(),
    Headers = [auth_header()],
    Request = {URL, Headers},
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(get, Request, [], []),
    JSON = jsx:decode(list_to_binary(Body)),

    [Data] = proplists:get_value(<<"data">>, JSON),

    proplists:get_value(<<"authorization_code">>, Data).