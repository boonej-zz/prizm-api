%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2014 1:27 PM
%%%-------------------------------------------------------------------
-module(post_server).
-author("joeconway").

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/7, start/7]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Network, PostID, UserID, Caption, ImageThumb, ImageLow, ImageHigh) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Network, PostID, UserID, Caption, ImageThumb, ImageLow, ImageHigh], []).

start(Network, PostID, UserID, Caption, ImageThumb, ImageLow, ImageHigh) ->
    supervisor:start_child(prism_post_supervisor, [Network, PostID, UserID, Caption, ImageThumb, ImageLow, ImageHigh]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(State) ->
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    ok = erlcloud_s3:configure(prism_config:s3_id(), prism_config:s3_key(), prism_config:s3_url(), 443),
    start_processing_post(State),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_processing_post([Network, PostID, UserID, Caption, ImageThumbURL, ImageLowURL, ImageHighURL]) ->

    PrismUserID = "5331c37af55aea9d360bac29",

    {ok, {{_Version, 200, _Reason}, _Headers, ImageHigh}} = httpc:request(ImageHighURL),
    {ok, {{_Version, 200, _Reason}, _Headers, ImageLow}} = httpc:request(ImageLowURL),
    {ok, {{_Version, 200, _Reason}, _Headers, ImageThumb}} = httpc:request(ImageThumbURL),

    BaseName = timestamp_string() ++ unique_id_string(),

    upload_image(ImageHigh, PrismUserID ++ "/" ++ BaseName ++ ".jpg"),
    upload_image(ImageLow, PrismUserID ++ "/" ++ BaseName ++ "_2.jpg"),
    upload_image(ImageThumb, PrismUserID ++ "/" ++ BaseName ++ "_4.jpg"),

    FullPath = "https://" ++ prism_config:s3_url() ++ "/" ++ prism_config:s3_bucket() ++ "/" ++ PrismUserID ++ "/" ++ BaseName ++ ".jpg",

    Token = binary_to_list(prism_auth:obtain_token()),
    URL = prism_config:base_url_string() ++ "/users/" ++ PrismUserID ++ "/posts",

    Body = jsx:encode([
        {<<"text">>, <<"test">>},
        {<<"creator">>, list_to_binary(PrismUserID)},
        {<<"category">>, <<"experience">>},
        {<<"scope">>, <<"public">>},
        {<<"provider">>, Network},
        {<<"provider_id">>, PostID},
        {<<"file_path">>, list_to_binary(FullPath)}
    ]),

    Headers = [{"Authorization", "Bearer " ++ Token}],

    Request = {URL, Headers, "application/json", Body},

    {ok, {{_Version, 200, _Reason}, _Headers, _ResponseBody}} = httpc:request(post, Request, [], []).

upload_image(ImageData, []) ->
    ok;
upload_image(ImageData, Path) ->
    erlcloud_s3:put_object(prism_config:s3_bucket(), Path, ImageData, [{acl, public_read}]).

timestamp_string() ->
    ok.

unique_id_string() ->
    ok.