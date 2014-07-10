%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2014 11:32 AM
%%%-------------------------------------------------------------------
-module(prism_instagram_process).
-author("joeconway").

-behaviour(gen_server).

%% API
-export([start_link/0, tag_updated/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(INSTAGRAM_CLIENT_ID, "9fd051f75f184a95a1a4e934e6353ae7").

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tag_updated() ->
    gen_server:cast(?SERVER, tag_updated).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(tag_updated, LastMinID) ->
    NewState = fetch_new_posts(LastMinID),
    {noreply, NewState};
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
fetch_new_posts(LastMinID) ->
    URL = url_for_last_min_id(LastMinID),

    {ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(URL),
    JSON = jsx:decode(list_to_binary(Body)),

    Pagination = proplists:get_value(<<"pagination">>, JSON),
    NewMinID = proplists:get_value(<<"min_tag_id">>, Pagination, LastMinID),

    Grams = proplists:get_value(<<"data">>, JSON, []),

    process_grams(Grams),

    NewMinID.

%

url_for_last_min_id([]) ->
    "https://api.instagram.com/v1/tags/prism/media/recent?client_id=" ++ ?INSTAGRAM_CLIENT_ID;
url_for_last_min_id(LastMinID) ->
    "https://api.instagram.com/v1/tags/prism/media/recent?client_id=" ++ ?INSTAGRAM_CLIENT_ID ++ "&min_tag_id="++ binary_to_list(LastMinID).

process_grams([]) ->
    ok;
process_grams(Data) ->
    [Gram | Tail] = Data,
    process_gram(Gram),
    process_grams(Tail).

process_gram(Gram) ->
    case proplists:get_value(<<"type">>, Gram) of
        <<"image">> ->
            process_image_gram(Gram);
        _ -> ok
    end.

process_image_gram(Gram) ->
    PostID = proplists:get_value(<<"id">>, Gram),

    Caption = proplists:get_value(<<"caption">>, Gram, <<"">>),
    CaptionText = proplists:get_value(<<"text">>, Caption, <<"">>),

    User = proplists:get_value(<<"user">>, Gram),
    UserID = proplists:get_value(<<"id">>, User),

    Images = proplists:get_value(<<"images">>, Gram),
    ImageThumbnailURL = proplists:get_value(<<"url">>, proplists:get_value(<<"thumbnail">>, Images)),
    ImageLowURL = proplists:get_value(<<"url">>, proplists:get_value(<<"low_resolution">>, Images)),
    ImageNormalURL = proplists:get_value(<<"url">>, proplists:get_value(<<"standard_resolution">>, Images)),

    post_server:start(<<"Instagram">>, PostID, UserID, CaptionText, ImageThumbnailURL, ImageLowURL, ImageNormalURL).