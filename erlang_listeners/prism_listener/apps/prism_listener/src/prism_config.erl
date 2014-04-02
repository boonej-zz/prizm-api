%%%-------------------------------------------------------------------
%%% @author joeconway
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2014 3:03 PM
%%%-------------------------------------------------------------------
-module(prism_config).
-author("joeconway").

-define(BASE_URL_STRING, "https://ec2-54-186-28-238.us-west-2.compute.amazonaws.com").
-define(PRISM_REDIRECT_URI, "https://ec2-54-200-41-62.us-west-2.compute.amazonaws.com/callback").

-define(PRISM_CLIENT_SECRET, "f27198fb-689d-4965-acb0-0e9c5f61ddec").
-define(PRISM_CLIENT_ID, "67e1fe4f-db1b-4d5c-bdc7-56270b0822e2").

-define(S3_KEY, "27F1TqpcdXOzi6DLmt9U4LCdlI71EhtwhClX0XMl").
-define(S3_ID, "AKIAI7E5TSPROBCA4YWA").
-define(S3_URL, "s3.amazonaws.com").
-define(S3_BUCKET, "higheraltitude.prism").

%{ok, Data} = file:read_file("/Users/joeconway/Projects/prism_api/node_modules/winston/node_modules/request/tests/unicycle.jpg").

%% API
-export([base_url_string/0, redirect_uri/0, client_secret/0, client_id/0,
         s3_key/0, s3_id/0, s3_url/0, s3_bucket/0]).

base_url_string() ->
    ?BASE_URL_STRING.

redirect_uri() ->
    ?PRISM_REDIRECT_URI.

client_secret() ->
    ?PRISM_CLIENT_SECRET.

client_id() ->
    ?PRISM_CLIENT_ID.

s3_key() ->
    ?S3_KEY.
s3_id() ->
    ?S3_ID.
s3_url() ->
    ?S3_URL.
s3_bucket() ->
    ?S3_BUCKET.