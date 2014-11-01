-module(my_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init({tcp, http}, Req, Opts) ->
  {ok, Req, undefined_state}.


handle(Req, State) ->
  {SessionCookie, _} = cowboy_req:cookie(<<"session_id">>, Req),
  {Req3, Body} =
    case SessionCookie of
      undefined -> {create_session(Req), "I don't know req"};
      SessionId ->
        case session_manager:get_user(SessionId) of
          {ok, {_, Name, _}} ->
            Reply = io_lib:format("You are ~p~n", [Name]),
            {Req, Reply};
          {error, not_found} ->
            {create_session(Req), "I have forgotten you"}
        end
    end,

  Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],

  {ok, Req4} = cowboy_req:reply(200, Headers, Body, Req3),
  {ok, Req4, State}.

terminate(Reason, Req, State) ->
  ok.

create_session(Req) ->
  SessionId = session_manager:get_random_session_id(),
  CookieProps = [{path, <<"/">>}, {max_age, 30 * 24 * 3600}], %30 days
  cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, CookieProps, Req).