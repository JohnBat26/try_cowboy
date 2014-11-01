-module(session_manager).

-export([init/0, get_user/1, get_random_session_id/0, generate_session_id/0]).

init() ->
  ets:new(sessions, [set, public, named_table]),
  ets:insert(sessions, {generate_session_id(), <<"Bob">>, 25}),
  ets:insert(sessions, {generate_session_id(), <<"Bill">>, 22}),
  ets:insert(sessions, {generate_session_id(), <<"John">>, 35}),
  ets:insert(sessions, {generate_session_id(), <<"Helen">>, 15}),
  ets:insert(sessions, {generate_session_id(), <<"David">>, 45}),
  ets:insert(sessions, {generate_session_id(), <<"Mark">>, 27}),
  ok.

get_user(SessionId) ->
  case ets:lookup(sessions, SessionId) of
    [User] -> {ok, User};
    [] -> {error, not_found}
  end.


get_random_session_id() ->
  Res = ets:match(sessions, {'$1', '_', '_'}),
  Res2 = lists:map(fun([Id]) -> Id end, Res),
  Index = crypto:rand_uniform(1, length(Res2)),
  lists:nth(Index, Res2).

generate_session_id() ->
  Str = integer_to_list(erlang:phash2({now(), make_ref()})),
  <<Hash:128/integer>> = erlang:md5(Str),
  list_to_binary(string:to_lower(integer_to_list(Hash, 16))).