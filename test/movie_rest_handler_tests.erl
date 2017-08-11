-module(movie_rest_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_supervisor tests
%%--------------------------------------------------------------------
movie_rest_handler_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      %% authorization tests
      fun({Pid, Req}) -> fun() -> basic_auth_pass(Pid, Req) end end,
      fun({Pid, Req}) -> fun() -> basic_auth_fail(Pid, Req) end end,
      %% missing_fields
      fun({Pid, Req}) -> fun() -> register_missing_fields(Pid, Req) end end,
      fun({Pid, Req}) -> fun() -> reserve_missing_fields(Pid, Req) end end,
      fun({Pid, Req}) -> fun() -> retrieve_missing_fields(Pid, Req) end end
     ]}.

setup() ->    
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = movie_server:start_link(),
    Req = meck:new(cowboy_req),
    meck:expect(cowboy_req, set_resp_body, fun(ResBody, _Req1) -> ResBody end),
    meck:expect(cowboy_req, delete_resp_header, fun(_Header, Req1) -> Req1 end),    
    meck:expect(cowboy_req, set_resp_header, fun(_Header, _Value, Req1) -> Req1 end),    
    {Pid, Req}.

basic_auth_pass(_Pid, Req) ->
    {ok, Req, State} = movie_rest_handler:rest_init(Req, [register]),
    meck:expect(cowboy_req, parse_header, fun(_Header, Request) -> 
						  {ok, {<<"basic">>, {<<"akhil@example.net">>, <<"secret">>}}, Request}
					  end),
    ?assertEqual({true, Req, State}, movie_rest_handler:is_authorized(Req, State)).
    
basic_auth_fail(_Pid, Req) ->
    {ok, Req, State} = movie_rest_handler:rest_init(Req, [register]),
    meck:expect(cowboy_req, parse_header, fun(_Header, Request) -> 
                                                  {ok, {<<"basic">>, {<<"akhil@example.net">>, <<"wrong">>}}, Request}
                                          end),
    ?assertEqual({{false, <<"Basic realm=\"cowboy\"">>}, Req, State}, movie_rest_handler:is_authorized(Req, State)).

register_missing_fields(_Pid, Req) ->
    {ok, Req, State} = movie_rest_handler:rest_init(Req, [register]),
    meck:expect(cowboy_req, body, fun(Request) -> 
					  {ok, <<"{\"imdbId\": \"tt0111161\", \"availableSeats\": 100}">>, Request}
				  end),
    ?assertEqual({true, <<"{\"error\":\"missing_fields\"}">>, State}, movie_rest_handler:json_to_movie(Req, State)).
reserve_missing_fields(_Pid, Req) ->
    {ok, Req, State} = movie_rest_handler:rest_init(Req, [reserve]),
    meck:expect(cowboy_req, body, fun(Request) -> 
                                          {ok, <<"{\"imdbId\": \"tt0111161\"}">>, Request}
                                  end),
    ?assertEqual({true, <<"{\"error\":\"missing_fields\"}">>, State}, movie_rest_handler:json_to_movie(Req, State)).
retrieve_missing_fields(_Pid, Req) ->
    {ok, Req, State} = movie_rest_handler:rest_init(Req, [retrieve]),
    meck:expect(cowboy_req, body, fun(Request) -> 
                                          {ok, <<"{\"screenId\": \"screen_123456\"}">>, Request}
                                  end),
    ?assertEqual({true, <<"{\"error\":\"missing_fields\"}">>, State}, movie_rest_handler:json_to_movie(Req, State)).
		     
cleanup({Pid, _Req}) ->    
    ?debugMsg("cleanup"),
    exit(Pid, normal),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    meck:unload(cowboy_req),
    ?assertEqual(false, is_process_alive(Pid)).
