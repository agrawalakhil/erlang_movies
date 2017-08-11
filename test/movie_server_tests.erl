-module(movie_server_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_server tests
%%--------------------------------------------------------------------
movie_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end,
      fun(Pid) -> fun() -> server_is_registered(Pid) end end,
      %% register tests
      fun(Pid) -> fun() -> register_ok(Pid) end end,
      fun(Pid) -> fun() -> register_exists(Pid) end end,
      %% reserve tests
      fun(Pid) -> fun() -> reserve_ok(Pid) end end,
      fun(Pid) -> fun() -> reserve_not_exists(Pid) end end,
      fun(Pid) -> fun() -> reserve_not_available(Pid) end end,
      %% retrieve tests
      fun(Pid) -> fun() -> retrieve_ok(Pid) end end,
      fun(Pid) -> fun() -> retrieve_not_exists(Pid) end end,
      fun(Pid) -> fun() -> retrieve_after_reserve(Pid) end end
     ]}.

setup() ->    
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = movie_server:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).
server_is_registered(Pid) ->
    ?assertEqual(Pid, whereis(movie_server)).

register_ok(_Pid) ->    
    ?assertEqual(ok, movie_server:register("tt0111161", "screen_123456", 100)).
register_exists(_Pid) ->
    movie_server:register("tt0111161", "screen_123456", 100),
    ?assertEqual({error, exists}, movie_server:register("tt0111161", "screen_123456", 100)).

reserve_ok(_Pid) ->
    movie_server:register("tt0111161", "screen_123456", 100),
    ?assertEqual(ok, movie_server:reserve("tt0111161", "screen_123456")).   
reserve_not_exists(_Pid) ->
    ?assertEqual({error, not_exists}, movie_server:reserve("tt0111161", "screen_123456")).
reserve_not_available(_Pid) ->    
    movie_server:register("tt0111161", "screen_123456", 100),
    lists:foreach(fun(_Seq) -> 
			  movie_server:reserve("tt0111161", "screen_123456")
		  end, lists:seq(1, 100)),
    ?assertEqual({error, not_available}, movie_server:reserve("tt0111161", "screen_123456")).

retrieve_ok(_Pid) ->
    movie_server:register("tt0111161", "screen_123456", 100),
    ?assertEqual({ok, {movie, "tt0111161", "screen_123456", "tt0111161:screen_123456", 100, 0}}, 
		 movie_server:retrieve("tt0111161", "screen_123456")).
retrieve_not_exists(_Pid) ->
    ?assertEqual({error, not_exists}, movie_server:retrieve("tt0111161", "screen_123456")).
retrieve_after_reserve(_Pid) ->
    movie_server:register("tt0111161", "screen_123456", 100),
    ?assertEqual({ok, {movie, "tt0111161", "screen_123456", "tt0111161:screen_123456", 100, 0}},
                 movie_server:retrieve("tt0111161", "screen_123456")),
    movie_server:reserve("tt0111161", "screen_123456"),
    ?assertEqual({ok, {movie, "tt0111161", "screen_123456", "tt0111161:screen_123456", 100, 1}},
                 movie_server:retrieve("tt0111161", "screen_123456")).

cleanup(Pid) ->    
    ?debugMsg("cleanup"),
    exit(Pid, normal),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    ?assertEqual(false, is_process_alive(Pid)).
