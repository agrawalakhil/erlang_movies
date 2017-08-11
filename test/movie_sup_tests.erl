-module(movie_sup_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_supervisor tests
%%--------------------------------------------------------------------
movie_sup_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end,
      fun(Pid) -> fun() -> server_is_registered(Pid) end end,
      fun(Pid) -> fun() -> server_started(Pid) end end,
      fun(Pid) -> fun() -> server_restarted(Pid) end end
     ]}.

setup() ->    
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = movie_sup:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

server_is_registered(Pid) ->
    ?assertEqual(Pid, whereis(movie_sup)).

server_started(_Pid) ->    
    ?assertEqual(true, is_process_alive(whereis(movie_server))).

server_restarted(_Pid) ->
    exit(whereis(movie_server), kill),
    timer:sleep(100),
    ?assertEqual(true, is_process_alive(whereis(movie_server))).   

cleanup(Pid) ->    
    ?debugMsg("cleanup"),
    exit(Pid, shutdown),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    ?assertEqual(false, is_process_alive(Pid)).
