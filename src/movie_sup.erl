%%%-------------------------------------------------------------------
%% @doc movie top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(movie_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(_Module, _Args) ->
    start_link().

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
    error_logger:info_msg("[movie_sup] starting with pid ~p and args ~p~n", [self(), Args]),
    SupFlags = {one_for_one, 1, 5},
    ChildSpecs = [{movie_server_spec,
                   {movie_server, start_link, []},
		   permanent,
		   brutal_kill,
		   worker,
		   [movie_server]}],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
