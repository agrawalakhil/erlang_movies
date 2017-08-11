%%%-------------------------------------------------------------------
%% @doc movie public API
%% @end
%%%-------------------------------------------------------------------

-module(movie_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/api/movie/register", movie_rest_handler, [register]},
					     {"/api/movie/reserve", movie_rest_handler, [reserve]},
					     {"/api/movie/retrieve", movie_rest_handler, [retrieve]},
					     {"/", cowboy_static, {file, "priv/www/index.html", [{mimetypes, cow_mimetypes, all}]}}
					    ]}
				     ]),
    {ok, _} = cowboy:start_http(my_http_listener, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]
			       ),
    movie_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
