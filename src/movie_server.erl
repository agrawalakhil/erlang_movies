-module(movie_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([register/3, reserve/2, retrieve/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {}).
-record(movie, {imdbId,
		screenId,
		movieTitle,
		availableSeats = 0 :: integer(),
		reservedSeats = 0 :: integer()}).

start_link() ->  
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% API Functions
%%====================================================================
register(IMDBId, ScreenId, AvailableSeats) ->
    gen_server:call(?MODULE, {register, IMDBId, ScreenId, AvailableSeats}).

reserve(IMDBId, ScreenId) ->
    gen_server:call(?MODULE, {reserve, IMDBId, ScreenId}).

retrieve(IMDBId, ScreenId) ->    
    gen_server:call(?MODULE, {retrieve, IMDBId, ScreenId}).
%%====================================================================
%% Gen Server Functions
%%====================================================================
init(_Args) ->
    process_flag(trap_exit, true),
    %% error_logger:info_msg("[movie_server] starting with pid ~p and args ~p~n", [self(), Args]),
    MoviesFileName = application:get_env(movie, movies_file_name, "movies.dets"),
    {ok, movies_db} = dets:open_file(movies_db, [{file, MoviesFileName}, {type, set}]),
    ok = dets:delete_all_objects(movies_db),
    %% error_logger:info_msg("[movie_server] opened dets table movies_db for file ~p~n", [MoviesFileName]),
    {ok, #state{}}.

handle_call({register, IMDBId, ScreenId, AvailableSeats}, _From, State) ->    
    {reply, register_movie(IMDBId, ScreenId, AvailableSeats), State};
handle_call({reserve, IMDBId, ScreenId}, _From, State) -> 
    {reply, reserve_movie(IMDBId, ScreenId), State};
handle_call({retrieve, IMDBId, ScreenId}, _From, State) ->    
    {reply, retrieve_movie(IMDBId, ScreenId), State}.

handle_cast(_Req, State) ->
    {noreply, State}.
	
handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_message("[movie_server] exiting process ~p with reason ~p~n", [Pid, Reason]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    dets:close(movies_db),
    %% error_logger:info_msg("[movie_server] terminating~n"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================
register_movie(IMDBId, ScreenId, AvailableSeats) ->
    try
	Movie = #movie{imdbId=IMDBId, 
		       screenId=ScreenId,
		       movieTitle=IMDBId ++ ":" ++ ScreenId,
		       availableSeats=AvailableSeats},
	case dets:lookup(movies_db, {IMDBId, ScreenId}) of
	    [] ->  ok = dets:insert(movies_db, {{IMDBId, ScreenId}, Movie}),
		   %% error_logger:info_msg("[movie_server] (call register_movie) movie: ~p~n", [Movie]),
		   dets:sync(movies_db);
	    _ -> {error, exists}
	end
    catch
	EClass:EReason -> 
	    error_logger:info_msg("[movie_server] error while registering movie ~p:~p~n~p~n", [EClass, EReason, erlang:get_stacktrace()]),
	    {error, EReason}
    end.
reserve_movie(IMDBId, ScreenId) ->
    case dets:lookup(movies_db, {IMDBId, ScreenId}) of
	[{_MovieId, Movie}] -> try
		       case Movie#movie.reservedSeats >= Movie#movie.availableSeats of
			   true -> {error, not_available};
			   false ->
			       ok = dets:insert(movies_db, {{IMDBId, ScreenId}, 
							    Movie#movie{reservedSeats=Movie#movie.reservedSeats+1}}), 
			       %% error_logger:info_msg("[movie_server] (call reserve_movie) movie: ~p~n", [Movie]),
			       dets:sync(movies_db)
		       end
		   catch
		       EClass:EReason -> 
			   io:fwrite("Error while reserving movie ~p:~p~n~p~n", [EClass, EReason, erlang:get_stacktrace()]),
			   {error, erlang:get_stacktrace()}
		   end;
	[] -> {error, not_exists}
    end.

retrieve_movie(IMDBId, ScreenId) ->        
    case dets:lookup(movies_db, {IMDBId, ScreenId}) of
	[{_MovieId, Movie}] -> %% error_logger:info_msg("[movie_server] (call retrieve_movie) for movie: ~p~n", [Movie]),
			       {ok, Movie};
	[] -> {error, not_exists};
	Error -> {error, Error}
    end.
