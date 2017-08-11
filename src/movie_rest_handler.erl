% @doc Movie REST handler.
-module(movie_rest_handler).

-export([init/3,
	 rest_init/2,
	 is_authorized/2,
         allowed_methods/2,
         content_types_accepted/2]).

-export([json_to_movie/2, pre_encode_cb/1]).

-record(state, {op}).

init(_, Req, Opts) ->    
    {upgrade, protocol, cowboy_rest, Req, Opts}.
rest_init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {ok, Req, State}.

is_authorized(Req, State) ->    
    case cowboy_req:parse_header(<<"authorization">>, Req) of
	{ok, {<<"basic">>, {<<"akhil@example.net">>, <<"secret">>}}, Req1} -> {true, Req1, State};
	Auth -> error_logger:info_msg("auth ~p~n", [Auth]),
		{{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

allowed_methods(Req, State) ->    
    Methods = [<<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, json_to_movie}], Req, State}.

json_to_movie(Req, #state{op=Op} = State) ->
    {ok, ReqBody, Req1} = cowboy_req:body(Req),
    ReqJson = jsx:decode(ReqBody),   
    error_logger:info_msg("request body received ~p~n", [ReqJson]),
    Response = case Op of
		   register -> 
		       case check_fields(ReqJson, [<<"imdbId">>, <<"screenId">>, <<"availableSeats">>]) of
			   [{<<"imdbId">>, ImdbId},
			    {<<"screenId">>, ScreenId},
			    {<<"availableSeats">>, AvailableSeats}] ->  movie_server:register(binary_to_list(ImdbId), 
											      binary_to_list(ScreenId), 
											      AvailableSeats);
			   _ -> {error, missing_fields}
		       end;
		   reserve -> case check_fields(ReqJson, [<<"imdbId">>, <<"screenId">>]) of
				  [{<<"imdbId">>, ImdbId},
				   {<<"screenId">>, ScreenId}] -> movie_server:reserve(binary_to_list(ImdbId), 
										       binary_to_list(ScreenId));
				  _ -> {error, missing_fields}
			      end;
		   retrieve -> case check_fields(ReqJson, [<<"imdbId">>, <<"screenId">>]) of
                                  [{<<"imdbId">>, ImdbId},
                                   {<<"screenId">>, ScreenId}] -> movie_server:retrieve(binary_to_list(ImdbId), 
											binary_to_list(ScreenId));
				   _ -> {error, missing_fields}
			       end
	       end,
    error_logger:info_msg("sending response ~p~n", [Response]),
    ResBody = jsx:encode(Response, [{pre_encode, fun movie_rest_handler:pre_encode_cb/1}]),
    Res1 = cowboy_req:set_resp_body(ResBody, Req1),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
    {true, Res3, State}.

check_fields(Json, Fields) ->
    lists:map(fun(Field) -> lists:keyfind(Field, 1, Json) end, Fields).

pre_encode_cb(Term) ->
    case Term of
	ok -> <<"ok">>;
	{error, Error} -> [{<<"error">>, iolist_to_binary(atom_to_list(Error))}];
	{ok, Res} -> [{<<"ok">>, lists:zip([<<"imdbId">>, <<"screenId">>, <<"movieTitle">>, <<"availableSeats">>, <<"reservedSeats">>], 
					   lists:map(fun(Value) ->
							     case is_list(Value) of
								 true -> iolist_to_binary(Value);
								 false -> Value
							     end
						     end, tl(tuple_to_list(Res))))}];
	_ -> Term			 
    end.
