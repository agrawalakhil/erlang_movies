# erlang_movies
Erlang movie ticket reservation system
Implement a ticket reservation system for movies. The interactions with the system should be http/json based.

Following functionalities are implemented
* REST API using cowboy web server
* Data storage using mnesia 

Unit Tests are provided for all the code modules to run and test. To compile, run the tests and examples, following steps needs to be followed

* `$ install erlang`
* `$ rebar get-deps`
* `$ rebar compile`
* `$ rebar eunit`

Clean run of the tests or specific suite

* `$ rebar get-deps clean compile eunit`
* `$ rebar clean compile eunit suite=movie_server`

To run the application

* `$ erl -pa ebin -pa deps/cowboy/ebin/ -pa deps/cowlib/ebin/ -pa deps/ranch/ebin/`
* `application:ensure_all_started(movie).`
* `$ curl http://localhost:8080/`

API
Following API end points are implemented
* Register a movie
* Reserve a seat at the movie
* Retrieve information about the movie

To call the API using akhil@example.net:secret as basic authentication.

    POST /api/movie/register HTTP/1.1
    Host: example.net
    Authorization: Basic YWtoaWxAZXhhbXBsZS5uZXQ6c2VjcmV0
    Content-Type: application/json
    Content-Length: 75
    
    {"imdbId": "tt0111161", "availableSeats": 100, "screenId": "screen_123456"}

When the request is authorized, the result of the API is converted to JSON. A reply from the server could look like this:

    HTTP/1.1 200 OK
    Content-Length: 4
    Content-Type: application/json
     
    "ok"

# Requests

      Request type    | Key            | Description
     ==========================================================================
                      | imdbId         | IMDB movie identifier
      register        | availableSeats | Total seats available for this movie
                      | screenId       | Externally managed identifier of
                      |                | when and where the movie is screened
     -----------------+----------------+---------------------------------------
                      | imdbId         | IMDB movie identifier
      reserve         | screenId       | Externally managed identifier of
                      |                | when and where the movie is screened
     -----------------+----------------+---------------------------------------
                      | imdbId         | IMDB movie identifier
      retrieve        | screenId       | Externally managed identifier of
                      |                | when and where the movie is screened 
     -----------------+----------------+---------------------------------------

# Responses

      Request type | Response                       | Description
     ===========================================================================
                   | ok                             | Registration was successful
                   | {error, missing_fields}        | Fields missing in the request
      register     | {error, not_allowed}           | Registration is not allowed
                   | {error, exists}                | Movie already exists
                   | {error, Error}                 | Registration failed
     --------------+--------------------------------+-------------------------------
                   | ok                             | Reservation was successful
				   | {error, missing_fields}        | Fields missing in the request
      reserve      | {error, not_exists}            | Movie was not registered
                   | {error, not_allowed}           | Reservation is not allowed
				   | {error, not_available}         | Seat not available for movie
                   | {error, Error}                 | Reservation failed
     --------------+--------------------------------+-------------------------------
                   | {ok,                           | Retrieval was successful
                   				   |  {"imdbId": "tt0111161",       | IMDB movie identifier
				   |   "screenId": "screen_123456", | Externally managed identifier
				   |   "movieTitle": "The Movie",   | Movie title
				   |   "availableSeats": 100,       | Total seats available
				   |   "reservedSeats": 50          | Total seats reserved
				   |  }                             |
	               | }                              |
			       | {error, missing_fields}        | Fields missing in the request
	  retrieve	   | {error, not_exists}            | Movie was not registered
                   | {error, not_allowed}           | Retrieval is not allowed
                   | {error, Error}                 | Retrieval failed
     --------------+--------------------------------+-------------------------------


In JSON the form would be either a string, or a map. `ok` would be `"ok"`, and
`{error, not_allowed}` would be `{"error":"not_allowed"}`.

# Test using curl
### Register Movie:
$ curl -X POST -d '{"imdbId": "tt0111161", "availableSeats": 100, "screenId": "screen_123456"}' -H "Authorization: Basic YWtoaWxAZXhhbXBsZS5uZXQ6c2VjcmV0" -H "Content-Type: application/json" http://localhost:8080/api/movie/register

### Reserve Seat:
$ curl -X POST -d '{"imdbId": "tt0111161", "screenId": "screen_123456"}' -H "Authorization: Basic YWtoaWxAZXhhbXBsZS5uZXQ6c2VjcmV0" -H "Content-Type: application/json" http://localhost:8080/api/movie/reserve

### Retrieve Movie:
$ curl -X POST -d '{"imdbId": "tt0111161", "screenId": "screen_123456"}' -H "Authorization: Basic YWtoaWxAZXhhbXBsZS5uZXQ6c2VjcmV0" -H "Content-Type: application/json" http://localhost:8080/api/movie/retrieve
