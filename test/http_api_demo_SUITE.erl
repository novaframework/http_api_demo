-module(http_api_demo_SUITE).

-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").

-define(OPTS, #{close => true,
                headers => #{'Content-Type' => <<"application/json">>}}).
-define(BASE_URL, <<"http://localhost:8080/pet">>).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Json = json:encode(#{<<"name">> => <<"Hades">>}, [maps, binary]),
    case shttpc:post(?BASE_URL, Json, ?OPTS) of
        #{status := {201, _}, body := Body} ->
            #{<<"id">> := Id} = json:decode(Body, [maps]),
            [{id, Id}, {name, <<"Hades">>} | Config]
    end.

end_per_suite(Config) ->
    Id = proplists:get_value(id, Config),
    shttpc:delete(<<?BASE_URL/binary, "/", Id/binary>>, ?OPTS),
    ok.



groups() -> [{pet, [sequence], [get_pet,
                                get_all_pets,
                                change_name]}].

all() ->
    [{group, pet}].

get_pet(Config) ->
    Id = proplists:get_value(id, Config),
    Name = proplists:get_value(name, Config),
    case shttpc:get( [?BASE_URL, "/", Id], ?OPTS) of
        #{status := {200, _}, body := Body} ->
            #{<<"id">> := Id,
              <<"name">> := Name} = json:decode(Body, [maps])
    end.

get_all_pets(Config) ->
    Id = proplists:get_value(id, Config),
    Name = proplists:get_value(name, Config),
    case shttpc:get([?BASE_URL], ?OPTS) of
        #{status := {200, _}, body := Body} ->
            [#{<<"id">> := Id,
               <<"name">> := Name}] = json:decode(Body, [maps])
    end.

change_name(Config) ->
    Id = proplists:get_value(id, Config),
    Name = <<"Pony">>,
    Json = json:encode(#{<<"name">> => Name}, [maps, binary]),
    case shttpc:put([?BASE_URL, "/", Id], Json, ?OPTS) of
        #{status := {200, _}, body := Body} ->
            #{<<"id">> := Id,
              <<"name">> := Name} = json:decode(Body, [maps])
    end.
