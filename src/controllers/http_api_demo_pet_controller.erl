-module(http_api_demo_pet_controller).
-export([manage_pet/1]).

manage_pet(#{req := #{method := <<"GET">>,
                      bindings := #{petid := PetId}}}) ->
    case http_api_demo_db:get_pet(PetId) of
        undefined ->
            {status, 404};
        {ok, PetMap} ->
            {json, 200, #{}, PetMap}
    end;
manage_pet(#{req := #{method := <<"PUT">>,
                      bindings := #{petid := PetId}},
             json := #{<<"name">> := Name}}) ->
    {ok, _} = http_api_demo_db:update_pet(PetId, Name),
    {json, 200, #{}, #{<<"id">> => PetId, <<"name">> => Name}};
manage_pet(#{req := #{method := <<"DELETE">>,
                      bindings := #{petid := PetId}}}) ->
    ok = http_api_demo_db:delete_pet(PetId),
    {status, 200}; 
manage_pet(#{req := #{method := <<"GET">>}}) ->
    case http_api_demo_db:get_pets() of
        {ok, PetList} ->

            {json, 200, #{}, PetList};
        _ ->
            {status, 500}
    end;
manage_pet(#{req := #{method := <<"POST">>},
             json := #{<<"name">> := Name}}) ->
    Id = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    http_api_demo_db:create_pet(Id, Name),
    {json, 201, #{}, #{<<"id">> => Id, <<"name">> => Name}}.
