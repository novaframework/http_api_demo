-module(http_api_demo_pet_controller).
-export([manage_pet/1]).

manage_pet(#{req := #{method := <<"GET">>,
                      bindings := #{petid := PetId}}}) ->
    case ets:lookup(pets, PetId) of
        [] ->
            {status, 404};
        [{PetId, Name}] ->
            {status, 200, #{}, json:encode(#{<<"id">> => PetId,
                                             <<"name">> => Name}, [maps, binary])}
    end;
manage_pet(#{req := #{method := <<"PUT">>,
                      bindings := #{petid := PetId}},
             json := #{<<"name">> := Name}}) ->
    true = ets:delete(pets, PetId),
    true = ets:insert(pets, {PetId, Name}),
    {status, 200, #{}, json:encode(#{<<"id">> => PetId,
                                     <<"name">> => Name}, [maps, binary])};
manage_pet(#{req := #{method := <<"DELETE">>,
                      bindings := #{petid := PetId}}}) ->
    true = ets:delete(pets, PetId),
    {status, 200}; 
manage_pet(#{req := #{method := <<"GET">>}}) ->
    List = ets:tab2list(pets),
    Body = [#{<<"id">> => Id,
              <<"name">> => Name} ||{Id, Name} <- List],
    {status, 200, #{}, json:encode(Body, [maps, binary])};
manage_pet(#{req := #{method := <<"POST">>},
             json := #{<<"name">> := Name}}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),  
    true = ets:insert(pets, {Id, Name}),
    {status, 201, #{}, json:encode(#{<<"id">> => Id, <<"name">> => Name}, [maps, binary])}.
