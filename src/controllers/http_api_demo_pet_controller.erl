-module(http_api_demo_pet_controller).
-export([get_pet/1,
         update_pet/1,
         remove_pet/1,
         get_pets/1,
         create_pet/1]).

get_pet(#{bindings := #{<<"petid">> := PetId}}) ->
    case ets:lookup(pets, PetId) of
        [] ->
            {status, 404};
        [{PetId, Name}] ->
            {json, 200, #{}, #{<<"id">> => PetId,
                               <<"name">> => Name}}
    end.

update_pet(#{bindings := #{<<"petid">> := PetId},
             json := #{<<"name">> := Name}}) ->
    true = ets:delete(pets, PetId),
    true = ets:insert(pets, {PetId, Name}),
    {json, 200, #{}, #{<<"id">> => PetId,
                       <<"name">> => Name}}.

remove_pet(#{bindings := #{<<"petid">> := PetId}}) ->
    true = ets:delete(pets, PetId),
    {status, 200}.

get_pets(_) ->
    List = ets:tab2list(pets),
    Body = [#{<<"id">> => Id,
              <<"name">> => Name} ||{Id, Name} <- List],
    {json, 200, #{}, Body}.

create_pet(#{json := #{<<"name">> := Name}}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),  
    true = ets:insert(pets, {Id, Name}),
    {json, 201, #{}, #{<<"id">> => Id, <<"name">> => Name}}.
