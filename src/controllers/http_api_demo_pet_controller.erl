-module(http_api_demo_pet_controller).
-export([
         get_pets/1,
         get_pet/1,
         create_pet/1,
         update_pet/1,
         remove_pet/1
        ]).

get_pets(_) ->
    List = ets:tab2list(pets),
    Body = [#{<<"id">> => Id,
              <<"name">> => Name} ||{Id, Name} <- List],
    {json, 200, #{}, Body}.

get_pet(#{bindings := #{<<"petid">> := PetId}}) ->
    case ets:lookup(pets, PetId) of
        [] ->
            {status, 404};
        [{PetId, Name}] ->
            {json, 200, #{}, #{<<"id">> => PetId,
                               <<"name">> => Name}}
    end.

create_pet(#{json := #{<<"name">> := Name}}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),  
    true = ets:insert(pets, {Id, Name}),
    {json, 201, #{}, #{<<"id">> => Id, <<"name">> => Name}}.

update_pet(#{bindings := #{<<"petid">> := PetId},
             json := #{<<"name">> := Name}}) ->
    case uuid:is_v4(uuid:string_to_uuid(PetId)) of
        true ->
            true = ets:delete(pets, PetId),
            true = ets:insert(pets, {PetId, Name}),
            {json, 200, #{}, #{<<"id">> => PetId,
                               <<"name">> => Name}};
        false ->
            {status, 400}
    end.

remove_pet(#{bindings := #{<<"petid">> := PetId}}) ->
    true = ets:delete(pets, PetId),
    {status, 200}.
