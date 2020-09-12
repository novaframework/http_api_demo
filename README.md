# http api server #

## Introduction ##

In this demo we will learn more about writing a server that doesn't use views also learn about plugins.

We have removed the views directory so we don't have any views.

## Plugins ##

Plugins is moudles or libraries that you can plug into Nova. They are a behaviour that have a pre and post handler.

Nova has some plugins in the library:
    - nova_security_plugin: This will handle the security flag that are set in the routing file.
    - nova_correlation_plugin: This will add a uuid(v4) as a x-correllation-id header in the response. It is set in the pre-request so you can use the uuid with all things in your system to track.
    - nova_cors_plugin: Will add cors headers to your response, also handle that you don't need to manage OPTIONS method in your controllers.
    - nova_request_plugin: If content-type header is application/json it will do a json decode on the body and add it into controller data.

In routing file plugins also have a priority in what order they will be run, sys.config:
```erlang
         {plugins, [
                    {pre_http_request, nova_correlation_plugin, #{}, 0},
                    {pre_http_request, nova_cors_plugin, #{}, 2},
                    {pre_http_request, nova_request_plugin, #{decode_json_body => true}, 10}
                   ]}
```

Here we can see that it will run:
    1. nova_correlation_plugin
    2. nova_cors_plugin
    4. nova_request_plugin

In request plugin we also add some options, that we want it to decode the json that we get and send it to controller in the controller_data.

## Routing file ##

What we want to do with this demo is to create am api that will list pets, add a pet and remove a pet, maybe change a pet.

```erlang
#{prefix => "",
  security => false,
  routes => [
            {"/", { http_api_demo_main_controller, index}, #{methods => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
```

This is the emplated routing file that we did get when we did the `rebar3 new nova http_api_demo`.

Now we want to add a pet api that will point to a controller with some functions.

```erlang
#{prefix => "",
  security => false,
  routes => [
            {"/pet", { http_api_demo_pet_controller, manage_pet}, #{methods => [options, get, post]}},
            {"/pet/:petid", { http_api_demo_pet_controller, manage_pet}, #{methods => [options, get, put]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
```

Now we have added two paths. One is `/pet` that will handle the post and list all pets. Then we have `/pet/:petid` that will handle to get a pet, change name on pet and delete a pet.

## Controller ##

Now we need to create the controller for this.

This is the templated controller that returns `Nova is running!`.

```erlang
-module(http_api_demo_main_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    {ok, [{message, "Nova is running!"}]}.
```

If we return {ok, Value} from a function in the controller Nova will think we have a view that we should add it to.

```erlang
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
```

This is the logic we have in the controller to handle the api request. What it will do is to setup an ets and do simple CRUD.

To test this start a terminal run `rebar3 shell` and in another terminal run `rebar3 ct`.

3 tests should pass. In the test file there are some examples on how to use the api:s.


## Database ##

For this demo we willl just use a ets table. Each time the node is started it is a fresh node.

