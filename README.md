# HTTP API Server #

A basic Nova HTTP API server demonstrating:

✔️ Routing  
✔️ CRUD (ETS)  
✔️ Plugins  
❌ Views  

## Routing

Let's start a new project with:

```
$ rebar3 new nova http_api_demo
```

If we open `./priv/http_api_demo.routes.erl`, we see this:

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

This is our basic routing file. 

We want to add routes for our CRUD app, let's do that now:

```erlang
#{prefix => "",
  security => false,
  routes => [
            {"/pet", { http_api_demo_pet_controller, get_pets}, #{methods => [options, get]}},
            {"/pet", { http_api_demo_pet_controller, create_pet}, #{methods => [options, post]}},
            {"/pet/:petid", { http_api_demo_pet_controller, get_pet}, #{methods => [options, get]}},
            {"/pet/:petid", { http_api_demo_pet_controller, update_pet}, #{methods => [options, put]}},
            {"/pet/:petid", { http_api_demo_pet_controller, remove_pet}, #{methods => [options, delete]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
```

We've added `OPTIONS` as one of the accepted methods in order to enable [CORS preflight](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS). Nova doesn't support CORS out of the box, instead it is insert via a plugin.

## Plugins

Plugins are modules or libraries that you can plug into Nova. They are a behaviour that have a pre and post handler.

Nova has some preexisting plugins in its library:
* `nova_security_plugin`: This will handle the security flag that are set in the routing file.
* `nova_correlation_plugin`: This will add a uuid(v4) as a x-correllation-id header in the response. It is set in the pre-request so you can use the uuid with all things in your system to track.
* `nova_cors_plugin`: Will add CORS headers to your response and handle so that you don't need to manually manage an OPTIONS method in your controller(s).
* `nova_request_plugin`: If content-type header is application/json it will do a json decode on the body and add it into controller data.

Let's add some ourself in `./config/sys.config`:
```erlang
{plugins, [
           {pre_http_request, nova_correlation_plugin, #{}},
           {pre_http_request, nova_cors_plugin, #{allow_origins => [<<"http://localhost:5500/">>]}},
           {pre_http_request, nova_request_plugin, #{decode_json_body => true}}
          ]}
```

Note that the plugins can take options, like `nova_request_plugin` automatically decoding JSON bodies.

## Controller

Our next step is to look at our controller.

Currently, we have standard the templated controller that returns `Nova is running!`.

```erlang
-module(http_api_demo_main_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    {ok, [{message, "Nova is running!"}]}.
```

Returning `{ok, Value}` tells Nova that we are expecting a view.

Lets replace it with our own methods:

```erlang
-module(http_api_demo_pet_controller).
-export([
         create_pet/1,
         get_pet/1,
         update_pet/1,
         remove_pet/1,
         get_pets/1
        ]).

create_pet(#{json := #{<<"name">> := Name}}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),  
    true = ets:insert(pets, {Id, Name}),
    {json, 201, #{}, #{<<"id">> => Id, <<"name">> => Name}}.

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

get_pets(_) ->
    List = ets:tab2list(pets),
    Body = [#{<<"id">> => Id,
              <<"name">> => Name} ||{Id, Name} <- List],
    {json, 200, #{}, Body}.
```

Using the bindings and body of our requests, we now have a fully functioning CRUD API against ETS.

To test this start a terminal run `rebar3 shell` and in another terminal run `rebar3 ct`.

3 tests should pass. The test files also provides some documentation on how to call our API.

If you would like to experiment against an external frontend, this repo provides a simple HTML file
`./client.html`. You will need to serve it yourself, and make sure that the config file allows that
port via `allow_origins` in `nova_cors_plugin`.