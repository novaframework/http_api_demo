-module(http_api_demo_db).

-export([get_pets/0,
         get_pet/1,
         create_pet/2,
         update_pet/2,
         delete_pet/1]).


get_pets() ->
    SQL = <<"SELECT * FROM pet">>,
    query(SQL, []).

get_pet(Id) ->
    SQL = <<"SELECT * FROM pet WHERE id = $1">>,
    query1(SQL, [Id]).

create_pet(Id, Name) ->
    SQL = <<"INSERT INTO pet (id, name) VALUES ($1, $2)">>,
    query1(SQL, [Id, Name]).

update_pet(Id, Name) ->
    SQL = <<"UPDATE pet SET name = $1 WHERE id = $2">>,
    query1(SQL, [Name, Id]).

delete_pet(Id) ->
    SQL = <<"DELETE FROM pet WHERE id = $1">>,
    query1(SQL, [Id]).

query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := 1} -> ok;
        #{command := select,
          rows := []} -> undefined;
        #{command := select,
          rows := [Row]} -> {ok, Row};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := 1} -> ok;
        #{command := delete} -> undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := Num} -> {ok, Num};
        #{command := select,
          rows := Rows} -> {ok, Rows};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := Num} -> {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.
