-module(operations).

-export([write_secure/3, read_secure/4]).

write_secure(function, Obj, Fun) ->
    case mnesia:write(Obj) of
        ok ->
            Fun();
        _ ->
            {error, "Transakcija neuspješna"}
    end;
write_secure(object, Obj, ReturnObj) ->
    case mnesia:write(Obj) of
        ok ->
            ReturnObj;
        _ ->
            {error, "Transakcija neuspješna"}
    end.

read_secure(function, Db, Id, Fun) ->
    case mnesia:read({Db, Id}) of
        [Obj] ->
            Fun(Obj);
        _ ->
            {error, "Zapis ne postoji"}
    end;
read_secure(object, Db, Id, ReturnObj) ->
    case mnesia:read({Db, Id}) of
        [_] ->
            ReturnObj;
        _ ->
            {error, "Zapis ne postoji"}
    end.
