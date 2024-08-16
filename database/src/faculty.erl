-module(faculty).

-include_lib("database/include/records.hrl").

dohvati_fakultete() ->
    Fun = fun(#db_fakultet{id = Id,
                           naziv = Naziv,
                           adresa = Adresa},
              Acc) ->
             [{Id, Naziv, Adresa} | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_fakultet) end),
    Record.

dohvati_fakultet(Id) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Id}) of
                 [#db_fakultet{naziv = N, adresa = A}] -> {Id, N, A};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_fakultet(Naziv, Adresa) ->
    Fun = fun() ->
             mnesia:write(#db_fakultet{id = ?ID,
                                       naziv = Naziv,
                                       adresa = Adresa})
          end,
    Trans_result = mnesia:transaction(Fun),
    case Trans_result of
        {aborted, Reason} ->
            {unable_to_insert, Reason};
        {atomic, Result} ->
            {done, Result};
        _ ->
            unable_to_insert
    end.
