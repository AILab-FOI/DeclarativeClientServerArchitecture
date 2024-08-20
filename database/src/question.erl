-module(question).

-include_lib("database/include/records.hrl").

-export([dodaj_pitanje/2, dohvati_pitanja/0, dohvati_pitanje/1, uredi_pitanje/3,
         obrisi_pitanje/1]).

dohvati_pitanja() ->
    Fun = fun(#db_pitanje{id = Id,
                          naziv = Naziv,
                          odgovori = Odgovori},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                odgovori => Odgovori}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_pitanje) end),
    Record.

dohvati_pitanje(Id) ->
    Fun = fun() ->
             case mnesia:read({db_pitanje, Id}) of
                 [#db_pitanje{naziv = N, odgovori = O}] ->
                     #{id => Id,
                       naziv => N,
                       odgovori => O};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_pitanje(Naziv, Odgovori) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_pitanje{id = Id,
                                           naziv = Naziv,
                                           odgovori = Odgovori})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_pitanje(Id) ->
    Fun = fun() -> mnesia:delete({db_pitanje, Id}) end,
    mnesia:transaction(Fun).

uredi_pitanje(Id, Naziv, Odgovori) ->
    Fun = fun() ->
             case mnesia:write(#db_pitanje{id = Id,
                                           naziv = Naziv,
                                           odgovori = Odgovori})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).
