-module(department).

-include_lib("database/include/records.hrl").

-export([dohvati_katedru/1, dodaj_katedru/1, obrisi_katedru/1, dohvati_katedre/0,
         uredi_katedru/2]).

dodaj_katedru(Naziv) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_katedra{id = Id, naziv = Naziv}) of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_katedru(Id) ->
    Fun = fun() -> mnesia:delete({db_katedra, Id}) end,
    mnesia:transaction(Fun).

dohvati_katedru(Id) ->
    Fun = fun() ->
             case mnesia:read({db_katedra, Id}) of
                 [#db_katedra{naziv = Naziv}] -> #{id => Id, naziv => Naziv};
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati_katedre() ->
    Fun = fun(#db_katedra{id = Id, naziv = Naziv}, Acc) -> [#{id => Id, naziv => Naziv} | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_katedra) end),
    Record.

uredi_katedru(Id, Naziv) ->
    Fun = fun() ->
             case mnesia:write(#db_katedra{id = Id, naziv = Naziv}) of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).
