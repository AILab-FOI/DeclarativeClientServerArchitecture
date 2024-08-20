-module(section).

-include_lib("database/include/records.hrl").

-export([dodaj_sekcija/2, dohvati_sekcije/0, dohvati_sekcija/1, uredi_sekcija/3,
         obrisi_sekcija/1]).

dohvati_sekcije() ->
    Fun = fun(#db_sekcija{id = Id,
                          naziv = Naziv,
                          opis = Opis},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                opis => Opis}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_sekcija) end),
    Record.

dohvati_sekcija(Id) ->
    Fun = fun() ->
             case mnesia:read({db_sekcija, Id}) of
                 [#db_sekcija{naziv = N, opis = O}] ->
                     #{id => Id,
                       naziv => N,
                       opis => O};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_sekcija(Naziv, Opis) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_sekcija{id = Id,
                                           naziv = Naziv,
                                           opis = Opis})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_sekcija(Id) ->
    Fun = fun() -> mnesia:delete({db_sekcija, Id}) end,
    mnesia:transaction(Fun).

uredi_sekcija(Id, Naziv, Opis) ->
    Fun = fun() ->
             case mnesia:write(#db_sekcija{id = Id,
                                           naziv = Naziv,
                                           opis = Opis})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).
