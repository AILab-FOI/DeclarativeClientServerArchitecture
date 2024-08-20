-module(content).

-include_lib("database/include/records.hrl").

-export([dodaj_sadrzaj/3, dohvati_sadrzaj/1, obrisi_sadrzaj/1, dohvati_sadrzaje/0,
         uredi_sadrzaj/4]).

dodaj_sadrzaj(Naziv, Tip, Vrijednost) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_sadrzaj{id = Id,
                                           naziv = Naziv,
                                           tip = Tip,
                                           vrijednost = Vrijednost})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_sadrzaj(Id) ->
    Fun = fun() -> mnesia:delete({db_sadrzaj, Id}) end,
    mnesia:transaction(Fun).

dohvati_sadrzaj(Id) ->
    Fun = fun() ->
             case mnesia:read({db_sadrzaj, Id}) of
                 [#db_sadrzaj{naziv = Naziv,
                              tip = Tip,
                              vrijednost = Vrijednost}] ->
                     #{id => Id,
                       naziv => Naziv,
                       tip => Tip,
                       vrijednost => Vrijednost};
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati_sadrzaje() ->
    Fun = fun(#db_sadrzaj{id = Id,
                          naziv = Naziv,
                          tip = Tip,
                          vrijednost = Vrijednost},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                tip => Tip,
                vrijednost => Vrijednost}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end),
    Record.

uredi_sadrzaj(Id, Naziv, Tip, Vrijednost) ->
    Fun = fun() ->
             case mnesia:write(#db_sadrzaj{id = Id,
                                           naziv = Naziv,
                                           tip = Tip,
                                           vrijednost = Vrijednost})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).
