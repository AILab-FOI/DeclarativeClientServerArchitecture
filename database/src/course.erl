-module(course).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_kolegij/1, dodaj_kolegij/2, dohvati_kolegije/0, obrisi_kolegij/1,
         uredi_kolegij/3, dodaj_sekciju_na_kolegij/2, dohvati_sekcije_na_kolegiju/1,
         dohvati_sekciju_na_kolegiju/2]).

dodaj_kolegij(Naziv, Skraceno) ->
    Id = ?ID,
    Fun = fun() ->
             io:format("~p~n~p~n", [Naziv, Skraceno]),
             case mnesia:write(#db_kolegij{id = Id,
                                           naziv = Naziv,
                                           skraceno = Skraceno})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(Id) ->
    Fun = fun() -> mnesia:delete({db_kolegij, Id}) end,
    mnesia:transaction(Fun).

dohvati_kolegij(Id) ->
    Fun = fun() ->
             case mnesia:read({db_kolegij, Id}) of
                 [#db_kolegij{naziv = Naziv, skraceno = Skraceno}] ->
                     #{id => Id,
                       naziv => Naziv,
                       skraceno => Skraceno};
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati_kolegije() ->
    Fun = fun(#db_kolegij{id = Id,
                          naziv = Naziv,
                          skraceno = Skraceno},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                skraceno => Skraceno}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end),
    Record.

uredi_kolegij(Id, Naziv, Skraceno) ->
    Fun = fun() ->
             case mnesia:write(#db_kolegij{id = Id,
                                           naziv = Naziv,
                                           skraceno = Skraceno})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

dodaj_sekciju_na_kolegij(IdKolegij, IdSekcija) ->
    Fun = fun() ->
             mnesia:write(#db_kolegij_sekcija{id_kolegij = IdKolegij, id_sekcija = IdSekcija})
          end,
    mnesia:transaction(Fun).

dohvati_sekcije_na_kolegiju(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_kolegij_sekcija{id_kolegij = Kolegij, id_sekcija = Sekcija})
                               when Kolegij =:= IdKolegij ->
                               Sekcija
                            end),
             case mnesia:select(db_kolegij_sekcija, Match) of
                 [] -> {"Fakultet nema korisnika"};
                 S ->
                     {atomic, Kolegij} = dohvati_kolegij(IdKolegij),
                     Sekcije =
                         lists:map(fun(Sekcija) ->
                                      {atomic, Result} = section:dohvati_sekcija(Sekcija),
                                      Result
                                   end,
                                   S),
                     #{kolegij => Kolegij, sekcije => Sekcije}
             end
          end,
    mnesia:transaction(Fun).

dohvati_sekciju_na_kolegiju(IdKolegij, IdSekcija) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_kolegij_sekcija{id_kolegij = Kolegij, id_sekcija = Sekcija})
                               when Kolegij =:= IdKolegij andalso Sekcija =:= IdSekcija ->
                               Sekcija
                            end),
             case mnesia:select(db_kolegij_sekcija, Match) of
                 [Obj] ->
                     io:format("~p~n", [Obj]),
                     {atomic, Kolegij} = dohvati_kolegij(IdKolegij),
                     {atomic, Sekcija} = section:dohvati_sekcija(Obj),
                     #{kolegij => Kolegij, sekcija => Sekcija};
                 [] -> {"Sekcija nije unutar kolegija"}
             end
          end,
    mnesia:transaction(Fun).
