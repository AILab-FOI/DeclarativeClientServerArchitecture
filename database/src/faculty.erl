-module(faculty).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj_fakultet/2, dohvati_fakultet/1, dohvati_fakultete/0, obrisi_fakultet/1,
         uredi_fakultet/3, dohvati_katedre_na_fakultetu/1, dodaj_katedru_na_fakultet/2,
         dohvati_katedru_na_fakultetu/2, dodaj_korisnika_na_fakultet/2,
         dohvati_korisnike_na_fakultetu/1, dohvati_korisnika_na_fakultetu/2]).

dohvati_fakultete() ->
    Fun = fun(#db_fakultet{id = Id,
                           naziv = Naziv,
                           adresa = Adresa},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                adresa => Adresa}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_fakultet) end),
    Record.

dohvati_fakultet(Id) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Id}) of
                 [#db_fakultet{naziv = N, adresa = A}] ->
                     #{id => Id,
                       naziv => N,
                       adresa => A};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_fakultet(Naziv, Adresa) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_fakultet{id = Id,
                                            naziv = Naziv,
                                            adresa = Adresa})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_fakultet(Id) ->
    Fun = fun() -> mnesia:delete({db_fakultet, Id}) end,
    mnesia:transaction(Fun).

uredi_fakultet(Id, Naziv, Adresa) ->
    Fun = fun() ->
             case mnesia:write(#db_fakultet{id = Id,
                                            naziv = Naziv,
                                            adresa = Adresa})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

dodaj_katedru_na_fakultet(IdFakultet, IdKatedra) ->
    Fun = fun() ->
             case mnesia:write(#db_fakultet_katedra{id_fakultet = IdFakultet,
                                                    id_katedra = IdKatedra})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_katedre_na_fakultetu(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_katedra{id_fakultet = Fakultet, id_katedra = Katedra})
                               when Fakultet =:= IdFakultet ->
                               Katedra
                            end),
             case mnesia:select(db_fakultet_katedra, Match) of
                 [] -> {"Fakultet nema katedre"};
                 K ->
                     Katedre =
                         lists:map(fun(Katedra) ->
                                      {atomic, Result} = department:dohvati_katedru(Katedra),
                                      Result
                                   end,
                                   K),
                     #{fakultet => IdFakultet, katedre => Katedre}
             end
          end,
    mnesia:transaction(Fun).

dohvati_katedru_na_fakultetu(IdFakultet, IdKatedra) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_katedra{id_fakultet = Fakultet, id_katedra = Katedra})
                               when Fakultet =:= IdFakultet andalso IdKatedra =:= Katedra ->
                               Katedra
                            end),
             case mnesia:select(db_fakultet_katedra, Match) of
                 [] -> {"Fakultet nema katedre"};
                 [K] ->
                     {atomic, Katedra} = department:dohvati_katedru(K),
                     #{fakultet => IdFakultet, katedra => Katedra}
             end
          end,
    mnesia:transaction(Fun).

dodaj_korisnika_na_fakultet(IdKorisnik, IdFakultet) ->
    Fun = fun() ->
             mnesia:write(#db_fakultet_korisnik{id_korisnik = IdKorisnik, id_fakultet = IdFakultet})
          end,
    mnesia:transaction(Fun).

dohvati_korisnike_na_fakultetu(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_korisnik{id_fakultet = Fakultet,
                                                      id_korisnik = Korisnik})
                               when Fakultet =:= IdFakultet ->
                               Korisnik
                            end),
             case mnesia:select(db_fakultet_korisnik, Match) of
                 [] -> {"Fakultet nema korisnika"};
                 K ->
                     Korisnici =
                         lists:map(fun(Korisnik) ->
                                      {atomic, Result} = user:dohvati_korisnika(Korisnik),
                                      Result
                                   end,
                                   K),
                     #{fakultet => IdFakultet, korisnici => Korisnici}
             end
          end,
    mnesia:transaction(Fun).

dohvati_korisnika_na_fakultetu(IdFakultet, IdKorisnik) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet_korisnik, IdFakultet, IdKorisnik}) of
                 [Obj] ->
                     {atomic, Korisnik} =
                         user:dohvati_korisnika(Obj#db_fakultet_korisnik.id_korisnik),
                     {atomic, Fakultet} = dohvati_fakultet(Obj#db_fakultet_korisnik.id_fakultet),
                     #{korisnik => Korisnik, fakultet => Fakultet};
                 [] -> {"Korisnik nije na fakultetu"}
             end
          end,
    mnesia:transaction(Fun).
