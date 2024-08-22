-module(fakultet_korisnik).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_fakultet/1, dodaj_korisnika_na_fakultet/2, dohvati_korisnike/1,
         ucitaj_korisnike/1, obrisi_fakultet/1, obrisi_korisnika/1]).

dodaj_korisnika_na_fakultet(IdKorisnik, IdFakultet) ->
    Fun = fun() ->
             mnesia:write(#db_fakultet_korisnik{id_korisnik = IdKorisnik, id_fakultet = IdFakultet})
          end,
    mnesia:transaction(Fun).

dohvati_korisnike(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_korisnik{id_fakultet = Fakultet,
                                                      id_korisnik = Korisnik})
                               when Fakultet =:= IdFakultet ->
                               Korisnik
                            end),
             case mnesia:select(db_fakultet_korisnik, Match) of
                 K ->
                     Korisnici =
                         lists:map(fun(Korisnik) ->
                                      {atomic, Result} = user:dohvati_korisnika(Korisnik),
                                      Result
                                   end,
                                   K),
                     Korisnici;
                 _ -> []
             end
          end,
    mnesia:transaction(Fun).

dohvati_fakultet(IdKorisnik) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet_korisnik, IdKorisnik}) of
                 [Obj] ->
                     {atomic, Fakultet} = fakultet:dohvati(Obj#db_fakultet_korisnik.id_fakultet),
                     Fakultet;
                 _ -> {"Korisnik nije povezan na fakultet"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_fakultet(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_korisnik{id_korisnik = Korisnik,
                                                      id_fakultet = Fakultet})
                               when Fakultet =:= IdFakultet ->
                               Korisnik
                            end),

             Delete = mnesia:select(db_fakultet_korisnik, Match),
             lists:foreach(fun(IdKorisnik) -> mnesia:delete({db_fakultet_korisnik, IdKorisnik}) end,
                           Delete)
          end,
    mnesia:transaction(Fun).

obrisi_korisnika(IdKorisnik) ->
    Fun = fun() -> mnesia:delete({db_fakultet_korisnik, IdKorisnik}) end,
    mnesia:transaction(Fun).

ucitaj_korisnike(#{id := Id} = M) ->
    {atomic, Korisnici} = dohvati_korisnike(Id),
    M#{korisnici => Korisnici}.
