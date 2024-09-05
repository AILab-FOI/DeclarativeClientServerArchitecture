-module(katedra_kolegij).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_kolegije/1, dohvati_katedru/1, dodaj_kolegij_na_katedru/2,
         obrisi_katedru/1, obrisi_kolegij/1, ucitaj_kolegije/1]).

dodaj_kolegij_na_katedru(IdKolegij, IdKatedra) ->
    Fun = fun() ->
             case mnesia:write(#db_katedra_kolegij{id_kolegij = IdKolegij, id_katedra = IdKatedra})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_kolegije(IdKatedra) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_katedra_kolegij{id_kolegij = Kolegij, id_katedra = Katedra})
                               when Katedra =:= IdKatedra ->
                               Kolegij
                            end),
             Kolegiji = mnesia:select(db_katedra_kolegij, Match),
             lists:map(fun(Kolegij) ->
                          {atomic, Result} = kolegij:dohvati(core, Kolegij),
                          Result
                       end,
                       Kolegiji)
          end,
    mnesia:transaction(Fun).

dohvati_katedru(IdKolegij) ->
    Fun = fun() ->
             case mnesia:read({db_katedra_kolegij, IdKolegij}) of
                 [Obj] ->
                     {atomic, Katedra} = katedra:dohvati(Obj#db_katedra_kolegij.id_katedra),
                     Katedra;
                 _ -> {"Korisnik nije povezan na fakultet"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_katedru(IdKatedra) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_katedra_kolegij{id_kolegij = Kolegij, id_katedra = Katedra})
                               when Katedra =:= IdKatedra ->
                               Kolegij
                            end),

             Delete = mnesia:select(db_katedra_kolegij, Match),
             lists:foreach(fun(IdKolegij) -> mnesia:delete({db_katedra_kolegij, IdKolegij}) end,
                           Delete)
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(IdKolegij) ->
    Fun = fun() -> mnesia:delete({db_katedra_kolegij, IdKolegij}) end,
    mnesia:transaction(Fun).

ucitaj_kolegije(#{id := Id} = M) ->
    {atomic, Kolegiji} = dohvati_kolegije(Id),
    M#{kolegiji => Kolegiji}.
