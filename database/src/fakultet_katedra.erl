-module(fakultet_katedra).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_katedre/1, dohvati_fakultet/1, dodaj_katedru_na_fakultet/2,
         ucitaj_katedre/1, obrisi_fakultet/1, obrisi_katedru/1]).

dodaj_katedru_na_fakultet(IdKatedra, IdFakultet) ->
    Fun = fun() ->
             case mnesia:write(#db_fakultet_katedra{id_fakultet = IdFakultet,
                                                    id_katedra = IdKatedra})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_katedre(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_katedra{id_katedra = Katedra, id_fakultet = Fakultet})
                               when Fakultet =:= IdFakultet ->
                               Katedra
                            end),
             Katedre = mnesia:select(db_fakultet_katedra, Match),
             lists:map(fun(Katedra) ->
                          {atomic, Result} = katedra:dohvati(Katedra),
                          Result
                       end,
                       Katedre)
          end,
    mnesia:transaction(Fun).

dohvati_fakultet(IdKatedra) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet_katedra, IdKatedra}) of
                 [Obj] ->
                     {atomic, Fakultet} = fakultet:dohvati(Obj#db_fakultet_katedra.id_fakultet),
                     Fakultet;
                 _ -> {"Korisnik nije povezan na fakultet"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_fakultet(IdFakultet) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_fakultet_katedra{id_katedra = Katedra, id_fakultet = Fakultet})
                               when Fakultet =:= IdFakultet ->
                               Katedra
                            end),

             Delete = mnesia:select(db_fakultet_katedra, Match),
             lists:foreach(fun(IdKatedra) -> mnesia:delete({db_fakultet_katedra, IdKatedra}) end,
                           Delete)
          end,
    mnesia:transaction(Fun).

obrisi_katedru(IdKatedra) ->
    Fun = fun() -> mnesia:delete({db_fakultet_katedra, IdKatedra}) end,
    mnesia:transaction(Fun).

ucitaj_katedre(#{id := Id} = M) ->
    {atomic, Katedre} = dohvati_katedre(Id),
    M#{katedre => Katedre}.
