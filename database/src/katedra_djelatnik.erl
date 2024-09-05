-module(katedra_djelatnik).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_katedre/1, dohvati_djelatnike/1, dodaj_djelatnika_na_katedru/3,
         ucitaj_djelatnike/1, ucitaj_katedre/1, obrisi_katedru/1, obrisi_djelatnika/1]).

dodaj_djelatnika_na_katedru(IdDjelatnik, IdKatedra, Tip) ->
    Fun = fun() ->
             case mnesia:write(#db_katedra_djelatnik{id_djelatnik = IdDjelatnik,
                                                     id_katedra = IdKatedra,
                                                     tip = Tip})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_djelatnike(IdKatedra) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_katedra_djelatnik{id_djelatnik = Djelatnik,
                                                      id_katedra = Katedra,
                                                      tip = Tip})
                               when Katedra =:= IdKatedra ->
                               {Djelatnik, Tip}
                            end),
             Djelatnici = mnesia:select(db_katedra_djelatnik, Match),
             lists:map(fun({Djelatnik, Tip}) ->
                          {atomic, Result} = korisnik:dohvati_korisnika(core, Djelatnik),
                          Result#{tip => Tip}
                       end,
                       Djelatnici)
          end,
    mnesia:transaction(Fun).

dohvati_katedre(IdDjelatnik) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_katedra_djelatnik{id_djelatnik = Djelatnik,
                                                      id_katedra = Katedra})
                               when Djelatnik =:= IdDjelatnik ->
                               Katedra
                            end),
             Katedre = mnesia:select(db_katedra_djelatnik, Match),
             lists:map(fun(Katedra) ->
                          {atomic, Result} = katedra:dohvati(Katedra),
                          Result
                       end,
                       Katedre)
          end,
    mnesia:transaction(Fun).

obrisi_katedru(IdKatedra) ->
    Fun = fun() -> mnesia:delete({db_katedra_djelatnik, IdKatedra}) end,
    mnesia:transaction(Fun).

obrisi_djelatnika(IdDjelatnik) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_katedra_djelatnik{id_djelatnik = Djelatnik,
                                                      id_katedra = Katedra})
                               when Djelatnik =:= IdDjelatnik ->
                               Katedra
                            end),
             % NOTE: Testirati!!!
             Delete = mnesia:select(db_katedra_djelatnik, Match),
             lists:foreach(fun(IdKatedra) -> mnesia:delete({db_katedra_djelatnik, IdKatedra}) end,
                           Delete)
          end,

    mnesia:transaction(Fun).

ucitaj_djelatnike(#{id := Id} = M) ->
    {atomic, Djelatnici} = dohvati_djelatnike(Id),
    M#{djelatnici => Djelatnici}.

ucitaj_katedre(#{id := Id} = M) ->
    {atomic, Katedre} = dohvati_katedre(Id),
    M#{katedre => Katedre}.
