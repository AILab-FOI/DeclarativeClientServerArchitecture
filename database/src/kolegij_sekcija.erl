-module(kolegij_sekcija).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_sekcije/1, dohvati_kolegije/1, dodaj_sekciju_na_kolegij/2,
         obrisi_kolegij/1, obrisi_sekciju/1, ucitaj_sekcije/1]).

dodaj_sekciju_na_kolegij(IdSekcija, IdKolegij) ->
    Fun = fun() ->
             case mnesia:write(#db_kolegij_sekcija{id_kolegij = IdKolegij, id_sekcija = IdSekcija})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_sekcije(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_kolegij_sekcija{id_kolegij = Kolegij, id_sekcija = Sekcija})
                               when Kolegij =:= IdKolegij ->
                               Sekcija
                            end),
             Sekcije = mnesia:select(db_kolegij_sekcija, Match),
             lists:map(fun(Sekcija) ->
                          {atomic, Result} = sekcija:dohvati(Sekcija),
                          Result
                       end,
                       Sekcije)
          end,
    mnesia:transaction(Fun).

dohvati_kolegije(IdSekcija) ->
    Fun = fun() ->
             case mnesia:read({db_kolegij_sekcija, IdSekcija}) of
                 [Obj] ->
                     {atomic, Katedra} = katedra:dohvati(Obj#db_katedra_kolegij.id_katedra),
                     Katedra;
                 _ -> {"Korisnik nije povezan na fakultet"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(IdKolegij) ->
    Fun = fun() -> mnesia:delete({db_kolegij_sekcija, IdKolegij}) end,
    mnesia:transaction(Fun).

obrisi_sekciju(IdSekcija) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_kolegij_sekcija{id_kolegij = Kolegij, id_sekcija = Sekcija} = R)
                               when Sekcija =:= IdSekcija ->
                               R
                            end),

             Delete = mnesia:select(db_kolegij_sekcija, Match),
             lists:foreach(fun(Rec) -> mnesia:delete_object(Rec) end, Delete)
          end,
    mnesia:transaction(Fun).

ucitaj_sekcije(#{id := Id} = M) ->
    {atomic, Sekcije} = dohvati_sekcije(Id),
    M#{sekcije => Sekcije}.
