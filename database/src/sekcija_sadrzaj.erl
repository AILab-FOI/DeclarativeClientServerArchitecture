-module(sekcija_sadrzaj).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_sadrzaj/1, dohvati_sekcije/1, dodaj_sadrzaj_na_sekciju/2,
         obrisi_sadrzaj/1, obrisi_sekcije/1, ucitaj_sadrzaj/1]).

dodaj_sadrzaj_na_sekciju(IdSadrzaj, IdSekcija) ->
    Fun = fun() ->
             case mnesia:write(#db_sekcija_sadrzaj{id_sadrzaj = IdSadrzaj, id_sekcija = IdSekcija})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_sadrzaj(IdSekcija) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_sekcija_sadrzaj{id_sadrzaj = Sadrzaj, id_sekcija = Sekcija})
                               when Sekcija =:= IdSekcija ->
                               Sadrzaj
                            end),
             Sadrzaji = mnesia:select(db_sekcija_sadrzaj, Match),
             lists:map(fun(Sadrzaj) ->
                          {atomic, Result} = sadrzaj:dohvati(Sadrzaj),
                          Result
                       end,
                       Sadrzaji)
          end,
    mnesia:transaction(Fun).

dohvati_sekcije(IdSadrzaj) ->
    Fun = fun() ->
             case mnesia:read({db_sekcija_sadrzaj, IdSadrzaj}) of
                 L ->
                     lists:map(fun(Sadrzaj) ->
                                  {atomic, Result} = sadrzaj:dohvati(Sadrzaj),
                                  Result
                               end,
                               L);
                 _ -> {"Korisnik nije povezan na fakultet"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_sadrzaj(IdSadrzaj) ->
    Fun = fun() -> mnesia:delete({db_sekcija_sadrzaj, IdSadrzaj}) end,
    mnesia:transaction(Fun).

obrisi_sekcije(IdSekcija) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_sekcija_sadrzaj{id_sadrzaj = Sadrzaj, id_sekcija = Sekcija})
                               when Sekcija =:= IdSekcija ->
                               Sadrzaj
                            end),

             Delete = mnesia:select(db_sekcija_sadrzaj, Match),
             lists:foreach(fun(IdKolegij) -> mnesia:delete({db_sekcija_sadrzaj, IdKolegij}) end,
                           Delete)
          end,
    mnesia:transaction(Fun).

ucitaj_sadrzaj(#{id := Id} = M) ->
    {atomic, Sadrzaj} = dohvati_sadrzaj(Id),
    M#{sadrzaj => Sadrzaj}.
