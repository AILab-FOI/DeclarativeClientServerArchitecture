-module(kviz).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/3, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/4]).

dohvati() ->
    Fun = fun(Kviz, Acc) -> [ucitaj(core, Kviz) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kviz) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_kviz, Id}) of
                 [Kviz] -> ucitaj(full, Kviz);
                 [] -> {error, "Kviz ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_kviz, Id}) of
                 [Kviz] -> ucitaj(Type, Kviz);
                 [] -> {error, "Kviz ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, DostupanOd, DostupanDo) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_kviz{id = Id,
                                        naziv = Naziv,
                                        dostupan_od = DostupanOd,
                                        dostupan_do = DostupanDo})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi(Id) ->
    Fun = fun() ->
             kviz_sadrzaj:obrisi_kviz(Id),
             mnesia:delete({db_kviz, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, DostupanOd, DostupanDo) ->
    Fun = fun() ->
             case mnesia:write(#db_kviz{id = Id,
                                        naziv = Naziv,
                                        dostupan_od = DostupanOd,
                                        dostupan_do = DostupanDo})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_kviz(R);
ucitaj(full, R) ->
    M0 = transform_kviz(R),
    kviz_sadrzaj:ucitaj_sekcije(M0).

transform_kviz(#db_kviz{id = Id,
                        naziv = Naziv,
                        dostupan_od = DostupanOd,
                        dostupan_do = DostupanDo}) ->
    #{id => Id,
      naziv => Naziv,
      dostupan_od => DostupanOd,
      dostupan_do => DostupanDo}.
