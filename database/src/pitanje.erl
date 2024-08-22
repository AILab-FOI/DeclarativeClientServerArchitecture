-module(pitanje).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/2, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/3]).

dohvati() ->
    Fun = fun(Pitanje, Acc) -> [ucitaj(core, Pitanje) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_pitanje) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_pitanje, Id}) of
                 [Pitanje] -> ucitaj(full, Pitanje);
                 [] -> {error, "Pitanje ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_pitanje, Id}) of
                 [Pitanje] -> ucitaj(Type, Pitanje);
                 [] -> {error, "Pitanje ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, Odgovori) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_pitanje{id = Id,
                                           naziv = Naziv,
                                           odgovori = Odgovori})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi(Id) ->
    Fun = fun() ->
             pitanje_sadrzaj:obrisi_pitanje(Id),
             mnesia:delete({db_pitanje, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, Odgovori) ->
    Fun = fun() ->
             case mnesia:write(#db_pitanje{id = Id,
                                           naziv = Naziv,
                                           odgovori = Odgovori})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_pitanje(R);
ucitaj(full, R) ->
    M0 = transform_pitanje(R),
    pitanje_sadrzaj:ucitaj_sekcije(M0).

transform_pitanje(#db_pitanje{id = Id,
                              naziv = Naziv,
                              odgovori = Odgovori}) ->
    #{id => Id,
      naziv => Naziv,
      odgovori => Odgovori}.
