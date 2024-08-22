-module(katedra).

-include_lib("database/include/records.hrl").

-export([dodaj/1, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/2]).

dohvati() ->
    Fun = fun(Katedra, Acc) -> [ucitaj(full, Katedra) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_katedra) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_katedra, Id}) of
                 [Katedra] -> ucitaj(full, Katedra);
                 [] -> {error, "Katedra ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_katedra, Id}) of
                 [Katedra] -> ucitaj(Type, Katedra);
                 [] -> {error, "Katedra ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_katedra{id = Id, naziv = Naziv}) of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi(Id) ->
    Fun = fun() ->
             katedra_djelatnik:obrisi_katedru(Id),
             katedra_kolegij:obrisi_katedru(Id),
             mnesia:delete({db_katedra, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv) ->
    Fun = fun() ->
             case mnesia:write(#db_katedra{id = Id, naziv = Naziv}) of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_katedra(R);
ucitaj(kolegiji, R) ->
    M0 = transform_katedra(R),
    katedra_kolegij:ucitaj_kolegije(M0);
ucitaj(djelatnici, R) ->
    M0 = transform_katedra(R),
    katedra_djelatnik:ucitaj_djelatnike(M0);
ucitaj(full, R) ->
    M0 = transform_katedra(R),
    M1 = katedra_kolegij:ucitaj_kolegije(M0),
    katedra_djelatnik:ucitaj_djelatnike(M1).

transform_katedra(#db_katedra{id = Id, naziv = Naziv}) ->
    #{id => Id, naziv => Naziv}.
