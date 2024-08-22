-module(fakultet).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/2, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/3]).

dohvati() ->
    Fun = fun(Fakultet, Acc) -> [ucitaj(full, Fakultet) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_fakultet) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Id}) of
                 [Fakultet] -> ucitaj(full, Fakultet);
                 [] -> {error, "Fakultet ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Id}) of
                 [Fakultet] -> ucitaj(Type, Fakultet);
                 [] -> {error, "Fakultet ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, Adresa) ->
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

obrisi(Id) ->
    Fun = fun() ->
             fakultet_korisnik:obrisi_fakultet(Id),
             fakultet_katedra:obrisi_fakultet(Id),
             mnesia:delete({db_fakultet, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, Adresa) ->
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

ucitaj(core, R) ->
    transform_fakultet(R);
ucitaj(katedre, R) ->
    M0 = transform_fakultet(R),
    fakultet_katedra:dohvati_katedre(M0);
ucitaj(korisnici, R) ->
    M0 = transform_fakultet(R),
    fakultet_korisnik:dohvati_korisnike(M0);
ucitaj(full, R) ->
    M0 = transform_fakultet(R),
    M1 = fakultet_katedra:dohvati_katedre(M0),
    fakultet_korisnik:dohvati_korisnike(M1).

transform_fakultet(#db_fakultet{id = Id,
                                naziv = Naziv,
                                adresa = Adresa}) ->
    #{id => Id,
      naziv => Naziv,
      adresa => Adresa}.
