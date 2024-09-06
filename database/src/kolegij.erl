-module(kolegij).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/2, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/4]).

dohvati() ->
    Fun = fun(Kolegij, Acc) -> [ucitaj(core, Kolegij) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_kolegij, Id}) of
                 [Kolegij] -> ucitaj(full, Kolegij);
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_kolegij, Id}) of
                 [Kolegij] -> ucitaj(Type, Kolegij);
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, Skraceno) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_kolegij{id = Id,
                                           naziv = Naziv,
                                           slika = <<"course.png">>,
                                           skraceno = Skraceno})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi(Id) ->
    Fun = fun() ->
             djelatnik_kolegij:obrisi_kolegij(Id),
             student_kolegij:obrisi_kolegij(Id),
             kolegij_sekcija:obrisi_kolegij(Id),
             mnesia:delete({db_kolegij, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, Skraceno, Slika) ->
    Fun = fun() ->
             case mnesia:write(#db_kolegij{id = Id,
                                           naziv = Naziv,
                                           slika = Slika,
                                           skraceno = Skraceno})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_kolegij(R);
ucitaj(sekcije, R) ->
    M0 = transform_kolegij(R),
    kolegij_sekcija:ucitaj_sekcije(M0);
ucitaj(djelatnici, R) ->
    M0 = transform_kolegij(R),
    djelatnik_kolegij:ucitaj_djelatnike(M0);
ucitaj(studenti, R) ->
    M0 = transform_kolegij(R),
    student_kolegij:ucitaj_studente(M0);
ucitaj(full, R) ->
    M0 = transform_kolegij(R),
    M1 = kolegij_sekcija:ucitaj_sekcije(M0),
    M2 = djelatnik_kolegij:ucitaj_djelatnike(M1),
    student_kolegij:ucitaj_studente(M2).

transform_kolegij(#db_kolegij{id = Id,
                              naziv = Naziv,
                              slika = Slika,
                              skraceno = Skraceno}) ->
    #{id => Id,
      slika => Slika,
      naziv => Naziv,
      skraceno => Skraceno}.
