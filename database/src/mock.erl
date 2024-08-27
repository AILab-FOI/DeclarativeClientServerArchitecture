-module(mock).

-include_lib("database/include/records.hrl").

-export([ucitaj_mock/0]).

ucitaj_mock() ->
    ucitaj(fakultete()),
    ucitaj(katedre()),
    ucitaj(kolegiji()),
    ucitaj(sekcije()),
    ucitaj(sadrzaj()),
    ucitaj(fakultet_katedra_keys()),
    ucitaj(katedra_kolegij_keys()),
    ucitaj(kolegij_sekcija_keys()),
    ucitaj(sekcija_sadrzaj_keys()).

fakultet_katedra_keys() ->
    [#db_fakultet_katedra{id_katedra = 1, id_fakultet = 1},
     #db_fakultet_katedra{id_katedra = 2, id_fakultet = 1},
     #db_fakultet_katedra{id_katedra = 3, id_fakultet = 1},
     #db_fakultet_katedra{id_katedra = 4, id_fakultet = 1},
     #db_fakultet_katedra{id_katedra = 5, id_fakultet = 1}].

katedra_kolegij_keys() ->
    [#db_katedra_kolegij{id_katedra = 3, id_kolegij = 1},
     #db_katedra_kolegij{id_katedra = 3, id_kolegij = 2},
     #db_katedra_kolegij{id_katedra = 3, id_kolegij = 6},
     #db_katedra_kolegij{id_katedra = 3, id_kolegij = 9},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 3},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 4},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 5},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 7},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 8},
     #db_katedra_kolegij{id_katedra = 4, id_kolegij = 10}].

kolegij_sekcija_keys() ->
    [#db_kolegij_sekcija{id_kolegij = 1, id_sekcija = 1},
     #db_kolegij_sekcija{id_kolegij = 2, id_sekcija = 2},
     #db_kolegij_sekcija{id_kolegij = 3, id_sekcija = 3},
     #db_kolegij_sekcija{id_kolegij = 9, id_sekcija = 4},
     #db_kolegij_sekcija{id_kolegij = 4, id_sekcija = 5},
     #db_kolegij_sekcija{id_kolegij = 5, id_sekcija = 6},
     #db_kolegij_sekcija{id_kolegij = 7, id_sekcija = 7}].

sekcija_sadrzaj_keys() ->
    [#db_sekcija_sadrzaj{id_sekcija = 1, id_sadrzaj = 1},
     #db_sekcija_sadrzaj{id_sekcija = 1, id_sadrzaj = 3},
     #db_sekcija_sadrzaj{id_sekcija = 1, id_sadrzaj = 5},
     #db_sekcija_sadrzaj{id_sekcija = 2, id_sadrzaj = 2},
     #db_sekcija_sadrzaj{id_sekcija = 1, id_sadrzaj = 4},
     #db_sekcija_sadrzaj{id_sekcija = 1, id_sadrzaj = 6}].

ucitaj(L) ->
    lists:foreach(fun(E) -> mnesia:dirty_write(E) end, L).

fakultete() ->
    [#db_fakultet{id = 1,
                  naziv = <<"FOI">>,
                  adresa = adresa()},
     #db_fakultet{id = 2,
                  naziv = <<"FER">>,
                  adresa = adresa()},
     #db_fakultet{id = 3,
                  naziv = <<"EFZG">>,
                  adresa = adresa()},
     #db_fakultet{id = 4,
                  naziv = <<"FFZG">>,
                  adresa = adresa()},
     #db_fakultet{id = 5,
                  naziv = <<"FKIT">>,
                  adresa = adresa()}].

adresa() ->
    #adresa{ulica = <<"Pavlinska ulica">>,
            grad = <<"Varaždin">>,
            postanski_broj = 42000,
            drzava = <<"Hrvatska">>,
            kucni_broj = <<"2">>}.

katedre() ->
    [#db_katedra{id = 1, naziv = <<"Katedra za gospodatstvo">>},
     #db_katedra{id = 2, naziv = <<"Katedra za organizaciju">>},
     #db_katedra{id = 3, naziv = <<"Katedra za kvantitivne metode">>},
     #db_katedra{id = 4, naziv = <<"Katedra za informatičke tehnologije i računarstvo">>},
     #db_katedra{id = 5, naziv = <<"Katedra za razvoj informacijskih sustava">>}].

kolegiji() ->
    [#db_kolegij{id = 1,
                 naziv = <<"Matematika 1">>,
                 skraceno = <<"Mat1">>},
     #db_kolegij{id = 2,
                 naziv = <<"Matematika 2">>,
                 skraceno = <<"Mat1">>},
     #db_kolegij{id = 3,
                 naziv = <<"Deklarativno programiranje">>,
                 skraceno = <<"DP">>},
     #db_kolegij{id = 4,
                 naziv = <<"Programiranje 1">>,
                 skraceno = <<"Prog1">>},
     #db_kolegij{id = 5,
                 naziv = <<"Programiranje 2">>,
                 skraceno = <<"Prog2">>},
     #db_kolegij{id = 6,
                 naziv = <<"Teorija odlučivanja">>,
                 skraceno = <<"TO">>},
     #db_kolegij{id = 7,
                 naziv = <<"Uzorci dizajna">>,
                 skraceno = <<"UzDiz">>},
     #db_kolegij{id = 8,
                 naziv = <<"Napredne web tehnologije i sustavi">>,
                 skraceno = <<"Nwtis">>},
     #db_kolegij{id = 9,
                 naziv = <<"Odabrana poglavlja matematike">>,
                 skraceno = <<"OPM">>},
     #db_kolegij{id = 10,
                 naziv = <<"Računalna grafika">>,
                 skraceno = <<"RG">>}].

sekcije() ->
    [#db_sekcija{id = 1,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju Mat1">>},
     #db_sekcija{id = 2,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju Mat2">>},
     #db_sekcija{id = 3,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju DP">>},
     #db_sekcija{id = 4,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju OPM">>},
     #db_sekcija{id = 5,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju Prog1">>},
     #db_sekcija{id = 6,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju Prog2">>},
     #db_sekcija{id = 7,
                 naziv = <<"Osnovne informacije">>,
                 opis = <<"Osnovne informacije o kolegiju UzDiz">>}].

sadrzaj() ->
    [#db_sadrzaj{id = 1,
                 naziv = <<"Model praćenja Mat1">>,
                 tip = poveznica,
                 vrijednost = poveznica()},
     #db_sadrzaj{id = 2,
                 naziv = <<"Model praćenja Mat2">>,
                 tip = poveznica,
                 vrijednost = poveznica()},
     #db_sadrzaj{id = 3,
                 naziv = <<"Nastavni Plan Mat1">>,
                 tip = poveznica,
                 vrijednost = lekcija()},
     #db_sadrzaj{id = 4,
                 naziv = <<"Nastavni Plan Mat2">>,
                 tip = poveznica,
                 vrijednost = lekcija()},
     #db_sadrzaj{id = 5,
                 naziv = <<"Nastavni Program Mat1">>,
                 tip = poveznica,
                 vrijednost = poveznica()},
     #db_sadrzaj{id = 6,
                 naziv = <<"Nastavni Program Mat2">>,
                 tip = poveznica,
                 vrijednost = dokument()}].

poveznica() ->
    #poveznica{referenca = <<"www.google.hr">>,
               vrijeme_kreiranja = calendar:universal_time()}.

dokument() ->
    #dokument{referenca = <<"www.google.hr">>, vrijeme_kreiranja = calendar:universal_time()}.

lekcija() ->
    #lekcija{sadrzaj = <<"Sadržaj lekcije 1">>,
             vrijeme_kreiranja = calendar:universal_time()}.
