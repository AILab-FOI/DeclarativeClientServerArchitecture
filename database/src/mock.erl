-module(mock).

-include_lib("database/include/records.hrl").

-export([ucitaj_mock/0]).

ucitaj_mock() ->
            ucitaj(fakulteti()),
            ucitaj(katedre()),
            ucitaj(kolegiji()),
            ucitaj(sekcije()),
            ucitaj(sadrzaj()),
            ucitaj_korisnike(),
            ucitaj(fakultet_katedra_keys()),
            ucitaj(katedra_kolegij_keys()),
            ucitaj(kolegij_student_keys()),
            ucitaj(kolegij_djelatnik_keys()),
            ucitaj(fakultet_korisnik_keys()),
            ucitaj(katedra_djelatnik_keys()),
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

fakultet_korisnik_keys() ->
            [#db_fakultet_korisnik{id_korisnik = 1, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 2, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 3, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 4, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 5, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 6, id_fakultet = 1}].

katedra_djelatnik_keys() ->
            [#db_katedra_djelatnik{id_katedra = 3, id_djelatnik = 4},
             #db_katedra_djelatnik{id_katedra = 3, id_djelatnik = 5}].

kolegij_student_keys() ->
            [#db_student_kolegij{id = {1, 1}, ocjene = []},
             #db_student_kolegij{id = {2, 1}, ocjene = []},
             #db_student_kolegij{id = {3, 1}, ocjene = []},
             #db_student_kolegij{id = {6, 1}, ocjene = []},
             #db_student_kolegij{id = {1, 2}, ocjene = []},
             #db_student_kolegij{id = {2, 2}, ocjene = []},
             #db_student_kolegij{id = {3, 2}, ocjene = []},
             #db_student_kolegij{id = {6, 2}, ocjene = []},
             #db_student_kolegij{id = {1, 3}, ocjene = []},
             #db_student_kolegij{id = {2, 3}, ocjene = []},
             #db_student_kolegij{id = {3, 3}, ocjene = []},
             #db_student_kolegij{id = {6, 3}, ocjene = []},
             #db_student_kolegij{id = {1, 4}, ocjene = []},
             #db_student_kolegij{id = {2, 4}, ocjene = []},
             #db_student_kolegij{id = {3, 4}, ocjene = []},
             #db_student_kolegij{id = {6, 4}, ocjene = []},
             #db_student_kolegij{id = {1, 5}, ocjene = []},
             #db_student_kolegij{id = {2, 5}, ocjene = []},
             #db_student_kolegij{id = {3, 5}, ocjene = []},
             #db_student_kolegij{id = {6, 5}, ocjene = []},
             #db_student_kolegij{id = {1, 6}, ocjene = []},
             #db_student_kolegij{id = {2, 6}, ocjene = []},
             #db_student_kolegij{id = {3, 6}, ocjene = []},
             #db_student_kolegij{id = {6, 6}, ocjene = []},
             #db_student_kolegij{id = {1, 7}, ocjene = []},
             #db_student_kolegij{id = {2, 7}, ocjene = []},
             #db_student_kolegij{id = {3, 7}, ocjene = []},
             #db_student_kolegij{id = {6, 7}, ocjene = []},
             #db_student_kolegij{id = {1, 8}, ocjene = []},
             #db_student_kolegij{id = {2, 8}, ocjene = []},
             #db_student_kolegij{id = {3, 8}, ocjene = []},
             #db_student_kolegij{id = {6, 8}, ocjene = []},
             #db_student_kolegij{id = {1, 9}, ocjene = []},
             #db_student_kolegij{id = {2, 9}, ocjene = []},
             #db_student_kolegij{id = {3, 9}, ocjene = []},
             #db_student_kolegij{id = {6, 9}, ocjene = []},
             #db_student_kolegij{id = {1, 10}, ocjene = []},
             #db_student_kolegij{id = {2, 10}, ocjene = []},
             #db_student_kolegij{id = {3, 10}, ocjene = []},
             #db_student_kolegij{id = {6, 10}, ocjene = []}].

kolegij_djelatnik_keys() ->
            [#db_djelatnik_kolegij{id = {4, 1}, status = nositelj},
             #db_djelatnik_kolegij{id = {5, 1}, status = asistent},
             #db_djelatnik_kolegij{id = {4, 2}, status = nositelj},
             #db_djelatnik_kolegij{id = {5, 2}, status = asistent},
             #db_djelatnik_kolegij{id = {4, 6}, status = nositelj},
             #db_djelatnik_kolegij{id = {5, 6}, status = asistent},
             #db_djelatnik_kolegij{id = {4, 9}, status = nositelj},
             #db_djelatnik_kolegij{id = {5, 9}, status = asistent}].

ucitaj(L) ->
            lists:foreach(fun(E) -> mnesia:dirty_write(E) end, L).

ucitaj_korisnike() ->
            lists:foreach(fun(#db_korisnik{lozinka = Lozinka} = Korisnik) ->
                             Salt = crypto:strong_rand_bytes(16),
                             Hash = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
                             mnesia:dirty_write(Korisnik#db_korisnik{lozinka = {Hash, Salt}})
                          end,
                          korisnici()).

korisnici() ->
            [#db_korisnik{id = 1,
                          ime = <<"Ivo"/utf8>>,
                          prezime = <<"Ivić"/utf8>>,
                          oib = 1,
                          slika = <<"21104.png">>,
                          uloga = student,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"iivic@foi.hr"/utf8>>,
                          dodatno = #student{nadimak = <<"Ivo"/utf8>>},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 2,
                          ime = <<"Petar"/utf8>>,
                          prezime = <<"Perić"/utf8>>,
                          oib = 2,
                          slika = <<"21104.png">>,
                          uloga = student,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"pperic@foi.hr"/utf8>>,
                          dodatno = #student{nadimak = <<"Pero"/utf8>>},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 3,
                          ime = <<"Ivana"/utf8>>,
                          prezime = <<"Ivančić"/utf8>>,
                          oib = 3,
                          slika = <<"21104.png">>,
                          uloga = student,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"iivancic@foi.hr"/utf8>>,
                          dodatno = #student{nadimak = <<"Ivana"/utf8>>},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 4,
                          ime = <<"Miho"/utf8>>,
                          prezime = <<"Mihić"/utf8>>,
                          oib = 4,
                          slika = <<"21104.png">>,
                          uloga = djelatnik,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"mmihic@foi.hr"/utf8>>,
                          dodatno = #djelatnik{kabinet = <<"135"/utf8>>, vrijeme_konzultacija = []},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 5,
                          ime = <<"Ana"/utf8>>,
                          prezime = <<"Anić"/utf8>>,
                          oib = 5,
                          slika = <<"21104.png">>,
                          uloga = djelatnik,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"aanic@foi.hr"/utf8>>,
                          dodatno = #djelatnik{kabinet = <<"135"/utf8>>, vrijeme_konzultacija = []},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 6,
                          ime = <<"Franjo"/utf8>>,
                          prezime = <<"Franjić"/utf8>>,
                          oib = 6,
                          slika = <<"21104.png">>,
                          uloga = student,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"ffranjic@foi.hr"/utf8>>,
                          dodatno = #student{nadimak = <<"Franjo"/utf8>>},
                          opis = <<"Opis"/utf8>>}].

fakulteti() ->
            [#db_fakultet{id = 1,
                          naziv = <<"FOI"/utf8>>,
                          logo = <<"">>,
                          adresa = adresa()},
             #db_fakultet{id = 2,
                          naziv = <<"FER"/utf8>>,
                          logo = <<"">>,
                          adresa = adresa()},
             #db_fakultet{id = 3,
                          naziv = <<"EFZG"/utf8>>,
                          logo = <<"">>,
                          adresa = adresa()},
             #db_fakultet{id = 4,
                          naziv = <<"FFZG"/utf8>>,
                          logo = <<"">>,
                          adresa = adresa()},
             #db_fakultet{id = 5,
                          naziv = <<"FKIT"/utf8>>,
                          logo = <<"">>,
                          adresa = adresa()}].

adresa() ->
            #adresa{ulica = <<"Pavlinska ulica"/utf8>>,
                    grad = <<"Varaždin"/utf8>>,
                    postanski_broj = 42000,
                    drzava = <<"Hrvatska"/utf8>>,
                    kucni_broj = <<"2"/utf8>>}.

katedre() ->
            [#db_katedra{id = 1, naziv = <<"Katedra za gospodatstvo"/utf8>>},
             #db_katedra{id = 2, naziv = <<"Katedra za organizaciju"/utf8>>},
             #db_katedra{id = 3, naziv = <<"Katedra za kvantitivne metode"/utf8>>},
             #db_katedra{id = 4,
                         naziv = <<"Katedra za informatičke tehnologije i računarstvo"/utf8>>},
             #db_katedra{id = 5, naziv = <<"Katedra za razvoj informacijskih sustava"/utf8>>}].

kolegiji() ->
            [#db_kolegij{id = 1,
                         naziv = <<"Matematika 1"/utf8>>,
                         slika = <<"21104.png">>,
                         skraceno = <<"Mat1"/utf8>>},
             #db_kolegij{id = 2,
                         naziv = <<"Matematika 2"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"Mat1"/utf8>>},
             #db_kolegij{id = 3,
                         naziv = <<"Deklarativno programiranje"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"DP"/utf8>>},
             #db_kolegij{id = 4,
                         naziv = <<"Programiranje 1"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"Prog1"/utf8>>},
             #db_kolegij{id = 5,
                         naziv = <<"Programiranje 2"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"Prog2"/utf8>>},
             #db_kolegij{id = 6,
                         naziv = <<"Teorija odlučivanja"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"TO"/utf8>>},
             #db_kolegij{id = 7,
                         naziv = <<"Uzorci dizajna"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"UzDiz"/utf8>>},
             #db_kolegij{id = 8,
                         naziv = <<"Napredne web tehnologije i sustavi"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"Nwtis"/utf8>>},
             #db_kolegij{id = 9,
                         naziv = <<"Odabrana poglavlja matematike"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"OPM"/utf8>>},
             #db_kolegij{id = 10,
                         naziv = <<"Računalna grafika"/utf8>>,
                         slika = <<"">>,
                         skraceno = <<"RG"/utf8>>}].

sekcije() ->
            [#db_sekcija{id = 1,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Mat1"/utf8>>},
             #db_sekcija{id = 2,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Mat2"/utf8>>},
             #db_sekcija{id = 3,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju DP"/utf8>>},
             #db_sekcija{id = 4,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju OPM"/utf8>>},
             #db_sekcija{id = 5,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Prog1"/utf8>>},
             #db_sekcija{id = 6,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Prog2"/utf8>>},
             #db_sekcija{id = 7,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju UzDiz"/utf8>>}].

sadrzaj() ->
            [#db_sadrzaj{id = 1,
                         naziv = <<"Model praćenja Mat1"/utf8>>,
                         tip = poveznica,
                         vrijednost = poveznica()},
             #db_sadrzaj{id = 2,
                         naziv = <<"Model praćenja Mat2"/utf8>>,
                         tip = poveznica,
                         vrijednost = poveznica()},
             #db_sadrzaj{id = 3,
                         naziv = <<"Nastavni Plan Mat1"/utf8>>,
                         tip = lekcija,
                         vrijednost = lekcija()},
             #db_sadrzaj{id = 4,
                         naziv = <<"Nastavni Plan Mat2"/utf8>>,
                         tip = lekcija,
                         vrijednost = lekcija()},
             #db_sadrzaj{id = 5,
                         naziv = <<"Nastavni Program Mat1"/utf8>>,
                         tip = poveznica,
                         vrijednost = poveznica()},
             #db_sadrzaj{id = 6,
                         naziv = <<"Nastavni Program Mat2"/utf8>>,
                         tip = dokument,
                         vrijednost = dokument()}].

poveznica() ->
            #poveznica{referenca = <<"https://www.google.hr"/utf8>>,
                       vrijeme_kreiranja = calendar:universal_time()}.

dokument() ->
            #dokument{referenca = <<"https://www.google.hr"/utf8>>,
                      vrijeme_kreiranja = calendar:universal_time()}.

lekcija() ->
            #lekcija{sadrzaj = <<"Sadržaj lekcije 1"/utf8>>,
                     slika = <<"">>,
                     vrijeme_kreiranja = calendar:universal_time()}.
