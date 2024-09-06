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
             #db_kolegij_sekcija{id_kolegij = 1, id_sekcija = 8},
             #db_kolegij_sekcija{id_kolegij = 1, id_sekcija = 9},
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
             % #db_sekcija_sadrzaj{id_sekcija = 2, id_sadrzaj = 2},
             % #db_sekcija_sadrzaj{id_sekcija = 2, id_sadrzaj = 4},
             % #db_sekcija_sadrzaj{id_sekcija = 2, id_sadrzaj = 6},
             #db_sekcija_sadrzaj{id_sekcija = 8, id_sadrzaj = 7},
             #db_sekcija_sadrzaj{id_sekcija = 8, id_sadrzaj = 8},
             #db_sekcija_sadrzaj{id_sekcija = 8, id_sadrzaj = 11},
             #db_sekcija_sadrzaj{id_sekcija = 9, id_sadrzaj = 9},
             #db_sekcija_sadrzaj{id_sekcija = 9, id_sadrzaj = 10}].

fakultet_korisnik_keys() ->
            [#db_fakultet_korisnik{id_korisnik = 1, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 2, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 3, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 4, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 5, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 7, id_fakultet = 1},
             #db_fakultet_korisnik{id_korisnik = 6, id_fakultet = 1}].

katedra_djelatnik_keys() ->
            [#db_katedra_djelatnik{id_katedra = 3,
                                   id_djelatnik = 4,
                                   tip = voditelj},
             #db_katedra_djelatnik{id_katedra = 3,
                                   id_djelatnik = 5,
                                   tip = suradnik}].

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
                          opis = <<"Student na Fakultetu Organizacije i Informatike"/utf8>>},
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
                          uloga = profesor,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"mmihic@foi.hr"/utf8>>,
                          dodatno = #djelatnik{kabinet = <<"135"/utf8>>, vrijeme_konzultacija = []},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 5,
                          ime = <<"Ana"/utf8>>,
                          prezime = <<"Anić"/utf8>>,
                          oib = 5,
                          slika = <<"21104.png">>,
                          uloga = asistent,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"aanic@foi.hr"/utf8>>,
                          dodatno = #djelatnik{kabinet = <<"135"/utf8>>, vrijeme_konzultacija = []},
                          opis = <<"Opis"/utf8>>},
             #db_korisnik{id = 7,
                          ime = <<"Kruno"/utf8>>,
                          prezime = <<"Krunić"/utf8>>,
                          oib = 7,
                          slika = <<"21104.png">>,
                          uloga = dekan,
                          lozinka = <<"lozinka"/utf8>>,
                          email = <<"kkrunic@foi.hr"/utf8>>,
                          dodatno = #djelatnik{kabinet = <<"135"/utf8>>, vrijeme_konzultacija = []},
                          opis = <<"Dekan Fakulteta Organizacije i Informatike"/utf8>>},
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
                          naziv = <<"Fakultet Organizacije i Informatike">>,
                          opis =
                                      <<"SUZG FOI djeluje 60 godina, što je za studij suvremenih tehnologija "
                                        "dugo razdoblje. Tijekom tog razdoblja Fakultet obrazuje najstručnije "
                                        "kadrove u području informacijskih znanosti (informatike) i "
                                        "informacijskih tehnologija, kao i ekonomije, organizacije, "
                                        "komunikologije i drugih srodnih područja."/utf8>>,
                          skraceno = <<"FOI"/utf8>>,
                          logo = <<"foi.png">>,
                          lokacija = {46.30772093396054, 16.33808609928215},
                          adresa = adresa()},
             #db_fakultet{id = 2,
                          naziv = <<"Fakultet Elektrotehnike i Računarstva"/utf8>>,
                          opis = <<"Opis Fakulteta"/utf8>>,
                          skraceno = <<"FER"/utf8>>,
                          logo = <<"fer.jpg">>,
                          lokacija = {31.44, 116.44},
                          adresa = adresa()},
             #db_fakultet{id = 3,
                          naziv = <<"Ekonomski Fakultet Zagreb">>,
                          opis = <<"Opis Fakulteta">>,
                          skraceno = <<"EFZG"/utf8>>,
                          logo = <<"efzg.png">>,
                          lokacija = {31.44, 116.44},
                          adresa = adresa()},
             #db_fakultet{id = 4,
                          naziv = <<"Filozofski Fakultet Zagreb">>,
                          opis = <<"Opis Fakulteta">>,
                          skraceno = <<"FFZG"/utf8>>,
                          logo = <<"ffzg.jpg">>,
                          lokacija = {31.44, 116.44},
                          adresa = adresa()},
             #db_fakultet{id = 5,
                          naziv = <<"Fakultet Kemijskog Inžinjerstva i Tehnologije"/utf8>>,
                          opis = <<"Opis Fakulteta">>,
                          skraceno = <<"FKIT"/utf8>>,
                          logo = <<"fkit.jpg">>,
                          lokacija = {31.44, 116.44},
                          adresa = adresa()}].

adresa() ->
            #adresa{ulica = <<"Pavlinska ulica"/utf8>>,
                    grad = <<"Varaždin"/utf8>>,
                    postanski_broj = 42000,
                    drzava = <<"Hrvatska"/utf8>>,
                    kucni_broj = <<"2"/utf8>>}.

katedre() ->
            [#db_katedra{id = 1,
                         naziv = <<"Katedra za gospodatstvo"/utf8>>,
                         opis = <<"Opis Katedre">>},
             #db_katedra{id = 2,
                         naziv = <<"Katedra za organizaciju"/utf8>>,
                         opis = <<"Opis Katedre">>},
             #db_katedra{id = 3,
                         naziv = <<"Katedra za kvantitivne metode"/utf8>>,
                         opis =
                                     <<"Katedra za kvantitativne metode bavi se proučavanjem i primjenom "
                                       "matematičkih i statističkih tehnika u analizi podataka, optimizaciji "
                                       "i donošenju odluka. Studenti stječu vještine za korištenje "
                                       "kvantitativnih metoda u poslovnim i ekonomskim situacijama, "
                                       "te razvijaju sposobnost analitičkog razmišljanja i rješavanja "
                                       "problema. Cilj katedre je osposobiti studente za primjenu kvantitati"
                                       "vnih alata u stvarnom svijetu, uz interdisciplinarni pristup.">>},
             #db_katedra{id = 4,
                         naziv = <<"Katedra za informatičke tehnologije i računarstvo"/utf8>>,
                         opis = <<"Opis Katedre">>},
             #db_katedra{id = 5,
                         naziv = <<"Katedra za razvoj informacijskih sustava"/utf8>>,
                         opis = <<"Opis Katedre">>}].

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
                         opis = <<"Osnovne informacije o kolegiju Mat1"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 2,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Mat2"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 3,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju DP"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 4,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju OPM"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 5,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Prog1"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 6,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju Prog2"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 7,
                         naziv = <<"Osnovne informacije"/utf8>>,
                         opis = <<"Osnovne informacije o kolegiju UzDiz"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 8,
                         naziv = <<"Uvod u matematičku indukciju"/utf8>>,
                         opis =
                                     <<"Uvodno poglavlje o pojmu matematičke indukcije unutar kojega "
                                       "dolazimo do spoznaje logičkog razmišljanja"/utf8>>,
                         vidljivo = true},
             #db_sekcija{id = 9,
                         naziv = <<"Teorija grafova"/utf8>>,
                         opis =
                                     <<"Teorija grafova jedno je od poglavlja matematike koje je moguće "
                                       "korisititi u svakodnevnom životu"/utf8>>,
                         vidljivo = true}].

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
                         naziv = <<"Nastavni Plan"/utf8>>,
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
                         vrijednost = dokument()},
             #db_sadrzaj{id = 7,
                         naziv = <<"Matematička indukcija"/utf8>>,
                         tip = dokument,
                         vrijednost = dokument()},
             #db_sadrzaj{id = 8,
                         naziv = <<"Video pojašnjenja"/utf8>>,
                         tip = poveznica,
                         vrijednost = poveznica()},
             #db_sadrzaj{id = 9,
                         naziv = <<"Šetnje grafom"/utf8>>,
                         tip = dokument,
                         vrijednost = dokument()},
             #db_sadrzaj{id = 10,
                         naziv = <<"Problem kineskog poštara"/utf8>>,
                         tip = lekcija,
                         vrijednost = lekcija()},
             #db_sadrzaj{id = 11,
                         naziv = <<"Predikatna logika"/utf8>>,
                         tip = lekcija,
                         vrijednost = lekcija()}].

poveznica() ->
            #poveznica{referenca = <<"https://www.google.hr"/utf8>>,
                       vrijeme_kreiranja = calendar:universal_time()}.

dokument() ->
            #dokument{referenca = <<"https://www.google.hr"/utf8>>,
                      vrijeme_kreiranja = calendar:universal_time()}.

lekcija() ->
            #lekcija{sadrzaj =
                                 <<"U današnje informacijsko doba, učinkovito upravljanje organizacijama "
                                   "zahtijeva pristupe različite od tradicionalnog strateškog planiranja"
                                   ", karakterističnog za uspješne organizacije industrijskog doba. "
                                   "Pored poznavanja i primjene metoda upravljanja, za učinkovito "
                                   "upravljanje organizacijama zahtijeva se i primjena suvremenih "
                                   "informacijsko komunikacijskih tehnologija u potpori metodama "
                                   "upravljanja. Misija organizacije određuje razlog njenog postojanja, "
                                   "dok vizija opisuje organizaciju u budućnosti. Konkretizacijom "
                                   "vizije menadžment definira strateške ciljeve koje u određenom "
                                   "vremenskom periodu organizacija mora postići. Procjenom spremnosti "
                                   "za ostvarenje pojedinog strateškog cilja, dobivaju se strategije, "
                                   "aktivnosti i ciljevi koje pojedine organizacijske razine moraju "
                                   "postići. Sukladno metodi uravnoteženih bodovnih tablica (Balanced "
                                   "Scorecard - BSC), vrši se upravljanje organizacijom na temelju "
                                   "mjerenja pristupa ciljevima. Sve navedene metode imaju jasne "
                                   "rezultate svoje primjene, koji se mogu metamodelirati. Ovim "
                                   "postupkom pojašnjavaju se prirodni odnosi medu metodama upravljanja "
                                   "i definira se procesna i podatkovna arhitektura CASE alata "
                                   "za potporu upravljanju mjerenjem performansi. Ono što menadžment "
                                   "mora znati, da bi organizaciju odveo u vremenu prema viziji, "
                                   "jest koliko je, u svakom trenutku, daleko od postavljenih ciljeva. "
                                   "Ova informacija se može dobiti iz jedinstvenog informacijskog "
                                   "sustava koji daje potporu mjerenju performansi na svim organizacijsk"
                                   "im razinama, a utemeljen je na prethodno definiranom metamodelu.\nSt"
                                   "udenti će dobiti znanja potrebna za razvoj elemenata misije, "
                                   "vizije, strateških ciljeva, strategija, i mjera. Isto tako "
                                   "će se upoznati sa strukturom i načinom izrade plana mjerenja "
                                   "performansi, kao i sa dizajnom i uporabom potrebne informatičke "
                                   "potpore mjerenju performansi. Bit će osposobljeni za samostalnu "
                                   "izradu plana mjerenja performansi i njegovu implementaciju "
                                   "u alatu za potporu mjerenju."/utf8>>,
                     slika = <<"">>,
                     vrijeme_kreiranja = calendar:universal_time()}.
