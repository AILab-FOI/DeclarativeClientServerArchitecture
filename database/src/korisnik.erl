-module(korisnik).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj_studenta/7, dodaj_djelatnika/8, prijava/2, dohvati_korisnika/2,
         dohvati_korisnike/0, obrisi_korisnika/1, uredi_studenta/4, uredi_djelatnika/5,
         uredi_korisnika/4, map_to_record/1, map_to_record_student/1, map_to_record_djelatnik/1]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka, Email, Opis, Nadimak) ->
    Dodatno = #student{nadimak = Nadimak},
    dodaj_korisnika(Ime, Prezime, Oib, Lozinka, Email, Opis, student, Dodatno).

dodaj_djelatnika(Ime, Prezime, Oib, Lozinka, Uloga, Email, Opis, Kabinet) ->
    Dodatno = #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = []},
    dodaj_korisnika(Ime, Prezime, Oib, Lozinka, Email, Opis, Uloga, Dodatno).

dodaj_korisnika(Ime, Prezime, Oib, Lozinka, Email, Opis, Uloga, Dodatno) ->
    Id = ?ID,
    Salt = crypto:strong_rand_bytes(16),
    Hash = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
    Fun = fun() ->
             case mnesia:index_read(db_korisnik, Oib, #db_korisnik.oib) == []
                  andalso mnesia:index_read(db_korisnik, Email, #db_korisnik.email) == []
             of
                 false -> {error, "Korisnik postoji"};
                 true ->
                     case mnesia:write(#db_korisnik{id = Id,
                                                    ime = Ime,
                                                    prezime = Prezime,
                                                    oib = Oib,
                                                    lozinka = {Hash, Salt},
                                                    opis = Opis,
                                                    slika = <<"21104.png">>,
                                                    email = Email,
                                                    uloga = Uloga,
                                                    dodatno = Dodatno})
                     of
                         ok -> {ok, Id};
                         _ -> {error, "Transakcija neuspješna"}
                     end
             end
          end,
    mnesia:transaction(Fun).

obrisi_korisnika(Id) ->
    Fun = fun() ->
             student_kolegij:obrisi_studenta(Id),
             djelatnik_kolegij:obrisi_djelatnika(Id),
             fakultet_korisnik:obrisi_korisnika(Id),
             katedra_djelatnik:obrisi_djelatnika(Id),
             mnesia:delete({db_korisnik, Id})
          end,
    mnesia:transaction(Fun).

uredi_studenta(Id, Opis, Nadimak, Slika) ->
    Dodatno = #student{nadimak = Nadimak},
    uredi_korisnika(Id, Opis, Dodatno, Slika).

uredi_djelatnika(Id, Opis, Kabinet, VrijemeKonzultacija, Slika) ->
    Dodatno = #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonzultacija},
    uredi_korisnika(Id, Opis, Dodatno, Slika).

uredi_korisnika(Id, Opis, Dodatno, Slika) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_korisnik,
                                    Id,
                                    fun(Korisnik) ->
                                       io:format("~p~n", [Slika]),
                                       NoviKorisnik =
                                           Korisnik#db_korisnik{opis = Opis,
                                                                dodatno = Dodatno,
                                                                slika = Slika},
                                       io:format("~p~n", [NoviKorisnik]),
                                       operations:write_secure(object, NoviKorisnik, {ok, Id})
                                    end)
          end,
    mnesia:transaction(Fun).

dohvati_korisnika(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_korisnik, Id}) of
                 [Korisnik] -> ucitaj(Type, Korisnik);
                 [] -> {error, "Korisnik ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

map_to_record(#{<<"id">> := Id,
                <<"ime">> := Ime,
                <<"prezime">> := Prezime,
                <<"oib">> := Oib,
                <<"lozinka">> := Lozinka,
                <<"email">> := Email,
                <<"uloga">> := Uloga,
                <<"opis">> := Opis,
                <<"dodatno">> := Dodatno,
                <<"slika">> := Slika}) ->
    case Uloga =:= student of
        true ->
            #db_korisnik{ime = Ime,
                         prezime = Prezime,
                         oib = Oib,
                         lozinka = Lozinka,
                         email = Email,
                         uloga = Uloga,
                         opis = Opis,
                         slika = Slika,
                         dodatno = map_to_record_student(Dodatno)};
        false ->
            #db_korisnik{ime = Ime,
                         prezime = Prezime,
                         oib = Oib,
                         lozinka = Lozinka,
                         email = Email,
                         uloga = Uloga,
                         opis = Opis,
                         slika = Slika,
                         dodatno = map_to_record_djelatnik(Dodatno)}
    end.

map_to_record_student(#{nadimak := Nadimak}) ->
    #student{nadimak = Nadimak}.

map_to_record_djelatnik(#{kabinet := Kabinet,
                          vrijeme_konzultacija := VrijemeKonzultacija}) ->
    #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonzultacija}.

ucitaj(core, R) ->
    transform_korisnik(R);
ucitaj(fakultet, R) ->
    M0 = transform_korisnik(R),
    fakultet_korisnik:ucitaj_fakultet(M0);
ucitaj(kolegiji, R) ->
    M0 = transform_korisnik(R),
    M1 = fakultet_korisnik:ucitaj_fakultet(M0),
    case R#db_korisnik.uloga =:= student of
        true ->
            student_kolegij:ucitaj_kolegije(M1);
        false ->
            M2 = djelatnik_kolegij:ucitaj_kolegije(M1),
            katedra_djelatnik:ucitaj_katedre(M2)
    end.

transform_korisnik(Korisnik) ->
    #{id => Korisnik#db_korisnik.id,
      ime => Korisnik#db_korisnik.ime,
      prezime => Korisnik#db_korisnik.prezime,
      oib => Korisnik#db_korisnik.oib,
      slika => Korisnik#db_korisnik.slika,
      email => Korisnik#db_korisnik.email,
      opis => Korisnik#db_korisnik.opis,
      uloga => parse_role(Korisnik#db_korisnik.uloga),
      dodatno => model_switch(Korisnik)}.

dohvati_korisnike() ->
    Fun = fun(#db_korisnik{} = M, Acc) -> [ucitaj(kolegiji, M) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_korisnik) end).

parse_role(student) ->
    <<"Student">>;
parse_role(dekan) ->
    <<"Dekan">>;
parse_role(profesor) ->
    <<"Profesor">>;
parse_role(asistent) ->
    <<"Asistent">>.

model_switch(#db_korisnik{uloga = Uloga, dodatno = Dodatno}) ->
    case Uloga of
        student ->
            #student{nadimak = Nadimak} = Dodatno,
            #{nadimak => Nadimak};
        _ ->
            #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonz} = Dodatno,
            #{kabinet => Kabinet, vrijeme_konzultacija => VrijemeKonz}
    end.

prijava(Email, Lozinka) ->
    Fun = fun() ->
             case mnesia:index_read(db_korisnik, Email, #db_korisnik.email) of
                 [#db_korisnik{id = Id, lozinka = {Hash, Salt}}] ->
                     Key = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
                     case Key == Hash of
                         true -> {ok, #{id => Id}};
                         false -> {error, "Lozinka nije ispravna"}
                     end;
                 [] -> {error, "Korisnik ne postoji"}
             end
          end,
    mnesia:transaction(Fun).
