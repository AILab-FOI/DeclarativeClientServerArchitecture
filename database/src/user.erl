-module(user).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj_studenta/7, dodaj_djelatnika/7, prijava/2, dohvati_korisnika/1,
         dohvati_korisnike/0, obrisi_korisnika/1, uredi_studenta/3, uredi_djelatnika/4,
         dodaj_studenta_na_kolegij/2, dodaj_djelatnika_na_kolegij/3, dodaj_djelatnika_na_katedru/2,
         dohvati_studenta_na_kolegiju/2, dohvati_kolegije_studenta/1]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka, Email, Opis, Nadimak) ->
    Dodatno = #student{nadimak = Nadimak},
    dodaj_korisnika(Ime, Prezime, Oib, Lozinka, Email, Opis, student, Dodatno).

dodaj_djelatnika(Ime, Prezime, Oib, Lozinka, Email, Opis, Kabinet) ->
    Dodatno = #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = []},
    dodaj_korisnika(Ime, Prezime, Oib, Lozinka, Email, Opis, djelatnik, Dodatno).

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
    Fun = fun() -> mnesia:delete({db_korisnik, Id}) end,
    mnesia:transaction(Fun).

uredi_studenta(Id, Opis, Nadimak) ->
    Dodatno = #student{nadimak = Nadimak},
    uredi_korisnika(Id, Opis, Dodatno).

uredi_djelatnika(Id, Opis, Kabinet, VrijemeKonzultacija) ->
    Dodatno = #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonzultacija},
    uredi_korisnika(Id, Opis, Dodatno).

uredi_korisnika(Id, Opis, Dodatno) ->
    Fun = fun() ->
             utils:read_secure(function,
                               db_korisnik,
                               Id,
                               fun(Korisnik) ->
                                  NoviKorisnik =
                                      Korisnik#db_korisnik{opis = Opis, dodatno = Dodatno},
                                  utils:write_secure(object, NoviKorisnik, {ok, Id})
                               end)
          end,
    mnesia:transaction(Fun).

dohvati_studenta(Korisnik) ->
    NoviKorisnik =
        Korisnik#db_korisnik{dodatno =
                                 #{nadimak => Korisnik#db_korisnik.dodatno#student.nadimak}},
    korisnik_object(NoviKorisnik).

dohvati_djelatnika(Korisnik) ->
    NoviKorisnik =
        Korisnik#db_korisnik{dodatno =
                                 #{kabinet => Korisnik#db_korisnik.dodatno#djelatnik.kabinet,
                                   vrijeme_konzultacija =>
                                       Korisnik#db_korisnik.dodatno#djelatnik.vrijeme_konzultacija}},
    korisnik_object(NoviKorisnik).

dohvati_korisnika(Id) ->
    Fun = fun() ->
             case mnesia:read({db_korisnik, Id}) of
                 [Korisnik] ->
                     case Korisnik#db_korisnik.uloga =:= student of
                         true -> dohvati_studenta(Korisnik);
                         false -> dohvati_djelatnika(Korisnik)
                     end;
                 [] -> {error, "Korisnik ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj_studenta_na_kolegij(IdStudent, IdKolegij) ->
    Fun = fun() -> mnesia:write(#db_student_kolegij{id = {IdStudent, IdKolegij}, ocjene = []})
          end,
    mnesia:transaction(Fun).

dodaj_djelatnika_na_kolegij(IdDjelatnik, IdKolegij, Status) ->
    Fun = fun() ->
             mnesia:write(#db_djelatnik_kolegij{id = {IdDjelatnik, IdKolegij}, status = Status})
          end,
    mnesia:transaction(Fun).

dodaj_djelatnika_na_katedru(IdDjelatnik, IdKatedra) ->
    Fun = fun() ->
             mnesia:write(#db_katedra_djelatnik{id_katedra = IdKatedra, id_djelatnik = IdDjelatnik})
          end,
    mnesia:transaction(Fun).

dohvati_studenta_na_kolegiju(IdStudent, IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}, ocjene = Ocjene})
                               when Student =:= IdStudent andalso Kolegij =:= IdKolegij ->
                               {Student, Ocjene}
                            end),
             case mnesia:select(db_student_kolegij, Match) of
                 [] -> {"Student nije na kolegiju"};
                 [{StudentId, Ocjene}] ->
                     {atomic, Result} = dohvati_korisnika(StudentId),
                     #{student => Result, ocjene => Ocjene}
             end
          end,
    mnesia:transaction(Fun).

dohvati_kolegije_studenta(IdStudent) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}, ocjene = Ocjene})
                               when Student =:= IdStudent ->
                               Kolegij
                            end),
             case mnesia:select(db_student_kolegij, Match) of
                 [] -> {"Student nema kolegija"};
                 Kolegiji ->
                     NoviKolegiji =
                         lists:map(fun(Kolegij) ->
                                      {atomic, Result} = course:dohvati_kolegij(Kolegij),
                                      Result
                                   end,
                                   Kolegiji),
                     #{student => IdStudent, kolegiji => NoviKolegiji}
             end
          end,
    mnesia:transaction(Fun).

korisnik_object(Korisnik) ->
    #{id => Korisnik#db_korisnik.id,
      ime => Korisnik#db_korisnik.ime,
      prezime => Korisnik#db_korisnik.prezime,
      oib => Korisnik#db_korisnik.oib,
      email => Korisnik#db_korisnik.email,
      opis => Korisnik#db_korisnik.opis,
      uloga => Korisnik#db_korisnik.uloga,
      dodatno => Korisnik#db_korisnik.dodatno}.

dohvati_korisnike() ->
    Fun = fun(#db_korisnik{id = Id,
                           ime = Ime,
                           prezime = Prezime,
                           oib = Oib,
                           opis = Opis,
                           uloga = Uloga,
                           email = Email,
                           dodatno = Dodatno},
              Acc) ->
             Extra = model_switch(Uloga, Dodatno),
             [#{id => Id,
                ime => Ime,
                prezime => Prezime,
                oib => Oib,
                email => Email,
                opis => Opis,
                uloga => parse_role(Uloga),
                dodatno => Extra}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_korisnik) end),
    Record.

parse_role(student) ->
    <<"Student">>;
parse_role(djelatnik) ->
    <<"Djelatnik">>.

model_switch(Role, Extra) ->
    case Role of
        student ->
            #student{nadimak = Nadimak} = Extra,
            #{nadimak => Nadimak};
        djelatnik ->
            #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonz} = Extra,
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
