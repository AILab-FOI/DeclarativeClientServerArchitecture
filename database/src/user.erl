-module(user).

-include_lib("database/include/records.hrl").

-export([dodaj_studenta/6, dodaj_djelatnika/7, prijava/2, dohvati_korisnika/1,
         dohvati_korisnike/0, obrisi_korisnika/1]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka, Email, Opis) ->
    Dodatno = #student{id_kolegij = []},
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

dohvati_korisnika(Id) ->
    Fun = fun() ->
             case mnesia:read({db_korisnik, Id}) of
                 [#db_korisnik{ime = Ime,
                               prezime = Prezime,
                               oib = Oib,
                               email = Email,
                               opis = Opis,
                               uloga = Uloga,
                               dodatno = Dodatno}] ->
                     % Kolegiji =
                     %     lists:map(fun(Kolegij) ->
                     %                  case mnesia:read({db_kolegij, Kolegij}) of
                     %                      [#db_kolegij{id = IdK, naziv = NazivK}] -> {IdK, NazivK};
                     %                      [] -> undefined
                     %                  end
                     %               end,
                     %               ),
                     #{id => Id,
                       ime => Ime,
                       prezime => Prezime,
                       oib => Oib,
                       email => Email,
                       opis => Opis,
                       uloga => Uloga,
                       dodatno => Dodatno};
                 [] -> {error, "Korisnik ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

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
            #student{id_kolegij = Kolegij} = Extra,
            #{kolegij => Kolegij};
        djelatnik ->
            #djelatnik{kabinet = Kabinet, vrijeme_konzultacija = VrijemeKonz} = Extra,
            #{kabinet => Kabinet, vrijeme_konzultacija => VrijemeKonz}
    end.

prijava(Email, Lozinka) ->
    Fun = fun() ->
             case mnesia:index_read(db_student, Email, #db_korisnik.email) of
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
