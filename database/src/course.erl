-module(course).

-include_lib("database/include/records.hrl").

-export([dohvati_kolegij/1, dodaj_kolegij/7, dohvati_kolegije/0, obrisi_kolegij/1]).

dodaj_kolegij(Ime, Prezime, Oib, Email, Opis, Uloga, Dodatno) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:index_read(db_korisnik, Oib, #db_korisnik.oib) == []
                  andalso mnesia:index_read(db_korisnik, Email, #db_korisnik.email) == []
             of
                 false -> {error, "Korisnik postoji"};
                 true ->
                     case mnesia:write(#db_kolegij{id = Id}) of
                         ok -> {ok, Id};
                         _ -> {error, "Transakcija neuspješna"}
                     end
             end
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(Id) ->
    Fun = fun() -> mnesia:delete({db_kolegij, Id}) end,
    mnesia:transaction(Fun).

dohvati_kolegij(Id) ->
    Fun = fun() ->
             case mnesia:read({db_korisnik, Id}) of
                 [#db_korisnik{ime = Ime,
                               prezime = Prezime,
                               oib = Oib,
                               email = Email,
                               opis = Opis,
                               uloga = Uloga,
                               dodatno = Dodatno}] ->
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

dohvati_kolegije() ->
    Fun = fun(#db_korisnik{id = Id,
                           ime = Ime,
                           prezime = Prezime,
                           oib = Oib,
                           opis = Opis,
                           email = Email,
                           dodatno = Dodatno},
              Acc) ->
             [#{id => Id,
                ime => Ime,
                prezime => Prezime,
                oib => Oib,
                email => Email,
                opis => Opis,
                dodatno => Dodatno}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end),
    Record.
