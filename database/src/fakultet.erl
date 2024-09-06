-module(fakultet).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/4, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/4,
         map_to_record_adresa/1]).

dohvati() ->
    Fun = fun(Fakultet, Acc) -> [ucitaj(core, Fakultet) | Acc] end,
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

dodaj(Naziv, Opis, Adresa, Lokacija) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_fakultet{id = Id,
                                            naziv = Naziv,
                                            lokacija = Lokacija,
                                            opis = Opis,
                                            logo = <<"21104.png">>,
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

uredi(Id, Naziv, Opis, Logo) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_fakultet,
                                    Id,
                                    fun(Fakultet) ->
                                       NoviFakultet =
                                           Fakultet#db_fakultet{opis = Opis,
                                                                naziv = Naziv,
                                                                logo = Logo},
                                       operations:write_secure(object, NoviFakultet, {ok, Id})
                                    end)
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_fakultet(R);
ucitaj(katedre, R) ->
    M0 = transform_fakultet(R),
    fakultet_katedra:ucitaj_katedre(M0);
ucitaj(korisnici, R) ->
    M0 = transform_fakultet(R),
    fakultet_korisnik:ucitaj_korisnike(M0);
ucitaj(full, R) ->
    M0 = transform_fakultet(R),
    M1 = fakultet_katedra:ucitaj_katedre(M0),
    fakultet_korisnik:ucitaj_korisnike(M1).

transform_fakultet(#db_fakultet{id = Id,
                                naziv = Naziv,
                                logo = Logo,
                                skraceno = Skraceno,
                                opis = Opis,
                                lokacija = {Lat, Long},
                                adresa =
                                    #adresa{grad = Grad,
                                            ulica = Ulica,
                                            drzava = Drzava,
                                            kucni_broj = KucniBroj,
                                            postanski_broj = PostanskiBroj}}) ->
    #{id => Id,
      naziv => Naziv,
      logo => Logo,
      skraceno => Skraceno,
      opis => Opis,
      lokacija => #{lat => Lat, long => Long},
      adresa =>
          #{grad => Grad,
            ulica => Ulica,
            drzava => Drzava,
            kucni_broj => KucniBroj,
            postanski_broj => PostanskiBroj}}.

map_to_record_adresa(#{grad := Grad,
                       ulica := Ulica,
                       postanski_broj := PostanskiBroj,
                       drzava := Drzava,
                       kucni_broj := KucniBroj}) ->
    #adresa{ulica = Ulica,
            grad = Grad,
            postanski_broj = PostanskiBroj,
            drzava = Drzava,
            kucni_broj = KucniBroj}.
