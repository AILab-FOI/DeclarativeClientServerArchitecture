-module(sekcija).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/2, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/3]).

dohvati() ->
    Fun = fun(Sekcija, Acc) -> [ucitaj(core, Sekcija) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_sekcija) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_sekcija, Id}) of
                 [Sekcija] -> ucitaj(full, Sekcija);
                 [] -> {error, "Sekcija ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_sekcija, Id}) of
                 [Sekcija] -> ucitaj(Type, Sekcija);
                 [] -> {error, "Sekcija ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, Opis) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_sekcija{id = Id,
                                           naziv = Naziv,
                                           opis = Opis})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi(Id) ->
    Fun = fun() ->
             sekcija_sadrzaj:obrisi_sekcija(Id),
             mnesia:delete({db_sekcija, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, Opis) ->
    Fun = fun() ->
             case mnesia:write(#db_sekcija{id = Id,
                                           naziv = Naziv,
                                           opis = Opis})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_sekcija(R);
ucitaj(full, R) ->
    M0 = transform_sekcija(R),
    sekcija_sadrzaj:ucitaj_sadrzaj(M0).

transform_sekcija(#db_sekcija{id = Id,
                              naziv = Naziv,
                              opis = Opis}) ->
    #{id => Id,
      naziv => Naziv,
      opis => Opis}.
