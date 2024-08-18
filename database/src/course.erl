-module(course).

-include_lib("database/include/records.hrl").

-export([dohvati_kolegij/1, dodaj_kolegij/2, dohvati_kolegije/0, obrisi_kolegij/1,
         dodaj_sekciju/3, obrisi_sekciju/1, uredi_sekciju/4, dodaj_sadrzaj/3, obrisi_sadrzaj/1,
         uredi_sadrzaj/3]).

dodaj_kolegij(Naziv, Skraceno) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:index_read(db_kolegij, Naziv, #db_kolegij.naziv) == [] of
                 false -> {error, "Korisnik postoji"};
                 true ->
                     case mnesia:write(#db_kolegij{id = Id,
                                                   naziv = Naziv,
                                                   skraceno = Skraceno,
                                                   sekcije = [],
                                                   sudionici = []})
                     of
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
             case mnesia:read({db_kolegij, Id}) of
                 [#db_kolegij{naziv = Naziv,
                              skraceno = Skraceno,
                              sudionici = Sudionici,
                              sekcije = Sekcije}] ->
                     #{id => Id,
                       naziv => Naziv,
                       skraceno => Skraceno,
                       sudionici => Sudionici,
                       sekcije => Sekcije};
                 [] -> {error, "Kolegij ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati_kolegije() ->
    Fun = fun(#db_kolegij{id = Id,
                          naziv = Naziv,
                          skraceno = Skraceno,
                          sudionici = Sudionici,
                          sekcije = Sekcije},
              Acc) ->
             % io:format(Id),
             NoveSekcije =
                 lists:map(fun(Sekcija) ->
                              case mnesia:read({db_sekcija, Sekcija}) of
                                  [#db_sekcija{id = IdS,
                                               naziv = NazivS,
                                               opis = Opis,
                                               sadrzaj = Sadrzaj}] ->
                                      #{id => IdS,
                                        naziv => NazivS,
                                        opis => Opis,
                                        sadrzaj => Sadrzaj};
                                  [] -> undefined
                              end
                           end,
                           Sekcije),
             io:format("~p", [NoveSekcije]),
             [#{id => Id,
                naziv => Naziv,
                skraceno => Skraceno,
                sudionici => Sudionici,
                sekcije => NoveSekcije}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end),
    Record.

dodaj_sekciju(Id, Naziv, Opis) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_kolegij,
                                    Id,
                                    fun(Kolegij) ->
                                       Sekcija =
                                           #db_sekcija{id = ?ID,
                                                       naziv = Naziv,
                                                       opis = Opis,
                                                       sadrzaj = []},
                                       operations:write_secure(function,
                                                               Sekcija,
                                                               fun() ->
                                                                  NoviKolegij =
                                                                      Kolegij#db_kolegij{sekcije =
                                                                                             [Sekcija#db_sekcija.id
                                                                                              | Kolegij#db_kolegij.sekcije]},
                                                                  operations:write_secure(object,
                                                                                          NoviKolegij,
                                                                                          {ok, Id})
                                                               end)
                                    end)
          end,
    mnesia:transaction(Fun).

uredi_sekciju(Id, Naziv, Opis, Sadrzaj) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_sekcija,
                                    Id,
                                    fun(Sekcija) ->
                                       NovaSekcija =
                                           Sekcija#db_sekcija{naziv = Naziv,
                                                              opis = Opis,
                                                              sadrzaj = Sadrzaj},
                                       operations:write_secure(obj, NovaSekcija, {ok, Id})
                                    end)
          end,
    mnesia:transaction(Fun).

obrisi_sekciju(Id) ->
    Fun = fun() -> mnesia:delete({db_sekcija, Id}) end,
    mnesia:transaction(Fun).

dodaj_sadrzaj(IdSekcija, Tip, Obj) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    {db_sekcija, IdSekcija},
                                    fun(Sekcija) ->
                                       Sadrzaj =
                                           #db_sadrzaj{id = ?ID,
                                                       tip = Tip,
                                                       vrijednost = Obj},
                                       operations:write_secure(function,
                                                               Sadrzaj,
                                                               fun() ->
                                                                  NovaSekcija =
                                                                      Sekcija#db_sekcija{sadrzaj =
                                                                                             [Sadrzaj
                                                                                              | Sekcija#db_sekcija.sadrzaj]},
                                                                  operations:write_secure(object,
                                                                                          NovaSekcija,
                                                                                          {ok,
                                                                                           IdSekcija})
                                                               end)
                                    end)
          end,
    mnesia:transaction(Fun).

uredi_sadrzaj(IdSadrzaj, Tip, Obj) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    {db_sadrzaj, IdSadrzaj},
                                    fun(Sadrzaj) ->
                                       NoviSadrzaj =
                                           Sadrzaj#db_sadrzaj{tip = Tip, vrijednost = Obj},
                                       operations:write_secure(object, NoviSadrzaj, {ok, IdSadrzaj})
                                    end)
          end,
    mnesia:transaction(Fun).

obrisi_sadrzaj(Id) ->
    Fun = fun() -> mnesia:delete({db_sekcija, Id}) end,
    mnesia:transaction(Fun).
