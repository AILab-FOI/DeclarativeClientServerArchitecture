-module(course).

-include_lib("database/include/records.hrl").

-export([dohvati_kolegij/1, dodaj_kolegij/2, dohvati_kolegije/0, obrisi_kolegij/1,
         dodaj_studenta_na_kolegij/2, obrisi_studenta_sa_kolegija/2, dodaj_sekciju/3,
         obrisi_sekciju/1, uredi_sekciju/4, dodaj_sadrzaj/5, obrisi_sadrzaj/1, uredi_sadrzaj/3]).

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
                     NoveSekcije =
                         lists:map(fun(Sekcija) ->
                                      case mnesia:read({db_sekcija, Sekcija}) of
                                          [#db_sekcija{id = IdS,
                                                       naziv = NazivS,
                                                       opis = Opis,
                                                       sadrzaj = Sadrzaj}] ->
                                              NoviSadrzaj =
                                                  lists:map(fun(S) ->
                                                               case mnesia:read({db_sadrzaj, S}) of
                                                                   [#db_sadrzaj{id = IdSadrzaj,
                                                                                tip = Tip,
                                                                                vrijednost =
                                                                                    Vrijednost}] ->
                                                                       #{id => IdSadrzaj,
                                                                         tip => Tip,
                                                                         vrijednost => Vrijednost};
                                                                   [] -> undefined
                                                               end
                                                            end,
                                                            Sadrzaj),
                                              #{id => IdS,
                                                naziv => NazivS,
                                                opis => Opis,
                                                sadrzaj => NoviSadrzaj};
                                          [] -> undefined
                                      end
                                   end,
                                   Sekcije),

                     #{id => Id,
                       naziv => Naziv,
                       skraceno => Skraceno,
                       sudionici => Sudionici,
                       sekcije => NoveSekcije};
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
             io:format("~p~n", [Sudionici]),
             NoviSudionici =
                 lists:map(fun(Sudionik) ->
                              case mnesia:read({db_korisnik, Sudionik}) of
                                  [#db_korisnik{id = IdKorisnik,
                                                ime = Ime,
                                                prezime = Prezime}] ->
                                      #{id => IdKorisnik,
                                        ime => Ime,
                                        prezime => Prezime};
                                  [] -> undefined
                              end
                           end,
                           Sudionici),
             NoveSekcije =
                 lists:map(fun(Sekcija) ->
                              case mnesia:read({db_sekcija, Sekcija}) of
                                  [#db_sekcija{id = IdS,
                                               naziv = NazivS,
                                               opis = Opis,
                                               sadrzaj = Sadrzaj}] ->
                                      NoviSadrzaj =
                                          lists:map(fun(S) ->
                                                       case mnesia:read({db_sadrzaj, S}) of
                                                           [#db_sadrzaj{id = IdSadrzaj,
                                                                        tip = Tip,
                                                                        naziv = NazivSadr,
                                                                        redoslijed = Redoslijed,
                                                                        vrijednost = Vrijednost}] ->
                                                               io:format("~p~n", [Sadrzaj]),
                                                               #{id => IdSadrzaj,
                                                                 tip => Tip,
                                                                 naziv => NazivSadr,
                                                                 redoslijed => Redoslijed,
                                                                 vrijednost => Vrijednost};
                                                           [] -> undefined
                                                       end
                                                    end,
                                                    Sadrzaj),

                                      #{id => IdS,
                                        naziv => NazivS,
                                        opis => Opis,
                                        sadrzaj => NoviSadrzaj};
                                  [] -> undefined
                              end
                           end,
                           Sekcije),

             [#{id => Id,
                naziv => Naziv,
                skraceno => Skraceno,
                sudionici => NoviSudionici,
                sekcije => NoveSekcije}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kolegij) end),
    Record.

dodaj_studenta_na_kolegij(IdStudent, IdKolegij) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_kolegij,
                                    IdKolegij,
                                    fun(Kolegij) ->
                                       Sudionici = Kolegij#db_kolegij.sudionici ++ [IdStudent],
                                       operations:write_secure(function,
                                                               Kolegij#db_kolegij{sudionici =
                                                                                      Sudionici},
                                                               fun() ->
                                                                  operations:read_secure(function,
                                                                                         db_korisnik,
                                                                                         IdStudent,
                                                                                         fun(Korisnik) ->
                                                                                            Kolegiji =
                                                                                                Korisnik#db_korisnik.kolegiji
                                                                                                ++ [IdKolegij],
                                                                                            operations:write_secure(object,
                                                                                                                    Korisnik#db_korisnik{kolegiji
                                                                                                                                             =
                                                                                                                                             Kolegiji},
                                                                                                                    {ok,
                                                                                                                     done})
                                                                                         end)
                                                               end)
                                    end)
          end,
    mnesia:transaction(Fun).

obrisi_studenta_sa_kolegija(IdStudent, IdKolegij) ->
    Fun = fun() ->
             operations:read_secure(function,
                                    db_kolegij,
                                    IdKolegij,
                                    fun(Kolegij) ->
                                       Sudionici =
                                           [Sudionik
                                            || Sudionik <- Kolegij#db_kolegij.sudionici,
                                               Sudionik /= IdStudent],
                                       operations:write_secure(function,
                                                               Kolegij#db_kolegij{sudionici =
                                                                                      Sudionici},
                                                               fun() ->
                                                                  operations:read_secure(function,
                                                                                         db_korisnik,
                                                                                         IdStudent,
                                                                                         fun(Korisnik) ->
                                                                                            Kolegiji =
                                                                                                [Kolegij2
                                                                                                 || Kolegij2
                                                                                                        <- Korisnik#db_korisnik.kolegiji,
                                                                                                    Kolegij2
                                                                                                    /= IdKolegij],
                                                                                            operations:write_secure(object,
                                                                                                                    Korisnik#db_korisnik{kolegiji
                                                                                                                                             =
                                                                                                                                             Kolegiji},
                                                                                                                    {ok,
                                                                                                                     done})
                                                                                         end)
                                                               end)
                                    end)
          end,
    mnesia:transaction(Fun).

dodaj_sekciju(IdKolegij, Naziv, Opis) ->
    Id = ?ID,
    Fun = fun() ->
             operations:read_secure(function,
                                    db_kolegij,
                                    IdKolegij,
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
    Fun = fun() ->
             mnesia:delete({db_sekcija, Id}),

             Kolegiji = mnesia:foldl(fun(Kolegij, Acc) -> [Kolegij | Acc] end, [], db_kolegij),
             lists:foreach(fun(Kolegij) ->
                              NoveSekcije =
                                  [Sekcija || Sekcija <- Kolegij#db_kolegij.sekcije, Sekcija /= Id],
                              mnesia:write(Kolegij#db_kolegij{sekcije = NoveSekcije})
                           end,
                           Kolegiji),
             ok
          end,
    mnesia:transaction(Fun).

dodaj_sadrzaj(IdSekcija, Naziv, Redoslijed, Tip, Obj) ->
    Id = ?ID,
    Fun = fun() ->
             operations:read_secure(function,
                                    db_sekcija,
                                    IdSekcija,
                                    fun(Sekcija) ->
                                       Sadrzaj =
                                           #db_sadrzaj{id = Id,
                                                       naziv = Naziv,
                                                       tip = Tip,
                                                       redoslijed = Redoslijed,
                                                       vrijednost = Obj},
                                       operations:write_secure(function,
                                                               Sadrzaj,
                                                               fun() ->
                                                                  NovaSekcija =
                                                                      Sekcija#db_sekcija{sadrzaj =
                                                                                             [Sadrzaj#db_sadrzaj.id
                                                                                              | Sekcija#db_sekcija.sadrzaj]},
                                                                  operations:write_secure(object,
                                                                                          NovaSekcija,
                                                                                          {ok, Id})
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
    Fun = fun() ->
             mnesia:delete({db_sadrzaj, Id}),

             Sekcije = mnesia:foldl(fun(Sekcija, Acc) -> [Sekcija | Acc] end, [], db_sekcija),
             lists:foreach(fun(Sekcija) ->
                              NoviSadrzaj =
                                  [Sadrzaj || Sadrzaj <- Sekcija#db_sekcija.sadrzaj, Sadrzaj /= Id],
                              mnesia:write(Sekcija#db_sekcija{sadrzaj = NoviSadrzaj})
                           end,
                           Sekcije),
             ok
          end,
    mnesia:transaction(Fun).
