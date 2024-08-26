-module(sadrzaj).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dodaj/3, dohvati/1, dohvati/2, dohvati/0, obrisi/1, uredi/4]).

dohvati() ->
    Fun = fun(Sadrzaj, Acc) -> [ucitaj(core, Sadrzaj) | Acc] end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_sadrzaj) end).

dohvati(Id) ->
    Fun = fun() ->
             case mnesia:read({db_sadrzaj, Id}) of
                 [Sadrzaj] -> ucitaj(core, Sadrzaj);
                 [] -> {error, "Sadrzaj ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dohvati(Type, Id) ->
    Fun = fun() ->
             case mnesia:read({db_sadrzaj, Id}) of
                 [Sadrzaj] -> ucitaj(Type, Sadrzaj);
                 [] -> {error, "Sadrzaj ne postoji"}
             end
          end,
    mnesia:transaction(Fun).

dodaj(Naziv, Tip, Vrijednost) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_sadrzaj{id = Id,
                                           naziv = Naziv,
                                           tip = parse_tip(to_atom, Tip),
                                           vrijednost =
                                               generate_vrijednost(parse_tip(to_atom, Tip),
                                                                   Vrijednost)})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

generate_vrijednost(poveznica, #{<<"referenca">> := Referenca}) ->
    #poveznica{referenca = Referenca, vrijeme_kreiranja = calendar:universal_time()};
generate_vrijednost(dokument, #{<<"referenca">> := Referenca}) ->
    #dokument{referenca = Referenca, vrijeme_kreiranja = calendar:universal_time()};
generate_vrijednost(lekcija, #{<<"sadrzaj">> := Sadrzaj}) ->
    #lekcija{sadrzaj = Sadrzaj, vrijeme_kreiranja = calendar:universal_time()};
generate_vrijednost(kviz, Vrijednost) ->
    Vrijednost.

parse_tip(to_string, Status) ->
    case Status of
        poveznica ->
            <<"poveznica">>;
        lekcija ->
            <<"lekcija">>;
        kviz ->
            <<"kviz">>;
        dokument ->
            <<"dokument">>
    end;
parse_tip(to_atom, Status) ->
    case Status of
        <<"poveznica">> ->
            poveznica;
        <<"lekcija">> ->
            lekcija;
        <<"kviz">> ->
            kviz;
        <<"dokument">> ->
            dokument
    end.

obrisi(Id) ->
    Fun = fun() ->
             sekcija_sadrzaj:obrisi_sekciju(Id),
             mnesia:delete({db_sadrzaj, Id})
          end,
    mnesia:transaction(Fun).

uredi(Id, Naziv, Tip, Vrijednost) ->
    Fun = fun() ->
             case mnesia:write(#db_sadrzaj{id = Id,
                                           naziv = Naziv,
                                           tip = Tip,
                                           vrijednost = Vrijednost})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

ucitaj(core, R) ->
    transform_sadrzaj(R).

transform_sadrzaj(#db_sadrzaj{id = Id,
                              naziv = Naziv,
                              tip = Tip,
                              vrijednost = Vrijednost}) ->
    io:format("DSA"),
    NovaVrijednost = transform_vrijednost(Tip, Vrijednost),
    #{id => Id,
      naziv => Naziv,
      tip => parse_tip(to_string, Tip),
      vrijednost => NovaVrijednost}.

transform_vrijednost(poveznica,
                     #poveznica{referenca = Referenca, vrijeme_kreiranja = VrijemeKreiranja}) ->
    #{referenca => Referenca,
      vrijeme_kreiranja => calendar:datetime_to_gregorian_seconds(VrijemeKreiranja)};
transform_vrijednost(dokument,
                     #dokument{referenca = Referenca, vrijeme_kreiranja = VrijemeKreiranja}) ->
    #{referenca => Referenca,
      vrijeme_kreiranja => calendar:datetime_to_gregorian_seconds(VrijemeKreiranja)};
transform_vrijednost(lekcija,
                     #lekcija{sadrzaj = Sadrzaj, vrijeme_kreiranja = VrijemeKreiranja}) ->
    #{sadrzaj => Sadrzaj,
      vrijeme_kreiranja => calendar:datetime_to_gregorian_seconds(VrijemeKreiranja)}.
