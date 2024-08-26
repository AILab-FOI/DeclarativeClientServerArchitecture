-module(djelatnik_kolegij).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_djelatnike/1, dohvati_kolegije/1, dodaj_djelatnika_na_kolegij/3,
         dohvati_djelatnika_na_kolegiju/2, ucitaj_djelatnike/1, obrisi_djelatnika/1,
         obrisi_djelatnika_na_kolegiju/2, obrisi_kolegij/1, ucitaj_kolegije/1]).

dodaj_djelatnika_na_kolegij(IdDjelatnik, IdKolegij, Status) ->
    Fun = fun() ->
             case mnesia:write(#db_djelatnik_kolegij{id = {IdDjelatnik, IdKolegij},
                                                     status = parse_status(to_atom, Status)})
             of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_djelatnike(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_djelatnik_kolegij{id = {Djelatnik, Kolegij}})
                               when Kolegij =:= IdKolegij ->
                               Djelatnik
                            end),
             Djelatnici = mnesia:select(db_djelatnik_kolegij, Match),
             lists:map(fun(Djelatnik) ->
                          {atomic, Result} = korisnik:dohvati_korisnika(core, Djelatnik),
                          Result
                       end,
                       Djelatnici)
          end,
    mnesia:transaction(Fun).

dohvati_kolegije(IdDjelatnik) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_djelatnik_kolegij{id = {Djelatnik, Kolegij}})
                               when Djelatnik =:= IdDjelatnik ->
                               Kolegij
                            end),
             Kolegiji = mnesia:select(db_djelatnik_kolegij, Match),
             lists:map(fun(Kolegij) ->
                          {atomic, Result} = kolegij:dohvati(core, Kolegij),
                          Result
                       end,
                       Kolegiji)
          end,
    mnesia:transaction(Fun).

dohvati_djelatnika_na_kolegiju(IdDjelatnik, IdKolegij) ->
    Fun = fun() ->
             case mnesia:read({db_djelatnik_kolegij, {IdDjelatnik, IdKolegij}}) of
                 [#db_djelatnik_kolegij{status = Status}] ->
                     {atomic, Student} = korisnik:dohvati_korisnika(core, IdDjelatnik),
                     {atomic, Kolegij} = kolegij:dohvati(full, IdKolegij),
                     #{djelatnik => Student,
                       kolegij => Kolegij,
                       status => parse_status(to_string, Status)};
                 _ -> {error, 'Djelatnik nije na kolegiju'}
             end
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_djelatnik_kolegij{id = {Djelatnik, Kolegij}})
                               when Kolegij =:= IdKolegij ->
                               {Djelatnik, Kolegij}
                            end),

             Delete = mnesia:select(db_djelatnik_kolegij, Match),
             lists:foreach(fun(Id) -> mnesia:delete({db_katedra_kolegij, Id}) end, Delete)
          end,
    mnesia:transaction(Fun).

obrisi_djelatnika(IdDjelatnik) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_djelatnik_kolegij{id = {Djelatnik, Kolegij}})
                               when Djelatnik =:= IdDjelatnik ->
                               {Djelatnik, Kolegij}
                            end),

             Delete = mnesia:select(db_djelatnik_kolegij, Match),
             lists:foreach(fun(Id) -> mnesia:delete({db_katedra_kolegij, Id}) end, Delete)
          end,
    mnesia:transaction(Fun).

obrisi_djelatnika_na_kolegiju(IdDjelatnik, IdKolegij) ->
    Fun = fun() -> mnesia:delete({db_djelatnik_kolegij, {IdDjelatnik, IdKolegij}}) end,
    mnesia:transaction(Fun).

ucitaj_djelatnike(#{id := Id} = M) ->
    {atomic, Djelatnici} = dohvati_djelatnike(Id),
    M#{djelatnici => Djelatnici}.

ucitaj_kolegije(#{id := Id} = M) ->
    {atomic, Kolegiji} = dohvati_kolegije(Id),
    M#{kolegiji => Kolegiji}.

parse_status(to_string, Status) ->
    case Status of
        nositelj ->
            <<"Nositelj">>;
        asistent ->
            <<"Asistent">>
    end;
parse_status(to_atom, Status) ->
    case Status of
        <<"Nositelj">> ->
            nositelj;
        <<"Asistent">> ->
            asistent
    end.
