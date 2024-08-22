-module(student_kolegij).

-include_lib("database/include/records.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([dohvati_studente/1, dohvati_kolegije/1, dodaj_studenta_na_kolegij/2,
         dohvati_studenta_na_kolegiju/2, ucitaj_studente/1, ucitaj_kolegije/1, obrisi_studenta/1,
         obrisi_studenta_na_kolegiju/2, obrisi_kolegij/1]).

dodaj_studenta_na_kolegij(IdStudent, IdKolegij) ->
    Fun = fun() ->
             case mnesia:write(#db_student_kolegij{id = {IdStudent, IdKolegij}, ocjene = []}) of
                 ok -> true;
                 _ -> false
             end
          end,
    mnesia:transaction(Fun).

dohvati_studente(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}})
                               when Kolegij =:= IdKolegij ->
                               Student
                            end),
             Studenti = mnesia:select(db_student_kolegij, Match),
             io:format("~p~n", [Studenti]),
             lists:map(fun(Student) ->
                          {atomic, Result} = korisnik:dohvati_korisnika(core, Student),
                          io:format("~p~n", [Result]),
                          Result
                       end,
                       Studenti)
          end,
    mnesia:transaction(Fun).

dohvati_kolegije(IdStudent) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}})
                               when Student =:= IdStudent ->
                               Kolegij
                            end),
             Kolegiji = mnesia:select(db_student_kolegij, Match),
             lists:map(fun(Kolegij) ->
                          {atomic, Result} = kolegij:dohvati(core, Kolegij),
                          Result
                       end,
                       Kolegiji)
          end,
    mnesia:transaction(Fun).

dohvati_studenta_na_kolegiju(IdStudent, IdKolegij) ->
    Fun = fun() ->
             case mnesia:read({db_student_kolegij, {IdStudent, IdKolegij}}) of
                 [#db_student_kolegij{id = {StudentId, KolegijId}, ocjene = Ocjene}] ->
                     {atomic, Student} = user:dohvati_korisnika(StudentId),
                     {atomic, Kolegij} = kolegij:dohvati(sekcije, KolegijId),
                     #{student => Student,
                       kolegij => Kolegij,
                       ocjene => Ocjene};
                 _ -> {error, 'Student nije na kolegiju'}
             end
          end,
    mnesia:transaction(Fun).

obrisi_kolegij(IdKolegij) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}})
                               when Kolegij =:= IdKolegij ->
                               {Student, Kolegij}
                            end),

             Delete = mnesia:select(db_student_kolegij, Match),
             lists:foreach(fun(Id) -> mnesia:delete({db_student_kolegij, Id}) end, Delete)
          end,
    mnesia:transaction(Fun).

obrisi_studenta(IdStudent) ->
    Fun = fun() ->
             Match =
                 ets:fun2ms(fun(#db_student_kolegij{id = {Student, Kolegij}})
                               when Student =:= IdStudent ->
                               {Student, Kolegij}
                            end),

             Delete = mnesia:select(db_student_kolegij, Match),
             lists:foreach(fun(Id) -> mnesia:delete({db_student_kolegij, Id}) end, Delete)
          end,
    mnesia:transaction(Fun).

obrisi_studenta_na_kolegiju(IdStudent, IdKolegij) ->
    Fun = fun() -> mnesia:delete({db_student_kolegij, {IdStudent, IdKolegij}}) end,
    mnesia:transaction(Fun).

ucitaj_studente(#{id := Id} = M) ->
    {atomic, Studenti} = dohvati_studente(Id),
    M#{studenti => Studenti}.

ucitaj_kolegije(#{id := Id} = M) ->
    {atomic, Kolegiji} = dohvati_kolegije(Id),
    M#{kolegiji => Kolegiji}.
