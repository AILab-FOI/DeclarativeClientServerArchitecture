-module(quiz).

-include_lib("database/include/records.hrl").

-export([dohvati_kvizove/0, dodaj_kviz/3, dohvati_kviz/1, uredi_kviz/4, obrisi_kviz/1]).

dohvati_kvizove() ->
    Fun = fun(#db_kviz{id = Id,
                       naziv = Naziv,
                       dostupan_do = Do,
                       dostupan_od = Od},
              Acc) ->
             [#{id => Id,
                naziv => Naziv,
                dostupan_od => Od,
                dostupan_do => Do}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_kviz) end),
    Record.

dohvati_kviz(Id) ->
    Fun = fun() ->
             case mnesia:read({db_kviz, Id}) of
                 [#db_kviz{naziv = N,
                           dostupan_od = Od,
                           dostupan_do = Do}] ->
                     #{id => Id,
                       naziv => N,
                       dostupan_od => Od,
                       dostupan_do => Do};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_kviz(Naziv, Do, Od) ->
    Id = ?ID,
    Fun = fun() ->
             case mnesia:write(#db_kviz{id = Id,
                                        naziv = Naziv,
                                        dostupan_do = Do,
                                        dostupan_od = Od})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).

obrisi_kviz(Id) ->
    Fun = fun() -> mnesia:delete({db_kviz, Id}) end,
    mnesia:transaction(Fun).

uredi_kviz(Id, Naziv, Od, Do) ->
    Fun = fun() ->
             case mnesia:write(#db_kviz{id = Id,
                                        naziv = Naziv,
                                        dostupan_od = Od,
                                        dostupan_do = Do})
             of
                 ok -> {ok, Id};
                 _ -> {error, "Transakcija neuspješna"}
             end
          end,
    mnesia:transaction(Fun).
