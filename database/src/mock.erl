-module(mock).

-include_lib("database/include/records.hrl").

-export([fakultet/0, katedra/0, kolegij/0, sekcija/0, sadrzaj/0, poveznica/0, dokument/0,
         lekcija/0]).

fakultet() ->
    #db_fakultet{id = ?ID,
                 naziv = <<"FOI">>,
                 adresa = adresa()}.

adresa() ->
    #adresa{ulica = <<"Pavlinska ulica">>,
            grad = <<"Varaždin">>,
            postanski_broj = 42000,
            drzava = <<"Hrvatska">>,
            kucni_broj = <<"2">>}.

katedra() ->
    #db_katedra{id = ?ID, naziv = <<"Katedra">>}.

kolegij() ->
    #db_kolegij{id = ?ID,
                naziv = "Matematika 1",
                skraceno = <<"Mat1">>}.

sekcija() ->
    #db_sekcija{id = ?ID,
                naziv = <<"Uvod">>,
                opis = <<"Uvodno poglavlje u kolegij Matematika 1">>}.

sadrzaj() ->
    #db_sadrzaj{id = ?ID,
                naziv = <<"Model praćenja">>,
                tip = poveznica,
                vrijednost = poveznica()}.

poveznica() ->
    #poveznica{referenca = <<"www.google.hr">>,
               vrijeme_kreiranja = calendar:universal_time()}.

dokument() ->
    #dokument{referenca = <<"www.google.hr">>, vrijeme_kreiranja = calendar:universal_time()}.

lekcija() ->
    #lekcija{sadrzaj = <<"Sadržaj lekcije 1">>,
             vrijeme_kreiranja = calendar:universal_time()}.
