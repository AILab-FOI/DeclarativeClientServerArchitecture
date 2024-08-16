-define(ID, erlang:unique_integer([positive])).

-type id() :: pos_integer().
-type adresa() ::
  #{grad := binary(),
    ulica := binary(),
    postanski_broj := number(),
    drzava := binary(),
    kucni_broj := binary()}.
-type status_djelatnika() :: nositelj | asistent.
-type datum_vrijeme() ::
  {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type lozinka() :: {binary(), binary()}.
-type student() :: #{id_kolegij := [id()]}.
-type djelatnik() :: #{vrijeme_konzultacija := [datum_vrijeme()], kabinet := binary()}.

-record(db_fakultet,
        {id :: id(),
         naziv :: binary(),
         adresa :: adresa(),
         id_katedre = [] :: [id()],
         id_djelatnici = [] :: [id()],
         id_student = [] :: [id()]}).
-record(db_katedra,
        {id :: id(), naziv :: binary(), id_djelatnik = [] :: [id()], id_kolegij = [] :: [id()]}).
-record(db_korisnik,
        {id :: id(),
         ime :: binary(),
         prezime :: binary(),
         oib :: integer(),
         lozinka :: lozinka(),
         uloga :: student | djelatnik,
         email :: binary(),
         opis :: binary(),
         dodatno :: student() | djelatnik()}).
-record(student, {id_kolegij = [] :: [id()]}).
-record(djelatnik, {kabinet :: binary(), vrijeme_konzultacija = [] :: [datum_vrijeme()]}).
-record(db_djelatnik_konfiguracija,
        {id :: id(), id_djelatnik :: id(), status :: status_djelatnika()}).
-record(db_kolegij,
        {id :: id(),
         naziv :: binary(),
         skraceno :: binary(),
         id_sudionik :: [id()],
         sekcija = [] :: [sekcija()]}).

-type sekcija() ::
  #{naziv := binary(),
    opis := binary(),
    sadrzaj := [id()]}.
-type dokument() ::
  #{naziv := binary(),
    referenca := binary(),
    vrijeme_kreiranja := datum_vrijeme()}.
-type lekcija() ::
  #{naziv := binary(),
    sadrzaj := binary(),
    vrijeme_kreiranja := datum_vrijeme()}.
-type poveznica() ::
  #{naziv := binary(),
    referenca := binary(),
    vrijeme_kreiranja := datum_vrijeme()}.
-type kviz_konfiguracija() ::
  #{student := id(),
    kviz := id(),
    bodovi := float()}.
-type sadrzaj() ::
  #{tip := dokument, vrijednost := dokument()} |
  #{tip := lekcija, vrijednost := lekcija()} |
  #{tip := poveznica, vrijednost := poveznica()} |
  #{tip := kviz, vrijednost := kviz_konfiguracija()}.

-record(sekcija, {naziv :: binary(), opis :: binary(), sadrzaj = [] :: [sadrzaj()]}).
% NOTE: Student nije bitan
-record(sadrzaj,
        {tip :: dokument | lekcija | poveznica | kviz,
         vrijednost :: dokument() | lekcija() | poveznica() | kviz_konfiguracija()}).
-record(dokument,
        {naziv :: binary(), referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(lekcija,
        {naziv :: binary(), sadrzaj :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(poveznica,
        {naziv :: binary(), referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
% NOTE: Ocjena po studentu
-record(kviz_konfiguracija, {student :: id(), kviz :: id(), bodovi :: float()}).
-record(db_kviz,
        {id :: id(),
         naziv :: binary(),
         dostupan_od :: datum_vrijeme(),
         dostupan_do :: datum_vrijeme(),
         id_pitanja = [] :: [id()]}).
-record(db_pitanje, {id :: id(), naslov :: binary(), odgovori = [] :: [binary()]}).
