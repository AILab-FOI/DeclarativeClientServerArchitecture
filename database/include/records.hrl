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
% NOTE: Korisnik
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
-record(student, {kolegiji = [] :: [id()]}).
-record(djelatnik, {kabinet :: binary(), vrijeme_konzultacija = [] :: [datum_vrijeme()]}).
-record(db_djelatnik_konfiguracija,
        {id :: id(), id_djelatnik :: id(), status :: status_djelatnika()}).

-type korisnik_ref() :: id().
% NOTE: Kolegij
-type kolegij() ::
  #{id := id(),
    naziv := binary(),
    skraceno := binary(),
    sudionici := [korisnik_ref()],
    sekcije := [sekcija()]}.

-record(db_kolegij,
        {id :: id(),
         naziv :: binary(),
         skraceno :: binary(),
         sudionici :: [id()],
         sekcije = [] :: [sekcija_ref()]}).

-type sekcija() ::
  #{id := id(),
    naziv := binary(),
    opis := binary(),
    sadrzaj := [sadrzaj()]}.
-type sekcija_ref() :: id().
-type sadrzaj_ref() :: id().
-type kviz_ref() :: id().
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
  #{id := id(),
    tip := dokument,
    vrijednost := dokument()} |
  #{id := id(),
    tip := lekcija,
    vrijednost := lekcija()} |
  #{id := id(),
    tip := poveznica,
    vrijednost := poveznica()} |
  #{id := id(),
    tip := kviz,
    vrijednost := kviz_ref()}.

-record(db_sekcija,
        {id :: id(), naziv :: binary(), opis :: binary(), sadrzaj = [] :: [sadrzaj_ref()]}).
% NOTE: Student nije bitan
-record(db_sadrzaj,
        {id :: id(),
         tip :: dokument | lekcija | poveznica | kviz,
         vrijednost :: dokument() | lekcija() | poveznica() | kviz_ref()}).
-record(dokument,
        {id :: id(),
         naziv :: binary(),
         referenca :: binary(),
         vrijeme_kreiranja :: datum_vrijeme()}).
-record(lekcija,
        {id :: id(),
         naziv :: binary(),
         sadrzaj :: binary(),
         vrijeme_kreiranja :: datum_vrijeme()}).
-record(poveznica,
        {id :: id(),
         naziv :: binary(),
         referenca :: binary(),
         vrijeme_kreiranja :: datum_vrijeme()}).
% NOTE: Ocjena po studentu
-record(db_kviz_konfiguracija, {student :: id(), kviz :: id(), bodovi :: float()}).
-record(db_kviz,
        {id :: id(),
         naziv :: binary(),
         dostupan_od :: datum_vrijeme(),
         dostupan_do :: datum_vrijeme(),
         id_pitanja = [] :: [id()]}).
-record(db_pitanje, {id :: id(), naslov :: binary(), odgovori = [] :: [binary()]}).
