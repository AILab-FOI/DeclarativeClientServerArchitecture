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
-type kolegij() ::
  #{id := id(),
    naziv := binary(),
    skraceno := binary(),
    sudionici := [korisnik_ref() | djelatnik_konfiguracija_ref()],
    sekcije := [sekcija()]}.
-type sekcija() ::
  #{id := id(),
    naziv := binary(),
    opis := binary(),
    sadrzaj := [sadrzaj()]}.
-type dokument() :: #{referenca := binary(), vrijeme_kreiranja := datum_vrijeme()}.
-type lekcija() :: #{sadrzaj := binary(), vrijeme_kreiranja := datum_vrijeme()}.
-type poveznica() :: #{referenca := binary(), vrijeme_kreiranja := datum_vrijeme()}.
-type kviz_konfiguracija() ::
  #{student := id(),
    kviz := id(),
    bodovi := float()}.
-type sadrzaj() ::
  #{id := id(),
    naziv := binary(),
    tip := dokument,
    vrijednost := dokument()} |
  #{id := id(),
    naziv := binary(),
    tip := lekcija,
    vrijednost := lekcija()} |
  #{id := id(),
    naziv := binary(),
    tip := poveznica,
    vrijednost := poveznica()} |
  #{id := id(),
    naziv := binary(),
    tip := kviz,
    vrijednost := kviz_ref()}.
-type sekcija_ref() :: id().
-type korisnik_ref() :: id().
-type sadrzaj_ref() :: id().
-type kviz_ref() :: id().
-type kolegij_ref() :: id().
-type djelatnik_konfiguracija_ref() :: id().

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
         kolegiji = [] :: [id()],
         dodatno :: student() | djelatnik()}).
-record(student, {ocjene = [] :: [id()]}).
-record(djelatnik, {kabinet :: binary(), vrijeme_konzultacija = [] :: [datum_vrijeme()]}).
-record(db_djelatnik_konfiguracija,
        {id_kolegij :: kolegij_ref(),
         id_djelatnik :: korisnik_ref(),
         status :: status_djelatnika()}).

% NOTE: Kolegij

-record(db_kolegij,
        {id :: id(),
         naziv :: binary(),
         skraceno :: binary(),
         sudionici = [] :: [korisnik_ref() | djelatnik_konfiguracija_ref()],
         sekcije = [] :: [sekcija_ref()]}).
-record(db_sekcija,
        {id :: id(), naziv :: binary(), opis :: binary(), sadrzaj = [] :: [sadrzaj_ref()]}).
% NOTE: Student nije bitan
-record(db_sadrzaj,
        {id :: id(),
         naziv :: binary(),
         tip :: dokument | lekcija | poveznica | kviz,
         redoslijed :: id(),
         vrijednost :: dokument() | lekcija() | poveznica() | kviz_ref()}).
-record(dokument, {referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(lekcija, {sadrzaj :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(poveznica, {referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
% NOTE: Ocjena po studentu
-record(db_kviz_konfiguracija, {student :: id(), kviz :: id(), bodovi :: float()}).
-record(db_kviz,
        {id :: id(),
         naziv :: binary(),
         dostupan_od :: datum_vrijeme(),
         dostupan_do :: datum_vrijeme(),
         id_pitanja = [] :: [id()]}).
-record(db_pitanje, {id :: id(), naslov :: binary(), odgovori = [] :: [binary()]}).
