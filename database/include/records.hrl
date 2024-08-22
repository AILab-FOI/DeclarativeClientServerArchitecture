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
-type odgovor() ::
  #{id := id(),
    vrijednost := binary(),
    tocan := boolean()}.
-type sekcija_ref() :: id().
-type korisnik_ref() :: id().
-type sadrzaj_ref() :: id().
-type kviz_ref() :: id().
-type fakultet_ref() :: id().
-type katedra_ref() :: id().
-type kolegij_ref() :: id().
-type pitanje_ref() :: id().
-type djelatnik_konfiguracija_ref() :: id().

-record(db_fakultet, {id :: id(), naziv :: binary(), adresa :: adresa()}).
-record(db_fakultet_korisnik,
        {id_korisnik :: korisnik_ref(), id_fakultet :: fakultet_ref()}).
-record(db_fakultet_katedra,
        {id_katedra :: katedra_ref(), id_fakultet :: fakultet_ref()}).
-record(db_katedra, {id :: id(), naziv :: binary()}).
-record(db_katedra_djelatnik,
        {id_katedra :: katedra_ref(), id_djelatnik :: korisnik_ref()}).
-record(db_katedra_kolegij, {id_kolegij :: kolegij_ref(), id_katedra :: katedra_ref()}).
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
-record(student, {nadimak :: binary()}).
-record(djelatnik, {kabinet :: binary(), vrijeme_konzultacija = [] :: [datum_vrijeme()]}).
-record(db_djelatnik_kolegij,
        {id :: {korisnik_ref(), kolegij_ref()}, status :: status_djelatnika()}).
-record(db_student_kolegij, {id :: {korisnik_ref(), kolegij_ref()}, ocjene :: [{}]}).
-record(db_kolegij, {id :: id(), naziv :: binary(), skraceno :: binary()}).
-record(db_kolegij_sekcija, {id_kolegij :: kolegij_ref(), id_sekcija :: sekcija_ref()}).
-record(db_sekcija, {id :: id(), naziv :: binary(), opis :: binary()}).
-record(db_sekcija_sadrzaj, {id_sadrzaj :: sadrzaj_ref(), id_sekcija :: sekcija_ref()}).
-record(db_sadrzaj,
        {id :: id(),
         naziv :: binary(),
         tip :: dokument | lekcija | poveznica | kviz,
         vrijednost :: dokument() | lekcija() | poveznica() | kviz_ref()}).
-record(dokument, {referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(lekcija, {sadrzaj :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(poveznica, {referenca :: binary(), vrijeme_kreiranja :: datum_vrijeme()}).
-record(db_kviz,
        {id :: id(),
         naziv :: binary(),
         dostupan_od :: datum_vrijeme(),
         dostupan_do :: datum_vrijeme()}).
-record(db_kviz_student,
        {id_kviz :: kviz_ref(),
         id_student :: korisnik_ref(),
         pitanje_odgovor :: [{pitanje_ref(), odgovor()}],
         vrijeme_rjesavanja :: datum_vrijeme(),
         bodovi :: float()}).
-record(db_pitanje, {id :: id(), naziv :: binary(), odgovori = [] :: [odgovor()]}).
-record(odgovor, {id :: id(), vrijednost :: binary(), tocan :: boolean()}).
