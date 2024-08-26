use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub struct Response<T> {
    pub data: T,
}

#[derive(Serialize, Deserialize)]
pub struct VecResponse {
    pub data: Vec<Fakultet>,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Tokens {
    refresh_token: String,
    access_token: String,
}
#[wasm_bindgen]
impl Tokens {
    #[wasm_bindgen(getter)]
    pub fn refresh_token(&self) -> String {
        self.refresh_token.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_refresh_token(&mut self, refresh_token: String) {
        self.refresh_token = refresh_token;
    }

    #[wasm_bindgen(getter)]
    pub fn access_token(&self) -> String {
        self.access_token.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_access_token(&mut self, access_token: String) {
        self.access_token = access_token;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Error {
    code: i32,
    message: String,
}

#[wasm_bindgen]
impl Error {
    #[wasm_bindgen(constructor)]
    pub fn new(code: i32, message: &str) -> Self {
        Error {
            code,
            message: message.to_string(),
        }
    }
    #[wasm_bindgen(getter)]
    pub fn code(&self) -> i32 {
        self.code.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_code(&mut self, code: i32) {
        self.code = code;
    }
    #[wasm_bindgen(getter)]
    pub fn message(&self) -> String {
        self.message.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_message(&mut self, message: String) {
        self.message = message;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Korisnik {
    id: i32,
    ime: String,
    prezime: String,
    oib: i32,
    uloga: String,
    email: String,
    opis: String,
    dodatno: Dodatno,
    #[serde(skip_serializing_if = "Option::is_none")]
    kolegiji: Option<Vec<Kolegij>>,
}

#[wasm_bindgen]
impl Korisnik {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }
    #[wasm_bindgen(getter)]
    pub fn ime(&self) -> String {
        self.ime.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_ime(&mut self, ime: String) {
        self.ime = ime;
    }

    #[wasm_bindgen(getter)]
    pub fn prezime(&self) -> String {
        self.prezime.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_prezime(&mut self, prezime: String) {
        self.prezime = prezime;
    }

    #[wasm_bindgen(getter)]
    pub fn oib(&self) -> i32 {
        self.oib.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_oib(&mut self, oib: i32) {
        self.oib = oib;
    }

    #[wasm_bindgen(getter)]
    pub fn uloga(&self) -> String {
        self.uloga.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_uloga(&mut self, uloga: String) {
        self.uloga = uloga;
    }

    #[wasm_bindgen(getter)]
    pub fn email(&self) -> String {
        self.email.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_email(&mut self, email: String) {
        self.email = email;
    }

    #[wasm_bindgen(getter)]
    pub fn opis(&self) -> String {
        self.opis.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_opis(&mut self, opis: String) {
        self.opis = opis;
    }
    #[wasm_bindgen(getter)]
    pub fn dodatno(&self) -> Dodatno {
        self.dodatno.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_dodatno(&mut self, dodatno: Dodatno) {
        self.dodatno = dodatno;
    }

    #[wasm_bindgen(getter)]
    pub fn kolegiji(&self) -> Vec<Kolegij> {
        self.kolegiji.clone().expect("Kolegiji")
    }

    #[wasm_bindgen(setter)]
    pub fn set_kolegiji(&mut self, kolegiji: Vec<Kolegij>) {
        self.kolegiji = Some(kolegiji);
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Dodatno {
    #[serde(skip_serializing_if = "Option::is_none")]
    nadimak: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    kabinet: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    vrijeme_konzultacija: Option<Vec<String>>,
}

#[wasm_bindgen]
impl Dodatno {
    #[wasm_bindgen(getter)]
    pub fn nadimak(&self) -> String {
        self.nadimak.clone().expect("nadimak")
    }

    #[wasm_bindgen(setter)]
    pub fn set_nadimak(&mut self, nadimak: String) {
        self.nadimak = Some(nadimak);
    }

    #[wasm_bindgen(getter)]
    pub fn kabinet(&self) -> String {
        self.kabinet.clone().expect("kabinet")
    }

    #[wasm_bindgen(setter)]
    pub fn set_kabinet(&mut self, kabinet: String) {
        self.kabinet = Some(kabinet);
    }
    #[wasm_bindgen(getter)]
    pub fn vrijeme_konzultacija(&self) -> Vec<String> {
        self.vrijeme_konzultacija
            .clone()
            .expect("vrijeme konzultacija")
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijeme_konzultacija(&mut self, vrijeme_konzultacija: Vec<String>) {
        self.vrijeme_konzultacija = Some(vrijeme_konzultacija);
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Fakultet {
    id: i32,
    naziv: String,
    adresa: Adresa,
    katedre: Vec<Katedra>,
    korisnici: Vec<Korisnik>,
}

#[wasm_bindgen]
impl Fakultet {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }

    #[wasm_bindgen(getter)]
    pub fn naziv(&self) -> String {
        self.naziv.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_naziv(&mut self, naziv: String) {
        self.naziv = naziv;
    }

    #[wasm_bindgen(getter)]
    pub fn adresa(&self) -> Adresa {
        self.adresa.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_adresa(&mut self, adresa: Adresa) {
        self.adresa = adresa;
    }

    #[wasm_bindgen(getter)]
    pub fn katedre(&self) -> Vec<Katedra> {
        self.katedre.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_katedre(&mut self, katedre: Vec<Katedra>) {
        self.katedre = katedre;
    }
    #[wasm_bindgen(getter)]
    pub fn korisnici(&self) -> Vec<Korisnik> {
        self.korisnici.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_korisnici(&mut self, korisnici: Vec<Korisnik>) {
        self.korisnici = korisnici;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Adresa {
    #[serde(skip_serializing_if = "Option::is_none")]
    ulica: Option<String>,
}

#[wasm_bindgen]
impl Adresa {
    #[wasm_bindgen(getter)]
    pub fn ulica(&self) -> String {
        self.ulica.clone().expect("Nije navedeno")
    }

    #[wasm_bindgen(setter)]
    pub fn set_ulica(&mut self, ulica: String) {
        self.ulica = Some(ulica);
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Kolegij {
    id: i32,
    naziv: String,
    skraceno: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    sekcije: Option<Vec<Sekcija>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    studenti: Option<Vec<Korisnik>>,
}

#[wasm_bindgen]
impl Kolegij {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }
    #[wasm_bindgen(getter)]
    pub fn naziv(&self) -> String {
        self.naziv.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_naziv(&mut self, naziv: String) {
        self.naziv = naziv;
    }

    #[wasm_bindgen(getter)]
    pub fn skraceno(&self) -> String {
        self.skraceno.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_skraceno(&mut self, skraceno: String) {
        self.skraceno = skraceno;
    }

    #[wasm_bindgen(getter)]
    pub fn sekcije(&self) -> Vec<Sekcija> {
        self.sekcije.clone().expect("Nema sekcija")
    }

    #[wasm_bindgen(setter)]
    pub fn set_sekcije(&mut self, sekcije: Vec<Sekcija>) {
        self.sekcije = Some(sekcije);
    }

    #[wasm_bindgen(getter)]
    pub fn studenti(&self) -> Vec<Korisnik> {
        self.studenti.clone().expect("Nema sekcija")
    }

    #[wasm_bindgen(setter)]
    pub fn set_studenti(&mut self, studenti: Vec<Korisnik>) {
        self.studenti = Some(studenti);
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct StudentKolegij {
    student: Korisnik,
    kolegij: Kolegij,
    // ocjene: Vec<i32>,
}

#[wasm_bindgen]
impl StudentKolegij {
    #[wasm_bindgen(getter)]
    pub fn student(&self) -> Korisnik {
        self.student.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_student(&mut self, student: Korisnik) {
        self.student = student;
    }
    #[wasm_bindgen(getter)]
    pub fn kolegij(&self) -> Kolegij {
        self.kolegij.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_kolegij(&mut self, kolegij: Kolegij) {
        self.kolegij = kolegij;
    }

    // #[wasm_bindgen(getter)]
    // pub fn ocjene(&self) -> Vec<i32> {
    //     self.ocjene.clone()
    // }
    //
    // #[wasm_bindgen(setter)]
    // pub fn set_ocjene(&mut self, ocjene: Vec<i32>) {
    //     self.ocjene = ocjene;
    // }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct DjelatnikKolegij {
    djelatnik: Korisnik,
    kolegij: Kolegij,
    status: String,
}

#[wasm_bindgen]
impl DjelatnikKolegij {
    #[wasm_bindgen(getter)]
    pub fn djelatnik(&self) -> Korisnik {
        self.djelatnik.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_djelatnik(&mut self, djelatnik: Korisnik) {
        self.djelatnik = djelatnik;
    }
    #[wasm_bindgen(getter)]
    pub fn kolegij(&self) -> Kolegij {
        self.kolegij.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_kolegij(&mut self, kolegij: Kolegij) {
        self.kolegij = kolegij;
    }
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String {
        self.status.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_status(&mut self, status: String) {
        self.status = status;
    }

    // #[wasm_bindgen(getter)]
    // pub fn ocjene(&self) -> Vec<i32> {
    //     self.ocjene.clone()
    // }
    //
    // #[wasm_bindgen(setter)]
    // pub fn set_ocjene(&mut self, ocjene: Vec<i32>) {
    //     self.ocjene = ocjene;
    // }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Katedra {
    id: i32,
    naziv: String,
}

#[wasm_bindgen]
impl Katedra {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }
    #[wasm_bindgen(getter)]
    pub fn naziv(&self) -> String {
        self.naziv.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_naziv(&mut self, naziv: String) {
        self.naziv = naziv;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Sekcija {
    id: i32,
    naziv: String,
    opis: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    sadrzaj: Option<Vec<Sadrzaj>>,
}

#[wasm_bindgen]
impl Sekcija {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }
    #[wasm_bindgen(getter)]
    pub fn naziv(&self) -> String {
        self.naziv.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_naziv(&mut self, naziv: String) {
        self.naziv = naziv;
    }

    #[wasm_bindgen(getter)]
    pub fn opis(&self) -> String {
        self.opis.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_opis(&mut self, opis: String) {
        self.opis = opis;
    }
    #[wasm_bindgen(getter)]
    pub fn sadrzaj(&self) -> Vec<Sadrzaj> {
        self.sadrzaj.clone().expect("sadrzaj")
    }

    #[wasm_bindgen(setter)]
    pub fn set_sadrzaj(&mut self, sadrzaj: Vec<Sadrzaj>) {
        self.sadrzaj = Some(sadrzaj);
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Sadrzaj {
    id: i32,
    naziv: String,
    tip: String,
    vrijednost: Poveznica,
}

#[wasm_bindgen]
impl Sadrzaj {
    #[wasm_bindgen(getter)]
    pub fn id(&self) -> i32 {
        self.id.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_id(&mut self, id: i32) {
        self.id = id;
    }
    #[wasm_bindgen(getter)]
    pub fn naziv(&self) -> String {
        self.naziv.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_naziv(&mut self, naziv: String) {
        self.naziv = naziv;
    }

    #[wasm_bindgen(getter)]
    pub fn tip(&self) -> String {
        self.tip.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_tip(&mut self, tip: String) {
        self.tip = tip;
    }

    #[wasm_bindgen(getter)]
    pub fn vrijednost(&self) -> Poveznica {
        self.vrijednost.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijednost(&mut self, vrijednost: Poveznica) {
        self.vrijednost = vrijednost;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Poveznica {
    referenca: String,
    vrijeme_kreiranja: u64,
}

#[wasm_bindgen]
impl Poveznica {
    #[wasm_bindgen(getter)]
    pub fn referenca(&self) -> String {
        self.referenca.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_referenca(&mut self, referenca: String) {
        self.referenca = referenca;
    }

    #[wasm_bindgen(getter)]
    pub fn vrijeme_kreiranja(&self) -> u64 {
        self.vrijeme_kreiranja.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijeme_kreiranja(&mut self, vrijeme_kreiranja: u64) {
        self.vrijeme_kreiranja = vrijeme_kreiranja;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone)]
pub struct Dokument {
    referenca: String,
    vrijeme_kreiranja: String,
}

#[wasm_bindgen]
impl Dokument {
    #[wasm_bindgen(getter)]
    pub fn referenca(&self) -> String {
        self.referenca.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_referenca(&mut self, referenca: String) {
        self.referenca = referenca;
    }

    #[wasm_bindgen(getter)]
    pub fn vrijeme_kreiranja(&self) -> String {
        self.vrijeme_kreiranja.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijeme_kreiranja(&mut self, vrijeme_kreiranja: String) {
        self.vrijeme_kreiranja = vrijeme_kreiranja;
    }
}
