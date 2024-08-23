use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub struct Response<T> {
    pub data: T,
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
#[derive(Serialize, Deserialize)]
pub struct Korisnik {
    id: i32,
    ime: String,
    prezime: String,
    oib: i32,
    uloga: String,
    email: String,
    opis: String,
    dodatno: Student,
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
    pub fn dodatno(&self) -> Student {
        self.dodatno.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_dodatno(&mut self, dodatno: Student) {
        self.dodatno = dodatno;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Fakultet {
    naziv: String,
    adresa: Adresa,
}

#[wasm_bindgen]
impl Fakultet {
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
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Adresa {
    ulica: String,
}

#[wasm_bindgen]
impl Adresa {
    #[wasm_bindgen(getter)]
    pub fn ulica(&self) -> String {
        self.ulica.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_ulica(&mut self, ulica: String) {
        self.ulica = ulica;
    }

    #[wasm_bindgen]
    pub fn clone(&self) -> Adresa {
        Adresa {
            ulica: self.ulica(),
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Student {
    nadimak: String,
}

#[wasm_bindgen]
impl Student {
    #[wasm_bindgen(getter)]
    pub fn nadimak(&self) -> String {
        self.nadimak.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_nadimak(&mut self, nadimak: String) {
        self.nadimak = nadimak;
    }

    pub fn clone(&self) -> Student {
        Student {
            nadimak: self.nadimak(),
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Djelatnik {
    kabinet: String,
    vrijeme_konzultacija: Vec<String>,
}

#[wasm_bindgen]
impl Djelatnik {
    #[wasm_bindgen(getter)]
    pub fn kabinet(&self) -> String {
        self.kabinet.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_kabinet(&mut self, kabinet: String) {
        self.kabinet = kabinet;
    }
    #[wasm_bindgen(getter)]
    pub fn vrijeme_konzultacija(&self) -> Vec<String> {
        self.vrijeme_konzultacija.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijeme_konzultacija(&mut self, vrijeme_konzultacija: Vec<String>) {
        self.vrijeme_konzultacija = vrijeme_konzultacija;
    }
}

// #[wasm_bindgen]
// pub enum StudentDjelatnik {
//     Student,
//     Djelatnik,
// }
//
// #[wasm_bindgen]
// impl StudentDjelatnik {
//     #[wasm_bindgen]
//     pub fn describe(&self) -> String {
//         match self {
//             StudentDjelatnik::Student => Student(Student {}),
//             StudentDjelatnik::Djelatnik => format!("Bar: {}", djelatnik.kabinet()),
//         }
//     }
// }

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Kolegij {
    id: i32,
    naziv: String,
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
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
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
#[derive(Serialize, Deserialize)]
pub struct Sekcija {
    id: i32,
    naziv: String,
    opis: String,
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
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Sadrzaj {
    id: i32,
    naziv: String,
    tip: String,
    vrijednost: Dokument,
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
    pub fn vrijednost(&self) -> Dokument {
        self.vrijednost.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_vrijednost(&mut self, vrijednost: Dokument) {
        self.vrijednost = vrijednost;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
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

    pub fn clone(&self) -> Dokument {
        Dokument {
            referenca: self.referenca(),
            vrijeme_kreiranja: self.vrijeme_kreiranja(),
        }
    }
}
