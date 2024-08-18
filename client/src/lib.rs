use std::{
    any::{self, Any},
    future::Future,
};

use reqwest::{self};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;
use web_sys::js_sys::Intl::DateTimeFormat;

#[derive(Serialize, Deserialize)]
struct Response<T> {
    data: T,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
struct Tokens {
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
struct Error {
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
struct Korisnik {
    id: i32,
    ime: String,
    prezime: String,
    oib: i32,
    uloga: String,
    email: String,
    opis: String,
    dodatno: StudentDjelatnik,
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
    pub fn dodatno(&self) -> StudentDjelatnik {
        self.dodatno.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_dodatno(&mut self, dodatno: StudentDjelatnik) {
        self.dodatno = dodatno;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
struct Student {
    kolegiji: Vec<i32>,
}

#[wasm_bindgen]
impl Student {
    #[wasm_bindgen(getter)]
    pub fn kolegiji(&self) -> Vec<i32> {
        self.kolegiji.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_kolegiji(&mut self, kolegiji: Vec<i32>) {
        self.kolegiji = kolegiji;
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
struct Djelatnik {
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

#[wasm_bindgen]
pub enum StudentDjelatnik {
    Student(Student),
    Djelatnik(Djelatnik),
}

#[wasm_bindgen]
impl StudentDjelatnik {
    #[wasm_bindgen]
    pub fn describe(&self) -> String {
        match self {
            StudentDjelatnik::Student(student) => format!("Foo: {}", student.kolegiji()),
            StudentDjelatnik::Djelatnik(djelatnik) => format!("Bar: {}", djelatnik.kabinet()),
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
struct Kolegij {
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
pub async fn dohvati_studente(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/student", token).await {
        Ok(response) => parse_data::<Vec<Student>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_korisnika(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/student/{}", id), token).await {
            Ok(response) => parse_data::<Student>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn login(email: String, password: String) -> Result<JsValue, JsValue> {
    let client = reqwest::Client::new();

    let korisnik = serde_json::json!({
        "email": email,
        "password": password
    });

    let result = match client
        .put("http://localhost:5000/login")
        .json(&korisnik)
        .send()
        .await
    {
        Ok(response) => {
            let status = response.status();

            if status.is_success() {
                let body = response.text().await.unwrap();
                let json_data: Response<Tokens> = serde_json::from_str(&body).unwrap();
                Ok(to_value(&json_data).unwrap())
            } else {
                let body = response.text().await.unwrap();
                let json_data: Response<String> = serde_json::from_str(&body).unwrap();
                let err = Error::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = Error::new(500, "Network error");
            Err(to_value(&err).unwrap())
        }
    };
    result
}

#[wasm_bindgen]
pub async fn refresh_tokens(token: String) -> Result<JsValue, JsValue> {
    let client = reqwest::Client::new();
    let token = serde_json::json!({
        "refresh_token": token
    });
    let result = match client
        .put("http://localhost:5000/jwt/refresh")
        .json(&token)
        .send()
        .await
    {
        Ok(response) => {
            let status = response.status();

            if status.is_success() {
                let body = response.text().await.unwrap();
                let json_data: Response<Tokens> = serde_json::from_str(&body).unwrap();
                Ok(to_value(&json_data).unwrap())
            } else {
                let body = response.text().await.unwrap();
                let json_data: Response<String> = serde_json::from_str(&body).unwrap();
                let err = Error::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = Error::new(500, "Network error");
            Err(to_value(&err).unwrap())
        }
    };
    result
}

pub fn create_get_request(
    uri: &str,
    token: String,
) -> impl Future<Output = Result<reqwest::Response, reqwest::Error>> {
    let client = reqwest::Client::new();
    client.get(uri).bearer_auth(token).send()
}

pub fn parse_network_err() -> Result<JsValue, JsValue> {
    let err = Error::new(500, "Network error");
    Err(to_value(&err).unwrap())
}

pub async fn parse_data<T>(response: reqwest::Response) -> Result<JsValue, JsValue>
where
    T: DeserializeOwned + serde::Serialize,
{
    let body = response.text().await.unwrap();
    let json_data: Response<T> = serde_json::from_str::<Response<T>>(&body).unwrap();
    Ok(to_value(&json_data).unwrap())
}
