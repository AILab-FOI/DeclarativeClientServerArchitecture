use reqwest;
use serde::{Deserialize, Serialize};
use serde_json;
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    // The `console.log` is quite polymorphic, so we can bind it with multiple
    // signatures. Note that we need to use `js_name` to ensure we always call
    // `log` in JS.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u32(a: u32);

    // Multiple arguments too!
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

#[derive(Serialize, Deserialize)]
struct Response<T> {
    data: T,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
struct Student {
    id: i32,
    ime: String,
    prezime: String,
    oib: i32,
    opis: String,
}

#[wasm_bindgen]
impl Student {
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
    pub fn opis(&self) -> String {
        self.opis.clone()
    }
    #[wasm_bindgen(setter)]
    pub fn set_opis(&mut self, opis: String) {
        self.opis = opis;
    }
}

#[wasm_bindgen]
pub async fn dohvati_studente(token: String) -> Result<JsValue, JsValue> {
    let client = reqwest::Client::new();
    let res = client
        .get("http://localhost:5000/student")
        .header("Authorization", format!("Bearer {}", token))
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let res_data = res.text().await.map_err(|e| e.to_string())?;
    let res_json: Response<Vec<Student>> =
        serde_json::from_str(&res_data).map_err(|e| e.to_string())?;
    Ok(to_value(&res_json).map_err(|e| e.to_string())?)
}

#[wasm_bindgen]
pub async fn login(email: String, password: String) -> Result<JsValue, JsValue> {
    let client = reqwest::Client::new();

    let korisnik = serde_json::json!({
        "email": email,
        "password": password
    });

    let res = client
        .put("http://localhost:5000/login")
        .json(&korisnik)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let res_data = res.text().await.map_err(|e| e.to_string())?;
    let res_json: Response<String> = serde_json::from_str(&res_data).map_err(|e| e.to_string())?;
    Ok(to_value(&res_json).map_err(|e| e.to_string())?)
}
