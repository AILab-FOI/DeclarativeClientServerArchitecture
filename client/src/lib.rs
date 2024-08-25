use std::future::Future;

use reqwest::{self};
use serde::de::DeserializeOwned;
use serde_wasm_bindgen::{from_value, to_value};
use types::Response;
use wasm_bindgen::prelude::*;

mod types;

#[wasm_bindgen]
pub async fn dohvati_fakultete(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/faculty", token).await {
        Ok(response) => parse_data::<Vec<types::Fakultet>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_fakultet(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/faculty/{}", id), token).await {
            Ok(response) => parse_data::<types::Fakultet>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dohvati_katedre(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/deparment", token).await {
        Ok(response) => parse_data::<Vec<types::Katedra>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_katedru(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!("http://localhost:5000/department/{}", id),
        token,
    )
    .await
    {
        Ok(response) => parse_data::<types::Katedra>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}
#[wasm_bindgen]
pub async fn dohvati_kolegije(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/course", token).await {
        Ok(response) => parse_data::<Vec<types::Kolegij>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_kolegij(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/course/{}", id), token).await {
            Ok(response) => parse_data::<types::Kolegij>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dohvati_sekcije(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/section", token).await {
        Ok(response) => parse_data::<Vec<types::Sekcija>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_sekciju(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/section/{}", id), token).await {
            Ok(response) => parse_data::<types::Sekcija>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dohvati_sadrzaje(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/content", token).await {
        Ok(response) => parse_data::<Vec<types::Sadrzaj>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_sadrzaj(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/content/{}", id), token).await {
            Ok(response) => parse_data::<types::Sadrzaj>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dohvati_korisnike(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/user", token).await {
        Ok(response) => parse_data::<Vec<types::Korisnik>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_korisnika(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/user/{}", id), token).await {
            Ok(response) => parse_data::<types::Korisnik>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dohvati_korisnika_na_kolegiju(
    id_korisnik: i32,
    id_kolegij: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!(
            "http://localhost:5000/student/course/{}/{}",
            id_korisnik, id_kolegij
        ),
        token,
    )
    .await
    {
        Ok(response) => parse_data::<types::StudentKolegij>(response).await,
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
        .post("http://localhost:5000/login")
        .json(&korisnik)
        .send()
        .await
    {
        Ok(response) => {
            let status = response.status();

            if status.is_success() {
                let body = response.text().await.unwrap();
                let json_data: types::Response<types::Tokens> =
                    serde_json::from_str(&body).unwrap();
                Ok(to_value(&json_data).unwrap())
            } else {
                let body = response.text().await.unwrap();
                let json_data: types::Response<String> = serde_json::from_str(&body).unwrap();
                let err = types::Error::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = types::Error::new(500, "Network error");
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
                let json_data: types::Response<types::Tokens> =
                    serde_json::from_str(&body).unwrap();
                Ok(to_value(&json_data).unwrap())
            } else {
                let body = response.text().await.unwrap();
                let json_data: types::Response<String> = serde_json::from_str(&body).unwrap();
                let err = types::Error::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = types::Error::new(500, "Network error");
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
    let err = types::Error::new(500, "Network error");
    Err(to_value(&err).unwrap())
}

pub async fn parse_data<T>(response: reqwest::Response) -> Result<JsValue, JsValue>
where
    T: DeserializeOwned + serde::Serialize,
{
    let body = response.text().await.unwrap();
    let json_data: types::Response<T> = match serde_json::from_str::<types::Response<T>>(&body) {
        Ok(data) => data,
        Err(_) => return Err(JsValue::from_str("Pogreška u parsiranju")),
    };
    Ok(to_value(&json_data.data).unwrap())
}

pub async fn parse_vec_data<T>(response: reqwest::Response) -> Result<JsValue, JsValue>
where
    T: DeserializeOwned + serde::Serialize,
{
    let body: String = match response.text().await {
        Ok(res) => res,
        Err(_) => return Err(JsValue::from_str("Pogreška u text")),
    };
    let json_data: types::VecResponse = match serde_json::from_str::<types::VecResponse>(&body) {
        Ok(data) => data,
        Err(_) => return Err(JsValue::from_str("Pogreška u parse")),
    };
    Ok(to_value(&json_data.data).unwrap())
}
