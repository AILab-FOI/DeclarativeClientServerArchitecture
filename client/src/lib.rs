use std::future::Future;

use reqwest::{self};
use serde::de::DeserializeOwned;
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;

mod types;

#[wasm_bindgen]
pub async fn dohvati_studente(token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/student", token).await {
        Ok(response) => parse_data::<Vec<types::Student>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_korisnika(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result =
        match create_get_request(&format!("http://localhost:5000/student/{}", id), token).await {
            Ok(response) => parse_data::<types::Student>(response).await,
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
    let json_data: types::Response<T> = serde_json::from_str::<types::Response<T>>(&body).unwrap();
    Ok(to_value(&json_data).unwrap())
}
