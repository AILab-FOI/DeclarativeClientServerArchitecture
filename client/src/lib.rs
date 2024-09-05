use std::future::Future;

use js_sys::Uint8Array;
use reqwest::multipart;
use serde::de::DeserializeOwned;
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;
use web_sys::console;

mod types;

#[wasm_bindgen]
pub async fn dohvati_fakultete() -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/faculty", "".to_string()).await {
        Ok(response) => parse_data::<Vec<types::Fakultet>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_fakultet(id: i32) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!("http://localhost:5000/faculty/{}", id),
        "".to_string(),
    )
    .await
    {
        Ok(response) => parse_data::<types::Fakultet>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_katedre() -> Result<JsValue, JsValue> {
    let result = match create_get_request("http://localhost:5000/deparment", "".to_string()).await {
        Ok(response) => parse_data::<Vec<types::Katedra>>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dohvati_katedru(id: i32) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!("http://localhost:5000/department/{}", id),
        "".to_string(),
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
pub async fn dohvati_studenta_na_kolegiju(
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
pub async fn dohvati_djelatnika_na_kolegiju(
    id_korisnik: i32,
    id_kolegij: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!(
            "http://localhost:5000/worker/course/{}/{}",
            id_korisnik, id_kolegij
        ),
        token,
    )
    .await
    {
        Ok(response) => parse_data::<types::DjelatnikKolegij>(response).await,
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
                let err = types::MyError::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = types::MyError::new(500, "Network error");
            Err(to_value(&err).unwrap())
        }
    };
    result
}

#[wasm_bindgen]
pub async fn send_multipart(
    database: &str,
    id: &str,
    data: &[u8],
    filetype: &str,
    filename: &str,
) -> Result<JsValue, JsValue> {
    let client = reqwest::Client::new();
    let form = multipart::Form::new().part("file", multipart::Part::bytes(data.to_vec()));
    let result = match client
        .post("http://localhost:5000/upload")
        .multipart(form)
        .send()
        .await
    {
        Ok(response) => {
            console::log_1(&"ASD".into());
            Ok(to_value(&"ASD").unwrap())
        }
        Err(_) => {
            let err = types::MyError::new(500, "Network error");
            Err(to_value(&err).unwrap())
        }
    };
    result
}

#[wasm_bindgen]
pub async fn upload_file(database: String, id: String, file: web_sys::File) {
    let reader = web_sys::FileReader::new().unwrap();
    let filename = file.name();
    let onload = Closure::wrap(Box::new(move |event: web_sys::ProgressEvent| {
        let target = event.target().unwrap();
        let reader = target.dyn_into::<web_sys::FileReader>().unwrap();
        let result = reader.result().unwrap();
        let array = Uint8Array::new(&result);
        let data = array.to_vec();
        send_multipart(&database, &id, &data, &"png", &filename.clone());
    }) as Box<dyn FnMut(_)>);

    reader.set_onload(Some(onload.as_ref().unchecked_ref()));
    reader.read_as_array_buffer(&file).unwrap();
    onload.forget();
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
                let err = types::MyError::new(status.as_u16() as i32, &json_data.data.to_string());
                Err(to_value(&err).unwrap())
            }
        }
        Err(_) => {
            let err = types::MyError::new(500, "Network error");
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
    let err = types::MyError::new(500, "Network error");
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
