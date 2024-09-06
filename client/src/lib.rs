use std::future::Future;

use js_sys::Uint8Array;
use reqwest::multipart;
use serde::de::DeserializeOwned;
use serde_json::Value;
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
pub async fn uredi_fakultet(
    id: i32,
    naziv: String,
    opis: String,
    logo: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "opis": opis,
        "logo": logo,
    });
    let result = match create_patch_request("http://localhost:5000/faculty", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
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
pub async fn dodaj_katedru(naziv: String, opis: String, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "opis": opis
    });
    let result = match create_post_request("http://localhost:5000/department", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn obrisi_katedru(id: i32, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
    });
    let result = match create_delete_request("http://localhost:5000/department", obj, token).await {
        Ok(response) => parse_data::<String>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_katedru(
    id: i32,
    naziv: String,
    opis: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "opis": opis,
    });
    let result = match create_patch_request("http://localhost:5000/department", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_katedru_na_fakultet(
    id_katedra: i32,
    id_fakultet: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "katedra": id_katedra,
        "fakultet":id_fakultet
    });
    let result =
        match create_post_request("http://localhost:5000/faculty_department", obj, token).await {
            Ok(response) => parse_data::<bool>(response).await,
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
pub async fn dodaj_kolegij(
    naziv: String,
    skraceno: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "skraceno": skraceno
    });
    let result = match create_post_request("http://localhost:5000/course", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn obrisi_kolegij(id: i32, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
    });
    let result = match create_delete_request("http://localhost:5000/course", obj, token).await {
        Ok(response) => parse_data::<String>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_kolegij(
    id: i32,
    naziv: String,
    skraceno: String,
    slika: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "skraceno": skraceno,
        "slika": slika
    });
    let result = match create_patch_request("http://localhost:5000/course", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_kolegij_na_katedru(
    id_katedra: i32,
    id_kolegij: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "katedra": id_katedra,
        "kolegij":id_kolegij
    });
    let result =
        match create_post_request("http://localhost:5000/department_course", obj, token).await {
            Ok(response) => parse_data::<bool>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn dodaj_djelatnika_na_katedru(
    id_katedra: i32,
    id_djelatnik: i32,
    tip: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "katedra": id_katedra,
        "djelatnik":id_djelatnik,
        "tip": tip
    });
    let result =
        match create_post_request("http://localhost:5000/department_worker", obj, token).await {
            Ok(response) => parse_data::<bool>(response).await,
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
pub async fn dodaj_sekciju(naziv: String, opis: String, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "opis": opis
    });
    let result = match create_post_request("http://localhost:5000/section/", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_sekciju(
    id: i32,
    naziv: String,
    opis: String,
    vidljivo: bool,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "opis": opis,
        "vidljivo": vidljivo,
    });
    let result = match create_patch_request("http://localhost:5000/section", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn obrisi_sekciju(id: i32, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
    });
    let result = match create_delete_request("http://localhost:5000/section", obj, token).await {
        Ok(response) => parse_data::<String>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_sekciju_na_kolegij(
    id_sekcija: i32,
    id_kolegij: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "kolegij": id_kolegij,
        "sekcija": id_sekcija,
    });
    let result = match create_post_request("http://localhost:5000/course_section", obj, token).await
    {
        Ok(response) => parse_data::<bool>(response).await,
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
pub async fn dodaj_dokument(
    naziv: String,
    poveznica: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "tip": "dokument",
        "vrijednost":{
            "referenca": poveznica
        }
    });
    let result = match create_post_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}
#[wasm_bindgen]
pub async fn uredi_dokument(
    id: i32,
    naziv: String,
    poveznica: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "tip": "dokument",
        "vrijednost":{
            "referenca": poveznica
        }
    });
    let result = match create_patch_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}
#[wasm_bindgen]
pub async fn dodaj_lekciju(
    naziv: String,
    sadrzaj: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "tip": "lekcija",
        "vrijednost":{
            "sadrzaj":sadrzaj
        }
    });
    let result = match create_post_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_lekciju(
    id: i32,
    naziv: String,
    sadrzaj: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "tip": "lekcija",
        "vrijednost":{
            "sadrzaj":sadrzaj
        }
    });
    let result = match create_patch_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_poveznicu(
    naziv: String,
    poveznica: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "naziv": naziv,
        "tip": "poveznica",
        "vrijednost":{
            "referenca": poveznica
        }
    });
    let result = match create_post_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_poveznicu(
    id: i32,
    naziv: String,
    poveznica: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "naziv": naziv,
        "tip": "poveznica",
        "vrijednost":{
            "referenca": poveznica
        }
    });
    let result = match create_patch_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_sadrzaj_na_sekciju(
    id_sekcija: i32,
    id_sadrzaj: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "sekcija": id_sekcija,
        "sadrzaj": id_sadrzaj
    });
    let result =
        match create_post_request("http://localhost:5000/section_content", obj, token).await {
            Ok(response) => parse_data::<bool>(response).await,
            Err(_) => parse_network_err(),
        };
    result
}

#[wasm_bindgen]
pub async fn obrisi_sadrzaj(id: i32, token: String) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
    });
    let result = match create_delete_request("http://localhost:5000/content", obj, token).await {
        Ok(response) => parse_data::<String>(response).await,
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
pub async fn dohvati_korisnike_na_fakultetu(id: i32, token: String) -> Result<JsValue, JsValue> {
    let result = match create_get_request(
        &format!("http://localhost:5000/faculty_user/{}", id),
        token,
    )
    .await
    {
        Ok(response) => parse_data::<Vec<types::Korisnik>>(response).await,
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
            "http://localhost:5000/student_course/{}/{}",
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
            "http://localhost:5000/worker_course/{}/{}",
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
pub async fn dodaj_studenta_na_kolegij(
    id_kolegij: i32,
    id_student: i32,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "student": id_student,
        "kolegij": id_kolegij
    });
    let result = match create_post_request("http://localhost:5000/student_course", obj, token).await
    {
        Ok(response) => parse_data::<bool>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn dodaj_djelatnika_na_kolegij(
    id_kolegij: i32,
    id_djelatnik: i32,
    tip: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "djelatnik": id_djelatnik,
        "kolegij": id_kolegij,
        "tip": tip
    });
    let result = match create_post_request("http://localhost:5000/worker_course", obj, token).await
    {
        Ok(response) => parse_data::<bool>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_studenta(
    id: i32,
    opis: String,
    nadimak: String,
    slika: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "opis": opis,
        "nadimak": nadimak,
        "slika": slika,
    });
    let result = match create_patch_request("http://localhost:5000/student", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
        Err(_) => parse_network_err(),
    };
    result
}

#[wasm_bindgen]
pub async fn uredi_djelatnika(
    id: i32,
    opis: String,
    kabinet: String,
    slika: String,
    token: String,
) -> Result<JsValue, JsValue> {
    let obj = serde_json::json!({
        "id": id,
        "opis": opis,
        "kabinet": kabinet,
        "slika": slika,
    });
    let result = match create_patch_request("http://localhost:5000/worker", obj, token).await {
        Ok(response) => parse_data::<i32>(response).await,
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
        Ok(response) => Ok(to_value(&"ASD").unwrap()),
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

pub fn create_get_request(
    uri: &str,
    token: String,
) -> impl Future<Output = Result<reqwest::Response, reqwest::Error>> {
    let client = reqwest::Client::new();
    client.get(uri).bearer_auth(token).send()
}

pub fn create_post_request(
    uri: &str,
    object: Value,
    token: String,
) -> impl Future<Output = Result<reqwest::Response, reqwest::Error>> {
    let client = reqwest::Client::new();
    client
        .post(uri)
        .header("Content-Type", "application/json")
        .json(&object)
        .bearer_auth(token)
        .send()
}
pub fn create_patch_request(
    uri: &str,
    object: Value,
    token: String,
) -> impl Future<Output = Result<reqwest::Response, reqwest::Error>> {
    let client = reqwest::Client::new();
    client.patch(uri).json(&object).bearer_auth(token).send()
}
pub fn create_delete_request(
    uri: &str,
    object: Value,
    token: String,
) -> impl Future<Output = Result<reqwest::Response, reqwest::Error>> {
    let client = reqwest::Client::new();
    client.delete(uri).json(&object).bearer_auth(token).send()
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
        Err(err) => {
            return Err(JsValue::from_str("Pogreška u parsiranju"));
        }
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
