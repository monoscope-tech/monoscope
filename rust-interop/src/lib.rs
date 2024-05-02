use hs_bindgen::*;
use testkit;
use testkit::base_request;
use testkit::base_request::TestContext;
use serde_json::{to_string, json};
use serde::Serialize;

fn to_json_string(obj: &impl Serialize) -> String {
    match to_string(obj) {
        Ok(json_string) => json_string,
        Err(e) => json!({"error": e.to_string()}).to_string(),
    }
}

#[hs_bindgen]
fn run_testkit(file: &str) -> String {
    let ctx = TestContext {
        plan: Some("plan".into()),
        file_source: "file source".into(),
        file: "file.tk.yaml".into(),
        path: ".".into(),
        step: Some("stage_name".into()),
        step_index: 0,
    };

    println!("RUST run_testkit file: {}", file);
    let result = tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(async { base_request::run_json(ctx, file.to_string(), true).await });

    match result {
        Ok(res) => to_json_string(&res),
        Err(e) => json!({"error": e.to_string()}).to_string(),
    }
}
