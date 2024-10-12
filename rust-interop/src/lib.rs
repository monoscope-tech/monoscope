use std::collections::HashMap;

use hs_bindgen::*;
use serde::Serialize;
use serde_json::{json, to_string};
use testkit;
use testkit::base_request;
use testkit::base_request::TestContext;

fn to_json_string(obj: &impl Serialize) -> String {
    match to_string(obj) {
        Ok(json_string) => json_string,
        Err(e) => json!({"error": e.to_string()}).to_string(),
    }
}

#[hs_bindgen]
fn run_testkit(file: &str, col: &str, local_vars: &str) -> String {
    let ctx = TestContext {
        plan: Some("plan".into()),
        file_source: "file source".into(),
        file: "file.tk.yaml".into(),
        path: ".".into(),
        step: Some("stage_name".into()),
        step_index: 0,
        should_log: false,
    };
    let local_vars_map: HashMap<String, String> =
        serde_json::from_str(local_vars).unwrap_or_default();

    let result = tokio::runtime::Runtime::new().unwrap().block_on(async {
        base_request::run_json(
            ctx,
            file.to_string(),
            Some(col.to_string()),
            Some(local_vars_map),
        )
        .await
    });

    match result {
        Ok(res) => to_json_string(&res),
        Err(e) => json!({
            "error": e.to_string(),
            "stacktrace": std::backtrace::Backtrace::force_capture().to_string(),
        })
        .to_string(),
    }
}
