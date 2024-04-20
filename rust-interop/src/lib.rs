use hs_bindgen::*;
use testkit;
use testkit::base_request;
use testkit::base_request::TestContext;

#[hs_bindgen]
fn run_testkit(file: &str) {
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
        Err(err) => println!("RError: {:?}", err),
        Ok(resp) => println!("ROK: {:?}", resp),
    }
}
