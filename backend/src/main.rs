use warp::Filter;

#[tokio::main]
async fn main() {
    // API routes
    let api = warp::path("api").and(warp::path("health")).map(|| "OK");

    // Static files - serve directly from dist/ without /static/ prefix
    let static_files = warp::fs::dir("../frontend/dist/");

    // Combine: API takes precedence, then static files
    let routes = api.or(static_files);

    warp::serve(routes).run(([0, 0, 0, 0], 8000)).await;
}
