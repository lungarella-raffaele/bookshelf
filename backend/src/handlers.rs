use super::models::Book;

pub async fn get_post(id: u64) -> Result<impl warp::Reply, warp::Rejection> {
    let book = Book {
        id,
        title: String::from("Solaris"),
        author: String::from("Stanisław Lem"),
    };

    Ok(warp::reply::json(&book))
}
