use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Book {
    pub id: u64,
    pub title: String,
    pub author: String,
}
