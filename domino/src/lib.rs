use std::{error::Error, io::Cursor};

use tiny_http::{Method, Response, Server, StatusCode};

const INDEX_HTML: &str = include_str!(env!("INDEX_HTML"));
const INDEX_JS: &str = include_str!(env!("INDEX_JS"));

fn str_resp(string: &str) -> Response<Cursor<&[u8]>> {
    let read = Cursor::new(string.as_bytes());
    Response::new(StatusCode(200), vec![], read, Some(string.len()), None)
}

pub fn launch(listen_url: &str) -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    let server = Server::http(listen_url)?;

    for request in server.incoming_requests() {
        let response = match (request.method(), request.url()) {
            (Method::Get, "/") => str_resp(INDEX_HTML),
            (Method::Get, "/index.js") => str_resp(INDEX_JS),
            _ => {
                eprintln!("unhandled {} {}", request.method(), request.url());
                continue;
            }
        };

        request.respond(response)?;
    }

    Ok(())
}
