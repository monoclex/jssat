use crate::Data;
use std::{error::Error, io::Cursor};

use miniserde::json;
use tiny_http::{Header, Method, Response, Server, StatusCode};

const INDEX_HTML: &str = include_str!(env!("INDEX_HTML"));
const INDEX_JS: &str = include_str!(env!("INDEX_JS"));

fn cors_headers() -> Vec<Header> {
    use std::str::FromStr;
    vec![Header::from_str("Access-Control-Allow-Origin: *").unwrap()]
}

fn resp_from_str(string: &str) -> Response<Cursor<&[u8]>> {
    let read = Cursor::new(string.as_bytes());
    Response::new(
        StatusCode(200),
        cors_headers(),
        read,
        Some(string.len()),
        None,
    )
}

fn resp_from_string(string: String) -> Response<Cursor<Vec<u8>>> {
    let str_len = string.len();
    let read = Cursor::new(string.into_bytes());
    Response::new(StatusCode(200), cors_headers(), read, Some(str_len), None)
}

pub fn launch(listen_url: &str, data: &Data) -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    let server = Server::http(listen_url)?;

    for request in server.incoming_requests() {
        eprintln!("handling {} {}", request.method(), request.url());

        let path = (request.url().trim_matches('/').split('/')).collect::<Vec<_>>();

        let response = match (request.method(), path.as_slice()) {
            (Method::Get, [""]) => request.respond(resp_from_str(INDEX_HTML)),
            (Method::Get, ["index.js"]) => request.respond(resp_from_str(INDEX_JS)),
            (Method::Get, ["overview"]) => {
                request.respond(resp_from_string(json::to_string(&data.overview())))
            }
            (Method::Get, ["moment", moment]) => {
                let moment = moment.parse::<usize>()?;
                request.respond(resp_from_string(json::to_string(&data.moment(moment))))
            }
            (unknown, route) => {
                println!("unhandled route {} {:?}", unknown, route);
                continue;
            }
        };

        response?;
    }

    Ok(())
}
