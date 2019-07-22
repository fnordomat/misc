extern crate clap;
extern crate libc;

use std::net::{TcpListener, TcpStream};

extern crate ot_poc;
use ot_poc::*;

use clap::{App, Arg};

#[macro_use]
extern crate log;
extern crate simplelog;
use simplelog::*;

fn main() -> Result<(), Box<std::error::Error>> {
    
    CombinedLogger::init(
        vec![
            TermLogger::new(LevelFilter::Warn, Config::default(), TerminalMode::Mixed).unwrap(),
        ]
    ).unwrap();
    
    // warn!("test");
    info!("test");
    debug!("test");

    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }

    println!("Hell O'World!");

    let matches = App::new("OT POC")
        .version("0.1.0")
        .author("fnordomat <GPG:46D46D1246803312401472B5A7427E237B7908CA>")
        .about("Oblivious Transfer POC")
        .arg(
            Arg::with_name("server")
                .short("s")
                .long("server")
                .help("Run as server"),
        )
        .arg(
            Arg::with_name("client")
                .short("c")
                .long("client")
                .help("Run as client"),
        )
        .get_matches();

    let run_server = matches.occurrences_of("server") > 0;
    let run_client = matches.occurrences_of("client") > 0;

    if (run_server && run_client) || (!run_server && !run_client) {
        eprintln!("Please specify either server or client mode.");
        std::process::exit(-1);
    }

    if run_server {
        let listener = TcpListener::bind("[::1]:51966")?;
        println!("This is OT POC, running in server mode.");
        for stream in listener.incoming() {
            server(stream?)?;
        }
    }

    if run_client {
        let socket = TcpStream::connect("[::1]:51966");
        println!("This is OT POC, running in client mode.");
        let socket = match socket {
            Ok(socket) => socket,
            Err(ref err) => {
                eprintln!("couldn't connect: {:}", err);
                std::process::exit(-1);
            }
        };
        client(socket)?;
    }

    Ok(())
}
