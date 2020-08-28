extern crate tokio;
use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;
use tokio::task;

use lazy_static::lazy_static;

extern crate clap;
use clap::{App, Arg};
use snow::{params::NoiseParams, Builder};

/**
 *
 * noisetokio: snow + tokio demo
 *
 *
 * This is an adaptation of examples/simple.rs from the great mcginty/snow crate
 * snow is an implementation of https://noiseprotocol.org/ for rust:
 *
 * "Crypto protocols that are simple, fast, and secure".
 *
 *
 * This example differs from the original example in 2 essential ways:
 *
 * * it uses tokio, rust's most popular asynchronous runtime
 *
 * * it demonstrates the use of a Post-Quantum Hybrid ciphersuite
 *   from the draft extension to the noise protocol specification
 *
 *
 */

static SECRET: &'static [u8] = b"i don't care for fidget spinners";
lazy_static! {
    static ref PARAMS: NoiseParams = "Noise_XXhfs+psk3_25519+Kyber1024_ChaChaPoly_BLAKE2s".parse().unwrap();
}

async fn server_handle_connection(mut socket: tokio::net::TcpStream) -> Result<(), anyhow::Error> {
    let mut tx_buf = vec![0u8; 65535];
    let mut msg_buf = vec![0u8; 65535];

    println!("handle connection.");
    let builder: Builder<'_> = Builder::new(PARAMS.clone());
    let static_key = builder.generate_keypair().unwrap().private;
    let mut noise = builder
        .local_private_key(&static_key)
        .psk(3, SECRET)
        .build_responder()?;

    println!("noise initialized.");

    println!("<- e");
    noise.read_message(&recv(&mut socket).await?, &mut msg_buf).map_err(
        |e| { eprintln!("{}", e); e }
    )?;

    println!("-> e, ee, s, es");
    let len = noise.write_message(&[0u8; 0], &mut tx_buf)?;
    send(&mut socket, &tx_buf[..len]).await?;

    println!("<- s, se");
    noise.read_message(&recv(&mut socket).await?, &mut msg_buf).map_err(
        |e| { eprintln!("{}", e); e }
    )?;

    // Transition the state machine into transport mode now that the handshake is complete.
    let mut noise = noise.into_transport_mode()?;
    println!("session established.");
    
    loop {
        let msg = recv(&mut socket).await?;
        let len = noise.read_message(&msg, &mut msg_buf).map_err(
            |e| { eprintln!("{}", e); e }
        )?;
        println!("client said: {}", String::from_utf8_lossy(&msg_buf[..len]));

        break;
    }

    println!("connection closed.");
    Ok(())
}

/// Hyper-basic stream transport receiver. 16-bit BE size followed by payload.
async fn recv(stream: &mut TcpStream) -> Result<Vec<u8>, anyhow::Error> {
    let mut msg_len_buf = [0u8; 2];
    stream.read_exact(&mut msg_len_buf).await?;
    let msg_len = ((msg_len_buf[0] as usize) << 8) + (msg_len_buf[1] as usize);
    
    let mut msg = vec![0u8; msg_len];
    stream.read_exact(&mut msg[..]).await?;
    Ok(msg)
}

/// Hyper-basic stream transport sender. 16-bit BE size followed by payload.
async fn send(stream: &mut TcpStream, buf: &[u8]) -> Result<(), anyhow::Error> {
    let msg_len_buf = [(buf.len() >> 8) as u8, (buf.len() & 0xff) as u8];
    stream.write_all(&msg_len_buf).await?;
    stream.write_all(buf).await?;
    Ok(())
}

async fn server() -> Result<(), anyhow::Error> {
    println!("listening on 127.0.0.1:9999");
    let mut listener = TcpListener::bind("127.0.0.1:9999").await?;

    loop {
        let (socket, _addr) = listener.accept().await?;
        socket.set_linger(None)?;
        task::spawn(
            server_handle_connection(socket)
        );
    }
}

async fn client() -> Result<(), anyhow::Error> {
    let mut tx_buf = vec![0u8; 65535];
    let mut msg_buf = vec![0u8; 65535];

    let builder: Builder<'_> = Builder::new(PARAMS.clone());
    let static_key = builder.generate_keypair().unwrap().private;
    let mut noise = builder
        .local_private_key(&static_key)
        .psk(3, SECRET)
        .build_initiator()?;

    println!("noise initialized.");

    // Connect to our server, which is hopefully listening.
    let mut stream = TcpStream::connect("127.0.0.1:9999").await?;
    println!("TCP stream connected.");

    println!("-> e");
    let len = noise.write_message(&[], &mut tx_buf)?;
    send(&mut stream, &tx_buf[..len]).await?;
    println!("ok, sent {}", len);

    println!("<- e, ee, s, es");
    noise.read_message(&recv(&mut stream).await?, &mut msg_buf).map_err(
        |e| { eprintln!("{}", e); e }
    )?;

    println!("-> s, se");
    let len = noise.write_message(&[], &mut msg_buf)?;
    send(&mut stream, &msg_buf[..len]).await?;

    let mut noise = noise.into_transport_mode()?;
    println!("session established.");

    // Get to the important business of sending secured data.
    // for _ in 0..10 {
    let len = noise.write_message(b"HACK ON", &mut tx_buf)?;
    send(&mut stream, &tx_buf[..len]).await?;
    println!("encrypted message sent.");
    // }

    Ok(())
}

fn main() {
    println!("noiseprotocol (snow) + tokio demo app");

    let matches = App::new("noisetokio")
        .about("Does awesome things")
        .arg(
            Arg::with_name("is_server")
                .short("s")
                .help("Be server not client"),
        )
        .get_matches();

    let is_server = matches.is_present("is_server");

    let mut rt = tokio::runtime::Runtime::new().unwrap();

    match if is_server {
        rt.block_on(server())
    } else {
        rt.block_on(client())
    } {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
        }
    };
}
