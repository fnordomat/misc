extern crate ot_poc;
use ot_poc::*;

use std::os::unix::net::UnixStream;
use std::thread;

extern crate rand_os;

extern crate openssl;
use openssl::rsa::{Padding, Rsa};

extern crate simplelog;
use simplelog::*;

use std::sync::{Once, ONCE_INIT};

fn setup() {
    CombinedLogger::init(
        vec![
            TermLogger::new(LevelFilter::Info, Config::default(), TerminalMode::Mixed).unwrap(),
        ]
    ).unwrap();
}

static INIT: Once = ONCE_INIT;

/// Confirm that openssl::rsa works as expected
#[test]
fn test_crypto_openssl_rsa() {
    INIT.call_once(|| {
        setup();
    });

    let rsa = Rsa::generate(1024).unwrap();
    let mut data = vec![0; rsa.size() as usize];
    let testword = b"foobar";
    assert!(testword.len() <= data.len());
    data[..testword.len() as usize].clone_from_slice(testword);
    let mut buf1 = vec![0; rsa.size() as usize];
    let encrypted_len1 = rsa.public_encrypt(&data, &mut buf1, Padding::NONE).unwrap();
    let mut buf2 = vec![0; rsa.size() as usize];
    let encrypted_len2 = rsa.public_encrypt(&data, &mut buf2, Padding::NONE).unwrap();
    assert_eq!(encrypted_len1, encrypted_len2);

    // Textbook RSA is a deterministic operation.
    assert_eq!(buf1, buf2);

    let mut buf1a = vec![0; rsa.size() as usize];
    let _decrypted_len = rsa
        .private_decrypt(&buf1, &mut buf1a, Padding::NONE)
        .unwrap();
    assert_eq!(data, buf1a);

    let mut buf3 = vec![0; rsa.size() as usize];
    let _decrypted_len = rsa
        .private_decrypt(&data, &mut buf3, Padding::NONE)
        .unwrap();
    assert_ne!(data, buf3);
    let mut buf3a = vec![0; rsa.size() as usize];
    let _encrypted_len = rsa
        .public_encrypt(&buf3, &mut buf3a, Padding::NONE)
        .unwrap();
    assert_eq!(data, buf3a);
}

/// Run the OT protocol
#[test]
fn test_clisrv() {
    INIT.call_once(|| {
        setup();
    });

    let (sock1, sock2) = match UnixStream::pair() {
        Ok((sock1, sock2)) => (sock1, sock2),
        Err(e) => {
            println!("Couldn't create a pair of sockets: {:?}", e);
            return;
        }
    };

    let cli_thr = thread::spawn(move || {
        client(sock1).unwrap();
    });

    let srv_thr = thread::spawn(move || {
        server(sock2).unwrap();
    });

    let r = cli_thr.join();
    if let Err(ref e) = r {
        eprintln!("error joining client: {:?}", e);
    }

    let r = srv_thr.join();
    if let Err(ref e) = r {
        eprintln!("error joining server: {:?}", e);
    }

    println!("test: done!");
}
