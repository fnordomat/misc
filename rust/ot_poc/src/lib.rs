#![allow(non_snake_case)]
use std::io::{Read, Write};
extern crate rand_os;
use rand_os::rand_core::RngCore;
use rand_os::OsRng;

extern crate openssl;
use openssl::rsa::{Padding, Rsa};

extern crate byteorder;
use byteorder::{BigEndian, WriteBytesExt, ReadBytesExt};
use std::io::Cursor;
use std::string::String;

#[macro_use]
extern crate log;
extern crate simplelog;

/// https://en.wikipedia.org/wiki/Oblivious_transfer

/// Ambiguously but compactly print a u8-string, replacing non-printable characters with dots
fn printString(s: &[u8]) -> String {
    let mut out : String = String::new();
    for x in s {
        if (*x as u8) < 32 || (*x as u8) >= 127 {
            out.push('.');
        } else {
            out.push(*x as char);
        }
    }
    out
}

pub fn xorInto(block1: &mut [u8], block2: &[u8]) {
    block2.iter().enumerate().for_each(
        |(i, x2)|
        {
            block1[i] ^= x2.clone();
        }
    );
}

fn receive_alice_key<T: Read + Write>(socket: &mut T) -> Result<openssl::rsa::Rsa<openssl::pkey::Public>, Box<std::error::Error>> {
    let mut alice_key : Vec<u8> = vec![];
    let mut alice_keylen_msg = vec![0,0,0,0, 0,0,0,0];
    info!("Bob: expecting key size to be sent.");
    let c = socket.read_exact(&mut alice_keylen_msg);
    info!("{:?}", &alice_keylen_msg[..]);
    let mut rdr = Cursor::new(&alice_keylen_msg);
    let maybe_len = rdr.read_u64::<BigEndian>();
    if let Err(e) = maybe_len {
        return Err(
            Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,format!("Error {:?} receiving key length.", e)))) }
    let alice_key_len = maybe_len? as usize;
    info!("Bob: keylen: {:?}, {:?}", c, alice_key_len);
    if alice_key_len > 1024*1024 {
        return Err(
            Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,"Public key implausibly large."))) }
    alice_key.resize(alice_key_len, 0);
    info!("Bob: expecting key to be sent.");
    let c = socket.read_exact(&mut alice_key);
    info!("Bob: key DER read: {:?}", c);

    let alice_key = Rsa::public_key_from_der(&alice_key[..]);
    let alice_key = match alice_key {
        Ok(ok) => { ok }
        Err(e) => {
            return Err(
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::Other,format!("Error {:?} decoding public key.",e))))
        }
    };

    info!("Bob: DER -> key success");

    return Ok(alice_key)
}

/// Client process "Bob"
pub fn client<T: Read + Write>(mut socket: T) -> Result<(), Box<std::error::Error>> {
    let mut rng = OsRng::new().unwrap();
    let choice  = (rng.next_u32() % 2 == 0) as u32;
    info!("Bob: my choice is {:}", choice);

    let alice_key = receive_alice_key(&mut socket)?;

    info!("Bob: expect values of sizes corresponding to key size: {:?}", alice_key.size());

    let mut ticket0 = vec![];
    ticket0.resize(alice_key.size() as usize, 0);
    let c = socket.read_exact(&mut ticket0);
    info!("Bob: ticket0 read: {:?}", c);

    let mut ticket1 = vec![];
    ticket1.resize(alice_key.size() as usize, 0);
    let c = socket.read_exact(&mut ticket1);
    info!("Bob: ticket1 read: {:?}", c);

    let mut nonce = vec![0; alice_key.size() as usize];
    rng.fill_bytes(&mut nonce[..]);
    nonce[0] = 0;
    info!("Bob: made nonce.");

    info!("Bob: my choice: {:}", choice);
    let mut msg = vec![0; alice_key.size() as usize];
    info!("Bob: resized msg buffer to {:}", alice_key.size());
    info!("Bob: {:} {:} {:}", nonce.len(), msg.len(), alice_key.size());

    match alice_key.public_encrypt(&nonce[..], &mut msg[..], Padding::NONE) {
        Ok(_) => {
            info!("Bob: encrypted the nonce.");
        }
        Err(e) => {
            info!("Bob: error {:} encrypting nonce.", e);
            return Err(
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::Other,format!("Error {:} encrypting nonce.", e))))
        }
    }

    if choice == 0 {
        xorInto(&mut msg, &ticket0[..]);
    } else {
        xorInto(&mut msg, &ticket1[..]);
    }

    info!("Bob: send msg");
    let c = socket.write(&msg[..]);
    info!("Bob: msg send: {:?}", c);

    let mut reply0 = vec![];
    let mut reply1 = vec![];
    reply0.resize(alice_key.size() as usize, 0);
    reply1.resize(alice_key.size() as usize, 0);
    let _c = socket.read_exact(&mut reply0);
    info!("Bob: reply0 read: {:?}", printString(&reply0[..]));
    let _c = socket.read_exact(&mut reply1);
    info!("Bob: reply1 read: {:?}", printString(&reply1[..]));

    let mut message = nonce.clone();

    if choice == 0 {
        xorInto(&mut message, &reply0);
    } else {
        xorInto(&mut message, &reply1);
    }

    info!("Bob: Alice's value {:} is {:}", choice, printString(&message[..]));
    println!("{:}", printString(&message[..]));

    Ok(())
}

/// Server process "Alice"
pub fn server<T: Read + Write>(mut socket: T) -> Result<(), Box<std::error::Error>> {
    let mut rng = OsRng::new().unwrap();
    let rsa = Rsa::generate(1024)?;
    info!("Alice: 1024bit key generated.");

    let message0 = b"FOO";
    let message1 = b"BAR";

    info!("Alice: my messages are \"{:}\" and \"{:}\", but I'm not telling Bob that.", printString(message0), printString(message1));

    let der = rsa.public_key_to_der()?;
    info!("Alice: my public key for this round is {:} bytes long.", der.len());

    let mut l = vec![];
    l.write_u64::<BigEndian>(der.len() as u64).unwrap();
    let c = socket.write(&l);
    info!("Alice: keylen sent? {:?}.", c);
    let c = socket.write(&der);
    info!("Alice: key sent? {:?}.", c);

    info!("Alice: key size {:?} bytes.", rsa.size());

    let mut ticket0 = vec![];
    ticket0.resize(rsa.size() as usize, 0);
    rng.fill_bytes(&mut ticket0[..]);
    ticket0[0] = 0;

    let mut ticket1 = vec![];
    ticket1.resize(rsa.size() as usize, 0);
    rng.fill_bytes(&mut ticket1[..]);
    ticket1[0] = 0;

    info!("Alice: tickets prepared.");
    let _ = socket.write(&ticket0[..]);
    let _ = socket.write(&ticket1[..]);
    info!("Alice: tickets sent.");

    let mut msg = vec![];
    msg.resize(rsa.size() as usize, 0);
    info!("Alice: resized message buffer to {:} (size of key).", rsa.size());

    let c = socket.read_exact(&mut msg);
    info!("Alice: msg recv: {:?}", c);
    
    let mut msg0 = msg.clone();
    let mut msg1 = msg.clone();

    xorInto(&mut msg0, &ticket0[..]);
    xorInto(&mut msg1, &ticket1[..]);

    let mut msgd0 = vec![];
    let mut msgd1 = vec![];
    msgd0.resize(rsa.size() as usize, 0);
    msgd1.resize(rsa.size() as usize, 0);

    match rsa.private_decrypt(&msg0[..], &mut msgd0[..], Padding::NONE) {
        Ok(_ok) => {
            info!("Alice: decrypted the message.");
        }
        Err(e) => {
            info!("Alice: error {:?} decrypting message.", e);
            return Err(
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::Other,format!("Error {:} decrypting msg.", e))))
        }
    }

    match rsa.private_decrypt(&msg1[..], &mut msgd1[..], Padding::NONE) {
        Ok(_ok) => {
            info!("Alice: decrypted the message.");
        }
        Err(e) => {
            info!("Alice: error {:?} decrypting message.", e);
            return Err(
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::Other,format!("Error {:} decrypting msg.", e))))
        }
    }

    // Alice now holds a message which is k0 or k1, Bob's nonce.
    info!("Alice: adding message-to-be-sent to Bob's nonce");
    xorInto(&mut msgd0, message0);
    xorInto(&mut msgd1, message1);

    info!("Alice: msgd0 (with message0) = {:?}", printString(&msgd0[..]));
    info!("Alice: msgd1 (with message1) = {:?}", printString(&msgd1[..]));

    let _ = socket.write(&msgd0[..]);
    let _ = socket.write(&msgd1[..]);
    info!("Alice: replies k0+m, k1+m sent.");

    Ok(())
}
