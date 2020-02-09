#![allow(non_snake_case)]
#![allow(unknown_lints)]
#![allow(non_snake_oil)]

extern crate rand_os;
use rand_os::OsRng;
extern crate curve25519_dalek;
use curve25519_dalek::constants;
use curve25519_dalek::scalar::Scalar;
extern crate subtle;
use subtle::ConstantTimeEq;

///
/// Encrypt, then decrypt something with elliptic curves.
///
#[test]
fn test_crypto_dalek_ecc() {
    let mut rng = OsRng::new().unwrap();

    let P = constants::RISTRETTO_BASEPOINT_COMPRESSED;
    let secret_key = Scalar::random(&mut rng);
    let msg = Scalar::random(&mut rng);
    let k = Scalar::random(&mut rng);

    let public_key = secret_key * P.decompress().unwrap();

    // The following line is nonsense, of course. The message would have to be reversibly embedded, as explained in this SE thread:
    // https://crypto.stackexchange.com/questions/76340/how-to-create-an-ec-point-from-a-plaintext-message-for-encryption
    let Msg = msg * P.decompress().unwrap();
    let kP = k * P.decompress().unwrap();
    let kYM = k * public_key + Msg;

    let should_be = Msg;

    let decrypted = kYM - (secret_key * kP);
    assert_eq!(
        decrypted.ct_eq(&(should_be)).unwrap_u8(),
        1,
        "testing equality of {:?} and {:?}",
        decrypted,
        should_be
    );
}
