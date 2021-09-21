# m3crypto

Cryptographic hashes and relatied functions, including HMAC, PBKDF2,
and SCRAM.

Exported modules:

* `Crypto_Hash`
* `Crypto_Hmac`
* `Crypto_Pbkdf2`
* `Crypto_Scram`


## Hashes

[Crypto_Hash](src/Crypto_Hash.i3) exports basic hash functions.

Usage:

```modula-3
IMPORT
  Crypto_Hash AS Hash;

VAR
  algo: Hash.Algorithm;
  digest, message: TEXT;
  hash: Hash.T;

BEGIN
  algo := Hash.Algorithm.SHA256;
  hash := Hash.New(algo);
  Hash.Update(hash, message);
  Hash.Digest(hash, digest);
END
```

Implemented algorithms:

* `Crypto_Hash.Algorithm.MD5`
* `Crypto_Hash.Algorithm.SHA1`
* `Crypto_Hash.Algorithm.SHA224`
* `Crypto_Hash.Algorithm.SHA256`


## HMAC

[Crypto_Hmac](src/Crypto_Hmac.i3) provides keyed one-way compression
for message authentication.

Usage:

```modula-3
IMPORT
  Crypto_Hash AS Hash,
  Crypto_Hmac AS Hmac;

VAR
  digest, message, password: TEXT;
  hash: Hash.T;
  hmac: Hmac.T;

BEGIN
  hash := Hash.New(algo);
  hmac := Hmac.New(hash, password);
  Hmac.Update(hmac, message);
  Hmac.Digest(hmac, digest)
END
```


## PBKDF2

[Crypto_Pbkdf2](src/Crypto_Pbkdf2.i3) implements key stretching.

Usage:

```modula-3
IMPORT
  Crypto_Hash   AS Hash,
  Crypto_Pbkdf2 AS Pbkdf2;

VAR
  algo: Hash.Algorithm;
  derivedKey, password, salt: TEXT;
  hash: Hash.T;
  iterations, keyLength: CARDINAL;

BEGIN
  hash := Hash.New(algo);
  derivedKey := Pbkdf2.DerivedKey(hash, password, salt, iterations, keyLength)
END
```


## SCRAM

[Crypto_Scram](src/Crypto_Scram.i3) supplies encryption primitives for
SCRAM authentication.

Usage:

```modula-3
IMPORT
  Crypto_Hash  AS Hash,
  Crypto_Scram AS Scram;

VAR
  clientKey, saltedPassword, serverKey, storedKey: TEXT;
  hash: Hash.T;
  iterations: CARDINAL;
  password, salt: TEXT;


BEGIN
  saltedPassword := Scram.SaltedPassword(hash, password, salt, iterations);
  clientKey := Scram.ClientKey(hash, saltedPassword);
  serverKey := Scram.ServerKey(hash, saltedPassword);
  storedKey := Scram.StoredKey(hash, clientKey)
END
```


## License

[MIT](LICENSE)
