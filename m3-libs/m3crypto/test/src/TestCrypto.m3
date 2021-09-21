(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE TestCrypto EXPORTS Main;

IMPORT
  Crypto_Hash   AS Hash,
  Crypto_Hmac   AS Hmac,
  Crypto_Pbkdf2 AS Pbkdf2,
  Crypto_Scram  AS Scram,
  Text;


PROCEDURE Test_Md5() =

  PROCEDURE Test(expected, message: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.MD5);
    BEGIN
      Hash.Update(hash, message);
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test;

  BEGIN
    (* https://tools.ietf.org/html/rfc1321 *)
    Test("d41d8cd98f00b204e9800998ecf8427e", "");
    Test("0cc175b9c0f1b6a831c399e269772661", "a");
    Test("900150983cd24fb0d6963f7d28e17f72", "abc");
    Test("f96b697d7cb7938d525a2f31aaf161d0", "message digest");
    Test("c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz");
    Test("d174ab98d277d9f5a5611c2c9f419d9f",
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    Test("57edf4a22be3c955ac49da2e2107b67a",
         "12345678901234567890123456789012345678901234567890123456789012345678901234567890");

    (* https://en.wikipedia.org/wiki/MD5 *)
    Test("9e107d9d372bb6826bd81d3542a419d6",
         "The quick brown fox jumps over the lazy dog");
    Test("e4d909c290d0fb1ca068ffaddf22cbd0",
         "The quick brown fox jumps over the lazy dog.")
  END Test_Md5;


PROCEDURE Test_Sha1() =

  PROCEDURE Test(expected, message: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA1);
    BEGIN
      Hash.Update(hash, message);
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test;

  PROCEDURE Test_1m() =
    (* FIPS 180-2 Secure Hash Standard *)
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA1);
    BEGIN
      FOR i := 1 TO 1000000 BY 50 DO
        Hash.Update(hash, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      END;
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, "34aa973cd4c4daa4f61eeb2bdbad27316534016f") *>
    END Test_1m;

  BEGIN
    Test("da39a3ee5e6b4b0d3255bfef95601890afd80709", "");
    Test("2fd4e1c67a2d28fced849ee1bb76e7391b93eb12",
         "The quick brown fox jumps over the lazy dog");
    Test("de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3",
         "The quick brown fox jumps over the lazy cog");
    Test("a9993e364706816aba3e25717850c26c9cd0d89d", "abc");
    Test("84983e441c3bd26ebaae4aa1f95129e5e54670f1",
         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    Test("86f7e437faa5a7fce15d1ddcb9eaeaea377667b8", "a");
    Test("e0c094e867ef46c350ef54a7f59dd60bed92ae83",
         "0123456701234567012345670123456701234567012345670123456701234567");

    (* FIPS 180-2 Secure Hash Standard *)
    Test("a9993e364706816aba3e25717850c26c9cd0d89d", "abc");
    Test("84983e441c3bd26ebaae4aa1f95129e5e54670f1",
         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");

    Test_1m()
  END Test_Sha1;


PROCEDURE Test_Sha2() =

  PROCEDURE Test256(expected, message: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA256);
    BEGIN
      Hash.Update(hash, message);
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test256;

  PROCEDURE Test224(expected, message: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA224);
    BEGIN
      Hash.Update(hash, message);
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test224;

  PROCEDURE Test_1m() =
    (* FIPS 180-2 Secure Hash Standard *)
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA256);
    BEGIN
      FOR i := 1 TO 1000000 BY 50 DO
        Hash.Update(hash, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      END;
      Hash.HexDigest(hash, actual);
      <* ASSERT Text.Equal(actual, "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0") *>
    END Test_1m;

  BEGIN
    Test224("d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f", "");
    Test224("730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525",
            "The quick brown fox jumps over the lazy dog");
    Test224("619cba8e8e05826e9b8c519c0a5c68f4fb653e8a3d8aa04bb2c8cd4c",
            "The quick brown fox jumps over the lazy dog.");

    (* FIPS 180-2 Secure Hash Standard *)
    Test256("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", "abc");
    Test256("248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1",
            "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");

    Test_1m()
  END Test_Sha2;


PROCEDURE Test_Hmac() =

  PROCEDURE Test_Md5(expected, key, data: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.MD5);
      hmac := Hmac.New(hash, key);
    BEGIN
      Hmac.Update(hmac, data);
      Hmac.HexDigest(hmac, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test_Md5;

  PROCEDURE Test_Sha1(expected, key, data: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA1);
      hmac := Hmac.New(hash, key);
    BEGIN
      Hmac.Update(hmac, data);
      Hmac.HexDigest(hmac, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test_Sha1;

  PROCEDURE Test_Sha224(expected, key, data: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA224);
      hmac := Hmac.New(hash, key);
    BEGIN
      Hmac.Update(hmac, data);
      Hmac.HexDigest(hmac, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test_Sha224;

  PROCEDURE Test_Sha256(expected, key, data: TEXT) =
    VAR
      actual: TEXT;
      hash := Hash.New(Hash.Algorithm.SHA256);
      hmac := Hmac.New(hash, key);
    BEGIN
      Hmac.Update(hmac, data);
      Hmac.HexDigest(hmac, actual);
      <* ASSERT Text.Equal(actual, expected) *>
    END Test_Sha256;

  BEGIN
    (* https://tools.ietf.org/html/rfc2202 *)
    Test_Md5("9294727a3638bb1c13f48ef8158bfc9d",
             "\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013",
             "Hi There");
    Test_Md5("750c783e6ab0b503eaa86e310a5db738",
             "Jefe",
             "what do ya want for nothing?");
    Test_Md5("56be34521d144c88dbb8c733f0e8b3f6",
             "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
             "\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335");
    Test_Md5("697eaf0aca3a3aea3a75164746ffaa79",
             "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\013\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19",
             "\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315");
    Test_Md5("56461ef2342edc00f9bab995690efd4c",
             "\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014",
             "Test With Truncation");
    Test_Md5("6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd",
             "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
             "Test Using Larger Than Block-Size Key - Hash Key First");
    Test_Md5("6f630fad67cda0ee1fb1f562db3aa53e",
             "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
             "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data");

    Test_Sha1("b617318655057264e28bc0b6fb378c8ef146be00",
              "\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013",
              "Hi There");
    Test_Sha1("effcdf6ae5eb2fa2d27416d5f184df9c259a7c79",
              "Jefe",
              "what do ya want for nothing?");
    Test_Sha1("125d7342b9ac11cd91a39af48aa17b4f63f175d3",
              "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
              "\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335");
    Test_Sha1("4c9007f4026250c6bc8414f9bf50c86c2d7235da",
              "\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031",
              "\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315");
    Test_Sha1("4c1a03424b55e07fe7f27be1d58bb9324a9a5a04",
              "\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014",
              "Test With Truncation");
    Test_Sha1("aa4ae5e15272d00e95705637ce8a3b55ed402112",
              "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
              "Test Using Larger Than Block-Size Key - Hash Key First");
    Test_Sha1("e8e99d0f45237d786d6bbaa7965c7808bbff1a91",
              "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
              "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data");

    (* https://tools.ietf.org/html/rfc4231 *)
    Test_Sha224("896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22",
                "\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013",
                "Hi There");
    Test_Sha256("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7",
                "\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013\013",
                "Hi There");

    Test_Sha224("a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44",
                "Jefe",
                "what do ya want for nothing?");
    Test_Sha256("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843",
                "Jefe",
                "what do ya want for nothing?");

    Test_Sha224("7fb3cb3588c6c1f6ffa9694d7d6ad2649365b0c1f65d69d1ec8333ea",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335");
    Test_Sha256("773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335\335");

    Test_Sha224("6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a",
                "\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031",
                "\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315");
    Test_Sha256("82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b",
                "\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031",
                "\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315");

    Test_Sha224("95e9a0db962095adaebe9b2d6f0dbce2d499f112f2d2b7273fa6870e",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "Test Using Larger Than Block-Size Key - Hash Key First");
    Test_Sha256("60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "Test Using Larger Than Block-Size Key - Hash Key First");

    Test_Sha224("3a854166ac5d9f023f54d517d0b39dbd946770db9c2b95c9f6f565d1",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.");
    Test_Sha256("9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2",
                "\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252\252",
                "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")
  END Test_Hmac;


  PROCEDURE Test_Pbkdf2() =

    PROCEDURE Test(expected, password, salt: TEXT; iterations, keyLen: CARDINAL) =
      VAR
        actual: TEXT;
        hash := Hash.New(Hash.Algorithm.SHA1);
      BEGIN
        actual := Pbkdf2.DerivedKey(hash, password, salt, iterations, keyLen);
        <* ASSERT Text.Equal(Hash.ToHexString(actual), expected) *>
      END Test;

    BEGIN
      (* https://tools.ietf.org/html/rfc6070 *)
      Test("0c60c80f961f0e71f3a9b524af6012062fe037a6", "password", "salt", 1, 20);
      Test("ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957", "password", "salt", 2, 20);
      Test("4b007901b765489abead49d926f721d065a429c1", "password", "salt", 4096, 20);
      Test("3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038",
           "passwordPASSWORDpassword",
           "saltSALTsaltSALTsaltSALTsaltSALTsalt",
           4096,
           25);
      Test("56fa6aa75548099dcc37d7f03425e0c3", "pass\000word", "sa\000lt", 4096, 16);

      (* SLOW
      Test("eefe3d61cd4da4e4e9945b3d6ba2158c2634e984", "password", "salt", 16777216, 20)
      *)
    END Test_Pbkdf2;


  PROCEDURE Test_Scram() =
    (* https://wiki.xmpp.org/web/SASL_Authentication_and_SCRAM *)
    VAR
      client, salted, server, stored: TEXT;
      sha1 := Hash.New(Hash.Algorithm.SHA1);
    BEGIN
      salted := Scram.SaltedPassword(sha1, "pencil", "\101\045\302\107\344\072\261\351\074\155\377\166", 4096);
      <* ASSERT Text.Equal("1d96ee3a529b5a5f9e47c01f229a2cb8a6e15f7d", Hash.ToHexString(salted)) *>
      client := Scram.ClientKey(sha1, salted);
      <* ASSERT Text.Equal("e234c47bf6c36696dd6d852b99aaa2ba26555728", Hash.ToHexString(client)) *>
      server := Scram.ServerKey(sha1, salted);
      <* ASSERT Text.Equal("0fe09258b3ac852ba502cc62ba903eaacdbf7d31", Hash.ToHexString(server)) *>
      stored := Scram.StoredKey(sha1, client);
      <* ASSERT Text.Equal("e9d94660c39d65c38fbad91c358f14da0eef2bd6", Hash.ToHexString(stored)) *>
    END Test_Scram;


BEGIN
  Test_Md5();
  Test_Sha1();
  Test_Sha2();
  Test_Hmac();
  Test_Pbkdf2();
  Test_Scram()
END TestCrypto.
