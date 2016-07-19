(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE CachedPowers;

FROM SimFP IMPORT GFP, Uint64, SignificandSize;

TYPE
  
  CP = RECORD
    significand : Uint64;
    binExp, decExp : INTEGER
  END;

CONST
  CachedPowersOffset = 348;  (* -1 * the first decimal_exponent. *)
  D_1_LOG2_10 = 0.30102999566398114D0;  (*  1 / lg(10) *)
  (* Difference between the decimal exponents in the table below. *)
  DecimalExponentDistance = 8;
  MinDecimalExponent = -348;
  MaxDecimalExponent = 340;

  CachedPowers = ARRAY [0..86] OF CP{
  CP{16_fa8fd5a0081c0288L, -1220, -348},
  CP{16_baaee17fa23ebf76L, -1193, -340},
  CP{16_8b16fb203055ac76L, -1166, -332},
  CP{16_cf42894a5dce35eaL, -1140, -324},
  CP{16_9a6bb0aa55653b2dL, -1113, -316},
  CP{16_e61acf033d1a45dfL, -1087, -308},
  CP{16_ab70fe17c79ac6caL, -1060, -300},
  CP{16_ff77b1fcbebcdc4fL, -1034, -292},
  CP{16_be5691ef416bd60cL, -1007, -284},
  CP{16_8dd01fad907ffc3cL, -980, -276},
  CP{16_d3515c2831559a83L, -954, -268},
  CP{16_9d71ac8fada6c9b5L, -927, -260},
  CP{16_ea9c227723ee8bcbL, -901, -252},
  CP{16_aecc49914078536dL, -874, -244},
  CP{16_823c12795db6ce57L, -847, -236},
  CP{16_c21094364dfb5637L, -821, -228},
  CP{16_9096ea6f3848984fL, -794, -220},
  CP{16_d77485cb25823ac7L, -768, -212},
  CP{16_a086cfcd97bf97f4L, -741, -204},
  CP{16_ef340a98172aace5L, -715, -196},
  CP{16_b23867fb2a35b28eL, -688, -188},
  CP{16_84c8d4dfd2c63f3bL, -661, -180},
  CP{16_c5dd44271ad3cdbaL, -635, -172},
  CP{16_936b9fcebb25c996L, -608, -164},
  CP{16_dbac6c247d62a584L, -582, -156},
  CP{16_a3ab66580d5fdaf6L, -555, -148},
  CP{16_f3e2f893dec3f126L, -529, -140},
  CP{16_b5b5ada8aaff80b8L, -502, -132},
  CP{16_87625f056c7c4a8bL, -475, -124},
  CP{16_c9bcff6034c13053L, -449, -116},
  CP{16_964e858c91ba2655L, -422, -108},
  CP{16_dff9772470297ebdL, -396, -100},
  CP{16_a6dfbd9fb8e5b88fL, -369, -92},
  CP{16_f8a95fcf88747d94L, -343, -84},
  CP{16_b94470938fa89bcfL, -316, -76},
  CP{16_8a08f0f8bf0f156bL, -289, -68},
  CP{16_cdb02555653131b6L, -263, -60},
  CP{16_993fe2c6d07b7facL, -236, -52},
  CP{16_e45c10c42a2b3b06L, -210, -44},
  CP{16_aa242499697392d3L, -183, -36},
  CP{16_fd87b5f28300ca0eL, -157, -28},
  CP{16_bce5086492111aebL, -130, -20},
  CP{16_8cbccc096f5088ccL, -103, -12},
  CP{16_d1b71758e219652cL, -77, -4},
  CP{16_9c40000000000000L, -50, 4},
  CP{16_e8d4a51000000000L, -24, 12},
  CP{16_ad78ebc5ac620000L, 3, 20},
  CP{16_813f3978f8940984L, 30, 28},
  CP{16_c097ce7bc90715b3L, 56, 36},
  CP{16_8f7e32ce7bea5c70L, 83, 44},
  CP{16_d5d238a4abe98068L, 109, 52},
  CP{16_9f4f2726179a2245L, 136, 60},
  CP{16_ed63a231d4c4fb27L, 162, 68},
  CP{16_b0de65388cc8ada8L, 189, 76},
  CP{16_83c7088e1aab65dbL, 216, 84},
  CP{16_c45d1df942711d9aL, 242, 92},
  CP{16_924d692ca61be758L, 269, 100},
  CP{16_da01ee641a708deaL, 295, 108},
  CP{16_a26da3999aef774aL, 322, 116},
  CP{16_f209787bb47d6b85L, 348, 124},
  CP{16_b454e4a179dd1877L, 375, 132},
  CP{16_865b86925b9bc5c2L, 402, 140},
  CP{16_c83553c5c8965d3dL, 428, 148},
  CP{16_952ab45cfa97a0b3L, 455, 156},
  CP{16_de469fbd99a05fe3L, 481, 164},
  CP{16_a59bc234db398c25L, 508, 172},
  CP{16_f6c69a72a3989f5cL, 534, 180},
  CP{16_b7dcbf5354e9beceL, 561, 188},
  CP{16_88fcf317f22241e2L, 588, 196},
  CP{16_cc20ce9bd35c78a5L, 614, 204},
  CP{16_98165af37b2153dfL, 641, 212},
  CP{16_e2a0b5dc971f303aL, 667, 220},
  CP{16_a8d9d1535ce3b396L, 694, 228},
  CP{16_fb9b7cd9a4a7443cL, 720, 236},
  CP{16_bb764c4ca7a44410L, 747, 244},
  CP{16_8bab8eefb6409c1aL, 774, 252},
  CP{16_d01fef10a657842cL, 800, 260},
  CP{16_9b10a4e5e9913129L, 827, 268},
  CP{16_e7109bfba19c0c9dL, 853, 276},
  CP{16_ac2820d9623bf429L, 880, 284},
  CP{16_80444b5e7aa7cf85L, 907, 292},
  CP{16_bf21e44003acdd2dL, 933, 300},
  CP{16_8e679c2f5e44ff8fL, 960, 308},
  CP{16_d433179d9c8cb841L, 986, 316},
  CP{16_9e19db92b4e31ba9L, 1013, 324},
  CP{16_eb96bf6ebadf77d9L, 1039, 332},
  CP{16_af87023b9bf0ee6bL, 1066, 340}    
  };

PROCEDURE GetCachedPowerForBinaryExponentRange(minExp, maxExp : INTEGER;
                                               VAR power : GFP;
                                               VAR decExp : INTEGER) =
  VAR
    cachedPower : CP;
    k,kQ,index,foo : INTEGER;
  BEGIN
    kQ := SignificandSize;
    k := CEILING(FLOAT((minExp + kQ - 1),LONGREAL) * D_1_LOG2_10);
    foo := CachedPowersOffset;
    index := ((foo + k - 1) DIV DecimalExponentDistance) + 1;
    <*ASSERT 0 <= index AND index < LAST(CachedPowers) *>
    cachedPower := CachedPowers[index];
    <*ASSERT minExp <= cachedPower.binExp *>
    <*ASSERT cachedPower.binExp <= maxExp *>
    decExp := cachedPower.decExp;
    power := NEW(GFP).init(cachedPower.significand, cachedPower.binExp);
  END GetCachedPowerForBinaryExponentRange; 

PROCEDURE GetCachedPowerForDecimalExponent(requestedExp : INTEGER;
                                           VAR power : GFP;
                                           VAR foundExp : INTEGER) =
  VAR
    cachedPower : CP;
    index : INTEGER;
  BEGIN
    <*ASSERT MinDecimalExponent <= requestedExp *>
    <*ASSERT requestedExp < MaxDecimalExponent + DecimalExponentDistance *>
    index := (requestedExp + CachedPowersOffset) DIV DecimalExponentDistance;
    cachedPower := CachedPowers[index];
    power := NEW(GFP).init(cachedPower.significand, cachedPower.binExp);
    foundExp := cachedPower.decExp;
    <*ASSERT foundExp <= requestedExp *>
    <*ASSERT requestedExp < foundExp + DecimalExponentDistance *>  
  END GetCachedPowerForDecimalExponent;
   
BEGIN
END CachedPowers.