MODULE Main;

IMPORT MD5,SHA256,Text,IO,Rd;

VAR
  res : TEXT;
  rd : Rd.T;

BEGIN
  res := MD5.FromText("abc");
  IO.Put(res & "\n");

  res := MD5.FromText("The quick brown fox jumps over the lazy dog");
  IO.Put(res & "\n");
  (* from wikipaedia https://en.wikipedia.org/wiki/MD5 *)
  <*ASSERT Text.Equal(res,"9e107d9d372bb6826bd81d3542a419d6") *>

  res := MD5.FromText("The quick brown fox jumps over the lazy dog.");
  IO.Put(res & "\n");
  <*ASSERT Text.Equal(res,"e4d909c290d0fb1ca068ffaddf22cbd0") *>

  rd := IO.OpenRead("xx.txt");
  res := MD5.FromFile(rd);
  IO.Put(res & "\n");

  res := SHA256.FromText("abc");
  IO.Put(res & "\n");

  res := SHA256.FromText("");
  IO.Put(res & "\n");
  (* from wikipaedia https://en.wikipedia.org/wiki/SHA-2 *)
  <*ASSERT Text.Equal(res,"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") *>

  res := SHA256.FromText("The quick brown fox jumps over the lazy dog");
  IO.Put(res & "\n");
  <*ASSERT Text.Equal(res, "d7a8fbb37d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592") *>

  res := SHA256.FromFile(rd);
  IO.Put(res & "\n");
END Main.
