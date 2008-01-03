@Insertion
procedure insertion;@
  var v: integer;
  begin
  @1 for i := 2 to N do@
    begin
    @2 v := a[i];@ @3 j := i;@
    @4 while a[j-1] > v do@
      begin @5 a[j] := a[j - 1];@ @6 j := j - 1@ end;
    end;
    @7 a[j] := v;@
  end;
@Insertion
