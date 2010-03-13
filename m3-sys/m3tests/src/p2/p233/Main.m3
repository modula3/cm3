UNSAFE MODULE Main;

(* This program endeavors to exercise comparisons
 * for subranges and enums that don't overlap
 * or overlap only at the edge.
 * This should probably be in the "c for code" section.
 * What is being tested is that many of these functions
 * return just a constant TRUE or FALSE.
 *)

<*UNUSED*>PROCEDURE AddressLT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<NIL; END AddressLT0;
<*UNUSED*>PROCEDURE AddressLE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<=NIL; END AddressLE0;
<*UNUSED*>PROCEDURE AddressGT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>NIL; END AddressGT0;
<*UNUSED*>PROCEDURE AddressGE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>=NIL; END AddressGE0;
<*UNUSED*>PROCEDURE AddressEQ0(a:ADDRESS):BOOLEAN=BEGIN RETURN a=NIL; END AddressEQ0;
<*UNUSED*>PROCEDURE AddressNE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a#NIL; END AddressNE0;


<*UNUSED*>PROCEDURE CardinalLT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<0; END CardinalLT0;
<*UNUSED*>PROCEDURE CardinalLE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=0; END CardinalLE0;
<*UNUSED*>PROCEDURE CardinalGT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>0; END CardinalGT0;
<*UNUSED*>PROCEDURE CardinalGE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=0; END CardinalGE0;
<*UNUSED*>PROCEDURE CardinalEQ0(a:CARDINAL):BOOLEAN=BEGIN RETURN a=0; END CardinalEQ0;
<*UNUSED*>PROCEDURE CardinalNE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a#0; END CardinalNE0;

<*UNUSED*>PROCEDURE LongcardLT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<0L; END LongcardLT0;
<*UNUSED*>PROCEDURE LongcardLE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<=0L; END LongcardLE0;
<*UNUSED*>PROCEDURE LongcardGT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>0L; END LongcardGT0;
<*UNUSED*>PROCEDURE LongcardGE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>=0L; END LongcardGE0;
<*UNUSED*>PROCEDURE LongcardEQ0(a:LONGCARD):BOOLEAN=BEGIN RETURN a=0L; END LongcardEQ0;
<*UNUSED*>PROCEDURE LongcardNE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a#0L; END LongcardNE0;


<*UNUSED*>PROCEDURE no_overlap_less_LT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_LT;
<*UNUSED*>PROCEDURE no_overlap_less_LE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_LE;
<*UNUSED*>PROCEDURE no_overlap_less_GT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_GT;
<*UNUSED*>PROCEDURE no_overlap_less_GE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_GE;
<*UNUSED*>PROCEDURE no_overlap_less_EQ(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_NE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_NE;

<*UNUSED*>PROCEDURE minimum_overlap_less_LT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_LE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_GT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_GE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_EQ(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_NE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_NE;


<*UNUSED*>PROCEDURE no_overlap_greater_LT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_LE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_GT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_GE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_EQ(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_NE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_NE;


<*UNUSED*>PROCEDURE minimum_overlap_greater_LT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_LE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_EQ(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_NE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_NE;


TYPE Numbers = {Zero, One, Two, Three, Four};
TYPE LowNumbers = [Numbers.Zero..Numbers.One];
TYPE HighNumbers = [Numbers.Three..Numbers.Four];
TYPE MiddleNumbers = [Numbers.One..Numbers.Three];


<*UNUSED*>PROCEDURE no_overlap_less_enum_LT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_LE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_EQ(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_enum_NE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_enum_NE;

<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_EQ(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_NE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_enum_NE;


<*UNUSED*>PROCEDURE no_overlap_greater_enum_LT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_LE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_EQ(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_NE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_enum_NE;


<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_EQ(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_NE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_enum_NE;

BEGIN
END Main.
