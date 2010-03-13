UNSAFE MODULE Main;

(* This program endeavors to exercise comparisons
 * for subranges and enums that don't overlap
 * or overlap only at the edge.
 * This should probably be in the "c for code" section.
 * What is being tested is that many of these functions
 * return just a constant TRUE or FALSE.
 *)

(* address < NIL should probably always be false, but ADDRESS is the only
 * case that isn't working (being optimized).
 *)

<*UNUSED*>PROCEDURE AddressLT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<NIL; END AddressLT0;
<*UNUSED*>PROCEDURE AddressLE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<=NIL; END AddressLE0;
<*UNUSED*>PROCEDURE AddressGT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>NIL; END AddressGT0;
<*UNUSED*>PROCEDURE AddressGE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>=NIL; END AddressGE0;
<*UNUSED*>PROCEDURE AddressEQ0(a:ADDRESS):BOOLEAN=BEGIN RETURN a=NIL; END AddressEQ0;
<*UNUSED*>PROCEDURE AddressNE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a#NIL; END AddressNE0;


(* Cardinal is always >= 0, etc. *)

<*UNUSED*>PROCEDURE CardinalLT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<0; END CardinalLT0;
<*UNUSED*>PROCEDURE CardinalLE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=0; END CardinalLE0;
<*UNUSED*>PROCEDURE CardinalGT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>0; END CardinalGT0;
<*UNUSED*>PROCEDURE CardinalGE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=0; END CardinalGE0;
<*UNUSED*>PROCEDURE CardinalEQ0(a:CARDINAL):BOOLEAN=BEGIN RETURN a=0; END CardinalEQ0;
<*UNUSED*>PROCEDURE CardinalNE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a#0; END CardinalNE0;


(* Cardinal is always > -1, etc. *)

<*UNUSED*>PROCEDURE CardinalLTNeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-1; END CardinalLTNeg1;
<*UNUSED*>PROCEDURE CardinalLENeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-1; END CardinalLENeg1;
<*UNUSED*>PROCEDURE CardinalGTNeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-1; END CardinalGTNeg1;
<*UNUSED*>PROCEDURE CardinalGENeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-1; END CardinalGENeg1;
<*UNUSED*>PROCEDURE CardinalNENeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-1; END CardinalNENeg1;
<*UNUSED*>PROCEDURE CardinalEQNeg1(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-1; END CardinalEQNeg1;


(* Cardinal is always > -2, etc. (same as -1) *)

<*UNUSED*>PROCEDURE CardinalLTNeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-2; END CardinalLTNeg2;
<*UNUSED*>PROCEDURE CardinalLENeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-2; END CardinalLENeg2;
<*UNUSED*>PROCEDURE CardinalGTNeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-2; END CardinalGTNeg2;
<*UNUSED*>PROCEDURE CardinalGENeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-2; END CardinalGENeg2;
<*UNUSED*>PROCEDURE CardinalNENeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-2; END CardinalNENeg2;
<*UNUSED*>PROCEDURE CardinalEQNeg2(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-2; END CardinalEQNeg2;


(* Longcard is always > -1, etc. (same as Cardinal) *)

<*UNUSED*>PROCEDURE LongcardLT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<0L; END LongcardLT0;
<*UNUSED*>PROCEDURE LongcardLE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<=0L; END LongcardLE0;
<*UNUSED*>PROCEDURE LongcardGT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>0L; END LongcardGT0;
<*UNUSED*>PROCEDURE LongcardGE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>=0L; END LongcardGE0;
<*UNUSED*>PROCEDURE LongcardEQ0(a:LONGCARD):BOOLEAN=BEGIN RETURN a=0L; END LongcardEQ0;
<*UNUSED*>PROCEDURE LongcardNE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a#0L; END LongcardNE0;


(* subranges with no overlap, left always less than right *)

<*UNUSED*>PROCEDURE no_overlap_less_LT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_LT;
<*UNUSED*>PROCEDURE no_overlap_less_LE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_LE;
<*UNUSED*>PROCEDURE no_overlap_less_GT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_GT;
<*UNUSED*>PROCEDURE no_overlap_less_GE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_GE;
<*UNUSED*>PROCEDURE no_overlap_less_EQ(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_NE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_NE;


(* subranges with overlap only on the edige, left always less or equal than right *)

<*UNUSED*>PROCEDURE minimum_overlap_less_LT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_LE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_GT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_GE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_EQ(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_NE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_NE;


(* subranges with no overlap, left always greater than right *)

<*UNUSED*>PROCEDURE no_overlap_greater_LT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_LE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_GT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_GE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_EQ(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_NE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_NE;


(* subranges with overlap only on the edige, left always greater or equal than right *)

<*UNUSED*>PROCEDURE minimum_overlap_greater_LT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_LE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_EQ(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_NE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_NE;


(* now enums instead of integer subranges *)


TYPE Numbers = {Zero, One, Two, Three, Four};
TYPE LowNumbers = [Numbers.Zero..Numbers.One];
TYPE HighNumbers = [Numbers.Three..Numbers.Four];
TYPE MiddleNumbers = [Numbers.One..Numbers.Three];


(* enums where left is always less than right *)

<*UNUSED*>PROCEDURE no_overlap_less_enum_LT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_LE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_EQ(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_enum_NE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_enum_NE;


(* enums that overlap only on the edge but otherwise the left is less than the right *)

<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_EQ(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_NE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_enum_NE;


(* enums where left is always greater than right *)

<*UNUSED*>PROCEDURE no_overlap_greater_enum_LT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_LE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_EQ(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_NE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_enum_NE;


(* enums that overlap only on the edge but otherwise the left is greater than the right *)

<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_EQ(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_NE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_enum_NE;


(* unlikely, but test the case of single element subranges *)

<*UNUSED*>PROCEDURE overlap_1_LT(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<b; END overlap_1_LT;
<*UNUSED*>PROCEDURE overlap_1_LE(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<=b; END overlap_1_LE;
<*UNUSED*>PROCEDURE overlap_1_GT(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>b; END overlap_1_GT;
<*UNUSED*>PROCEDURE overlap_1_GE(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>=b; END overlap_1_GE;
<*UNUSED*>PROCEDURE overlap_1_EQ(a,b:[0..0]):BOOLEAN=BEGIN RETURN a=b; END overlap_1_EQ;
<*UNUSED*>PROCEDURE overlap_1_NE(a,b:[0..0]):BOOLEAN=BEGIN RETURN a#b; END overlap_1_NE;


(* ORD(enum) vs. negative *)

<*UNUSED*>PROCEDURE ord_enum_vs_negative__LT(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)<-1; END ord_enum_vs_negative__LT;
<*UNUSED*>PROCEDURE ord_enum_vs_negative__LE(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)<=-1; END ord_enum_vs_negative__LE;
<*UNUSED*>PROCEDURE ord_enum_vs_negative__GT(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)>-1; END ord_enum_vs_negative__GT;
<*UNUSED*>PROCEDURE ord_enum_vs_negative__GE(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)>=-1; END ord_enum_vs_negative__GE;
<*UNUSED*>PROCEDURE ord_enum_vs_negative__EQ(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)=-1; END ord_enum_vs_negative__EQ;
<*UNUSED*>PROCEDURE ord_enum_vs_negative__NE(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)#-1; END ord_enum_vs_negative__NE;


(* ABS vs. negative *)

<*UNUSED*>PROCEDURE abs_vs_negative_LT(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<-1; END abs_vs_negative_LT;
<*UNUSED*>PROCEDURE abs_vs_negative_LE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<=-1; END abs_vs_negative_LE;
<*UNUSED*>PROCEDURE abs_vs_negative_GT(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>-1; END abs_vs_negative_GT;
<*UNUSED*>PROCEDURE abs_vs_negative_GE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=-1; END abs_vs_negative_GE;
<*UNUSED*>PROCEDURE abs_vs_negative_EQ(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)=-1; END abs_vs_negative_EQ;
<*UNUSED*>PROCEDURE abs_vs_negative_NE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)#-1; END abs_vs_negative_NE;


(* ABS vs. 0 *)

<*UNUSED*>PROCEDURE abs_vs_zero_LT(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<0; END abs_vs_zero_LT;
<*UNUSED*>PROCEDURE abs_vs_zero_LE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<=0; END abs_vs_zero_LE;
<*UNUSED*>PROCEDURE abs_vs_zero_GT(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>0; END abs_vs_zero_GT;
<*UNUSED*>PROCEDURE abs_vs_zero_GE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=0; END abs_vs_zero_GE;
<*UNUSED*>PROCEDURE abs_vs_zero_EQ(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)=0; END abs_vs_zero_EQ;
<*UNUSED*>PROCEDURE abs_vs_zero_NE(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)#0; END abs_vs_zero_NE;


(* NEG(ABS) vs. CARDINAL (doesn't work) *)

<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_LT(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))<b; END neg_abs_vs_cardinal_LT;
<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_LE(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))<=b; END neg_abs_vs_cardinal_LE;
<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_GT(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))>b; END neg_abs_vs_cardinal_GT;
<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_GE(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))>=b; END neg_abs_vs_cardinal_GE;
<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_EQ(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))=b; END neg_abs_vs_cardinal_EQ;
<*UNUSED*>PROCEDURE neg_abs_vs_cardinal_NE(a:INTEGER;b:CARDINAL):BOOLEAN=BEGIN RETURN (-ABS(a))#b; END neg_abs_vs_cardinal_NE;


(* NEG(ABS) vs. 0 *)

<*UNUSED*>PROCEDURE neg_abs_vs_zero_LT(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<0; END neg_abs_vs_zero_LT;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_LE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=0; END neg_abs_vs_zero_LE;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_GT(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>0; END neg_abs_vs_zero_GT;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_GE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>=0; END neg_abs_vs_zero_GE;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_EQ(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))=0; END neg_abs_vs_zero_EQ;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_NE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))#0; END neg_abs_vs_zero_NE;


(* NEG(ABS) vs. 1 *)

<*UNUSED*>PROCEDURE neg_abs_vs_one_LT(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<1; END neg_abs_vs_one_LT;
<*UNUSED*>PROCEDURE neg_abs_vs_one_LE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=1; END neg_abs_vs_one_LE;
<*UNUSED*>PROCEDURE neg_abs_vs_one_GT(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>1; END neg_abs_vs_one_GT;
<*UNUSED*>PROCEDURE neg_abs_vs_one_GE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>=1; END neg_abs_vs_one_GE;
<*UNUSED*>PROCEDURE neg_abs_vs_one_EQ(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))=1; END neg_abs_vs_one_EQ;
<*UNUSED*>PROCEDURE neg_abs_vs_one_NE(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))#1; END neg_abs_vs_one_NE;

BEGIN
END Main.
