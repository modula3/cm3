UNSAFE MODULE Main;

(* This program endeavors to exercise comparisons
 * for subranges and enums that don't overlap
 * or overlap only at the edge.
 * This should probably be in the "c for code" section.
 * What is being tested is that all of these functions
 * return just a constant TRUE or FALSE.
 *)

(* Cardinal is always >= 0, etc. *)

<*UNUSED*>PROCEDURE CardinalLT0_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<0; END CardinalLT0_false;
<*UNUSED*>PROCEDURE CardinalGE0_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=0; END CardinalGE0_true;


(* Cardinal is always > -1, etc. *)

<*UNUSED*>PROCEDURE CardinalLTNeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-1; END CardinalLTNeg1_false;
<*UNUSED*>PROCEDURE CardinalLENeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-1; END CardinalLENeg1_false;
<*UNUSED*>PROCEDURE CardinalGTNeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-1; END CardinalGTNeg1_true;
<*UNUSED*>PROCEDURE CardinalGENeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-1; END CardinalGENeg1_true;
<*UNUSED*>PROCEDURE CardinalNENeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-1; END CardinalNENeg1_true;
<*UNUSED*>PROCEDURE CardinalEQNeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-1; END CardinalEQNeg1_false;


(* Cardinal is always > -2, etc. (same as -1) *)

<*UNUSED*>PROCEDURE CardinalLTNeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-2; END CardinalLTNeg2_false;
<*UNUSED*>PROCEDURE CardinalLENeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-2; END CardinalLENeg2_false;
<*UNUSED*>PROCEDURE CardinalGTNeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-2; END CardinalGTNeg2_true;
<*UNUSED*>PROCEDURE CardinalGENeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-2; END CardinalGENeg2_true;
<*UNUSED*>PROCEDURE CardinalNENeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-2; END CardinalNENeg2_true;
<*UNUSED*>PROCEDURE CardinalEQNeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-2; END CardinalEQNeg2_false;


(* Longcard is always > -1, etc. (same as Cardinal) *)

<*UNUSED*>PROCEDURE LongcardLT0_false(a:LONGCARD):BOOLEAN=BEGIN RETURN a<0L; END LongcardLT0_false;
<*UNUSED*>PROCEDURE LongcardGE0_true(a:LONGCARD):BOOLEAN=BEGIN RETURN a>=0L; END LongcardGE0_true;


(* subranges with no overlap, left always less than right *)

<*UNUSED*>PROCEDURE no_overlap_less_LT_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_LT_true;
<*UNUSED*>PROCEDURE no_overlap_less_LE_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_LE_true;
<*UNUSED*>PROCEDURE no_overlap_less_GT_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_GT_false;
<*UNUSED*>PROCEDURE no_overlap_less_GE_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_GE_false;
<*UNUSED*>PROCEDURE no_overlap_less_EQ_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_EQ_false;
<*UNUSED*>PROCEDURE no_overlap_less_NE_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_NE_true;


(* subranges with overlap only on the edge, left always less or equal than right *)

<*UNUSED*>PROCEDURE minimum_overlap_less_LE_true(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_LE_true;
<*UNUSED*>PROCEDURE minimum_overlap_less_GT_false(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_GT_false;


(* subranges with no overlap, left always greater than right *)

<*UNUSED*>PROCEDURE no_overlap_greater_LT_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_LT_false;
<*UNUSED*>PROCEDURE no_overlap_greater_LE_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_LE_false;
<*UNUSED*>PROCEDURE no_overlap_greater_GT_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_GT_true;
<*UNUSED*>PROCEDURE no_overlap_greater_GE_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_GE_true;
<*UNUSED*>PROCEDURE no_overlap_greater_EQ_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_EQ_false;
<*UNUSED*>PROCEDURE no_overlap_greater_NE_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_NE_true;


(* subranges with overlap only on the edige, left always greater or equal than right *)

<*UNUSED*>PROCEDURE minimum_overlap_greater_LT_false(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_LT_false;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GE_true(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_GE_true;


(* now enums instead of integer subranges *)


TYPE Numbers = {Zero, One, Two, Three, Four};
TYPE LowNumbers = [Numbers.Zero..Numbers.One];
TYPE HighNumbers = [Numbers.Three..Numbers.Four];
TYPE MiddleNumbers = [Numbers.One..Numbers.Three];


(* enums where left is always less than right *)

<*UNUSED*>PROCEDURE no_overlap_less_enum_LT_true(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_enum_LT_true;
<*UNUSED*>PROCEDURE no_overlap_less_enum_LE_true(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_enum_LE_true;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GT_false(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_enum_GT_false;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GE_false(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_enum_GE_false;
<*UNUSED*>PROCEDURE no_overlap_less_enum_EQ_false(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_enum_EQ_false;
<*UNUSED*>PROCEDURE no_overlap_less_enum_NE_true(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_enum_NE_true;


(* enums that overlap only on the edge but otherwise the left is less than the right *)

<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LE_true(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_enum_LE_true;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GT_false(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_enum_GT_false;


(* enums where left is always greater than right *)

<*UNUSED*>PROCEDURE no_overlap_greater_enum_LT_false(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_enum_LT_false;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_LE_false(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_enum_LE_false;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GT_true(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_enum_GT_true;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GE_true(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_enum_GE_true;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_EQ_false(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_enum_EQ_false;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_NE_true(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_enum_NE_true;


(* enums that overlap only on the edge but otherwise the left is greater than the right *)

<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LT_true(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_enum_LT_true;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GE_false(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_enum_GE_false;


(* unlikely, but test the case of single element subranges *)

<*UNUSED*>PROCEDURE overlap_1_LT_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<b; END overlap_1_LT_false;
<*UNUSED*>PROCEDURE overlap_1_LE_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<=b; END overlap_1_LE_true;
<*UNUSED*>PROCEDURE overlap_1_GT_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>b; END overlap_1_GT_false;
<*UNUSED*>PROCEDURE overlap_1_GE_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>=b; END overlap_1_GE_true;
<*UNUSED*>PROCEDURE overlap_1_EQ_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a=b; END overlap_1_EQ_true;
<*UNUSED*>PROCEDURE overlap_1_NE_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a#b; END overlap_1_NE_false;


(* ORD(enum) vs. negative *)

<*UNUSED*>PROCEDURE ord_enum_vs_negative_LT_false(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)<-1; END ord_enum_vs_negative_LT_false;
<*UNUSED*>PROCEDURE ord_enum_vs_negative_LE_false(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)<=-1; END ord_enum_vs_negative_LE_false;
<*UNUSED*>PROCEDURE ord_enum_vs_negative_GT_true(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)>-1; END ord_enum_vs_negative_GT_true;
<*UNUSED*>PROCEDURE ord_enum_vs_negative_GE_true(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)>=-1; END ord_enum_vs_negative_GE_true;
<*UNUSED*>PROCEDURE ord_enum_vs_negative_EQ_false(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)=-1; END ord_enum_vs_negative_EQ_false;
<*UNUSED*>PROCEDURE ord_enum_vs_negative_NE_true(a:Numbers):BOOLEAN=BEGIN RETURN ORD(a)#-1; END ord_enum_vs_negative_NE_true;


(* ABS vs. negative *)

<*UNUSED*>PROCEDURE abs_vs_negative_LT_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<-1; END abs_vs_negative_LT_false;
<*UNUSED*>PROCEDURE abs_vs_negative_LE_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<=-1; END abs_vs_negative_LE_false;
<*UNUSED*>PROCEDURE abs_vs_negative_GT_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>-1; END abs_vs_negative_GT_true;
<*UNUSED*>PROCEDURE abs_vs_negative_GE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=-1; END abs_vs_negative_GE_true;
<*UNUSED*>PROCEDURE abs_vs_negative_EQ_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)=-1; END abs_vs_negative_EQ_false;
<*UNUSED*>PROCEDURE abs_vs_negative_NE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)#-1; END abs_vs_negative_NE_true;


(* ABS vs. 0 *)

<*UNUSED*>PROCEDURE abs_vs_zero_LT_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<0; END abs_vs_zero_LT_false;
<*UNUSED*>PROCEDURE abs_vs_zero_GE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=0; END abs_vs_zero_GE_true;


(* NEG(ABS) vs. 0 *)

<*UNUSED*>PROCEDURE neg_abs_vs_zero_LE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=0; END neg_abs_vs_zero_LE_true;
<*UNUSED*>PROCEDURE neg_abs_vs_zero_GT_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>0; END neg_abs_vs_zero_GT_false;


(* NEG(ABS) vs. 1 *)

<*UNUSED*>PROCEDURE neg_abs_vs_one_LT_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<1; END neg_abs_vs_one_LT_true;
<*UNUSED*>PROCEDURE neg_abs_vs_one_LE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=1; END neg_abs_vs_one_LE_true;
<*UNUSED*>PROCEDURE neg_abs_vs_one_GT_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>1; END neg_abs_vs_one_GT_false;
<*UNUSED*>PROCEDURE neg_abs_vs_one_GE_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>=1; END neg_abs_vs_one_GE_false;
<*UNUSED*>PROCEDURE neg_abs_vs_one_EQ_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))=1; END neg_abs_vs_one_EQ_false;
<*UNUSED*>PROCEDURE neg_abs_vs_one_NE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))#1; END neg_abs_vs_one_NE_true;

BEGIN
END Main.
