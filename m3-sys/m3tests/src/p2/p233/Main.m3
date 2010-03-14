UNSAFE MODULE Main;

(* This program endeavors to exercise comparisons
 * for subranges and enums that don't overlap
 * or overlap only at the edge.
 * This should probably be in the "c for code" section.
 * What is being tested is that all of these functions
 * return just a constant TRUE or FALSE.
 *)

(* Cardinal is always >= 0, etc. *)

PROCEDURE CardinalLT0_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<0; END CardinalLT0_false;
PROCEDURE CardinalGE0_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=0; END CardinalGE0_true;


(* Cardinal is always > -1, etc. *)

PROCEDURE CardinalLTNeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-1; END CardinalLTNeg1_false;
PROCEDURE CardinalLENeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-1; END CardinalLENeg1_false;
PROCEDURE CardinalGTNeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-1; END CardinalGTNeg1_true;
PROCEDURE CardinalGENeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-1; END CardinalGENeg1_true;
PROCEDURE CardinalNENeg1_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-1; END CardinalNENeg1_true;
PROCEDURE CardinalEQNeg1_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-1; END CardinalEQNeg1_false;


(* Cardinal is always > -2, etc. (same as -1) *)

PROCEDURE CardinalLTNeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<-2; END CardinalLTNeg2_false;
PROCEDURE CardinalLENeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=-2; END CardinalLENeg2_false;
PROCEDURE CardinalGTNeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>-2; END CardinalGTNeg2_true;
PROCEDURE CardinalGENeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=-2; END CardinalGENeg2_true;
PROCEDURE CardinalNENeg2_true(a:CARDINAL):BOOLEAN=BEGIN RETURN a#-2; END CardinalNENeg2_true;
PROCEDURE CardinalEQNeg2_false(a:CARDINAL):BOOLEAN=BEGIN RETURN a=-2; END CardinalEQNeg2_false;


(* Longcard is always > -1, etc. (same as Cardinal) *)

PROCEDURE LongcardLT0_false(a:LONGCARD):BOOLEAN=BEGIN RETURN a<0L; END LongcardLT0_false;
PROCEDURE LongcardGE0_true(a:LONGCARD):BOOLEAN=BEGIN RETURN a>=0L; END LongcardGE0_true;


(* subranges with no overlap, left always less than right *)

PROCEDURE no_overlap_less_LT_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_LT_true;
PROCEDURE no_overlap_less_LE_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_LE_true;
PROCEDURE no_overlap_less_GT_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_GT_false;
PROCEDURE no_overlap_less_GE_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_GE_false;
PROCEDURE no_overlap_less_EQ_false(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_EQ_false;
PROCEDURE no_overlap_less_NE_true(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_NE_true;


(* subranges with overlap only on the edge, left always less or equal than right *)

PROCEDURE minimum_overlap_less_LE_true(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_LE_true;
PROCEDURE minimum_overlap_less_GT_false(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_GT_false;


(* subranges with no overlap, left always greater than right *)

PROCEDURE no_overlap_greater_LT_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_LT_false;
PROCEDURE no_overlap_greater_LE_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_LE_false;
PROCEDURE no_overlap_greater_GT_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_GT_true;
PROCEDURE no_overlap_greater_GE_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_GE_true;
PROCEDURE no_overlap_greater_EQ_false(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_EQ_false;
PROCEDURE no_overlap_greater_NE_true(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_NE_true;


(* subranges with overlap only on the edige, left always greater or equal than right *)

PROCEDURE minimum_overlap_greater_LT_false(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_LT_false;
PROCEDURE minimum_overlap_greater_GE_true(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_GE_true;


(* now enums instead of integer subranges *)


TYPE Number = {Zero, One, Two, Three, Four};
TYPE LowNumber = [Number.Zero..Number.One];
TYPE HighNumber = [Number.Three..Number.Four];
TYPE MiddleNumber = [Number.One..Number.Three];


(* enums where left is always less than right *)

PROCEDURE no_overlap_less_enum_LT_true(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_enum_LT_true;
PROCEDURE no_overlap_less_enum_LE_true(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_enum_LE_true;
PROCEDURE no_overlap_less_enum_GT_false(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_enum_GT_false;
PROCEDURE no_overlap_less_enum_GE_false(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_enum_GE_false;
PROCEDURE no_overlap_less_enum_EQ_false(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_enum_EQ_false;
PROCEDURE no_overlap_less_enum_NE_true(a:LowNumber; b:HighNumber):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_enum_NE_true;


(* enums that overlap only on the edge but otherwise the left is less than the right *)

PROCEDURE minimum_overlap_less_enum_LE_true(a:LowNumber; b:MiddleNumber):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_enum_LE_true;
PROCEDURE minimum_overlap_less_enum_GT_false(a:LowNumber; b:MiddleNumber):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_enum_GT_false;


(* enums where left is always greater than right *)

PROCEDURE no_overlap_greater_enum_LT_false(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_enum_LT_false;
PROCEDURE no_overlap_greater_enum_LE_false(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_enum_LE_false;
PROCEDURE no_overlap_greater_enum_GT_true(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_enum_GT_true;
PROCEDURE no_overlap_greater_enum_GE_true(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_enum_GE_true;
PROCEDURE no_overlap_greater_enum_EQ_false(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_enum_EQ_false;
PROCEDURE no_overlap_greater_enum_NE_true(a:HighNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_enum_NE_true;


(* enums that overlap only on the edge but otherwise the left is greater than the right *)

PROCEDURE minimum_overlap_greater_enum_LT_false(a:MiddleNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_enum_LT_false;
PROCEDURE minimum_overlap_greater_enum_GE_true(a:MiddleNumber; b:LowNumber):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_enum_GE_true;


(* unlikely, but test the case of single element subranges *)

PROCEDURE overlap_1_LT_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<b; END overlap_1_LT_false;
PROCEDURE overlap_1_LE_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a<=b; END overlap_1_LE_true;
PROCEDURE overlap_1_GT_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>b; END overlap_1_GT_false;
PROCEDURE overlap_1_GE_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a>=b; END overlap_1_GE_true;
PROCEDURE overlap_1_EQ_true(a,b:[0..0]):BOOLEAN=BEGIN RETURN a=b; END overlap_1_EQ_true;
PROCEDURE overlap_1_NE_false(a,b:[0..0]):BOOLEAN=BEGIN RETURN a#b; END overlap_1_NE_false;


(* ORD(enum) vs. negative *)

PROCEDURE ord_enum_vs_negative_LT_false(a:Number):BOOLEAN=BEGIN RETURN ORD(a)<-1; END ord_enum_vs_negative_LT_false;
PROCEDURE ord_enum_vs_negative_LE_false(a:Number):BOOLEAN=BEGIN RETURN ORD(a)<=-1; END ord_enum_vs_negative_LE_false;
PROCEDURE ord_enum_vs_negative_GT_true(a:Number):BOOLEAN=BEGIN RETURN ORD(a)>-1; END ord_enum_vs_negative_GT_true;
PROCEDURE ord_enum_vs_negative_GE_true(a:Number):BOOLEAN=BEGIN RETURN ORD(a)>=-1; END ord_enum_vs_negative_GE_true;
PROCEDURE ord_enum_vs_negative_EQ_false(a:Number):BOOLEAN=BEGIN RETURN ORD(a)=-1; END ord_enum_vs_negative_EQ_false;
PROCEDURE ord_enum_vs_negative_NE_true(a:Number):BOOLEAN=BEGIN RETURN ORD(a)#-1; END ord_enum_vs_negative_NE_true;


(* ABS vs. negative (these don't work) *)

PROCEDURE abs_vs_negative_LT_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<-1; END abs_vs_negative_LT_false;
PROCEDURE abs_vs_negative_LE_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<=-1; END abs_vs_negative_LE_false;
PROCEDURE abs_vs_negative_GT_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>-1; END abs_vs_negative_GT_true;
PROCEDURE abs_vs_negative_GE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=-1; END abs_vs_negative_GE_true;
PROCEDURE abs_vs_negative_EQ_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)=-1; END abs_vs_negative_EQ_false;
PROCEDURE abs_vs_negative_NE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)#-1; END abs_vs_negative_NE_true;


(* ABS vs. 0 (these don't work) *)

PROCEDURE abs_vs_zero_LT_false(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)<0; END abs_vs_zero_LT_false;
PROCEDURE abs_vs_zero_GE_true(a:INTEGER):BOOLEAN=BEGIN RETURN ABS(a)>=0; END abs_vs_zero_GE_true;


(* NEG(ABS) vs. 0 (these don't work) *)

PROCEDURE neg_abs_vs_zero_LE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=0; END neg_abs_vs_zero_LE_true;
PROCEDURE neg_abs_vs_zero_GT_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>0; END neg_abs_vs_zero_GT_false;


(* NEG(ABS) vs. 1 (these don't work) *)

PROCEDURE neg_abs_vs_one_LT_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<1; END neg_abs_vs_one_LT_true;
PROCEDURE neg_abs_vs_one_LE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))<=1; END neg_abs_vs_one_LE_true;
PROCEDURE neg_abs_vs_one_GT_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>1; END neg_abs_vs_one_GT_false;
PROCEDURE neg_abs_vs_one_GE_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))>=1; END neg_abs_vs_one_GE_false;
PROCEDURE neg_abs_vs_one_EQ_false(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))=1; END neg_abs_vs_one_EQ_false;
PROCEDURE neg_abs_vs_one_NE_true(a:INTEGER):BOOLEAN=BEGIN RETURN (-ABS(a))#1; END neg_abs_vs_one_NE_true;

BEGIN
<* ASSERT NOT CardinalLT0_false(0) *>
<* ASSERT CardinalGE0_true(0) *>
<* ASSERT NOT CardinalLTNeg1_false(0) *>
<* ASSERT NOT CardinalLENeg1_false(0) *>
<* ASSERT CardinalGTNeg1_true(0) *>
<* ASSERT CardinalGENeg1_true(0) *>
<* ASSERT CardinalNENeg1_true(0) *>
<* ASSERT NOT CardinalEQNeg1_false(0) *>
<* ASSERT NOT CardinalLTNeg2_false(0) *>
<* ASSERT NOT CardinalLENeg2_false(0) *>
<* ASSERT CardinalGTNeg2_true(0) *>
<* ASSERT CardinalGENeg2_true(0) *>
<* ASSERT CardinalNENeg2_true(0) *>
<* ASSERT NOT CardinalEQNeg2_false(0) *>
<* ASSERT NOT LongcardLT0_false(0L) *>
<* ASSERT LongcardGE0_true(0L) *>
<* ASSERT no_overlap_less_LT_true(0,2) *>
<* ASSERT no_overlap_less_LE_true(0,2) *>
<* ASSERT NOT no_overlap_less_GT_false(0,2) *>
<* ASSERT NOT no_overlap_less_GE_false(0,2) *>
<* ASSERT NOT no_overlap_less_EQ_false(0,2) *>
<* ASSERT no_overlap_less_NE_true(0,2) *>
<* ASSERT minimum_overlap_less_LE_true(0,1) *>
<* ASSERT NOT minimum_overlap_less_GT_false(0,1) *>
<* ASSERT NOT no_overlap_greater_LT_false(2,0) *>
<* ASSERT NOT no_overlap_greater_LE_false(2,0) *>
<* ASSERT no_overlap_greater_GT_true(2,0) *>
<* ASSERT no_overlap_greater_GE_true(2,0) *>
<* ASSERT NOT no_overlap_greater_EQ_false(2,0) *>
<* ASSERT no_overlap_greater_NE_true(2,0) *>
<* ASSERT NOT minimum_overlap_greater_LT_false(1,0) *>
<* ASSERT minimum_overlap_greater_GE_true(1,0) *>
<* ASSERT no_overlap_less_enum_LT_true(Number.Zero, Number.Four) *>
<* ASSERT no_overlap_less_enum_LE_true(Number.Zero, Number.Four) *>
<* ASSERT NOT no_overlap_less_enum_GT_false(Number.Zero, Number.Four) *>
<* ASSERT NOT no_overlap_less_enum_GE_false(Number.Zero, Number.Four) *>
<* ASSERT NOT no_overlap_less_enum_EQ_false(Number.Zero, Number.Four) *>
<* ASSERT no_overlap_less_enum_NE_true(Number.Zero, Number.Four) *>
<* ASSERT minimum_overlap_less_enum_LE_true(Number.Zero, Number.One) *>
<* ASSERT NOT minimum_overlap_less_enum_GT_false(Number.Zero, Number.One) *>
<* ASSERT NOT no_overlap_greater_enum_LT_false(Number.Four, Number.Zero) *>
<* ASSERT NOT no_overlap_greater_enum_LE_false(Number.Four, Number.Zero) *>
<* ASSERT no_overlap_greater_enum_GT_true(Number.Four, Number.Zero) *>
<* ASSERT no_overlap_greater_enum_GE_true(Number.Four, Number.Zero) *>
<* ASSERT NOT no_overlap_greater_enum_EQ_false(Number.Four, Number.Zero) *>
<* ASSERT no_overlap_greater_enum_NE_true(Number.Four, Number.Zero) *>
<* ASSERT NOT minimum_overlap_greater_enum_LT_false(Number.One, Number.Zero) *>
<* ASSERT minimum_overlap_greater_enum_GE_true(Number.One, Number.Zero) *>
<* ASSERT NOT overlap_1_LT_false(0,0) *>
<* ASSERT overlap_1_LE_true(0,0) *>
<* ASSERT NOT overlap_1_GT_false(0,0) *>
<* ASSERT overlap_1_GE_true(0,0) *>
<* ASSERT overlap_1_EQ_true(0,0) *>
<* ASSERT NOT overlap_1_NE_false(0,0) *>
<* ASSERT NOT ord_enum_vs_negative_LT_false(Number.Zero) *>
<* ASSERT NOT ord_enum_vs_negative_LE_false(Number.Zero) *>
<* ASSERT ord_enum_vs_negative_GT_true(Number.Zero) *>
<* ASSERT ord_enum_vs_negative_GE_true(Number.Zero) *>
<* ASSERT NOT ord_enum_vs_negative_EQ_false(Number.Zero) *>
<* ASSERT ord_enum_vs_negative_NE_true(Number.Zero) *>
<* ASSERT NOT abs_vs_negative_LT_false(0) *>
<* ASSERT NOT abs_vs_negative_LE_false(0) *>
<* ASSERT abs_vs_negative_GT_true(0) *>
<* ASSERT abs_vs_negative_GE_true(0) *>
<* ASSERT NOT abs_vs_negative_EQ_false(0) *>
<* ASSERT abs_vs_negative_NE_true(0) *>
<* ASSERT NOT abs_vs_zero_LT_false(0) *>
<* ASSERT abs_vs_zero_GE_true(0) *>
<* ASSERT neg_abs_vs_zero_LE_true(0) *>
<* ASSERT NOT neg_abs_vs_zero_GT_false(0) *>
<* ASSERT neg_abs_vs_one_LT_true(0) *>
<* ASSERT neg_abs_vs_one_LE_true(0) *>
<* ASSERT NOT neg_abs_vs_one_GT_false(0) *>
<* ASSERT NOT neg_abs_vs_one_GE_false(0) *>
<* ASSERT NOT neg_abs_vs_one_EQ_false(0) *>
<* ASSERT neg_abs_vs_one_NE_true(0) *>

END Main.
