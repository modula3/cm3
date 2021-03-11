MODULE Main;

IMPORT Cstdint, Long;

VAR vi8:Cstdint.int8_t;
VAR vu64:Cstdint.uint64_t;

BEGIN

EVAL Long.LT(vu64,VAL(vi8, LONGINT));

END Main.
