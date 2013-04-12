MODULE Main;
IMPORT RTIO;

TYPE P1 = PROCEDURE();
TYPE R1 = RECORD a := 999; END;

PROCEDURE F2(p: P1) =
BEGIN
    p();
END F2;

PROCEDURE Main(
    param_integer:INTEGER;
    param_integer_uplevel:INTEGER;
    VAR var_param_integer:INTEGER;
    VAR var_param_integer_uplevel:INTEGER;
    READONLY readonly_param_integer:INTEGER;
    READONLY readonly_param_integer_uplevel:INTEGER;
    param_record:R1;
    param_record_uplevel:R1;
    VAR var_param_record:R1;
    VAR var_param_record_uplevel:R1;
    READONLY readonly_param_record:R1;
    READONLY readonly_param_record_uplevel:R1
    ):R1 =
VAR
    local_integer:INTEGER;
    local_integer_uplevel:INTEGER;
    local_record:R1;
    local_record_uplevel:R1;
PROCEDURE F1() =
    BEGIN
        INC(param_integer_uplevel, readonly_param_integer_uplevel);
        INC(var_param_integer_uplevel, readonly_param_integer_uplevel);
        INC(param_record_uplevel.a, readonly_param_record_uplevel.a);
        INC(var_param_record_uplevel.a, readonly_param_record_uplevel.a);
        INC(local_integer_uplevel, 1);
        INC(local_record_uplevel.a, 2);
    END F1;
BEGIN
    INC(param_integer_uplevel, readonly_param_integer_uplevel);
    INC(var_param_integer_uplevel, readonly_param_integer_uplevel);
    INC(param_record_uplevel.a, readonly_param_record_uplevel.a);
    INC(var_param_record_uplevel.a, readonly_param_record_uplevel.a);
    INC(local_integer_uplevel, 1);
    INC(local_record_uplevel.a, 2);
    INC(param_integer, readonly_param_integer);
    INC(var_param_integer, readonly_param_integer);
    INC(param_record.a, readonly_param_record.a);
    INC(var_param_record.a, readonly_param_record.a);
    INC(local_integer, 1);
    INC(local_record.a, 2);
    F1();
    F2(F1);
    IF FALSE THEN RETURN R1{} END;
    IF FALSE THEN RETURN param_record END;
    IF FALSE THEN RETURN param_record_uplevel END;
    IF FALSE THEN RETURN var_param_record END;
    IF FALSE THEN RETURN var_param_record_uplevel END;
    IF FALSE THEN RETURN readonly_param_record END;
    IF FALSE THEN RETURN readonly_param_record_uplevel END;
    IF FALSE THEN RETURN local_record_uplevel END;
    IF FALSE THEN RETURN local_record END;
    RETURN R1{};
END Main;

VAR
    xparam_integer:INTEGER := 1000;
    xparam_integer_uplevel:INTEGER := 2000;
    xvar_param_integer:INTEGER := 3000;
    xvar_param_integer_uplevel:INTEGER := 4000;
    xreadonly_param_integer:INTEGER := 5000;
    xreadonly_param_integer_uplevel:INTEGER := 6000;
    xparam_record:=R1{a:=7000};
    xparam_record_uplevel:=R1{a:=8000};
    xvar_param_record:=R1{a:=9000};
    xvar_param_record_uplevel:=R1{a:=10000};
    xreadonly_param_record:=R1{a:=11000};
    xreadonly_param_record_uplevel:=R1{a:=12000};
BEGIN
    EVAL Main(
        xparam_integer,
        xparam_integer_uplevel,
        xvar_param_integer,
        xvar_param_integer_uplevel,
        xreadonly_param_integer,
        xreadonly_param_integer_uplevel,
        xparam_record,
        xparam_record_uplevel,
        xvar_param_record,
        xvar_param_record_uplevel,
        xreadonly_param_record,
        xreadonly_param_record_uplevel
        );
    RTIO.Flush();
END Main.
