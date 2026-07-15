; ModuleID = 'Plus'
source_filename = "Plus"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define i64 @Plus__uPlus_var_i8_i8() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1176, metadata !DIExpression()), !dbg !2336
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_i8() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1177, metadata !DIExpression()), !dbg !2337
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_i8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1178, metadata !DIExpression()), !dbg !2338
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1179, metadata !DIExpression()), !dbg !2338
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1180, metadata !DIExpression()), !dbg !2338
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_i8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1181, metadata !DIExpression()), !dbg !2339
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1182, metadata !DIExpression()), !dbg !2339
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1183, metadata !DIExpression()), !dbg !2339
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_u64() personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1184, metadata !DIExpression()), !dbg !2340
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i8_u64() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1185, metadata !DIExpression()), !dbg !2341
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i8_u64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1186, metadata !DIExpression()), !dbg !2342
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1187, metadata !DIExpression()), !dbg !2342
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1188, metadata !DIExpression()), !dbg !2342
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i8_u64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1189, metadata !DIExpression()), !dbg !2343
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1190, metadata !DIExpression()), !dbg !2343
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1191, metadata !DIExpression()), !dbg !2343
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i8_i32() personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1192, metadata !DIExpression()), !dbg !2344
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_i32() personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1193, metadata !DIExpression()), !dbg !2345
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i8 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_i32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1194, metadata !DIExpression()), !dbg !2346
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1195, metadata !DIExpression()), !dbg !2346
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1196, metadata !DIExpression()), !dbg !2346
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_i32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1197, metadata !DIExpression()), !dbg !2347
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1198, metadata !DIExpression()), !dbg !2347
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1199, metadata !DIExpression()), !dbg !2347
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_LC() personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1200, metadata !DIExpression()), !dbg !2348
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i8_LC() personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1201, metadata !DIExpression()), !dbg !2349
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i8_LC(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !44 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1202, metadata !DIExpression()), !dbg !2350
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1203, metadata !DIExpression()), !dbg !2350
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1204, metadata !DIExpression()), !dbg !2350
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i8_LC(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !46 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1205, metadata !DIExpression()), !dbg !2351
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1206, metadata !DIExpression()), !dbg !2351
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1207, metadata !DIExpression()), !dbg !2351
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i8_u16() personality ptr @__gxx_personality_v0 !dbg !48 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1208, metadata !DIExpression()), !dbg !2352
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_u16() personality ptr @__gxx_personality_v0 !dbg !50 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1209, metadata !DIExpression()), !dbg !2353
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = sext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_u16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !52 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1210, metadata !DIExpression()), !dbg !2354
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1211, metadata !DIExpression()), !dbg !2354
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1212, metadata !DIExpression()), !dbg !2354
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_u16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !54 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1213, metadata !DIExpression()), !dbg !2355
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1214, metadata !DIExpression()), !dbg !2355
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1215, metadata !DIExpression()), !dbg !2355
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_I() personality ptr @__gxx_personality_v0 !dbg !56 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1216, metadata !DIExpression()), !dbg !2356
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i8 @Plus__Plus_var_i8_I() personality ptr @__gxx_personality_v0 !dbg !58 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1217, metadata !DIExpression()), !dbg !2357
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = sext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_param_i8_I(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !60 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1218, metadata !DIExpression()), !dbg !2358
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1219, metadata !DIExpression()), !dbg !2358
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1220, metadata !DIExpression()), !dbg !2358
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i8 @Plus__Plus_param_i8_I(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !62 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1221, metadata !DIExpression()), !dbg !2359
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1222, metadata !DIExpression()), !dbg !2359
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1223, metadata !DIExpression()), !dbg !2359
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_var_i8_i64() personality ptr @__gxx_personality_v0 !dbg !64 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1224, metadata !DIExpression()), !dbg !2360
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i8_i64() personality ptr @__gxx_personality_v0 !dbg !66 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1225, metadata !DIExpression()), !dbg !2361
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i8_i64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !68 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1226, metadata !DIExpression()), !dbg !2362
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1227, metadata !DIExpression()), !dbg !2362
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1228, metadata !DIExpression()), !dbg !2362
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i8_i64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !70 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1229, metadata !DIExpression()), !dbg !2363
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1230, metadata !DIExpression()), !dbg !2363
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1231, metadata !DIExpression()), !dbg !2363
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i8_i16() personality ptr @__gxx_personality_v0 !dbg !72 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1232, metadata !DIExpression()), !dbg !2364
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_i16() personality ptr @__gxx_personality_v0 !dbg !74 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1233, metadata !DIExpression()), !dbg !2365
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i8 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_i16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !76 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1234, metadata !DIExpression()), !dbg !2366
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1235, metadata !DIExpression()), !dbg !2366
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1236, metadata !DIExpression()), !dbg !2366
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_i16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !78 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1237, metadata !DIExpression()), !dbg !2367
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1238, metadata !DIExpression()), !dbg !2367
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1239, metadata !DIExpression()), !dbg !2367
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_C() personality ptr @__gxx_personality_v0 !dbg !80 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1240, metadata !DIExpression()), !dbg !2368
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i8 @Plus__Plus_var_i8_C() personality ptr @__gxx_personality_v0 !dbg !82 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1241, metadata !DIExpression()), !dbg !2369
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = sext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_param_i8_C(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !84 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1242, metadata !DIExpression()), !dbg !2370
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1243, metadata !DIExpression()), !dbg !2370
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1244, metadata !DIExpression()), !dbg !2370
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i8 @Plus__Plus_param_i8_C(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !86 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1245, metadata !DIExpression()), !dbg !2371
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1246, metadata !DIExpression()), !dbg !2371
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1247, metadata !DIExpression()), !dbg !2371
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_var_i8_u32() personality ptr @__gxx_personality_v0 !dbg !88 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1248, metadata !DIExpression()), !dbg !2372
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_u32() personality ptr @__gxx_personality_v0 !dbg !90 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1249, metadata !DIExpression()), !dbg !2373
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = sext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_u32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !92 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1250, metadata !DIExpression()), !dbg !2374
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1251, metadata !DIExpression()), !dbg !2374
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1252, metadata !DIExpression()), !dbg !2374
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_u32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !94 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1253, metadata !DIExpression()), !dbg !2375
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1254, metadata !DIExpression()), !dbg !2375
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1255, metadata !DIExpression()), !dbg !2375
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_u8() personality ptr @__gxx_personality_v0 !dbg !96 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1256, metadata !DIExpression()), !dbg !2376
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_i8_u8() personality ptr @__gxx_personality_v0 !dbg !98 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1257, metadata !DIExpression()), !dbg !2377
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = sext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_i8_u8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !100 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1258, metadata !DIExpression()), !dbg !2378
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1259, metadata !DIExpression()), !dbg !2378
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1260, metadata !DIExpression()), !dbg !2378
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_i8_u8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !102 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1261, metadata !DIExpression()), !dbg !2379
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1262, metadata !DIExpression()), !dbg !2379
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1263, metadata !DIExpression()), !dbg !2379
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_i8_L() personality ptr @__gxx_personality_v0 !dbg !104 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1264, metadata !DIExpression()), !dbg !2380
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i8_L() personality ptr @__gxx_personality_v0 !dbg !106 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1265, metadata !DIExpression()), !dbg !2381
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t2 = sext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i8_L(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !108 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1266, metadata !DIExpression()), !dbg !2382
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1267, metadata !DIExpression()), !dbg !2382
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1268, metadata !DIExpression()), !dbg !2382
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i8_L(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !110 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1269, metadata !DIExpression()), !dbg !2383
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1270, metadata !DIExpression()), !dbg !2383
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1271, metadata !DIExpression()), !dbg !2383
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_i8() personality ptr @__gxx_personality_v0 !dbg !112 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1272, metadata !DIExpression()), !dbg !2384
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_i8() personality ptr @__gxx_personality_v0 !dbg !114 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1273, metadata !DIExpression()), !dbg !2385
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !116 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1274, metadata !DIExpression()), !dbg !2386
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1275, metadata !DIExpression()), !dbg !2386
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1276, metadata !DIExpression()), !dbg !2386
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !118 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1277, metadata !DIExpression()), !dbg !2387
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1278, metadata !DIExpression()), !dbg !2387
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1279, metadata !DIExpression()), !dbg !2387
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_u64() personality ptr @__gxx_personality_v0 !dbg !120 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1280, metadata !DIExpression()), !dbg !2388
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_u64() personality ptr @__gxx_personality_v0 !dbg !122 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1281, metadata !DIExpression()), !dbg !2389
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !124 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1282, metadata !DIExpression()), !dbg !2390
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1283, metadata !DIExpression()), !dbg !2390
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1284, metadata !DIExpression()), !dbg !2390
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !126 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1285, metadata !DIExpression()), !dbg !2391
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1286, metadata !DIExpression()), !dbg !2391
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1287, metadata !DIExpression()), !dbg !2391
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u64_i32() personality ptr @__gxx_personality_v0 !dbg !128 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1288, metadata !DIExpression()), !dbg !2392
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_i32() personality ptr @__gxx_personality_v0 !dbg !130 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1289, metadata !DIExpression()), !dbg !2393
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !132 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1290, metadata !DIExpression()), !dbg !2394
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1291, metadata !DIExpression()), !dbg !2394
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1292, metadata !DIExpression()), !dbg !2394
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !134 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1293, metadata !DIExpression()), !dbg !2395
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1294, metadata !DIExpression()), !dbg !2395
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1295, metadata !DIExpression()), !dbg !2395
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_LC() personality ptr @__gxx_personality_v0 !dbg !136 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1296, metadata !DIExpression()), !dbg !2396
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_LC() personality ptr @__gxx_personality_v0 !dbg !138 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1297, metadata !DIExpression()), !dbg !2397
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !140 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1298, metadata !DIExpression()), !dbg !2398
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1299, metadata !DIExpression()), !dbg !2398
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1300, metadata !DIExpression()), !dbg !2398
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !142 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1301, metadata !DIExpression()), !dbg !2399
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1302, metadata !DIExpression()), !dbg !2399
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1303, metadata !DIExpression()), !dbg !2399
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u64_u16() personality ptr @__gxx_personality_v0 !dbg !144 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1304, metadata !DIExpression()), !dbg !2400
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_u16() personality ptr @__gxx_personality_v0 !dbg !146 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1305, metadata !DIExpression()), !dbg !2401
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !148 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1306, metadata !DIExpression()), !dbg !2402
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1307, metadata !DIExpression()), !dbg !2402
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1308, metadata !DIExpression()), !dbg !2402
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !150 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1309, metadata !DIExpression()), !dbg !2403
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1310, metadata !DIExpression()), !dbg !2403
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1311, metadata !DIExpression()), !dbg !2403
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_I() personality ptr @__gxx_personality_v0 !dbg !152 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1312, metadata !DIExpression()), !dbg !2404
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_I() personality ptr @__gxx_personality_v0 !dbg !154 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1313, metadata !DIExpression()), !dbg !2405
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !156 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1314, metadata !DIExpression()), !dbg !2406
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1315, metadata !DIExpression()), !dbg !2406
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1316, metadata !DIExpression()), !dbg !2406
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !158 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1317, metadata !DIExpression()), !dbg !2407
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1318, metadata !DIExpression()), !dbg !2407
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1319, metadata !DIExpression()), !dbg !2407
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u64_i64() personality ptr @__gxx_personality_v0 !dbg !160 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1320, metadata !DIExpression()), !dbg !2408
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_i64() personality ptr @__gxx_personality_v0 !dbg !162 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1321, metadata !DIExpression()), !dbg !2409
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !164 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1322, metadata !DIExpression()), !dbg !2410
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1323, metadata !DIExpression()), !dbg !2410
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1324, metadata !DIExpression()), !dbg !2410
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !166 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1325, metadata !DIExpression()), !dbg !2411
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1326, metadata !DIExpression()), !dbg !2411
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1327, metadata !DIExpression()), !dbg !2411
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u64_i16() personality ptr @__gxx_personality_v0 !dbg !168 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1328, metadata !DIExpression()), !dbg !2412
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_i16() personality ptr @__gxx_personality_v0 !dbg !170 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1329, metadata !DIExpression()), !dbg !2413
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !172 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1330, metadata !DIExpression()), !dbg !2414
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1331, metadata !DIExpression()), !dbg !2414
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1332, metadata !DIExpression()), !dbg !2414
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !174 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1333, metadata !DIExpression()), !dbg !2415
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1334, metadata !DIExpression()), !dbg !2415
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1335, metadata !DIExpression()), !dbg !2415
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_C() personality ptr @__gxx_personality_v0 !dbg !176 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1336, metadata !DIExpression()), !dbg !2416
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_C() personality ptr @__gxx_personality_v0 !dbg !178 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1337, metadata !DIExpression()), !dbg !2417
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !180 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1338, metadata !DIExpression()), !dbg !2418
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1339, metadata !DIExpression()), !dbg !2418
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1340, metadata !DIExpression()), !dbg !2418
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !182 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1341, metadata !DIExpression()), !dbg !2419
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1342, metadata !DIExpression()), !dbg !2419
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1343, metadata !DIExpression()), !dbg !2419
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u64_u32() personality ptr @__gxx_personality_v0 !dbg !184 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1344, metadata !DIExpression()), !dbg !2420
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_u32() personality ptr @__gxx_personality_v0 !dbg !186 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1345, metadata !DIExpression()), !dbg !2421
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !188 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1346, metadata !DIExpression()), !dbg !2422
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1347, metadata !DIExpression()), !dbg !2422
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1348, metadata !DIExpression()), !dbg !2422
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !190 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1349, metadata !DIExpression()), !dbg !2423
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1350, metadata !DIExpression()), !dbg !2423
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1351, metadata !DIExpression()), !dbg !2423
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_u8() personality ptr @__gxx_personality_v0 !dbg !192 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1352, metadata !DIExpression()), !dbg !2424
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u64_u8() personality ptr @__gxx_personality_v0 !dbg !194 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1353, metadata !DIExpression()), !dbg !2425
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u64_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !196 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1354, metadata !DIExpression()), !dbg !2426
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1355, metadata !DIExpression()), !dbg !2426
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1356, metadata !DIExpression()), !dbg !2426
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u64_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !198 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1357, metadata !DIExpression()), !dbg !2427
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1358, metadata !DIExpression()), !dbg !2427
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1359, metadata !DIExpression()), !dbg !2427
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u64_L() personality ptr @__gxx_personality_v0 !dbg !200 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1360, metadata !DIExpression()), !dbg !2428
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_u64_L() personality ptr @__gxx_personality_v0 !dbg !202 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1361, metadata !DIExpression()), !dbg !2429
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_u64_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !204 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1362, metadata !DIExpression()), !dbg !2430
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1363, metadata !DIExpression()), !dbg !2430
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1364, metadata !DIExpression()), !dbg !2430
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_u64_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !206 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1365, metadata !DIExpression()), !dbg !2431
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1366, metadata !DIExpression()), !dbg !2431
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1367, metadata !DIExpression()), !dbg !2431
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define double @Plus__Plus_var_f64_f64() personality ptr @__gxx_personality_v0 !dbg !208 {
entry:
  %_result.slot = alloca double
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1368, metadata !DIExpression()), !dbg !2432
  %t1 = load double, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 120)
  %t2 = load double, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 120)
  %t3 = fadd double %t1, %t2
  ret double %t3
}

define double @Plus__Plus_param_f64_f64(double %a.a, double %a.b) personality ptr @__gxx_personality_v0 !dbg !210 {
entry:
  %_result.slot = alloca double
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1369, metadata !DIExpression()), !dbg !2433
  %b.slot = alloca double
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1370, metadata !DIExpression()), !dbg !2433
  %a.slot = alloca double
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1371, metadata !DIExpression()), !dbg !2433
  store double %a.a, ptr %a.slot
  store double %a.b, ptr %b.slot
  %t1 = load double, ptr %a.slot
  %t2 = load double, ptr %b.slot
  %t3 = fadd double %t1, %t2
  ret double %t3
}

define i64 @Plus__uPlus_var_i32_i8() personality ptr @__gxx_personality_v0 !dbg !212 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1372, metadata !DIExpression()), !dbg !2434
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_i8() personality ptr @__gxx_personality_v0 !dbg !214 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1373, metadata !DIExpression()), !dbg !2435
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i32 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_i8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !216 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1374, metadata !DIExpression()), !dbg !2436
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1375, metadata !DIExpression()), !dbg !2436
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1376, metadata !DIExpression()), !dbg !2436
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_i8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !218 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1377, metadata !DIExpression()), !dbg !2437
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1378, metadata !DIExpression()), !dbg !2437
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1379, metadata !DIExpression()), !dbg !2437
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_u64() personality ptr @__gxx_personality_v0 !dbg !220 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1380, metadata !DIExpression()), !dbg !2438
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i32_u64() personality ptr @__gxx_personality_v0 !dbg !222 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1381, metadata !DIExpression()), !dbg !2439
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i32_u64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !224 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1382, metadata !DIExpression()), !dbg !2440
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1383, metadata !DIExpression()), !dbg !2440
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1384, metadata !DIExpression()), !dbg !2440
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i32_u64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !226 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1385, metadata !DIExpression()), !dbg !2441
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1386, metadata !DIExpression()), !dbg !2441
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1387, metadata !DIExpression()), !dbg !2441
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i32_i32() personality ptr @__gxx_personality_v0 !dbg !228 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1388, metadata !DIExpression()), !dbg !2442
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_i32() personality ptr @__gxx_personality_v0 !dbg !230 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1389, metadata !DIExpression()), !dbg !2443
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_i32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !232 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1390, metadata !DIExpression()), !dbg !2444
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1391, metadata !DIExpression()), !dbg !2444
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1392, metadata !DIExpression()), !dbg !2444
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_i32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !234 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1393, metadata !DIExpression()), !dbg !2445
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1394, metadata !DIExpression()), !dbg !2445
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1395, metadata !DIExpression()), !dbg !2445
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_LC() personality ptr @__gxx_personality_v0 !dbg !236 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1396, metadata !DIExpression()), !dbg !2446
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i32_LC() personality ptr @__gxx_personality_v0 !dbg !238 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1397, metadata !DIExpression()), !dbg !2447
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i32_LC(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !240 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1398, metadata !DIExpression()), !dbg !2448
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1399, metadata !DIExpression()), !dbg !2448
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1400, metadata !DIExpression()), !dbg !2448
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i32_LC(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !242 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1401, metadata !DIExpression()), !dbg !2449
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1402, metadata !DIExpression()), !dbg !2449
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1403, metadata !DIExpression()), !dbg !2449
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i32_u16() personality ptr @__gxx_personality_v0 !dbg !244 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1404, metadata !DIExpression()), !dbg !2450
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_u16() personality ptr @__gxx_personality_v0 !dbg !246 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1405, metadata !DIExpression()), !dbg !2451
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = sext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_u16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !248 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1406, metadata !DIExpression()), !dbg !2452
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1407, metadata !DIExpression()), !dbg !2452
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1408, metadata !DIExpression()), !dbg !2452
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_u16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !250 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1409, metadata !DIExpression()), !dbg !2453
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1410, metadata !DIExpression()), !dbg !2453
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1411, metadata !DIExpression()), !dbg !2453
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_I() personality ptr @__gxx_personality_v0 !dbg !252 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1412, metadata !DIExpression()), !dbg !2454
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i32 @Plus__Plus_var_i32_I() personality ptr @__gxx_personality_v0 !dbg !254 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1413, metadata !DIExpression()), !dbg !2455
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = sext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_param_i32_I(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !256 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1414, metadata !DIExpression()), !dbg !2456
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1415, metadata !DIExpression()), !dbg !2456
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1416, metadata !DIExpression()), !dbg !2456
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i32 @Plus__Plus_param_i32_I(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !258 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1417, metadata !DIExpression()), !dbg !2457
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1418, metadata !DIExpression()), !dbg !2457
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1419, metadata !DIExpression()), !dbg !2457
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_var_i32_i64() personality ptr @__gxx_personality_v0 !dbg !260 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1420, metadata !DIExpression()), !dbg !2458
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i32_i64() personality ptr @__gxx_personality_v0 !dbg !262 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1421, metadata !DIExpression()), !dbg !2459
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i32_i64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !264 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1422, metadata !DIExpression()), !dbg !2460
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1423, metadata !DIExpression()), !dbg !2460
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1424, metadata !DIExpression()), !dbg !2460
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i32_i64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !266 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1425, metadata !DIExpression()), !dbg !2461
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1426, metadata !DIExpression()), !dbg !2461
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1427, metadata !DIExpression()), !dbg !2461
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i32_i16() personality ptr @__gxx_personality_v0 !dbg !268 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1428, metadata !DIExpression()), !dbg !2462
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_i16() personality ptr @__gxx_personality_v0 !dbg !270 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1429, metadata !DIExpression()), !dbg !2463
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i32 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_i16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !272 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1430, metadata !DIExpression()), !dbg !2464
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1431, metadata !DIExpression()), !dbg !2464
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1432, metadata !DIExpression()), !dbg !2464
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_i16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !274 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1433, metadata !DIExpression()), !dbg !2465
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1434, metadata !DIExpression()), !dbg !2465
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1435, metadata !DIExpression()), !dbg !2465
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_C() personality ptr @__gxx_personality_v0 !dbg !276 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1436, metadata !DIExpression()), !dbg !2466
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i32 @Plus__Plus_var_i32_C() personality ptr @__gxx_personality_v0 !dbg !278 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1437, metadata !DIExpression()), !dbg !2467
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = sext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_param_i32_C(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !280 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1438, metadata !DIExpression()), !dbg !2468
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1439, metadata !DIExpression()), !dbg !2468
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1440, metadata !DIExpression()), !dbg !2468
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i32 @Plus__Plus_param_i32_C(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !282 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1441, metadata !DIExpression()), !dbg !2469
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1442, metadata !DIExpression()), !dbg !2469
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1443, metadata !DIExpression()), !dbg !2469
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_var_i32_u32() personality ptr @__gxx_personality_v0 !dbg !284 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1444, metadata !DIExpression()), !dbg !2470
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_u32() personality ptr @__gxx_personality_v0 !dbg !286 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1445, metadata !DIExpression()), !dbg !2471
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = sext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_u32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !288 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1446, metadata !DIExpression()), !dbg !2472
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1447, metadata !DIExpression()), !dbg !2472
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1448, metadata !DIExpression()), !dbg !2472
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_u32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !290 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1449, metadata !DIExpression()), !dbg !2473
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1450, metadata !DIExpression()), !dbg !2473
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1451, metadata !DIExpression()), !dbg !2473
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_u8() personality ptr @__gxx_personality_v0 !dbg !292 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1452, metadata !DIExpression()), !dbg !2474
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_i32_u8() personality ptr @__gxx_personality_v0 !dbg !294 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1453, metadata !DIExpression()), !dbg !2475
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = sext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_i32_u8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !296 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1454, metadata !DIExpression()), !dbg !2476
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1455, metadata !DIExpression()), !dbg !2476
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1456, metadata !DIExpression()), !dbg !2476
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_i32_u8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !298 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1457, metadata !DIExpression()), !dbg !2477
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1458, metadata !DIExpression()), !dbg !2477
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1459, metadata !DIExpression()), !dbg !2477
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_i32_L() personality ptr @__gxx_personality_v0 !dbg !300 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1460, metadata !DIExpression()), !dbg !2478
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i32_L() personality ptr @__gxx_personality_v0 !dbg !302 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1461, metadata !DIExpression()), !dbg !2479
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t2 = sext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i32_L(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !304 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1462, metadata !DIExpression()), !dbg !2480
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1463, metadata !DIExpression()), !dbg !2480
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1464, metadata !DIExpression()), !dbg !2480
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i32_L(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !306 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1465, metadata !DIExpression()), !dbg !2481
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1466, metadata !DIExpression()), !dbg !2481
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1467, metadata !DIExpression()), !dbg !2481
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_i8() personality ptr @__gxx_personality_v0 !dbg !308 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1468, metadata !DIExpression()), !dbg !2482
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_i8() personality ptr @__gxx_personality_v0 !dbg !310 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1469, metadata !DIExpression()), !dbg !2483
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !312 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1470, metadata !DIExpression()), !dbg !2484
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1471, metadata !DIExpression()), !dbg !2484
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1472, metadata !DIExpression()), !dbg !2484
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !314 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1473, metadata !DIExpression()), !dbg !2485
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1474, metadata !DIExpression()), !dbg !2485
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1475, metadata !DIExpression()), !dbg !2485
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_u64() personality ptr @__gxx_personality_v0 !dbg !316 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1476, metadata !DIExpression()), !dbg !2486
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_u64() personality ptr @__gxx_personality_v0 !dbg !318 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1477, metadata !DIExpression()), !dbg !2487
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !320 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1478, metadata !DIExpression()), !dbg !2488
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1479, metadata !DIExpression()), !dbg !2488
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1480, metadata !DIExpression()), !dbg !2488
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !322 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1481, metadata !DIExpression()), !dbg !2489
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1482, metadata !DIExpression()), !dbg !2489
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1483, metadata !DIExpression()), !dbg !2489
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_LC_i32() personality ptr @__gxx_personality_v0 !dbg !324 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1484, metadata !DIExpression()), !dbg !2490
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_i32() personality ptr @__gxx_personality_v0 !dbg !326 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1485, metadata !DIExpression()), !dbg !2491
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !328 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1486, metadata !DIExpression()), !dbg !2492
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1487, metadata !DIExpression()), !dbg !2492
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1488, metadata !DIExpression()), !dbg !2492
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !330 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1489, metadata !DIExpression()), !dbg !2493
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1490, metadata !DIExpression()), !dbg !2493
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1491, metadata !DIExpression()), !dbg !2493
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_LC() personality ptr @__gxx_personality_v0 !dbg !332 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1492, metadata !DIExpression()), !dbg !2494
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_LC() personality ptr @__gxx_personality_v0 !dbg !334 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1493, metadata !DIExpression()), !dbg !2495
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !336 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1494, metadata !DIExpression()), !dbg !2496
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1495, metadata !DIExpression()), !dbg !2496
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1496, metadata !DIExpression()), !dbg !2496
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !338 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1497, metadata !DIExpression()), !dbg !2497
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1498, metadata !DIExpression()), !dbg !2497
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1499, metadata !DIExpression()), !dbg !2497
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_LC_u16() personality ptr @__gxx_personality_v0 !dbg !340 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1500, metadata !DIExpression()), !dbg !2498
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_u16() personality ptr @__gxx_personality_v0 !dbg !342 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1501, metadata !DIExpression()), !dbg !2499
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !344 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1502, metadata !DIExpression()), !dbg !2500
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1503, metadata !DIExpression()), !dbg !2500
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1504, metadata !DIExpression()), !dbg !2500
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !346 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1505, metadata !DIExpression()), !dbg !2501
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1506, metadata !DIExpression()), !dbg !2501
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1507, metadata !DIExpression()), !dbg !2501
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_I() personality ptr @__gxx_personality_v0 !dbg !348 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1508, metadata !DIExpression()), !dbg !2502
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_I() personality ptr @__gxx_personality_v0 !dbg !350 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1509, metadata !DIExpression()), !dbg !2503
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !352 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1510, metadata !DIExpression()), !dbg !2504
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1511, metadata !DIExpression()), !dbg !2504
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1512, metadata !DIExpression()), !dbg !2504
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !354 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1513, metadata !DIExpression()), !dbg !2505
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1514, metadata !DIExpression()), !dbg !2505
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1515, metadata !DIExpression()), !dbg !2505
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_LC_i64() personality ptr @__gxx_personality_v0 !dbg !356 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1516, metadata !DIExpression()), !dbg !2506
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_i64() personality ptr @__gxx_personality_v0 !dbg !358 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1517, metadata !DIExpression()), !dbg !2507
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !360 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1518, metadata !DIExpression()), !dbg !2508
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1519, metadata !DIExpression()), !dbg !2508
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1520, metadata !DIExpression()), !dbg !2508
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !362 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1521, metadata !DIExpression()), !dbg !2509
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1522, metadata !DIExpression()), !dbg !2509
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1523, metadata !DIExpression()), !dbg !2509
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_LC_i16() personality ptr @__gxx_personality_v0 !dbg !364 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1524, metadata !DIExpression()), !dbg !2510
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_i16() personality ptr @__gxx_personality_v0 !dbg !366 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1525, metadata !DIExpression()), !dbg !2511
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !368 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1526, metadata !DIExpression()), !dbg !2512
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1527, metadata !DIExpression()), !dbg !2512
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1528, metadata !DIExpression()), !dbg !2512
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !370 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1529, metadata !DIExpression()), !dbg !2513
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1530, metadata !DIExpression()), !dbg !2513
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1531, metadata !DIExpression()), !dbg !2513
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_C() personality ptr @__gxx_personality_v0 !dbg !372 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1532, metadata !DIExpression()), !dbg !2514
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_C() personality ptr @__gxx_personality_v0 !dbg !374 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1533, metadata !DIExpression()), !dbg !2515
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !376 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1534, metadata !DIExpression()), !dbg !2516
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1535, metadata !DIExpression()), !dbg !2516
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1536, metadata !DIExpression()), !dbg !2516
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !378 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1537, metadata !DIExpression()), !dbg !2517
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1538, metadata !DIExpression()), !dbg !2517
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1539, metadata !DIExpression()), !dbg !2517
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_LC_u32() personality ptr @__gxx_personality_v0 !dbg !380 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1540, metadata !DIExpression()), !dbg !2518
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_u32() personality ptr @__gxx_personality_v0 !dbg !382 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1541, metadata !DIExpression()), !dbg !2519
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !384 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1542, metadata !DIExpression()), !dbg !2520
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1543, metadata !DIExpression()), !dbg !2520
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1544, metadata !DIExpression()), !dbg !2520
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !386 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1545, metadata !DIExpression()), !dbg !2521
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1546, metadata !DIExpression()), !dbg !2521
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1547, metadata !DIExpression()), !dbg !2521
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_u8() personality ptr @__gxx_personality_v0 !dbg !388 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1548, metadata !DIExpression()), !dbg !2522
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_LC_u8() personality ptr @__gxx_personality_v0 !dbg !390 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1549, metadata !DIExpression()), !dbg !2523
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_LC_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !392 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1550, metadata !DIExpression()), !dbg !2524
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1551, metadata !DIExpression()), !dbg !2524
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1552, metadata !DIExpression()), !dbg !2524
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_LC_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !394 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1553, metadata !DIExpression()), !dbg !2525
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1554, metadata !DIExpression()), !dbg !2525
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1555, metadata !DIExpression()), !dbg !2525
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_LC_L() personality ptr @__gxx_personality_v0 !dbg !396 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1556, metadata !DIExpression()), !dbg !2526
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_LC_L() personality ptr @__gxx_personality_v0 !dbg !398 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1557, metadata !DIExpression()), !dbg !2527
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_LC_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !400 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1558, metadata !DIExpression()), !dbg !2528
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1559, metadata !DIExpression()), !dbg !2528
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1560, metadata !DIExpression()), !dbg !2528
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_LC_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !402 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1561, metadata !DIExpression()), !dbg !2529
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1562, metadata !DIExpression()), !dbg !2529
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1563, metadata !DIExpression()), !dbg !2529
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u16_i8() personality ptr @__gxx_personality_v0 !dbg !404 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1564, metadata !DIExpression()), !dbg !2530
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_i8() personality ptr @__gxx_personality_v0 !dbg !406 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1565, metadata !DIExpression()), !dbg !2531
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i16 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_i8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !408 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1566, metadata !DIExpression()), !dbg !2532
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1567, metadata !DIExpression()), !dbg !2532
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1568, metadata !DIExpression()), !dbg !2532
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_i8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !410 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1569, metadata !DIExpression()), !dbg !2533
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1570, metadata !DIExpression()), !dbg !2533
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1571, metadata !DIExpression()), !dbg !2533
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_u64() personality ptr @__gxx_personality_v0 !dbg !412 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1572, metadata !DIExpression()), !dbg !2534
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u16_u64() personality ptr @__gxx_personality_v0 !dbg !414 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1573, metadata !DIExpression()), !dbg !2535
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u16_u64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !416 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1574, metadata !DIExpression()), !dbg !2536
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1575, metadata !DIExpression()), !dbg !2536
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1576, metadata !DIExpression()), !dbg !2536
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u16_u64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !418 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1577, metadata !DIExpression()), !dbg !2537
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1578, metadata !DIExpression()), !dbg !2537
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1579, metadata !DIExpression()), !dbg !2537
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u16_i32() personality ptr @__gxx_personality_v0 !dbg !420 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1580, metadata !DIExpression()), !dbg !2538
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_i32() personality ptr @__gxx_personality_v0 !dbg !422 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1581, metadata !DIExpression()), !dbg !2539
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i16 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_i32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !424 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1582, metadata !DIExpression()), !dbg !2540
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1583, metadata !DIExpression()), !dbg !2540
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1584, metadata !DIExpression()), !dbg !2540
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_i32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !426 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1585, metadata !DIExpression()), !dbg !2541
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1586, metadata !DIExpression()), !dbg !2541
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1587, metadata !DIExpression()), !dbg !2541
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_LC() personality ptr @__gxx_personality_v0 !dbg !428 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1588, metadata !DIExpression()), !dbg !2542
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u16_LC() personality ptr @__gxx_personality_v0 !dbg !430 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1589, metadata !DIExpression()), !dbg !2543
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u16_LC(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !432 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1590, metadata !DIExpression()), !dbg !2544
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1591, metadata !DIExpression()), !dbg !2544
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1592, metadata !DIExpression()), !dbg !2544
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u16_LC(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !434 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1593, metadata !DIExpression()), !dbg !2545
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1594, metadata !DIExpression()), !dbg !2545
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1595, metadata !DIExpression()), !dbg !2545
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u16_u16() personality ptr @__gxx_personality_v0 !dbg !436 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1596, metadata !DIExpression()), !dbg !2546
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_u16() personality ptr @__gxx_personality_v0 !dbg !438 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1597, metadata !DIExpression()), !dbg !2547
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_u16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !440 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1598, metadata !DIExpression()), !dbg !2548
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1599, metadata !DIExpression()), !dbg !2548
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1600, metadata !DIExpression()), !dbg !2548
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_u16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !442 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1601, metadata !DIExpression()), !dbg !2549
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1602, metadata !DIExpression()), !dbg !2549
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1603, metadata !DIExpression()), !dbg !2549
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_I() personality ptr @__gxx_personality_v0 !dbg !444 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1604, metadata !DIExpression()), !dbg !2550
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i16 @Plus__Plus_var_u16_I() personality ptr @__gxx_personality_v0 !dbg !446 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1605, metadata !DIExpression()), !dbg !2551
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_param_u16_I(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !448 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1606, metadata !DIExpression()), !dbg !2552
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1607, metadata !DIExpression()), !dbg !2552
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1608, metadata !DIExpression()), !dbg !2552
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i16 @Plus__Plus_param_u16_I(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !450 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1609, metadata !DIExpression()), !dbg !2553
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1610, metadata !DIExpression()), !dbg !2553
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1611, metadata !DIExpression()), !dbg !2553
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_var_u16_i64() personality ptr @__gxx_personality_v0 !dbg !452 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1612, metadata !DIExpression()), !dbg !2554
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u16_i64() personality ptr @__gxx_personality_v0 !dbg !454 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1613, metadata !DIExpression()), !dbg !2555
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u16_i64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !456 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1614, metadata !DIExpression()), !dbg !2556
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1615, metadata !DIExpression()), !dbg !2556
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1616, metadata !DIExpression()), !dbg !2556
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u16_i64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !458 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1617, metadata !DIExpression()), !dbg !2557
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1618, metadata !DIExpression()), !dbg !2557
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1619, metadata !DIExpression()), !dbg !2557
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u16_i16() personality ptr @__gxx_personality_v0 !dbg !460 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1620, metadata !DIExpression()), !dbg !2558
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_i16() personality ptr @__gxx_personality_v0 !dbg !462 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1621, metadata !DIExpression()), !dbg !2559
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i16 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_i16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !464 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1622, metadata !DIExpression()), !dbg !2560
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1623, metadata !DIExpression()), !dbg !2560
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1624, metadata !DIExpression()), !dbg !2560
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_i16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !466 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1625, metadata !DIExpression()), !dbg !2561
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1626, metadata !DIExpression()), !dbg !2561
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1627, metadata !DIExpression()), !dbg !2561
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_C() personality ptr @__gxx_personality_v0 !dbg !468 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1628, metadata !DIExpression()), !dbg !2562
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i16 @Plus__Plus_var_u16_C() personality ptr @__gxx_personality_v0 !dbg !470 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1629, metadata !DIExpression()), !dbg !2563
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_param_u16_C(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !472 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1630, metadata !DIExpression()), !dbg !2564
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1631, metadata !DIExpression()), !dbg !2564
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1632, metadata !DIExpression()), !dbg !2564
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i16 @Plus__Plus_param_u16_C(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !474 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1633, metadata !DIExpression()), !dbg !2565
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1634, metadata !DIExpression()), !dbg !2565
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1635, metadata !DIExpression()), !dbg !2565
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_var_u16_u32() personality ptr @__gxx_personality_v0 !dbg !476 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1636, metadata !DIExpression()), !dbg !2566
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_u32() personality ptr @__gxx_personality_v0 !dbg !478 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1637, metadata !DIExpression()), !dbg !2567
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_u32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !480 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1638, metadata !DIExpression()), !dbg !2568
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1639, metadata !DIExpression()), !dbg !2568
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1640, metadata !DIExpression()), !dbg !2568
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_u32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !482 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1641, metadata !DIExpression()), !dbg !2569
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1642, metadata !DIExpression()), !dbg !2569
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1643, metadata !DIExpression()), !dbg !2569
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_u8() personality ptr @__gxx_personality_v0 !dbg !484 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1644, metadata !DIExpression()), !dbg !2570
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_u16_u8() personality ptr @__gxx_personality_v0 !dbg !486 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1645, metadata !DIExpression()), !dbg !2571
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_u16_u8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !488 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1646, metadata !DIExpression()), !dbg !2572
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1647, metadata !DIExpression()), !dbg !2572
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1648, metadata !DIExpression()), !dbg !2572
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_u16_u8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !490 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1649, metadata !DIExpression()), !dbg !2573
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1650, metadata !DIExpression()), !dbg !2573
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1651, metadata !DIExpression()), !dbg !2573
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_u16_L() personality ptr @__gxx_personality_v0 !dbg !492 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1652, metadata !DIExpression()), !dbg !2574
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u16_L() personality ptr @__gxx_personality_v0 !dbg !494 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1653, metadata !DIExpression()), !dbg !2575
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t2 = zext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u16_L(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !496 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1654, metadata !DIExpression()), !dbg !2576
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1655, metadata !DIExpression()), !dbg !2576
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1656, metadata !DIExpression()), !dbg !2576
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u16_L(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !498 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1657, metadata !DIExpression()), !dbg !2577
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1658, metadata !DIExpression()), !dbg !2577
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1659, metadata !DIExpression()), !dbg !2577
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_i8() personality ptr @__gxx_personality_v0 !dbg !500 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1660, metadata !DIExpression()), !dbg !2578
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_i8() personality ptr @__gxx_personality_v0 !dbg !502 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1661, metadata !DIExpression()), !dbg !2579
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !504 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1662, metadata !DIExpression()), !dbg !2580
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1663, metadata !DIExpression()), !dbg !2580
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1664, metadata !DIExpression()), !dbg !2580
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !506 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1665, metadata !DIExpression()), !dbg !2581
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1666, metadata !DIExpression()), !dbg !2581
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1667, metadata !DIExpression()), !dbg !2581
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_u64() personality ptr @__gxx_personality_v0 !dbg !508 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1668, metadata !DIExpression()), !dbg !2582
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_u64() personality ptr @__gxx_personality_v0 !dbg !510 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1669, metadata !DIExpression()), !dbg !2583
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !512 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1670, metadata !DIExpression()), !dbg !2584
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1671, metadata !DIExpression()), !dbg !2584
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1672, metadata !DIExpression()), !dbg !2584
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !514 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1673, metadata !DIExpression()), !dbg !2585
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1674, metadata !DIExpression()), !dbg !2585
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1675, metadata !DIExpression()), !dbg !2585
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_I_i32() personality ptr @__gxx_personality_v0 !dbg !516 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1676, metadata !DIExpression()), !dbg !2586
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_i32() personality ptr @__gxx_personality_v0 !dbg !518 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1677, metadata !DIExpression()), !dbg !2587
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !520 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1678, metadata !DIExpression()), !dbg !2588
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1679, metadata !DIExpression()), !dbg !2588
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1680, metadata !DIExpression()), !dbg !2588
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !522 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1681, metadata !DIExpression()), !dbg !2589
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1682, metadata !DIExpression()), !dbg !2589
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1683, metadata !DIExpression()), !dbg !2589
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_LC() personality ptr @__gxx_personality_v0 !dbg !524 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1684, metadata !DIExpression()), !dbg !2590
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_LC() personality ptr @__gxx_personality_v0 !dbg !526 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1685, metadata !DIExpression()), !dbg !2591
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !528 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1686, metadata !DIExpression()), !dbg !2592
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1687, metadata !DIExpression()), !dbg !2592
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1688, metadata !DIExpression()), !dbg !2592
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !530 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1689, metadata !DIExpression()), !dbg !2593
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1690, metadata !DIExpression()), !dbg !2593
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1691, metadata !DIExpression()), !dbg !2593
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_I_u16() personality ptr @__gxx_personality_v0 !dbg !532 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1692, metadata !DIExpression()), !dbg !2594
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_u16() personality ptr @__gxx_personality_v0 !dbg !534 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1693, metadata !DIExpression()), !dbg !2595
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !536 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1694, metadata !DIExpression()), !dbg !2596
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1695, metadata !DIExpression()), !dbg !2596
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1696, metadata !DIExpression()), !dbg !2596
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !538 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1697, metadata !DIExpression()), !dbg !2597
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1698, metadata !DIExpression()), !dbg !2597
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1699, metadata !DIExpression()), !dbg !2597
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_I() personality ptr @__gxx_personality_v0 !dbg !540 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1700, metadata !DIExpression()), !dbg !2598
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_I() personality ptr @__gxx_personality_v0 !dbg !542 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1701, metadata !DIExpression()), !dbg !2599
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !544 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1702, metadata !DIExpression()), !dbg !2600
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1703, metadata !DIExpression()), !dbg !2600
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1704, metadata !DIExpression()), !dbg !2600
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !546 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1705, metadata !DIExpression()), !dbg !2601
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1706, metadata !DIExpression()), !dbg !2601
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1707, metadata !DIExpression()), !dbg !2601
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_I_i64() personality ptr @__gxx_personality_v0 !dbg !548 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1708, metadata !DIExpression()), !dbg !2602
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_i64() personality ptr @__gxx_personality_v0 !dbg !550 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1709, metadata !DIExpression()), !dbg !2603
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !552 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1710, metadata !DIExpression()), !dbg !2604
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1711, metadata !DIExpression()), !dbg !2604
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1712, metadata !DIExpression()), !dbg !2604
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !554 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1713, metadata !DIExpression()), !dbg !2605
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1714, metadata !DIExpression()), !dbg !2605
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1715, metadata !DIExpression()), !dbg !2605
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_I_i16() personality ptr @__gxx_personality_v0 !dbg !556 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1716, metadata !DIExpression()), !dbg !2606
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_i16() personality ptr @__gxx_personality_v0 !dbg !558 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1717, metadata !DIExpression()), !dbg !2607
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !560 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1718, metadata !DIExpression()), !dbg !2608
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1719, metadata !DIExpression()), !dbg !2608
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1720, metadata !DIExpression()), !dbg !2608
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !562 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1721, metadata !DIExpression()), !dbg !2609
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1722, metadata !DIExpression()), !dbg !2609
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1723, metadata !DIExpression()), !dbg !2609
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_C() personality ptr @__gxx_personality_v0 !dbg !564 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1724, metadata !DIExpression()), !dbg !2610
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_C() personality ptr @__gxx_personality_v0 !dbg !566 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1725, metadata !DIExpression()), !dbg !2611
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !568 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1726, metadata !DIExpression()), !dbg !2612
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1727, metadata !DIExpression()), !dbg !2612
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1728, metadata !DIExpression()), !dbg !2612
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !570 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1729, metadata !DIExpression()), !dbg !2613
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1730, metadata !DIExpression()), !dbg !2613
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1731, metadata !DIExpression()), !dbg !2613
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_I_u32() personality ptr @__gxx_personality_v0 !dbg !572 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1732, metadata !DIExpression()), !dbg !2614
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_u32() personality ptr @__gxx_personality_v0 !dbg !574 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1733, metadata !DIExpression()), !dbg !2615
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !576 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1734, metadata !DIExpression()), !dbg !2616
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1735, metadata !DIExpression()), !dbg !2616
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1736, metadata !DIExpression()), !dbg !2616
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !578 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1737, metadata !DIExpression()), !dbg !2617
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1738, metadata !DIExpression()), !dbg !2617
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1739, metadata !DIExpression()), !dbg !2617
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_u8() personality ptr @__gxx_personality_v0 !dbg !580 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1740, metadata !DIExpression()), !dbg !2618
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_I_u8() personality ptr @__gxx_personality_v0 !dbg !582 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1741, metadata !DIExpression()), !dbg !2619
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_I_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !584 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1742, metadata !DIExpression()), !dbg !2620
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1743, metadata !DIExpression()), !dbg !2620
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1744, metadata !DIExpression()), !dbg !2620
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_I_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !586 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1745, metadata !DIExpression()), !dbg !2621
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1746, metadata !DIExpression()), !dbg !2621
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1747, metadata !DIExpression()), !dbg !2621
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_I_L() personality ptr @__gxx_personality_v0 !dbg !588 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1748, metadata !DIExpression()), !dbg !2622
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_I_L() personality ptr @__gxx_personality_v0 !dbg !590 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1749, metadata !DIExpression()), !dbg !2623
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_I_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !592 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1750, metadata !DIExpression()), !dbg !2624
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1751, metadata !DIExpression()), !dbg !2624
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1752, metadata !DIExpression()), !dbg !2624
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_I_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !594 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1753, metadata !DIExpression()), !dbg !2625
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1754, metadata !DIExpression()), !dbg !2625
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1755, metadata !DIExpression()), !dbg !2625
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_i8() personality ptr @__gxx_personality_v0 !dbg !596 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1756, metadata !DIExpression()), !dbg !2626
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_i8() personality ptr @__gxx_personality_v0 !dbg !598 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1757, metadata !DIExpression()), !dbg !2627
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !600 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1758, metadata !DIExpression()), !dbg !2628
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1759, metadata !DIExpression()), !dbg !2628
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1760, metadata !DIExpression()), !dbg !2628
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !602 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1761, metadata !DIExpression()), !dbg !2629
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1762, metadata !DIExpression()), !dbg !2629
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1763, metadata !DIExpression()), !dbg !2629
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_u64() personality ptr @__gxx_personality_v0 !dbg !604 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1764, metadata !DIExpression()), !dbg !2630
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_u64() personality ptr @__gxx_personality_v0 !dbg !606 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1765, metadata !DIExpression()), !dbg !2631
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !608 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1766, metadata !DIExpression()), !dbg !2632
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1767, metadata !DIExpression()), !dbg !2632
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1768, metadata !DIExpression()), !dbg !2632
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !610 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1769, metadata !DIExpression()), !dbg !2633
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1770, metadata !DIExpression()), !dbg !2633
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1771, metadata !DIExpression()), !dbg !2633
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_i32() personality ptr @__gxx_personality_v0 !dbg !612 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1772, metadata !DIExpression()), !dbg !2634
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_i32() personality ptr @__gxx_personality_v0 !dbg !614 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1773, metadata !DIExpression()), !dbg !2635
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !616 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1774, metadata !DIExpression()), !dbg !2636
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1775, metadata !DIExpression()), !dbg !2636
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1776, metadata !DIExpression()), !dbg !2636
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !618 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1777, metadata !DIExpression()), !dbg !2637
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1778, metadata !DIExpression()), !dbg !2637
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1779, metadata !DIExpression()), !dbg !2637
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_LC() personality ptr @__gxx_personality_v0 !dbg !620 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1780, metadata !DIExpression()), !dbg !2638
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_LC() personality ptr @__gxx_personality_v0 !dbg !622 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1781, metadata !DIExpression()), !dbg !2639
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !624 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1782, metadata !DIExpression()), !dbg !2640
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1783, metadata !DIExpression()), !dbg !2640
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1784, metadata !DIExpression()), !dbg !2640
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !626 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1785, metadata !DIExpression()), !dbg !2641
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1786, metadata !DIExpression()), !dbg !2641
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1787, metadata !DIExpression()), !dbg !2641
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_u16() personality ptr @__gxx_personality_v0 !dbg !628 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1788, metadata !DIExpression()), !dbg !2642
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_u16() personality ptr @__gxx_personality_v0 !dbg !630 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1789, metadata !DIExpression()), !dbg !2643
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !632 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1790, metadata !DIExpression()), !dbg !2644
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1791, metadata !DIExpression()), !dbg !2644
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1792, metadata !DIExpression()), !dbg !2644
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !634 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1793, metadata !DIExpression()), !dbg !2645
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1794, metadata !DIExpression()), !dbg !2645
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1795, metadata !DIExpression()), !dbg !2645
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_I() personality ptr @__gxx_personality_v0 !dbg !636 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1796, metadata !DIExpression()), !dbg !2646
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_I() personality ptr @__gxx_personality_v0 !dbg !638 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1797, metadata !DIExpression()), !dbg !2647
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !640 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1798, metadata !DIExpression()), !dbg !2648
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1799, metadata !DIExpression()), !dbg !2648
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1800, metadata !DIExpression()), !dbg !2648
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !642 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1801, metadata !DIExpression()), !dbg !2649
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1802, metadata !DIExpression()), !dbg !2649
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1803, metadata !DIExpression()), !dbg !2649
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_i64() personality ptr @__gxx_personality_v0 !dbg !644 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1804, metadata !DIExpression()), !dbg !2650
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_i64() personality ptr @__gxx_personality_v0 !dbg !646 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1805, metadata !DIExpression()), !dbg !2651
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !648 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1806, metadata !DIExpression()), !dbg !2652
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1807, metadata !DIExpression()), !dbg !2652
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1808, metadata !DIExpression()), !dbg !2652
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !650 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1809, metadata !DIExpression()), !dbg !2653
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1810, metadata !DIExpression()), !dbg !2653
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1811, metadata !DIExpression()), !dbg !2653
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_i16() personality ptr @__gxx_personality_v0 !dbg !652 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1812, metadata !DIExpression()), !dbg !2654
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_i16() personality ptr @__gxx_personality_v0 !dbg !654 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1813, metadata !DIExpression()), !dbg !2655
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !656 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1814, metadata !DIExpression()), !dbg !2656
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1815, metadata !DIExpression()), !dbg !2656
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1816, metadata !DIExpression()), !dbg !2656
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !658 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1817, metadata !DIExpression()), !dbg !2657
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1818, metadata !DIExpression()), !dbg !2657
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1819, metadata !DIExpression()), !dbg !2657
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_C() personality ptr @__gxx_personality_v0 !dbg !660 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1820, metadata !DIExpression()), !dbg !2658
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_C() personality ptr @__gxx_personality_v0 !dbg !662 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1821, metadata !DIExpression()), !dbg !2659
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !664 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1822, metadata !DIExpression()), !dbg !2660
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1823, metadata !DIExpression()), !dbg !2660
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1824, metadata !DIExpression()), !dbg !2660
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !666 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1825, metadata !DIExpression()), !dbg !2661
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1826, metadata !DIExpression()), !dbg !2661
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1827, metadata !DIExpression()), !dbg !2661
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_i64_u32() personality ptr @__gxx_personality_v0 !dbg !668 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1828, metadata !DIExpression()), !dbg !2662
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_u32() personality ptr @__gxx_personality_v0 !dbg !670 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1829, metadata !DIExpression()), !dbg !2663
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !672 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1830, metadata !DIExpression()), !dbg !2664
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1831, metadata !DIExpression()), !dbg !2664
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1832, metadata !DIExpression()), !dbg !2664
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !674 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1833, metadata !DIExpression()), !dbg !2665
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1834, metadata !DIExpression()), !dbg !2665
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1835, metadata !DIExpression()), !dbg !2665
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_u8() personality ptr @__gxx_personality_v0 !dbg !676 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1836, metadata !DIExpression()), !dbg !2666
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i64_u8() personality ptr @__gxx_personality_v0 !dbg !678 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1837, metadata !DIExpression()), !dbg !2667
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i64_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !680 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1838, metadata !DIExpression()), !dbg !2668
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1839, metadata !DIExpression()), !dbg !2668
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1840, metadata !DIExpression()), !dbg !2668
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i64_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !682 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1841, metadata !DIExpression()), !dbg !2669
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1842, metadata !DIExpression()), !dbg !2669
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1843, metadata !DIExpression()), !dbg !2669
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i64_L() personality ptr @__gxx_personality_v0 !dbg !684 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1844, metadata !DIExpression()), !dbg !2670
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_i64_L() personality ptr @__gxx_personality_v0 !dbg !686 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1845, metadata !DIExpression()), !dbg !2671
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_i64_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !688 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1846, metadata !DIExpression()), !dbg !2672
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1847, metadata !DIExpression()), !dbg !2672
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1848, metadata !DIExpression()), !dbg !2672
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_i64_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !690 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1849, metadata !DIExpression()), !dbg !2673
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1850, metadata !DIExpression()), !dbg !2673
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1851, metadata !DIExpression()), !dbg !2673
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define float @Plus__Plus_var_f32_f32() personality ptr @__gxx_personality_v0 !dbg !692 {
entry:
  %_result.slot = alloca float
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1852, metadata !DIExpression()), !dbg !2674
  %t1 = load float, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 168)
  %t2 = load float, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 168)
  %t3 = fadd float %t1, %t2
  ret float %t3
}

define float @Plus__Plus_param_f32_f32(float %a.a, float %a.b) personality ptr @__gxx_personality_v0 !dbg !694 {
entry:
  %_result.slot = alloca float
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1853, metadata !DIExpression()), !dbg !2675
  %b.slot = alloca float
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1854, metadata !DIExpression()), !dbg !2675
  %a.slot = alloca float
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1855, metadata !DIExpression()), !dbg !2675
  store float %a.a, ptr %a.slot
  store float %a.b, ptr %b.slot
  %t1 = load float, ptr %a.slot
  %t2 = load float, ptr %b.slot
  %t3 = fadd float %t1, %t2
  ret float %t3
}

define i64 @Plus__uPlus_var_i16_i8() personality ptr @__gxx_personality_v0 !dbg !696 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1856, metadata !DIExpression()), !dbg !2676
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_i8() personality ptr @__gxx_personality_v0 !dbg !698 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1857, metadata !DIExpression()), !dbg !2677
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i16 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_i8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !700 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1858, metadata !DIExpression()), !dbg !2678
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1859, metadata !DIExpression()), !dbg !2678
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1860, metadata !DIExpression()), !dbg !2678
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_i8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !702 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1861, metadata !DIExpression()), !dbg !2679
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1862, metadata !DIExpression()), !dbg !2679
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1863, metadata !DIExpression()), !dbg !2679
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_u64() personality ptr @__gxx_personality_v0 !dbg !704 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1864, metadata !DIExpression()), !dbg !2680
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i16_u64() personality ptr @__gxx_personality_v0 !dbg !706 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1865, metadata !DIExpression()), !dbg !2681
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i16_u64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !708 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1866, metadata !DIExpression()), !dbg !2682
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1867, metadata !DIExpression()), !dbg !2682
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1868, metadata !DIExpression()), !dbg !2682
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i16_u64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !710 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1869, metadata !DIExpression()), !dbg !2683
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1870, metadata !DIExpression()), !dbg !2683
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1871, metadata !DIExpression()), !dbg !2683
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i16_i32() personality ptr @__gxx_personality_v0 !dbg !712 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1872, metadata !DIExpression()), !dbg !2684
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_i32() personality ptr @__gxx_personality_v0 !dbg !714 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1873, metadata !DIExpression()), !dbg !2685
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i16 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_i32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !716 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1874, metadata !DIExpression()), !dbg !2686
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1875, metadata !DIExpression()), !dbg !2686
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1876, metadata !DIExpression()), !dbg !2686
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_i32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !718 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1877, metadata !DIExpression()), !dbg !2687
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1878, metadata !DIExpression()), !dbg !2687
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1879, metadata !DIExpression()), !dbg !2687
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_LC() personality ptr @__gxx_personality_v0 !dbg !720 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1880, metadata !DIExpression()), !dbg !2688
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i16_LC() personality ptr @__gxx_personality_v0 !dbg !722 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1881, metadata !DIExpression()), !dbg !2689
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i16_LC(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !724 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1882, metadata !DIExpression()), !dbg !2690
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1883, metadata !DIExpression()), !dbg !2690
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1884, metadata !DIExpression()), !dbg !2690
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i16_LC(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !726 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1885, metadata !DIExpression()), !dbg !2691
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1886, metadata !DIExpression()), !dbg !2691
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1887, metadata !DIExpression()), !dbg !2691
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i16_u16() personality ptr @__gxx_personality_v0 !dbg !728 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1888, metadata !DIExpression()), !dbg !2692
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_u16() personality ptr @__gxx_personality_v0 !dbg !730 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1889, metadata !DIExpression()), !dbg !2693
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = sext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_u16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !732 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1890, metadata !DIExpression()), !dbg !2694
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1891, metadata !DIExpression()), !dbg !2694
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1892, metadata !DIExpression()), !dbg !2694
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_u16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !734 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1893, metadata !DIExpression()), !dbg !2695
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1894, metadata !DIExpression()), !dbg !2695
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1895, metadata !DIExpression()), !dbg !2695
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_I() personality ptr @__gxx_personality_v0 !dbg !736 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1896, metadata !DIExpression()), !dbg !2696
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i16 @Plus__Plus_var_i16_I() personality ptr @__gxx_personality_v0 !dbg !738 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1897, metadata !DIExpression()), !dbg !2697
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = sext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_param_i16_I(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !740 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1898, metadata !DIExpression()), !dbg !2698
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1899, metadata !DIExpression()), !dbg !2698
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1900, metadata !DIExpression()), !dbg !2698
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i16 @Plus__Plus_param_i16_I(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !742 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1901, metadata !DIExpression()), !dbg !2699
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1902, metadata !DIExpression()), !dbg !2699
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1903, metadata !DIExpression()), !dbg !2699
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_var_i16_i64() personality ptr @__gxx_personality_v0 !dbg !744 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1904, metadata !DIExpression()), !dbg !2700
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i16_i64() personality ptr @__gxx_personality_v0 !dbg !746 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1905, metadata !DIExpression()), !dbg !2701
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i16_i64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !748 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1906, metadata !DIExpression()), !dbg !2702
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1907, metadata !DIExpression()), !dbg !2702
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1908, metadata !DIExpression()), !dbg !2702
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i16_i64(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !750 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1909, metadata !DIExpression()), !dbg !2703
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1910, metadata !DIExpression()), !dbg !2703
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1911, metadata !DIExpression()), !dbg !2703
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_i16_i16() personality ptr @__gxx_personality_v0 !dbg !752 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1912, metadata !DIExpression()), !dbg !2704
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_i16() personality ptr @__gxx_personality_v0 !dbg !754 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1913, metadata !DIExpression()), !dbg !2705
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_i16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !756 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1914, metadata !DIExpression()), !dbg !2706
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1915, metadata !DIExpression()), !dbg !2706
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1916, metadata !DIExpression()), !dbg !2706
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_i16(i16 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !758 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1917, metadata !DIExpression()), !dbg !2707
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1918, metadata !DIExpression()), !dbg !2707
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1919, metadata !DIExpression()), !dbg !2707
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_C() personality ptr @__gxx_personality_v0 !dbg !760 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1920, metadata !DIExpression()), !dbg !2708
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i16 @Plus__Plus_var_i16_C() personality ptr @__gxx_personality_v0 !dbg !762 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1921, metadata !DIExpression()), !dbg !2709
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = sext i16 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_param_i16_C(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !764 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1922, metadata !DIExpression()), !dbg !2710
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1923, metadata !DIExpression()), !dbg !2710
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1924, metadata !DIExpression()), !dbg !2710
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i16 @Plus__Plus_param_i16_C(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !766 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1925, metadata !DIExpression()), !dbg !2711
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1926, metadata !DIExpression()), !dbg !2711
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1927, metadata !DIExpression()), !dbg !2711
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i16
  ret i16 %t5
}

define i64 @Plus__uPlus_var_i16_u32() personality ptr @__gxx_personality_v0 !dbg !768 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1928, metadata !DIExpression()), !dbg !2712
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_u32() personality ptr @__gxx_personality_v0 !dbg !770 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1929, metadata !DIExpression()), !dbg !2713
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = sext i16 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_u32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !772 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1930, metadata !DIExpression()), !dbg !2714
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1931, metadata !DIExpression()), !dbg !2714
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1932, metadata !DIExpression()), !dbg !2714
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_u32(i16 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !774 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1933, metadata !DIExpression()), !dbg !2715
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1934, metadata !DIExpression()), !dbg !2715
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1935, metadata !DIExpression()), !dbg !2715
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_u8() personality ptr @__gxx_personality_v0 !dbg !776 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1936, metadata !DIExpression()), !dbg !2716
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_var_i16_u8() personality ptr @__gxx_personality_v0 !dbg !778 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1937, metadata !DIExpression()), !dbg !2717
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = sext i16 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_param_i16_u8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !780 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1938, metadata !DIExpression()), !dbg !2718
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1939, metadata !DIExpression()), !dbg !2718
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1940, metadata !DIExpression()), !dbg !2718
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i16 @Plus__Plus_param_i16_u8(i16 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !782 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1941, metadata !DIExpression()), !dbg !2719
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1942, metadata !DIExpression()), !dbg !2719
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1943, metadata !DIExpression()), !dbg !2719
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i16
  ret i16 %t6
}

define i64 @Plus__uPlus_var_i16_L() personality ptr @__gxx_personality_v0 !dbg !784 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1944, metadata !DIExpression()), !dbg !2720
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_i16_L() personality ptr @__gxx_personality_v0 !dbg !786 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1945, metadata !DIExpression()), !dbg !2721
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t2 = sext i16 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_i16_L(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !788 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1946, metadata !DIExpression()), !dbg !2722
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1947, metadata !DIExpression()), !dbg !2722
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1948, metadata !DIExpression()), !dbg !2722
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_i16_L(i16 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !790 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1949, metadata !DIExpression()), !dbg !2723
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1950, metadata !DIExpression()), !dbg !2723
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1951, metadata !DIExpression()), !dbg !2723
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_i8() personality ptr @__gxx_personality_v0 !dbg !792 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1952, metadata !DIExpression()), !dbg !2724
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_i8() personality ptr @__gxx_personality_v0 !dbg !794 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1953, metadata !DIExpression()), !dbg !2725
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !796 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1954, metadata !DIExpression()), !dbg !2726
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1955, metadata !DIExpression()), !dbg !2726
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1956, metadata !DIExpression()), !dbg !2726
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !798 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1957, metadata !DIExpression()), !dbg !2727
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1958, metadata !DIExpression()), !dbg !2727
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1959, metadata !DIExpression()), !dbg !2727
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_u64() personality ptr @__gxx_personality_v0 !dbg !800 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1960, metadata !DIExpression()), !dbg !2728
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_u64() personality ptr @__gxx_personality_v0 !dbg !802 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1961, metadata !DIExpression()), !dbg !2729
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !804 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1962, metadata !DIExpression()), !dbg !2730
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1963, metadata !DIExpression()), !dbg !2730
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1964, metadata !DIExpression()), !dbg !2730
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !806 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1965, metadata !DIExpression()), !dbg !2731
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1966, metadata !DIExpression()), !dbg !2731
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1967, metadata !DIExpression()), !dbg !2731
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_C_i32() personality ptr @__gxx_personality_v0 !dbg !808 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1968, metadata !DIExpression()), !dbg !2732
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_i32() personality ptr @__gxx_personality_v0 !dbg !810 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1969, metadata !DIExpression()), !dbg !2733
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !812 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1970, metadata !DIExpression()), !dbg !2734
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1971, metadata !DIExpression()), !dbg !2734
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1972, metadata !DIExpression()), !dbg !2734
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !814 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1973, metadata !DIExpression()), !dbg !2735
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1974, metadata !DIExpression()), !dbg !2735
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1975, metadata !DIExpression()), !dbg !2735
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_LC() personality ptr @__gxx_personality_v0 !dbg !816 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1976, metadata !DIExpression()), !dbg !2736
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_LC() personality ptr @__gxx_personality_v0 !dbg !818 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1977, metadata !DIExpression()), !dbg !2737
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !820 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1978, metadata !DIExpression()), !dbg !2738
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1979, metadata !DIExpression()), !dbg !2738
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1980, metadata !DIExpression()), !dbg !2738
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !822 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1981, metadata !DIExpression()), !dbg !2739
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1982, metadata !DIExpression()), !dbg !2739
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1983, metadata !DIExpression()), !dbg !2739
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_C_u16() personality ptr @__gxx_personality_v0 !dbg !824 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1984, metadata !DIExpression()), !dbg !2740
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_u16() personality ptr @__gxx_personality_v0 !dbg !826 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1985, metadata !DIExpression()), !dbg !2741
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !828 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1986, metadata !DIExpression()), !dbg !2742
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1987, metadata !DIExpression()), !dbg !2742
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1988, metadata !DIExpression()), !dbg !2742
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !830 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1989, metadata !DIExpression()), !dbg !2743
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1990, metadata !DIExpression()), !dbg !2743
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1991, metadata !DIExpression()), !dbg !2743
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_I() personality ptr @__gxx_personality_v0 !dbg !832 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1992, metadata !DIExpression()), !dbg !2744
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_I() personality ptr @__gxx_personality_v0 !dbg !834 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1993, metadata !DIExpression()), !dbg !2745
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !836 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1994, metadata !DIExpression()), !dbg !2746
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1995, metadata !DIExpression()), !dbg !2746
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1996, metadata !DIExpression()), !dbg !2746
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !838 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !1997, metadata !DIExpression()), !dbg !2747
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !1998, metadata !DIExpression()), !dbg !2747
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !1999, metadata !DIExpression()), !dbg !2747
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_C_i64() personality ptr @__gxx_personality_v0 !dbg !840 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2000, metadata !DIExpression()), !dbg !2748
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_i64() personality ptr @__gxx_personality_v0 !dbg !842 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2001, metadata !DIExpression()), !dbg !2749
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !844 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2002, metadata !DIExpression()), !dbg !2750
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2003, metadata !DIExpression()), !dbg !2750
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2004, metadata !DIExpression()), !dbg !2750
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !846 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2005, metadata !DIExpression()), !dbg !2751
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2006, metadata !DIExpression()), !dbg !2751
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2007, metadata !DIExpression()), !dbg !2751
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_C_i16() personality ptr @__gxx_personality_v0 !dbg !848 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2008, metadata !DIExpression()), !dbg !2752
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_i16() personality ptr @__gxx_personality_v0 !dbg !850 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2009, metadata !DIExpression()), !dbg !2753
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !852 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2010, metadata !DIExpression()), !dbg !2754
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2011, metadata !DIExpression()), !dbg !2754
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2012, metadata !DIExpression()), !dbg !2754
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !854 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2013, metadata !DIExpression()), !dbg !2755
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2014, metadata !DIExpression()), !dbg !2755
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2015, metadata !DIExpression()), !dbg !2755
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_C() personality ptr @__gxx_personality_v0 !dbg !856 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2016, metadata !DIExpression()), !dbg !2756
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_C() personality ptr @__gxx_personality_v0 !dbg !858 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2017, metadata !DIExpression()), !dbg !2757
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !860 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2018, metadata !DIExpression()), !dbg !2758
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2019, metadata !DIExpression()), !dbg !2758
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2020, metadata !DIExpression()), !dbg !2758
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !862 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2021, metadata !DIExpression()), !dbg !2759
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2022, metadata !DIExpression()), !dbg !2759
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2023, metadata !DIExpression()), !dbg !2759
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_C_u32() personality ptr @__gxx_personality_v0 !dbg !864 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2024, metadata !DIExpression()), !dbg !2760
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_u32() personality ptr @__gxx_personality_v0 !dbg !866 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2025, metadata !DIExpression()), !dbg !2761
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !868 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2026, metadata !DIExpression()), !dbg !2762
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2027, metadata !DIExpression()), !dbg !2762
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2028, metadata !DIExpression()), !dbg !2762
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !870 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2029, metadata !DIExpression()), !dbg !2763
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2030, metadata !DIExpression()), !dbg !2763
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2031, metadata !DIExpression()), !dbg !2763
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_u8() personality ptr @__gxx_personality_v0 !dbg !872 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2032, metadata !DIExpression()), !dbg !2764
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_C_u8() personality ptr @__gxx_personality_v0 !dbg !874 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2033, metadata !DIExpression()), !dbg !2765
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_C_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !876 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2034, metadata !DIExpression()), !dbg !2766
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2035, metadata !DIExpression()), !dbg !2766
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2036, metadata !DIExpression()), !dbg !2766
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_C_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !878 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2037, metadata !DIExpression()), !dbg !2767
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2038, metadata !DIExpression()), !dbg !2767
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2039, metadata !DIExpression()), !dbg !2767
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_C_L() personality ptr @__gxx_personality_v0 !dbg !880 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2040, metadata !DIExpression()), !dbg !2768
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_C_L() personality ptr @__gxx_personality_v0 !dbg !882 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2041, metadata !DIExpression()), !dbg !2769
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_C_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !884 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2042, metadata !DIExpression()), !dbg !2770
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2043, metadata !DIExpression()), !dbg !2770
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2044, metadata !DIExpression()), !dbg !2770
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_C_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !886 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2045, metadata !DIExpression()), !dbg !2771
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2046, metadata !DIExpression()), !dbg !2771
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2047, metadata !DIExpression()), !dbg !2771
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_u32_i8() personality ptr @__gxx_personality_v0 !dbg !888 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2048, metadata !DIExpression()), !dbg !2772
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_i8() personality ptr @__gxx_personality_v0 !dbg !890 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2049, metadata !DIExpression()), !dbg !2773
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i32 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_i8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !892 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2050, metadata !DIExpression()), !dbg !2774
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2051, metadata !DIExpression()), !dbg !2774
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2052, metadata !DIExpression()), !dbg !2774
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_i8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !894 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2053, metadata !DIExpression()), !dbg !2775
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2054, metadata !DIExpression()), !dbg !2775
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2055, metadata !DIExpression()), !dbg !2775
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_u64() personality ptr @__gxx_personality_v0 !dbg !896 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2056, metadata !DIExpression()), !dbg !2776
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u32_u64() personality ptr @__gxx_personality_v0 !dbg !898 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2057, metadata !DIExpression()), !dbg !2777
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u32_u64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !900 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2058, metadata !DIExpression()), !dbg !2778
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2059, metadata !DIExpression()), !dbg !2778
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2060, metadata !DIExpression()), !dbg !2778
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u32_u64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !902 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2061, metadata !DIExpression()), !dbg !2779
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2062, metadata !DIExpression()), !dbg !2779
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2063, metadata !DIExpression()), !dbg !2779
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u32_i32() personality ptr @__gxx_personality_v0 !dbg !904 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2064, metadata !DIExpression()), !dbg !2780
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_i32() personality ptr @__gxx_personality_v0 !dbg !906 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2065, metadata !DIExpression()), !dbg !2781
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i32 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_i32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !908 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2066, metadata !DIExpression()), !dbg !2782
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2067, metadata !DIExpression()), !dbg !2782
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2068, metadata !DIExpression()), !dbg !2782
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_i32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !910 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2069, metadata !DIExpression()), !dbg !2783
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2070, metadata !DIExpression()), !dbg !2783
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2071, metadata !DIExpression()), !dbg !2783
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_LC() personality ptr @__gxx_personality_v0 !dbg !912 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2072, metadata !DIExpression()), !dbg !2784
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u32_LC() personality ptr @__gxx_personality_v0 !dbg !914 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2073, metadata !DIExpression()), !dbg !2785
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u32_LC(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !916 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2074, metadata !DIExpression()), !dbg !2786
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2075, metadata !DIExpression()), !dbg !2786
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2076, metadata !DIExpression()), !dbg !2786
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u32_LC(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !918 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2077, metadata !DIExpression()), !dbg !2787
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2078, metadata !DIExpression()), !dbg !2787
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2079, metadata !DIExpression()), !dbg !2787
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u32_u16() personality ptr @__gxx_personality_v0 !dbg !920 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2080, metadata !DIExpression()), !dbg !2788
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_u16() personality ptr @__gxx_personality_v0 !dbg !922 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2081, metadata !DIExpression()), !dbg !2789
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_u16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !924 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2082, metadata !DIExpression()), !dbg !2790
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2083, metadata !DIExpression()), !dbg !2790
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2084, metadata !DIExpression()), !dbg !2790
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_u16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !926 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2085, metadata !DIExpression()), !dbg !2791
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2086, metadata !DIExpression()), !dbg !2791
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2087, metadata !DIExpression()), !dbg !2791
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_I() personality ptr @__gxx_personality_v0 !dbg !928 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2088, metadata !DIExpression()), !dbg !2792
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i32 @Plus__Plus_var_u32_I() personality ptr @__gxx_personality_v0 !dbg !930 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2089, metadata !DIExpression()), !dbg !2793
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_param_u32_I(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !932 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2090, metadata !DIExpression()), !dbg !2794
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2091, metadata !DIExpression()), !dbg !2794
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2092, metadata !DIExpression()), !dbg !2794
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i32 @Plus__Plus_param_u32_I(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !934 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2093, metadata !DIExpression()), !dbg !2795
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2094, metadata !DIExpression()), !dbg !2795
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2095, metadata !DIExpression()), !dbg !2795
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_var_u32_i64() personality ptr @__gxx_personality_v0 !dbg !936 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2096, metadata !DIExpression()), !dbg !2796
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u32_i64() personality ptr @__gxx_personality_v0 !dbg !938 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2097, metadata !DIExpression()), !dbg !2797
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u32_i64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !940 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2098, metadata !DIExpression()), !dbg !2798
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2099, metadata !DIExpression()), !dbg !2798
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2100, metadata !DIExpression()), !dbg !2798
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u32_i64(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !942 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2101, metadata !DIExpression()), !dbg !2799
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2102, metadata !DIExpression()), !dbg !2799
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2103, metadata !DIExpression()), !dbg !2799
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u32_i16() personality ptr @__gxx_personality_v0 !dbg !944 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2104, metadata !DIExpression()), !dbg !2800
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_i16() personality ptr @__gxx_personality_v0 !dbg !946 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2105, metadata !DIExpression()), !dbg !2801
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i32 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_i16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !948 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2106, metadata !DIExpression()), !dbg !2802
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2107, metadata !DIExpression()), !dbg !2802
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2108, metadata !DIExpression()), !dbg !2802
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_i16(i32 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !950 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2109, metadata !DIExpression()), !dbg !2803
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2110, metadata !DIExpression()), !dbg !2803
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2111, metadata !DIExpression()), !dbg !2803
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_C() personality ptr @__gxx_personality_v0 !dbg !952 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2112, metadata !DIExpression()), !dbg !2804
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i32 @Plus__Plus_var_u32_C() personality ptr @__gxx_personality_v0 !dbg !954 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2113, metadata !DIExpression()), !dbg !2805
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i32 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_param_u32_C(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !956 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2114, metadata !DIExpression()), !dbg !2806
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2115, metadata !DIExpression()), !dbg !2806
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2116, metadata !DIExpression()), !dbg !2806
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i32 @Plus__Plus_param_u32_C(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !958 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2117, metadata !DIExpression()), !dbg !2807
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2118, metadata !DIExpression()), !dbg !2807
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2119, metadata !DIExpression()), !dbg !2807
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i32
  ret i32 %t5
}

define i64 @Plus__uPlus_var_u32_u32() personality ptr @__gxx_personality_v0 !dbg !960 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2120, metadata !DIExpression()), !dbg !2808
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_u32() personality ptr @__gxx_personality_v0 !dbg !962 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2121, metadata !DIExpression()), !dbg !2809
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_u32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !964 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2122, metadata !DIExpression()), !dbg !2810
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2123, metadata !DIExpression()), !dbg !2810
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2124, metadata !DIExpression()), !dbg !2810
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_u32(i32 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !966 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2125, metadata !DIExpression()), !dbg !2811
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2126, metadata !DIExpression()), !dbg !2811
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2127, metadata !DIExpression()), !dbg !2811
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_u8() personality ptr @__gxx_personality_v0 !dbg !968 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2128, metadata !DIExpression()), !dbg !2812
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_var_u32_u8() personality ptr @__gxx_personality_v0 !dbg !970 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2129, metadata !DIExpression()), !dbg !2813
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i32 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_param_u32_u8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !972 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2130, metadata !DIExpression()), !dbg !2814
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2131, metadata !DIExpression()), !dbg !2814
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2132, metadata !DIExpression()), !dbg !2814
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i32 @Plus__Plus_param_u32_u8(i32 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !974 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2133, metadata !DIExpression()), !dbg !2815
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2134, metadata !DIExpression()), !dbg !2815
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2135, metadata !DIExpression()), !dbg !2815
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i32
  ret i32 %t6
}

define i64 @Plus__uPlus_var_u32_L() personality ptr @__gxx_personality_v0 !dbg !976 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2136, metadata !DIExpression()), !dbg !2816
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u32_L() personality ptr @__gxx_personality_v0 !dbg !978 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2137, metadata !DIExpression()), !dbg !2817
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t2 = zext i32 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u32_L(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !980 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2138, metadata !DIExpression()), !dbg !2818
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2139, metadata !DIExpression()), !dbg !2818
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2140, metadata !DIExpression()), !dbg !2818
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u32_L(i32 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !982 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2141, metadata !DIExpression()), !dbg !2819
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2142, metadata !DIExpression()), !dbg !2819
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2143, metadata !DIExpression()), !dbg !2819
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u8_i8() personality ptr @__gxx_personality_v0 !dbg !984 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2144, metadata !DIExpression()), !dbg !2820
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_i8() personality ptr @__gxx_personality_v0 !dbg !986 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2145, metadata !DIExpression()), !dbg !2821
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = zext i8 %t1 to i64
  %t4 = sext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_i8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !988 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2146, metadata !DIExpression()), !dbg !2822
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2147, metadata !DIExpression()), !dbg !2822
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2148, metadata !DIExpression()), !dbg !2822
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_i8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !990 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2149, metadata !DIExpression()), !dbg !2823
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2150, metadata !DIExpression()), !dbg !2823
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2151, metadata !DIExpression()), !dbg !2823
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_u64() personality ptr @__gxx_personality_v0 !dbg !992 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2152, metadata !DIExpression()), !dbg !2824
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u8_u64() personality ptr @__gxx_personality_v0 !dbg !994 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2153, metadata !DIExpression()), !dbg !2825
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u8_u64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !996 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2154, metadata !DIExpression()), !dbg !2826
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2155, metadata !DIExpression()), !dbg !2826
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2156, metadata !DIExpression()), !dbg !2826
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u8_u64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !998 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2157, metadata !DIExpression()), !dbg !2827
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2158, metadata !DIExpression()), !dbg !2827
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2159, metadata !DIExpression()), !dbg !2827
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u8_i32() personality ptr @__gxx_personality_v0 !dbg !1000 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2160, metadata !DIExpression()), !dbg !2828
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_i32() personality ptr @__gxx_personality_v0 !dbg !1002 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2161, metadata !DIExpression()), !dbg !2829
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = zext i8 %t1 to i64
  %t4 = sext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_i32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1004 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2162, metadata !DIExpression()), !dbg !2830
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2163, metadata !DIExpression()), !dbg !2830
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2164, metadata !DIExpression()), !dbg !2830
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_i32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1006 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2165, metadata !DIExpression()), !dbg !2831
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2166, metadata !DIExpression()), !dbg !2831
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2167, metadata !DIExpression()), !dbg !2831
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_LC() personality ptr @__gxx_personality_v0 !dbg !1008 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2168, metadata !DIExpression()), !dbg !2832
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u8_LC() personality ptr @__gxx_personality_v0 !dbg !1010 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2169, metadata !DIExpression()), !dbg !2833
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u8_LC(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1012 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2170, metadata !DIExpression()), !dbg !2834
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2171, metadata !DIExpression()), !dbg !2834
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2172, metadata !DIExpression()), !dbg !2834
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u8_LC(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1014 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2173, metadata !DIExpression()), !dbg !2835
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2174, metadata !DIExpression()), !dbg !2835
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2175, metadata !DIExpression()), !dbg !2835
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u8_u16() personality ptr @__gxx_personality_v0 !dbg !1016 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2176, metadata !DIExpression()), !dbg !2836
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_u16() personality ptr @__gxx_personality_v0 !dbg !1018 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2177, metadata !DIExpression()), !dbg !2837
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_u16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1020 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2178, metadata !DIExpression()), !dbg !2838
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2179, metadata !DIExpression()), !dbg !2838
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2180, metadata !DIExpression()), !dbg !2838
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_u16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1022 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2181, metadata !DIExpression()), !dbg !2839
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2182, metadata !DIExpression()), !dbg !2839
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2183, metadata !DIExpression()), !dbg !2839
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_I() personality ptr @__gxx_personality_v0 !dbg !1024 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2184, metadata !DIExpression()), !dbg !2840
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i8 @Plus__Plus_var_u8_I() personality ptr @__gxx_personality_v0 !dbg !1026 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2185, metadata !DIExpression()), !dbg !2841
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_param_u8_I(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1028 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2186, metadata !DIExpression()), !dbg !2842
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2187, metadata !DIExpression()), !dbg !2842
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2188, metadata !DIExpression()), !dbg !2842
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i8 @Plus__Plus_param_u8_I(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1030 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2189, metadata !DIExpression()), !dbg !2843
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2190, metadata !DIExpression()), !dbg !2843
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2191, metadata !DIExpression()), !dbg !2843
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_var_u8_i64() personality ptr @__gxx_personality_v0 !dbg !1032 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2192, metadata !DIExpression()), !dbg !2844
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u8_i64() personality ptr @__gxx_personality_v0 !dbg !1034 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2193, metadata !DIExpression()), !dbg !2845
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u8_i64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1036 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2194, metadata !DIExpression()), !dbg !2846
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2195, metadata !DIExpression()), !dbg !2846
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2196, metadata !DIExpression()), !dbg !2846
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u8_i64(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1038 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2197, metadata !DIExpression()), !dbg !2847
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2198, metadata !DIExpression()), !dbg !2847
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2199, metadata !DIExpression()), !dbg !2847
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_u8_i16() personality ptr @__gxx_personality_v0 !dbg !1040 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2200, metadata !DIExpression()), !dbg !2848
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_i16() personality ptr @__gxx_personality_v0 !dbg !1042 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2201, metadata !DIExpression()), !dbg !2849
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = zext i8 %t1 to i64
  %t4 = sext i16 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_i16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1044 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2202, metadata !DIExpression()), !dbg !2850
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2203, metadata !DIExpression()), !dbg !2850
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2204, metadata !DIExpression()), !dbg !2850
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_i16(i8 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1046 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2205, metadata !DIExpression()), !dbg !2851
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2206, metadata !DIExpression()), !dbg !2851
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2207, metadata !DIExpression()), !dbg !2851
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = sext i16 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_C() personality ptr @__gxx_personality_v0 !dbg !1048 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2208, metadata !DIExpression()), !dbg !2852
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  ret i64 %t4
}

define i8 @Plus__Plus_var_u8_C() personality ptr @__gxx_personality_v0 !dbg !1050 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2209, metadata !DIExpression()), !dbg !2853
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = zext i8 %t1 to i64
  %t4 = add i64 %t3, %t2
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_param_u8_C(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1052 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2210, metadata !DIExpression()), !dbg !2854
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2211, metadata !DIExpression()), !dbg !2854
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2212, metadata !DIExpression()), !dbg !2854
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i8 @Plus__Plus_param_u8_C(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1054 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2213, metadata !DIExpression()), !dbg !2855
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2214, metadata !DIExpression()), !dbg !2855
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2215, metadata !DIExpression()), !dbg !2855
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  %t5 = trunc i64 %t4 to i8
  ret i8 %t5
}

define i64 @Plus__uPlus_var_u8_u32() personality ptr @__gxx_personality_v0 !dbg !1056 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2216, metadata !DIExpression()), !dbg !2856
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_u32() personality ptr @__gxx_personality_v0 !dbg !1058 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2217, metadata !DIExpression()), !dbg !2857
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i32 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_u32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1060 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2218, metadata !DIExpression()), !dbg !2858
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2219, metadata !DIExpression()), !dbg !2858
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2220, metadata !DIExpression()), !dbg !2858
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_u32(i8 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1062 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2221, metadata !DIExpression()), !dbg !2859
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2222, metadata !DIExpression()), !dbg !2859
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2223, metadata !DIExpression()), !dbg !2859
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i32 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_u8() personality ptr @__gxx_personality_v0 !dbg !1064 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2224, metadata !DIExpression()), !dbg !2860
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_var_u8_u8() personality ptr @__gxx_personality_v0 !dbg !1066 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2225, metadata !DIExpression()), !dbg !2861
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t1 to i64
  %t4 = zext i8 %t2 to i64
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_param_u8_u8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1068 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2226, metadata !DIExpression()), !dbg !2862
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2227, metadata !DIExpression()), !dbg !2862
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2228, metadata !DIExpression()), !dbg !2862
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  ret i64 %t5
}

define i8 @Plus__Plus_param_u8_u8(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1070 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2229, metadata !DIExpression()), !dbg !2863
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2230, metadata !DIExpression()), !dbg !2863
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2231, metadata !DIExpression()), !dbg !2863
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = add i64 %t3, %t4
  %t6 = trunc i64 %t5 to i8
  ret i8 %t6
}

define i64 @Plus__uPlus_var_u8_L() personality ptr @__gxx_personality_v0 !dbg !1072 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2232, metadata !DIExpression()), !dbg !2864
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_u8_L() personality ptr @__gxx_personality_v0 !dbg !1074 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2233, metadata !DIExpression()), !dbg !2865
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t2 = zext i8 %t1 to i64
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_u8_L(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1076 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2234, metadata !DIExpression()), !dbg !2866
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2235, metadata !DIExpression()), !dbg !2866
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2236, metadata !DIExpression()), !dbg !2866
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_u8_L(i8 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1078 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2237, metadata !DIExpression()), !dbg !2867
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2238, metadata !DIExpression()), !dbg !2867
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2239, metadata !DIExpression()), !dbg !2867
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_i8() personality ptr @__gxx_personality_v0 !dbg !1080 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2240, metadata !DIExpression()), !dbg !2868
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_i8() personality ptr @__gxx_personality_v0 !dbg !1082 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2241, metadata !DIExpression()), !dbg !2869
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  %t3 = sext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1084 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2242, metadata !DIExpression()), !dbg !2870
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2243, metadata !DIExpression()), !dbg !2870
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2244, metadata !DIExpression()), !dbg !2870
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_i8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1086 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2245, metadata !DIExpression()), !dbg !2871
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2246, metadata !DIExpression()), !dbg !2871
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2247, metadata !DIExpression()), !dbg !2871
  store i64 %a.a, ptr %a.slot
  %t1 = sext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_u64() personality ptr @__gxx_personality_v0 !dbg !1088 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2248, metadata !DIExpression()), !dbg !2872
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_u64() personality ptr @__gxx_personality_v0 !dbg !1090 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2249, metadata !DIExpression()), !dbg !2873
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1092 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2250, metadata !DIExpression()), !dbg !2874
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2251, metadata !DIExpression()), !dbg !2874
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2252, metadata !DIExpression()), !dbg !2874
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_u64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1094 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2253, metadata !DIExpression()), !dbg !2875
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2254, metadata !DIExpression()), !dbg !2875
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2255, metadata !DIExpression()), !dbg !2875
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_L_i32() personality ptr @__gxx_personality_v0 !dbg !1096 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2256, metadata !DIExpression()), !dbg !2876
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_i32() personality ptr @__gxx_personality_v0 !dbg !1098 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2257, metadata !DIExpression()), !dbg !2877
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  %t3 = sext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1100 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2258, metadata !DIExpression()), !dbg !2878
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2259, metadata !DIExpression()), !dbg !2878
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2260, metadata !DIExpression()), !dbg !2878
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_i32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1102 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2261, metadata !DIExpression()), !dbg !2879
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2262, metadata !DIExpression()), !dbg !2879
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2263, metadata !DIExpression()), !dbg !2879
  store i64 %a.a, ptr %a.slot
  %t1 = sext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_LC() personality ptr @__gxx_personality_v0 !dbg !1104 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2264, metadata !DIExpression()), !dbg !2880
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_LC() personality ptr @__gxx_personality_v0 !dbg !1106 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2265, metadata !DIExpression()), !dbg !2881
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1108 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2266, metadata !DIExpression()), !dbg !2882
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2267, metadata !DIExpression()), !dbg !2882
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2268, metadata !DIExpression()), !dbg !2882
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_LC(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1110 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2269, metadata !DIExpression()), !dbg !2883
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2270, metadata !DIExpression()), !dbg !2883
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2271, metadata !DIExpression()), !dbg !2883
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_L_u16() personality ptr @__gxx_personality_v0 !dbg !1112 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2272, metadata !DIExpression()), !dbg !2884
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_u16() personality ptr @__gxx_personality_v0 !dbg !1114 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2273, metadata !DIExpression()), !dbg !2885
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  %t3 = zext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1116 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2274, metadata !DIExpression()), !dbg !2886
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2275, metadata !DIExpression()), !dbg !2886
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2276, metadata !DIExpression()), !dbg !2886
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_u16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1118 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2277, metadata !DIExpression()), !dbg !2887
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2278, metadata !DIExpression()), !dbg !2887
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2279, metadata !DIExpression()), !dbg !2887
  store i64 %a.a, ptr %a.slot
  %t1 = zext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_I() personality ptr @__gxx_personality_v0 !dbg !1120 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2280, metadata !DIExpression()), !dbg !2888
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_I() personality ptr @__gxx_personality_v0 !dbg !1122 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2281, metadata !DIExpression()), !dbg !2889
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1124 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2282, metadata !DIExpression()), !dbg !2890
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2283, metadata !DIExpression()), !dbg !2890
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2284, metadata !DIExpression()), !dbg !2890
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_I(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1126 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2285, metadata !DIExpression()), !dbg !2891
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2286, metadata !DIExpression()), !dbg !2891
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2287, metadata !DIExpression()), !dbg !2891
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_L_i64() personality ptr @__gxx_personality_v0 !dbg !1128 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2288, metadata !DIExpression()), !dbg !2892
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_i64() personality ptr @__gxx_personality_v0 !dbg !1130 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2289, metadata !DIExpression()), !dbg !2893
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1132 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2290, metadata !DIExpression()), !dbg !2894
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2291, metadata !DIExpression()), !dbg !2894
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2292, metadata !DIExpression()), !dbg !2894
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_i64(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1134 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2293, metadata !DIExpression()), !dbg !2895
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2294, metadata !DIExpression()), !dbg !2895
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2295, metadata !DIExpression()), !dbg !2895
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_L_i16() personality ptr @__gxx_personality_v0 !dbg !1136 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2296, metadata !DIExpression()), !dbg !2896
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_i16() personality ptr @__gxx_personality_v0 !dbg !1138 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2297, metadata !DIExpression()), !dbg !2897
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  %t3 = sext i16 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1140 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2298, metadata !DIExpression()), !dbg !2898
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2299, metadata !DIExpression()), !dbg !2898
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2300, metadata !DIExpression()), !dbg !2898
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_i16(i64 %a.a, i16 %a.b) personality ptr @__gxx_personality_v0 !dbg !1142 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2301, metadata !DIExpression()), !dbg !2899
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2302, metadata !DIExpression()), !dbg !2899
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2303, metadata !DIExpression()), !dbg !2899
  store i64 %a.a, ptr %a.slot
  %t1 = sext i16 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_C() personality ptr @__gxx_personality_v0 !dbg !1144 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2304, metadata !DIExpression()), !dbg !2900
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_C() personality ptr @__gxx_personality_v0 !dbg !1146 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2305, metadata !DIExpression()), !dbg !2901
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1148 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2306, metadata !DIExpression()), !dbg !2902
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2307, metadata !DIExpression()), !dbg !2902
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2308, metadata !DIExpression()), !dbg !2902
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_C(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1150 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2309, metadata !DIExpression()), !dbg !2903
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2310, metadata !DIExpression()), !dbg !2903
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2311, metadata !DIExpression()), !dbg !2903
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_var_L_u32() personality ptr @__gxx_personality_v0 !dbg !1152 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2312, metadata !DIExpression()), !dbg !2904
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_u32() personality ptr @__gxx_personality_v0 !dbg !1154 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2313, metadata !DIExpression()), !dbg !2905
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t3 = zext i32 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1156 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2314, metadata !DIExpression()), !dbg !2906
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2315, metadata !DIExpression()), !dbg !2906
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2316, metadata !DIExpression()), !dbg !2906
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_u32(i64 %a.a, i32 %a.b) personality ptr @__gxx_personality_v0 !dbg !1158 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2317, metadata !DIExpression()), !dbg !2907
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2318, metadata !DIExpression()), !dbg !2907
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2319, metadata !DIExpression()), !dbg !2907
  store i64 %a.a, ptr %a.slot
  %t1 = zext i32 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_u8() personality ptr @__gxx_personality_v0 !dbg !1160 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2320, metadata !DIExpression()), !dbg !2908
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_var_L_u8() personality ptr @__gxx_personality_v0 !dbg !1162 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2321, metadata !DIExpression()), !dbg !2909
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  %t3 = zext i8 %t2 to i64
  %t4 = add i64 %t1, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_param_L_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1164 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2322, metadata !DIExpression()), !dbg !2910
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2323, metadata !DIExpression()), !dbg !2910
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2324, metadata !DIExpression()), !dbg !2910
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__Plus_param_L_u8(i64 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !1166 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2325, metadata !DIExpression()), !dbg !2911
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2326, metadata !DIExpression()), !dbg !2911
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2327, metadata !DIExpression()), !dbg !2911
  store i64 %a.a, ptr %a.slot
  %t1 = zext i8 %a.b to i64
  store i64 %t1, ptr %b.slot
  %t2 = load i64, ptr %a.slot
  %t3 = load i64, ptr %b.slot
  %t4 = add i64 %t2, %t3
  ret i64 %t4
}

define i64 @Plus__uPlus_var_L_L() personality ptr @__gxx_personality_v0 !dbg !1168 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2328, metadata !DIExpression()), !dbg !2912
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_var_L_L() personality ptr @__gxx_personality_v0 !dbg !1170 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2329, metadata !DIExpression()), !dbg !2913
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t2 = load i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__uPlus_param_L_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1172 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2330, metadata !DIExpression()), !dbg !2914
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2331, metadata !DIExpression()), !dbg !2914
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2332, metadata !DIExpression()), !dbg !2914
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define i64 @Plus__Plus_param_L_L(i64 %a.a, i64 %a.b) personality ptr @__gxx_personality_v0 !dbg !1174 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !2333, metadata !DIExpression()), !dbg !2915
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !2334, metadata !DIExpression()), !dbg !2915
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !2335, metadata !DIExpression()), !dbg !2915
  store i64 %a.a, ptr %a.slot
  store i64 %a.b, ptr %b.slot
  %t1 = load i64, ptr %a.slot
  %t2 = load i64, ptr %b.slot
  %t3 = add i64 %t1, %t2
  ret i64 %t3
}

define void @Plus__Plus_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = trunc i64 145 to i8
  store i8 %t1, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
  store i64 146, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
  store double 0x406264bc6a7ef9db, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 120)
  %t2 = trunc i64 149 to i32
  store i32 %t2, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
  store i64 150, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
  %t3 = trunc i64 151 to i16
  store i16 %t3, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
  store i64 152, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
  store i64 153, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
  store float 0x406344f5c0000000, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 168)
  %t4 = trunc i64 156 to i16
  store i16 %t4, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
  store i64 157, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
  %t5 = trunc i64 158 to i32
  store i32 %t5, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
  %t6 = trunc i64 159 to i8
  store i8 %t6, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
  store i64 160, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
  ret void
}

define weak ptr @Plus_I3(i64 %mode) {
entry:
  ret ptr @Plus_M3_info
}

; RT0.ImportInfo chain for Plus
declare ptr @Long_I3(i64)
declare ptr @Word_I3(i64)
declare ptr @Cstdint_I3(i64)
@Plus_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Plus_I3, ptr @Plus_M3_imp.1 }
@Plus_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Long_I3, ptr @Plus_M3_imp.2 }
@Plus_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Word_I3, ptr @Plus_M3_imp.3 }
@Plus_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Cstdint_I3, ptr null }

; RT0.ModuleInfo for Plus (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [112 x i8] }
@Plus_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Plus_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Plus_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [112 x i8] zeroinitializer  ; user globals (112 bytes)
}
@Plus__vi8 = alias i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 104)
@Plus__vu64 = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 112)
@Plus__vf64 = alias double, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 120)
@Plus__vi32 = alias i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 128)
@Plus__vLC = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 136)
@Plus__vu16 = alias i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 144)
@Plus__vI = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 152)
@Plus__vi64 = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 160)
@Plus__vf32 = alias float, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 168)
@Plus__vi16 = alias i16, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 172)
@Plus__vC = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 176)
@Plus__vu32 = alias i32, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 184)
@Plus__vu8 = alias i8, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 188)
@Plus__vL = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 192)
@Plus__offset = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 200)
@Plus__count = alias i64, ptr getelementptr inbounds (i8, ptr @Plus_M3_info, i64 208)

define ptr @Plus_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @Plus__Plus_M3()
  br label %done
done:
  ret ptr @Plus_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_i8", linkageName: "Plus__uPlus_var_i8_i8", scope: !4, file: !3, line: 54, type: !6, scopeLine: 54, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Plus__Plus_var_i8_i8", linkageName: "Plus__Plus_var_i8_i8", scope: !4, file: !3, line: 55, type: !6, scopeLine: 55, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_i8", linkageName: "Plus__uPlus_param_i8_i8", scope: !4, file: !3, line: 56, type: !6, scopeLine: 56, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Plus__Plus_param_i8_i8", linkageName: "Plus__Plus_param_i8_i8", scope: !4, file: !3, line: 57, type: !6, scopeLine: 57, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_u64", linkageName: "Plus__uPlus_var_i8_u64", scope: !4, file: !3, line: 58, type: !6, scopeLine: 58, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Plus__Plus_var_i8_u64", linkageName: "Plus__Plus_var_i8_u64", scope: !4, file: !3, line: 59, type: !6, scopeLine: 59, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_u64", linkageName: "Plus__uPlus_param_i8_u64", scope: !4, file: !3, line: 60, type: !6, scopeLine: 60, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Plus__Plus_param_i8_u64", linkageName: "Plus__Plus_param_i8_u64", scope: !4, file: !3, line: 61, type: !6, scopeLine: 61, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_i32", linkageName: "Plus__uPlus_var_i8_i32", scope: !4, file: !3, line: 62, type: !6, scopeLine: 62, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Plus__Plus_var_i8_i32", linkageName: "Plus__Plus_var_i8_i32", scope: !4, file: !3, line: 63, type: !6, scopeLine: 63, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_i32", linkageName: "Plus__uPlus_param_i8_i32", scope: !4, file: !3, line: 64, type: !6, scopeLine: 64, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Plus__Plus_param_i8_i32", linkageName: "Plus__Plus_param_i8_i32", scope: !4, file: !3, line: 65, type: !6, scopeLine: 65, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_LC", linkageName: "Plus__uPlus_var_i8_LC", scope: !4, file: !3, line: 66, type: !6, scopeLine: 66, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "Plus__Plus_var_i8_LC", linkageName: "Plus__Plus_var_i8_LC", scope: !4, file: !3, line: 67, type: !6, scopeLine: 67, unit: !2, spFlags: DISPFlagDefinition)
!44 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_LC", linkageName: "Plus__uPlus_param_i8_LC", scope: !4, file: !3, line: 68, type: !6, scopeLine: 68, unit: !2, spFlags: DISPFlagDefinition)
!46 = distinct !DISubprogram(name: "Plus__Plus_param_i8_LC", linkageName: "Plus__Plus_param_i8_LC", scope: !4, file: !3, line: 69, type: !6, scopeLine: 69, unit: !2, spFlags: DISPFlagDefinition)
!48 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_u16", linkageName: "Plus__uPlus_var_i8_u16", scope: !4, file: !3, line: 70, type: !6, scopeLine: 70, unit: !2, spFlags: DISPFlagDefinition)
!50 = distinct !DISubprogram(name: "Plus__Plus_var_i8_u16", linkageName: "Plus__Plus_var_i8_u16", scope: !4, file: !3, line: 71, type: !6, scopeLine: 71, unit: !2, spFlags: DISPFlagDefinition)
!52 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_u16", linkageName: "Plus__uPlus_param_i8_u16", scope: !4, file: !3, line: 72, type: !6, scopeLine: 72, unit: !2, spFlags: DISPFlagDefinition)
!54 = distinct !DISubprogram(name: "Plus__Plus_param_i8_u16", linkageName: "Plus__Plus_param_i8_u16", scope: !4, file: !3, line: 73, type: !6, scopeLine: 73, unit: !2, spFlags: DISPFlagDefinition)
!56 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_I", linkageName: "Plus__uPlus_var_i8_I", scope: !4, file: !3, line: 74, type: !6, scopeLine: 74, unit: !2, spFlags: DISPFlagDefinition)
!58 = distinct !DISubprogram(name: "Plus__Plus_var_i8_I", linkageName: "Plus__Plus_var_i8_I", scope: !4, file: !3, line: 75, type: !6, scopeLine: 75, unit: !2, spFlags: DISPFlagDefinition)
!60 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_I", linkageName: "Plus__uPlus_param_i8_I", scope: !4, file: !3, line: 76, type: !6, scopeLine: 76, unit: !2, spFlags: DISPFlagDefinition)
!62 = distinct !DISubprogram(name: "Plus__Plus_param_i8_I", linkageName: "Plus__Plus_param_i8_I", scope: !4, file: !3, line: 77, type: !6, scopeLine: 77, unit: !2, spFlags: DISPFlagDefinition)
!64 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_i64", linkageName: "Plus__uPlus_var_i8_i64", scope: !4, file: !3, line: 78, type: !6, scopeLine: 78, unit: !2, spFlags: DISPFlagDefinition)
!66 = distinct !DISubprogram(name: "Plus__Plus_var_i8_i64", linkageName: "Plus__Plus_var_i8_i64", scope: !4, file: !3, line: 79, type: !6, scopeLine: 79, unit: !2, spFlags: DISPFlagDefinition)
!68 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_i64", linkageName: "Plus__uPlus_param_i8_i64", scope: !4, file: !3, line: 80, type: !6, scopeLine: 80, unit: !2, spFlags: DISPFlagDefinition)
!70 = distinct !DISubprogram(name: "Plus__Plus_param_i8_i64", linkageName: "Plus__Plus_param_i8_i64", scope: !4, file: !3, line: 81, type: !6, scopeLine: 81, unit: !2, spFlags: DISPFlagDefinition)
!72 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_i16", linkageName: "Plus__uPlus_var_i8_i16", scope: !4, file: !3, line: 82, type: !6, scopeLine: 82, unit: !2, spFlags: DISPFlagDefinition)
!74 = distinct !DISubprogram(name: "Plus__Plus_var_i8_i16", linkageName: "Plus__Plus_var_i8_i16", scope: !4, file: !3, line: 83, type: !6, scopeLine: 83, unit: !2, spFlags: DISPFlagDefinition)
!76 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_i16", linkageName: "Plus__uPlus_param_i8_i16", scope: !4, file: !3, line: 84, type: !6, scopeLine: 84, unit: !2, spFlags: DISPFlagDefinition)
!78 = distinct !DISubprogram(name: "Plus__Plus_param_i8_i16", linkageName: "Plus__Plus_param_i8_i16", scope: !4, file: !3, line: 85, type: !6, scopeLine: 85, unit: !2, spFlags: DISPFlagDefinition)
!80 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_C", linkageName: "Plus__uPlus_var_i8_C", scope: !4, file: !3, line: 86, type: !6, scopeLine: 86, unit: !2, spFlags: DISPFlagDefinition)
!82 = distinct !DISubprogram(name: "Plus__Plus_var_i8_C", linkageName: "Plus__Plus_var_i8_C", scope: !4, file: !3, line: 87, type: !6, scopeLine: 87, unit: !2, spFlags: DISPFlagDefinition)
!84 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_C", linkageName: "Plus__uPlus_param_i8_C", scope: !4, file: !3, line: 88, type: !6, scopeLine: 88, unit: !2, spFlags: DISPFlagDefinition)
!86 = distinct !DISubprogram(name: "Plus__Plus_param_i8_C", linkageName: "Plus__Plus_param_i8_C", scope: !4, file: !3, line: 89, type: !6, scopeLine: 89, unit: !2, spFlags: DISPFlagDefinition)
!88 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_u32", linkageName: "Plus__uPlus_var_i8_u32", scope: !4, file: !3, line: 90, type: !6, scopeLine: 90, unit: !2, spFlags: DISPFlagDefinition)
!90 = distinct !DISubprogram(name: "Plus__Plus_var_i8_u32", linkageName: "Plus__Plus_var_i8_u32", scope: !4, file: !3, line: 91, type: !6, scopeLine: 91, unit: !2, spFlags: DISPFlagDefinition)
!92 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_u32", linkageName: "Plus__uPlus_param_i8_u32", scope: !4, file: !3, line: 92, type: !6, scopeLine: 92, unit: !2, spFlags: DISPFlagDefinition)
!94 = distinct !DISubprogram(name: "Plus__Plus_param_i8_u32", linkageName: "Plus__Plus_param_i8_u32", scope: !4, file: !3, line: 93, type: !6, scopeLine: 93, unit: !2, spFlags: DISPFlagDefinition)
!96 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_u8", linkageName: "Plus__uPlus_var_i8_u8", scope: !4, file: !3, line: 94, type: !6, scopeLine: 94, unit: !2, spFlags: DISPFlagDefinition)
!98 = distinct !DISubprogram(name: "Plus__Plus_var_i8_u8", linkageName: "Plus__Plus_var_i8_u8", scope: !4, file: !3, line: 95, type: !6, scopeLine: 95, unit: !2, spFlags: DISPFlagDefinition)
!100 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_u8", linkageName: "Plus__uPlus_param_i8_u8", scope: !4, file: !3, line: 96, type: !6, scopeLine: 96, unit: !2, spFlags: DISPFlagDefinition)
!102 = distinct !DISubprogram(name: "Plus__Plus_param_i8_u8", linkageName: "Plus__Plus_param_i8_u8", scope: !4, file: !3, line: 97, type: !6, scopeLine: 97, unit: !2, spFlags: DISPFlagDefinition)
!104 = distinct !DISubprogram(name: "Plus__uPlus_var_i8_L", linkageName: "Plus__uPlus_var_i8_L", scope: !4, file: !3, line: 98, type: !6, scopeLine: 98, unit: !2, spFlags: DISPFlagDefinition)
!106 = distinct !DISubprogram(name: "Plus__Plus_var_i8_L", linkageName: "Plus__Plus_var_i8_L", scope: !4, file: !3, line: 99, type: !6, scopeLine: 99, unit: !2, spFlags: DISPFlagDefinition)
!108 = distinct !DISubprogram(name: "Plus__uPlus_param_i8_L", linkageName: "Plus__uPlus_param_i8_L", scope: !4, file: !3, line: 100, type: !6, scopeLine: 100, unit: !2, spFlags: DISPFlagDefinition)
!110 = distinct !DISubprogram(name: "Plus__Plus_param_i8_L", linkageName: "Plus__Plus_param_i8_L", scope: !4, file: !3, line: 101, type: !6, scopeLine: 101, unit: !2, spFlags: DISPFlagDefinition)
!112 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_i8", linkageName: "Plus__uPlus_var_u64_i8", scope: !4, file: !3, line: 102, type: !6, scopeLine: 102, unit: !2, spFlags: DISPFlagDefinition)
!114 = distinct !DISubprogram(name: "Plus__Plus_var_u64_i8", linkageName: "Plus__Plus_var_u64_i8", scope: !4, file: !3, line: 103, type: !6, scopeLine: 103, unit: !2, spFlags: DISPFlagDefinition)
!116 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_i8", linkageName: "Plus__uPlus_param_u64_i8", scope: !4, file: !3, line: 104, type: !6, scopeLine: 104, unit: !2, spFlags: DISPFlagDefinition)
!118 = distinct !DISubprogram(name: "Plus__Plus_param_u64_i8", linkageName: "Plus__Plus_param_u64_i8", scope: !4, file: !3, line: 105, type: !6, scopeLine: 105, unit: !2, spFlags: DISPFlagDefinition)
!120 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_u64", linkageName: "Plus__uPlus_var_u64_u64", scope: !4, file: !3, line: 106, type: !6, scopeLine: 106, unit: !2, spFlags: DISPFlagDefinition)
!122 = distinct !DISubprogram(name: "Plus__Plus_var_u64_u64", linkageName: "Plus__Plus_var_u64_u64", scope: !4, file: !3, line: 107, type: !6, scopeLine: 107, unit: !2, spFlags: DISPFlagDefinition)
!124 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_u64", linkageName: "Plus__uPlus_param_u64_u64", scope: !4, file: !3, line: 108, type: !6, scopeLine: 108, unit: !2, spFlags: DISPFlagDefinition)
!126 = distinct !DISubprogram(name: "Plus__Plus_param_u64_u64", linkageName: "Plus__Plus_param_u64_u64", scope: !4, file: !3, line: 109, type: !6, scopeLine: 109, unit: !2, spFlags: DISPFlagDefinition)
!128 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_i32", linkageName: "Plus__uPlus_var_u64_i32", scope: !4, file: !3, line: 110, type: !6, scopeLine: 110, unit: !2, spFlags: DISPFlagDefinition)
!130 = distinct !DISubprogram(name: "Plus__Plus_var_u64_i32", linkageName: "Plus__Plus_var_u64_i32", scope: !4, file: !3, line: 111, type: !6, scopeLine: 111, unit: !2, spFlags: DISPFlagDefinition)
!132 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_i32", linkageName: "Plus__uPlus_param_u64_i32", scope: !4, file: !3, line: 112, type: !6, scopeLine: 112, unit: !2, spFlags: DISPFlagDefinition)
!134 = distinct !DISubprogram(name: "Plus__Plus_param_u64_i32", linkageName: "Plus__Plus_param_u64_i32", scope: !4, file: !3, line: 113, type: !6, scopeLine: 113, unit: !2, spFlags: DISPFlagDefinition)
!136 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_LC", linkageName: "Plus__uPlus_var_u64_LC", scope: !4, file: !3, line: 114, type: !6, scopeLine: 114, unit: !2, spFlags: DISPFlagDefinition)
!138 = distinct !DISubprogram(name: "Plus__Plus_var_u64_LC", linkageName: "Plus__Plus_var_u64_LC", scope: !4, file: !3, line: 115, type: !6, scopeLine: 115, unit: !2, spFlags: DISPFlagDefinition)
!140 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_LC", linkageName: "Plus__uPlus_param_u64_LC", scope: !4, file: !3, line: 116, type: !6, scopeLine: 116, unit: !2, spFlags: DISPFlagDefinition)
!142 = distinct !DISubprogram(name: "Plus__Plus_param_u64_LC", linkageName: "Plus__Plus_param_u64_LC", scope: !4, file: !3, line: 117, type: !6, scopeLine: 117, unit: !2, spFlags: DISPFlagDefinition)
!144 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_u16", linkageName: "Plus__uPlus_var_u64_u16", scope: !4, file: !3, line: 118, type: !6, scopeLine: 118, unit: !2, spFlags: DISPFlagDefinition)
!146 = distinct !DISubprogram(name: "Plus__Plus_var_u64_u16", linkageName: "Plus__Plus_var_u64_u16", scope: !4, file: !3, line: 119, type: !6, scopeLine: 119, unit: !2, spFlags: DISPFlagDefinition)
!148 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_u16", linkageName: "Plus__uPlus_param_u64_u16", scope: !4, file: !3, line: 120, type: !6, scopeLine: 120, unit: !2, spFlags: DISPFlagDefinition)
!150 = distinct !DISubprogram(name: "Plus__Plus_param_u64_u16", linkageName: "Plus__Plus_param_u64_u16", scope: !4, file: !3, line: 121, type: !6, scopeLine: 121, unit: !2, spFlags: DISPFlagDefinition)
!152 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_I", linkageName: "Plus__uPlus_var_u64_I", scope: !4, file: !3, line: 122, type: !6, scopeLine: 122, unit: !2, spFlags: DISPFlagDefinition)
!154 = distinct !DISubprogram(name: "Plus__Plus_var_u64_I", linkageName: "Plus__Plus_var_u64_I", scope: !4, file: !3, line: 123, type: !6, scopeLine: 123, unit: !2, spFlags: DISPFlagDefinition)
!156 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_I", linkageName: "Plus__uPlus_param_u64_I", scope: !4, file: !3, line: 124, type: !6, scopeLine: 124, unit: !2, spFlags: DISPFlagDefinition)
!158 = distinct !DISubprogram(name: "Plus__Plus_param_u64_I", linkageName: "Plus__Plus_param_u64_I", scope: !4, file: !3, line: 125, type: !6, scopeLine: 125, unit: !2, spFlags: DISPFlagDefinition)
!160 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_i64", linkageName: "Plus__uPlus_var_u64_i64", scope: !4, file: !3, line: 126, type: !6, scopeLine: 126, unit: !2, spFlags: DISPFlagDefinition)
!162 = distinct !DISubprogram(name: "Plus__Plus_var_u64_i64", linkageName: "Plus__Plus_var_u64_i64", scope: !4, file: !3, line: 127, type: !6, scopeLine: 127, unit: !2, spFlags: DISPFlagDefinition)
!164 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_i64", linkageName: "Plus__uPlus_param_u64_i64", scope: !4, file: !3, line: 128, type: !6, scopeLine: 128, unit: !2, spFlags: DISPFlagDefinition)
!166 = distinct !DISubprogram(name: "Plus__Plus_param_u64_i64", linkageName: "Plus__Plus_param_u64_i64", scope: !4, file: !3, line: 129, type: !6, scopeLine: 129, unit: !2, spFlags: DISPFlagDefinition)
!168 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_i16", linkageName: "Plus__uPlus_var_u64_i16", scope: !4, file: !3, line: 130, type: !6, scopeLine: 130, unit: !2, spFlags: DISPFlagDefinition)
!170 = distinct !DISubprogram(name: "Plus__Plus_var_u64_i16", linkageName: "Plus__Plus_var_u64_i16", scope: !4, file: !3, line: 131, type: !6, scopeLine: 131, unit: !2, spFlags: DISPFlagDefinition)
!172 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_i16", linkageName: "Plus__uPlus_param_u64_i16", scope: !4, file: !3, line: 132, type: !6, scopeLine: 132, unit: !2, spFlags: DISPFlagDefinition)
!174 = distinct !DISubprogram(name: "Plus__Plus_param_u64_i16", linkageName: "Plus__Plus_param_u64_i16", scope: !4, file: !3, line: 133, type: !6, scopeLine: 133, unit: !2, spFlags: DISPFlagDefinition)
!176 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_C", linkageName: "Plus__uPlus_var_u64_C", scope: !4, file: !3, line: 134, type: !6, scopeLine: 134, unit: !2, spFlags: DISPFlagDefinition)
!178 = distinct !DISubprogram(name: "Plus__Plus_var_u64_C", linkageName: "Plus__Plus_var_u64_C", scope: !4, file: !3, line: 135, type: !6, scopeLine: 135, unit: !2, spFlags: DISPFlagDefinition)
!180 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_C", linkageName: "Plus__uPlus_param_u64_C", scope: !4, file: !3, line: 136, type: !6, scopeLine: 136, unit: !2, spFlags: DISPFlagDefinition)
!182 = distinct !DISubprogram(name: "Plus__Plus_param_u64_C", linkageName: "Plus__Plus_param_u64_C", scope: !4, file: !3, line: 137, type: !6, scopeLine: 137, unit: !2, spFlags: DISPFlagDefinition)
!184 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_u32", linkageName: "Plus__uPlus_var_u64_u32", scope: !4, file: !3, line: 138, type: !6, scopeLine: 138, unit: !2, spFlags: DISPFlagDefinition)
!186 = distinct !DISubprogram(name: "Plus__Plus_var_u64_u32", linkageName: "Plus__Plus_var_u64_u32", scope: !4, file: !3, line: 139, type: !6, scopeLine: 139, unit: !2, spFlags: DISPFlagDefinition)
!188 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_u32", linkageName: "Plus__uPlus_param_u64_u32", scope: !4, file: !3, line: 140, type: !6, scopeLine: 140, unit: !2, spFlags: DISPFlagDefinition)
!190 = distinct !DISubprogram(name: "Plus__Plus_param_u64_u32", linkageName: "Plus__Plus_param_u64_u32", scope: !4, file: !3, line: 141, type: !6, scopeLine: 141, unit: !2, spFlags: DISPFlagDefinition)
!192 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_u8", linkageName: "Plus__uPlus_var_u64_u8", scope: !4, file: !3, line: 142, type: !6, scopeLine: 142, unit: !2, spFlags: DISPFlagDefinition)
!194 = distinct !DISubprogram(name: "Plus__Plus_var_u64_u8", linkageName: "Plus__Plus_var_u64_u8", scope: !4, file: !3, line: 143, type: !6, scopeLine: 143, unit: !2, spFlags: DISPFlagDefinition)
!196 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_u8", linkageName: "Plus__uPlus_param_u64_u8", scope: !4, file: !3, line: 144, type: !6, scopeLine: 144, unit: !2, spFlags: DISPFlagDefinition)
!198 = distinct !DISubprogram(name: "Plus__Plus_param_u64_u8", linkageName: "Plus__Plus_param_u64_u8", scope: !4, file: !3, line: 145, type: !6, scopeLine: 145, unit: !2, spFlags: DISPFlagDefinition)
!200 = distinct !DISubprogram(name: "Plus__uPlus_var_u64_L", linkageName: "Plus__uPlus_var_u64_L", scope: !4, file: !3, line: 146, type: !6, scopeLine: 146, unit: !2, spFlags: DISPFlagDefinition)
!202 = distinct !DISubprogram(name: "Plus__Plus_var_u64_L", linkageName: "Plus__Plus_var_u64_L", scope: !4, file: !3, line: 147, type: !6, scopeLine: 147, unit: !2, spFlags: DISPFlagDefinition)
!204 = distinct !DISubprogram(name: "Plus__uPlus_param_u64_L", linkageName: "Plus__uPlus_param_u64_L", scope: !4, file: !3, line: 148, type: !6, scopeLine: 148, unit: !2, spFlags: DISPFlagDefinition)
!206 = distinct !DISubprogram(name: "Plus__Plus_param_u64_L", linkageName: "Plus__Plus_param_u64_L", scope: !4, file: !3, line: 149, type: !6, scopeLine: 149, unit: !2, spFlags: DISPFlagDefinition)
!208 = distinct !DISubprogram(name: "Plus__Plus_var_f64_f64", linkageName: "Plus__Plus_var_f64_f64", scope: !4, file: !3, line: 150, type: !6, scopeLine: 150, unit: !2, spFlags: DISPFlagDefinition)
!210 = distinct !DISubprogram(name: "Plus__Plus_param_f64_f64", linkageName: "Plus__Plus_param_f64_f64", scope: !4, file: !3, line: 151, type: !6, scopeLine: 151, unit: !2, spFlags: DISPFlagDefinition)
!212 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_i8", linkageName: "Plus__uPlus_var_i32_i8", scope: !4, file: !3, line: 152, type: !6, scopeLine: 152, unit: !2, spFlags: DISPFlagDefinition)
!214 = distinct !DISubprogram(name: "Plus__Plus_var_i32_i8", linkageName: "Plus__Plus_var_i32_i8", scope: !4, file: !3, line: 153, type: !6, scopeLine: 153, unit: !2, spFlags: DISPFlagDefinition)
!216 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_i8", linkageName: "Plus__uPlus_param_i32_i8", scope: !4, file: !3, line: 154, type: !6, scopeLine: 154, unit: !2, spFlags: DISPFlagDefinition)
!218 = distinct !DISubprogram(name: "Plus__Plus_param_i32_i8", linkageName: "Plus__Plus_param_i32_i8", scope: !4, file: !3, line: 155, type: !6, scopeLine: 155, unit: !2, spFlags: DISPFlagDefinition)
!220 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_u64", linkageName: "Plus__uPlus_var_i32_u64", scope: !4, file: !3, line: 156, type: !6, scopeLine: 156, unit: !2, spFlags: DISPFlagDefinition)
!222 = distinct !DISubprogram(name: "Plus__Plus_var_i32_u64", linkageName: "Plus__Plus_var_i32_u64", scope: !4, file: !3, line: 157, type: !6, scopeLine: 157, unit: !2, spFlags: DISPFlagDefinition)
!224 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_u64", linkageName: "Plus__uPlus_param_i32_u64", scope: !4, file: !3, line: 158, type: !6, scopeLine: 158, unit: !2, spFlags: DISPFlagDefinition)
!226 = distinct !DISubprogram(name: "Plus__Plus_param_i32_u64", linkageName: "Plus__Plus_param_i32_u64", scope: !4, file: !3, line: 159, type: !6, scopeLine: 159, unit: !2, spFlags: DISPFlagDefinition)
!228 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_i32", linkageName: "Plus__uPlus_var_i32_i32", scope: !4, file: !3, line: 160, type: !6, scopeLine: 160, unit: !2, spFlags: DISPFlagDefinition)
!230 = distinct !DISubprogram(name: "Plus__Plus_var_i32_i32", linkageName: "Plus__Plus_var_i32_i32", scope: !4, file: !3, line: 161, type: !6, scopeLine: 161, unit: !2, spFlags: DISPFlagDefinition)
!232 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_i32", linkageName: "Plus__uPlus_param_i32_i32", scope: !4, file: !3, line: 162, type: !6, scopeLine: 162, unit: !2, spFlags: DISPFlagDefinition)
!234 = distinct !DISubprogram(name: "Plus__Plus_param_i32_i32", linkageName: "Plus__Plus_param_i32_i32", scope: !4, file: !3, line: 163, type: !6, scopeLine: 163, unit: !2, spFlags: DISPFlagDefinition)
!236 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_LC", linkageName: "Plus__uPlus_var_i32_LC", scope: !4, file: !3, line: 164, type: !6, scopeLine: 164, unit: !2, spFlags: DISPFlagDefinition)
!238 = distinct !DISubprogram(name: "Plus__Plus_var_i32_LC", linkageName: "Plus__Plus_var_i32_LC", scope: !4, file: !3, line: 165, type: !6, scopeLine: 165, unit: !2, spFlags: DISPFlagDefinition)
!240 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_LC", linkageName: "Plus__uPlus_param_i32_LC", scope: !4, file: !3, line: 166, type: !6, scopeLine: 166, unit: !2, spFlags: DISPFlagDefinition)
!242 = distinct !DISubprogram(name: "Plus__Plus_param_i32_LC", linkageName: "Plus__Plus_param_i32_LC", scope: !4, file: !3, line: 167, type: !6, scopeLine: 167, unit: !2, spFlags: DISPFlagDefinition)
!244 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_u16", linkageName: "Plus__uPlus_var_i32_u16", scope: !4, file: !3, line: 168, type: !6, scopeLine: 168, unit: !2, spFlags: DISPFlagDefinition)
!246 = distinct !DISubprogram(name: "Plus__Plus_var_i32_u16", linkageName: "Plus__Plus_var_i32_u16", scope: !4, file: !3, line: 169, type: !6, scopeLine: 169, unit: !2, spFlags: DISPFlagDefinition)
!248 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_u16", linkageName: "Plus__uPlus_param_i32_u16", scope: !4, file: !3, line: 170, type: !6, scopeLine: 170, unit: !2, spFlags: DISPFlagDefinition)
!250 = distinct !DISubprogram(name: "Plus__Plus_param_i32_u16", linkageName: "Plus__Plus_param_i32_u16", scope: !4, file: !3, line: 171, type: !6, scopeLine: 171, unit: !2, spFlags: DISPFlagDefinition)
!252 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_I", linkageName: "Plus__uPlus_var_i32_I", scope: !4, file: !3, line: 172, type: !6, scopeLine: 172, unit: !2, spFlags: DISPFlagDefinition)
!254 = distinct !DISubprogram(name: "Plus__Plus_var_i32_I", linkageName: "Plus__Plus_var_i32_I", scope: !4, file: !3, line: 173, type: !6, scopeLine: 173, unit: !2, spFlags: DISPFlagDefinition)
!256 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_I", linkageName: "Plus__uPlus_param_i32_I", scope: !4, file: !3, line: 174, type: !6, scopeLine: 174, unit: !2, spFlags: DISPFlagDefinition)
!258 = distinct !DISubprogram(name: "Plus__Plus_param_i32_I", linkageName: "Plus__Plus_param_i32_I", scope: !4, file: !3, line: 175, type: !6, scopeLine: 175, unit: !2, spFlags: DISPFlagDefinition)
!260 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_i64", linkageName: "Plus__uPlus_var_i32_i64", scope: !4, file: !3, line: 176, type: !6, scopeLine: 176, unit: !2, spFlags: DISPFlagDefinition)
!262 = distinct !DISubprogram(name: "Plus__Plus_var_i32_i64", linkageName: "Plus__Plus_var_i32_i64", scope: !4, file: !3, line: 177, type: !6, scopeLine: 177, unit: !2, spFlags: DISPFlagDefinition)
!264 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_i64", linkageName: "Plus__uPlus_param_i32_i64", scope: !4, file: !3, line: 178, type: !6, scopeLine: 178, unit: !2, spFlags: DISPFlagDefinition)
!266 = distinct !DISubprogram(name: "Plus__Plus_param_i32_i64", linkageName: "Plus__Plus_param_i32_i64", scope: !4, file: !3, line: 179, type: !6, scopeLine: 179, unit: !2, spFlags: DISPFlagDefinition)
!268 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_i16", linkageName: "Plus__uPlus_var_i32_i16", scope: !4, file: !3, line: 180, type: !6, scopeLine: 180, unit: !2, spFlags: DISPFlagDefinition)
!270 = distinct !DISubprogram(name: "Plus__Plus_var_i32_i16", linkageName: "Plus__Plus_var_i32_i16", scope: !4, file: !3, line: 181, type: !6, scopeLine: 181, unit: !2, spFlags: DISPFlagDefinition)
!272 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_i16", linkageName: "Plus__uPlus_param_i32_i16", scope: !4, file: !3, line: 182, type: !6, scopeLine: 182, unit: !2, spFlags: DISPFlagDefinition)
!274 = distinct !DISubprogram(name: "Plus__Plus_param_i32_i16", linkageName: "Plus__Plus_param_i32_i16", scope: !4, file: !3, line: 183, type: !6, scopeLine: 183, unit: !2, spFlags: DISPFlagDefinition)
!276 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_C", linkageName: "Plus__uPlus_var_i32_C", scope: !4, file: !3, line: 184, type: !6, scopeLine: 184, unit: !2, spFlags: DISPFlagDefinition)
!278 = distinct !DISubprogram(name: "Plus__Plus_var_i32_C", linkageName: "Plus__Plus_var_i32_C", scope: !4, file: !3, line: 185, type: !6, scopeLine: 185, unit: !2, spFlags: DISPFlagDefinition)
!280 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_C", linkageName: "Plus__uPlus_param_i32_C", scope: !4, file: !3, line: 186, type: !6, scopeLine: 186, unit: !2, spFlags: DISPFlagDefinition)
!282 = distinct !DISubprogram(name: "Plus__Plus_param_i32_C", linkageName: "Plus__Plus_param_i32_C", scope: !4, file: !3, line: 187, type: !6, scopeLine: 187, unit: !2, spFlags: DISPFlagDefinition)
!284 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_u32", linkageName: "Plus__uPlus_var_i32_u32", scope: !4, file: !3, line: 188, type: !6, scopeLine: 188, unit: !2, spFlags: DISPFlagDefinition)
!286 = distinct !DISubprogram(name: "Plus__Plus_var_i32_u32", linkageName: "Plus__Plus_var_i32_u32", scope: !4, file: !3, line: 189, type: !6, scopeLine: 189, unit: !2, spFlags: DISPFlagDefinition)
!288 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_u32", linkageName: "Plus__uPlus_param_i32_u32", scope: !4, file: !3, line: 190, type: !6, scopeLine: 190, unit: !2, spFlags: DISPFlagDefinition)
!290 = distinct !DISubprogram(name: "Plus__Plus_param_i32_u32", linkageName: "Plus__Plus_param_i32_u32", scope: !4, file: !3, line: 191, type: !6, scopeLine: 191, unit: !2, spFlags: DISPFlagDefinition)
!292 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_u8", linkageName: "Plus__uPlus_var_i32_u8", scope: !4, file: !3, line: 192, type: !6, scopeLine: 192, unit: !2, spFlags: DISPFlagDefinition)
!294 = distinct !DISubprogram(name: "Plus__Plus_var_i32_u8", linkageName: "Plus__Plus_var_i32_u8", scope: !4, file: !3, line: 193, type: !6, scopeLine: 193, unit: !2, spFlags: DISPFlagDefinition)
!296 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_u8", linkageName: "Plus__uPlus_param_i32_u8", scope: !4, file: !3, line: 194, type: !6, scopeLine: 194, unit: !2, spFlags: DISPFlagDefinition)
!298 = distinct !DISubprogram(name: "Plus__Plus_param_i32_u8", linkageName: "Plus__Plus_param_i32_u8", scope: !4, file: !3, line: 195, type: !6, scopeLine: 195, unit: !2, spFlags: DISPFlagDefinition)
!300 = distinct !DISubprogram(name: "Plus__uPlus_var_i32_L", linkageName: "Plus__uPlus_var_i32_L", scope: !4, file: !3, line: 196, type: !6, scopeLine: 196, unit: !2, spFlags: DISPFlagDefinition)
!302 = distinct !DISubprogram(name: "Plus__Plus_var_i32_L", linkageName: "Plus__Plus_var_i32_L", scope: !4, file: !3, line: 197, type: !6, scopeLine: 197, unit: !2, spFlags: DISPFlagDefinition)
!304 = distinct !DISubprogram(name: "Plus__uPlus_param_i32_L", linkageName: "Plus__uPlus_param_i32_L", scope: !4, file: !3, line: 198, type: !6, scopeLine: 198, unit: !2, spFlags: DISPFlagDefinition)
!306 = distinct !DISubprogram(name: "Plus__Plus_param_i32_L", linkageName: "Plus__Plus_param_i32_L", scope: !4, file: !3, line: 199, type: !6, scopeLine: 199, unit: !2, spFlags: DISPFlagDefinition)
!308 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_i8", linkageName: "Plus__uPlus_var_LC_i8", scope: !4, file: !3, line: 200, type: !6, scopeLine: 200, unit: !2, spFlags: DISPFlagDefinition)
!310 = distinct !DISubprogram(name: "Plus__Plus_var_LC_i8", linkageName: "Plus__Plus_var_LC_i8", scope: !4, file: !3, line: 201, type: !6, scopeLine: 201, unit: !2, spFlags: DISPFlagDefinition)
!312 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_i8", linkageName: "Plus__uPlus_param_LC_i8", scope: !4, file: !3, line: 202, type: !6, scopeLine: 202, unit: !2, spFlags: DISPFlagDefinition)
!314 = distinct !DISubprogram(name: "Plus__Plus_param_LC_i8", linkageName: "Plus__Plus_param_LC_i8", scope: !4, file: !3, line: 203, type: !6, scopeLine: 203, unit: !2, spFlags: DISPFlagDefinition)
!316 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_u64", linkageName: "Plus__uPlus_var_LC_u64", scope: !4, file: !3, line: 204, type: !6, scopeLine: 204, unit: !2, spFlags: DISPFlagDefinition)
!318 = distinct !DISubprogram(name: "Plus__Plus_var_LC_u64", linkageName: "Plus__Plus_var_LC_u64", scope: !4, file: !3, line: 205, type: !6, scopeLine: 205, unit: !2, spFlags: DISPFlagDefinition)
!320 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_u64", linkageName: "Plus__uPlus_param_LC_u64", scope: !4, file: !3, line: 206, type: !6, scopeLine: 206, unit: !2, spFlags: DISPFlagDefinition)
!322 = distinct !DISubprogram(name: "Plus__Plus_param_LC_u64", linkageName: "Plus__Plus_param_LC_u64", scope: !4, file: !3, line: 207, type: !6, scopeLine: 207, unit: !2, spFlags: DISPFlagDefinition)
!324 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_i32", linkageName: "Plus__uPlus_var_LC_i32", scope: !4, file: !3, line: 208, type: !6, scopeLine: 208, unit: !2, spFlags: DISPFlagDefinition)
!326 = distinct !DISubprogram(name: "Plus__Plus_var_LC_i32", linkageName: "Plus__Plus_var_LC_i32", scope: !4, file: !3, line: 209, type: !6, scopeLine: 209, unit: !2, spFlags: DISPFlagDefinition)
!328 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_i32", linkageName: "Plus__uPlus_param_LC_i32", scope: !4, file: !3, line: 210, type: !6, scopeLine: 210, unit: !2, spFlags: DISPFlagDefinition)
!330 = distinct !DISubprogram(name: "Plus__Plus_param_LC_i32", linkageName: "Plus__Plus_param_LC_i32", scope: !4, file: !3, line: 211, type: !6, scopeLine: 211, unit: !2, spFlags: DISPFlagDefinition)
!332 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_LC", linkageName: "Plus__uPlus_var_LC_LC", scope: !4, file: !3, line: 212, type: !6, scopeLine: 212, unit: !2, spFlags: DISPFlagDefinition)
!334 = distinct !DISubprogram(name: "Plus__Plus_var_LC_LC", linkageName: "Plus__Plus_var_LC_LC", scope: !4, file: !3, line: 213, type: !6, scopeLine: 213, unit: !2, spFlags: DISPFlagDefinition)
!336 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_LC", linkageName: "Plus__uPlus_param_LC_LC", scope: !4, file: !3, line: 214, type: !6, scopeLine: 214, unit: !2, spFlags: DISPFlagDefinition)
!338 = distinct !DISubprogram(name: "Plus__Plus_param_LC_LC", linkageName: "Plus__Plus_param_LC_LC", scope: !4, file: !3, line: 215, type: !6, scopeLine: 215, unit: !2, spFlags: DISPFlagDefinition)
!340 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_u16", linkageName: "Plus__uPlus_var_LC_u16", scope: !4, file: !3, line: 216, type: !6, scopeLine: 216, unit: !2, spFlags: DISPFlagDefinition)
!342 = distinct !DISubprogram(name: "Plus__Plus_var_LC_u16", linkageName: "Plus__Plus_var_LC_u16", scope: !4, file: !3, line: 217, type: !6, scopeLine: 217, unit: !2, spFlags: DISPFlagDefinition)
!344 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_u16", linkageName: "Plus__uPlus_param_LC_u16", scope: !4, file: !3, line: 218, type: !6, scopeLine: 218, unit: !2, spFlags: DISPFlagDefinition)
!346 = distinct !DISubprogram(name: "Plus__Plus_param_LC_u16", linkageName: "Plus__Plus_param_LC_u16", scope: !4, file: !3, line: 219, type: !6, scopeLine: 219, unit: !2, spFlags: DISPFlagDefinition)
!348 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_I", linkageName: "Plus__uPlus_var_LC_I", scope: !4, file: !3, line: 220, type: !6, scopeLine: 220, unit: !2, spFlags: DISPFlagDefinition)
!350 = distinct !DISubprogram(name: "Plus__Plus_var_LC_I", linkageName: "Plus__Plus_var_LC_I", scope: !4, file: !3, line: 221, type: !6, scopeLine: 221, unit: !2, spFlags: DISPFlagDefinition)
!352 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_I", linkageName: "Plus__uPlus_param_LC_I", scope: !4, file: !3, line: 222, type: !6, scopeLine: 222, unit: !2, spFlags: DISPFlagDefinition)
!354 = distinct !DISubprogram(name: "Plus__Plus_param_LC_I", linkageName: "Plus__Plus_param_LC_I", scope: !4, file: !3, line: 223, type: !6, scopeLine: 223, unit: !2, spFlags: DISPFlagDefinition)
!356 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_i64", linkageName: "Plus__uPlus_var_LC_i64", scope: !4, file: !3, line: 224, type: !6, scopeLine: 224, unit: !2, spFlags: DISPFlagDefinition)
!358 = distinct !DISubprogram(name: "Plus__Plus_var_LC_i64", linkageName: "Plus__Plus_var_LC_i64", scope: !4, file: !3, line: 225, type: !6, scopeLine: 225, unit: !2, spFlags: DISPFlagDefinition)
!360 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_i64", linkageName: "Plus__uPlus_param_LC_i64", scope: !4, file: !3, line: 226, type: !6, scopeLine: 226, unit: !2, spFlags: DISPFlagDefinition)
!362 = distinct !DISubprogram(name: "Plus__Plus_param_LC_i64", linkageName: "Plus__Plus_param_LC_i64", scope: !4, file: !3, line: 227, type: !6, scopeLine: 227, unit: !2, spFlags: DISPFlagDefinition)
!364 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_i16", linkageName: "Plus__uPlus_var_LC_i16", scope: !4, file: !3, line: 228, type: !6, scopeLine: 228, unit: !2, spFlags: DISPFlagDefinition)
!366 = distinct !DISubprogram(name: "Plus__Plus_var_LC_i16", linkageName: "Plus__Plus_var_LC_i16", scope: !4, file: !3, line: 229, type: !6, scopeLine: 229, unit: !2, spFlags: DISPFlagDefinition)
!368 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_i16", linkageName: "Plus__uPlus_param_LC_i16", scope: !4, file: !3, line: 230, type: !6, scopeLine: 230, unit: !2, spFlags: DISPFlagDefinition)
!370 = distinct !DISubprogram(name: "Plus__Plus_param_LC_i16", linkageName: "Plus__Plus_param_LC_i16", scope: !4, file: !3, line: 231, type: !6, scopeLine: 231, unit: !2, spFlags: DISPFlagDefinition)
!372 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_C", linkageName: "Plus__uPlus_var_LC_C", scope: !4, file: !3, line: 232, type: !6, scopeLine: 232, unit: !2, spFlags: DISPFlagDefinition)
!374 = distinct !DISubprogram(name: "Plus__Plus_var_LC_C", linkageName: "Plus__Plus_var_LC_C", scope: !4, file: !3, line: 233, type: !6, scopeLine: 233, unit: !2, spFlags: DISPFlagDefinition)
!376 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_C", linkageName: "Plus__uPlus_param_LC_C", scope: !4, file: !3, line: 234, type: !6, scopeLine: 234, unit: !2, spFlags: DISPFlagDefinition)
!378 = distinct !DISubprogram(name: "Plus__Plus_param_LC_C", linkageName: "Plus__Plus_param_LC_C", scope: !4, file: !3, line: 235, type: !6, scopeLine: 235, unit: !2, spFlags: DISPFlagDefinition)
!380 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_u32", linkageName: "Plus__uPlus_var_LC_u32", scope: !4, file: !3, line: 236, type: !6, scopeLine: 236, unit: !2, spFlags: DISPFlagDefinition)
!382 = distinct !DISubprogram(name: "Plus__Plus_var_LC_u32", linkageName: "Plus__Plus_var_LC_u32", scope: !4, file: !3, line: 237, type: !6, scopeLine: 237, unit: !2, spFlags: DISPFlagDefinition)
!384 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_u32", linkageName: "Plus__uPlus_param_LC_u32", scope: !4, file: !3, line: 238, type: !6, scopeLine: 238, unit: !2, spFlags: DISPFlagDefinition)
!386 = distinct !DISubprogram(name: "Plus__Plus_param_LC_u32", linkageName: "Plus__Plus_param_LC_u32", scope: !4, file: !3, line: 239, type: !6, scopeLine: 239, unit: !2, spFlags: DISPFlagDefinition)
!388 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_u8", linkageName: "Plus__uPlus_var_LC_u8", scope: !4, file: !3, line: 240, type: !6, scopeLine: 240, unit: !2, spFlags: DISPFlagDefinition)
!390 = distinct !DISubprogram(name: "Plus__Plus_var_LC_u8", linkageName: "Plus__Plus_var_LC_u8", scope: !4, file: !3, line: 241, type: !6, scopeLine: 241, unit: !2, spFlags: DISPFlagDefinition)
!392 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_u8", linkageName: "Plus__uPlus_param_LC_u8", scope: !4, file: !3, line: 242, type: !6, scopeLine: 242, unit: !2, spFlags: DISPFlagDefinition)
!394 = distinct !DISubprogram(name: "Plus__Plus_param_LC_u8", linkageName: "Plus__Plus_param_LC_u8", scope: !4, file: !3, line: 243, type: !6, scopeLine: 243, unit: !2, spFlags: DISPFlagDefinition)
!396 = distinct !DISubprogram(name: "Plus__uPlus_var_LC_L", linkageName: "Plus__uPlus_var_LC_L", scope: !4, file: !3, line: 244, type: !6, scopeLine: 244, unit: !2, spFlags: DISPFlagDefinition)
!398 = distinct !DISubprogram(name: "Plus__Plus_var_LC_L", linkageName: "Plus__Plus_var_LC_L", scope: !4, file: !3, line: 245, type: !6, scopeLine: 245, unit: !2, spFlags: DISPFlagDefinition)
!400 = distinct !DISubprogram(name: "Plus__uPlus_param_LC_L", linkageName: "Plus__uPlus_param_LC_L", scope: !4, file: !3, line: 246, type: !6, scopeLine: 246, unit: !2, spFlags: DISPFlagDefinition)
!402 = distinct !DISubprogram(name: "Plus__Plus_param_LC_L", linkageName: "Plus__Plus_param_LC_L", scope: !4, file: !3, line: 247, type: !6, scopeLine: 247, unit: !2, spFlags: DISPFlagDefinition)
!404 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_i8", linkageName: "Plus__uPlus_var_u16_i8", scope: !4, file: !3, line: 248, type: !6, scopeLine: 248, unit: !2, spFlags: DISPFlagDefinition)
!406 = distinct !DISubprogram(name: "Plus__Plus_var_u16_i8", linkageName: "Plus__Plus_var_u16_i8", scope: !4, file: !3, line: 249, type: !6, scopeLine: 249, unit: !2, spFlags: DISPFlagDefinition)
!408 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_i8", linkageName: "Plus__uPlus_param_u16_i8", scope: !4, file: !3, line: 250, type: !6, scopeLine: 250, unit: !2, spFlags: DISPFlagDefinition)
!410 = distinct !DISubprogram(name: "Plus__Plus_param_u16_i8", linkageName: "Plus__Plus_param_u16_i8", scope: !4, file: !3, line: 251, type: !6, scopeLine: 251, unit: !2, spFlags: DISPFlagDefinition)
!412 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_u64", linkageName: "Plus__uPlus_var_u16_u64", scope: !4, file: !3, line: 252, type: !6, scopeLine: 252, unit: !2, spFlags: DISPFlagDefinition)
!414 = distinct !DISubprogram(name: "Plus__Plus_var_u16_u64", linkageName: "Plus__Plus_var_u16_u64", scope: !4, file: !3, line: 253, type: !6, scopeLine: 253, unit: !2, spFlags: DISPFlagDefinition)
!416 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_u64", linkageName: "Plus__uPlus_param_u16_u64", scope: !4, file: !3, line: 254, type: !6, scopeLine: 254, unit: !2, spFlags: DISPFlagDefinition)
!418 = distinct !DISubprogram(name: "Plus__Plus_param_u16_u64", linkageName: "Plus__Plus_param_u16_u64", scope: !4, file: !3, line: 255, type: !6, scopeLine: 255, unit: !2, spFlags: DISPFlagDefinition)
!420 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_i32", linkageName: "Plus__uPlus_var_u16_i32", scope: !4, file: !3, line: 256, type: !6, scopeLine: 256, unit: !2, spFlags: DISPFlagDefinition)
!422 = distinct !DISubprogram(name: "Plus__Plus_var_u16_i32", linkageName: "Plus__Plus_var_u16_i32", scope: !4, file: !3, line: 257, type: !6, scopeLine: 257, unit: !2, spFlags: DISPFlagDefinition)
!424 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_i32", linkageName: "Plus__uPlus_param_u16_i32", scope: !4, file: !3, line: 258, type: !6, scopeLine: 258, unit: !2, spFlags: DISPFlagDefinition)
!426 = distinct !DISubprogram(name: "Plus__Plus_param_u16_i32", linkageName: "Plus__Plus_param_u16_i32", scope: !4, file: !3, line: 259, type: !6, scopeLine: 259, unit: !2, spFlags: DISPFlagDefinition)
!428 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_LC", linkageName: "Plus__uPlus_var_u16_LC", scope: !4, file: !3, line: 260, type: !6, scopeLine: 260, unit: !2, spFlags: DISPFlagDefinition)
!430 = distinct !DISubprogram(name: "Plus__Plus_var_u16_LC", linkageName: "Plus__Plus_var_u16_LC", scope: !4, file: !3, line: 261, type: !6, scopeLine: 261, unit: !2, spFlags: DISPFlagDefinition)
!432 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_LC", linkageName: "Plus__uPlus_param_u16_LC", scope: !4, file: !3, line: 262, type: !6, scopeLine: 262, unit: !2, spFlags: DISPFlagDefinition)
!434 = distinct !DISubprogram(name: "Plus__Plus_param_u16_LC", linkageName: "Plus__Plus_param_u16_LC", scope: !4, file: !3, line: 263, type: !6, scopeLine: 263, unit: !2, spFlags: DISPFlagDefinition)
!436 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_u16", linkageName: "Plus__uPlus_var_u16_u16", scope: !4, file: !3, line: 264, type: !6, scopeLine: 264, unit: !2, spFlags: DISPFlagDefinition)
!438 = distinct !DISubprogram(name: "Plus__Plus_var_u16_u16", linkageName: "Plus__Plus_var_u16_u16", scope: !4, file: !3, line: 265, type: !6, scopeLine: 265, unit: !2, spFlags: DISPFlagDefinition)
!440 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_u16", linkageName: "Plus__uPlus_param_u16_u16", scope: !4, file: !3, line: 266, type: !6, scopeLine: 266, unit: !2, spFlags: DISPFlagDefinition)
!442 = distinct !DISubprogram(name: "Plus__Plus_param_u16_u16", linkageName: "Plus__Plus_param_u16_u16", scope: !4, file: !3, line: 267, type: !6, scopeLine: 267, unit: !2, spFlags: DISPFlagDefinition)
!444 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_I", linkageName: "Plus__uPlus_var_u16_I", scope: !4, file: !3, line: 268, type: !6, scopeLine: 268, unit: !2, spFlags: DISPFlagDefinition)
!446 = distinct !DISubprogram(name: "Plus__Plus_var_u16_I", linkageName: "Plus__Plus_var_u16_I", scope: !4, file: !3, line: 269, type: !6, scopeLine: 269, unit: !2, spFlags: DISPFlagDefinition)
!448 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_I", linkageName: "Plus__uPlus_param_u16_I", scope: !4, file: !3, line: 270, type: !6, scopeLine: 270, unit: !2, spFlags: DISPFlagDefinition)
!450 = distinct !DISubprogram(name: "Plus__Plus_param_u16_I", linkageName: "Plus__Plus_param_u16_I", scope: !4, file: !3, line: 271, type: !6, scopeLine: 271, unit: !2, spFlags: DISPFlagDefinition)
!452 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_i64", linkageName: "Plus__uPlus_var_u16_i64", scope: !4, file: !3, line: 272, type: !6, scopeLine: 272, unit: !2, spFlags: DISPFlagDefinition)
!454 = distinct !DISubprogram(name: "Plus__Plus_var_u16_i64", linkageName: "Plus__Plus_var_u16_i64", scope: !4, file: !3, line: 273, type: !6, scopeLine: 273, unit: !2, spFlags: DISPFlagDefinition)
!456 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_i64", linkageName: "Plus__uPlus_param_u16_i64", scope: !4, file: !3, line: 274, type: !6, scopeLine: 274, unit: !2, spFlags: DISPFlagDefinition)
!458 = distinct !DISubprogram(name: "Plus__Plus_param_u16_i64", linkageName: "Plus__Plus_param_u16_i64", scope: !4, file: !3, line: 275, type: !6, scopeLine: 275, unit: !2, spFlags: DISPFlagDefinition)
!460 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_i16", linkageName: "Plus__uPlus_var_u16_i16", scope: !4, file: !3, line: 276, type: !6, scopeLine: 276, unit: !2, spFlags: DISPFlagDefinition)
!462 = distinct !DISubprogram(name: "Plus__Plus_var_u16_i16", linkageName: "Plus__Plus_var_u16_i16", scope: !4, file: !3, line: 277, type: !6, scopeLine: 277, unit: !2, spFlags: DISPFlagDefinition)
!464 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_i16", linkageName: "Plus__uPlus_param_u16_i16", scope: !4, file: !3, line: 278, type: !6, scopeLine: 278, unit: !2, spFlags: DISPFlagDefinition)
!466 = distinct !DISubprogram(name: "Plus__Plus_param_u16_i16", linkageName: "Plus__Plus_param_u16_i16", scope: !4, file: !3, line: 279, type: !6, scopeLine: 279, unit: !2, spFlags: DISPFlagDefinition)
!468 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_C", linkageName: "Plus__uPlus_var_u16_C", scope: !4, file: !3, line: 280, type: !6, scopeLine: 280, unit: !2, spFlags: DISPFlagDefinition)
!470 = distinct !DISubprogram(name: "Plus__Plus_var_u16_C", linkageName: "Plus__Plus_var_u16_C", scope: !4, file: !3, line: 281, type: !6, scopeLine: 281, unit: !2, spFlags: DISPFlagDefinition)
!472 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_C", linkageName: "Plus__uPlus_param_u16_C", scope: !4, file: !3, line: 282, type: !6, scopeLine: 282, unit: !2, spFlags: DISPFlagDefinition)
!474 = distinct !DISubprogram(name: "Plus__Plus_param_u16_C", linkageName: "Plus__Plus_param_u16_C", scope: !4, file: !3, line: 283, type: !6, scopeLine: 283, unit: !2, spFlags: DISPFlagDefinition)
!476 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_u32", linkageName: "Plus__uPlus_var_u16_u32", scope: !4, file: !3, line: 284, type: !6, scopeLine: 284, unit: !2, spFlags: DISPFlagDefinition)
!478 = distinct !DISubprogram(name: "Plus__Plus_var_u16_u32", linkageName: "Plus__Plus_var_u16_u32", scope: !4, file: !3, line: 285, type: !6, scopeLine: 285, unit: !2, spFlags: DISPFlagDefinition)
!480 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_u32", linkageName: "Plus__uPlus_param_u16_u32", scope: !4, file: !3, line: 286, type: !6, scopeLine: 286, unit: !2, spFlags: DISPFlagDefinition)
!482 = distinct !DISubprogram(name: "Plus__Plus_param_u16_u32", linkageName: "Plus__Plus_param_u16_u32", scope: !4, file: !3, line: 287, type: !6, scopeLine: 287, unit: !2, spFlags: DISPFlagDefinition)
!484 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_u8", linkageName: "Plus__uPlus_var_u16_u8", scope: !4, file: !3, line: 288, type: !6, scopeLine: 288, unit: !2, spFlags: DISPFlagDefinition)
!486 = distinct !DISubprogram(name: "Plus__Plus_var_u16_u8", linkageName: "Plus__Plus_var_u16_u8", scope: !4, file: !3, line: 289, type: !6, scopeLine: 289, unit: !2, spFlags: DISPFlagDefinition)
!488 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_u8", linkageName: "Plus__uPlus_param_u16_u8", scope: !4, file: !3, line: 290, type: !6, scopeLine: 290, unit: !2, spFlags: DISPFlagDefinition)
!490 = distinct !DISubprogram(name: "Plus__Plus_param_u16_u8", linkageName: "Plus__Plus_param_u16_u8", scope: !4, file: !3, line: 291, type: !6, scopeLine: 291, unit: !2, spFlags: DISPFlagDefinition)
!492 = distinct !DISubprogram(name: "Plus__uPlus_var_u16_L", linkageName: "Plus__uPlus_var_u16_L", scope: !4, file: !3, line: 292, type: !6, scopeLine: 292, unit: !2, spFlags: DISPFlagDefinition)
!494 = distinct !DISubprogram(name: "Plus__Plus_var_u16_L", linkageName: "Plus__Plus_var_u16_L", scope: !4, file: !3, line: 293, type: !6, scopeLine: 293, unit: !2, spFlags: DISPFlagDefinition)
!496 = distinct !DISubprogram(name: "Plus__uPlus_param_u16_L", linkageName: "Plus__uPlus_param_u16_L", scope: !4, file: !3, line: 294, type: !6, scopeLine: 294, unit: !2, spFlags: DISPFlagDefinition)
!498 = distinct !DISubprogram(name: "Plus__Plus_param_u16_L", linkageName: "Plus__Plus_param_u16_L", scope: !4, file: !3, line: 295, type: !6, scopeLine: 295, unit: !2, spFlags: DISPFlagDefinition)
!500 = distinct !DISubprogram(name: "Plus__uPlus_var_I_i8", linkageName: "Plus__uPlus_var_I_i8", scope: !4, file: !3, line: 296, type: !6, scopeLine: 296, unit: !2, spFlags: DISPFlagDefinition)
!502 = distinct !DISubprogram(name: "Plus__Plus_var_I_i8", linkageName: "Plus__Plus_var_I_i8", scope: !4, file: !3, line: 297, type: !6, scopeLine: 297, unit: !2, spFlags: DISPFlagDefinition)
!504 = distinct !DISubprogram(name: "Plus__uPlus_param_I_i8", linkageName: "Plus__uPlus_param_I_i8", scope: !4, file: !3, line: 298, type: !6, scopeLine: 298, unit: !2, spFlags: DISPFlagDefinition)
!506 = distinct !DISubprogram(name: "Plus__Plus_param_I_i8", linkageName: "Plus__Plus_param_I_i8", scope: !4, file: !3, line: 299, type: !6, scopeLine: 299, unit: !2, spFlags: DISPFlagDefinition)
!508 = distinct !DISubprogram(name: "Plus__uPlus_var_I_u64", linkageName: "Plus__uPlus_var_I_u64", scope: !4, file: !3, line: 300, type: !6, scopeLine: 300, unit: !2, spFlags: DISPFlagDefinition)
!510 = distinct !DISubprogram(name: "Plus__Plus_var_I_u64", linkageName: "Plus__Plus_var_I_u64", scope: !4, file: !3, line: 301, type: !6, scopeLine: 301, unit: !2, spFlags: DISPFlagDefinition)
!512 = distinct !DISubprogram(name: "Plus__uPlus_param_I_u64", linkageName: "Plus__uPlus_param_I_u64", scope: !4, file: !3, line: 302, type: !6, scopeLine: 302, unit: !2, spFlags: DISPFlagDefinition)
!514 = distinct !DISubprogram(name: "Plus__Plus_param_I_u64", linkageName: "Plus__Plus_param_I_u64", scope: !4, file: !3, line: 303, type: !6, scopeLine: 303, unit: !2, spFlags: DISPFlagDefinition)
!516 = distinct !DISubprogram(name: "Plus__uPlus_var_I_i32", linkageName: "Plus__uPlus_var_I_i32", scope: !4, file: !3, line: 304, type: !6, scopeLine: 304, unit: !2, spFlags: DISPFlagDefinition)
!518 = distinct !DISubprogram(name: "Plus__Plus_var_I_i32", linkageName: "Plus__Plus_var_I_i32", scope: !4, file: !3, line: 305, type: !6, scopeLine: 305, unit: !2, spFlags: DISPFlagDefinition)
!520 = distinct !DISubprogram(name: "Plus__uPlus_param_I_i32", linkageName: "Plus__uPlus_param_I_i32", scope: !4, file: !3, line: 306, type: !6, scopeLine: 306, unit: !2, spFlags: DISPFlagDefinition)
!522 = distinct !DISubprogram(name: "Plus__Plus_param_I_i32", linkageName: "Plus__Plus_param_I_i32", scope: !4, file: !3, line: 307, type: !6, scopeLine: 307, unit: !2, spFlags: DISPFlagDefinition)
!524 = distinct !DISubprogram(name: "Plus__uPlus_var_I_LC", linkageName: "Plus__uPlus_var_I_LC", scope: !4, file: !3, line: 308, type: !6, scopeLine: 308, unit: !2, spFlags: DISPFlagDefinition)
!526 = distinct !DISubprogram(name: "Plus__Plus_var_I_LC", linkageName: "Plus__Plus_var_I_LC", scope: !4, file: !3, line: 309, type: !6, scopeLine: 309, unit: !2, spFlags: DISPFlagDefinition)
!528 = distinct !DISubprogram(name: "Plus__uPlus_param_I_LC", linkageName: "Plus__uPlus_param_I_LC", scope: !4, file: !3, line: 310, type: !6, scopeLine: 310, unit: !2, spFlags: DISPFlagDefinition)
!530 = distinct !DISubprogram(name: "Plus__Plus_param_I_LC", linkageName: "Plus__Plus_param_I_LC", scope: !4, file: !3, line: 311, type: !6, scopeLine: 311, unit: !2, spFlags: DISPFlagDefinition)
!532 = distinct !DISubprogram(name: "Plus__uPlus_var_I_u16", linkageName: "Plus__uPlus_var_I_u16", scope: !4, file: !3, line: 312, type: !6, scopeLine: 312, unit: !2, spFlags: DISPFlagDefinition)
!534 = distinct !DISubprogram(name: "Plus__Plus_var_I_u16", linkageName: "Plus__Plus_var_I_u16", scope: !4, file: !3, line: 313, type: !6, scopeLine: 313, unit: !2, spFlags: DISPFlagDefinition)
!536 = distinct !DISubprogram(name: "Plus__uPlus_param_I_u16", linkageName: "Plus__uPlus_param_I_u16", scope: !4, file: !3, line: 314, type: !6, scopeLine: 314, unit: !2, spFlags: DISPFlagDefinition)
!538 = distinct !DISubprogram(name: "Plus__Plus_param_I_u16", linkageName: "Plus__Plus_param_I_u16", scope: !4, file: !3, line: 315, type: !6, scopeLine: 315, unit: !2, spFlags: DISPFlagDefinition)
!540 = distinct !DISubprogram(name: "Plus__uPlus_var_I_I", linkageName: "Plus__uPlus_var_I_I", scope: !4, file: !3, line: 316, type: !6, scopeLine: 316, unit: !2, spFlags: DISPFlagDefinition)
!542 = distinct !DISubprogram(name: "Plus__Plus_var_I_I", linkageName: "Plus__Plus_var_I_I", scope: !4, file: !3, line: 317, type: !6, scopeLine: 317, unit: !2, spFlags: DISPFlagDefinition)
!544 = distinct !DISubprogram(name: "Plus__uPlus_param_I_I", linkageName: "Plus__uPlus_param_I_I", scope: !4, file: !3, line: 318, type: !6, scopeLine: 318, unit: !2, spFlags: DISPFlagDefinition)
!546 = distinct !DISubprogram(name: "Plus__Plus_param_I_I", linkageName: "Plus__Plus_param_I_I", scope: !4, file: !3, line: 319, type: !6, scopeLine: 319, unit: !2, spFlags: DISPFlagDefinition)
!548 = distinct !DISubprogram(name: "Plus__uPlus_var_I_i64", linkageName: "Plus__uPlus_var_I_i64", scope: !4, file: !3, line: 320, type: !6, scopeLine: 320, unit: !2, spFlags: DISPFlagDefinition)
!550 = distinct !DISubprogram(name: "Plus__Plus_var_I_i64", linkageName: "Plus__Plus_var_I_i64", scope: !4, file: !3, line: 321, type: !6, scopeLine: 321, unit: !2, spFlags: DISPFlagDefinition)
!552 = distinct !DISubprogram(name: "Plus__uPlus_param_I_i64", linkageName: "Plus__uPlus_param_I_i64", scope: !4, file: !3, line: 322, type: !6, scopeLine: 322, unit: !2, spFlags: DISPFlagDefinition)
!554 = distinct !DISubprogram(name: "Plus__Plus_param_I_i64", linkageName: "Plus__Plus_param_I_i64", scope: !4, file: !3, line: 323, type: !6, scopeLine: 323, unit: !2, spFlags: DISPFlagDefinition)
!556 = distinct !DISubprogram(name: "Plus__uPlus_var_I_i16", linkageName: "Plus__uPlus_var_I_i16", scope: !4, file: !3, line: 324, type: !6, scopeLine: 324, unit: !2, spFlags: DISPFlagDefinition)
!558 = distinct !DISubprogram(name: "Plus__Plus_var_I_i16", linkageName: "Plus__Plus_var_I_i16", scope: !4, file: !3, line: 325, type: !6, scopeLine: 325, unit: !2, spFlags: DISPFlagDefinition)
!560 = distinct !DISubprogram(name: "Plus__uPlus_param_I_i16", linkageName: "Plus__uPlus_param_I_i16", scope: !4, file: !3, line: 326, type: !6, scopeLine: 326, unit: !2, spFlags: DISPFlagDefinition)
!562 = distinct !DISubprogram(name: "Plus__Plus_param_I_i16", linkageName: "Plus__Plus_param_I_i16", scope: !4, file: !3, line: 327, type: !6, scopeLine: 327, unit: !2, spFlags: DISPFlagDefinition)
!564 = distinct !DISubprogram(name: "Plus__uPlus_var_I_C", linkageName: "Plus__uPlus_var_I_C", scope: !4, file: !3, line: 328, type: !6, scopeLine: 328, unit: !2, spFlags: DISPFlagDefinition)
!566 = distinct !DISubprogram(name: "Plus__Plus_var_I_C", linkageName: "Plus__Plus_var_I_C", scope: !4, file: !3, line: 329, type: !6, scopeLine: 329, unit: !2, spFlags: DISPFlagDefinition)
!568 = distinct !DISubprogram(name: "Plus__uPlus_param_I_C", linkageName: "Plus__uPlus_param_I_C", scope: !4, file: !3, line: 330, type: !6, scopeLine: 330, unit: !2, spFlags: DISPFlagDefinition)
!570 = distinct !DISubprogram(name: "Plus__Plus_param_I_C", linkageName: "Plus__Plus_param_I_C", scope: !4, file: !3, line: 331, type: !6, scopeLine: 331, unit: !2, spFlags: DISPFlagDefinition)
!572 = distinct !DISubprogram(name: "Plus__uPlus_var_I_u32", linkageName: "Plus__uPlus_var_I_u32", scope: !4, file: !3, line: 332, type: !6, scopeLine: 332, unit: !2, spFlags: DISPFlagDefinition)
!574 = distinct !DISubprogram(name: "Plus__Plus_var_I_u32", linkageName: "Plus__Plus_var_I_u32", scope: !4, file: !3, line: 333, type: !6, scopeLine: 333, unit: !2, spFlags: DISPFlagDefinition)
!576 = distinct !DISubprogram(name: "Plus__uPlus_param_I_u32", linkageName: "Plus__uPlus_param_I_u32", scope: !4, file: !3, line: 334, type: !6, scopeLine: 334, unit: !2, spFlags: DISPFlagDefinition)
!578 = distinct !DISubprogram(name: "Plus__Plus_param_I_u32", linkageName: "Plus__Plus_param_I_u32", scope: !4, file: !3, line: 335, type: !6, scopeLine: 335, unit: !2, spFlags: DISPFlagDefinition)
!580 = distinct !DISubprogram(name: "Plus__uPlus_var_I_u8", linkageName: "Plus__uPlus_var_I_u8", scope: !4, file: !3, line: 336, type: !6, scopeLine: 336, unit: !2, spFlags: DISPFlagDefinition)
!582 = distinct !DISubprogram(name: "Plus__Plus_var_I_u8", linkageName: "Plus__Plus_var_I_u8", scope: !4, file: !3, line: 337, type: !6, scopeLine: 337, unit: !2, spFlags: DISPFlagDefinition)
!584 = distinct !DISubprogram(name: "Plus__uPlus_param_I_u8", linkageName: "Plus__uPlus_param_I_u8", scope: !4, file: !3, line: 338, type: !6, scopeLine: 338, unit: !2, spFlags: DISPFlagDefinition)
!586 = distinct !DISubprogram(name: "Plus__Plus_param_I_u8", linkageName: "Plus__Plus_param_I_u8", scope: !4, file: !3, line: 339, type: !6, scopeLine: 339, unit: !2, spFlags: DISPFlagDefinition)
!588 = distinct !DISubprogram(name: "Plus__uPlus_var_I_L", linkageName: "Plus__uPlus_var_I_L", scope: !4, file: !3, line: 340, type: !6, scopeLine: 340, unit: !2, spFlags: DISPFlagDefinition)
!590 = distinct !DISubprogram(name: "Plus__Plus_var_I_L", linkageName: "Plus__Plus_var_I_L", scope: !4, file: !3, line: 341, type: !6, scopeLine: 341, unit: !2, spFlags: DISPFlagDefinition)
!592 = distinct !DISubprogram(name: "Plus__uPlus_param_I_L", linkageName: "Plus__uPlus_param_I_L", scope: !4, file: !3, line: 342, type: !6, scopeLine: 342, unit: !2, spFlags: DISPFlagDefinition)
!594 = distinct !DISubprogram(name: "Plus__Plus_param_I_L", linkageName: "Plus__Plus_param_I_L", scope: !4, file: !3, line: 343, type: !6, scopeLine: 343, unit: !2, spFlags: DISPFlagDefinition)
!596 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_i8", linkageName: "Plus__uPlus_var_i64_i8", scope: !4, file: !3, line: 344, type: !6, scopeLine: 344, unit: !2, spFlags: DISPFlagDefinition)
!598 = distinct !DISubprogram(name: "Plus__Plus_var_i64_i8", linkageName: "Plus__Plus_var_i64_i8", scope: !4, file: !3, line: 345, type: !6, scopeLine: 345, unit: !2, spFlags: DISPFlagDefinition)
!600 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_i8", linkageName: "Plus__uPlus_param_i64_i8", scope: !4, file: !3, line: 346, type: !6, scopeLine: 346, unit: !2, spFlags: DISPFlagDefinition)
!602 = distinct !DISubprogram(name: "Plus__Plus_param_i64_i8", linkageName: "Plus__Plus_param_i64_i8", scope: !4, file: !3, line: 347, type: !6, scopeLine: 347, unit: !2, spFlags: DISPFlagDefinition)
!604 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_u64", linkageName: "Plus__uPlus_var_i64_u64", scope: !4, file: !3, line: 348, type: !6, scopeLine: 348, unit: !2, spFlags: DISPFlagDefinition)
!606 = distinct !DISubprogram(name: "Plus__Plus_var_i64_u64", linkageName: "Plus__Plus_var_i64_u64", scope: !4, file: !3, line: 349, type: !6, scopeLine: 349, unit: !2, spFlags: DISPFlagDefinition)
!608 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_u64", linkageName: "Plus__uPlus_param_i64_u64", scope: !4, file: !3, line: 350, type: !6, scopeLine: 350, unit: !2, spFlags: DISPFlagDefinition)
!610 = distinct !DISubprogram(name: "Plus__Plus_param_i64_u64", linkageName: "Plus__Plus_param_i64_u64", scope: !4, file: !3, line: 351, type: !6, scopeLine: 351, unit: !2, spFlags: DISPFlagDefinition)
!612 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_i32", linkageName: "Plus__uPlus_var_i64_i32", scope: !4, file: !3, line: 352, type: !6, scopeLine: 352, unit: !2, spFlags: DISPFlagDefinition)
!614 = distinct !DISubprogram(name: "Plus__Plus_var_i64_i32", linkageName: "Plus__Plus_var_i64_i32", scope: !4, file: !3, line: 353, type: !6, scopeLine: 353, unit: !2, spFlags: DISPFlagDefinition)
!616 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_i32", linkageName: "Plus__uPlus_param_i64_i32", scope: !4, file: !3, line: 354, type: !6, scopeLine: 354, unit: !2, spFlags: DISPFlagDefinition)
!618 = distinct !DISubprogram(name: "Plus__Plus_param_i64_i32", linkageName: "Plus__Plus_param_i64_i32", scope: !4, file: !3, line: 355, type: !6, scopeLine: 355, unit: !2, spFlags: DISPFlagDefinition)
!620 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_LC", linkageName: "Plus__uPlus_var_i64_LC", scope: !4, file: !3, line: 356, type: !6, scopeLine: 356, unit: !2, spFlags: DISPFlagDefinition)
!622 = distinct !DISubprogram(name: "Plus__Plus_var_i64_LC", linkageName: "Plus__Plus_var_i64_LC", scope: !4, file: !3, line: 357, type: !6, scopeLine: 357, unit: !2, spFlags: DISPFlagDefinition)
!624 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_LC", linkageName: "Plus__uPlus_param_i64_LC", scope: !4, file: !3, line: 358, type: !6, scopeLine: 358, unit: !2, spFlags: DISPFlagDefinition)
!626 = distinct !DISubprogram(name: "Plus__Plus_param_i64_LC", linkageName: "Plus__Plus_param_i64_LC", scope: !4, file: !3, line: 359, type: !6, scopeLine: 359, unit: !2, spFlags: DISPFlagDefinition)
!628 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_u16", linkageName: "Plus__uPlus_var_i64_u16", scope: !4, file: !3, line: 360, type: !6, scopeLine: 360, unit: !2, spFlags: DISPFlagDefinition)
!630 = distinct !DISubprogram(name: "Plus__Plus_var_i64_u16", linkageName: "Plus__Plus_var_i64_u16", scope: !4, file: !3, line: 361, type: !6, scopeLine: 361, unit: !2, spFlags: DISPFlagDefinition)
!632 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_u16", linkageName: "Plus__uPlus_param_i64_u16", scope: !4, file: !3, line: 362, type: !6, scopeLine: 362, unit: !2, spFlags: DISPFlagDefinition)
!634 = distinct !DISubprogram(name: "Plus__Plus_param_i64_u16", linkageName: "Plus__Plus_param_i64_u16", scope: !4, file: !3, line: 363, type: !6, scopeLine: 363, unit: !2, spFlags: DISPFlagDefinition)
!636 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_I", linkageName: "Plus__uPlus_var_i64_I", scope: !4, file: !3, line: 364, type: !6, scopeLine: 364, unit: !2, spFlags: DISPFlagDefinition)
!638 = distinct !DISubprogram(name: "Plus__Plus_var_i64_I", linkageName: "Plus__Plus_var_i64_I", scope: !4, file: !3, line: 365, type: !6, scopeLine: 365, unit: !2, spFlags: DISPFlagDefinition)
!640 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_I", linkageName: "Plus__uPlus_param_i64_I", scope: !4, file: !3, line: 366, type: !6, scopeLine: 366, unit: !2, spFlags: DISPFlagDefinition)
!642 = distinct !DISubprogram(name: "Plus__Plus_param_i64_I", linkageName: "Plus__Plus_param_i64_I", scope: !4, file: !3, line: 367, type: !6, scopeLine: 367, unit: !2, spFlags: DISPFlagDefinition)
!644 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_i64", linkageName: "Plus__uPlus_var_i64_i64", scope: !4, file: !3, line: 368, type: !6, scopeLine: 368, unit: !2, spFlags: DISPFlagDefinition)
!646 = distinct !DISubprogram(name: "Plus__Plus_var_i64_i64", linkageName: "Plus__Plus_var_i64_i64", scope: !4, file: !3, line: 369, type: !6, scopeLine: 369, unit: !2, spFlags: DISPFlagDefinition)
!648 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_i64", linkageName: "Plus__uPlus_param_i64_i64", scope: !4, file: !3, line: 370, type: !6, scopeLine: 370, unit: !2, spFlags: DISPFlagDefinition)
!650 = distinct !DISubprogram(name: "Plus__Plus_param_i64_i64", linkageName: "Plus__Plus_param_i64_i64", scope: !4, file: !3, line: 371, type: !6, scopeLine: 371, unit: !2, spFlags: DISPFlagDefinition)
!652 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_i16", linkageName: "Plus__uPlus_var_i64_i16", scope: !4, file: !3, line: 372, type: !6, scopeLine: 372, unit: !2, spFlags: DISPFlagDefinition)
!654 = distinct !DISubprogram(name: "Plus__Plus_var_i64_i16", linkageName: "Plus__Plus_var_i64_i16", scope: !4, file: !3, line: 373, type: !6, scopeLine: 373, unit: !2, spFlags: DISPFlagDefinition)
!656 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_i16", linkageName: "Plus__uPlus_param_i64_i16", scope: !4, file: !3, line: 374, type: !6, scopeLine: 374, unit: !2, spFlags: DISPFlagDefinition)
!658 = distinct !DISubprogram(name: "Plus__Plus_param_i64_i16", linkageName: "Plus__Plus_param_i64_i16", scope: !4, file: !3, line: 375, type: !6, scopeLine: 375, unit: !2, spFlags: DISPFlagDefinition)
!660 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_C", linkageName: "Plus__uPlus_var_i64_C", scope: !4, file: !3, line: 376, type: !6, scopeLine: 376, unit: !2, spFlags: DISPFlagDefinition)
!662 = distinct !DISubprogram(name: "Plus__Plus_var_i64_C", linkageName: "Plus__Plus_var_i64_C", scope: !4, file: !3, line: 377, type: !6, scopeLine: 377, unit: !2, spFlags: DISPFlagDefinition)
!664 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_C", linkageName: "Plus__uPlus_param_i64_C", scope: !4, file: !3, line: 378, type: !6, scopeLine: 378, unit: !2, spFlags: DISPFlagDefinition)
!666 = distinct !DISubprogram(name: "Plus__Plus_param_i64_C", linkageName: "Plus__Plus_param_i64_C", scope: !4, file: !3, line: 379, type: !6, scopeLine: 379, unit: !2, spFlags: DISPFlagDefinition)
!668 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_u32", linkageName: "Plus__uPlus_var_i64_u32", scope: !4, file: !3, line: 380, type: !6, scopeLine: 380, unit: !2, spFlags: DISPFlagDefinition)
!670 = distinct !DISubprogram(name: "Plus__Plus_var_i64_u32", linkageName: "Plus__Plus_var_i64_u32", scope: !4, file: !3, line: 381, type: !6, scopeLine: 381, unit: !2, spFlags: DISPFlagDefinition)
!672 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_u32", linkageName: "Plus__uPlus_param_i64_u32", scope: !4, file: !3, line: 382, type: !6, scopeLine: 382, unit: !2, spFlags: DISPFlagDefinition)
!674 = distinct !DISubprogram(name: "Plus__Plus_param_i64_u32", linkageName: "Plus__Plus_param_i64_u32", scope: !4, file: !3, line: 383, type: !6, scopeLine: 383, unit: !2, spFlags: DISPFlagDefinition)
!676 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_u8", linkageName: "Plus__uPlus_var_i64_u8", scope: !4, file: !3, line: 384, type: !6, scopeLine: 384, unit: !2, spFlags: DISPFlagDefinition)
!678 = distinct !DISubprogram(name: "Plus__Plus_var_i64_u8", linkageName: "Plus__Plus_var_i64_u8", scope: !4, file: !3, line: 385, type: !6, scopeLine: 385, unit: !2, spFlags: DISPFlagDefinition)
!680 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_u8", linkageName: "Plus__uPlus_param_i64_u8", scope: !4, file: !3, line: 386, type: !6, scopeLine: 386, unit: !2, spFlags: DISPFlagDefinition)
!682 = distinct !DISubprogram(name: "Plus__Plus_param_i64_u8", linkageName: "Plus__Plus_param_i64_u8", scope: !4, file: !3, line: 387, type: !6, scopeLine: 387, unit: !2, spFlags: DISPFlagDefinition)
!684 = distinct !DISubprogram(name: "Plus__uPlus_var_i64_L", linkageName: "Plus__uPlus_var_i64_L", scope: !4, file: !3, line: 388, type: !6, scopeLine: 388, unit: !2, spFlags: DISPFlagDefinition)
!686 = distinct !DISubprogram(name: "Plus__Plus_var_i64_L", linkageName: "Plus__Plus_var_i64_L", scope: !4, file: !3, line: 389, type: !6, scopeLine: 389, unit: !2, spFlags: DISPFlagDefinition)
!688 = distinct !DISubprogram(name: "Plus__uPlus_param_i64_L", linkageName: "Plus__uPlus_param_i64_L", scope: !4, file: !3, line: 390, type: !6, scopeLine: 390, unit: !2, spFlags: DISPFlagDefinition)
!690 = distinct !DISubprogram(name: "Plus__Plus_param_i64_L", linkageName: "Plus__Plus_param_i64_L", scope: !4, file: !3, line: 391, type: !6, scopeLine: 391, unit: !2, spFlags: DISPFlagDefinition)
!692 = distinct !DISubprogram(name: "Plus__Plus_var_f32_f32", linkageName: "Plus__Plus_var_f32_f32", scope: !4, file: !3, line: 392, type: !6, scopeLine: 392, unit: !2, spFlags: DISPFlagDefinition)
!694 = distinct !DISubprogram(name: "Plus__Plus_param_f32_f32", linkageName: "Plus__Plus_param_f32_f32", scope: !4, file: !3, line: 393, type: !6, scopeLine: 393, unit: !2, spFlags: DISPFlagDefinition)
!696 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_i8", linkageName: "Plus__uPlus_var_i16_i8", scope: !4, file: !3, line: 394, type: !6, scopeLine: 394, unit: !2, spFlags: DISPFlagDefinition)
!698 = distinct !DISubprogram(name: "Plus__Plus_var_i16_i8", linkageName: "Plus__Plus_var_i16_i8", scope: !4, file: !3, line: 395, type: !6, scopeLine: 395, unit: !2, spFlags: DISPFlagDefinition)
!700 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_i8", linkageName: "Plus__uPlus_param_i16_i8", scope: !4, file: !3, line: 396, type: !6, scopeLine: 396, unit: !2, spFlags: DISPFlagDefinition)
!702 = distinct !DISubprogram(name: "Plus__Plus_param_i16_i8", linkageName: "Plus__Plus_param_i16_i8", scope: !4, file: !3, line: 397, type: !6, scopeLine: 397, unit: !2, spFlags: DISPFlagDefinition)
!704 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_u64", linkageName: "Plus__uPlus_var_i16_u64", scope: !4, file: !3, line: 398, type: !6, scopeLine: 398, unit: !2, spFlags: DISPFlagDefinition)
!706 = distinct !DISubprogram(name: "Plus__Plus_var_i16_u64", linkageName: "Plus__Plus_var_i16_u64", scope: !4, file: !3, line: 399, type: !6, scopeLine: 399, unit: !2, spFlags: DISPFlagDefinition)
!708 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_u64", linkageName: "Plus__uPlus_param_i16_u64", scope: !4, file: !3, line: 400, type: !6, scopeLine: 400, unit: !2, spFlags: DISPFlagDefinition)
!710 = distinct !DISubprogram(name: "Plus__Plus_param_i16_u64", linkageName: "Plus__Plus_param_i16_u64", scope: !4, file: !3, line: 401, type: !6, scopeLine: 401, unit: !2, spFlags: DISPFlagDefinition)
!712 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_i32", linkageName: "Plus__uPlus_var_i16_i32", scope: !4, file: !3, line: 402, type: !6, scopeLine: 402, unit: !2, spFlags: DISPFlagDefinition)
!714 = distinct !DISubprogram(name: "Plus__Plus_var_i16_i32", linkageName: "Plus__Plus_var_i16_i32", scope: !4, file: !3, line: 403, type: !6, scopeLine: 403, unit: !2, spFlags: DISPFlagDefinition)
!716 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_i32", linkageName: "Plus__uPlus_param_i16_i32", scope: !4, file: !3, line: 404, type: !6, scopeLine: 404, unit: !2, spFlags: DISPFlagDefinition)
!718 = distinct !DISubprogram(name: "Plus__Plus_param_i16_i32", linkageName: "Plus__Plus_param_i16_i32", scope: !4, file: !3, line: 405, type: !6, scopeLine: 405, unit: !2, spFlags: DISPFlagDefinition)
!720 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_LC", linkageName: "Plus__uPlus_var_i16_LC", scope: !4, file: !3, line: 406, type: !6, scopeLine: 406, unit: !2, spFlags: DISPFlagDefinition)
!722 = distinct !DISubprogram(name: "Plus__Plus_var_i16_LC", linkageName: "Plus__Plus_var_i16_LC", scope: !4, file: !3, line: 407, type: !6, scopeLine: 407, unit: !2, spFlags: DISPFlagDefinition)
!724 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_LC", linkageName: "Plus__uPlus_param_i16_LC", scope: !4, file: !3, line: 408, type: !6, scopeLine: 408, unit: !2, spFlags: DISPFlagDefinition)
!726 = distinct !DISubprogram(name: "Plus__Plus_param_i16_LC", linkageName: "Plus__Plus_param_i16_LC", scope: !4, file: !3, line: 409, type: !6, scopeLine: 409, unit: !2, spFlags: DISPFlagDefinition)
!728 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_u16", linkageName: "Plus__uPlus_var_i16_u16", scope: !4, file: !3, line: 410, type: !6, scopeLine: 410, unit: !2, spFlags: DISPFlagDefinition)
!730 = distinct !DISubprogram(name: "Plus__Plus_var_i16_u16", linkageName: "Plus__Plus_var_i16_u16", scope: !4, file: !3, line: 411, type: !6, scopeLine: 411, unit: !2, spFlags: DISPFlagDefinition)
!732 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_u16", linkageName: "Plus__uPlus_param_i16_u16", scope: !4, file: !3, line: 412, type: !6, scopeLine: 412, unit: !2, spFlags: DISPFlagDefinition)
!734 = distinct !DISubprogram(name: "Plus__Plus_param_i16_u16", linkageName: "Plus__Plus_param_i16_u16", scope: !4, file: !3, line: 413, type: !6, scopeLine: 413, unit: !2, spFlags: DISPFlagDefinition)
!736 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_I", linkageName: "Plus__uPlus_var_i16_I", scope: !4, file: !3, line: 414, type: !6, scopeLine: 414, unit: !2, spFlags: DISPFlagDefinition)
!738 = distinct !DISubprogram(name: "Plus__Plus_var_i16_I", linkageName: "Plus__Plus_var_i16_I", scope: !4, file: !3, line: 415, type: !6, scopeLine: 415, unit: !2, spFlags: DISPFlagDefinition)
!740 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_I", linkageName: "Plus__uPlus_param_i16_I", scope: !4, file: !3, line: 416, type: !6, scopeLine: 416, unit: !2, spFlags: DISPFlagDefinition)
!742 = distinct !DISubprogram(name: "Plus__Plus_param_i16_I", linkageName: "Plus__Plus_param_i16_I", scope: !4, file: !3, line: 417, type: !6, scopeLine: 417, unit: !2, spFlags: DISPFlagDefinition)
!744 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_i64", linkageName: "Plus__uPlus_var_i16_i64", scope: !4, file: !3, line: 418, type: !6, scopeLine: 418, unit: !2, spFlags: DISPFlagDefinition)
!746 = distinct !DISubprogram(name: "Plus__Plus_var_i16_i64", linkageName: "Plus__Plus_var_i16_i64", scope: !4, file: !3, line: 419, type: !6, scopeLine: 419, unit: !2, spFlags: DISPFlagDefinition)
!748 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_i64", linkageName: "Plus__uPlus_param_i16_i64", scope: !4, file: !3, line: 420, type: !6, scopeLine: 420, unit: !2, spFlags: DISPFlagDefinition)
!750 = distinct !DISubprogram(name: "Plus__Plus_param_i16_i64", linkageName: "Plus__Plus_param_i16_i64", scope: !4, file: !3, line: 421, type: !6, scopeLine: 421, unit: !2, spFlags: DISPFlagDefinition)
!752 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_i16", linkageName: "Plus__uPlus_var_i16_i16", scope: !4, file: !3, line: 422, type: !6, scopeLine: 422, unit: !2, spFlags: DISPFlagDefinition)
!754 = distinct !DISubprogram(name: "Plus__Plus_var_i16_i16", linkageName: "Plus__Plus_var_i16_i16", scope: !4, file: !3, line: 423, type: !6, scopeLine: 423, unit: !2, spFlags: DISPFlagDefinition)
!756 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_i16", linkageName: "Plus__uPlus_param_i16_i16", scope: !4, file: !3, line: 424, type: !6, scopeLine: 424, unit: !2, spFlags: DISPFlagDefinition)
!758 = distinct !DISubprogram(name: "Plus__Plus_param_i16_i16", linkageName: "Plus__Plus_param_i16_i16", scope: !4, file: !3, line: 425, type: !6, scopeLine: 425, unit: !2, spFlags: DISPFlagDefinition)
!760 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_C", linkageName: "Plus__uPlus_var_i16_C", scope: !4, file: !3, line: 426, type: !6, scopeLine: 426, unit: !2, spFlags: DISPFlagDefinition)
!762 = distinct !DISubprogram(name: "Plus__Plus_var_i16_C", linkageName: "Plus__Plus_var_i16_C", scope: !4, file: !3, line: 427, type: !6, scopeLine: 427, unit: !2, spFlags: DISPFlagDefinition)
!764 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_C", linkageName: "Plus__uPlus_param_i16_C", scope: !4, file: !3, line: 428, type: !6, scopeLine: 428, unit: !2, spFlags: DISPFlagDefinition)
!766 = distinct !DISubprogram(name: "Plus__Plus_param_i16_C", linkageName: "Plus__Plus_param_i16_C", scope: !4, file: !3, line: 429, type: !6, scopeLine: 429, unit: !2, spFlags: DISPFlagDefinition)
!768 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_u32", linkageName: "Plus__uPlus_var_i16_u32", scope: !4, file: !3, line: 430, type: !6, scopeLine: 430, unit: !2, spFlags: DISPFlagDefinition)
!770 = distinct !DISubprogram(name: "Plus__Plus_var_i16_u32", linkageName: "Plus__Plus_var_i16_u32", scope: !4, file: !3, line: 431, type: !6, scopeLine: 431, unit: !2, spFlags: DISPFlagDefinition)
!772 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_u32", linkageName: "Plus__uPlus_param_i16_u32", scope: !4, file: !3, line: 432, type: !6, scopeLine: 432, unit: !2, spFlags: DISPFlagDefinition)
!774 = distinct !DISubprogram(name: "Plus__Plus_param_i16_u32", linkageName: "Plus__Plus_param_i16_u32", scope: !4, file: !3, line: 433, type: !6, scopeLine: 433, unit: !2, spFlags: DISPFlagDefinition)
!776 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_u8", linkageName: "Plus__uPlus_var_i16_u8", scope: !4, file: !3, line: 434, type: !6, scopeLine: 434, unit: !2, spFlags: DISPFlagDefinition)
!778 = distinct !DISubprogram(name: "Plus__Plus_var_i16_u8", linkageName: "Plus__Plus_var_i16_u8", scope: !4, file: !3, line: 435, type: !6, scopeLine: 435, unit: !2, spFlags: DISPFlagDefinition)
!780 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_u8", linkageName: "Plus__uPlus_param_i16_u8", scope: !4, file: !3, line: 436, type: !6, scopeLine: 436, unit: !2, spFlags: DISPFlagDefinition)
!782 = distinct !DISubprogram(name: "Plus__Plus_param_i16_u8", linkageName: "Plus__Plus_param_i16_u8", scope: !4, file: !3, line: 437, type: !6, scopeLine: 437, unit: !2, spFlags: DISPFlagDefinition)
!784 = distinct !DISubprogram(name: "Plus__uPlus_var_i16_L", linkageName: "Plus__uPlus_var_i16_L", scope: !4, file: !3, line: 438, type: !6, scopeLine: 438, unit: !2, spFlags: DISPFlagDefinition)
!786 = distinct !DISubprogram(name: "Plus__Plus_var_i16_L", linkageName: "Plus__Plus_var_i16_L", scope: !4, file: !3, line: 439, type: !6, scopeLine: 439, unit: !2, spFlags: DISPFlagDefinition)
!788 = distinct !DISubprogram(name: "Plus__uPlus_param_i16_L", linkageName: "Plus__uPlus_param_i16_L", scope: !4, file: !3, line: 440, type: !6, scopeLine: 440, unit: !2, spFlags: DISPFlagDefinition)
!790 = distinct !DISubprogram(name: "Plus__Plus_param_i16_L", linkageName: "Plus__Plus_param_i16_L", scope: !4, file: !3, line: 441, type: !6, scopeLine: 441, unit: !2, spFlags: DISPFlagDefinition)
!792 = distinct !DISubprogram(name: "Plus__uPlus_var_C_i8", linkageName: "Plus__uPlus_var_C_i8", scope: !4, file: !3, line: 442, type: !6, scopeLine: 442, unit: !2, spFlags: DISPFlagDefinition)
!794 = distinct !DISubprogram(name: "Plus__Plus_var_C_i8", linkageName: "Plus__Plus_var_C_i8", scope: !4, file: !3, line: 443, type: !6, scopeLine: 443, unit: !2, spFlags: DISPFlagDefinition)
!796 = distinct !DISubprogram(name: "Plus__uPlus_param_C_i8", linkageName: "Plus__uPlus_param_C_i8", scope: !4, file: !3, line: 444, type: !6, scopeLine: 444, unit: !2, spFlags: DISPFlagDefinition)
!798 = distinct !DISubprogram(name: "Plus__Plus_param_C_i8", linkageName: "Plus__Plus_param_C_i8", scope: !4, file: !3, line: 445, type: !6, scopeLine: 445, unit: !2, spFlags: DISPFlagDefinition)
!800 = distinct !DISubprogram(name: "Plus__uPlus_var_C_u64", linkageName: "Plus__uPlus_var_C_u64", scope: !4, file: !3, line: 446, type: !6, scopeLine: 446, unit: !2, spFlags: DISPFlagDefinition)
!802 = distinct !DISubprogram(name: "Plus__Plus_var_C_u64", linkageName: "Plus__Plus_var_C_u64", scope: !4, file: !3, line: 447, type: !6, scopeLine: 447, unit: !2, spFlags: DISPFlagDefinition)
!804 = distinct !DISubprogram(name: "Plus__uPlus_param_C_u64", linkageName: "Plus__uPlus_param_C_u64", scope: !4, file: !3, line: 448, type: !6, scopeLine: 448, unit: !2, spFlags: DISPFlagDefinition)
!806 = distinct !DISubprogram(name: "Plus__Plus_param_C_u64", linkageName: "Plus__Plus_param_C_u64", scope: !4, file: !3, line: 449, type: !6, scopeLine: 449, unit: !2, spFlags: DISPFlagDefinition)
!808 = distinct !DISubprogram(name: "Plus__uPlus_var_C_i32", linkageName: "Plus__uPlus_var_C_i32", scope: !4, file: !3, line: 450, type: !6, scopeLine: 450, unit: !2, spFlags: DISPFlagDefinition)
!810 = distinct !DISubprogram(name: "Plus__Plus_var_C_i32", linkageName: "Plus__Plus_var_C_i32", scope: !4, file: !3, line: 451, type: !6, scopeLine: 451, unit: !2, spFlags: DISPFlagDefinition)
!812 = distinct !DISubprogram(name: "Plus__uPlus_param_C_i32", linkageName: "Plus__uPlus_param_C_i32", scope: !4, file: !3, line: 452, type: !6, scopeLine: 452, unit: !2, spFlags: DISPFlagDefinition)
!814 = distinct !DISubprogram(name: "Plus__Plus_param_C_i32", linkageName: "Plus__Plus_param_C_i32", scope: !4, file: !3, line: 453, type: !6, scopeLine: 453, unit: !2, spFlags: DISPFlagDefinition)
!816 = distinct !DISubprogram(name: "Plus__uPlus_var_C_LC", linkageName: "Plus__uPlus_var_C_LC", scope: !4, file: !3, line: 454, type: !6, scopeLine: 454, unit: !2, spFlags: DISPFlagDefinition)
!818 = distinct !DISubprogram(name: "Plus__Plus_var_C_LC", linkageName: "Plus__Plus_var_C_LC", scope: !4, file: !3, line: 455, type: !6, scopeLine: 455, unit: !2, spFlags: DISPFlagDefinition)
!820 = distinct !DISubprogram(name: "Plus__uPlus_param_C_LC", linkageName: "Plus__uPlus_param_C_LC", scope: !4, file: !3, line: 456, type: !6, scopeLine: 456, unit: !2, spFlags: DISPFlagDefinition)
!822 = distinct !DISubprogram(name: "Plus__Plus_param_C_LC", linkageName: "Plus__Plus_param_C_LC", scope: !4, file: !3, line: 457, type: !6, scopeLine: 457, unit: !2, spFlags: DISPFlagDefinition)
!824 = distinct !DISubprogram(name: "Plus__uPlus_var_C_u16", linkageName: "Plus__uPlus_var_C_u16", scope: !4, file: !3, line: 458, type: !6, scopeLine: 458, unit: !2, spFlags: DISPFlagDefinition)
!826 = distinct !DISubprogram(name: "Plus__Plus_var_C_u16", linkageName: "Plus__Plus_var_C_u16", scope: !4, file: !3, line: 459, type: !6, scopeLine: 459, unit: !2, spFlags: DISPFlagDefinition)
!828 = distinct !DISubprogram(name: "Plus__uPlus_param_C_u16", linkageName: "Plus__uPlus_param_C_u16", scope: !4, file: !3, line: 460, type: !6, scopeLine: 460, unit: !2, spFlags: DISPFlagDefinition)
!830 = distinct !DISubprogram(name: "Plus__Plus_param_C_u16", linkageName: "Plus__Plus_param_C_u16", scope: !4, file: !3, line: 461, type: !6, scopeLine: 461, unit: !2, spFlags: DISPFlagDefinition)
!832 = distinct !DISubprogram(name: "Plus__uPlus_var_C_I", linkageName: "Plus__uPlus_var_C_I", scope: !4, file: !3, line: 462, type: !6, scopeLine: 462, unit: !2, spFlags: DISPFlagDefinition)
!834 = distinct !DISubprogram(name: "Plus__Plus_var_C_I", linkageName: "Plus__Plus_var_C_I", scope: !4, file: !3, line: 463, type: !6, scopeLine: 463, unit: !2, spFlags: DISPFlagDefinition)
!836 = distinct !DISubprogram(name: "Plus__uPlus_param_C_I", linkageName: "Plus__uPlus_param_C_I", scope: !4, file: !3, line: 464, type: !6, scopeLine: 464, unit: !2, spFlags: DISPFlagDefinition)
!838 = distinct !DISubprogram(name: "Plus__Plus_param_C_I", linkageName: "Plus__Plus_param_C_I", scope: !4, file: !3, line: 465, type: !6, scopeLine: 465, unit: !2, spFlags: DISPFlagDefinition)
!840 = distinct !DISubprogram(name: "Plus__uPlus_var_C_i64", linkageName: "Plus__uPlus_var_C_i64", scope: !4, file: !3, line: 466, type: !6, scopeLine: 466, unit: !2, spFlags: DISPFlagDefinition)
!842 = distinct !DISubprogram(name: "Plus__Plus_var_C_i64", linkageName: "Plus__Plus_var_C_i64", scope: !4, file: !3, line: 467, type: !6, scopeLine: 467, unit: !2, spFlags: DISPFlagDefinition)
!844 = distinct !DISubprogram(name: "Plus__uPlus_param_C_i64", linkageName: "Plus__uPlus_param_C_i64", scope: !4, file: !3, line: 468, type: !6, scopeLine: 468, unit: !2, spFlags: DISPFlagDefinition)
!846 = distinct !DISubprogram(name: "Plus__Plus_param_C_i64", linkageName: "Plus__Plus_param_C_i64", scope: !4, file: !3, line: 469, type: !6, scopeLine: 469, unit: !2, spFlags: DISPFlagDefinition)
!848 = distinct !DISubprogram(name: "Plus__uPlus_var_C_i16", linkageName: "Plus__uPlus_var_C_i16", scope: !4, file: !3, line: 470, type: !6, scopeLine: 470, unit: !2, spFlags: DISPFlagDefinition)
!850 = distinct !DISubprogram(name: "Plus__Plus_var_C_i16", linkageName: "Plus__Plus_var_C_i16", scope: !4, file: !3, line: 471, type: !6, scopeLine: 471, unit: !2, spFlags: DISPFlagDefinition)
!852 = distinct !DISubprogram(name: "Plus__uPlus_param_C_i16", linkageName: "Plus__uPlus_param_C_i16", scope: !4, file: !3, line: 472, type: !6, scopeLine: 472, unit: !2, spFlags: DISPFlagDefinition)
!854 = distinct !DISubprogram(name: "Plus__Plus_param_C_i16", linkageName: "Plus__Plus_param_C_i16", scope: !4, file: !3, line: 473, type: !6, scopeLine: 473, unit: !2, spFlags: DISPFlagDefinition)
!856 = distinct !DISubprogram(name: "Plus__uPlus_var_C_C", linkageName: "Plus__uPlus_var_C_C", scope: !4, file: !3, line: 474, type: !6, scopeLine: 474, unit: !2, spFlags: DISPFlagDefinition)
!858 = distinct !DISubprogram(name: "Plus__Plus_var_C_C", linkageName: "Plus__Plus_var_C_C", scope: !4, file: !3, line: 475, type: !6, scopeLine: 475, unit: !2, spFlags: DISPFlagDefinition)
!860 = distinct !DISubprogram(name: "Plus__uPlus_param_C_C", linkageName: "Plus__uPlus_param_C_C", scope: !4, file: !3, line: 476, type: !6, scopeLine: 476, unit: !2, spFlags: DISPFlagDefinition)
!862 = distinct !DISubprogram(name: "Plus__Plus_param_C_C", linkageName: "Plus__Plus_param_C_C", scope: !4, file: !3, line: 477, type: !6, scopeLine: 477, unit: !2, spFlags: DISPFlagDefinition)
!864 = distinct !DISubprogram(name: "Plus__uPlus_var_C_u32", linkageName: "Plus__uPlus_var_C_u32", scope: !4, file: !3, line: 478, type: !6, scopeLine: 478, unit: !2, spFlags: DISPFlagDefinition)
!866 = distinct !DISubprogram(name: "Plus__Plus_var_C_u32", linkageName: "Plus__Plus_var_C_u32", scope: !4, file: !3, line: 479, type: !6, scopeLine: 479, unit: !2, spFlags: DISPFlagDefinition)
!868 = distinct !DISubprogram(name: "Plus__uPlus_param_C_u32", linkageName: "Plus__uPlus_param_C_u32", scope: !4, file: !3, line: 480, type: !6, scopeLine: 480, unit: !2, spFlags: DISPFlagDefinition)
!870 = distinct !DISubprogram(name: "Plus__Plus_param_C_u32", linkageName: "Plus__Plus_param_C_u32", scope: !4, file: !3, line: 481, type: !6, scopeLine: 481, unit: !2, spFlags: DISPFlagDefinition)
!872 = distinct !DISubprogram(name: "Plus__uPlus_var_C_u8", linkageName: "Plus__uPlus_var_C_u8", scope: !4, file: !3, line: 482, type: !6, scopeLine: 482, unit: !2, spFlags: DISPFlagDefinition)
!874 = distinct !DISubprogram(name: "Plus__Plus_var_C_u8", linkageName: "Plus__Plus_var_C_u8", scope: !4, file: !3, line: 483, type: !6, scopeLine: 483, unit: !2, spFlags: DISPFlagDefinition)
!876 = distinct !DISubprogram(name: "Plus__uPlus_param_C_u8", linkageName: "Plus__uPlus_param_C_u8", scope: !4, file: !3, line: 484, type: !6, scopeLine: 484, unit: !2, spFlags: DISPFlagDefinition)
!878 = distinct !DISubprogram(name: "Plus__Plus_param_C_u8", linkageName: "Plus__Plus_param_C_u8", scope: !4, file: !3, line: 485, type: !6, scopeLine: 485, unit: !2, spFlags: DISPFlagDefinition)
!880 = distinct !DISubprogram(name: "Plus__uPlus_var_C_L", linkageName: "Plus__uPlus_var_C_L", scope: !4, file: !3, line: 486, type: !6, scopeLine: 486, unit: !2, spFlags: DISPFlagDefinition)
!882 = distinct !DISubprogram(name: "Plus__Plus_var_C_L", linkageName: "Plus__Plus_var_C_L", scope: !4, file: !3, line: 487, type: !6, scopeLine: 487, unit: !2, spFlags: DISPFlagDefinition)
!884 = distinct !DISubprogram(name: "Plus__uPlus_param_C_L", linkageName: "Plus__uPlus_param_C_L", scope: !4, file: !3, line: 488, type: !6, scopeLine: 488, unit: !2, spFlags: DISPFlagDefinition)
!886 = distinct !DISubprogram(name: "Plus__Plus_param_C_L", linkageName: "Plus__Plus_param_C_L", scope: !4, file: !3, line: 489, type: !6, scopeLine: 489, unit: !2, spFlags: DISPFlagDefinition)
!888 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_i8", linkageName: "Plus__uPlus_var_u32_i8", scope: !4, file: !3, line: 490, type: !6, scopeLine: 490, unit: !2, spFlags: DISPFlagDefinition)
!890 = distinct !DISubprogram(name: "Plus__Plus_var_u32_i8", linkageName: "Plus__Plus_var_u32_i8", scope: !4, file: !3, line: 491, type: !6, scopeLine: 491, unit: !2, spFlags: DISPFlagDefinition)
!892 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_i8", linkageName: "Plus__uPlus_param_u32_i8", scope: !4, file: !3, line: 492, type: !6, scopeLine: 492, unit: !2, spFlags: DISPFlagDefinition)
!894 = distinct !DISubprogram(name: "Plus__Plus_param_u32_i8", linkageName: "Plus__Plus_param_u32_i8", scope: !4, file: !3, line: 493, type: !6, scopeLine: 493, unit: !2, spFlags: DISPFlagDefinition)
!896 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_u64", linkageName: "Plus__uPlus_var_u32_u64", scope: !4, file: !3, line: 494, type: !6, scopeLine: 494, unit: !2, spFlags: DISPFlagDefinition)
!898 = distinct !DISubprogram(name: "Plus__Plus_var_u32_u64", linkageName: "Plus__Plus_var_u32_u64", scope: !4, file: !3, line: 495, type: !6, scopeLine: 495, unit: !2, spFlags: DISPFlagDefinition)
!900 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_u64", linkageName: "Plus__uPlus_param_u32_u64", scope: !4, file: !3, line: 496, type: !6, scopeLine: 496, unit: !2, spFlags: DISPFlagDefinition)
!902 = distinct !DISubprogram(name: "Plus__Plus_param_u32_u64", linkageName: "Plus__Plus_param_u32_u64", scope: !4, file: !3, line: 497, type: !6, scopeLine: 497, unit: !2, spFlags: DISPFlagDefinition)
!904 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_i32", linkageName: "Plus__uPlus_var_u32_i32", scope: !4, file: !3, line: 498, type: !6, scopeLine: 498, unit: !2, spFlags: DISPFlagDefinition)
!906 = distinct !DISubprogram(name: "Plus__Plus_var_u32_i32", linkageName: "Plus__Plus_var_u32_i32", scope: !4, file: !3, line: 499, type: !6, scopeLine: 499, unit: !2, spFlags: DISPFlagDefinition)
!908 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_i32", linkageName: "Plus__uPlus_param_u32_i32", scope: !4, file: !3, line: 500, type: !6, scopeLine: 500, unit: !2, spFlags: DISPFlagDefinition)
!910 = distinct !DISubprogram(name: "Plus__Plus_param_u32_i32", linkageName: "Plus__Plus_param_u32_i32", scope: !4, file: !3, line: 501, type: !6, scopeLine: 501, unit: !2, spFlags: DISPFlagDefinition)
!912 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_LC", linkageName: "Plus__uPlus_var_u32_LC", scope: !4, file: !3, line: 502, type: !6, scopeLine: 502, unit: !2, spFlags: DISPFlagDefinition)
!914 = distinct !DISubprogram(name: "Plus__Plus_var_u32_LC", linkageName: "Plus__Plus_var_u32_LC", scope: !4, file: !3, line: 503, type: !6, scopeLine: 503, unit: !2, spFlags: DISPFlagDefinition)
!916 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_LC", linkageName: "Plus__uPlus_param_u32_LC", scope: !4, file: !3, line: 504, type: !6, scopeLine: 504, unit: !2, spFlags: DISPFlagDefinition)
!918 = distinct !DISubprogram(name: "Plus__Plus_param_u32_LC", linkageName: "Plus__Plus_param_u32_LC", scope: !4, file: !3, line: 505, type: !6, scopeLine: 505, unit: !2, spFlags: DISPFlagDefinition)
!920 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_u16", linkageName: "Plus__uPlus_var_u32_u16", scope: !4, file: !3, line: 506, type: !6, scopeLine: 506, unit: !2, spFlags: DISPFlagDefinition)
!922 = distinct !DISubprogram(name: "Plus__Plus_var_u32_u16", linkageName: "Plus__Plus_var_u32_u16", scope: !4, file: !3, line: 507, type: !6, scopeLine: 507, unit: !2, spFlags: DISPFlagDefinition)
!924 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_u16", linkageName: "Plus__uPlus_param_u32_u16", scope: !4, file: !3, line: 508, type: !6, scopeLine: 508, unit: !2, spFlags: DISPFlagDefinition)
!926 = distinct !DISubprogram(name: "Plus__Plus_param_u32_u16", linkageName: "Plus__Plus_param_u32_u16", scope: !4, file: !3, line: 509, type: !6, scopeLine: 509, unit: !2, spFlags: DISPFlagDefinition)
!928 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_I", linkageName: "Plus__uPlus_var_u32_I", scope: !4, file: !3, line: 510, type: !6, scopeLine: 510, unit: !2, spFlags: DISPFlagDefinition)
!930 = distinct !DISubprogram(name: "Plus__Plus_var_u32_I", linkageName: "Plus__Plus_var_u32_I", scope: !4, file: !3, line: 511, type: !6, scopeLine: 511, unit: !2, spFlags: DISPFlagDefinition)
!932 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_I", linkageName: "Plus__uPlus_param_u32_I", scope: !4, file: !3, line: 512, type: !6, scopeLine: 512, unit: !2, spFlags: DISPFlagDefinition)
!934 = distinct !DISubprogram(name: "Plus__Plus_param_u32_I", linkageName: "Plus__Plus_param_u32_I", scope: !4, file: !3, line: 513, type: !6, scopeLine: 513, unit: !2, spFlags: DISPFlagDefinition)
!936 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_i64", linkageName: "Plus__uPlus_var_u32_i64", scope: !4, file: !3, line: 514, type: !6, scopeLine: 514, unit: !2, spFlags: DISPFlagDefinition)
!938 = distinct !DISubprogram(name: "Plus__Plus_var_u32_i64", linkageName: "Plus__Plus_var_u32_i64", scope: !4, file: !3, line: 515, type: !6, scopeLine: 515, unit: !2, spFlags: DISPFlagDefinition)
!940 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_i64", linkageName: "Plus__uPlus_param_u32_i64", scope: !4, file: !3, line: 516, type: !6, scopeLine: 516, unit: !2, spFlags: DISPFlagDefinition)
!942 = distinct !DISubprogram(name: "Plus__Plus_param_u32_i64", linkageName: "Plus__Plus_param_u32_i64", scope: !4, file: !3, line: 517, type: !6, scopeLine: 517, unit: !2, spFlags: DISPFlagDefinition)
!944 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_i16", linkageName: "Plus__uPlus_var_u32_i16", scope: !4, file: !3, line: 518, type: !6, scopeLine: 518, unit: !2, spFlags: DISPFlagDefinition)
!946 = distinct !DISubprogram(name: "Plus__Plus_var_u32_i16", linkageName: "Plus__Plus_var_u32_i16", scope: !4, file: !3, line: 519, type: !6, scopeLine: 519, unit: !2, spFlags: DISPFlagDefinition)
!948 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_i16", linkageName: "Plus__uPlus_param_u32_i16", scope: !4, file: !3, line: 520, type: !6, scopeLine: 520, unit: !2, spFlags: DISPFlagDefinition)
!950 = distinct !DISubprogram(name: "Plus__Plus_param_u32_i16", linkageName: "Plus__Plus_param_u32_i16", scope: !4, file: !3, line: 521, type: !6, scopeLine: 521, unit: !2, spFlags: DISPFlagDefinition)
!952 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_C", linkageName: "Plus__uPlus_var_u32_C", scope: !4, file: !3, line: 522, type: !6, scopeLine: 522, unit: !2, spFlags: DISPFlagDefinition)
!954 = distinct !DISubprogram(name: "Plus__Plus_var_u32_C", linkageName: "Plus__Plus_var_u32_C", scope: !4, file: !3, line: 523, type: !6, scopeLine: 523, unit: !2, spFlags: DISPFlagDefinition)
!956 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_C", linkageName: "Plus__uPlus_param_u32_C", scope: !4, file: !3, line: 524, type: !6, scopeLine: 524, unit: !2, spFlags: DISPFlagDefinition)
!958 = distinct !DISubprogram(name: "Plus__Plus_param_u32_C", linkageName: "Plus__Plus_param_u32_C", scope: !4, file: !3, line: 525, type: !6, scopeLine: 525, unit: !2, spFlags: DISPFlagDefinition)
!960 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_u32", linkageName: "Plus__uPlus_var_u32_u32", scope: !4, file: !3, line: 526, type: !6, scopeLine: 526, unit: !2, spFlags: DISPFlagDefinition)
!962 = distinct !DISubprogram(name: "Plus__Plus_var_u32_u32", linkageName: "Plus__Plus_var_u32_u32", scope: !4, file: !3, line: 527, type: !6, scopeLine: 527, unit: !2, spFlags: DISPFlagDefinition)
!964 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_u32", linkageName: "Plus__uPlus_param_u32_u32", scope: !4, file: !3, line: 528, type: !6, scopeLine: 528, unit: !2, spFlags: DISPFlagDefinition)
!966 = distinct !DISubprogram(name: "Plus__Plus_param_u32_u32", linkageName: "Plus__Plus_param_u32_u32", scope: !4, file: !3, line: 529, type: !6, scopeLine: 529, unit: !2, spFlags: DISPFlagDefinition)
!968 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_u8", linkageName: "Plus__uPlus_var_u32_u8", scope: !4, file: !3, line: 530, type: !6, scopeLine: 530, unit: !2, spFlags: DISPFlagDefinition)
!970 = distinct !DISubprogram(name: "Plus__Plus_var_u32_u8", linkageName: "Plus__Plus_var_u32_u8", scope: !4, file: !3, line: 531, type: !6, scopeLine: 531, unit: !2, spFlags: DISPFlagDefinition)
!972 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_u8", linkageName: "Plus__uPlus_param_u32_u8", scope: !4, file: !3, line: 532, type: !6, scopeLine: 532, unit: !2, spFlags: DISPFlagDefinition)
!974 = distinct !DISubprogram(name: "Plus__Plus_param_u32_u8", linkageName: "Plus__Plus_param_u32_u8", scope: !4, file: !3, line: 533, type: !6, scopeLine: 533, unit: !2, spFlags: DISPFlagDefinition)
!976 = distinct !DISubprogram(name: "Plus__uPlus_var_u32_L", linkageName: "Plus__uPlus_var_u32_L", scope: !4, file: !3, line: 534, type: !6, scopeLine: 534, unit: !2, spFlags: DISPFlagDefinition)
!978 = distinct !DISubprogram(name: "Plus__Plus_var_u32_L", linkageName: "Plus__Plus_var_u32_L", scope: !4, file: !3, line: 535, type: !6, scopeLine: 535, unit: !2, spFlags: DISPFlagDefinition)
!980 = distinct !DISubprogram(name: "Plus__uPlus_param_u32_L", linkageName: "Plus__uPlus_param_u32_L", scope: !4, file: !3, line: 536, type: !6, scopeLine: 536, unit: !2, spFlags: DISPFlagDefinition)
!982 = distinct !DISubprogram(name: "Plus__Plus_param_u32_L", linkageName: "Plus__Plus_param_u32_L", scope: !4, file: !3, line: 537, type: !6, scopeLine: 537, unit: !2, spFlags: DISPFlagDefinition)
!984 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_i8", linkageName: "Plus__uPlus_var_u8_i8", scope: !4, file: !3, line: 538, type: !6, scopeLine: 538, unit: !2, spFlags: DISPFlagDefinition)
!986 = distinct !DISubprogram(name: "Plus__Plus_var_u8_i8", linkageName: "Plus__Plus_var_u8_i8", scope: !4, file: !3, line: 539, type: !6, scopeLine: 539, unit: !2, spFlags: DISPFlagDefinition)
!988 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_i8", linkageName: "Plus__uPlus_param_u8_i8", scope: !4, file: !3, line: 540, type: !6, scopeLine: 540, unit: !2, spFlags: DISPFlagDefinition)
!990 = distinct !DISubprogram(name: "Plus__Plus_param_u8_i8", linkageName: "Plus__Plus_param_u8_i8", scope: !4, file: !3, line: 541, type: !6, scopeLine: 541, unit: !2, spFlags: DISPFlagDefinition)
!992 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_u64", linkageName: "Plus__uPlus_var_u8_u64", scope: !4, file: !3, line: 542, type: !6, scopeLine: 542, unit: !2, spFlags: DISPFlagDefinition)
!994 = distinct !DISubprogram(name: "Plus__Plus_var_u8_u64", linkageName: "Plus__Plus_var_u8_u64", scope: !4, file: !3, line: 543, type: !6, scopeLine: 543, unit: !2, spFlags: DISPFlagDefinition)
!996 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_u64", linkageName: "Plus__uPlus_param_u8_u64", scope: !4, file: !3, line: 544, type: !6, scopeLine: 544, unit: !2, spFlags: DISPFlagDefinition)
!998 = distinct !DISubprogram(name: "Plus__Plus_param_u8_u64", linkageName: "Plus__Plus_param_u8_u64", scope: !4, file: !3, line: 545, type: !6, scopeLine: 545, unit: !2, spFlags: DISPFlagDefinition)
!1000 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_i32", linkageName: "Plus__uPlus_var_u8_i32", scope: !4, file: !3, line: 546, type: !6, scopeLine: 546, unit: !2, spFlags: DISPFlagDefinition)
!1002 = distinct !DISubprogram(name: "Plus__Plus_var_u8_i32", linkageName: "Plus__Plus_var_u8_i32", scope: !4, file: !3, line: 547, type: !6, scopeLine: 547, unit: !2, spFlags: DISPFlagDefinition)
!1004 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_i32", linkageName: "Plus__uPlus_param_u8_i32", scope: !4, file: !3, line: 548, type: !6, scopeLine: 548, unit: !2, spFlags: DISPFlagDefinition)
!1006 = distinct !DISubprogram(name: "Plus__Plus_param_u8_i32", linkageName: "Plus__Plus_param_u8_i32", scope: !4, file: !3, line: 549, type: !6, scopeLine: 549, unit: !2, spFlags: DISPFlagDefinition)
!1008 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_LC", linkageName: "Plus__uPlus_var_u8_LC", scope: !4, file: !3, line: 550, type: !6, scopeLine: 550, unit: !2, spFlags: DISPFlagDefinition)
!1010 = distinct !DISubprogram(name: "Plus__Plus_var_u8_LC", linkageName: "Plus__Plus_var_u8_LC", scope: !4, file: !3, line: 551, type: !6, scopeLine: 551, unit: !2, spFlags: DISPFlagDefinition)
!1012 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_LC", linkageName: "Plus__uPlus_param_u8_LC", scope: !4, file: !3, line: 552, type: !6, scopeLine: 552, unit: !2, spFlags: DISPFlagDefinition)
!1014 = distinct !DISubprogram(name: "Plus__Plus_param_u8_LC", linkageName: "Plus__Plus_param_u8_LC", scope: !4, file: !3, line: 553, type: !6, scopeLine: 553, unit: !2, spFlags: DISPFlagDefinition)
!1016 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_u16", linkageName: "Plus__uPlus_var_u8_u16", scope: !4, file: !3, line: 554, type: !6, scopeLine: 554, unit: !2, spFlags: DISPFlagDefinition)
!1018 = distinct !DISubprogram(name: "Plus__Plus_var_u8_u16", linkageName: "Plus__Plus_var_u8_u16", scope: !4, file: !3, line: 555, type: !6, scopeLine: 555, unit: !2, spFlags: DISPFlagDefinition)
!1020 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_u16", linkageName: "Plus__uPlus_param_u8_u16", scope: !4, file: !3, line: 556, type: !6, scopeLine: 556, unit: !2, spFlags: DISPFlagDefinition)
!1022 = distinct !DISubprogram(name: "Plus__Plus_param_u8_u16", linkageName: "Plus__Plus_param_u8_u16", scope: !4, file: !3, line: 557, type: !6, scopeLine: 557, unit: !2, spFlags: DISPFlagDefinition)
!1024 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_I", linkageName: "Plus__uPlus_var_u8_I", scope: !4, file: !3, line: 558, type: !6, scopeLine: 558, unit: !2, spFlags: DISPFlagDefinition)
!1026 = distinct !DISubprogram(name: "Plus__Plus_var_u8_I", linkageName: "Plus__Plus_var_u8_I", scope: !4, file: !3, line: 559, type: !6, scopeLine: 559, unit: !2, spFlags: DISPFlagDefinition)
!1028 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_I", linkageName: "Plus__uPlus_param_u8_I", scope: !4, file: !3, line: 560, type: !6, scopeLine: 560, unit: !2, spFlags: DISPFlagDefinition)
!1030 = distinct !DISubprogram(name: "Plus__Plus_param_u8_I", linkageName: "Plus__Plus_param_u8_I", scope: !4, file: !3, line: 561, type: !6, scopeLine: 561, unit: !2, spFlags: DISPFlagDefinition)
!1032 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_i64", linkageName: "Plus__uPlus_var_u8_i64", scope: !4, file: !3, line: 562, type: !6, scopeLine: 562, unit: !2, spFlags: DISPFlagDefinition)
!1034 = distinct !DISubprogram(name: "Plus__Plus_var_u8_i64", linkageName: "Plus__Plus_var_u8_i64", scope: !4, file: !3, line: 563, type: !6, scopeLine: 563, unit: !2, spFlags: DISPFlagDefinition)
!1036 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_i64", linkageName: "Plus__uPlus_param_u8_i64", scope: !4, file: !3, line: 564, type: !6, scopeLine: 564, unit: !2, spFlags: DISPFlagDefinition)
!1038 = distinct !DISubprogram(name: "Plus__Plus_param_u8_i64", linkageName: "Plus__Plus_param_u8_i64", scope: !4, file: !3, line: 565, type: !6, scopeLine: 565, unit: !2, spFlags: DISPFlagDefinition)
!1040 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_i16", linkageName: "Plus__uPlus_var_u8_i16", scope: !4, file: !3, line: 566, type: !6, scopeLine: 566, unit: !2, spFlags: DISPFlagDefinition)
!1042 = distinct !DISubprogram(name: "Plus__Plus_var_u8_i16", linkageName: "Plus__Plus_var_u8_i16", scope: !4, file: !3, line: 567, type: !6, scopeLine: 567, unit: !2, spFlags: DISPFlagDefinition)
!1044 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_i16", linkageName: "Plus__uPlus_param_u8_i16", scope: !4, file: !3, line: 568, type: !6, scopeLine: 568, unit: !2, spFlags: DISPFlagDefinition)
!1046 = distinct !DISubprogram(name: "Plus__Plus_param_u8_i16", linkageName: "Plus__Plus_param_u8_i16", scope: !4, file: !3, line: 569, type: !6, scopeLine: 569, unit: !2, spFlags: DISPFlagDefinition)
!1048 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_C", linkageName: "Plus__uPlus_var_u8_C", scope: !4, file: !3, line: 570, type: !6, scopeLine: 570, unit: !2, spFlags: DISPFlagDefinition)
!1050 = distinct !DISubprogram(name: "Plus__Plus_var_u8_C", linkageName: "Plus__Plus_var_u8_C", scope: !4, file: !3, line: 571, type: !6, scopeLine: 571, unit: !2, spFlags: DISPFlagDefinition)
!1052 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_C", linkageName: "Plus__uPlus_param_u8_C", scope: !4, file: !3, line: 572, type: !6, scopeLine: 572, unit: !2, spFlags: DISPFlagDefinition)
!1054 = distinct !DISubprogram(name: "Plus__Plus_param_u8_C", linkageName: "Plus__Plus_param_u8_C", scope: !4, file: !3, line: 573, type: !6, scopeLine: 573, unit: !2, spFlags: DISPFlagDefinition)
!1056 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_u32", linkageName: "Plus__uPlus_var_u8_u32", scope: !4, file: !3, line: 574, type: !6, scopeLine: 574, unit: !2, spFlags: DISPFlagDefinition)
!1058 = distinct !DISubprogram(name: "Plus__Plus_var_u8_u32", linkageName: "Plus__Plus_var_u8_u32", scope: !4, file: !3, line: 575, type: !6, scopeLine: 575, unit: !2, spFlags: DISPFlagDefinition)
!1060 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_u32", linkageName: "Plus__uPlus_param_u8_u32", scope: !4, file: !3, line: 576, type: !6, scopeLine: 576, unit: !2, spFlags: DISPFlagDefinition)
!1062 = distinct !DISubprogram(name: "Plus__Plus_param_u8_u32", linkageName: "Plus__Plus_param_u8_u32", scope: !4, file: !3, line: 577, type: !6, scopeLine: 577, unit: !2, spFlags: DISPFlagDefinition)
!1064 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_u8", linkageName: "Plus__uPlus_var_u8_u8", scope: !4, file: !3, line: 578, type: !6, scopeLine: 578, unit: !2, spFlags: DISPFlagDefinition)
!1066 = distinct !DISubprogram(name: "Plus__Plus_var_u8_u8", linkageName: "Plus__Plus_var_u8_u8", scope: !4, file: !3, line: 579, type: !6, scopeLine: 579, unit: !2, spFlags: DISPFlagDefinition)
!1068 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_u8", linkageName: "Plus__uPlus_param_u8_u8", scope: !4, file: !3, line: 580, type: !6, scopeLine: 580, unit: !2, spFlags: DISPFlagDefinition)
!1070 = distinct !DISubprogram(name: "Plus__Plus_param_u8_u8", linkageName: "Plus__Plus_param_u8_u8", scope: !4, file: !3, line: 581, type: !6, scopeLine: 581, unit: !2, spFlags: DISPFlagDefinition)
!1072 = distinct !DISubprogram(name: "Plus__uPlus_var_u8_L", linkageName: "Plus__uPlus_var_u8_L", scope: !4, file: !3, line: 582, type: !6, scopeLine: 582, unit: !2, spFlags: DISPFlagDefinition)
!1074 = distinct !DISubprogram(name: "Plus__Plus_var_u8_L", linkageName: "Plus__Plus_var_u8_L", scope: !4, file: !3, line: 583, type: !6, scopeLine: 583, unit: !2, spFlags: DISPFlagDefinition)
!1076 = distinct !DISubprogram(name: "Plus__uPlus_param_u8_L", linkageName: "Plus__uPlus_param_u8_L", scope: !4, file: !3, line: 584, type: !6, scopeLine: 584, unit: !2, spFlags: DISPFlagDefinition)
!1078 = distinct !DISubprogram(name: "Plus__Plus_param_u8_L", linkageName: "Plus__Plus_param_u8_L", scope: !4, file: !3, line: 585, type: !6, scopeLine: 585, unit: !2, spFlags: DISPFlagDefinition)
!1080 = distinct !DISubprogram(name: "Plus__uPlus_var_L_i8", linkageName: "Plus__uPlus_var_L_i8", scope: !4, file: !3, line: 586, type: !6, scopeLine: 586, unit: !2, spFlags: DISPFlagDefinition)
!1082 = distinct !DISubprogram(name: "Plus__Plus_var_L_i8", linkageName: "Plus__Plus_var_L_i8", scope: !4, file: !3, line: 587, type: !6, scopeLine: 587, unit: !2, spFlags: DISPFlagDefinition)
!1084 = distinct !DISubprogram(name: "Plus__uPlus_param_L_i8", linkageName: "Plus__uPlus_param_L_i8", scope: !4, file: !3, line: 588, type: !6, scopeLine: 588, unit: !2, spFlags: DISPFlagDefinition)
!1086 = distinct !DISubprogram(name: "Plus__Plus_param_L_i8", linkageName: "Plus__Plus_param_L_i8", scope: !4, file: !3, line: 589, type: !6, scopeLine: 589, unit: !2, spFlags: DISPFlagDefinition)
!1088 = distinct !DISubprogram(name: "Plus__uPlus_var_L_u64", linkageName: "Plus__uPlus_var_L_u64", scope: !4, file: !3, line: 590, type: !6, scopeLine: 590, unit: !2, spFlags: DISPFlagDefinition)
!1090 = distinct !DISubprogram(name: "Plus__Plus_var_L_u64", linkageName: "Plus__Plus_var_L_u64", scope: !4, file: !3, line: 591, type: !6, scopeLine: 591, unit: !2, spFlags: DISPFlagDefinition)
!1092 = distinct !DISubprogram(name: "Plus__uPlus_param_L_u64", linkageName: "Plus__uPlus_param_L_u64", scope: !4, file: !3, line: 592, type: !6, scopeLine: 592, unit: !2, spFlags: DISPFlagDefinition)
!1094 = distinct !DISubprogram(name: "Plus__Plus_param_L_u64", linkageName: "Plus__Plus_param_L_u64", scope: !4, file: !3, line: 593, type: !6, scopeLine: 593, unit: !2, spFlags: DISPFlagDefinition)
!1096 = distinct !DISubprogram(name: "Plus__uPlus_var_L_i32", linkageName: "Plus__uPlus_var_L_i32", scope: !4, file: !3, line: 594, type: !6, scopeLine: 594, unit: !2, spFlags: DISPFlagDefinition)
!1098 = distinct !DISubprogram(name: "Plus__Plus_var_L_i32", linkageName: "Plus__Plus_var_L_i32", scope: !4, file: !3, line: 595, type: !6, scopeLine: 595, unit: !2, spFlags: DISPFlagDefinition)
!1100 = distinct !DISubprogram(name: "Plus__uPlus_param_L_i32", linkageName: "Plus__uPlus_param_L_i32", scope: !4, file: !3, line: 596, type: !6, scopeLine: 596, unit: !2, spFlags: DISPFlagDefinition)
!1102 = distinct !DISubprogram(name: "Plus__Plus_param_L_i32", linkageName: "Plus__Plus_param_L_i32", scope: !4, file: !3, line: 597, type: !6, scopeLine: 597, unit: !2, spFlags: DISPFlagDefinition)
!1104 = distinct !DISubprogram(name: "Plus__uPlus_var_L_LC", linkageName: "Plus__uPlus_var_L_LC", scope: !4, file: !3, line: 598, type: !6, scopeLine: 598, unit: !2, spFlags: DISPFlagDefinition)
!1106 = distinct !DISubprogram(name: "Plus__Plus_var_L_LC", linkageName: "Plus__Plus_var_L_LC", scope: !4, file: !3, line: 599, type: !6, scopeLine: 599, unit: !2, spFlags: DISPFlagDefinition)
!1108 = distinct !DISubprogram(name: "Plus__uPlus_param_L_LC", linkageName: "Plus__uPlus_param_L_LC", scope: !4, file: !3, line: 600, type: !6, scopeLine: 600, unit: !2, spFlags: DISPFlagDefinition)
!1110 = distinct !DISubprogram(name: "Plus__Plus_param_L_LC", linkageName: "Plus__Plus_param_L_LC", scope: !4, file: !3, line: 601, type: !6, scopeLine: 601, unit: !2, spFlags: DISPFlagDefinition)
!1112 = distinct !DISubprogram(name: "Plus__uPlus_var_L_u16", linkageName: "Plus__uPlus_var_L_u16", scope: !4, file: !3, line: 602, type: !6, scopeLine: 602, unit: !2, spFlags: DISPFlagDefinition)
!1114 = distinct !DISubprogram(name: "Plus__Plus_var_L_u16", linkageName: "Plus__Plus_var_L_u16", scope: !4, file: !3, line: 603, type: !6, scopeLine: 603, unit: !2, spFlags: DISPFlagDefinition)
!1116 = distinct !DISubprogram(name: "Plus__uPlus_param_L_u16", linkageName: "Plus__uPlus_param_L_u16", scope: !4, file: !3, line: 604, type: !6, scopeLine: 604, unit: !2, spFlags: DISPFlagDefinition)
!1118 = distinct !DISubprogram(name: "Plus__Plus_param_L_u16", linkageName: "Plus__Plus_param_L_u16", scope: !4, file: !3, line: 605, type: !6, scopeLine: 605, unit: !2, spFlags: DISPFlagDefinition)
!1120 = distinct !DISubprogram(name: "Plus__uPlus_var_L_I", linkageName: "Plus__uPlus_var_L_I", scope: !4, file: !3, line: 606, type: !6, scopeLine: 606, unit: !2, spFlags: DISPFlagDefinition)
!1122 = distinct !DISubprogram(name: "Plus__Plus_var_L_I", linkageName: "Plus__Plus_var_L_I", scope: !4, file: !3, line: 607, type: !6, scopeLine: 607, unit: !2, spFlags: DISPFlagDefinition)
!1124 = distinct !DISubprogram(name: "Plus__uPlus_param_L_I", linkageName: "Plus__uPlus_param_L_I", scope: !4, file: !3, line: 608, type: !6, scopeLine: 608, unit: !2, spFlags: DISPFlagDefinition)
!1126 = distinct !DISubprogram(name: "Plus__Plus_param_L_I", linkageName: "Plus__Plus_param_L_I", scope: !4, file: !3, line: 609, type: !6, scopeLine: 609, unit: !2, spFlags: DISPFlagDefinition)
!1128 = distinct !DISubprogram(name: "Plus__uPlus_var_L_i64", linkageName: "Plus__uPlus_var_L_i64", scope: !4, file: !3, line: 610, type: !6, scopeLine: 610, unit: !2, spFlags: DISPFlagDefinition)
!1130 = distinct !DISubprogram(name: "Plus__Plus_var_L_i64", linkageName: "Plus__Plus_var_L_i64", scope: !4, file: !3, line: 611, type: !6, scopeLine: 611, unit: !2, spFlags: DISPFlagDefinition)
!1132 = distinct !DISubprogram(name: "Plus__uPlus_param_L_i64", linkageName: "Plus__uPlus_param_L_i64", scope: !4, file: !3, line: 612, type: !6, scopeLine: 612, unit: !2, spFlags: DISPFlagDefinition)
!1134 = distinct !DISubprogram(name: "Plus__Plus_param_L_i64", linkageName: "Plus__Plus_param_L_i64", scope: !4, file: !3, line: 613, type: !6, scopeLine: 613, unit: !2, spFlags: DISPFlagDefinition)
!1136 = distinct !DISubprogram(name: "Plus__uPlus_var_L_i16", linkageName: "Plus__uPlus_var_L_i16", scope: !4, file: !3, line: 614, type: !6, scopeLine: 614, unit: !2, spFlags: DISPFlagDefinition)
!1138 = distinct !DISubprogram(name: "Plus__Plus_var_L_i16", linkageName: "Plus__Plus_var_L_i16", scope: !4, file: !3, line: 615, type: !6, scopeLine: 615, unit: !2, spFlags: DISPFlagDefinition)
!1140 = distinct !DISubprogram(name: "Plus__uPlus_param_L_i16", linkageName: "Plus__uPlus_param_L_i16", scope: !4, file: !3, line: 616, type: !6, scopeLine: 616, unit: !2, spFlags: DISPFlagDefinition)
!1142 = distinct !DISubprogram(name: "Plus__Plus_param_L_i16", linkageName: "Plus__Plus_param_L_i16", scope: !4, file: !3, line: 617, type: !6, scopeLine: 617, unit: !2, spFlags: DISPFlagDefinition)
!1144 = distinct !DISubprogram(name: "Plus__uPlus_var_L_C", linkageName: "Plus__uPlus_var_L_C", scope: !4, file: !3, line: 618, type: !6, scopeLine: 618, unit: !2, spFlags: DISPFlagDefinition)
!1146 = distinct !DISubprogram(name: "Plus__Plus_var_L_C", linkageName: "Plus__Plus_var_L_C", scope: !4, file: !3, line: 619, type: !6, scopeLine: 619, unit: !2, spFlags: DISPFlagDefinition)
!1148 = distinct !DISubprogram(name: "Plus__uPlus_param_L_C", linkageName: "Plus__uPlus_param_L_C", scope: !4, file: !3, line: 620, type: !6, scopeLine: 620, unit: !2, spFlags: DISPFlagDefinition)
!1150 = distinct !DISubprogram(name: "Plus__Plus_param_L_C", linkageName: "Plus__Plus_param_L_C", scope: !4, file: !3, line: 621, type: !6, scopeLine: 621, unit: !2, spFlags: DISPFlagDefinition)
!1152 = distinct !DISubprogram(name: "Plus__uPlus_var_L_u32", linkageName: "Plus__uPlus_var_L_u32", scope: !4, file: !3, line: 622, type: !6, scopeLine: 622, unit: !2, spFlags: DISPFlagDefinition)
!1154 = distinct !DISubprogram(name: "Plus__Plus_var_L_u32", linkageName: "Plus__Plus_var_L_u32", scope: !4, file: !3, line: 623, type: !6, scopeLine: 623, unit: !2, spFlags: DISPFlagDefinition)
!1156 = distinct !DISubprogram(name: "Plus__uPlus_param_L_u32", linkageName: "Plus__uPlus_param_L_u32", scope: !4, file: !3, line: 624, type: !6, scopeLine: 624, unit: !2, spFlags: DISPFlagDefinition)
!1158 = distinct !DISubprogram(name: "Plus__Plus_param_L_u32", linkageName: "Plus__Plus_param_L_u32", scope: !4, file: !3, line: 625, type: !6, scopeLine: 625, unit: !2, spFlags: DISPFlagDefinition)
!1160 = distinct !DISubprogram(name: "Plus__uPlus_var_L_u8", linkageName: "Plus__uPlus_var_L_u8", scope: !4, file: !3, line: 626, type: !6, scopeLine: 626, unit: !2, spFlags: DISPFlagDefinition)
!1162 = distinct !DISubprogram(name: "Plus__Plus_var_L_u8", linkageName: "Plus__Plus_var_L_u8", scope: !4, file: !3, line: 627, type: !6, scopeLine: 627, unit: !2, spFlags: DISPFlagDefinition)
!1164 = distinct !DISubprogram(name: "Plus__uPlus_param_L_u8", linkageName: "Plus__uPlus_param_L_u8", scope: !4, file: !3, line: 628, type: !6, scopeLine: 628, unit: !2, spFlags: DISPFlagDefinition)
!1166 = distinct !DISubprogram(name: "Plus__Plus_param_L_u8", linkageName: "Plus__Plus_param_L_u8", scope: !4, file: !3, line: 629, type: !6, scopeLine: 629, unit: !2, spFlags: DISPFlagDefinition)
!1168 = distinct !DISubprogram(name: "Plus__uPlus_var_L_L", linkageName: "Plus__uPlus_var_L_L", scope: !4, file: !3, line: 630, type: !6, scopeLine: 630, unit: !2, spFlags: DISPFlagDefinition)
!1170 = distinct !DISubprogram(name: "Plus__Plus_var_L_L", linkageName: "Plus__Plus_var_L_L", scope: !4, file: !3, line: 631, type: !6, scopeLine: 631, unit: !2, spFlags: DISPFlagDefinition)
!1172 = distinct !DISubprogram(name: "Plus__uPlus_param_L_L", linkageName: "Plus__uPlus_param_L_L", scope: !4, file: !3, line: 632, type: !6, scopeLine: 632, unit: !2, spFlags: DISPFlagDefinition)
!1174 = distinct !DISubprogram(name: "Plus__Plus_param_L_L", linkageName: "Plus__Plus_param_L_L", scope: !4, file: !3, line: 633, type: !6, scopeLine: 633, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!23 = !DILocation(line: 0, column: 0, scope: !22)
!25 = !DILocation(line: 0, column: 0, scope: !24)
!27 = !DILocation(line: 0, column: 0, scope: !26)
!29 = !DILocation(line: 0, column: 0, scope: !28)
!31 = !DILocation(line: 0, column: 0, scope: !30)
!33 = !DILocation(line: 0, column: 0, scope: !32)
!35 = !DILocation(line: 0, column: 0, scope: !34)
!37 = !DILocation(line: 0, column: 0, scope: !36)
!39 = !DILocation(line: 0, column: 0, scope: !38)
!41 = !DILocation(line: 0, column: 0, scope: !40)
!43 = !DILocation(line: 0, column: 0, scope: !42)
!45 = !DILocation(line: 0, column: 0, scope: !44)
!47 = !DILocation(line: 0, column: 0, scope: !46)
!49 = !DILocation(line: 0, column: 0, scope: !48)
!51 = !DILocation(line: 0, column: 0, scope: !50)
!53 = !DILocation(line: 0, column: 0, scope: !52)
!55 = !DILocation(line: 0, column: 0, scope: !54)
!57 = !DILocation(line: 0, column: 0, scope: !56)
!59 = !DILocation(line: 0, column: 0, scope: !58)
!61 = !DILocation(line: 0, column: 0, scope: !60)
!63 = !DILocation(line: 0, column: 0, scope: !62)
!65 = !DILocation(line: 0, column: 0, scope: !64)
!67 = !DILocation(line: 0, column: 0, scope: !66)
!69 = !DILocation(line: 0, column: 0, scope: !68)
!71 = !DILocation(line: 0, column: 0, scope: !70)
!73 = !DILocation(line: 0, column: 0, scope: !72)
!75 = !DILocation(line: 0, column: 0, scope: !74)
!77 = !DILocation(line: 0, column: 0, scope: !76)
!79 = !DILocation(line: 0, column: 0, scope: !78)
!81 = !DILocation(line: 0, column: 0, scope: !80)
!83 = !DILocation(line: 0, column: 0, scope: !82)
!85 = !DILocation(line: 0, column: 0, scope: !84)
!87 = !DILocation(line: 0, column: 0, scope: !86)
!89 = !DILocation(line: 0, column: 0, scope: !88)
!91 = !DILocation(line: 0, column: 0, scope: !90)
!93 = !DILocation(line: 0, column: 0, scope: !92)
!95 = !DILocation(line: 0, column: 0, scope: !94)
!97 = !DILocation(line: 0, column: 0, scope: !96)
!99 = !DILocation(line: 0, column: 0, scope: !98)
!101 = !DILocation(line: 0, column: 0, scope: !100)
!103 = !DILocation(line: 0, column: 0, scope: !102)
!105 = !DILocation(line: 0, column: 0, scope: !104)
!107 = !DILocation(line: 0, column: 0, scope: !106)
!109 = !DILocation(line: 0, column: 0, scope: !108)
!111 = !DILocation(line: 0, column: 0, scope: !110)
!113 = !DILocation(line: 0, column: 0, scope: !112)
!115 = !DILocation(line: 0, column: 0, scope: !114)
!117 = !DILocation(line: 0, column: 0, scope: !116)
!119 = !DILocation(line: 0, column: 0, scope: !118)
!121 = !DILocation(line: 0, column: 0, scope: !120)
!123 = !DILocation(line: 0, column: 0, scope: !122)
!125 = !DILocation(line: 0, column: 0, scope: !124)
!127 = !DILocation(line: 0, column: 0, scope: !126)
!129 = !DILocation(line: 0, column: 0, scope: !128)
!131 = !DILocation(line: 0, column: 0, scope: !130)
!133 = !DILocation(line: 0, column: 0, scope: !132)
!135 = !DILocation(line: 0, column: 0, scope: !134)
!137 = !DILocation(line: 0, column: 0, scope: !136)
!139 = !DILocation(line: 0, column: 0, scope: !138)
!141 = !DILocation(line: 0, column: 0, scope: !140)
!143 = !DILocation(line: 0, column: 0, scope: !142)
!145 = !DILocation(line: 0, column: 0, scope: !144)
!147 = !DILocation(line: 0, column: 0, scope: !146)
!149 = !DILocation(line: 0, column: 0, scope: !148)
!151 = !DILocation(line: 0, column: 0, scope: !150)
!153 = !DILocation(line: 0, column: 0, scope: !152)
!155 = !DILocation(line: 0, column: 0, scope: !154)
!157 = !DILocation(line: 0, column: 0, scope: !156)
!159 = !DILocation(line: 0, column: 0, scope: !158)
!161 = !DILocation(line: 0, column: 0, scope: !160)
!163 = !DILocation(line: 0, column: 0, scope: !162)
!165 = !DILocation(line: 0, column: 0, scope: !164)
!167 = !DILocation(line: 0, column: 0, scope: !166)
!169 = !DILocation(line: 0, column: 0, scope: !168)
!171 = !DILocation(line: 0, column: 0, scope: !170)
!173 = !DILocation(line: 0, column: 0, scope: !172)
!175 = !DILocation(line: 0, column: 0, scope: !174)
!177 = !DILocation(line: 0, column: 0, scope: !176)
!179 = !DILocation(line: 0, column: 0, scope: !178)
!181 = !DILocation(line: 0, column: 0, scope: !180)
!183 = !DILocation(line: 0, column: 0, scope: !182)
!185 = !DILocation(line: 0, column: 0, scope: !184)
!187 = !DILocation(line: 0, column: 0, scope: !186)
!189 = !DILocation(line: 0, column: 0, scope: !188)
!191 = !DILocation(line: 0, column: 0, scope: !190)
!193 = !DILocation(line: 0, column: 0, scope: !192)
!195 = !DILocation(line: 0, column: 0, scope: !194)
!197 = !DILocation(line: 0, column: 0, scope: !196)
!199 = !DILocation(line: 0, column: 0, scope: !198)
!201 = !DILocation(line: 0, column: 0, scope: !200)
!203 = !DILocation(line: 0, column: 0, scope: !202)
!205 = !DILocation(line: 0, column: 0, scope: !204)
!207 = !DILocation(line: 0, column: 0, scope: !206)
!209 = !DILocation(line: 0, column: 0, scope: !208)
!211 = !DILocation(line: 0, column: 0, scope: !210)
!213 = !DILocation(line: 0, column: 0, scope: !212)
!215 = !DILocation(line: 0, column: 0, scope: !214)
!217 = !DILocation(line: 0, column: 0, scope: !216)
!219 = !DILocation(line: 0, column: 0, scope: !218)
!221 = !DILocation(line: 0, column: 0, scope: !220)
!223 = !DILocation(line: 0, column: 0, scope: !222)
!225 = !DILocation(line: 0, column: 0, scope: !224)
!227 = !DILocation(line: 0, column: 0, scope: !226)
!229 = !DILocation(line: 0, column: 0, scope: !228)
!231 = !DILocation(line: 0, column: 0, scope: !230)
!233 = !DILocation(line: 0, column: 0, scope: !232)
!235 = !DILocation(line: 0, column: 0, scope: !234)
!237 = !DILocation(line: 0, column: 0, scope: !236)
!239 = !DILocation(line: 0, column: 0, scope: !238)
!241 = !DILocation(line: 0, column: 0, scope: !240)
!243 = !DILocation(line: 0, column: 0, scope: !242)
!245 = !DILocation(line: 0, column: 0, scope: !244)
!247 = !DILocation(line: 0, column: 0, scope: !246)
!249 = !DILocation(line: 0, column: 0, scope: !248)
!251 = !DILocation(line: 0, column: 0, scope: !250)
!253 = !DILocation(line: 0, column: 0, scope: !252)
!255 = !DILocation(line: 0, column: 0, scope: !254)
!257 = !DILocation(line: 0, column: 0, scope: !256)
!259 = !DILocation(line: 0, column: 0, scope: !258)
!261 = !DILocation(line: 0, column: 0, scope: !260)
!263 = !DILocation(line: 0, column: 0, scope: !262)
!265 = !DILocation(line: 0, column: 0, scope: !264)
!267 = !DILocation(line: 0, column: 0, scope: !266)
!269 = !DILocation(line: 0, column: 0, scope: !268)
!271 = !DILocation(line: 0, column: 0, scope: !270)
!273 = !DILocation(line: 0, column: 0, scope: !272)
!275 = !DILocation(line: 0, column: 0, scope: !274)
!277 = !DILocation(line: 0, column: 0, scope: !276)
!279 = !DILocation(line: 0, column: 0, scope: !278)
!281 = !DILocation(line: 0, column: 0, scope: !280)
!283 = !DILocation(line: 0, column: 0, scope: !282)
!285 = !DILocation(line: 0, column: 0, scope: !284)
!287 = !DILocation(line: 0, column: 0, scope: !286)
!289 = !DILocation(line: 0, column: 0, scope: !288)
!291 = !DILocation(line: 0, column: 0, scope: !290)
!293 = !DILocation(line: 0, column: 0, scope: !292)
!295 = !DILocation(line: 0, column: 0, scope: !294)
!297 = !DILocation(line: 0, column: 0, scope: !296)
!299 = !DILocation(line: 0, column: 0, scope: !298)
!301 = !DILocation(line: 0, column: 0, scope: !300)
!303 = !DILocation(line: 0, column: 0, scope: !302)
!305 = !DILocation(line: 0, column: 0, scope: !304)
!307 = !DILocation(line: 0, column: 0, scope: !306)
!309 = !DILocation(line: 0, column: 0, scope: !308)
!311 = !DILocation(line: 0, column: 0, scope: !310)
!313 = !DILocation(line: 0, column: 0, scope: !312)
!315 = !DILocation(line: 0, column: 0, scope: !314)
!317 = !DILocation(line: 0, column: 0, scope: !316)
!319 = !DILocation(line: 0, column: 0, scope: !318)
!321 = !DILocation(line: 0, column: 0, scope: !320)
!323 = !DILocation(line: 0, column: 0, scope: !322)
!325 = !DILocation(line: 0, column: 0, scope: !324)
!327 = !DILocation(line: 0, column: 0, scope: !326)
!329 = !DILocation(line: 0, column: 0, scope: !328)
!331 = !DILocation(line: 0, column: 0, scope: !330)
!333 = !DILocation(line: 0, column: 0, scope: !332)
!335 = !DILocation(line: 0, column: 0, scope: !334)
!337 = !DILocation(line: 0, column: 0, scope: !336)
!339 = !DILocation(line: 0, column: 0, scope: !338)
!341 = !DILocation(line: 0, column: 0, scope: !340)
!343 = !DILocation(line: 0, column: 0, scope: !342)
!345 = !DILocation(line: 0, column: 0, scope: !344)
!347 = !DILocation(line: 0, column: 0, scope: !346)
!349 = !DILocation(line: 0, column: 0, scope: !348)
!351 = !DILocation(line: 0, column: 0, scope: !350)
!353 = !DILocation(line: 0, column: 0, scope: !352)
!355 = !DILocation(line: 0, column: 0, scope: !354)
!357 = !DILocation(line: 0, column: 0, scope: !356)
!359 = !DILocation(line: 0, column: 0, scope: !358)
!361 = !DILocation(line: 0, column: 0, scope: !360)
!363 = !DILocation(line: 0, column: 0, scope: !362)
!365 = !DILocation(line: 0, column: 0, scope: !364)
!367 = !DILocation(line: 0, column: 0, scope: !366)
!369 = !DILocation(line: 0, column: 0, scope: !368)
!371 = !DILocation(line: 0, column: 0, scope: !370)
!373 = !DILocation(line: 0, column: 0, scope: !372)
!375 = !DILocation(line: 0, column: 0, scope: !374)
!377 = !DILocation(line: 0, column: 0, scope: !376)
!379 = !DILocation(line: 0, column: 0, scope: !378)
!381 = !DILocation(line: 0, column: 0, scope: !380)
!383 = !DILocation(line: 0, column: 0, scope: !382)
!385 = !DILocation(line: 0, column: 0, scope: !384)
!387 = !DILocation(line: 0, column: 0, scope: !386)
!389 = !DILocation(line: 0, column: 0, scope: !388)
!391 = !DILocation(line: 0, column: 0, scope: !390)
!393 = !DILocation(line: 0, column: 0, scope: !392)
!395 = !DILocation(line: 0, column: 0, scope: !394)
!397 = !DILocation(line: 0, column: 0, scope: !396)
!399 = !DILocation(line: 0, column: 0, scope: !398)
!401 = !DILocation(line: 0, column: 0, scope: !400)
!403 = !DILocation(line: 0, column: 0, scope: !402)
!405 = !DILocation(line: 0, column: 0, scope: !404)
!407 = !DILocation(line: 0, column: 0, scope: !406)
!409 = !DILocation(line: 0, column: 0, scope: !408)
!411 = !DILocation(line: 0, column: 0, scope: !410)
!413 = !DILocation(line: 0, column: 0, scope: !412)
!415 = !DILocation(line: 0, column: 0, scope: !414)
!417 = !DILocation(line: 0, column: 0, scope: !416)
!419 = !DILocation(line: 0, column: 0, scope: !418)
!421 = !DILocation(line: 0, column: 0, scope: !420)
!423 = !DILocation(line: 0, column: 0, scope: !422)
!425 = !DILocation(line: 0, column: 0, scope: !424)
!427 = !DILocation(line: 0, column: 0, scope: !426)
!429 = !DILocation(line: 0, column: 0, scope: !428)
!431 = !DILocation(line: 0, column: 0, scope: !430)
!433 = !DILocation(line: 0, column: 0, scope: !432)
!435 = !DILocation(line: 0, column: 0, scope: !434)
!437 = !DILocation(line: 0, column: 0, scope: !436)
!439 = !DILocation(line: 0, column: 0, scope: !438)
!441 = !DILocation(line: 0, column: 0, scope: !440)
!443 = !DILocation(line: 0, column: 0, scope: !442)
!445 = !DILocation(line: 0, column: 0, scope: !444)
!447 = !DILocation(line: 0, column: 0, scope: !446)
!449 = !DILocation(line: 0, column: 0, scope: !448)
!451 = !DILocation(line: 0, column: 0, scope: !450)
!453 = !DILocation(line: 0, column: 0, scope: !452)
!455 = !DILocation(line: 0, column: 0, scope: !454)
!457 = !DILocation(line: 0, column: 0, scope: !456)
!459 = !DILocation(line: 0, column: 0, scope: !458)
!461 = !DILocation(line: 0, column: 0, scope: !460)
!463 = !DILocation(line: 0, column: 0, scope: !462)
!465 = !DILocation(line: 0, column: 0, scope: !464)
!467 = !DILocation(line: 0, column: 0, scope: !466)
!469 = !DILocation(line: 0, column: 0, scope: !468)
!471 = !DILocation(line: 0, column: 0, scope: !470)
!473 = !DILocation(line: 0, column: 0, scope: !472)
!475 = !DILocation(line: 0, column: 0, scope: !474)
!477 = !DILocation(line: 0, column: 0, scope: !476)
!479 = !DILocation(line: 0, column: 0, scope: !478)
!481 = !DILocation(line: 0, column: 0, scope: !480)
!483 = !DILocation(line: 0, column: 0, scope: !482)
!485 = !DILocation(line: 0, column: 0, scope: !484)
!487 = !DILocation(line: 0, column: 0, scope: !486)
!489 = !DILocation(line: 0, column: 0, scope: !488)
!491 = !DILocation(line: 0, column: 0, scope: !490)
!493 = !DILocation(line: 0, column: 0, scope: !492)
!495 = !DILocation(line: 0, column: 0, scope: !494)
!497 = !DILocation(line: 0, column: 0, scope: !496)
!499 = !DILocation(line: 0, column: 0, scope: !498)
!501 = !DILocation(line: 0, column: 0, scope: !500)
!503 = !DILocation(line: 0, column: 0, scope: !502)
!505 = !DILocation(line: 0, column: 0, scope: !504)
!507 = !DILocation(line: 0, column: 0, scope: !506)
!509 = !DILocation(line: 0, column: 0, scope: !508)
!511 = !DILocation(line: 0, column: 0, scope: !510)
!513 = !DILocation(line: 0, column: 0, scope: !512)
!515 = !DILocation(line: 0, column: 0, scope: !514)
!517 = !DILocation(line: 0, column: 0, scope: !516)
!519 = !DILocation(line: 0, column: 0, scope: !518)
!521 = !DILocation(line: 0, column: 0, scope: !520)
!523 = !DILocation(line: 0, column: 0, scope: !522)
!525 = !DILocation(line: 0, column: 0, scope: !524)
!527 = !DILocation(line: 0, column: 0, scope: !526)
!529 = !DILocation(line: 0, column: 0, scope: !528)
!531 = !DILocation(line: 0, column: 0, scope: !530)
!533 = !DILocation(line: 0, column: 0, scope: !532)
!535 = !DILocation(line: 0, column: 0, scope: !534)
!537 = !DILocation(line: 0, column: 0, scope: !536)
!539 = !DILocation(line: 0, column: 0, scope: !538)
!541 = !DILocation(line: 0, column: 0, scope: !540)
!543 = !DILocation(line: 0, column: 0, scope: !542)
!545 = !DILocation(line: 0, column: 0, scope: !544)
!547 = !DILocation(line: 0, column: 0, scope: !546)
!549 = !DILocation(line: 0, column: 0, scope: !548)
!551 = !DILocation(line: 0, column: 0, scope: !550)
!553 = !DILocation(line: 0, column: 0, scope: !552)
!555 = !DILocation(line: 0, column: 0, scope: !554)
!557 = !DILocation(line: 0, column: 0, scope: !556)
!559 = !DILocation(line: 0, column: 0, scope: !558)
!561 = !DILocation(line: 0, column: 0, scope: !560)
!563 = !DILocation(line: 0, column: 0, scope: !562)
!565 = !DILocation(line: 0, column: 0, scope: !564)
!567 = !DILocation(line: 0, column: 0, scope: !566)
!569 = !DILocation(line: 0, column: 0, scope: !568)
!571 = !DILocation(line: 0, column: 0, scope: !570)
!573 = !DILocation(line: 0, column: 0, scope: !572)
!575 = !DILocation(line: 0, column: 0, scope: !574)
!577 = !DILocation(line: 0, column: 0, scope: !576)
!579 = !DILocation(line: 0, column: 0, scope: !578)
!581 = !DILocation(line: 0, column: 0, scope: !580)
!583 = !DILocation(line: 0, column: 0, scope: !582)
!585 = !DILocation(line: 0, column: 0, scope: !584)
!587 = !DILocation(line: 0, column: 0, scope: !586)
!589 = !DILocation(line: 0, column: 0, scope: !588)
!591 = !DILocation(line: 0, column: 0, scope: !590)
!593 = !DILocation(line: 0, column: 0, scope: !592)
!595 = !DILocation(line: 0, column: 0, scope: !594)
!597 = !DILocation(line: 0, column: 0, scope: !596)
!599 = !DILocation(line: 0, column: 0, scope: !598)
!601 = !DILocation(line: 0, column: 0, scope: !600)
!603 = !DILocation(line: 0, column: 0, scope: !602)
!605 = !DILocation(line: 0, column: 0, scope: !604)
!607 = !DILocation(line: 0, column: 0, scope: !606)
!609 = !DILocation(line: 0, column: 0, scope: !608)
!611 = !DILocation(line: 0, column: 0, scope: !610)
!613 = !DILocation(line: 0, column: 0, scope: !612)
!615 = !DILocation(line: 0, column: 0, scope: !614)
!617 = !DILocation(line: 0, column: 0, scope: !616)
!619 = !DILocation(line: 0, column: 0, scope: !618)
!621 = !DILocation(line: 0, column: 0, scope: !620)
!623 = !DILocation(line: 0, column: 0, scope: !622)
!625 = !DILocation(line: 0, column: 0, scope: !624)
!627 = !DILocation(line: 0, column: 0, scope: !626)
!629 = !DILocation(line: 0, column: 0, scope: !628)
!631 = !DILocation(line: 0, column: 0, scope: !630)
!633 = !DILocation(line: 0, column: 0, scope: !632)
!635 = !DILocation(line: 0, column: 0, scope: !634)
!637 = !DILocation(line: 0, column: 0, scope: !636)
!639 = !DILocation(line: 0, column: 0, scope: !638)
!641 = !DILocation(line: 0, column: 0, scope: !640)
!643 = !DILocation(line: 0, column: 0, scope: !642)
!645 = !DILocation(line: 0, column: 0, scope: !644)
!647 = !DILocation(line: 0, column: 0, scope: !646)
!649 = !DILocation(line: 0, column: 0, scope: !648)
!651 = !DILocation(line: 0, column: 0, scope: !650)
!653 = !DILocation(line: 0, column: 0, scope: !652)
!655 = !DILocation(line: 0, column: 0, scope: !654)
!657 = !DILocation(line: 0, column: 0, scope: !656)
!659 = !DILocation(line: 0, column: 0, scope: !658)
!661 = !DILocation(line: 0, column: 0, scope: !660)
!663 = !DILocation(line: 0, column: 0, scope: !662)
!665 = !DILocation(line: 0, column: 0, scope: !664)
!667 = !DILocation(line: 0, column: 0, scope: !666)
!669 = !DILocation(line: 0, column: 0, scope: !668)
!671 = !DILocation(line: 0, column: 0, scope: !670)
!673 = !DILocation(line: 0, column: 0, scope: !672)
!675 = !DILocation(line: 0, column: 0, scope: !674)
!677 = !DILocation(line: 0, column: 0, scope: !676)
!679 = !DILocation(line: 0, column: 0, scope: !678)
!681 = !DILocation(line: 0, column: 0, scope: !680)
!683 = !DILocation(line: 0, column: 0, scope: !682)
!685 = !DILocation(line: 0, column: 0, scope: !684)
!687 = !DILocation(line: 0, column: 0, scope: !686)
!689 = !DILocation(line: 0, column: 0, scope: !688)
!691 = !DILocation(line: 0, column: 0, scope: !690)
!693 = !DILocation(line: 0, column: 0, scope: !692)
!695 = !DILocation(line: 0, column: 0, scope: !694)
!697 = !DILocation(line: 0, column: 0, scope: !696)
!699 = !DILocation(line: 0, column: 0, scope: !698)
!701 = !DILocation(line: 0, column: 0, scope: !700)
!703 = !DILocation(line: 0, column: 0, scope: !702)
!705 = !DILocation(line: 0, column: 0, scope: !704)
!707 = !DILocation(line: 0, column: 0, scope: !706)
!709 = !DILocation(line: 0, column: 0, scope: !708)
!711 = !DILocation(line: 0, column: 0, scope: !710)
!713 = !DILocation(line: 0, column: 0, scope: !712)
!715 = !DILocation(line: 0, column: 0, scope: !714)
!717 = !DILocation(line: 0, column: 0, scope: !716)
!719 = !DILocation(line: 0, column: 0, scope: !718)
!721 = !DILocation(line: 0, column: 0, scope: !720)
!723 = !DILocation(line: 0, column: 0, scope: !722)
!725 = !DILocation(line: 0, column: 0, scope: !724)
!727 = !DILocation(line: 0, column: 0, scope: !726)
!729 = !DILocation(line: 0, column: 0, scope: !728)
!731 = !DILocation(line: 0, column: 0, scope: !730)
!733 = !DILocation(line: 0, column: 0, scope: !732)
!735 = !DILocation(line: 0, column: 0, scope: !734)
!737 = !DILocation(line: 0, column: 0, scope: !736)
!739 = !DILocation(line: 0, column: 0, scope: !738)
!741 = !DILocation(line: 0, column: 0, scope: !740)
!743 = !DILocation(line: 0, column: 0, scope: !742)
!745 = !DILocation(line: 0, column: 0, scope: !744)
!747 = !DILocation(line: 0, column: 0, scope: !746)
!749 = !DILocation(line: 0, column: 0, scope: !748)
!751 = !DILocation(line: 0, column: 0, scope: !750)
!753 = !DILocation(line: 0, column: 0, scope: !752)
!755 = !DILocation(line: 0, column: 0, scope: !754)
!757 = !DILocation(line: 0, column: 0, scope: !756)
!759 = !DILocation(line: 0, column: 0, scope: !758)
!761 = !DILocation(line: 0, column: 0, scope: !760)
!763 = !DILocation(line: 0, column: 0, scope: !762)
!765 = !DILocation(line: 0, column: 0, scope: !764)
!767 = !DILocation(line: 0, column: 0, scope: !766)
!769 = !DILocation(line: 0, column: 0, scope: !768)
!771 = !DILocation(line: 0, column: 0, scope: !770)
!773 = !DILocation(line: 0, column: 0, scope: !772)
!775 = !DILocation(line: 0, column: 0, scope: !774)
!777 = !DILocation(line: 0, column: 0, scope: !776)
!779 = !DILocation(line: 0, column: 0, scope: !778)
!781 = !DILocation(line: 0, column: 0, scope: !780)
!783 = !DILocation(line: 0, column: 0, scope: !782)
!785 = !DILocation(line: 0, column: 0, scope: !784)
!787 = !DILocation(line: 0, column: 0, scope: !786)
!789 = !DILocation(line: 0, column: 0, scope: !788)
!791 = !DILocation(line: 0, column: 0, scope: !790)
!793 = !DILocation(line: 0, column: 0, scope: !792)
!795 = !DILocation(line: 0, column: 0, scope: !794)
!797 = !DILocation(line: 0, column: 0, scope: !796)
!799 = !DILocation(line: 0, column: 0, scope: !798)
!801 = !DILocation(line: 0, column: 0, scope: !800)
!803 = !DILocation(line: 0, column: 0, scope: !802)
!805 = !DILocation(line: 0, column: 0, scope: !804)
!807 = !DILocation(line: 0, column: 0, scope: !806)
!809 = !DILocation(line: 0, column: 0, scope: !808)
!811 = !DILocation(line: 0, column: 0, scope: !810)
!813 = !DILocation(line: 0, column: 0, scope: !812)
!815 = !DILocation(line: 0, column: 0, scope: !814)
!817 = !DILocation(line: 0, column: 0, scope: !816)
!819 = !DILocation(line: 0, column: 0, scope: !818)
!821 = !DILocation(line: 0, column: 0, scope: !820)
!823 = !DILocation(line: 0, column: 0, scope: !822)
!825 = !DILocation(line: 0, column: 0, scope: !824)
!827 = !DILocation(line: 0, column: 0, scope: !826)
!829 = !DILocation(line: 0, column: 0, scope: !828)
!831 = !DILocation(line: 0, column: 0, scope: !830)
!833 = !DILocation(line: 0, column: 0, scope: !832)
!835 = !DILocation(line: 0, column: 0, scope: !834)
!837 = !DILocation(line: 0, column: 0, scope: !836)
!839 = !DILocation(line: 0, column: 0, scope: !838)
!841 = !DILocation(line: 0, column: 0, scope: !840)
!843 = !DILocation(line: 0, column: 0, scope: !842)
!845 = !DILocation(line: 0, column: 0, scope: !844)
!847 = !DILocation(line: 0, column: 0, scope: !846)
!849 = !DILocation(line: 0, column: 0, scope: !848)
!851 = !DILocation(line: 0, column: 0, scope: !850)
!853 = !DILocation(line: 0, column: 0, scope: !852)
!855 = !DILocation(line: 0, column: 0, scope: !854)
!857 = !DILocation(line: 0, column: 0, scope: !856)
!859 = !DILocation(line: 0, column: 0, scope: !858)
!861 = !DILocation(line: 0, column: 0, scope: !860)
!863 = !DILocation(line: 0, column: 0, scope: !862)
!865 = !DILocation(line: 0, column: 0, scope: !864)
!867 = !DILocation(line: 0, column: 0, scope: !866)
!869 = !DILocation(line: 0, column: 0, scope: !868)
!871 = !DILocation(line: 0, column: 0, scope: !870)
!873 = !DILocation(line: 0, column: 0, scope: !872)
!875 = !DILocation(line: 0, column: 0, scope: !874)
!877 = !DILocation(line: 0, column: 0, scope: !876)
!879 = !DILocation(line: 0, column: 0, scope: !878)
!881 = !DILocation(line: 0, column: 0, scope: !880)
!883 = !DILocation(line: 0, column: 0, scope: !882)
!885 = !DILocation(line: 0, column: 0, scope: !884)
!887 = !DILocation(line: 0, column: 0, scope: !886)
!889 = !DILocation(line: 0, column: 0, scope: !888)
!891 = !DILocation(line: 0, column: 0, scope: !890)
!893 = !DILocation(line: 0, column: 0, scope: !892)
!895 = !DILocation(line: 0, column: 0, scope: !894)
!897 = !DILocation(line: 0, column: 0, scope: !896)
!899 = !DILocation(line: 0, column: 0, scope: !898)
!901 = !DILocation(line: 0, column: 0, scope: !900)
!903 = !DILocation(line: 0, column: 0, scope: !902)
!905 = !DILocation(line: 0, column: 0, scope: !904)
!907 = !DILocation(line: 0, column: 0, scope: !906)
!909 = !DILocation(line: 0, column: 0, scope: !908)
!911 = !DILocation(line: 0, column: 0, scope: !910)
!913 = !DILocation(line: 0, column: 0, scope: !912)
!915 = !DILocation(line: 0, column: 0, scope: !914)
!917 = !DILocation(line: 0, column: 0, scope: !916)
!919 = !DILocation(line: 0, column: 0, scope: !918)
!921 = !DILocation(line: 0, column: 0, scope: !920)
!923 = !DILocation(line: 0, column: 0, scope: !922)
!925 = !DILocation(line: 0, column: 0, scope: !924)
!927 = !DILocation(line: 0, column: 0, scope: !926)
!929 = !DILocation(line: 0, column: 0, scope: !928)
!931 = !DILocation(line: 0, column: 0, scope: !930)
!933 = !DILocation(line: 0, column: 0, scope: !932)
!935 = !DILocation(line: 0, column: 0, scope: !934)
!937 = !DILocation(line: 0, column: 0, scope: !936)
!939 = !DILocation(line: 0, column: 0, scope: !938)
!941 = !DILocation(line: 0, column: 0, scope: !940)
!943 = !DILocation(line: 0, column: 0, scope: !942)
!945 = !DILocation(line: 0, column: 0, scope: !944)
!947 = !DILocation(line: 0, column: 0, scope: !946)
!949 = !DILocation(line: 0, column: 0, scope: !948)
!951 = !DILocation(line: 0, column: 0, scope: !950)
!953 = !DILocation(line: 0, column: 0, scope: !952)
!955 = !DILocation(line: 0, column: 0, scope: !954)
!957 = !DILocation(line: 0, column: 0, scope: !956)
!959 = !DILocation(line: 0, column: 0, scope: !958)
!961 = !DILocation(line: 0, column: 0, scope: !960)
!963 = !DILocation(line: 0, column: 0, scope: !962)
!965 = !DILocation(line: 0, column: 0, scope: !964)
!967 = !DILocation(line: 0, column: 0, scope: !966)
!969 = !DILocation(line: 0, column: 0, scope: !968)
!971 = !DILocation(line: 0, column: 0, scope: !970)
!973 = !DILocation(line: 0, column: 0, scope: !972)
!975 = !DILocation(line: 0, column: 0, scope: !974)
!977 = !DILocation(line: 0, column: 0, scope: !976)
!979 = !DILocation(line: 0, column: 0, scope: !978)
!981 = !DILocation(line: 0, column: 0, scope: !980)
!983 = !DILocation(line: 0, column: 0, scope: !982)
!985 = !DILocation(line: 0, column: 0, scope: !984)
!987 = !DILocation(line: 0, column: 0, scope: !986)
!989 = !DILocation(line: 0, column: 0, scope: !988)
!991 = !DILocation(line: 0, column: 0, scope: !990)
!993 = !DILocation(line: 0, column: 0, scope: !992)
!995 = !DILocation(line: 0, column: 0, scope: !994)
!997 = !DILocation(line: 0, column: 0, scope: !996)
!999 = !DILocation(line: 0, column: 0, scope: !998)
!1001 = !DILocation(line: 0, column: 0, scope: !1000)
!1003 = !DILocation(line: 0, column: 0, scope: !1002)
!1005 = !DILocation(line: 0, column: 0, scope: !1004)
!1007 = !DILocation(line: 0, column: 0, scope: !1006)
!1009 = !DILocation(line: 0, column: 0, scope: !1008)
!1011 = !DILocation(line: 0, column: 0, scope: !1010)
!1013 = !DILocation(line: 0, column: 0, scope: !1012)
!1015 = !DILocation(line: 0, column: 0, scope: !1014)
!1017 = !DILocation(line: 0, column: 0, scope: !1016)
!1019 = !DILocation(line: 0, column: 0, scope: !1018)
!1021 = !DILocation(line: 0, column: 0, scope: !1020)
!1023 = !DILocation(line: 0, column: 0, scope: !1022)
!1025 = !DILocation(line: 0, column: 0, scope: !1024)
!1027 = !DILocation(line: 0, column: 0, scope: !1026)
!1029 = !DILocation(line: 0, column: 0, scope: !1028)
!1031 = !DILocation(line: 0, column: 0, scope: !1030)
!1033 = !DILocation(line: 0, column: 0, scope: !1032)
!1035 = !DILocation(line: 0, column: 0, scope: !1034)
!1037 = !DILocation(line: 0, column: 0, scope: !1036)
!1039 = !DILocation(line: 0, column: 0, scope: !1038)
!1041 = !DILocation(line: 0, column: 0, scope: !1040)
!1043 = !DILocation(line: 0, column: 0, scope: !1042)
!1045 = !DILocation(line: 0, column: 0, scope: !1044)
!1047 = !DILocation(line: 0, column: 0, scope: !1046)
!1049 = !DILocation(line: 0, column: 0, scope: !1048)
!1051 = !DILocation(line: 0, column: 0, scope: !1050)
!1053 = !DILocation(line: 0, column: 0, scope: !1052)
!1055 = !DILocation(line: 0, column: 0, scope: !1054)
!1057 = !DILocation(line: 0, column: 0, scope: !1056)
!1059 = !DILocation(line: 0, column: 0, scope: !1058)
!1061 = !DILocation(line: 0, column: 0, scope: !1060)
!1063 = !DILocation(line: 0, column: 0, scope: !1062)
!1065 = !DILocation(line: 0, column: 0, scope: !1064)
!1067 = !DILocation(line: 0, column: 0, scope: !1066)
!1069 = !DILocation(line: 0, column: 0, scope: !1068)
!1071 = !DILocation(line: 0, column: 0, scope: !1070)
!1073 = !DILocation(line: 0, column: 0, scope: !1072)
!1075 = !DILocation(line: 0, column: 0, scope: !1074)
!1077 = !DILocation(line: 0, column: 0, scope: !1076)
!1079 = !DILocation(line: 0, column: 0, scope: !1078)
!1081 = !DILocation(line: 0, column: 0, scope: !1080)
!1083 = !DILocation(line: 0, column: 0, scope: !1082)
!1085 = !DILocation(line: 0, column: 0, scope: !1084)
!1087 = !DILocation(line: 0, column: 0, scope: !1086)
!1089 = !DILocation(line: 0, column: 0, scope: !1088)
!1091 = !DILocation(line: 0, column: 0, scope: !1090)
!1093 = !DILocation(line: 0, column: 0, scope: !1092)
!1095 = !DILocation(line: 0, column: 0, scope: !1094)
!1097 = !DILocation(line: 0, column: 0, scope: !1096)
!1099 = !DILocation(line: 0, column: 0, scope: !1098)
!1101 = !DILocation(line: 0, column: 0, scope: !1100)
!1103 = !DILocation(line: 0, column: 0, scope: !1102)
!1105 = !DILocation(line: 0, column: 0, scope: !1104)
!1107 = !DILocation(line: 0, column: 0, scope: !1106)
!1109 = !DILocation(line: 0, column: 0, scope: !1108)
!1111 = !DILocation(line: 0, column: 0, scope: !1110)
!1113 = !DILocation(line: 0, column: 0, scope: !1112)
!1115 = !DILocation(line: 0, column: 0, scope: !1114)
!1117 = !DILocation(line: 0, column: 0, scope: !1116)
!1119 = !DILocation(line: 0, column: 0, scope: !1118)
!1121 = !DILocation(line: 0, column: 0, scope: !1120)
!1123 = !DILocation(line: 0, column: 0, scope: !1122)
!1125 = !DILocation(line: 0, column: 0, scope: !1124)
!1127 = !DILocation(line: 0, column: 0, scope: !1126)
!1129 = !DILocation(line: 0, column: 0, scope: !1128)
!1131 = !DILocation(line: 0, column: 0, scope: !1130)
!1133 = !DILocation(line: 0, column: 0, scope: !1132)
!1135 = !DILocation(line: 0, column: 0, scope: !1134)
!1137 = !DILocation(line: 0, column: 0, scope: !1136)
!1139 = !DILocation(line: 0, column: 0, scope: !1138)
!1141 = !DILocation(line: 0, column: 0, scope: !1140)
!1143 = !DILocation(line: 0, column: 0, scope: !1142)
!1145 = !DILocation(line: 0, column: 0, scope: !1144)
!1147 = !DILocation(line: 0, column: 0, scope: !1146)
!1149 = !DILocation(line: 0, column: 0, scope: !1148)
!1151 = !DILocation(line: 0, column: 0, scope: !1150)
!1153 = !DILocation(line: 0, column: 0, scope: !1152)
!1155 = !DILocation(line: 0, column: 0, scope: !1154)
!1157 = !DILocation(line: 0, column: 0, scope: !1156)
!1159 = !DILocation(line: 0, column: 0, scope: !1158)
!1161 = !DILocation(line: 0, column: 0, scope: !1160)
!1163 = !DILocation(line: 0, column: 0, scope: !1162)
!1165 = !DILocation(line: 0, column: 0, scope: !1164)
!1167 = !DILocation(line: 0, column: 0, scope: !1166)
!1169 = !DILocation(line: 0, column: 0, scope: !1168)
!1171 = !DILocation(line: 0, column: 0, scope: !1170)
!1173 = !DILocation(line: 0, column: 0, scope: !1172)
!1175 = !DILocation(line: 0, column: 0, scope: !1174)
!1176 = !DILocalVariable(name: "_result", scope: !16, file: !3, line: 54, type: !7)
!1177 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 55, type: !7)
!1178 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 56, type: !7)
!1179 = !DILocalVariable(name: "b", scope: !20, file: !3, line: 56, type: !7)
!1180 = !DILocalVariable(name: "a", scope: !20, file: !3, line: 56, type: !7)
!1181 = !DILocalVariable(name: "_result", scope: !22, file: !3, line: 57, type: !7)
!1182 = !DILocalVariable(name: "b", scope: !22, file: !3, line: 57, type: !7)
!1183 = !DILocalVariable(name: "a", scope: !22, file: !3, line: 57, type: !7)
!1184 = !DILocalVariable(name: "_result", scope: !24, file: !3, line: 58, type: !7)
!1185 = !DILocalVariable(name: "_result", scope: !26, file: !3, line: 59, type: !7)
!1186 = !DILocalVariable(name: "_result", scope: !28, file: !3, line: 60, type: !7)
!1187 = !DILocalVariable(name: "b", scope: !28, file: !3, line: 60, type: !7)
!1188 = !DILocalVariable(name: "a", scope: !28, file: !3, line: 60, type: !7)
!1189 = !DILocalVariable(name: "_result", scope: !30, file: !3, line: 61, type: !7)
!1190 = !DILocalVariable(name: "b", scope: !30, file: !3, line: 61, type: !7)
!1191 = !DILocalVariable(name: "a", scope: !30, file: !3, line: 61, type: !7)
!1192 = !DILocalVariable(name: "_result", scope: !32, file: !3, line: 62, type: !7)
!1193 = !DILocalVariable(name: "_result", scope: !34, file: !3, line: 63, type: !7)
!1194 = !DILocalVariable(name: "_result", scope: !36, file: !3, line: 64, type: !7)
!1195 = !DILocalVariable(name: "b", scope: !36, file: !3, line: 64, type: !7)
!1196 = !DILocalVariable(name: "a", scope: !36, file: !3, line: 64, type: !7)
!1197 = !DILocalVariable(name: "_result", scope: !38, file: !3, line: 65, type: !7)
!1198 = !DILocalVariable(name: "b", scope: !38, file: !3, line: 65, type: !7)
!1199 = !DILocalVariable(name: "a", scope: !38, file: !3, line: 65, type: !7)
!1200 = !DILocalVariable(name: "_result", scope: !40, file: !3, line: 66, type: !7)
!1201 = !DILocalVariable(name: "_result", scope: !42, file: !3, line: 67, type: !7)
!1202 = !DILocalVariable(name: "_result", scope: !44, file: !3, line: 68, type: !7)
!1203 = !DILocalVariable(name: "b", scope: !44, file: !3, line: 68, type: !7)
!1204 = !DILocalVariable(name: "a", scope: !44, file: !3, line: 68, type: !7)
!1205 = !DILocalVariable(name: "_result", scope: !46, file: !3, line: 69, type: !7)
!1206 = !DILocalVariable(name: "b", scope: !46, file: !3, line: 69, type: !7)
!1207 = !DILocalVariable(name: "a", scope: !46, file: !3, line: 69, type: !7)
!1208 = !DILocalVariable(name: "_result", scope: !48, file: !3, line: 70, type: !7)
!1209 = !DILocalVariable(name: "_result", scope: !50, file: !3, line: 71, type: !7)
!1210 = !DILocalVariable(name: "_result", scope: !52, file: !3, line: 72, type: !7)
!1211 = !DILocalVariable(name: "b", scope: !52, file: !3, line: 72, type: !7)
!1212 = !DILocalVariable(name: "a", scope: !52, file: !3, line: 72, type: !7)
!1213 = !DILocalVariable(name: "_result", scope: !54, file: !3, line: 73, type: !7)
!1214 = !DILocalVariable(name: "b", scope: !54, file: !3, line: 73, type: !7)
!1215 = !DILocalVariable(name: "a", scope: !54, file: !3, line: 73, type: !7)
!1216 = !DILocalVariable(name: "_result", scope: !56, file: !3, line: 74, type: !7)
!1217 = !DILocalVariable(name: "_result", scope: !58, file: !3, line: 75, type: !7)
!1218 = !DILocalVariable(name: "_result", scope: !60, file: !3, line: 76, type: !7)
!1219 = !DILocalVariable(name: "b", scope: !60, file: !3, line: 76, type: !7)
!1220 = !DILocalVariable(name: "a", scope: !60, file: !3, line: 76, type: !7)
!1221 = !DILocalVariable(name: "_result", scope: !62, file: !3, line: 77, type: !7)
!1222 = !DILocalVariable(name: "b", scope: !62, file: !3, line: 77, type: !7)
!1223 = !DILocalVariable(name: "a", scope: !62, file: !3, line: 77, type: !7)
!1224 = !DILocalVariable(name: "_result", scope: !64, file: !3, line: 78, type: !7)
!1225 = !DILocalVariable(name: "_result", scope: !66, file: !3, line: 79, type: !7)
!1226 = !DILocalVariable(name: "_result", scope: !68, file: !3, line: 80, type: !7)
!1227 = !DILocalVariable(name: "b", scope: !68, file: !3, line: 80, type: !7)
!1228 = !DILocalVariable(name: "a", scope: !68, file: !3, line: 80, type: !7)
!1229 = !DILocalVariable(name: "_result", scope: !70, file: !3, line: 81, type: !7)
!1230 = !DILocalVariable(name: "b", scope: !70, file: !3, line: 81, type: !7)
!1231 = !DILocalVariable(name: "a", scope: !70, file: !3, line: 81, type: !7)
!1232 = !DILocalVariable(name: "_result", scope: !72, file: !3, line: 82, type: !7)
!1233 = !DILocalVariable(name: "_result", scope: !74, file: !3, line: 83, type: !7)
!1234 = !DILocalVariable(name: "_result", scope: !76, file: !3, line: 84, type: !7)
!1235 = !DILocalVariable(name: "b", scope: !76, file: !3, line: 84, type: !7)
!1236 = !DILocalVariable(name: "a", scope: !76, file: !3, line: 84, type: !7)
!1237 = !DILocalVariable(name: "_result", scope: !78, file: !3, line: 85, type: !7)
!1238 = !DILocalVariable(name: "b", scope: !78, file: !3, line: 85, type: !7)
!1239 = !DILocalVariable(name: "a", scope: !78, file: !3, line: 85, type: !7)
!1240 = !DILocalVariable(name: "_result", scope: !80, file: !3, line: 86, type: !7)
!1241 = !DILocalVariable(name: "_result", scope: !82, file: !3, line: 87, type: !7)
!1242 = !DILocalVariable(name: "_result", scope: !84, file: !3, line: 88, type: !7)
!1243 = !DILocalVariable(name: "b", scope: !84, file: !3, line: 88, type: !7)
!1244 = !DILocalVariable(name: "a", scope: !84, file: !3, line: 88, type: !7)
!1245 = !DILocalVariable(name: "_result", scope: !86, file: !3, line: 89, type: !7)
!1246 = !DILocalVariable(name: "b", scope: !86, file: !3, line: 89, type: !7)
!1247 = !DILocalVariable(name: "a", scope: !86, file: !3, line: 89, type: !7)
!1248 = !DILocalVariable(name: "_result", scope: !88, file: !3, line: 90, type: !7)
!1249 = !DILocalVariable(name: "_result", scope: !90, file: !3, line: 91, type: !7)
!1250 = !DILocalVariable(name: "_result", scope: !92, file: !3, line: 92, type: !7)
!1251 = !DILocalVariable(name: "b", scope: !92, file: !3, line: 92, type: !7)
!1252 = !DILocalVariable(name: "a", scope: !92, file: !3, line: 92, type: !7)
!1253 = !DILocalVariable(name: "_result", scope: !94, file: !3, line: 93, type: !7)
!1254 = !DILocalVariable(name: "b", scope: !94, file: !3, line: 93, type: !7)
!1255 = !DILocalVariable(name: "a", scope: !94, file: !3, line: 93, type: !7)
!1256 = !DILocalVariable(name: "_result", scope: !96, file: !3, line: 94, type: !7)
!1257 = !DILocalVariable(name: "_result", scope: !98, file: !3, line: 95, type: !7)
!1258 = !DILocalVariable(name: "_result", scope: !100, file: !3, line: 96, type: !7)
!1259 = !DILocalVariable(name: "b", scope: !100, file: !3, line: 96, type: !7)
!1260 = !DILocalVariable(name: "a", scope: !100, file: !3, line: 96, type: !7)
!1261 = !DILocalVariable(name: "_result", scope: !102, file: !3, line: 97, type: !7)
!1262 = !DILocalVariable(name: "b", scope: !102, file: !3, line: 97, type: !7)
!1263 = !DILocalVariable(name: "a", scope: !102, file: !3, line: 97, type: !7)
!1264 = !DILocalVariable(name: "_result", scope: !104, file: !3, line: 98, type: !7)
!1265 = !DILocalVariable(name: "_result", scope: !106, file: !3, line: 99, type: !7)
!1266 = !DILocalVariable(name: "_result", scope: !108, file: !3, line: 100, type: !7)
!1267 = !DILocalVariable(name: "b", scope: !108, file: !3, line: 100, type: !7)
!1268 = !DILocalVariable(name: "a", scope: !108, file: !3, line: 100, type: !7)
!1269 = !DILocalVariable(name: "_result", scope: !110, file: !3, line: 101, type: !7)
!1270 = !DILocalVariable(name: "b", scope: !110, file: !3, line: 101, type: !7)
!1271 = !DILocalVariable(name: "a", scope: !110, file: !3, line: 101, type: !7)
!1272 = !DILocalVariable(name: "_result", scope: !112, file: !3, line: 102, type: !7)
!1273 = !DILocalVariable(name: "_result", scope: !114, file: !3, line: 103, type: !7)
!1274 = !DILocalVariable(name: "_result", scope: !116, file: !3, line: 104, type: !7)
!1275 = !DILocalVariable(name: "b", scope: !116, file: !3, line: 104, type: !7)
!1276 = !DILocalVariable(name: "a", scope: !116, file: !3, line: 104, type: !7)
!1277 = !DILocalVariable(name: "_result", scope: !118, file: !3, line: 105, type: !7)
!1278 = !DILocalVariable(name: "b", scope: !118, file: !3, line: 105, type: !7)
!1279 = !DILocalVariable(name: "a", scope: !118, file: !3, line: 105, type: !7)
!1280 = !DILocalVariable(name: "_result", scope: !120, file: !3, line: 106, type: !7)
!1281 = !DILocalVariable(name: "_result", scope: !122, file: !3, line: 107, type: !7)
!1282 = !DILocalVariable(name: "_result", scope: !124, file: !3, line: 108, type: !7)
!1283 = !DILocalVariable(name: "b", scope: !124, file: !3, line: 108, type: !7)
!1284 = !DILocalVariable(name: "a", scope: !124, file: !3, line: 108, type: !7)
!1285 = !DILocalVariable(name: "_result", scope: !126, file: !3, line: 109, type: !7)
!1286 = !DILocalVariable(name: "b", scope: !126, file: !3, line: 109, type: !7)
!1287 = !DILocalVariable(name: "a", scope: !126, file: !3, line: 109, type: !7)
!1288 = !DILocalVariable(name: "_result", scope: !128, file: !3, line: 110, type: !7)
!1289 = !DILocalVariable(name: "_result", scope: !130, file: !3, line: 111, type: !7)
!1290 = !DILocalVariable(name: "_result", scope: !132, file: !3, line: 112, type: !7)
!1291 = !DILocalVariable(name: "b", scope: !132, file: !3, line: 112, type: !7)
!1292 = !DILocalVariable(name: "a", scope: !132, file: !3, line: 112, type: !7)
!1293 = !DILocalVariable(name: "_result", scope: !134, file: !3, line: 113, type: !7)
!1294 = !DILocalVariable(name: "b", scope: !134, file: !3, line: 113, type: !7)
!1295 = !DILocalVariable(name: "a", scope: !134, file: !3, line: 113, type: !7)
!1296 = !DILocalVariable(name: "_result", scope: !136, file: !3, line: 114, type: !7)
!1297 = !DILocalVariable(name: "_result", scope: !138, file: !3, line: 115, type: !7)
!1298 = !DILocalVariable(name: "_result", scope: !140, file: !3, line: 116, type: !7)
!1299 = !DILocalVariable(name: "b", scope: !140, file: !3, line: 116, type: !7)
!1300 = !DILocalVariable(name: "a", scope: !140, file: !3, line: 116, type: !7)
!1301 = !DILocalVariable(name: "_result", scope: !142, file: !3, line: 117, type: !7)
!1302 = !DILocalVariable(name: "b", scope: !142, file: !3, line: 117, type: !7)
!1303 = !DILocalVariable(name: "a", scope: !142, file: !3, line: 117, type: !7)
!1304 = !DILocalVariable(name: "_result", scope: !144, file: !3, line: 118, type: !7)
!1305 = !DILocalVariable(name: "_result", scope: !146, file: !3, line: 119, type: !7)
!1306 = !DILocalVariable(name: "_result", scope: !148, file: !3, line: 120, type: !7)
!1307 = !DILocalVariable(name: "b", scope: !148, file: !3, line: 120, type: !7)
!1308 = !DILocalVariable(name: "a", scope: !148, file: !3, line: 120, type: !7)
!1309 = !DILocalVariable(name: "_result", scope: !150, file: !3, line: 121, type: !7)
!1310 = !DILocalVariable(name: "b", scope: !150, file: !3, line: 121, type: !7)
!1311 = !DILocalVariable(name: "a", scope: !150, file: !3, line: 121, type: !7)
!1312 = !DILocalVariable(name: "_result", scope: !152, file: !3, line: 122, type: !7)
!1313 = !DILocalVariable(name: "_result", scope: !154, file: !3, line: 123, type: !7)
!1314 = !DILocalVariable(name: "_result", scope: !156, file: !3, line: 124, type: !7)
!1315 = !DILocalVariable(name: "b", scope: !156, file: !3, line: 124, type: !7)
!1316 = !DILocalVariable(name: "a", scope: !156, file: !3, line: 124, type: !7)
!1317 = !DILocalVariable(name: "_result", scope: !158, file: !3, line: 125, type: !7)
!1318 = !DILocalVariable(name: "b", scope: !158, file: !3, line: 125, type: !7)
!1319 = !DILocalVariable(name: "a", scope: !158, file: !3, line: 125, type: !7)
!1320 = !DILocalVariable(name: "_result", scope: !160, file: !3, line: 126, type: !7)
!1321 = !DILocalVariable(name: "_result", scope: !162, file: !3, line: 127, type: !7)
!1322 = !DILocalVariable(name: "_result", scope: !164, file: !3, line: 128, type: !7)
!1323 = !DILocalVariable(name: "b", scope: !164, file: !3, line: 128, type: !7)
!1324 = !DILocalVariable(name: "a", scope: !164, file: !3, line: 128, type: !7)
!1325 = !DILocalVariable(name: "_result", scope: !166, file: !3, line: 129, type: !7)
!1326 = !DILocalVariable(name: "b", scope: !166, file: !3, line: 129, type: !7)
!1327 = !DILocalVariable(name: "a", scope: !166, file: !3, line: 129, type: !7)
!1328 = !DILocalVariable(name: "_result", scope: !168, file: !3, line: 130, type: !7)
!1329 = !DILocalVariable(name: "_result", scope: !170, file: !3, line: 131, type: !7)
!1330 = !DILocalVariable(name: "_result", scope: !172, file: !3, line: 132, type: !7)
!1331 = !DILocalVariable(name: "b", scope: !172, file: !3, line: 132, type: !7)
!1332 = !DILocalVariable(name: "a", scope: !172, file: !3, line: 132, type: !7)
!1333 = !DILocalVariable(name: "_result", scope: !174, file: !3, line: 133, type: !7)
!1334 = !DILocalVariable(name: "b", scope: !174, file: !3, line: 133, type: !7)
!1335 = !DILocalVariable(name: "a", scope: !174, file: !3, line: 133, type: !7)
!1336 = !DILocalVariable(name: "_result", scope: !176, file: !3, line: 134, type: !7)
!1337 = !DILocalVariable(name: "_result", scope: !178, file: !3, line: 135, type: !7)
!1338 = !DILocalVariable(name: "_result", scope: !180, file: !3, line: 136, type: !7)
!1339 = !DILocalVariable(name: "b", scope: !180, file: !3, line: 136, type: !7)
!1340 = !DILocalVariable(name: "a", scope: !180, file: !3, line: 136, type: !7)
!1341 = !DILocalVariable(name: "_result", scope: !182, file: !3, line: 137, type: !7)
!1342 = !DILocalVariable(name: "b", scope: !182, file: !3, line: 137, type: !7)
!1343 = !DILocalVariable(name: "a", scope: !182, file: !3, line: 137, type: !7)
!1344 = !DILocalVariable(name: "_result", scope: !184, file: !3, line: 138, type: !7)
!1345 = !DILocalVariable(name: "_result", scope: !186, file: !3, line: 139, type: !7)
!1346 = !DILocalVariable(name: "_result", scope: !188, file: !3, line: 140, type: !7)
!1347 = !DILocalVariable(name: "b", scope: !188, file: !3, line: 140, type: !7)
!1348 = !DILocalVariable(name: "a", scope: !188, file: !3, line: 140, type: !7)
!1349 = !DILocalVariable(name: "_result", scope: !190, file: !3, line: 141, type: !7)
!1350 = !DILocalVariable(name: "b", scope: !190, file: !3, line: 141, type: !7)
!1351 = !DILocalVariable(name: "a", scope: !190, file: !3, line: 141, type: !7)
!1352 = !DILocalVariable(name: "_result", scope: !192, file: !3, line: 142, type: !7)
!1353 = !DILocalVariable(name: "_result", scope: !194, file: !3, line: 143, type: !7)
!1354 = !DILocalVariable(name: "_result", scope: !196, file: !3, line: 144, type: !7)
!1355 = !DILocalVariable(name: "b", scope: !196, file: !3, line: 144, type: !7)
!1356 = !DILocalVariable(name: "a", scope: !196, file: !3, line: 144, type: !7)
!1357 = !DILocalVariable(name: "_result", scope: !198, file: !3, line: 145, type: !7)
!1358 = !DILocalVariable(name: "b", scope: !198, file: !3, line: 145, type: !7)
!1359 = !DILocalVariable(name: "a", scope: !198, file: !3, line: 145, type: !7)
!1360 = !DILocalVariable(name: "_result", scope: !200, file: !3, line: 146, type: !7)
!1361 = !DILocalVariable(name: "_result", scope: !202, file: !3, line: 147, type: !7)
!1362 = !DILocalVariable(name: "_result", scope: !204, file: !3, line: 148, type: !7)
!1363 = !DILocalVariable(name: "b", scope: !204, file: !3, line: 148, type: !7)
!1364 = !DILocalVariable(name: "a", scope: !204, file: !3, line: 148, type: !7)
!1365 = !DILocalVariable(name: "_result", scope: !206, file: !3, line: 149, type: !7)
!1366 = !DILocalVariable(name: "b", scope: !206, file: !3, line: 149, type: !7)
!1367 = !DILocalVariable(name: "a", scope: !206, file: !3, line: 149, type: !7)
!1368 = !DILocalVariable(name: "_result", scope: !208, file: !3, line: 150, type: !13)
!1369 = !DILocalVariable(name: "_result", scope: !210, file: !3, line: 151, type: !13)
!1370 = !DILocalVariable(name: "b", scope: !210, file: !3, line: 151, type: !13)
!1371 = !DILocalVariable(name: "a", scope: !210, file: !3, line: 151, type: !13)
!1372 = !DILocalVariable(name: "_result", scope: !212, file: !3, line: 152, type: !7)
!1373 = !DILocalVariable(name: "_result", scope: !214, file: !3, line: 153, type: !7)
!1374 = !DILocalVariable(name: "_result", scope: !216, file: !3, line: 154, type: !7)
!1375 = !DILocalVariable(name: "b", scope: !216, file: !3, line: 154, type: !7)
!1376 = !DILocalVariable(name: "a", scope: !216, file: !3, line: 154, type: !7)
!1377 = !DILocalVariable(name: "_result", scope: !218, file: !3, line: 155, type: !7)
!1378 = !DILocalVariable(name: "b", scope: !218, file: !3, line: 155, type: !7)
!1379 = !DILocalVariable(name: "a", scope: !218, file: !3, line: 155, type: !7)
!1380 = !DILocalVariable(name: "_result", scope: !220, file: !3, line: 156, type: !7)
!1381 = !DILocalVariable(name: "_result", scope: !222, file: !3, line: 157, type: !7)
!1382 = !DILocalVariable(name: "_result", scope: !224, file: !3, line: 158, type: !7)
!1383 = !DILocalVariable(name: "b", scope: !224, file: !3, line: 158, type: !7)
!1384 = !DILocalVariable(name: "a", scope: !224, file: !3, line: 158, type: !7)
!1385 = !DILocalVariable(name: "_result", scope: !226, file: !3, line: 159, type: !7)
!1386 = !DILocalVariable(name: "b", scope: !226, file: !3, line: 159, type: !7)
!1387 = !DILocalVariable(name: "a", scope: !226, file: !3, line: 159, type: !7)
!1388 = !DILocalVariable(name: "_result", scope: !228, file: !3, line: 160, type: !7)
!1389 = !DILocalVariable(name: "_result", scope: !230, file: !3, line: 161, type: !7)
!1390 = !DILocalVariable(name: "_result", scope: !232, file: !3, line: 162, type: !7)
!1391 = !DILocalVariable(name: "b", scope: !232, file: !3, line: 162, type: !7)
!1392 = !DILocalVariable(name: "a", scope: !232, file: !3, line: 162, type: !7)
!1393 = !DILocalVariable(name: "_result", scope: !234, file: !3, line: 163, type: !7)
!1394 = !DILocalVariable(name: "b", scope: !234, file: !3, line: 163, type: !7)
!1395 = !DILocalVariable(name: "a", scope: !234, file: !3, line: 163, type: !7)
!1396 = !DILocalVariable(name: "_result", scope: !236, file: !3, line: 164, type: !7)
!1397 = !DILocalVariable(name: "_result", scope: !238, file: !3, line: 165, type: !7)
!1398 = !DILocalVariable(name: "_result", scope: !240, file: !3, line: 166, type: !7)
!1399 = !DILocalVariable(name: "b", scope: !240, file: !3, line: 166, type: !7)
!1400 = !DILocalVariable(name: "a", scope: !240, file: !3, line: 166, type: !7)
!1401 = !DILocalVariable(name: "_result", scope: !242, file: !3, line: 167, type: !7)
!1402 = !DILocalVariable(name: "b", scope: !242, file: !3, line: 167, type: !7)
!1403 = !DILocalVariable(name: "a", scope: !242, file: !3, line: 167, type: !7)
!1404 = !DILocalVariable(name: "_result", scope: !244, file: !3, line: 168, type: !7)
!1405 = !DILocalVariable(name: "_result", scope: !246, file: !3, line: 169, type: !7)
!1406 = !DILocalVariable(name: "_result", scope: !248, file: !3, line: 170, type: !7)
!1407 = !DILocalVariable(name: "b", scope: !248, file: !3, line: 170, type: !7)
!1408 = !DILocalVariable(name: "a", scope: !248, file: !3, line: 170, type: !7)
!1409 = !DILocalVariable(name: "_result", scope: !250, file: !3, line: 171, type: !7)
!1410 = !DILocalVariable(name: "b", scope: !250, file: !3, line: 171, type: !7)
!1411 = !DILocalVariable(name: "a", scope: !250, file: !3, line: 171, type: !7)
!1412 = !DILocalVariable(name: "_result", scope: !252, file: !3, line: 172, type: !7)
!1413 = !DILocalVariable(name: "_result", scope: !254, file: !3, line: 173, type: !7)
!1414 = !DILocalVariable(name: "_result", scope: !256, file: !3, line: 174, type: !7)
!1415 = !DILocalVariable(name: "b", scope: !256, file: !3, line: 174, type: !7)
!1416 = !DILocalVariable(name: "a", scope: !256, file: !3, line: 174, type: !7)
!1417 = !DILocalVariable(name: "_result", scope: !258, file: !3, line: 175, type: !7)
!1418 = !DILocalVariable(name: "b", scope: !258, file: !3, line: 175, type: !7)
!1419 = !DILocalVariable(name: "a", scope: !258, file: !3, line: 175, type: !7)
!1420 = !DILocalVariable(name: "_result", scope: !260, file: !3, line: 176, type: !7)
!1421 = !DILocalVariable(name: "_result", scope: !262, file: !3, line: 177, type: !7)
!1422 = !DILocalVariable(name: "_result", scope: !264, file: !3, line: 178, type: !7)
!1423 = !DILocalVariable(name: "b", scope: !264, file: !3, line: 178, type: !7)
!1424 = !DILocalVariable(name: "a", scope: !264, file: !3, line: 178, type: !7)
!1425 = !DILocalVariable(name: "_result", scope: !266, file: !3, line: 179, type: !7)
!1426 = !DILocalVariable(name: "b", scope: !266, file: !3, line: 179, type: !7)
!1427 = !DILocalVariable(name: "a", scope: !266, file: !3, line: 179, type: !7)
!1428 = !DILocalVariable(name: "_result", scope: !268, file: !3, line: 180, type: !7)
!1429 = !DILocalVariable(name: "_result", scope: !270, file: !3, line: 181, type: !7)
!1430 = !DILocalVariable(name: "_result", scope: !272, file: !3, line: 182, type: !7)
!1431 = !DILocalVariable(name: "b", scope: !272, file: !3, line: 182, type: !7)
!1432 = !DILocalVariable(name: "a", scope: !272, file: !3, line: 182, type: !7)
!1433 = !DILocalVariable(name: "_result", scope: !274, file: !3, line: 183, type: !7)
!1434 = !DILocalVariable(name: "b", scope: !274, file: !3, line: 183, type: !7)
!1435 = !DILocalVariable(name: "a", scope: !274, file: !3, line: 183, type: !7)
!1436 = !DILocalVariable(name: "_result", scope: !276, file: !3, line: 184, type: !7)
!1437 = !DILocalVariable(name: "_result", scope: !278, file: !3, line: 185, type: !7)
!1438 = !DILocalVariable(name: "_result", scope: !280, file: !3, line: 186, type: !7)
!1439 = !DILocalVariable(name: "b", scope: !280, file: !3, line: 186, type: !7)
!1440 = !DILocalVariable(name: "a", scope: !280, file: !3, line: 186, type: !7)
!1441 = !DILocalVariable(name: "_result", scope: !282, file: !3, line: 187, type: !7)
!1442 = !DILocalVariable(name: "b", scope: !282, file: !3, line: 187, type: !7)
!1443 = !DILocalVariable(name: "a", scope: !282, file: !3, line: 187, type: !7)
!1444 = !DILocalVariable(name: "_result", scope: !284, file: !3, line: 188, type: !7)
!1445 = !DILocalVariable(name: "_result", scope: !286, file: !3, line: 189, type: !7)
!1446 = !DILocalVariable(name: "_result", scope: !288, file: !3, line: 190, type: !7)
!1447 = !DILocalVariable(name: "b", scope: !288, file: !3, line: 190, type: !7)
!1448 = !DILocalVariable(name: "a", scope: !288, file: !3, line: 190, type: !7)
!1449 = !DILocalVariable(name: "_result", scope: !290, file: !3, line: 191, type: !7)
!1450 = !DILocalVariable(name: "b", scope: !290, file: !3, line: 191, type: !7)
!1451 = !DILocalVariable(name: "a", scope: !290, file: !3, line: 191, type: !7)
!1452 = !DILocalVariable(name: "_result", scope: !292, file: !3, line: 192, type: !7)
!1453 = !DILocalVariable(name: "_result", scope: !294, file: !3, line: 193, type: !7)
!1454 = !DILocalVariable(name: "_result", scope: !296, file: !3, line: 194, type: !7)
!1455 = !DILocalVariable(name: "b", scope: !296, file: !3, line: 194, type: !7)
!1456 = !DILocalVariable(name: "a", scope: !296, file: !3, line: 194, type: !7)
!1457 = !DILocalVariable(name: "_result", scope: !298, file: !3, line: 195, type: !7)
!1458 = !DILocalVariable(name: "b", scope: !298, file: !3, line: 195, type: !7)
!1459 = !DILocalVariable(name: "a", scope: !298, file: !3, line: 195, type: !7)
!1460 = !DILocalVariable(name: "_result", scope: !300, file: !3, line: 196, type: !7)
!1461 = !DILocalVariable(name: "_result", scope: !302, file: !3, line: 197, type: !7)
!1462 = !DILocalVariable(name: "_result", scope: !304, file: !3, line: 198, type: !7)
!1463 = !DILocalVariable(name: "b", scope: !304, file: !3, line: 198, type: !7)
!1464 = !DILocalVariable(name: "a", scope: !304, file: !3, line: 198, type: !7)
!1465 = !DILocalVariable(name: "_result", scope: !306, file: !3, line: 199, type: !7)
!1466 = !DILocalVariable(name: "b", scope: !306, file: !3, line: 199, type: !7)
!1467 = !DILocalVariable(name: "a", scope: !306, file: !3, line: 199, type: !7)
!1468 = !DILocalVariable(name: "_result", scope: !308, file: !3, line: 200, type: !7)
!1469 = !DILocalVariable(name: "_result", scope: !310, file: !3, line: 201, type: !7)
!1470 = !DILocalVariable(name: "_result", scope: !312, file: !3, line: 202, type: !7)
!1471 = !DILocalVariable(name: "b", scope: !312, file: !3, line: 202, type: !7)
!1472 = !DILocalVariable(name: "a", scope: !312, file: !3, line: 202, type: !7)
!1473 = !DILocalVariable(name: "_result", scope: !314, file: !3, line: 203, type: !7)
!1474 = !DILocalVariable(name: "b", scope: !314, file: !3, line: 203, type: !7)
!1475 = !DILocalVariable(name: "a", scope: !314, file: !3, line: 203, type: !7)
!1476 = !DILocalVariable(name: "_result", scope: !316, file: !3, line: 204, type: !7)
!1477 = !DILocalVariable(name: "_result", scope: !318, file: !3, line: 205, type: !7)
!1478 = !DILocalVariable(name: "_result", scope: !320, file: !3, line: 206, type: !7)
!1479 = !DILocalVariable(name: "b", scope: !320, file: !3, line: 206, type: !7)
!1480 = !DILocalVariable(name: "a", scope: !320, file: !3, line: 206, type: !7)
!1481 = !DILocalVariable(name: "_result", scope: !322, file: !3, line: 207, type: !7)
!1482 = !DILocalVariable(name: "b", scope: !322, file: !3, line: 207, type: !7)
!1483 = !DILocalVariable(name: "a", scope: !322, file: !3, line: 207, type: !7)
!1484 = !DILocalVariable(name: "_result", scope: !324, file: !3, line: 208, type: !7)
!1485 = !DILocalVariable(name: "_result", scope: !326, file: !3, line: 209, type: !7)
!1486 = !DILocalVariable(name: "_result", scope: !328, file: !3, line: 210, type: !7)
!1487 = !DILocalVariable(name: "b", scope: !328, file: !3, line: 210, type: !7)
!1488 = !DILocalVariable(name: "a", scope: !328, file: !3, line: 210, type: !7)
!1489 = !DILocalVariable(name: "_result", scope: !330, file: !3, line: 211, type: !7)
!1490 = !DILocalVariable(name: "b", scope: !330, file: !3, line: 211, type: !7)
!1491 = !DILocalVariable(name: "a", scope: !330, file: !3, line: 211, type: !7)
!1492 = !DILocalVariable(name: "_result", scope: !332, file: !3, line: 212, type: !7)
!1493 = !DILocalVariable(name: "_result", scope: !334, file: !3, line: 213, type: !7)
!1494 = !DILocalVariable(name: "_result", scope: !336, file: !3, line: 214, type: !7)
!1495 = !DILocalVariable(name: "b", scope: !336, file: !3, line: 214, type: !7)
!1496 = !DILocalVariable(name: "a", scope: !336, file: !3, line: 214, type: !7)
!1497 = !DILocalVariable(name: "_result", scope: !338, file: !3, line: 215, type: !7)
!1498 = !DILocalVariable(name: "b", scope: !338, file: !3, line: 215, type: !7)
!1499 = !DILocalVariable(name: "a", scope: !338, file: !3, line: 215, type: !7)
!1500 = !DILocalVariable(name: "_result", scope: !340, file: !3, line: 216, type: !7)
!1501 = !DILocalVariable(name: "_result", scope: !342, file: !3, line: 217, type: !7)
!1502 = !DILocalVariable(name: "_result", scope: !344, file: !3, line: 218, type: !7)
!1503 = !DILocalVariable(name: "b", scope: !344, file: !3, line: 218, type: !7)
!1504 = !DILocalVariable(name: "a", scope: !344, file: !3, line: 218, type: !7)
!1505 = !DILocalVariable(name: "_result", scope: !346, file: !3, line: 219, type: !7)
!1506 = !DILocalVariable(name: "b", scope: !346, file: !3, line: 219, type: !7)
!1507 = !DILocalVariable(name: "a", scope: !346, file: !3, line: 219, type: !7)
!1508 = !DILocalVariable(name: "_result", scope: !348, file: !3, line: 220, type: !7)
!1509 = !DILocalVariable(name: "_result", scope: !350, file: !3, line: 221, type: !7)
!1510 = !DILocalVariable(name: "_result", scope: !352, file: !3, line: 222, type: !7)
!1511 = !DILocalVariable(name: "b", scope: !352, file: !3, line: 222, type: !7)
!1512 = !DILocalVariable(name: "a", scope: !352, file: !3, line: 222, type: !7)
!1513 = !DILocalVariable(name: "_result", scope: !354, file: !3, line: 223, type: !7)
!1514 = !DILocalVariable(name: "b", scope: !354, file: !3, line: 223, type: !7)
!1515 = !DILocalVariable(name: "a", scope: !354, file: !3, line: 223, type: !7)
!1516 = !DILocalVariable(name: "_result", scope: !356, file: !3, line: 224, type: !7)
!1517 = !DILocalVariable(name: "_result", scope: !358, file: !3, line: 225, type: !7)
!1518 = !DILocalVariable(name: "_result", scope: !360, file: !3, line: 226, type: !7)
!1519 = !DILocalVariable(name: "b", scope: !360, file: !3, line: 226, type: !7)
!1520 = !DILocalVariable(name: "a", scope: !360, file: !3, line: 226, type: !7)
!1521 = !DILocalVariable(name: "_result", scope: !362, file: !3, line: 227, type: !7)
!1522 = !DILocalVariable(name: "b", scope: !362, file: !3, line: 227, type: !7)
!1523 = !DILocalVariable(name: "a", scope: !362, file: !3, line: 227, type: !7)
!1524 = !DILocalVariable(name: "_result", scope: !364, file: !3, line: 228, type: !7)
!1525 = !DILocalVariable(name: "_result", scope: !366, file: !3, line: 229, type: !7)
!1526 = !DILocalVariable(name: "_result", scope: !368, file: !3, line: 230, type: !7)
!1527 = !DILocalVariable(name: "b", scope: !368, file: !3, line: 230, type: !7)
!1528 = !DILocalVariable(name: "a", scope: !368, file: !3, line: 230, type: !7)
!1529 = !DILocalVariable(name: "_result", scope: !370, file: !3, line: 231, type: !7)
!1530 = !DILocalVariable(name: "b", scope: !370, file: !3, line: 231, type: !7)
!1531 = !DILocalVariable(name: "a", scope: !370, file: !3, line: 231, type: !7)
!1532 = !DILocalVariable(name: "_result", scope: !372, file: !3, line: 232, type: !7)
!1533 = !DILocalVariable(name: "_result", scope: !374, file: !3, line: 233, type: !7)
!1534 = !DILocalVariable(name: "_result", scope: !376, file: !3, line: 234, type: !7)
!1535 = !DILocalVariable(name: "b", scope: !376, file: !3, line: 234, type: !7)
!1536 = !DILocalVariable(name: "a", scope: !376, file: !3, line: 234, type: !7)
!1537 = !DILocalVariable(name: "_result", scope: !378, file: !3, line: 235, type: !7)
!1538 = !DILocalVariable(name: "b", scope: !378, file: !3, line: 235, type: !7)
!1539 = !DILocalVariable(name: "a", scope: !378, file: !3, line: 235, type: !7)
!1540 = !DILocalVariable(name: "_result", scope: !380, file: !3, line: 236, type: !7)
!1541 = !DILocalVariable(name: "_result", scope: !382, file: !3, line: 237, type: !7)
!1542 = !DILocalVariable(name: "_result", scope: !384, file: !3, line: 238, type: !7)
!1543 = !DILocalVariable(name: "b", scope: !384, file: !3, line: 238, type: !7)
!1544 = !DILocalVariable(name: "a", scope: !384, file: !3, line: 238, type: !7)
!1545 = !DILocalVariable(name: "_result", scope: !386, file: !3, line: 239, type: !7)
!1546 = !DILocalVariable(name: "b", scope: !386, file: !3, line: 239, type: !7)
!1547 = !DILocalVariable(name: "a", scope: !386, file: !3, line: 239, type: !7)
!1548 = !DILocalVariable(name: "_result", scope: !388, file: !3, line: 240, type: !7)
!1549 = !DILocalVariable(name: "_result", scope: !390, file: !3, line: 241, type: !7)
!1550 = !DILocalVariable(name: "_result", scope: !392, file: !3, line: 242, type: !7)
!1551 = !DILocalVariable(name: "b", scope: !392, file: !3, line: 242, type: !7)
!1552 = !DILocalVariable(name: "a", scope: !392, file: !3, line: 242, type: !7)
!1553 = !DILocalVariable(name: "_result", scope: !394, file: !3, line: 243, type: !7)
!1554 = !DILocalVariable(name: "b", scope: !394, file: !3, line: 243, type: !7)
!1555 = !DILocalVariable(name: "a", scope: !394, file: !3, line: 243, type: !7)
!1556 = !DILocalVariable(name: "_result", scope: !396, file: !3, line: 244, type: !7)
!1557 = !DILocalVariable(name: "_result", scope: !398, file: !3, line: 245, type: !7)
!1558 = !DILocalVariable(name: "_result", scope: !400, file: !3, line: 246, type: !7)
!1559 = !DILocalVariable(name: "b", scope: !400, file: !3, line: 246, type: !7)
!1560 = !DILocalVariable(name: "a", scope: !400, file: !3, line: 246, type: !7)
!1561 = !DILocalVariable(name: "_result", scope: !402, file: !3, line: 247, type: !7)
!1562 = !DILocalVariable(name: "b", scope: !402, file: !3, line: 247, type: !7)
!1563 = !DILocalVariable(name: "a", scope: !402, file: !3, line: 247, type: !7)
!1564 = !DILocalVariable(name: "_result", scope: !404, file: !3, line: 248, type: !7)
!1565 = !DILocalVariable(name: "_result", scope: !406, file: !3, line: 249, type: !7)
!1566 = !DILocalVariable(name: "_result", scope: !408, file: !3, line: 250, type: !7)
!1567 = !DILocalVariable(name: "b", scope: !408, file: !3, line: 250, type: !7)
!1568 = !DILocalVariable(name: "a", scope: !408, file: !3, line: 250, type: !7)
!1569 = !DILocalVariable(name: "_result", scope: !410, file: !3, line: 251, type: !7)
!1570 = !DILocalVariable(name: "b", scope: !410, file: !3, line: 251, type: !7)
!1571 = !DILocalVariable(name: "a", scope: !410, file: !3, line: 251, type: !7)
!1572 = !DILocalVariable(name: "_result", scope: !412, file: !3, line: 252, type: !7)
!1573 = !DILocalVariable(name: "_result", scope: !414, file: !3, line: 253, type: !7)
!1574 = !DILocalVariable(name: "_result", scope: !416, file: !3, line: 254, type: !7)
!1575 = !DILocalVariable(name: "b", scope: !416, file: !3, line: 254, type: !7)
!1576 = !DILocalVariable(name: "a", scope: !416, file: !3, line: 254, type: !7)
!1577 = !DILocalVariable(name: "_result", scope: !418, file: !3, line: 255, type: !7)
!1578 = !DILocalVariable(name: "b", scope: !418, file: !3, line: 255, type: !7)
!1579 = !DILocalVariable(name: "a", scope: !418, file: !3, line: 255, type: !7)
!1580 = !DILocalVariable(name: "_result", scope: !420, file: !3, line: 256, type: !7)
!1581 = !DILocalVariable(name: "_result", scope: !422, file: !3, line: 257, type: !7)
!1582 = !DILocalVariable(name: "_result", scope: !424, file: !3, line: 258, type: !7)
!1583 = !DILocalVariable(name: "b", scope: !424, file: !3, line: 258, type: !7)
!1584 = !DILocalVariable(name: "a", scope: !424, file: !3, line: 258, type: !7)
!1585 = !DILocalVariable(name: "_result", scope: !426, file: !3, line: 259, type: !7)
!1586 = !DILocalVariable(name: "b", scope: !426, file: !3, line: 259, type: !7)
!1587 = !DILocalVariable(name: "a", scope: !426, file: !3, line: 259, type: !7)
!1588 = !DILocalVariable(name: "_result", scope: !428, file: !3, line: 260, type: !7)
!1589 = !DILocalVariable(name: "_result", scope: !430, file: !3, line: 261, type: !7)
!1590 = !DILocalVariable(name: "_result", scope: !432, file: !3, line: 262, type: !7)
!1591 = !DILocalVariable(name: "b", scope: !432, file: !3, line: 262, type: !7)
!1592 = !DILocalVariable(name: "a", scope: !432, file: !3, line: 262, type: !7)
!1593 = !DILocalVariable(name: "_result", scope: !434, file: !3, line: 263, type: !7)
!1594 = !DILocalVariable(name: "b", scope: !434, file: !3, line: 263, type: !7)
!1595 = !DILocalVariable(name: "a", scope: !434, file: !3, line: 263, type: !7)
!1596 = !DILocalVariable(name: "_result", scope: !436, file: !3, line: 264, type: !7)
!1597 = !DILocalVariable(name: "_result", scope: !438, file: !3, line: 265, type: !7)
!1598 = !DILocalVariable(name: "_result", scope: !440, file: !3, line: 266, type: !7)
!1599 = !DILocalVariable(name: "b", scope: !440, file: !3, line: 266, type: !7)
!1600 = !DILocalVariable(name: "a", scope: !440, file: !3, line: 266, type: !7)
!1601 = !DILocalVariable(name: "_result", scope: !442, file: !3, line: 267, type: !7)
!1602 = !DILocalVariable(name: "b", scope: !442, file: !3, line: 267, type: !7)
!1603 = !DILocalVariable(name: "a", scope: !442, file: !3, line: 267, type: !7)
!1604 = !DILocalVariable(name: "_result", scope: !444, file: !3, line: 268, type: !7)
!1605 = !DILocalVariable(name: "_result", scope: !446, file: !3, line: 269, type: !7)
!1606 = !DILocalVariable(name: "_result", scope: !448, file: !3, line: 270, type: !7)
!1607 = !DILocalVariable(name: "b", scope: !448, file: !3, line: 270, type: !7)
!1608 = !DILocalVariable(name: "a", scope: !448, file: !3, line: 270, type: !7)
!1609 = !DILocalVariable(name: "_result", scope: !450, file: !3, line: 271, type: !7)
!1610 = !DILocalVariable(name: "b", scope: !450, file: !3, line: 271, type: !7)
!1611 = !DILocalVariable(name: "a", scope: !450, file: !3, line: 271, type: !7)
!1612 = !DILocalVariable(name: "_result", scope: !452, file: !3, line: 272, type: !7)
!1613 = !DILocalVariable(name: "_result", scope: !454, file: !3, line: 273, type: !7)
!1614 = !DILocalVariable(name: "_result", scope: !456, file: !3, line: 274, type: !7)
!1615 = !DILocalVariable(name: "b", scope: !456, file: !3, line: 274, type: !7)
!1616 = !DILocalVariable(name: "a", scope: !456, file: !3, line: 274, type: !7)
!1617 = !DILocalVariable(name: "_result", scope: !458, file: !3, line: 275, type: !7)
!1618 = !DILocalVariable(name: "b", scope: !458, file: !3, line: 275, type: !7)
!1619 = !DILocalVariable(name: "a", scope: !458, file: !3, line: 275, type: !7)
!1620 = !DILocalVariable(name: "_result", scope: !460, file: !3, line: 276, type: !7)
!1621 = !DILocalVariable(name: "_result", scope: !462, file: !3, line: 277, type: !7)
!1622 = !DILocalVariable(name: "_result", scope: !464, file: !3, line: 278, type: !7)
!1623 = !DILocalVariable(name: "b", scope: !464, file: !3, line: 278, type: !7)
!1624 = !DILocalVariable(name: "a", scope: !464, file: !3, line: 278, type: !7)
!1625 = !DILocalVariable(name: "_result", scope: !466, file: !3, line: 279, type: !7)
!1626 = !DILocalVariable(name: "b", scope: !466, file: !3, line: 279, type: !7)
!1627 = !DILocalVariable(name: "a", scope: !466, file: !3, line: 279, type: !7)
!1628 = !DILocalVariable(name: "_result", scope: !468, file: !3, line: 280, type: !7)
!1629 = !DILocalVariable(name: "_result", scope: !470, file: !3, line: 281, type: !7)
!1630 = !DILocalVariable(name: "_result", scope: !472, file: !3, line: 282, type: !7)
!1631 = !DILocalVariable(name: "b", scope: !472, file: !3, line: 282, type: !7)
!1632 = !DILocalVariable(name: "a", scope: !472, file: !3, line: 282, type: !7)
!1633 = !DILocalVariable(name: "_result", scope: !474, file: !3, line: 283, type: !7)
!1634 = !DILocalVariable(name: "b", scope: !474, file: !3, line: 283, type: !7)
!1635 = !DILocalVariable(name: "a", scope: !474, file: !3, line: 283, type: !7)
!1636 = !DILocalVariable(name: "_result", scope: !476, file: !3, line: 284, type: !7)
!1637 = !DILocalVariable(name: "_result", scope: !478, file: !3, line: 285, type: !7)
!1638 = !DILocalVariable(name: "_result", scope: !480, file: !3, line: 286, type: !7)
!1639 = !DILocalVariable(name: "b", scope: !480, file: !3, line: 286, type: !7)
!1640 = !DILocalVariable(name: "a", scope: !480, file: !3, line: 286, type: !7)
!1641 = !DILocalVariable(name: "_result", scope: !482, file: !3, line: 287, type: !7)
!1642 = !DILocalVariable(name: "b", scope: !482, file: !3, line: 287, type: !7)
!1643 = !DILocalVariable(name: "a", scope: !482, file: !3, line: 287, type: !7)
!1644 = !DILocalVariable(name: "_result", scope: !484, file: !3, line: 288, type: !7)
!1645 = !DILocalVariable(name: "_result", scope: !486, file: !3, line: 289, type: !7)
!1646 = !DILocalVariable(name: "_result", scope: !488, file: !3, line: 290, type: !7)
!1647 = !DILocalVariable(name: "b", scope: !488, file: !3, line: 290, type: !7)
!1648 = !DILocalVariable(name: "a", scope: !488, file: !3, line: 290, type: !7)
!1649 = !DILocalVariable(name: "_result", scope: !490, file: !3, line: 291, type: !7)
!1650 = !DILocalVariable(name: "b", scope: !490, file: !3, line: 291, type: !7)
!1651 = !DILocalVariable(name: "a", scope: !490, file: !3, line: 291, type: !7)
!1652 = !DILocalVariable(name: "_result", scope: !492, file: !3, line: 292, type: !7)
!1653 = !DILocalVariable(name: "_result", scope: !494, file: !3, line: 293, type: !7)
!1654 = !DILocalVariable(name: "_result", scope: !496, file: !3, line: 294, type: !7)
!1655 = !DILocalVariable(name: "b", scope: !496, file: !3, line: 294, type: !7)
!1656 = !DILocalVariable(name: "a", scope: !496, file: !3, line: 294, type: !7)
!1657 = !DILocalVariable(name: "_result", scope: !498, file: !3, line: 295, type: !7)
!1658 = !DILocalVariable(name: "b", scope: !498, file: !3, line: 295, type: !7)
!1659 = !DILocalVariable(name: "a", scope: !498, file: !3, line: 295, type: !7)
!1660 = !DILocalVariable(name: "_result", scope: !500, file: !3, line: 296, type: !7)
!1661 = !DILocalVariable(name: "_result", scope: !502, file: !3, line: 297, type: !7)
!1662 = !DILocalVariable(name: "_result", scope: !504, file: !3, line: 298, type: !7)
!1663 = !DILocalVariable(name: "b", scope: !504, file: !3, line: 298, type: !7)
!1664 = !DILocalVariable(name: "a", scope: !504, file: !3, line: 298, type: !7)
!1665 = !DILocalVariable(name: "_result", scope: !506, file: !3, line: 299, type: !7)
!1666 = !DILocalVariable(name: "b", scope: !506, file: !3, line: 299, type: !7)
!1667 = !DILocalVariable(name: "a", scope: !506, file: !3, line: 299, type: !7)
!1668 = !DILocalVariable(name: "_result", scope: !508, file: !3, line: 300, type: !7)
!1669 = !DILocalVariable(name: "_result", scope: !510, file: !3, line: 301, type: !7)
!1670 = !DILocalVariable(name: "_result", scope: !512, file: !3, line: 302, type: !7)
!1671 = !DILocalVariable(name: "b", scope: !512, file: !3, line: 302, type: !7)
!1672 = !DILocalVariable(name: "a", scope: !512, file: !3, line: 302, type: !7)
!1673 = !DILocalVariable(name: "_result", scope: !514, file: !3, line: 303, type: !7)
!1674 = !DILocalVariable(name: "b", scope: !514, file: !3, line: 303, type: !7)
!1675 = !DILocalVariable(name: "a", scope: !514, file: !3, line: 303, type: !7)
!1676 = !DILocalVariable(name: "_result", scope: !516, file: !3, line: 304, type: !7)
!1677 = !DILocalVariable(name: "_result", scope: !518, file: !3, line: 305, type: !7)
!1678 = !DILocalVariable(name: "_result", scope: !520, file: !3, line: 306, type: !7)
!1679 = !DILocalVariable(name: "b", scope: !520, file: !3, line: 306, type: !7)
!1680 = !DILocalVariable(name: "a", scope: !520, file: !3, line: 306, type: !7)
!1681 = !DILocalVariable(name: "_result", scope: !522, file: !3, line: 307, type: !7)
!1682 = !DILocalVariable(name: "b", scope: !522, file: !3, line: 307, type: !7)
!1683 = !DILocalVariable(name: "a", scope: !522, file: !3, line: 307, type: !7)
!1684 = !DILocalVariable(name: "_result", scope: !524, file: !3, line: 308, type: !7)
!1685 = !DILocalVariable(name: "_result", scope: !526, file: !3, line: 309, type: !7)
!1686 = !DILocalVariable(name: "_result", scope: !528, file: !3, line: 310, type: !7)
!1687 = !DILocalVariable(name: "b", scope: !528, file: !3, line: 310, type: !7)
!1688 = !DILocalVariable(name: "a", scope: !528, file: !3, line: 310, type: !7)
!1689 = !DILocalVariable(name: "_result", scope: !530, file: !3, line: 311, type: !7)
!1690 = !DILocalVariable(name: "b", scope: !530, file: !3, line: 311, type: !7)
!1691 = !DILocalVariable(name: "a", scope: !530, file: !3, line: 311, type: !7)
!1692 = !DILocalVariable(name: "_result", scope: !532, file: !3, line: 312, type: !7)
!1693 = !DILocalVariable(name: "_result", scope: !534, file: !3, line: 313, type: !7)
!1694 = !DILocalVariable(name: "_result", scope: !536, file: !3, line: 314, type: !7)
!1695 = !DILocalVariable(name: "b", scope: !536, file: !3, line: 314, type: !7)
!1696 = !DILocalVariable(name: "a", scope: !536, file: !3, line: 314, type: !7)
!1697 = !DILocalVariable(name: "_result", scope: !538, file: !3, line: 315, type: !7)
!1698 = !DILocalVariable(name: "b", scope: !538, file: !3, line: 315, type: !7)
!1699 = !DILocalVariable(name: "a", scope: !538, file: !3, line: 315, type: !7)
!1700 = !DILocalVariable(name: "_result", scope: !540, file: !3, line: 316, type: !7)
!1701 = !DILocalVariable(name: "_result", scope: !542, file: !3, line: 317, type: !7)
!1702 = !DILocalVariable(name: "_result", scope: !544, file: !3, line: 318, type: !7)
!1703 = !DILocalVariable(name: "b", scope: !544, file: !3, line: 318, type: !7)
!1704 = !DILocalVariable(name: "a", scope: !544, file: !3, line: 318, type: !7)
!1705 = !DILocalVariable(name: "_result", scope: !546, file: !3, line: 319, type: !7)
!1706 = !DILocalVariable(name: "b", scope: !546, file: !3, line: 319, type: !7)
!1707 = !DILocalVariable(name: "a", scope: !546, file: !3, line: 319, type: !7)
!1708 = !DILocalVariable(name: "_result", scope: !548, file: !3, line: 320, type: !7)
!1709 = !DILocalVariable(name: "_result", scope: !550, file: !3, line: 321, type: !7)
!1710 = !DILocalVariable(name: "_result", scope: !552, file: !3, line: 322, type: !7)
!1711 = !DILocalVariable(name: "b", scope: !552, file: !3, line: 322, type: !7)
!1712 = !DILocalVariable(name: "a", scope: !552, file: !3, line: 322, type: !7)
!1713 = !DILocalVariable(name: "_result", scope: !554, file: !3, line: 323, type: !7)
!1714 = !DILocalVariable(name: "b", scope: !554, file: !3, line: 323, type: !7)
!1715 = !DILocalVariable(name: "a", scope: !554, file: !3, line: 323, type: !7)
!1716 = !DILocalVariable(name: "_result", scope: !556, file: !3, line: 324, type: !7)
!1717 = !DILocalVariable(name: "_result", scope: !558, file: !3, line: 325, type: !7)
!1718 = !DILocalVariable(name: "_result", scope: !560, file: !3, line: 326, type: !7)
!1719 = !DILocalVariable(name: "b", scope: !560, file: !3, line: 326, type: !7)
!1720 = !DILocalVariable(name: "a", scope: !560, file: !3, line: 326, type: !7)
!1721 = !DILocalVariable(name: "_result", scope: !562, file: !3, line: 327, type: !7)
!1722 = !DILocalVariable(name: "b", scope: !562, file: !3, line: 327, type: !7)
!1723 = !DILocalVariable(name: "a", scope: !562, file: !3, line: 327, type: !7)
!1724 = !DILocalVariable(name: "_result", scope: !564, file: !3, line: 328, type: !7)
!1725 = !DILocalVariable(name: "_result", scope: !566, file: !3, line: 329, type: !7)
!1726 = !DILocalVariable(name: "_result", scope: !568, file: !3, line: 330, type: !7)
!1727 = !DILocalVariable(name: "b", scope: !568, file: !3, line: 330, type: !7)
!1728 = !DILocalVariable(name: "a", scope: !568, file: !3, line: 330, type: !7)
!1729 = !DILocalVariable(name: "_result", scope: !570, file: !3, line: 331, type: !7)
!1730 = !DILocalVariable(name: "b", scope: !570, file: !3, line: 331, type: !7)
!1731 = !DILocalVariable(name: "a", scope: !570, file: !3, line: 331, type: !7)
!1732 = !DILocalVariable(name: "_result", scope: !572, file: !3, line: 332, type: !7)
!1733 = !DILocalVariable(name: "_result", scope: !574, file: !3, line: 333, type: !7)
!1734 = !DILocalVariable(name: "_result", scope: !576, file: !3, line: 334, type: !7)
!1735 = !DILocalVariable(name: "b", scope: !576, file: !3, line: 334, type: !7)
!1736 = !DILocalVariable(name: "a", scope: !576, file: !3, line: 334, type: !7)
!1737 = !DILocalVariable(name: "_result", scope: !578, file: !3, line: 335, type: !7)
!1738 = !DILocalVariable(name: "b", scope: !578, file: !3, line: 335, type: !7)
!1739 = !DILocalVariable(name: "a", scope: !578, file: !3, line: 335, type: !7)
!1740 = !DILocalVariable(name: "_result", scope: !580, file: !3, line: 336, type: !7)
!1741 = !DILocalVariable(name: "_result", scope: !582, file: !3, line: 337, type: !7)
!1742 = !DILocalVariable(name: "_result", scope: !584, file: !3, line: 338, type: !7)
!1743 = !DILocalVariable(name: "b", scope: !584, file: !3, line: 338, type: !7)
!1744 = !DILocalVariable(name: "a", scope: !584, file: !3, line: 338, type: !7)
!1745 = !DILocalVariable(name: "_result", scope: !586, file: !3, line: 339, type: !7)
!1746 = !DILocalVariable(name: "b", scope: !586, file: !3, line: 339, type: !7)
!1747 = !DILocalVariable(name: "a", scope: !586, file: !3, line: 339, type: !7)
!1748 = !DILocalVariable(name: "_result", scope: !588, file: !3, line: 340, type: !7)
!1749 = !DILocalVariable(name: "_result", scope: !590, file: !3, line: 341, type: !7)
!1750 = !DILocalVariable(name: "_result", scope: !592, file: !3, line: 342, type: !7)
!1751 = !DILocalVariable(name: "b", scope: !592, file: !3, line: 342, type: !7)
!1752 = !DILocalVariable(name: "a", scope: !592, file: !3, line: 342, type: !7)
!1753 = !DILocalVariable(name: "_result", scope: !594, file: !3, line: 343, type: !7)
!1754 = !DILocalVariable(name: "b", scope: !594, file: !3, line: 343, type: !7)
!1755 = !DILocalVariable(name: "a", scope: !594, file: !3, line: 343, type: !7)
!1756 = !DILocalVariable(name: "_result", scope: !596, file: !3, line: 344, type: !7)
!1757 = !DILocalVariable(name: "_result", scope: !598, file: !3, line: 345, type: !7)
!1758 = !DILocalVariable(name: "_result", scope: !600, file: !3, line: 346, type: !7)
!1759 = !DILocalVariable(name: "b", scope: !600, file: !3, line: 346, type: !7)
!1760 = !DILocalVariable(name: "a", scope: !600, file: !3, line: 346, type: !7)
!1761 = !DILocalVariable(name: "_result", scope: !602, file: !3, line: 347, type: !7)
!1762 = !DILocalVariable(name: "b", scope: !602, file: !3, line: 347, type: !7)
!1763 = !DILocalVariable(name: "a", scope: !602, file: !3, line: 347, type: !7)
!1764 = !DILocalVariable(name: "_result", scope: !604, file: !3, line: 348, type: !7)
!1765 = !DILocalVariable(name: "_result", scope: !606, file: !3, line: 349, type: !7)
!1766 = !DILocalVariable(name: "_result", scope: !608, file: !3, line: 350, type: !7)
!1767 = !DILocalVariable(name: "b", scope: !608, file: !3, line: 350, type: !7)
!1768 = !DILocalVariable(name: "a", scope: !608, file: !3, line: 350, type: !7)
!1769 = !DILocalVariable(name: "_result", scope: !610, file: !3, line: 351, type: !7)
!1770 = !DILocalVariable(name: "b", scope: !610, file: !3, line: 351, type: !7)
!1771 = !DILocalVariable(name: "a", scope: !610, file: !3, line: 351, type: !7)
!1772 = !DILocalVariable(name: "_result", scope: !612, file: !3, line: 352, type: !7)
!1773 = !DILocalVariable(name: "_result", scope: !614, file: !3, line: 353, type: !7)
!1774 = !DILocalVariable(name: "_result", scope: !616, file: !3, line: 354, type: !7)
!1775 = !DILocalVariable(name: "b", scope: !616, file: !3, line: 354, type: !7)
!1776 = !DILocalVariable(name: "a", scope: !616, file: !3, line: 354, type: !7)
!1777 = !DILocalVariable(name: "_result", scope: !618, file: !3, line: 355, type: !7)
!1778 = !DILocalVariable(name: "b", scope: !618, file: !3, line: 355, type: !7)
!1779 = !DILocalVariable(name: "a", scope: !618, file: !3, line: 355, type: !7)
!1780 = !DILocalVariable(name: "_result", scope: !620, file: !3, line: 356, type: !7)
!1781 = !DILocalVariable(name: "_result", scope: !622, file: !3, line: 357, type: !7)
!1782 = !DILocalVariable(name: "_result", scope: !624, file: !3, line: 358, type: !7)
!1783 = !DILocalVariable(name: "b", scope: !624, file: !3, line: 358, type: !7)
!1784 = !DILocalVariable(name: "a", scope: !624, file: !3, line: 358, type: !7)
!1785 = !DILocalVariable(name: "_result", scope: !626, file: !3, line: 359, type: !7)
!1786 = !DILocalVariable(name: "b", scope: !626, file: !3, line: 359, type: !7)
!1787 = !DILocalVariable(name: "a", scope: !626, file: !3, line: 359, type: !7)
!1788 = !DILocalVariable(name: "_result", scope: !628, file: !3, line: 360, type: !7)
!1789 = !DILocalVariable(name: "_result", scope: !630, file: !3, line: 361, type: !7)
!1790 = !DILocalVariable(name: "_result", scope: !632, file: !3, line: 362, type: !7)
!1791 = !DILocalVariable(name: "b", scope: !632, file: !3, line: 362, type: !7)
!1792 = !DILocalVariable(name: "a", scope: !632, file: !3, line: 362, type: !7)
!1793 = !DILocalVariable(name: "_result", scope: !634, file: !3, line: 363, type: !7)
!1794 = !DILocalVariable(name: "b", scope: !634, file: !3, line: 363, type: !7)
!1795 = !DILocalVariable(name: "a", scope: !634, file: !3, line: 363, type: !7)
!1796 = !DILocalVariable(name: "_result", scope: !636, file: !3, line: 364, type: !7)
!1797 = !DILocalVariable(name: "_result", scope: !638, file: !3, line: 365, type: !7)
!1798 = !DILocalVariable(name: "_result", scope: !640, file: !3, line: 366, type: !7)
!1799 = !DILocalVariable(name: "b", scope: !640, file: !3, line: 366, type: !7)
!1800 = !DILocalVariable(name: "a", scope: !640, file: !3, line: 366, type: !7)
!1801 = !DILocalVariable(name: "_result", scope: !642, file: !3, line: 367, type: !7)
!1802 = !DILocalVariable(name: "b", scope: !642, file: !3, line: 367, type: !7)
!1803 = !DILocalVariable(name: "a", scope: !642, file: !3, line: 367, type: !7)
!1804 = !DILocalVariable(name: "_result", scope: !644, file: !3, line: 368, type: !7)
!1805 = !DILocalVariable(name: "_result", scope: !646, file: !3, line: 369, type: !7)
!1806 = !DILocalVariable(name: "_result", scope: !648, file: !3, line: 370, type: !7)
!1807 = !DILocalVariable(name: "b", scope: !648, file: !3, line: 370, type: !7)
!1808 = !DILocalVariable(name: "a", scope: !648, file: !3, line: 370, type: !7)
!1809 = !DILocalVariable(name: "_result", scope: !650, file: !3, line: 371, type: !7)
!1810 = !DILocalVariable(name: "b", scope: !650, file: !3, line: 371, type: !7)
!1811 = !DILocalVariable(name: "a", scope: !650, file: !3, line: 371, type: !7)
!1812 = !DILocalVariable(name: "_result", scope: !652, file: !3, line: 372, type: !7)
!1813 = !DILocalVariable(name: "_result", scope: !654, file: !3, line: 373, type: !7)
!1814 = !DILocalVariable(name: "_result", scope: !656, file: !3, line: 374, type: !7)
!1815 = !DILocalVariable(name: "b", scope: !656, file: !3, line: 374, type: !7)
!1816 = !DILocalVariable(name: "a", scope: !656, file: !3, line: 374, type: !7)
!1817 = !DILocalVariable(name: "_result", scope: !658, file: !3, line: 375, type: !7)
!1818 = !DILocalVariable(name: "b", scope: !658, file: !3, line: 375, type: !7)
!1819 = !DILocalVariable(name: "a", scope: !658, file: !3, line: 375, type: !7)
!1820 = !DILocalVariable(name: "_result", scope: !660, file: !3, line: 376, type: !7)
!1821 = !DILocalVariable(name: "_result", scope: !662, file: !3, line: 377, type: !7)
!1822 = !DILocalVariable(name: "_result", scope: !664, file: !3, line: 378, type: !7)
!1823 = !DILocalVariable(name: "b", scope: !664, file: !3, line: 378, type: !7)
!1824 = !DILocalVariable(name: "a", scope: !664, file: !3, line: 378, type: !7)
!1825 = !DILocalVariable(name: "_result", scope: !666, file: !3, line: 379, type: !7)
!1826 = !DILocalVariable(name: "b", scope: !666, file: !3, line: 379, type: !7)
!1827 = !DILocalVariable(name: "a", scope: !666, file: !3, line: 379, type: !7)
!1828 = !DILocalVariable(name: "_result", scope: !668, file: !3, line: 380, type: !7)
!1829 = !DILocalVariable(name: "_result", scope: !670, file: !3, line: 381, type: !7)
!1830 = !DILocalVariable(name: "_result", scope: !672, file: !3, line: 382, type: !7)
!1831 = !DILocalVariable(name: "b", scope: !672, file: !3, line: 382, type: !7)
!1832 = !DILocalVariable(name: "a", scope: !672, file: !3, line: 382, type: !7)
!1833 = !DILocalVariable(name: "_result", scope: !674, file: !3, line: 383, type: !7)
!1834 = !DILocalVariable(name: "b", scope: !674, file: !3, line: 383, type: !7)
!1835 = !DILocalVariable(name: "a", scope: !674, file: !3, line: 383, type: !7)
!1836 = !DILocalVariable(name: "_result", scope: !676, file: !3, line: 384, type: !7)
!1837 = !DILocalVariable(name: "_result", scope: !678, file: !3, line: 385, type: !7)
!1838 = !DILocalVariable(name: "_result", scope: !680, file: !3, line: 386, type: !7)
!1839 = !DILocalVariable(name: "b", scope: !680, file: !3, line: 386, type: !7)
!1840 = !DILocalVariable(name: "a", scope: !680, file: !3, line: 386, type: !7)
!1841 = !DILocalVariable(name: "_result", scope: !682, file: !3, line: 387, type: !7)
!1842 = !DILocalVariable(name: "b", scope: !682, file: !3, line: 387, type: !7)
!1843 = !DILocalVariable(name: "a", scope: !682, file: !3, line: 387, type: !7)
!1844 = !DILocalVariable(name: "_result", scope: !684, file: !3, line: 388, type: !7)
!1845 = !DILocalVariable(name: "_result", scope: !686, file: !3, line: 389, type: !7)
!1846 = !DILocalVariable(name: "_result", scope: !688, file: !3, line: 390, type: !7)
!1847 = !DILocalVariable(name: "b", scope: !688, file: !3, line: 390, type: !7)
!1848 = !DILocalVariable(name: "a", scope: !688, file: !3, line: 390, type: !7)
!1849 = !DILocalVariable(name: "_result", scope: !690, file: !3, line: 391, type: !7)
!1850 = !DILocalVariable(name: "b", scope: !690, file: !3, line: 391, type: !7)
!1851 = !DILocalVariable(name: "a", scope: !690, file: !3, line: 391, type: !7)
!1852 = !DILocalVariable(name: "_result", scope: !692, file: !3, line: 392, type: !12)
!1853 = !DILocalVariable(name: "_result", scope: !694, file: !3, line: 393, type: !12)
!1854 = !DILocalVariable(name: "b", scope: !694, file: !3, line: 393, type: !12)
!1855 = !DILocalVariable(name: "a", scope: !694, file: !3, line: 393, type: !12)
!1856 = !DILocalVariable(name: "_result", scope: !696, file: !3, line: 394, type: !7)
!1857 = !DILocalVariable(name: "_result", scope: !698, file: !3, line: 395, type: !7)
!1858 = !DILocalVariable(name: "_result", scope: !700, file: !3, line: 396, type: !7)
!1859 = !DILocalVariable(name: "b", scope: !700, file: !3, line: 396, type: !7)
!1860 = !DILocalVariable(name: "a", scope: !700, file: !3, line: 396, type: !7)
!1861 = !DILocalVariable(name: "_result", scope: !702, file: !3, line: 397, type: !7)
!1862 = !DILocalVariable(name: "b", scope: !702, file: !3, line: 397, type: !7)
!1863 = !DILocalVariable(name: "a", scope: !702, file: !3, line: 397, type: !7)
!1864 = !DILocalVariable(name: "_result", scope: !704, file: !3, line: 398, type: !7)
!1865 = !DILocalVariable(name: "_result", scope: !706, file: !3, line: 399, type: !7)
!1866 = !DILocalVariable(name: "_result", scope: !708, file: !3, line: 400, type: !7)
!1867 = !DILocalVariable(name: "b", scope: !708, file: !3, line: 400, type: !7)
!1868 = !DILocalVariable(name: "a", scope: !708, file: !3, line: 400, type: !7)
!1869 = !DILocalVariable(name: "_result", scope: !710, file: !3, line: 401, type: !7)
!1870 = !DILocalVariable(name: "b", scope: !710, file: !3, line: 401, type: !7)
!1871 = !DILocalVariable(name: "a", scope: !710, file: !3, line: 401, type: !7)
!1872 = !DILocalVariable(name: "_result", scope: !712, file: !3, line: 402, type: !7)
!1873 = !DILocalVariable(name: "_result", scope: !714, file: !3, line: 403, type: !7)
!1874 = !DILocalVariable(name: "_result", scope: !716, file: !3, line: 404, type: !7)
!1875 = !DILocalVariable(name: "b", scope: !716, file: !3, line: 404, type: !7)
!1876 = !DILocalVariable(name: "a", scope: !716, file: !3, line: 404, type: !7)
!1877 = !DILocalVariable(name: "_result", scope: !718, file: !3, line: 405, type: !7)
!1878 = !DILocalVariable(name: "b", scope: !718, file: !3, line: 405, type: !7)
!1879 = !DILocalVariable(name: "a", scope: !718, file: !3, line: 405, type: !7)
!1880 = !DILocalVariable(name: "_result", scope: !720, file: !3, line: 406, type: !7)
!1881 = !DILocalVariable(name: "_result", scope: !722, file: !3, line: 407, type: !7)
!1882 = !DILocalVariable(name: "_result", scope: !724, file: !3, line: 408, type: !7)
!1883 = !DILocalVariable(name: "b", scope: !724, file: !3, line: 408, type: !7)
!1884 = !DILocalVariable(name: "a", scope: !724, file: !3, line: 408, type: !7)
!1885 = !DILocalVariable(name: "_result", scope: !726, file: !3, line: 409, type: !7)
!1886 = !DILocalVariable(name: "b", scope: !726, file: !3, line: 409, type: !7)
!1887 = !DILocalVariable(name: "a", scope: !726, file: !3, line: 409, type: !7)
!1888 = !DILocalVariable(name: "_result", scope: !728, file: !3, line: 410, type: !7)
!1889 = !DILocalVariable(name: "_result", scope: !730, file: !3, line: 411, type: !7)
!1890 = !DILocalVariable(name: "_result", scope: !732, file: !3, line: 412, type: !7)
!1891 = !DILocalVariable(name: "b", scope: !732, file: !3, line: 412, type: !7)
!1892 = !DILocalVariable(name: "a", scope: !732, file: !3, line: 412, type: !7)
!1893 = !DILocalVariable(name: "_result", scope: !734, file: !3, line: 413, type: !7)
!1894 = !DILocalVariable(name: "b", scope: !734, file: !3, line: 413, type: !7)
!1895 = !DILocalVariable(name: "a", scope: !734, file: !3, line: 413, type: !7)
!1896 = !DILocalVariable(name: "_result", scope: !736, file: !3, line: 414, type: !7)
!1897 = !DILocalVariable(name: "_result", scope: !738, file: !3, line: 415, type: !7)
!1898 = !DILocalVariable(name: "_result", scope: !740, file: !3, line: 416, type: !7)
!1899 = !DILocalVariable(name: "b", scope: !740, file: !3, line: 416, type: !7)
!1900 = !DILocalVariable(name: "a", scope: !740, file: !3, line: 416, type: !7)
!1901 = !DILocalVariable(name: "_result", scope: !742, file: !3, line: 417, type: !7)
!1902 = !DILocalVariable(name: "b", scope: !742, file: !3, line: 417, type: !7)
!1903 = !DILocalVariable(name: "a", scope: !742, file: !3, line: 417, type: !7)
!1904 = !DILocalVariable(name: "_result", scope: !744, file: !3, line: 418, type: !7)
!1905 = !DILocalVariable(name: "_result", scope: !746, file: !3, line: 419, type: !7)
!1906 = !DILocalVariable(name: "_result", scope: !748, file: !3, line: 420, type: !7)
!1907 = !DILocalVariable(name: "b", scope: !748, file: !3, line: 420, type: !7)
!1908 = !DILocalVariable(name: "a", scope: !748, file: !3, line: 420, type: !7)
!1909 = !DILocalVariable(name: "_result", scope: !750, file: !3, line: 421, type: !7)
!1910 = !DILocalVariable(name: "b", scope: !750, file: !3, line: 421, type: !7)
!1911 = !DILocalVariable(name: "a", scope: !750, file: !3, line: 421, type: !7)
!1912 = !DILocalVariable(name: "_result", scope: !752, file: !3, line: 422, type: !7)
!1913 = !DILocalVariable(name: "_result", scope: !754, file: !3, line: 423, type: !7)
!1914 = !DILocalVariable(name: "_result", scope: !756, file: !3, line: 424, type: !7)
!1915 = !DILocalVariable(name: "b", scope: !756, file: !3, line: 424, type: !7)
!1916 = !DILocalVariable(name: "a", scope: !756, file: !3, line: 424, type: !7)
!1917 = !DILocalVariable(name: "_result", scope: !758, file: !3, line: 425, type: !7)
!1918 = !DILocalVariable(name: "b", scope: !758, file: !3, line: 425, type: !7)
!1919 = !DILocalVariable(name: "a", scope: !758, file: !3, line: 425, type: !7)
!1920 = !DILocalVariable(name: "_result", scope: !760, file: !3, line: 426, type: !7)
!1921 = !DILocalVariable(name: "_result", scope: !762, file: !3, line: 427, type: !7)
!1922 = !DILocalVariable(name: "_result", scope: !764, file: !3, line: 428, type: !7)
!1923 = !DILocalVariable(name: "b", scope: !764, file: !3, line: 428, type: !7)
!1924 = !DILocalVariable(name: "a", scope: !764, file: !3, line: 428, type: !7)
!1925 = !DILocalVariable(name: "_result", scope: !766, file: !3, line: 429, type: !7)
!1926 = !DILocalVariable(name: "b", scope: !766, file: !3, line: 429, type: !7)
!1927 = !DILocalVariable(name: "a", scope: !766, file: !3, line: 429, type: !7)
!1928 = !DILocalVariable(name: "_result", scope: !768, file: !3, line: 430, type: !7)
!1929 = !DILocalVariable(name: "_result", scope: !770, file: !3, line: 431, type: !7)
!1930 = !DILocalVariable(name: "_result", scope: !772, file: !3, line: 432, type: !7)
!1931 = !DILocalVariable(name: "b", scope: !772, file: !3, line: 432, type: !7)
!1932 = !DILocalVariable(name: "a", scope: !772, file: !3, line: 432, type: !7)
!1933 = !DILocalVariable(name: "_result", scope: !774, file: !3, line: 433, type: !7)
!1934 = !DILocalVariable(name: "b", scope: !774, file: !3, line: 433, type: !7)
!1935 = !DILocalVariable(name: "a", scope: !774, file: !3, line: 433, type: !7)
!1936 = !DILocalVariable(name: "_result", scope: !776, file: !3, line: 434, type: !7)
!1937 = !DILocalVariable(name: "_result", scope: !778, file: !3, line: 435, type: !7)
!1938 = !DILocalVariable(name: "_result", scope: !780, file: !3, line: 436, type: !7)
!1939 = !DILocalVariable(name: "b", scope: !780, file: !3, line: 436, type: !7)
!1940 = !DILocalVariable(name: "a", scope: !780, file: !3, line: 436, type: !7)
!1941 = !DILocalVariable(name: "_result", scope: !782, file: !3, line: 437, type: !7)
!1942 = !DILocalVariable(name: "b", scope: !782, file: !3, line: 437, type: !7)
!1943 = !DILocalVariable(name: "a", scope: !782, file: !3, line: 437, type: !7)
!1944 = !DILocalVariable(name: "_result", scope: !784, file: !3, line: 438, type: !7)
!1945 = !DILocalVariable(name: "_result", scope: !786, file: !3, line: 439, type: !7)
!1946 = !DILocalVariable(name: "_result", scope: !788, file: !3, line: 440, type: !7)
!1947 = !DILocalVariable(name: "b", scope: !788, file: !3, line: 440, type: !7)
!1948 = !DILocalVariable(name: "a", scope: !788, file: !3, line: 440, type: !7)
!1949 = !DILocalVariable(name: "_result", scope: !790, file: !3, line: 441, type: !7)
!1950 = !DILocalVariable(name: "b", scope: !790, file: !3, line: 441, type: !7)
!1951 = !DILocalVariable(name: "a", scope: !790, file: !3, line: 441, type: !7)
!1952 = !DILocalVariable(name: "_result", scope: !792, file: !3, line: 442, type: !7)
!1953 = !DILocalVariable(name: "_result", scope: !794, file: !3, line: 443, type: !7)
!1954 = !DILocalVariable(name: "_result", scope: !796, file: !3, line: 444, type: !7)
!1955 = !DILocalVariable(name: "b", scope: !796, file: !3, line: 444, type: !7)
!1956 = !DILocalVariable(name: "a", scope: !796, file: !3, line: 444, type: !7)
!1957 = !DILocalVariable(name: "_result", scope: !798, file: !3, line: 445, type: !7)
!1958 = !DILocalVariable(name: "b", scope: !798, file: !3, line: 445, type: !7)
!1959 = !DILocalVariable(name: "a", scope: !798, file: !3, line: 445, type: !7)
!1960 = !DILocalVariable(name: "_result", scope: !800, file: !3, line: 446, type: !7)
!1961 = !DILocalVariable(name: "_result", scope: !802, file: !3, line: 447, type: !7)
!1962 = !DILocalVariable(name: "_result", scope: !804, file: !3, line: 448, type: !7)
!1963 = !DILocalVariable(name: "b", scope: !804, file: !3, line: 448, type: !7)
!1964 = !DILocalVariable(name: "a", scope: !804, file: !3, line: 448, type: !7)
!1965 = !DILocalVariable(name: "_result", scope: !806, file: !3, line: 449, type: !7)
!1966 = !DILocalVariable(name: "b", scope: !806, file: !3, line: 449, type: !7)
!1967 = !DILocalVariable(name: "a", scope: !806, file: !3, line: 449, type: !7)
!1968 = !DILocalVariable(name: "_result", scope: !808, file: !3, line: 450, type: !7)
!1969 = !DILocalVariable(name: "_result", scope: !810, file: !3, line: 451, type: !7)
!1970 = !DILocalVariable(name: "_result", scope: !812, file: !3, line: 452, type: !7)
!1971 = !DILocalVariable(name: "b", scope: !812, file: !3, line: 452, type: !7)
!1972 = !DILocalVariable(name: "a", scope: !812, file: !3, line: 452, type: !7)
!1973 = !DILocalVariable(name: "_result", scope: !814, file: !3, line: 453, type: !7)
!1974 = !DILocalVariable(name: "b", scope: !814, file: !3, line: 453, type: !7)
!1975 = !DILocalVariable(name: "a", scope: !814, file: !3, line: 453, type: !7)
!1976 = !DILocalVariable(name: "_result", scope: !816, file: !3, line: 454, type: !7)
!1977 = !DILocalVariable(name: "_result", scope: !818, file: !3, line: 455, type: !7)
!1978 = !DILocalVariable(name: "_result", scope: !820, file: !3, line: 456, type: !7)
!1979 = !DILocalVariable(name: "b", scope: !820, file: !3, line: 456, type: !7)
!1980 = !DILocalVariable(name: "a", scope: !820, file: !3, line: 456, type: !7)
!1981 = !DILocalVariable(name: "_result", scope: !822, file: !3, line: 457, type: !7)
!1982 = !DILocalVariable(name: "b", scope: !822, file: !3, line: 457, type: !7)
!1983 = !DILocalVariable(name: "a", scope: !822, file: !3, line: 457, type: !7)
!1984 = !DILocalVariable(name: "_result", scope: !824, file: !3, line: 458, type: !7)
!1985 = !DILocalVariable(name: "_result", scope: !826, file: !3, line: 459, type: !7)
!1986 = !DILocalVariable(name: "_result", scope: !828, file: !3, line: 460, type: !7)
!1987 = !DILocalVariable(name: "b", scope: !828, file: !3, line: 460, type: !7)
!1988 = !DILocalVariable(name: "a", scope: !828, file: !3, line: 460, type: !7)
!1989 = !DILocalVariable(name: "_result", scope: !830, file: !3, line: 461, type: !7)
!1990 = !DILocalVariable(name: "b", scope: !830, file: !3, line: 461, type: !7)
!1991 = !DILocalVariable(name: "a", scope: !830, file: !3, line: 461, type: !7)
!1992 = !DILocalVariable(name: "_result", scope: !832, file: !3, line: 462, type: !7)
!1993 = !DILocalVariable(name: "_result", scope: !834, file: !3, line: 463, type: !7)
!1994 = !DILocalVariable(name: "_result", scope: !836, file: !3, line: 464, type: !7)
!1995 = !DILocalVariable(name: "b", scope: !836, file: !3, line: 464, type: !7)
!1996 = !DILocalVariable(name: "a", scope: !836, file: !3, line: 464, type: !7)
!1997 = !DILocalVariable(name: "_result", scope: !838, file: !3, line: 465, type: !7)
!1998 = !DILocalVariable(name: "b", scope: !838, file: !3, line: 465, type: !7)
!1999 = !DILocalVariable(name: "a", scope: !838, file: !3, line: 465, type: !7)
!2000 = !DILocalVariable(name: "_result", scope: !840, file: !3, line: 466, type: !7)
!2001 = !DILocalVariable(name: "_result", scope: !842, file: !3, line: 467, type: !7)
!2002 = !DILocalVariable(name: "_result", scope: !844, file: !3, line: 468, type: !7)
!2003 = !DILocalVariable(name: "b", scope: !844, file: !3, line: 468, type: !7)
!2004 = !DILocalVariable(name: "a", scope: !844, file: !3, line: 468, type: !7)
!2005 = !DILocalVariable(name: "_result", scope: !846, file: !3, line: 469, type: !7)
!2006 = !DILocalVariable(name: "b", scope: !846, file: !3, line: 469, type: !7)
!2007 = !DILocalVariable(name: "a", scope: !846, file: !3, line: 469, type: !7)
!2008 = !DILocalVariable(name: "_result", scope: !848, file: !3, line: 470, type: !7)
!2009 = !DILocalVariable(name: "_result", scope: !850, file: !3, line: 471, type: !7)
!2010 = !DILocalVariable(name: "_result", scope: !852, file: !3, line: 472, type: !7)
!2011 = !DILocalVariable(name: "b", scope: !852, file: !3, line: 472, type: !7)
!2012 = !DILocalVariable(name: "a", scope: !852, file: !3, line: 472, type: !7)
!2013 = !DILocalVariable(name: "_result", scope: !854, file: !3, line: 473, type: !7)
!2014 = !DILocalVariable(name: "b", scope: !854, file: !3, line: 473, type: !7)
!2015 = !DILocalVariable(name: "a", scope: !854, file: !3, line: 473, type: !7)
!2016 = !DILocalVariable(name: "_result", scope: !856, file: !3, line: 474, type: !7)
!2017 = !DILocalVariable(name: "_result", scope: !858, file: !3, line: 475, type: !7)
!2018 = !DILocalVariable(name: "_result", scope: !860, file: !3, line: 476, type: !7)
!2019 = !DILocalVariable(name: "b", scope: !860, file: !3, line: 476, type: !7)
!2020 = !DILocalVariable(name: "a", scope: !860, file: !3, line: 476, type: !7)
!2021 = !DILocalVariable(name: "_result", scope: !862, file: !3, line: 477, type: !7)
!2022 = !DILocalVariable(name: "b", scope: !862, file: !3, line: 477, type: !7)
!2023 = !DILocalVariable(name: "a", scope: !862, file: !3, line: 477, type: !7)
!2024 = !DILocalVariable(name: "_result", scope: !864, file: !3, line: 478, type: !7)
!2025 = !DILocalVariable(name: "_result", scope: !866, file: !3, line: 479, type: !7)
!2026 = !DILocalVariable(name: "_result", scope: !868, file: !3, line: 480, type: !7)
!2027 = !DILocalVariable(name: "b", scope: !868, file: !3, line: 480, type: !7)
!2028 = !DILocalVariable(name: "a", scope: !868, file: !3, line: 480, type: !7)
!2029 = !DILocalVariable(name: "_result", scope: !870, file: !3, line: 481, type: !7)
!2030 = !DILocalVariable(name: "b", scope: !870, file: !3, line: 481, type: !7)
!2031 = !DILocalVariable(name: "a", scope: !870, file: !3, line: 481, type: !7)
!2032 = !DILocalVariable(name: "_result", scope: !872, file: !3, line: 482, type: !7)
!2033 = !DILocalVariable(name: "_result", scope: !874, file: !3, line: 483, type: !7)
!2034 = !DILocalVariable(name: "_result", scope: !876, file: !3, line: 484, type: !7)
!2035 = !DILocalVariable(name: "b", scope: !876, file: !3, line: 484, type: !7)
!2036 = !DILocalVariable(name: "a", scope: !876, file: !3, line: 484, type: !7)
!2037 = !DILocalVariable(name: "_result", scope: !878, file: !3, line: 485, type: !7)
!2038 = !DILocalVariable(name: "b", scope: !878, file: !3, line: 485, type: !7)
!2039 = !DILocalVariable(name: "a", scope: !878, file: !3, line: 485, type: !7)
!2040 = !DILocalVariable(name: "_result", scope: !880, file: !3, line: 486, type: !7)
!2041 = !DILocalVariable(name: "_result", scope: !882, file: !3, line: 487, type: !7)
!2042 = !DILocalVariable(name: "_result", scope: !884, file: !3, line: 488, type: !7)
!2043 = !DILocalVariable(name: "b", scope: !884, file: !3, line: 488, type: !7)
!2044 = !DILocalVariable(name: "a", scope: !884, file: !3, line: 488, type: !7)
!2045 = !DILocalVariable(name: "_result", scope: !886, file: !3, line: 489, type: !7)
!2046 = !DILocalVariable(name: "b", scope: !886, file: !3, line: 489, type: !7)
!2047 = !DILocalVariable(name: "a", scope: !886, file: !3, line: 489, type: !7)
!2048 = !DILocalVariable(name: "_result", scope: !888, file: !3, line: 490, type: !7)
!2049 = !DILocalVariable(name: "_result", scope: !890, file: !3, line: 491, type: !7)
!2050 = !DILocalVariable(name: "_result", scope: !892, file: !3, line: 492, type: !7)
!2051 = !DILocalVariable(name: "b", scope: !892, file: !3, line: 492, type: !7)
!2052 = !DILocalVariable(name: "a", scope: !892, file: !3, line: 492, type: !7)
!2053 = !DILocalVariable(name: "_result", scope: !894, file: !3, line: 493, type: !7)
!2054 = !DILocalVariable(name: "b", scope: !894, file: !3, line: 493, type: !7)
!2055 = !DILocalVariable(name: "a", scope: !894, file: !3, line: 493, type: !7)
!2056 = !DILocalVariable(name: "_result", scope: !896, file: !3, line: 494, type: !7)
!2057 = !DILocalVariable(name: "_result", scope: !898, file: !3, line: 495, type: !7)
!2058 = !DILocalVariable(name: "_result", scope: !900, file: !3, line: 496, type: !7)
!2059 = !DILocalVariable(name: "b", scope: !900, file: !3, line: 496, type: !7)
!2060 = !DILocalVariable(name: "a", scope: !900, file: !3, line: 496, type: !7)
!2061 = !DILocalVariable(name: "_result", scope: !902, file: !3, line: 497, type: !7)
!2062 = !DILocalVariable(name: "b", scope: !902, file: !3, line: 497, type: !7)
!2063 = !DILocalVariable(name: "a", scope: !902, file: !3, line: 497, type: !7)
!2064 = !DILocalVariable(name: "_result", scope: !904, file: !3, line: 498, type: !7)
!2065 = !DILocalVariable(name: "_result", scope: !906, file: !3, line: 499, type: !7)
!2066 = !DILocalVariable(name: "_result", scope: !908, file: !3, line: 500, type: !7)
!2067 = !DILocalVariable(name: "b", scope: !908, file: !3, line: 500, type: !7)
!2068 = !DILocalVariable(name: "a", scope: !908, file: !3, line: 500, type: !7)
!2069 = !DILocalVariable(name: "_result", scope: !910, file: !3, line: 501, type: !7)
!2070 = !DILocalVariable(name: "b", scope: !910, file: !3, line: 501, type: !7)
!2071 = !DILocalVariable(name: "a", scope: !910, file: !3, line: 501, type: !7)
!2072 = !DILocalVariable(name: "_result", scope: !912, file: !3, line: 502, type: !7)
!2073 = !DILocalVariable(name: "_result", scope: !914, file: !3, line: 503, type: !7)
!2074 = !DILocalVariable(name: "_result", scope: !916, file: !3, line: 504, type: !7)
!2075 = !DILocalVariable(name: "b", scope: !916, file: !3, line: 504, type: !7)
!2076 = !DILocalVariable(name: "a", scope: !916, file: !3, line: 504, type: !7)
!2077 = !DILocalVariable(name: "_result", scope: !918, file: !3, line: 505, type: !7)
!2078 = !DILocalVariable(name: "b", scope: !918, file: !3, line: 505, type: !7)
!2079 = !DILocalVariable(name: "a", scope: !918, file: !3, line: 505, type: !7)
!2080 = !DILocalVariable(name: "_result", scope: !920, file: !3, line: 506, type: !7)
!2081 = !DILocalVariable(name: "_result", scope: !922, file: !3, line: 507, type: !7)
!2082 = !DILocalVariable(name: "_result", scope: !924, file: !3, line: 508, type: !7)
!2083 = !DILocalVariable(name: "b", scope: !924, file: !3, line: 508, type: !7)
!2084 = !DILocalVariable(name: "a", scope: !924, file: !3, line: 508, type: !7)
!2085 = !DILocalVariable(name: "_result", scope: !926, file: !3, line: 509, type: !7)
!2086 = !DILocalVariable(name: "b", scope: !926, file: !3, line: 509, type: !7)
!2087 = !DILocalVariable(name: "a", scope: !926, file: !3, line: 509, type: !7)
!2088 = !DILocalVariable(name: "_result", scope: !928, file: !3, line: 510, type: !7)
!2089 = !DILocalVariable(name: "_result", scope: !930, file: !3, line: 511, type: !7)
!2090 = !DILocalVariable(name: "_result", scope: !932, file: !3, line: 512, type: !7)
!2091 = !DILocalVariable(name: "b", scope: !932, file: !3, line: 512, type: !7)
!2092 = !DILocalVariable(name: "a", scope: !932, file: !3, line: 512, type: !7)
!2093 = !DILocalVariable(name: "_result", scope: !934, file: !3, line: 513, type: !7)
!2094 = !DILocalVariable(name: "b", scope: !934, file: !3, line: 513, type: !7)
!2095 = !DILocalVariable(name: "a", scope: !934, file: !3, line: 513, type: !7)
!2096 = !DILocalVariable(name: "_result", scope: !936, file: !3, line: 514, type: !7)
!2097 = !DILocalVariable(name: "_result", scope: !938, file: !3, line: 515, type: !7)
!2098 = !DILocalVariable(name: "_result", scope: !940, file: !3, line: 516, type: !7)
!2099 = !DILocalVariable(name: "b", scope: !940, file: !3, line: 516, type: !7)
!2100 = !DILocalVariable(name: "a", scope: !940, file: !3, line: 516, type: !7)
!2101 = !DILocalVariable(name: "_result", scope: !942, file: !3, line: 517, type: !7)
!2102 = !DILocalVariable(name: "b", scope: !942, file: !3, line: 517, type: !7)
!2103 = !DILocalVariable(name: "a", scope: !942, file: !3, line: 517, type: !7)
!2104 = !DILocalVariable(name: "_result", scope: !944, file: !3, line: 518, type: !7)
!2105 = !DILocalVariable(name: "_result", scope: !946, file: !3, line: 519, type: !7)
!2106 = !DILocalVariable(name: "_result", scope: !948, file: !3, line: 520, type: !7)
!2107 = !DILocalVariable(name: "b", scope: !948, file: !3, line: 520, type: !7)
!2108 = !DILocalVariable(name: "a", scope: !948, file: !3, line: 520, type: !7)
!2109 = !DILocalVariable(name: "_result", scope: !950, file: !3, line: 521, type: !7)
!2110 = !DILocalVariable(name: "b", scope: !950, file: !3, line: 521, type: !7)
!2111 = !DILocalVariable(name: "a", scope: !950, file: !3, line: 521, type: !7)
!2112 = !DILocalVariable(name: "_result", scope: !952, file: !3, line: 522, type: !7)
!2113 = !DILocalVariable(name: "_result", scope: !954, file: !3, line: 523, type: !7)
!2114 = !DILocalVariable(name: "_result", scope: !956, file: !3, line: 524, type: !7)
!2115 = !DILocalVariable(name: "b", scope: !956, file: !3, line: 524, type: !7)
!2116 = !DILocalVariable(name: "a", scope: !956, file: !3, line: 524, type: !7)
!2117 = !DILocalVariable(name: "_result", scope: !958, file: !3, line: 525, type: !7)
!2118 = !DILocalVariable(name: "b", scope: !958, file: !3, line: 525, type: !7)
!2119 = !DILocalVariable(name: "a", scope: !958, file: !3, line: 525, type: !7)
!2120 = !DILocalVariable(name: "_result", scope: !960, file: !3, line: 526, type: !7)
!2121 = !DILocalVariable(name: "_result", scope: !962, file: !3, line: 527, type: !7)
!2122 = !DILocalVariable(name: "_result", scope: !964, file: !3, line: 528, type: !7)
!2123 = !DILocalVariable(name: "b", scope: !964, file: !3, line: 528, type: !7)
!2124 = !DILocalVariable(name: "a", scope: !964, file: !3, line: 528, type: !7)
!2125 = !DILocalVariable(name: "_result", scope: !966, file: !3, line: 529, type: !7)
!2126 = !DILocalVariable(name: "b", scope: !966, file: !3, line: 529, type: !7)
!2127 = !DILocalVariable(name: "a", scope: !966, file: !3, line: 529, type: !7)
!2128 = !DILocalVariable(name: "_result", scope: !968, file: !3, line: 530, type: !7)
!2129 = !DILocalVariable(name: "_result", scope: !970, file: !3, line: 531, type: !7)
!2130 = !DILocalVariable(name: "_result", scope: !972, file: !3, line: 532, type: !7)
!2131 = !DILocalVariable(name: "b", scope: !972, file: !3, line: 532, type: !7)
!2132 = !DILocalVariable(name: "a", scope: !972, file: !3, line: 532, type: !7)
!2133 = !DILocalVariable(name: "_result", scope: !974, file: !3, line: 533, type: !7)
!2134 = !DILocalVariable(name: "b", scope: !974, file: !3, line: 533, type: !7)
!2135 = !DILocalVariable(name: "a", scope: !974, file: !3, line: 533, type: !7)
!2136 = !DILocalVariable(name: "_result", scope: !976, file: !3, line: 534, type: !7)
!2137 = !DILocalVariable(name: "_result", scope: !978, file: !3, line: 535, type: !7)
!2138 = !DILocalVariable(name: "_result", scope: !980, file: !3, line: 536, type: !7)
!2139 = !DILocalVariable(name: "b", scope: !980, file: !3, line: 536, type: !7)
!2140 = !DILocalVariable(name: "a", scope: !980, file: !3, line: 536, type: !7)
!2141 = !DILocalVariable(name: "_result", scope: !982, file: !3, line: 537, type: !7)
!2142 = !DILocalVariable(name: "b", scope: !982, file: !3, line: 537, type: !7)
!2143 = !DILocalVariable(name: "a", scope: !982, file: !3, line: 537, type: !7)
!2144 = !DILocalVariable(name: "_result", scope: !984, file: !3, line: 538, type: !7)
!2145 = !DILocalVariable(name: "_result", scope: !986, file: !3, line: 539, type: !7)
!2146 = !DILocalVariable(name: "_result", scope: !988, file: !3, line: 540, type: !7)
!2147 = !DILocalVariable(name: "b", scope: !988, file: !3, line: 540, type: !7)
!2148 = !DILocalVariable(name: "a", scope: !988, file: !3, line: 540, type: !7)
!2149 = !DILocalVariable(name: "_result", scope: !990, file: !3, line: 541, type: !7)
!2150 = !DILocalVariable(name: "b", scope: !990, file: !3, line: 541, type: !7)
!2151 = !DILocalVariable(name: "a", scope: !990, file: !3, line: 541, type: !7)
!2152 = !DILocalVariable(name: "_result", scope: !992, file: !3, line: 542, type: !7)
!2153 = !DILocalVariable(name: "_result", scope: !994, file: !3, line: 543, type: !7)
!2154 = !DILocalVariable(name: "_result", scope: !996, file: !3, line: 544, type: !7)
!2155 = !DILocalVariable(name: "b", scope: !996, file: !3, line: 544, type: !7)
!2156 = !DILocalVariable(name: "a", scope: !996, file: !3, line: 544, type: !7)
!2157 = !DILocalVariable(name: "_result", scope: !998, file: !3, line: 545, type: !7)
!2158 = !DILocalVariable(name: "b", scope: !998, file: !3, line: 545, type: !7)
!2159 = !DILocalVariable(name: "a", scope: !998, file: !3, line: 545, type: !7)
!2160 = !DILocalVariable(name: "_result", scope: !1000, file: !3, line: 546, type: !7)
!2161 = !DILocalVariable(name: "_result", scope: !1002, file: !3, line: 547, type: !7)
!2162 = !DILocalVariable(name: "_result", scope: !1004, file: !3, line: 548, type: !7)
!2163 = !DILocalVariable(name: "b", scope: !1004, file: !3, line: 548, type: !7)
!2164 = !DILocalVariable(name: "a", scope: !1004, file: !3, line: 548, type: !7)
!2165 = !DILocalVariable(name: "_result", scope: !1006, file: !3, line: 549, type: !7)
!2166 = !DILocalVariable(name: "b", scope: !1006, file: !3, line: 549, type: !7)
!2167 = !DILocalVariable(name: "a", scope: !1006, file: !3, line: 549, type: !7)
!2168 = !DILocalVariable(name: "_result", scope: !1008, file: !3, line: 550, type: !7)
!2169 = !DILocalVariable(name: "_result", scope: !1010, file: !3, line: 551, type: !7)
!2170 = !DILocalVariable(name: "_result", scope: !1012, file: !3, line: 552, type: !7)
!2171 = !DILocalVariable(name: "b", scope: !1012, file: !3, line: 552, type: !7)
!2172 = !DILocalVariable(name: "a", scope: !1012, file: !3, line: 552, type: !7)
!2173 = !DILocalVariable(name: "_result", scope: !1014, file: !3, line: 553, type: !7)
!2174 = !DILocalVariable(name: "b", scope: !1014, file: !3, line: 553, type: !7)
!2175 = !DILocalVariable(name: "a", scope: !1014, file: !3, line: 553, type: !7)
!2176 = !DILocalVariable(name: "_result", scope: !1016, file: !3, line: 554, type: !7)
!2177 = !DILocalVariable(name: "_result", scope: !1018, file: !3, line: 555, type: !7)
!2178 = !DILocalVariable(name: "_result", scope: !1020, file: !3, line: 556, type: !7)
!2179 = !DILocalVariable(name: "b", scope: !1020, file: !3, line: 556, type: !7)
!2180 = !DILocalVariable(name: "a", scope: !1020, file: !3, line: 556, type: !7)
!2181 = !DILocalVariable(name: "_result", scope: !1022, file: !3, line: 557, type: !7)
!2182 = !DILocalVariable(name: "b", scope: !1022, file: !3, line: 557, type: !7)
!2183 = !DILocalVariable(name: "a", scope: !1022, file: !3, line: 557, type: !7)
!2184 = !DILocalVariable(name: "_result", scope: !1024, file: !3, line: 558, type: !7)
!2185 = !DILocalVariable(name: "_result", scope: !1026, file: !3, line: 559, type: !7)
!2186 = !DILocalVariable(name: "_result", scope: !1028, file: !3, line: 560, type: !7)
!2187 = !DILocalVariable(name: "b", scope: !1028, file: !3, line: 560, type: !7)
!2188 = !DILocalVariable(name: "a", scope: !1028, file: !3, line: 560, type: !7)
!2189 = !DILocalVariable(name: "_result", scope: !1030, file: !3, line: 561, type: !7)
!2190 = !DILocalVariable(name: "b", scope: !1030, file: !3, line: 561, type: !7)
!2191 = !DILocalVariable(name: "a", scope: !1030, file: !3, line: 561, type: !7)
!2192 = !DILocalVariable(name: "_result", scope: !1032, file: !3, line: 562, type: !7)
!2193 = !DILocalVariable(name: "_result", scope: !1034, file: !3, line: 563, type: !7)
!2194 = !DILocalVariable(name: "_result", scope: !1036, file: !3, line: 564, type: !7)
!2195 = !DILocalVariable(name: "b", scope: !1036, file: !3, line: 564, type: !7)
!2196 = !DILocalVariable(name: "a", scope: !1036, file: !3, line: 564, type: !7)
!2197 = !DILocalVariable(name: "_result", scope: !1038, file: !3, line: 565, type: !7)
!2198 = !DILocalVariable(name: "b", scope: !1038, file: !3, line: 565, type: !7)
!2199 = !DILocalVariable(name: "a", scope: !1038, file: !3, line: 565, type: !7)
!2200 = !DILocalVariable(name: "_result", scope: !1040, file: !3, line: 566, type: !7)
!2201 = !DILocalVariable(name: "_result", scope: !1042, file: !3, line: 567, type: !7)
!2202 = !DILocalVariable(name: "_result", scope: !1044, file: !3, line: 568, type: !7)
!2203 = !DILocalVariable(name: "b", scope: !1044, file: !3, line: 568, type: !7)
!2204 = !DILocalVariable(name: "a", scope: !1044, file: !3, line: 568, type: !7)
!2205 = !DILocalVariable(name: "_result", scope: !1046, file: !3, line: 569, type: !7)
!2206 = !DILocalVariable(name: "b", scope: !1046, file: !3, line: 569, type: !7)
!2207 = !DILocalVariable(name: "a", scope: !1046, file: !3, line: 569, type: !7)
!2208 = !DILocalVariable(name: "_result", scope: !1048, file: !3, line: 570, type: !7)
!2209 = !DILocalVariable(name: "_result", scope: !1050, file: !3, line: 571, type: !7)
!2210 = !DILocalVariable(name: "_result", scope: !1052, file: !3, line: 572, type: !7)
!2211 = !DILocalVariable(name: "b", scope: !1052, file: !3, line: 572, type: !7)
!2212 = !DILocalVariable(name: "a", scope: !1052, file: !3, line: 572, type: !7)
!2213 = !DILocalVariable(name: "_result", scope: !1054, file: !3, line: 573, type: !7)
!2214 = !DILocalVariable(name: "b", scope: !1054, file: !3, line: 573, type: !7)
!2215 = !DILocalVariable(name: "a", scope: !1054, file: !3, line: 573, type: !7)
!2216 = !DILocalVariable(name: "_result", scope: !1056, file: !3, line: 574, type: !7)
!2217 = !DILocalVariable(name: "_result", scope: !1058, file: !3, line: 575, type: !7)
!2218 = !DILocalVariable(name: "_result", scope: !1060, file: !3, line: 576, type: !7)
!2219 = !DILocalVariable(name: "b", scope: !1060, file: !3, line: 576, type: !7)
!2220 = !DILocalVariable(name: "a", scope: !1060, file: !3, line: 576, type: !7)
!2221 = !DILocalVariable(name: "_result", scope: !1062, file: !3, line: 577, type: !7)
!2222 = !DILocalVariable(name: "b", scope: !1062, file: !3, line: 577, type: !7)
!2223 = !DILocalVariable(name: "a", scope: !1062, file: !3, line: 577, type: !7)
!2224 = !DILocalVariable(name: "_result", scope: !1064, file: !3, line: 578, type: !7)
!2225 = !DILocalVariable(name: "_result", scope: !1066, file: !3, line: 579, type: !7)
!2226 = !DILocalVariable(name: "_result", scope: !1068, file: !3, line: 580, type: !7)
!2227 = !DILocalVariable(name: "b", scope: !1068, file: !3, line: 580, type: !7)
!2228 = !DILocalVariable(name: "a", scope: !1068, file: !3, line: 580, type: !7)
!2229 = !DILocalVariable(name: "_result", scope: !1070, file: !3, line: 581, type: !7)
!2230 = !DILocalVariable(name: "b", scope: !1070, file: !3, line: 581, type: !7)
!2231 = !DILocalVariable(name: "a", scope: !1070, file: !3, line: 581, type: !7)
!2232 = !DILocalVariable(name: "_result", scope: !1072, file: !3, line: 582, type: !7)
!2233 = !DILocalVariable(name: "_result", scope: !1074, file: !3, line: 583, type: !7)
!2234 = !DILocalVariable(name: "_result", scope: !1076, file: !3, line: 584, type: !7)
!2235 = !DILocalVariable(name: "b", scope: !1076, file: !3, line: 584, type: !7)
!2236 = !DILocalVariable(name: "a", scope: !1076, file: !3, line: 584, type: !7)
!2237 = !DILocalVariable(name: "_result", scope: !1078, file: !3, line: 585, type: !7)
!2238 = !DILocalVariable(name: "b", scope: !1078, file: !3, line: 585, type: !7)
!2239 = !DILocalVariable(name: "a", scope: !1078, file: !3, line: 585, type: !7)
!2240 = !DILocalVariable(name: "_result", scope: !1080, file: !3, line: 586, type: !7)
!2241 = !DILocalVariable(name: "_result", scope: !1082, file: !3, line: 587, type: !7)
!2242 = !DILocalVariable(name: "_result", scope: !1084, file: !3, line: 588, type: !7)
!2243 = !DILocalVariable(name: "b", scope: !1084, file: !3, line: 588, type: !7)
!2244 = !DILocalVariable(name: "a", scope: !1084, file: !3, line: 588, type: !7)
!2245 = !DILocalVariable(name: "_result", scope: !1086, file: !3, line: 589, type: !7)
!2246 = !DILocalVariable(name: "b", scope: !1086, file: !3, line: 589, type: !7)
!2247 = !DILocalVariable(name: "a", scope: !1086, file: !3, line: 589, type: !7)
!2248 = !DILocalVariable(name: "_result", scope: !1088, file: !3, line: 590, type: !7)
!2249 = !DILocalVariable(name: "_result", scope: !1090, file: !3, line: 591, type: !7)
!2250 = !DILocalVariable(name: "_result", scope: !1092, file: !3, line: 592, type: !7)
!2251 = !DILocalVariable(name: "b", scope: !1092, file: !3, line: 592, type: !7)
!2252 = !DILocalVariable(name: "a", scope: !1092, file: !3, line: 592, type: !7)
!2253 = !DILocalVariable(name: "_result", scope: !1094, file: !3, line: 593, type: !7)
!2254 = !DILocalVariable(name: "b", scope: !1094, file: !3, line: 593, type: !7)
!2255 = !DILocalVariable(name: "a", scope: !1094, file: !3, line: 593, type: !7)
!2256 = !DILocalVariable(name: "_result", scope: !1096, file: !3, line: 594, type: !7)
!2257 = !DILocalVariable(name: "_result", scope: !1098, file: !3, line: 595, type: !7)
!2258 = !DILocalVariable(name: "_result", scope: !1100, file: !3, line: 596, type: !7)
!2259 = !DILocalVariable(name: "b", scope: !1100, file: !3, line: 596, type: !7)
!2260 = !DILocalVariable(name: "a", scope: !1100, file: !3, line: 596, type: !7)
!2261 = !DILocalVariable(name: "_result", scope: !1102, file: !3, line: 597, type: !7)
!2262 = !DILocalVariable(name: "b", scope: !1102, file: !3, line: 597, type: !7)
!2263 = !DILocalVariable(name: "a", scope: !1102, file: !3, line: 597, type: !7)
!2264 = !DILocalVariable(name: "_result", scope: !1104, file: !3, line: 598, type: !7)
!2265 = !DILocalVariable(name: "_result", scope: !1106, file: !3, line: 599, type: !7)
!2266 = !DILocalVariable(name: "_result", scope: !1108, file: !3, line: 600, type: !7)
!2267 = !DILocalVariable(name: "b", scope: !1108, file: !3, line: 600, type: !7)
!2268 = !DILocalVariable(name: "a", scope: !1108, file: !3, line: 600, type: !7)
!2269 = !DILocalVariable(name: "_result", scope: !1110, file: !3, line: 601, type: !7)
!2270 = !DILocalVariable(name: "b", scope: !1110, file: !3, line: 601, type: !7)
!2271 = !DILocalVariable(name: "a", scope: !1110, file: !3, line: 601, type: !7)
!2272 = !DILocalVariable(name: "_result", scope: !1112, file: !3, line: 602, type: !7)
!2273 = !DILocalVariable(name: "_result", scope: !1114, file: !3, line: 603, type: !7)
!2274 = !DILocalVariable(name: "_result", scope: !1116, file: !3, line: 604, type: !7)
!2275 = !DILocalVariable(name: "b", scope: !1116, file: !3, line: 604, type: !7)
!2276 = !DILocalVariable(name: "a", scope: !1116, file: !3, line: 604, type: !7)
!2277 = !DILocalVariable(name: "_result", scope: !1118, file: !3, line: 605, type: !7)
!2278 = !DILocalVariable(name: "b", scope: !1118, file: !3, line: 605, type: !7)
!2279 = !DILocalVariable(name: "a", scope: !1118, file: !3, line: 605, type: !7)
!2280 = !DILocalVariable(name: "_result", scope: !1120, file: !3, line: 606, type: !7)
!2281 = !DILocalVariable(name: "_result", scope: !1122, file: !3, line: 607, type: !7)
!2282 = !DILocalVariable(name: "_result", scope: !1124, file: !3, line: 608, type: !7)
!2283 = !DILocalVariable(name: "b", scope: !1124, file: !3, line: 608, type: !7)
!2284 = !DILocalVariable(name: "a", scope: !1124, file: !3, line: 608, type: !7)
!2285 = !DILocalVariable(name: "_result", scope: !1126, file: !3, line: 609, type: !7)
!2286 = !DILocalVariable(name: "b", scope: !1126, file: !3, line: 609, type: !7)
!2287 = !DILocalVariable(name: "a", scope: !1126, file: !3, line: 609, type: !7)
!2288 = !DILocalVariable(name: "_result", scope: !1128, file: !3, line: 610, type: !7)
!2289 = !DILocalVariable(name: "_result", scope: !1130, file: !3, line: 611, type: !7)
!2290 = !DILocalVariable(name: "_result", scope: !1132, file: !3, line: 612, type: !7)
!2291 = !DILocalVariable(name: "b", scope: !1132, file: !3, line: 612, type: !7)
!2292 = !DILocalVariable(name: "a", scope: !1132, file: !3, line: 612, type: !7)
!2293 = !DILocalVariable(name: "_result", scope: !1134, file: !3, line: 613, type: !7)
!2294 = !DILocalVariable(name: "b", scope: !1134, file: !3, line: 613, type: !7)
!2295 = !DILocalVariable(name: "a", scope: !1134, file: !3, line: 613, type: !7)
!2296 = !DILocalVariable(name: "_result", scope: !1136, file: !3, line: 614, type: !7)
!2297 = !DILocalVariable(name: "_result", scope: !1138, file: !3, line: 615, type: !7)
!2298 = !DILocalVariable(name: "_result", scope: !1140, file: !3, line: 616, type: !7)
!2299 = !DILocalVariable(name: "b", scope: !1140, file: !3, line: 616, type: !7)
!2300 = !DILocalVariable(name: "a", scope: !1140, file: !3, line: 616, type: !7)
!2301 = !DILocalVariable(name: "_result", scope: !1142, file: !3, line: 617, type: !7)
!2302 = !DILocalVariable(name: "b", scope: !1142, file: !3, line: 617, type: !7)
!2303 = !DILocalVariable(name: "a", scope: !1142, file: !3, line: 617, type: !7)
!2304 = !DILocalVariable(name: "_result", scope: !1144, file: !3, line: 618, type: !7)
!2305 = !DILocalVariable(name: "_result", scope: !1146, file: !3, line: 619, type: !7)
!2306 = !DILocalVariable(name: "_result", scope: !1148, file: !3, line: 620, type: !7)
!2307 = !DILocalVariable(name: "b", scope: !1148, file: !3, line: 620, type: !7)
!2308 = !DILocalVariable(name: "a", scope: !1148, file: !3, line: 620, type: !7)
!2309 = !DILocalVariable(name: "_result", scope: !1150, file: !3, line: 621, type: !7)
!2310 = !DILocalVariable(name: "b", scope: !1150, file: !3, line: 621, type: !7)
!2311 = !DILocalVariable(name: "a", scope: !1150, file: !3, line: 621, type: !7)
!2312 = !DILocalVariable(name: "_result", scope: !1152, file: !3, line: 622, type: !7)
!2313 = !DILocalVariable(name: "_result", scope: !1154, file: !3, line: 623, type: !7)
!2314 = !DILocalVariable(name: "_result", scope: !1156, file: !3, line: 624, type: !7)
!2315 = !DILocalVariable(name: "b", scope: !1156, file: !3, line: 624, type: !7)
!2316 = !DILocalVariable(name: "a", scope: !1156, file: !3, line: 624, type: !7)
!2317 = !DILocalVariable(name: "_result", scope: !1158, file: !3, line: 625, type: !7)
!2318 = !DILocalVariable(name: "b", scope: !1158, file: !3, line: 625, type: !7)
!2319 = !DILocalVariable(name: "a", scope: !1158, file: !3, line: 625, type: !7)
!2320 = !DILocalVariable(name: "_result", scope: !1160, file: !3, line: 626, type: !7)
!2321 = !DILocalVariable(name: "_result", scope: !1162, file: !3, line: 627, type: !7)
!2322 = !DILocalVariable(name: "_result", scope: !1164, file: !3, line: 628, type: !7)
!2323 = !DILocalVariable(name: "b", scope: !1164, file: !3, line: 628, type: !7)
!2324 = !DILocalVariable(name: "a", scope: !1164, file: !3, line: 628, type: !7)
!2325 = !DILocalVariable(name: "_result", scope: !1166, file: !3, line: 629, type: !7)
!2326 = !DILocalVariable(name: "b", scope: !1166, file: !3, line: 629, type: !7)
!2327 = !DILocalVariable(name: "a", scope: !1166, file: !3, line: 629, type: !7)
!2328 = !DILocalVariable(name: "_result", scope: !1168, file: !3, line: 630, type: !7)
!2329 = !DILocalVariable(name: "_result", scope: !1170, file: !3, line: 631, type: !7)
!2330 = !DILocalVariable(name: "_result", scope: !1172, file: !3, line: 632, type: !7)
!2331 = !DILocalVariable(name: "b", scope: !1172, file: !3, line: 632, type: !7)
!2332 = !DILocalVariable(name: "a", scope: !1172, file: !3, line: 632, type: !7)
!2333 = !DILocalVariable(name: "_result", scope: !1174, file: !3, line: 633, type: !7)
!2334 = !DILocalVariable(name: "b", scope: !1174, file: !3, line: 633, type: !7)
!2335 = !DILocalVariable(name: "a", scope: !1174, file: !3, line: 633, type: !7)
!2336 = !DILocation(line: 54, column: 0, scope: !16)
!2337 = !DILocation(line: 55, column: 0, scope: !18)
!2338 = !DILocation(line: 56, column: 0, scope: !20)
!2339 = !DILocation(line: 57, column: 0, scope: !22)
!2340 = !DILocation(line: 58, column: 0, scope: !24)
!2341 = !DILocation(line: 59, column: 0, scope: !26)
!2342 = !DILocation(line: 60, column: 0, scope: !28)
!2343 = !DILocation(line: 61, column: 0, scope: !30)
!2344 = !DILocation(line: 62, column: 0, scope: !32)
!2345 = !DILocation(line: 63, column: 0, scope: !34)
!2346 = !DILocation(line: 64, column: 0, scope: !36)
!2347 = !DILocation(line: 65, column: 0, scope: !38)
!2348 = !DILocation(line: 66, column: 0, scope: !40)
!2349 = !DILocation(line: 67, column: 0, scope: !42)
!2350 = !DILocation(line: 68, column: 0, scope: !44)
!2351 = !DILocation(line: 69, column: 0, scope: !46)
!2352 = !DILocation(line: 70, column: 0, scope: !48)
!2353 = !DILocation(line: 71, column: 0, scope: !50)
!2354 = !DILocation(line: 72, column: 0, scope: !52)
!2355 = !DILocation(line: 73, column: 0, scope: !54)
!2356 = !DILocation(line: 74, column: 0, scope: !56)
!2357 = !DILocation(line: 75, column: 0, scope: !58)
!2358 = !DILocation(line: 76, column: 0, scope: !60)
!2359 = !DILocation(line: 77, column: 0, scope: !62)
!2360 = !DILocation(line: 78, column: 0, scope: !64)
!2361 = !DILocation(line: 79, column: 0, scope: !66)
!2362 = !DILocation(line: 80, column: 0, scope: !68)
!2363 = !DILocation(line: 81, column: 0, scope: !70)
!2364 = !DILocation(line: 82, column: 0, scope: !72)
!2365 = !DILocation(line: 83, column: 0, scope: !74)
!2366 = !DILocation(line: 84, column: 0, scope: !76)
!2367 = !DILocation(line: 85, column: 0, scope: !78)
!2368 = !DILocation(line: 86, column: 0, scope: !80)
!2369 = !DILocation(line: 87, column: 0, scope: !82)
!2370 = !DILocation(line: 88, column: 0, scope: !84)
!2371 = !DILocation(line: 89, column: 0, scope: !86)
!2372 = !DILocation(line: 90, column: 0, scope: !88)
!2373 = !DILocation(line: 91, column: 0, scope: !90)
!2374 = !DILocation(line: 92, column: 0, scope: !92)
!2375 = !DILocation(line: 93, column: 0, scope: !94)
!2376 = !DILocation(line: 94, column: 0, scope: !96)
!2377 = !DILocation(line: 95, column: 0, scope: !98)
!2378 = !DILocation(line: 96, column: 0, scope: !100)
!2379 = !DILocation(line: 97, column: 0, scope: !102)
!2380 = !DILocation(line: 98, column: 0, scope: !104)
!2381 = !DILocation(line: 99, column: 0, scope: !106)
!2382 = !DILocation(line: 100, column: 0, scope: !108)
!2383 = !DILocation(line: 101, column: 0, scope: !110)
!2384 = !DILocation(line: 102, column: 0, scope: !112)
!2385 = !DILocation(line: 103, column: 0, scope: !114)
!2386 = !DILocation(line: 104, column: 0, scope: !116)
!2387 = !DILocation(line: 105, column: 0, scope: !118)
!2388 = !DILocation(line: 106, column: 0, scope: !120)
!2389 = !DILocation(line: 107, column: 0, scope: !122)
!2390 = !DILocation(line: 108, column: 0, scope: !124)
!2391 = !DILocation(line: 109, column: 0, scope: !126)
!2392 = !DILocation(line: 110, column: 0, scope: !128)
!2393 = !DILocation(line: 111, column: 0, scope: !130)
!2394 = !DILocation(line: 112, column: 0, scope: !132)
!2395 = !DILocation(line: 113, column: 0, scope: !134)
!2396 = !DILocation(line: 114, column: 0, scope: !136)
!2397 = !DILocation(line: 115, column: 0, scope: !138)
!2398 = !DILocation(line: 116, column: 0, scope: !140)
!2399 = !DILocation(line: 117, column: 0, scope: !142)
!2400 = !DILocation(line: 118, column: 0, scope: !144)
!2401 = !DILocation(line: 119, column: 0, scope: !146)
!2402 = !DILocation(line: 120, column: 0, scope: !148)
!2403 = !DILocation(line: 121, column: 0, scope: !150)
!2404 = !DILocation(line: 122, column: 0, scope: !152)
!2405 = !DILocation(line: 123, column: 0, scope: !154)
!2406 = !DILocation(line: 124, column: 0, scope: !156)
!2407 = !DILocation(line: 125, column: 0, scope: !158)
!2408 = !DILocation(line: 126, column: 0, scope: !160)
!2409 = !DILocation(line: 127, column: 0, scope: !162)
!2410 = !DILocation(line: 128, column: 0, scope: !164)
!2411 = !DILocation(line: 129, column: 0, scope: !166)
!2412 = !DILocation(line: 130, column: 0, scope: !168)
!2413 = !DILocation(line: 131, column: 0, scope: !170)
!2414 = !DILocation(line: 132, column: 0, scope: !172)
!2415 = !DILocation(line: 133, column: 0, scope: !174)
!2416 = !DILocation(line: 134, column: 0, scope: !176)
!2417 = !DILocation(line: 135, column: 0, scope: !178)
!2418 = !DILocation(line: 136, column: 0, scope: !180)
!2419 = !DILocation(line: 137, column: 0, scope: !182)
!2420 = !DILocation(line: 138, column: 0, scope: !184)
!2421 = !DILocation(line: 139, column: 0, scope: !186)
!2422 = !DILocation(line: 140, column: 0, scope: !188)
!2423 = !DILocation(line: 141, column: 0, scope: !190)
!2424 = !DILocation(line: 142, column: 0, scope: !192)
!2425 = !DILocation(line: 143, column: 0, scope: !194)
!2426 = !DILocation(line: 144, column: 0, scope: !196)
!2427 = !DILocation(line: 145, column: 0, scope: !198)
!2428 = !DILocation(line: 146, column: 0, scope: !200)
!2429 = !DILocation(line: 147, column: 0, scope: !202)
!2430 = !DILocation(line: 148, column: 0, scope: !204)
!2431 = !DILocation(line: 149, column: 0, scope: !206)
!2432 = !DILocation(line: 150, column: 0, scope: !208)
!2433 = !DILocation(line: 151, column: 0, scope: !210)
!2434 = !DILocation(line: 152, column: 0, scope: !212)
!2435 = !DILocation(line: 153, column: 0, scope: !214)
!2436 = !DILocation(line: 154, column: 0, scope: !216)
!2437 = !DILocation(line: 155, column: 0, scope: !218)
!2438 = !DILocation(line: 156, column: 0, scope: !220)
!2439 = !DILocation(line: 157, column: 0, scope: !222)
!2440 = !DILocation(line: 158, column: 0, scope: !224)
!2441 = !DILocation(line: 159, column: 0, scope: !226)
!2442 = !DILocation(line: 160, column: 0, scope: !228)
!2443 = !DILocation(line: 161, column: 0, scope: !230)
!2444 = !DILocation(line: 162, column: 0, scope: !232)
!2445 = !DILocation(line: 163, column: 0, scope: !234)
!2446 = !DILocation(line: 164, column: 0, scope: !236)
!2447 = !DILocation(line: 165, column: 0, scope: !238)
!2448 = !DILocation(line: 166, column: 0, scope: !240)
!2449 = !DILocation(line: 167, column: 0, scope: !242)
!2450 = !DILocation(line: 168, column: 0, scope: !244)
!2451 = !DILocation(line: 169, column: 0, scope: !246)
!2452 = !DILocation(line: 170, column: 0, scope: !248)
!2453 = !DILocation(line: 171, column: 0, scope: !250)
!2454 = !DILocation(line: 172, column: 0, scope: !252)
!2455 = !DILocation(line: 173, column: 0, scope: !254)
!2456 = !DILocation(line: 174, column: 0, scope: !256)
!2457 = !DILocation(line: 175, column: 0, scope: !258)
!2458 = !DILocation(line: 176, column: 0, scope: !260)
!2459 = !DILocation(line: 177, column: 0, scope: !262)
!2460 = !DILocation(line: 178, column: 0, scope: !264)
!2461 = !DILocation(line: 179, column: 0, scope: !266)
!2462 = !DILocation(line: 180, column: 0, scope: !268)
!2463 = !DILocation(line: 181, column: 0, scope: !270)
!2464 = !DILocation(line: 182, column: 0, scope: !272)
!2465 = !DILocation(line: 183, column: 0, scope: !274)
!2466 = !DILocation(line: 184, column: 0, scope: !276)
!2467 = !DILocation(line: 185, column: 0, scope: !278)
!2468 = !DILocation(line: 186, column: 0, scope: !280)
!2469 = !DILocation(line: 187, column: 0, scope: !282)
!2470 = !DILocation(line: 188, column: 0, scope: !284)
!2471 = !DILocation(line: 189, column: 0, scope: !286)
!2472 = !DILocation(line: 190, column: 0, scope: !288)
!2473 = !DILocation(line: 191, column: 0, scope: !290)
!2474 = !DILocation(line: 192, column: 0, scope: !292)
!2475 = !DILocation(line: 193, column: 0, scope: !294)
!2476 = !DILocation(line: 194, column: 0, scope: !296)
!2477 = !DILocation(line: 195, column: 0, scope: !298)
!2478 = !DILocation(line: 196, column: 0, scope: !300)
!2479 = !DILocation(line: 197, column: 0, scope: !302)
!2480 = !DILocation(line: 198, column: 0, scope: !304)
!2481 = !DILocation(line: 199, column: 0, scope: !306)
!2482 = !DILocation(line: 200, column: 0, scope: !308)
!2483 = !DILocation(line: 201, column: 0, scope: !310)
!2484 = !DILocation(line: 202, column: 0, scope: !312)
!2485 = !DILocation(line: 203, column: 0, scope: !314)
!2486 = !DILocation(line: 204, column: 0, scope: !316)
!2487 = !DILocation(line: 205, column: 0, scope: !318)
!2488 = !DILocation(line: 206, column: 0, scope: !320)
!2489 = !DILocation(line: 207, column: 0, scope: !322)
!2490 = !DILocation(line: 208, column: 0, scope: !324)
!2491 = !DILocation(line: 209, column: 0, scope: !326)
!2492 = !DILocation(line: 210, column: 0, scope: !328)
!2493 = !DILocation(line: 211, column: 0, scope: !330)
!2494 = !DILocation(line: 212, column: 0, scope: !332)
!2495 = !DILocation(line: 213, column: 0, scope: !334)
!2496 = !DILocation(line: 214, column: 0, scope: !336)
!2497 = !DILocation(line: 215, column: 0, scope: !338)
!2498 = !DILocation(line: 216, column: 0, scope: !340)
!2499 = !DILocation(line: 217, column: 0, scope: !342)
!2500 = !DILocation(line: 218, column: 0, scope: !344)
!2501 = !DILocation(line: 219, column: 0, scope: !346)
!2502 = !DILocation(line: 220, column: 0, scope: !348)
!2503 = !DILocation(line: 221, column: 0, scope: !350)
!2504 = !DILocation(line: 222, column: 0, scope: !352)
!2505 = !DILocation(line: 223, column: 0, scope: !354)
!2506 = !DILocation(line: 224, column: 0, scope: !356)
!2507 = !DILocation(line: 225, column: 0, scope: !358)
!2508 = !DILocation(line: 226, column: 0, scope: !360)
!2509 = !DILocation(line: 227, column: 0, scope: !362)
!2510 = !DILocation(line: 228, column: 0, scope: !364)
!2511 = !DILocation(line: 229, column: 0, scope: !366)
!2512 = !DILocation(line: 230, column: 0, scope: !368)
!2513 = !DILocation(line: 231, column: 0, scope: !370)
!2514 = !DILocation(line: 232, column: 0, scope: !372)
!2515 = !DILocation(line: 233, column: 0, scope: !374)
!2516 = !DILocation(line: 234, column: 0, scope: !376)
!2517 = !DILocation(line: 235, column: 0, scope: !378)
!2518 = !DILocation(line: 236, column: 0, scope: !380)
!2519 = !DILocation(line: 237, column: 0, scope: !382)
!2520 = !DILocation(line: 238, column: 0, scope: !384)
!2521 = !DILocation(line: 239, column: 0, scope: !386)
!2522 = !DILocation(line: 240, column: 0, scope: !388)
!2523 = !DILocation(line: 241, column: 0, scope: !390)
!2524 = !DILocation(line: 242, column: 0, scope: !392)
!2525 = !DILocation(line: 243, column: 0, scope: !394)
!2526 = !DILocation(line: 244, column: 0, scope: !396)
!2527 = !DILocation(line: 245, column: 0, scope: !398)
!2528 = !DILocation(line: 246, column: 0, scope: !400)
!2529 = !DILocation(line: 247, column: 0, scope: !402)
!2530 = !DILocation(line: 248, column: 0, scope: !404)
!2531 = !DILocation(line: 249, column: 0, scope: !406)
!2532 = !DILocation(line: 250, column: 0, scope: !408)
!2533 = !DILocation(line: 251, column: 0, scope: !410)
!2534 = !DILocation(line: 252, column: 0, scope: !412)
!2535 = !DILocation(line: 253, column: 0, scope: !414)
!2536 = !DILocation(line: 254, column: 0, scope: !416)
!2537 = !DILocation(line: 255, column: 0, scope: !418)
!2538 = !DILocation(line: 256, column: 0, scope: !420)
!2539 = !DILocation(line: 257, column: 0, scope: !422)
!2540 = !DILocation(line: 258, column: 0, scope: !424)
!2541 = !DILocation(line: 259, column: 0, scope: !426)
!2542 = !DILocation(line: 260, column: 0, scope: !428)
!2543 = !DILocation(line: 261, column: 0, scope: !430)
!2544 = !DILocation(line: 262, column: 0, scope: !432)
!2545 = !DILocation(line: 263, column: 0, scope: !434)
!2546 = !DILocation(line: 264, column: 0, scope: !436)
!2547 = !DILocation(line: 265, column: 0, scope: !438)
!2548 = !DILocation(line: 266, column: 0, scope: !440)
!2549 = !DILocation(line: 267, column: 0, scope: !442)
!2550 = !DILocation(line: 268, column: 0, scope: !444)
!2551 = !DILocation(line: 269, column: 0, scope: !446)
!2552 = !DILocation(line: 270, column: 0, scope: !448)
!2553 = !DILocation(line: 271, column: 0, scope: !450)
!2554 = !DILocation(line: 272, column: 0, scope: !452)
!2555 = !DILocation(line: 273, column: 0, scope: !454)
!2556 = !DILocation(line: 274, column: 0, scope: !456)
!2557 = !DILocation(line: 275, column: 0, scope: !458)
!2558 = !DILocation(line: 276, column: 0, scope: !460)
!2559 = !DILocation(line: 277, column: 0, scope: !462)
!2560 = !DILocation(line: 278, column: 0, scope: !464)
!2561 = !DILocation(line: 279, column: 0, scope: !466)
!2562 = !DILocation(line: 280, column: 0, scope: !468)
!2563 = !DILocation(line: 281, column: 0, scope: !470)
!2564 = !DILocation(line: 282, column: 0, scope: !472)
!2565 = !DILocation(line: 283, column: 0, scope: !474)
!2566 = !DILocation(line: 284, column: 0, scope: !476)
!2567 = !DILocation(line: 285, column: 0, scope: !478)
!2568 = !DILocation(line: 286, column: 0, scope: !480)
!2569 = !DILocation(line: 287, column: 0, scope: !482)
!2570 = !DILocation(line: 288, column: 0, scope: !484)
!2571 = !DILocation(line: 289, column: 0, scope: !486)
!2572 = !DILocation(line: 290, column: 0, scope: !488)
!2573 = !DILocation(line: 291, column: 0, scope: !490)
!2574 = !DILocation(line: 292, column: 0, scope: !492)
!2575 = !DILocation(line: 293, column: 0, scope: !494)
!2576 = !DILocation(line: 294, column: 0, scope: !496)
!2577 = !DILocation(line: 295, column: 0, scope: !498)
!2578 = !DILocation(line: 296, column: 0, scope: !500)
!2579 = !DILocation(line: 297, column: 0, scope: !502)
!2580 = !DILocation(line: 298, column: 0, scope: !504)
!2581 = !DILocation(line: 299, column: 0, scope: !506)
!2582 = !DILocation(line: 300, column: 0, scope: !508)
!2583 = !DILocation(line: 301, column: 0, scope: !510)
!2584 = !DILocation(line: 302, column: 0, scope: !512)
!2585 = !DILocation(line: 303, column: 0, scope: !514)
!2586 = !DILocation(line: 304, column: 0, scope: !516)
!2587 = !DILocation(line: 305, column: 0, scope: !518)
!2588 = !DILocation(line: 306, column: 0, scope: !520)
!2589 = !DILocation(line: 307, column: 0, scope: !522)
!2590 = !DILocation(line: 308, column: 0, scope: !524)
!2591 = !DILocation(line: 309, column: 0, scope: !526)
!2592 = !DILocation(line: 310, column: 0, scope: !528)
!2593 = !DILocation(line: 311, column: 0, scope: !530)
!2594 = !DILocation(line: 312, column: 0, scope: !532)
!2595 = !DILocation(line: 313, column: 0, scope: !534)
!2596 = !DILocation(line: 314, column: 0, scope: !536)
!2597 = !DILocation(line: 315, column: 0, scope: !538)
!2598 = !DILocation(line: 316, column: 0, scope: !540)
!2599 = !DILocation(line: 317, column: 0, scope: !542)
!2600 = !DILocation(line: 318, column: 0, scope: !544)
!2601 = !DILocation(line: 319, column: 0, scope: !546)
!2602 = !DILocation(line: 320, column: 0, scope: !548)
!2603 = !DILocation(line: 321, column: 0, scope: !550)
!2604 = !DILocation(line: 322, column: 0, scope: !552)
!2605 = !DILocation(line: 323, column: 0, scope: !554)
!2606 = !DILocation(line: 324, column: 0, scope: !556)
!2607 = !DILocation(line: 325, column: 0, scope: !558)
!2608 = !DILocation(line: 326, column: 0, scope: !560)
!2609 = !DILocation(line: 327, column: 0, scope: !562)
!2610 = !DILocation(line: 328, column: 0, scope: !564)
!2611 = !DILocation(line: 329, column: 0, scope: !566)
!2612 = !DILocation(line: 330, column: 0, scope: !568)
!2613 = !DILocation(line: 331, column: 0, scope: !570)
!2614 = !DILocation(line: 332, column: 0, scope: !572)
!2615 = !DILocation(line: 333, column: 0, scope: !574)
!2616 = !DILocation(line: 334, column: 0, scope: !576)
!2617 = !DILocation(line: 335, column: 0, scope: !578)
!2618 = !DILocation(line: 336, column: 0, scope: !580)
!2619 = !DILocation(line: 337, column: 0, scope: !582)
!2620 = !DILocation(line: 338, column: 0, scope: !584)
!2621 = !DILocation(line: 339, column: 0, scope: !586)
!2622 = !DILocation(line: 340, column: 0, scope: !588)
!2623 = !DILocation(line: 341, column: 0, scope: !590)
!2624 = !DILocation(line: 342, column: 0, scope: !592)
!2625 = !DILocation(line: 343, column: 0, scope: !594)
!2626 = !DILocation(line: 344, column: 0, scope: !596)
!2627 = !DILocation(line: 345, column: 0, scope: !598)
!2628 = !DILocation(line: 346, column: 0, scope: !600)
!2629 = !DILocation(line: 347, column: 0, scope: !602)
!2630 = !DILocation(line: 348, column: 0, scope: !604)
!2631 = !DILocation(line: 349, column: 0, scope: !606)
!2632 = !DILocation(line: 350, column: 0, scope: !608)
!2633 = !DILocation(line: 351, column: 0, scope: !610)
!2634 = !DILocation(line: 352, column: 0, scope: !612)
!2635 = !DILocation(line: 353, column: 0, scope: !614)
!2636 = !DILocation(line: 354, column: 0, scope: !616)
!2637 = !DILocation(line: 355, column: 0, scope: !618)
!2638 = !DILocation(line: 356, column: 0, scope: !620)
!2639 = !DILocation(line: 357, column: 0, scope: !622)
!2640 = !DILocation(line: 358, column: 0, scope: !624)
!2641 = !DILocation(line: 359, column: 0, scope: !626)
!2642 = !DILocation(line: 360, column: 0, scope: !628)
!2643 = !DILocation(line: 361, column: 0, scope: !630)
!2644 = !DILocation(line: 362, column: 0, scope: !632)
!2645 = !DILocation(line: 363, column: 0, scope: !634)
!2646 = !DILocation(line: 364, column: 0, scope: !636)
!2647 = !DILocation(line: 365, column: 0, scope: !638)
!2648 = !DILocation(line: 366, column: 0, scope: !640)
!2649 = !DILocation(line: 367, column: 0, scope: !642)
!2650 = !DILocation(line: 368, column: 0, scope: !644)
!2651 = !DILocation(line: 369, column: 0, scope: !646)
!2652 = !DILocation(line: 370, column: 0, scope: !648)
!2653 = !DILocation(line: 371, column: 0, scope: !650)
!2654 = !DILocation(line: 372, column: 0, scope: !652)
!2655 = !DILocation(line: 373, column: 0, scope: !654)
!2656 = !DILocation(line: 374, column: 0, scope: !656)
!2657 = !DILocation(line: 375, column: 0, scope: !658)
!2658 = !DILocation(line: 376, column: 0, scope: !660)
!2659 = !DILocation(line: 377, column: 0, scope: !662)
!2660 = !DILocation(line: 378, column: 0, scope: !664)
!2661 = !DILocation(line: 379, column: 0, scope: !666)
!2662 = !DILocation(line: 380, column: 0, scope: !668)
!2663 = !DILocation(line: 381, column: 0, scope: !670)
!2664 = !DILocation(line: 382, column: 0, scope: !672)
!2665 = !DILocation(line: 383, column: 0, scope: !674)
!2666 = !DILocation(line: 384, column: 0, scope: !676)
!2667 = !DILocation(line: 385, column: 0, scope: !678)
!2668 = !DILocation(line: 386, column: 0, scope: !680)
!2669 = !DILocation(line: 387, column: 0, scope: !682)
!2670 = !DILocation(line: 388, column: 0, scope: !684)
!2671 = !DILocation(line: 389, column: 0, scope: !686)
!2672 = !DILocation(line: 390, column: 0, scope: !688)
!2673 = !DILocation(line: 391, column: 0, scope: !690)
!2674 = !DILocation(line: 392, column: 0, scope: !692)
!2675 = !DILocation(line: 393, column: 0, scope: !694)
!2676 = !DILocation(line: 394, column: 0, scope: !696)
!2677 = !DILocation(line: 395, column: 0, scope: !698)
!2678 = !DILocation(line: 396, column: 0, scope: !700)
!2679 = !DILocation(line: 397, column: 0, scope: !702)
!2680 = !DILocation(line: 398, column: 0, scope: !704)
!2681 = !DILocation(line: 399, column: 0, scope: !706)
!2682 = !DILocation(line: 400, column: 0, scope: !708)
!2683 = !DILocation(line: 401, column: 0, scope: !710)
!2684 = !DILocation(line: 402, column: 0, scope: !712)
!2685 = !DILocation(line: 403, column: 0, scope: !714)
!2686 = !DILocation(line: 404, column: 0, scope: !716)
!2687 = !DILocation(line: 405, column: 0, scope: !718)
!2688 = !DILocation(line: 406, column: 0, scope: !720)
!2689 = !DILocation(line: 407, column: 0, scope: !722)
!2690 = !DILocation(line: 408, column: 0, scope: !724)
!2691 = !DILocation(line: 409, column: 0, scope: !726)
!2692 = !DILocation(line: 410, column: 0, scope: !728)
!2693 = !DILocation(line: 411, column: 0, scope: !730)
!2694 = !DILocation(line: 412, column: 0, scope: !732)
!2695 = !DILocation(line: 413, column: 0, scope: !734)
!2696 = !DILocation(line: 414, column: 0, scope: !736)
!2697 = !DILocation(line: 415, column: 0, scope: !738)
!2698 = !DILocation(line: 416, column: 0, scope: !740)
!2699 = !DILocation(line: 417, column: 0, scope: !742)
!2700 = !DILocation(line: 418, column: 0, scope: !744)
!2701 = !DILocation(line: 419, column: 0, scope: !746)
!2702 = !DILocation(line: 420, column: 0, scope: !748)
!2703 = !DILocation(line: 421, column: 0, scope: !750)
!2704 = !DILocation(line: 422, column: 0, scope: !752)
!2705 = !DILocation(line: 423, column: 0, scope: !754)
!2706 = !DILocation(line: 424, column: 0, scope: !756)
!2707 = !DILocation(line: 425, column: 0, scope: !758)
!2708 = !DILocation(line: 426, column: 0, scope: !760)
!2709 = !DILocation(line: 427, column: 0, scope: !762)
!2710 = !DILocation(line: 428, column: 0, scope: !764)
!2711 = !DILocation(line: 429, column: 0, scope: !766)
!2712 = !DILocation(line: 430, column: 0, scope: !768)
!2713 = !DILocation(line: 431, column: 0, scope: !770)
!2714 = !DILocation(line: 432, column: 0, scope: !772)
!2715 = !DILocation(line: 433, column: 0, scope: !774)
!2716 = !DILocation(line: 434, column: 0, scope: !776)
!2717 = !DILocation(line: 435, column: 0, scope: !778)
!2718 = !DILocation(line: 436, column: 0, scope: !780)
!2719 = !DILocation(line: 437, column: 0, scope: !782)
!2720 = !DILocation(line: 438, column: 0, scope: !784)
!2721 = !DILocation(line: 439, column: 0, scope: !786)
!2722 = !DILocation(line: 440, column: 0, scope: !788)
!2723 = !DILocation(line: 441, column: 0, scope: !790)
!2724 = !DILocation(line: 442, column: 0, scope: !792)
!2725 = !DILocation(line: 443, column: 0, scope: !794)
!2726 = !DILocation(line: 444, column: 0, scope: !796)
!2727 = !DILocation(line: 445, column: 0, scope: !798)
!2728 = !DILocation(line: 446, column: 0, scope: !800)
!2729 = !DILocation(line: 447, column: 0, scope: !802)
!2730 = !DILocation(line: 448, column: 0, scope: !804)
!2731 = !DILocation(line: 449, column: 0, scope: !806)
!2732 = !DILocation(line: 450, column: 0, scope: !808)
!2733 = !DILocation(line: 451, column: 0, scope: !810)
!2734 = !DILocation(line: 452, column: 0, scope: !812)
!2735 = !DILocation(line: 453, column: 0, scope: !814)
!2736 = !DILocation(line: 454, column: 0, scope: !816)
!2737 = !DILocation(line: 455, column: 0, scope: !818)
!2738 = !DILocation(line: 456, column: 0, scope: !820)
!2739 = !DILocation(line: 457, column: 0, scope: !822)
!2740 = !DILocation(line: 458, column: 0, scope: !824)
!2741 = !DILocation(line: 459, column: 0, scope: !826)
!2742 = !DILocation(line: 460, column: 0, scope: !828)
!2743 = !DILocation(line: 461, column: 0, scope: !830)
!2744 = !DILocation(line: 462, column: 0, scope: !832)
!2745 = !DILocation(line: 463, column: 0, scope: !834)
!2746 = !DILocation(line: 464, column: 0, scope: !836)
!2747 = !DILocation(line: 465, column: 0, scope: !838)
!2748 = !DILocation(line: 466, column: 0, scope: !840)
!2749 = !DILocation(line: 467, column: 0, scope: !842)
!2750 = !DILocation(line: 468, column: 0, scope: !844)
!2751 = !DILocation(line: 469, column: 0, scope: !846)
!2752 = !DILocation(line: 470, column: 0, scope: !848)
!2753 = !DILocation(line: 471, column: 0, scope: !850)
!2754 = !DILocation(line: 472, column: 0, scope: !852)
!2755 = !DILocation(line: 473, column: 0, scope: !854)
!2756 = !DILocation(line: 474, column: 0, scope: !856)
!2757 = !DILocation(line: 475, column: 0, scope: !858)
!2758 = !DILocation(line: 476, column: 0, scope: !860)
!2759 = !DILocation(line: 477, column: 0, scope: !862)
!2760 = !DILocation(line: 478, column: 0, scope: !864)
!2761 = !DILocation(line: 479, column: 0, scope: !866)
!2762 = !DILocation(line: 480, column: 0, scope: !868)
!2763 = !DILocation(line: 481, column: 0, scope: !870)
!2764 = !DILocation(line: 482, column: 0, scope: !872)
!2765 = !DILocation(line: 483, column: 0, scope: !874)
!2766 = !DILocation(line: 484, column: 0, scope: !876)
!2767 = !DILocation(line: 485, column: 0, scope: !878)
!2768 = !DILocation(line: 486, column: 0, scope: !880)
!2769 = !DILocation(line: 487, column: 0, scope: !882)
!2770 = !DILocation(line: 488, column: 0, scope: !884)
!2771 = !DILocation(line: 489, column: 0, scope: !886)
!2772 = !DILocation(line: 490, column: 0, scope: !888)
!2773 = !DILocation(line: 491, column: 0, scope: !890)
!2774 = !DILocation(line: 492, column: 0, scope: !892)
!2775 = !DILocation(line: 493, column: 0, scope: !894)
!2776 = !DILocation(line: 494, column: 0, scope: !896)
!2777 = !DILocation(line: 495, column: 0, scope: !898)
!2778 = !DILocation(line: 496, column: 0, scope: !900)
!2779 = !DILocation(line: 497, column: 0, scope: !902)
!2780 = !DILocation(line: 498, column: 0, scope: !904)
!2781 = !DILocation(line: 499, column: 0, scope: !906)
!2782 = !DILocation(line: 500, column: 0, scope: !908)
!2783 = !DILocation(line: 501, column: 0, scope: !910)
!2784 = !DILocation(line: 502, column: 0, scope: !912)
!2785 = !DILocation(line: 503, column: 0, scope: !914)
!2786 = !DILocation(line: 504, column: 0, scope: !916)
!2787 = !DILocation(line: 505, column: 0, scope: !918)
!2788 = !DILocation(line: 506, column: 0, scope: !920)
!2789 = !DILocation(line: 507, column: 0, scope: !922)
!2790 = !DILocation(line: 508, column: 0, scope: !924)
!2791 = !DILocation(line: 509, column: 0, scope: !926)
!2792 = !DILocation(line: 510, column: 0, scope: !928)
!2793 = !DILocation(line: 511, column: 0, scope: !930)
!2794 = !DILocation(line: 512, column: 0, scope: !932)
!2795 = !DILocation(line: 513, column: 0, scope: !934)
!2796 = !DILocation(line: 514, column: 0, scope: !936)
!2797 = !DILocation(line: 515, column: 0, scope: !938)
!2798 = !DILocation(line: 516, column: 0, scope: !940)
!2799 = !DILocation(line: 517, column: 0, scope: !942)
!2800 = !DILocation(line: 518, column: 0, scope: !944)
!2801 = !DILocation(line: 519, column: 0, scope: !946)
!2802 = !DILocation(line: 520, column: 0, scope: !948)
!2803 = !DILocation(line: 521, column: 0, scope: !950)
!2804 = !DILocation(line: 522, column: 0, scope: !952)
!2805 = !DILocation(line: 523, column: 0, scope: !954)
!2806 = !DILocation(line: 524, column: 0, scope: !956)
!2807 = !DILocation(line: 525, column: 0, scope: !958)
!2808 = !DILocation(line: 526, column: 0, scope: !960)
!2809 = !DILocation(line: 527, column: 0, scope: !962)
!2810 = !DILocation(line: 528, column: 0, scope: !964)
!2811 = !DILocation(line: 529, column: 0, scope: !966)
!2812 = !DILocation(line: 530, column: 0, scope: !968)
!2813 = !DILocation(line: 531, column: 0, scope: !970)
!2814 = !DILocation(line: 532, column: 0, scope: !972)
!2815 = !DILocation(line: 533, column: 0, scope: !974)
!2816 = !DILocation(line: 534, column: 0, scope: !976)
!2817 = !DILocation(line: 535, column: 0, scope: !978)
!2818 = !DILocation(line: 536, column: 0, scope: !980)
!2819 = !DILocation(line: 537, column: 0, scope: !982)
!2820 = !DILocation(line: 538, column: 0, scope: !984)
!2821 = !DILocation(line: 539, column: 0, scope: !986)
!2822 = !DILocation(line: 540, column: 0, scope: !988)
!2823 = !DILocation(line: 541, column: 0, scope: !990)
!2824 = !DILocation(line: 542, column: 0, scope: !992)
!2825 = !DILocation(line: 543, column: 0, scope: !994)
!2826 = !DILocation(line: 544, column: 0, scope: !996)
!2827 = !DILocation(line: 545, column: 0, scope: !998)
!2828 = !DILocation(line: 546, column: 0, scope: !1000)
!2829 = !DILocation(line: 547, column: 0, scope: !1002)
!2830 = !DILocation(line: 548, column: 0, scope: !1004)
!2831 = !DILocation(line: 549, column: 0, scope: !1006)
!2832 = !DILocation(line: 550, column: 0, scope: !1008)
!2833 = !DILocation(line: 551, column: 0, scope: !1010)
!2834 = !DILocation(line: 552, column: 0, scope: !1012)
!2835 = !DILocation(line: 553, column: 0, scope: !1014)
!2836 = !DILocation(line: 554, column: 0, scope: !1016)
!2837 = !DILocation(line: 555, column: 0, scope: !1018)
!2838 = !DILocation(line: 556, column: 0, scope: !1020)
!2839 = !DILocation(line: 557, column: 0, scope: !1022)
!2840 = !DILocation(line: 558, column: 0, scope: !1024)
!2841 = !DILocation(line: 559, column: 0, scope: !1026)
!2842 = !DILocation(line: 560, column: 0, scope: !1028)
!2843 = !DILocation(line: 561, column: 0, scope: !1030)
!2844 = !DILocation(line: 562, column: 0, scope: !1032)
!2845 = !DILocation(line: 563, column: 0, scope: !1034)
!2846 = !DILocation(line: 564, column: 0, scope: !1036)
!2847 = !DILocation(line: 565, column: 0, scope: !1038)
!2848 = !DILocation(line: 566, column: 0, scope: !1040)
!2849 = !DILocation(line: 567, column: 0, scope: !1042)
!2850 = !DILocation(line: 568, column: 0, scope: !1044)
!2851 = !DILocation(line: 569, column: 0, scope: !1046)
!2852 = !DILocation(line: 570, column: 0, scope: !1048)
!2853 = !DILocation(line: 571, column: 0, scope: !1050)
!2854 = !DILocation(line: 572, column: 0, scope: !1052)
!2855 = !DILocation(line: 573, column: 0, scope: !1054)
!2856 = !DILocation(line: 574, column: 0, scope: !1056)
!2857 = !DILocation(line: 575, column: 0, scope: !1058)
!2858 = !DILocation(line: 576, column: 0, scope: !1060)
!2859 = !DILocation(line: 577, column: 0, scope: !1062)
!2860 = !DILocation(line: 578, column: 0, scope: !1064)
!2861 = !DILocation(line: 579, column: 0, scope: !1066)
!2862 = !DILocation(line: 580, column: 0, scope: !1068)
!2863 = !DILocation(line: 581, column: 0, scope: !1070)
!2864 = !DILocation(line: 582, column: 0, scope: !1072)
!2865 = !DILocation(line: 583, column: 0, scope: !1074)
!2866 = !DILocation(line: 584, column: 0, scope: !1076)
!2867 = !DILocation(line: 585, column: 0, scope: !1078)
!2868 = !DILocation(line: 586, column: 0, scope: !1080)
!2869 = !DILocation(line: 587, column: 0, scope: !1082)
!2870 = !DILocation(line: 588, column: 0, scope: !1084)
!2871 = !DILocation(line: 589, column: 0, scope: !1086)
!2872 = !DILocation(line: 590, column: 0, scope: !1088)
!2873 = !DILocation(line: 591, column: 0, scope: !1090)
!2874 = !DILocation(line: 592, column: 0, scope: !1092)
!2875 = !DILocation(line: 593, column: 0, scope: !1094)
!2876 = !DILocation(line: 594, column: 0, scope: !1096)
!2877 = !DILocation(line: 595, column: 0, scope: !1098)
!2878 = !DILocation(line: 596, column: 0, scope: !1100)
!2879 = !DILocation(line: 597, column: 0, scope: !1102)
!2880 = !DILocation(line: 598, column: 0, scope: !1104)
!2881 = !DILocation(line: 599, column: 0, scope: !1106)
!2882 = !DILocation(line: 600, column: 0, scope: !1108)
!2883 = !DILocation(line: 601, column: 0, scope: !1110)
!2884 = !DILocation(line: 602, column: 0, scope: !1112)
!2885 = !DILocation(line: 603, column: 0, scope: !1114)
!2886 = !DILocation(line: 604, column: 0, scope: !1116)
!2887 = !DILocation(line: 605, column: 0, scope: !1118)
!2888 = !DILocation(line: 606, column: 0, scope: !1120)
!2889 = !DILocation(line: 607, column: 0, scope: !1122)
!2890 = !DILocation(line: 608, column: 0, scope: !1124)
!2891 = !DILocation(line: 609, column: 0, scope: !1126)
!2892 = !DILocation(line: 610, column: 0, scope: !1128)
!2893 = !DILocation(line: 611, column: 0, scope: !1130)
!2894 = !DILocation(line: 612, column: 0, scope: !1132)
!2895 = !DILocation(line: 613, column: 0, scope: !1134)
!2896 = !DILocation(line: 614, column: 0, scope: !1136)
!2897 = !DILocation(line: 615, column: 0, scope: !1138)
!2898 = !DILocation(line: 616, column: 0, scope: !1140)
!2899 = !DILocation(line: 617, column: 0, scope: !1142)
!2900 = !DILocation(line: 618, column: 0, scope: !1144)
!2901 = !DILocation(line: 619, column: 0, scope: !1146)
!2902 = !DILocation(line: 620, column: 0, scope: !1148)
!2903 = !DILocation(line: 621, column: 0, scope: !1150)
!2904 = !DILocation(line: 622, column: 0, scope: !1152)
!2905 = !DILocation(line: 623, column: 0, scope: !1154)
!2906 = !DILocation(line: 624, column: 0, scope: !1156)
!2907 = !DILocation(line: 625, column: 0, scope: !1158)
!2908 = !DILocation(line: 626, column: 0, scope: !1160)
!2909 = !DILocation(line: 627, column: 0, scope: !1162)
!2910 = !DILocation(line: 628, column: 0, scope: !1164)
!2911 = !DILocation(line: 629, column: 0, scope: !1166)
!2912 = !DILocation(line: 630, column: 0, scope: !1168)
!2913 = !DILocation(line: 631, column: 0, scope: !1170)
!2914 = !DILocation(line: 632, column: 0, scope: !1172)
!2915 = !DILocation(line: 633, column: 0, scope: !1174)
!3 = !DIFile(filename: "Plus.m3", directory: "../ARM64_DARWIN")
!4 = !DINamespace(name: "Plus", scope: !2)
!5 = !{null}
!6 = !DISubroutineType(types: !5)
!7 = !DIBasicType(name: "INTEGER", size: 64, encoding: DW_ATE_signed)
!8 = !DIBasicType(name: "CARDINAL", size: 64, encoding: DW_ATE_unsigned)
!9 = !DIBasicType(name: "INTEGER32", size: 32, encoding: DW_ATE_signed)
!10 = !DIBasicType(name: "CARDINAL32", size: 32, encoding: DW_ATE_unsigned)
!11 = !DIBasicType(name: "BOOLEAN", size: 1, encoding: DW_ATE_boolean)
!12 = !DIBasicType(name: "REAL", size: 32, encoding: DW_ATE_float)
!13 = !DIBasicType(name: "LONGREAL", size: 64, encoding: DW_ATE_float)
!14 = !DIBasicType(name: "CHAR", size: 8, encoding: DW_ATE_unsigned_char)
!15 = !DIBasicType(name: "ADDRESS", size: 64, encoding: DW_ATE_address)
!2 = distinct !DICompileUnit(language: DW_LANG_Modula3, file: !3, producer: "CM3 MSIR", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}
