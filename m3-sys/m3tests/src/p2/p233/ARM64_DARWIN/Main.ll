; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @RTHooks__ReportFault(ptr, i64)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define i1 @Main__CardinalLT0_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !168, metadata !DIExpression()), !dbg !358
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !169, metadata !DIExpression()), !dbg !358
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = icmp slt i64 %t1, 0
  ret i1 %t2
}

define i1 @Main__CardinalGE0_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !170, metadata !DIExpression()), !dbg !359
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !171, metadata !DIExpression()), !dbg !359
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = icmp sge i64 %t1, 0
  ret i1 %t2
}

define i1 @Main__CardinalLTNeg1_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !172, metadata !DIExpression()), !dbg !360
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !173, metadata !DIExpression()), !dbg !360
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp slt i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalLENeg1_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !174, metadata !DIExpression()), !dbg !361
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !175, metadata !DIExpression()), !dbg !361
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp sle i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalGTNeg1_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !176, metadata !DIExpression()), !dbg !362
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !177, metadata !DIExpression()), !dbg !362
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp sgt i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalGENeg1_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !178, metadata !DIExpression()), !dbg !363
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !179, metadata !DIExpression()), !dbg !363
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp sge i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalNENeg1_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !180, metadata !DIExpression()), !dbg !364
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !181, metadata !DIExpression()), !dbg !364
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp ne i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalEQNeg1_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !182, metadata !DIExpression()), !dbg !365
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !183, metadata !DIExpression()), !dbg !365
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 1
  %t3 = icmp eq i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalLTNeg2_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !184, metadata !DIExpression()), !dbg !366
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !185, metadata !DIExpression()), !dbg !366
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp slt i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalLENeg2_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !186, metadata !DIExpression()), !dbg !367
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !187, metadata !DIExpression()), !dbg !367
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp sle i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalGTNeg2_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !188, metadata !DIExpression()), !dbg !368
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !189, metadata !DIExpression()), !dbg !368
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp sgt i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalGENeg2_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !190, metadata !DIExpression()), !dbg !369
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !191, metadata !DIExpression()), !dbg !369
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp sge i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalNENeg2_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !192, metadata !DIExpression()), !dbg !370
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !193, metadata !DIExpression()), !dbg !370
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp ne i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__CardinalEQNeg2_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !194, metadata !DIExpression()), !dbg !371
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !195, metadata !DIExpression()), !dbg !371
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = sub i64 0, 2
  %t3 = icmp eq i64 %t1, %t2
  ret i1 %t3
}

define i1 @Main__LongcardLT0_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !44 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !196, metadata !DIExpression()), !dbg !372
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !197, metadata !DIExpression()), !dbg !372
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = icmp slt i64 %t1, 0
  ret i1 %t2
}

define i1 @Main__LongcardGE0_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !46 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !198, metadata !DIExpression()), !dbg !373
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !199, metadata !DIExpression()), !dbg !373
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  %t2 = icmp sge i64 %t1, 0
  ret i1 %t2
}

define i1 @Main__no_overlap_less_LT_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !48 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !200, metadata !DIExpression()), !dbg !374
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !201, metadata !DIExpression()), !dbg !374
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !202, metadata !DIExpression()), !dbg !374
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_LE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !50 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !203, metadata !DIExpression()), !dbg !375
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !204, metadata !DIExpression()), !dbg !375
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !205, metadata !DIExpression()), !dbg !375
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_GT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !52 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !206, metadata !DIExpression()), !dbg !376
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !207, metadata !DIExpression()), !dbg !376
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !208, metadata !DIExpression()), !dbg !376
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_GE_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !54 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !209, metadata !DIExpression()), !dbg !377
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !210, metadata !DIExpression()), !dbg !377
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !211, metadata !DIExpression()), !dbg !377
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_EQ_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !56 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !212, metadata !DIExpression()), !dbg !378
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !213, metadata !DIExpression()), !dbg !378
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !214, metadata !DIExpression()), !dbg !378
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp eq i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_NE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !58 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !215, metadata !DIExpression()), !dbg !379
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !216, metadata !DIExpression()), !dbg !379
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !217, metadata !DIExpression()), !dbg !379
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp ne i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_less_LE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !60 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !218, metadata !DIExpression()), !dbg !380
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !219, metadata !DIExpression()), !dbg !380
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !220, metadata !DIExpression()), !dbg !380
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_less_GT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !62 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !221, metadata !DIExpression()), !dbg !381
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !222, metadata !DIExpression()), !dbg !381
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !223, metadata !DIExpression()), !dbg !381
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_LT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !64 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !224, metadata !DIExpression()), !dbg !382
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !225, metadata !DIExpression()), !dbg !382
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !226, metadata !DIExpression()), !dbg !382
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_LE_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !66 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !227, metadata !DIExpression()), !dbg !383
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !228, metadata !DIExpression()), !dbg !383
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !229, metadata !DIExpression()), !dbg !383
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_GT_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !68 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !230, metadata !DIExpression()), !dbg !384
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !231, metadata !DIExpression()), !dbg !384
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !232, metadata !DIExpression()), !dbg !384
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_GE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !70 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !233, metadata !DIExpression()), !dbg !385
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !234, metadata !DIExpression()), !dbg !385
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !235, metadata !DIExpression()), !dbg !385
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_EQ_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !72 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !236, metadata !DIExpression()), !dbg !386
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !237, metadata !DIExpression()), !dbg !386
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !238, metadata !DIExpression()), !dbg !386
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp eq i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_NE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !74 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !239, metadata !DIExpression()), !dbg !387
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !240, metadata !DIExpression()), !dbg !387
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !241, metadata !DIExpression()), !dbg !387
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp ne i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_greater_LT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !76 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !242, metadata !DIExpression()), !dbg !388
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !243, metadata !DIExpression()), !dbg !388
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !244, metadata !DIExpression()), !dbg !388
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_greater_GE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !78 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !245, metadata !DIExpression()), !dbg !389
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !246, metadata !DIExpression()), !dbg !389
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !247, metadata !DIExpression()), !dbg !389
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_LT_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !80 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !248, metadata !DIExpression()), !dbg !390
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !249, metadata !DIExpression()), !dbg !390
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !250, metadata !DIExpression()), !dbg !390
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_LE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !82 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !251, metadata !DIExpression()), !dbg !391
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !252, metadata !DIExpression()), !dbg !391
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !253, metadata !DIExpression()), !dbg !391
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_GT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !84 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !254, metadata !DIExpression()), !dbg !392
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !255, metadata !DIExpression()), !dbg !392
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !256, metadata !DIExpression()), !dbg !392
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_GE_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !86 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !257, metadata !DIExpression()), !dbg !393
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !258, metadata !DIExpression()), !dbg !393
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !259, metadata !DIExpression()), !dbg !393
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_EQ_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !88 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !260, metadata !DIExpression()), !dbg !394
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !261, metadata !DIExpression()), !dbg !394
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !262, metadata !DIExpression()), !dbg !394
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp eq i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_less_enum_NE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !90 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !263, metadata !DIExpression()), !dbg !395
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !264, metadata !DIExpression()), !dbg !395
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !265, metadata !DIExpression()), !dbg !395
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp ne i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_less_enum_LE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !92 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !266, metadata !DIExpression()), !dbg !396
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !267, metadata !DIExpression()), !dbg !396
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !268, metadata !DIExpression()), !dbg !396
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_less_enum_GT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !94 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !269, metadata !DIExpression()), !dbg !397
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !270, metadata !DIExpression()), !dbg !397
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !271, metadata !DIExpression()), !dbg !397
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_LT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !96 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !272, metadata !DIExpression()), !dbg !398
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !273, metadata !DIExpression()), !dbg !398
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !274, metadata !DIExpression()), !dbg !398
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_LE_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !98 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !275, metadata !DIExpression()), !dbg !399
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !276, metadata !DIExpression()), !dbg !399
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !277, metadata !DIExpression()), !dbg !399
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_GT_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !100 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !278, metadata !DIExpression()), !dbg !400
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !279, metadata !DIExpression()), !dbg !400
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !280, metadata !DIExpression()), !dbg !400
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_GE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !102 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !281, metadata !DIExpression()), !dbg !401
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !282, metadata !DIExpression()), !dbg !401
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !283, metadata !DIExpression()), !dbg !401
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_EQ_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !104 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !284, metadata !DIExpression()), !dbg !402
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !285, metadata !DIExpression()), !dbg !402
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !286, metadata !DIExpression()), !dbg !402
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp eq i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__no_overlap_greater_enum_NE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !106 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !287, metadata !DIExpression()), !dbg !403
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !288, metadata !DIExpression()), !dbg !403
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !289, metadata !DIExpression()), !dbg !403
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp ne i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_greater_enum_LT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !108 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !290, metadata !DIExpression()), !dbg !404
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !291, metadata !DIExpression()), !dbg !404
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !292, metadata !DIExpression()), !dbg !404
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__minimum_overlap_greater_enum_GE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !110 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !293, metadata !DIExpression()), !dbg !405
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !294, metadata !DIExpression()), !dbg !405
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !295, metadata !DIExpression()), !dbg !405
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_LT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !112 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !296, metadata !DIExpression()), !dbg !406
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !297, metadata !DIExpression()), !dbg !406
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !298, metadata !DIExpression()), !dbg !406
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp slt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_LE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !114 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !299, metadata !DIExpression()), !dbg !407
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !300, metadata !DIExpression()), !dbg !407
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !301, metadata !DIExpression()), !dbg !407
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sle i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_GT_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !116 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !302, metadata !DIExpression()), !dbg !408
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !303, metadata !DIExpression()), !dbg !408
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !304, metadata !DIExpression()), !dbg !408
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sgt i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_GE_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !118 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !305, metadata !DIExpression()), !dbg !409
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !306, metadata !DIExpression()), !dbg !409
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !307, metadata !DIExpression()), !dbg !409
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp sge i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_EQ_true(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !120 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !308, metadata !DIExpression()), !dbg !410
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !309, metadata !DIExpression()), !dbg !410
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !310, metadata !DIExpression()), !dbg !410
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp eq i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__overlap_1_NE_false(i8 %a.a, i8 %a.b) personality ptr @__gxx_personality_v0 !dbg !122 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !311, metadata !DIExpression()), !dbg !411
  %b.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !312, metadata !DIExpression()), !dbg !411
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !313, metadata !DIExpression()), !dbg !411
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = zext i8 %a.b to i64
  store i64 %t2, ptr %b.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %a.slot
  %t4 = load i64, ptr %b.slot
  %t5 = icmp ne i64 %t3, %t4
  ret i1 %t5
}

define i1 @Main__ord_enum_vs_negative_LT_false(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !124 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !314, metadata !DIExpression()), !dbg !412
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !315, metadata !DIExpression()), !dbg !412
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp slt i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__ord_enum_vs_negative_LE_false(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !126 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !316, metadata !DIExpression()), !dbg !413
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !317, metadata !DIExpression()), !dbg !413
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp sle i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__ord_enum_vs_negative_GT_true(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !128 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !318, metadata !DIExpression()), !dbg !414
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !319, metadata !DIExpression()), !dbg !414
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp sgt i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__ord_enum_vs_negative_GE_true(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !130 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !320, metadata !DIExpression()), !dbg !415
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !321, metadata !DIExpression()), !dbg !415
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp sge i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__ord_enum_vs_negative_EQ_false(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !132 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !322, metadata !DIExpression()), !dbg !416
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !323, metadata !DIExpression()), !dbg !416
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp eq i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__ord_enum_vs_negative_NE_true(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !134 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !324, metadata !DIExpression()), !dbg !417
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !325, metadata !DIExpression()), !dbg !417
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t2 = load i64, ptr %a.slot
  %t3 = sub i64 0, 1
  %t4 = icmp ne i64 %t2, %t3
  ret i1 %t4
}

define i1 @Main__abs_vs_negative_LT_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !136 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !326, metadata !DIExpression()), !dbg !418
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !327, metadata !DIExpression()), !dbg !418
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp slt i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_negative_LE_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !138 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !328, metadata !DIExpression()), !dbg !419
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !329, metadata !DIExpression()), !dbg !419
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp sle i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_negative_GT_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !140 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !330, metadata !DIExpression()), !dbg !420
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !331, metadata !DIExpression()), !dbg !420
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp sgt i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_negative_GE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !142 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !332, metadata !DIExpression()), !dbg !421
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !333, metadata !DIExpression()), !dbg !421
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp sge i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_negative_EQ_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !144 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !334, metadata !DIExpression()), !dbg !422
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !335, metadata !DIExpression()), !dbg !422
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp eq i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_negative_NE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !146 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !336, metadata !DIExpression()), !dbg !423
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !337, metadata !DIExpression()), !dbg !423
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, 1
  %t7 = icmp ne i64 %t5, %t6
  ret i1 %t7
}

define i1 @Main__abs_vs_zero_LT_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !148 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !338, metadata !DIExpression()), !dbg !424
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !339, metadata !DIExpression()), !dbg !424
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = icmp slt i64 %t5, 0
  ret i1 %t6
}

define i1 @Main__abs_vs_zero_GE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !150 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !340, metadata !DIExpression()), !dbg !425
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !341, metadata !DIExpression()), !dbg !425
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = icmp sge i64 %t5, 0
  ret i1 %t6
}

define i1 @Main__neg_abs_vs_zero_LE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !152 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !342, metadata !DIExpression()), !dbg !426
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !343, metadata !DIExpression()), !dbg !426
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp sle i64 %t6, 0
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_zero_GT_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !154 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !344, metadata !DIExpression()), !dbg !427
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !345, metadata !DIExpression()), !dbg !427
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp sgt i64 %t6, 0
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_LT_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !156 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !346, metadata !DIExpression()), !dbg !428
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !347, metadata !DIExpression()), !dbg !428
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp slt i64 %t6, 1
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_LE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !158 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !348, metadata !DIExpression()), !dbg !429
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !349, metadata !DIExpression()), !dbg !429
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp sle i64 %t6, 1
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_GT_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !160 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !350, metadata !DIExpression()), !dbg !430
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !351, metadata !DIExpression()), !dbg !430
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp sgt i64 %t6, 1
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_GE_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !162 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !352, metadata !DIExpression()), !dbg !431
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !353, metadata !DIExpression()), !dbg !431
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp sge i64 %t6, 1
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_EQ_false(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !164 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !354, metadata !DIExpression()), !dbg !432
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !355, metadata !DIExpression()), !dbg !432
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp eq i64 %t6, 1
  ret i1 %t7
}

define i1 @Main__neg_abs_vs_one_NE_true(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !166 {
entry:
  %t2 = alloca i64
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !356, metadata !DIExpression()), !dbg !433
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !357, metadata !DIExpression()), !dbg !433
  store i64 %a.a, ptr %a.slot
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr %a.slot
  store i64 %t1, ptr %t2
  %t3 = icmp slt i64 %t1, 0
  br i1 %t3, label %abs.neg.1, label %abs.merge.2
abs.neg.1:
  %t4 = sub i64 0, %t1
  store i64 %t4, ptr %t2
  br label %abs.merge.2
abs.merge.2:
  %t5 = load i64, ptr %t2
  %t6 = sub i64 0, %t5
  %t7 = icmp ne i64 %t6, 1
  ret i1 %t7
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = call i1 @Main__CardinalLT0_false(i64 0)
  %t2 = icmp eq i1 %t1, 0
  %t3 = icmp eq i1 %t2, 0
  br i1 %t3, label %check.fault.1, label %check.cont.2
check.fault.1:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5376)
  unreachable
check.cont.2:
  %t4 = call i1 @Main__CardinalGE0_true(i64 0)
  %t5 = icmp eq i1 %t4, 0
  br i1 %t5, label %check.fault.3, label %check.cont.4
check.fault.3:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5408)
  unreachable
check.cont.4:
  %t6 = call i1 @Main__CardinalLTNeg1_false(i64 0)
  %t7 = icmp eq i1 %t6, 0
  %t8 = icmp eq i1 %t7, 0
  br i1 %t8, label %check.fault.5, label %check.cont.6
check.fault.5:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5440)
  unreachable
check.cont.6:
  %t9 = call i1 @Main__CardinalLENeg1_false(i64 0)
  %t10 = icmp eq i1 %t9, 0
  %t11 = icmp eq i1 %t10, 0
  br i1 %t11, label %check.fault.7, label %check.cont.8
check.fault.7:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5472)
  unreachable
check.cont.8:
  %t12 = call i1 @Main__CardinalGTNeg1_true(i64 0)
  %t13 = icmp eq i1 %t12, 0
  br i1 %t13, label %check.fault.9, label %check.cont.10
check.fault.9:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5504)
  unreachable
check.cont.10:
  %t14 = call i1 @Main__CardinalGENeg1_true(i64 0)
  %t15 = icmp eq i1 %t14, 0
  br i1 %t15, label %check.fault.11, label %check.cont.12
check.fault.11:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5536)
  unreachable
check.cont.12:
  %t16 = call i1 @Main__CardinalNENeg1_true(i64 0)
  %t17 = icmp eq i1 %t16, 0
  br i1 %t17, label %check.fault.13, label %check.cont.14
check.fault.13:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5568)
  unreachable
check.cont.14:
  %t18 = call i1 @Main__CardinalEQNeg1_false(i64 0)
  %t19 = icmp eq i1 %t18, 0
  %t20 = icmp eq i1 %t19, 0
  br i1 %t20, label %check.fault.15, label %check.cont.16
check.fault.15:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5600)
  unreachable
check.cont.16:
  %t21 = call i1 @Main__CardinalLTNeg2_false(i64 0)
  %t22 = icmp eq i1 %t21, 0
  %t23 = icmp eq i1 %t22, 0
  br i1 %t23, label %check.fault.17, label %check.cont.18
check.fault.17:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5632)
  unreachable
check.cont.18:
  %t24 = call i1 @Main__CardinalLENeg2_false(i64 0)
  %t25 = icmp eq i1 %t24, 0
  %t26 = icmp eq i1 %t25, 0
  br i1 %t26, label %check.fault.19, label %check.cont.20
check.fault.19:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5664)
  unreachable
check.cont.20:
  %t27 = call i1 @Main__CardinalGTNeg2_true(i64 0)
  %t28 = icmp eq i1 %t27, 0
  br i1 %t28, label %check.fault.21, label %check.cont.22
check.fault.21:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5696)
  unreachable
check.cont.22:
  %t29 = call i1 @Main__CardinalGENeg2_true(i64 0)
  %t30 = icmp eq i1 %t29, 0
  br i1 %t30, label %check.fault.23, label %check.cont.24
check.fault.23:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5728)
  unreachable
check.cont.24:
  %t31 = call i1 @Main__CardinalNENeg2_true(i64 0)
  %t32 = icmp eq i1 %t31, 0
  br i1 %t32, label %check.fault.25, label %check.cont.26
check.fault.25:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5760)
  unreachable
check.cont.26:
  %t33 = call i1 @Main__CardinalEQNeg2_false(i64 0)
  %t34 = icmp eq i1 %t33, 0
  %t35 = icmp eq i1 %t34, 0
  br i1 %t35, label %check.fault.27, label %check.cont.28
check.fault.27:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5792)
  unreachable
check.cont.28:
  %t36 = call i1 @Main__LongcardLT0_false(i64 0)
  %t37 = icmp eq i1 %t36, 0
  %t38 = icmp eq i1 %t37, 0
  br i1 %t38, label %check.fault.29, label %check.cont.30
check.fault.29:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5824)
  unreachable
check.cont.30:
  %t39 = call i1 @Main__LongcardGE0_true(i64 0)
  %t40 = icmp eq i1 %t39, 0
  br i1 %t40, label %check.fault.31, label %check.cont.32
check.fault.31:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5856)
  unreachable
check.cont.32:
  %t41 = trunc i64 0 to i8
  %t42 = trunc i64 2 to i8
  %t43 = call i1 @Main__no_overlap_less_LT_true(i8 %t41, i8 %t42)
  %t44 = icmp eq i1 %t43, 0
  br i1 %t44, label %check.fault.33, label %check.cont.34
check.fault.33:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5888)
  unreachable
check.cont.34:
  %t45 = trunc i64 0 to i8
  %t46 = trunc i64 2 to i8
  %t47 = call i1 @Main__no_overlap_less_LE_true(i8 %t45, i8 %t46)
  %t48 = icmp eq i1 %t47, 0
  br i1 %t48, label %check.fault.35, label %check.cont.36
check.fault.35:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5920)
  unreachable
check.cont.36:
  %t49 = trunc i64 0 to i8
  %t50 = trunc i64 2 to i8
  %t51 = call i1 @Main__no_overlap_less_GT_false(i8 %t49, i8 %t50)
  %t52 = icmp eq i1 %t51, 0
  %t53 = icmp eq i1 %t52, 0
  br i1 %t53, label %check.fault.37, label %check.cont.38
check.fault.37:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5952)
  unreachable
check.cont.38:
  %t54 = trunc i64 0 to i8
  %t55 = trunc i64 2 to i8
  %t56 = call i1 @Main__no_overlap_less_GE_false(i8 %t54, i8 %t55)
  %t57 = icmp eq i1 %t56, 0
  %t58 = icmp eq i1 %t57, 0
  br i1 %t58, label %check.fault.39, label %check.cont.40
check.fault.39:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5984)
  unreachable
check.cont.40:
  %t59 = trunc i64 0 to i8
  %t60 = trunc i64 2 to i8
  %t61 = call i1 @Main__no_overlap_less_EQ_false(i8 %t59, i8 %t60)
  %t62 = icmp eq i1 %t61, 0
  %t63 = icmp eq i1 %t62, 0
  br i1 %t63, label %check.fault.41, label %check.cont.42
check.fault.41:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6016)
  unreachable
check.cont.42:
  %t64 = trunc i64 0 to i8
  %t65 = trunc i64 2 to i8
  %t66 = call i1 @Main__no_overlap_less_NE_true(i8 %t64, i8 %t65)
  %t67 = icmp eq i1 %t66, 0
  br i1 %t67, label %check.fault.43, label %check.cont.44
check.fault.43:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6048)
  unreachable
check.cont.44:
  %t68 = trunc i64 0 to i8
  %t69 = trunc i64 1 to i8
  %t70 = call i1 @Main__minimum_overlap_less_LE_true(i8 %t68, i8 %t69)
  %t71 = icmp eq i1 %t70, 0
  br i1 %t71, label %check.fault.45, label %check.cont.46
check.fault.45:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6080)
  unreachable
check.cont.46:
  %t72 = trunc i64 0 to i8
  %t73 = trunc i64 1 to i8
  %t74 = call i1 @Main__minimum_overlap_less_GT_false(i8 %t72, i8 %t73)
  %t75 = icmp eq i1 %t74, 0
  %t76 = icmp eq i1 %t75, 0
  br i1 %t76, label %check.fault.47, label %check.cont.48
check.fault.47:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6112)
  unreachable
check.cont.48:
  %t77 = trunc i64 2 to i8
  %t78 = trunc i64 0 to i8
  %t79 = call i1 @Main__no_overlap_greater_LT_false(i8 %t77, i8 %t78)
  %t80 = icmp eq i1 %t79, 0
  %t81 = icmp eq i1 %t80, 0
  br i1 %t81, label %check.fault.49, label %check.cont.50
check.fault.49:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6144)
  unreachable
check.cont.50:
  %t82 = trunc i64 2 to i8
  %t83 = trunc i64 0 to i8
  %t84 = call i1 @Main__no_overlap_greater_LE_false(i8 %t82, i8 %t83)
  %t85 = icmp eq i1 %t84, 0
  %t86 = icmp eq i1 %t85, 0
  br i1 %t86, label %check.fault.51, label %check.cont.52
check.fault.51:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6176)
  unreachable
check.cont.52:
  %t87 = trunc i64 2 to i8
  %t88 = trunc i64 0 to i8
  %t89 = call i1 @Main__no_overlap_greater_GT_true(i8 %t87, i8 %t88)
  %t90 = icmp eq i1 %t89, 0
  br i1 %t90, label %check.fault.53, label %check.cont.54
check.fault.53:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6208)
  unreachable
check.cont.54:
  %t91 = trunc i64 2 to i8
  %t92 = trunc i64 0 to i8
  %t93 = call i1 @Main__no_overlap_greater_GE_true(i8 %t91, i8 %t92)
  %t94 = icmp eq i1 %t93, 0
  br i1 %t94, label %check.fault.55, label %check.cont.56
check.fault.55:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6240)
  unreachable
check.cont.56:
  %t95 = trunc i64 2 to i8
  %t96 = trunc i64 0 to i8
  %t97 = call i1 @Main__no_overlap_greater_EQ_false(i8 %t95, i8 %t96)
  %t98 = icmp eq i1 %t97, 0
  %t99 = icmp eq i1 %t98, 0
  br i1 %t99, label %check.fault.57, label %check.cont.58
check.fault.57:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6272)
  unreachable
check.cont.58:
  %t100 = trunc i64 2 to i8
  %t101 = trunc i64 0 to i8
  %t102 = call i1 @Main__no_overlap_greater_NE_true(i8 %t100, i8 %t101)
  %t103 = icmp eq i1 %t102, 0
  br i1 %t103, label %check.fault.59, label %check.cont.60
check.fault.59:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6304)
  unreachable
check.cont.60:
  %t104 = trunc i64 1 to i8
  %t105 = trunc i64 0 to i8
  %t106 = call i1 @Main__minimum_overlap_greater_LT_false(i8 %t104, i8 %t105)
  %t107 = icmp eq i1 %t106, 0
  %t108 = icmp eq i1 %t107, 0
  br i1 %t108, label %check.fault.61, label %check.cont.62
check.fault.61:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6336)
  unreachable
check.cont.62:
  %t109 = trunc i64 1 to i8
  %t110 = trunc i64 0 to i8
  %t111 = call i1 @Main__minimum_overlap_greater_GE_true(i8 %t109, i8 %t110)
  %t112 = icmp eq i1 %t111, 0
  br i1 %t112, label %check.fault.63, label %check.cont.64
check.fault.63:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6368)
  unreachable
check.cont.64:
  %t113 = call i1 @Main__no_overlap_less_enum_LT_true(i8 0, i8 4)
  %t114 = icmp eq i1 %t113, 0
  br i1 %t114, label %check.fault.65, label %check.cont.66
check.fault.65:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6400)
  unreachable
check.cont.66:
  %t115 = call i1 @Main__no_overlap_less_enum_LE_true(i8 0, i8 4)
  %t116 = icmp eq i1 %t115, 0
  br i1 %t116, label %check.fault.67, label %check.cont.68
check.fault.67:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6432)
  unreachable
check.cont.68:
  %t117 = call i1 @Main__no_overlap_less_enum_GT_false(i8 0, i8 4)
  %t118 = icmp eq i1 %t117, 0
  %t119 = icmp eq i1 %t118, 0
  br i1 %t119, label %check.fault.69, label %check.cont.70
check.fault.69:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6464)
  unreachable
check.cont.70:
  %t120 = call i1 @Main__no_overlap_less_enum_GE_false(i8 0, i8 4)
  %t121 = icmp eq i1 %t120, 0
  %t122 = icmp eq i1 %t121, 0
  br i1 %t122, label %check.fault.71, label %check.cont.72
check.fault.71:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6496)
  unreachable
check.cont.72:
  %t123 = call i1 @Main__no_overlap_less_enum_EQ_false(i8 0, i8 4)
  %t124 = icmp eq i1 %t123, 0
  %t125 = icmp eq i1 %t124, 0
  br i1 %t125, label %check.fault.73, label %check.cont.74
check.fault.73:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6528)
  unreachable
check.cont.74:
  %t126 = call i1 @Main__no_overlap_less_enum_NE_true(i8 0, i8 4)
  %t127 = icmp eq i1 %t126, 0
  br i1 %t127, label %check.fault.75, label %check.cont.76
check.fault.75:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6560)
  unreachable
check.cont.76:
  %t128 = call i1 @Main__minimum_overlap_less_enum_LE_true(i8 0, i8 1)
  %t129 = icmp eq i1 %t128, 0
  br i1 %t129, label %check.fault.77, label %check.cont.78
check.fault.77:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6592)
  unreachable
check.cont.78:
  %t130 = call i1 @Main__minimum_overlap_less_enum_GT_false(i8 0, i8 1)
  %t131 = icmp eq i1 %t130, 0
  %t132 = icmp eq i1 %t131, 0
  br i1 %t132, label %check.fault.79, label %check.cont.80
check.fault.79:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6624)
  unreachable
check.cont.80:
  %t133 = call i1 @Main__no_overlap_greater_enum_LT_false(i8 4, i8 0)
  %t134 = icmp eq i1 %t133, 0
  %t135 = icmp eq i1 %t134, 0
  br i1 %t135, label %check.fault.81, label %check.cont.82
check.fault.81:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6656)
  unreachable
check.cont.82:
  %t136 = call i1 @Main__no_overlap_greater_enum_LE_false(i8 4, i8 0)
  %t137 = icmp eq i1 %t136, 0
  %t138 = icmp eq i1 %t137, 0
  br i1 %t138, label %check.fault.83, label %check.cont.84
check.fault.83:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6688)
  unreachable
check.cont.84:
  %t139 = call i1 @Main__no_overlap_greater_enum_GT_true(i8 4, i8 0)
  %t140 = icmp eq i1 %t139, 0
  br i1 %t140, label %check.fault.85, label %check.cont.86
check.fault.85:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6720)
  unreachable
check.cont.86:
  %t141 = call i1 @Main__no_overlap_greater_enum_GE_true(i8 4, i8 0)
  %t142 = icmp eq i1 %t141, 0
  br i1 %t142, label %check.fault.87, label %check.cont.88
check.fault.87:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6752)
  unreachable
check.cont.88:
  %t143 = call i1 @Main__no_overlap_greater_enum_EQ_false(i8 4, i8 0)
  %t144 = icmp eq i1 %t143, 0
  %t145 = icmp eq i1 %t144, 0
  br i1 %t145, label %check.fault.89, label %check.cont.90
check.fault.89:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6784)
  unreachable
check.cont.90:
  %t146 = call i1 @Main__no_overlap_greater_enum_NE_true(i8 4, i8 0)
  %t147 = icmp eq i1 %t146, 0
  br i1 %t147, label %check.fault.91, label %check.cont.92
check.fault.91:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6816)
  unreachable
check.cont.92:
  %t148 = call i1 @Main__minimum_overlap_greater_enum_LT_false(i8 1, i8 0)
  %t149 = icmp eq i1 %t148, 0
  %t150 = icmp eq i1 %t149, 0
  br i1 %t150, label %check.fault.93, label %check.cont.94
check.fault.93:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6848)
  unreachable
check.cont.94:
  %t151 = call i1 @Main__minimum_overlap_greater_enum_GE_true(i8 1, i8 0)
  %t152 = icmp eq i1 %t151, 0
  br i1 %t152, label %check.fault.95, label %check.cont.96
check.fault.95:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6880)
  unreachable
check.cont.96:
  %t153 = trunc i64 0 to i8
  %t154 = trunc i64 0 to i8
  %t155 = call i1 @Main__overlap_1_LT_false(i8 %t153, i8 %t154)
  %t156 = icmp eq i1 %t155, 0
  %t157 = icmp eq i1 %t156, 0
  br i1 %t157, label %check.fault.97, label %check.cont.98
check.fault.97:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6912)
  unreachable
check.cont.98:
  %t158 = trunc i64 0 to i8
  %t159 = trunc i64 0 to i8
  %t160 = call i1 @Main__overlap_1_LE_true(i8 %t158, i8 %t159)
  %t161 = icmp eq i1 %t160, 0
  br i1 %t161, label %check.fault.99, label %check.cont.100
check.fault.99:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6944)
  unreachable
check.cont.100:
  %t162 = trunc i64 0 to i8
  %t163 = trunc i64 0 to i8
  %t164 = call i1 @Main__overlap_1_GT_false(i8 %t162, i8 %t163)
  %t165 = icmp eq i1 %t164, 0
  %t166 = icmp eq i1 %t165, 0
  br i1 %t166, label %check.fault.101, label %check.cont.102
check.fault.101:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6976)
  unreachable
check.cont.102:
  %t167 = trunc i64 0 to i8
  %t168 = trunc i64 0 to i8
  %t169 = call i1 @Main__overlap_1_GE_true(i8 %t167, i8 %t168)
  %t170 = icmp eq i1 %t169, 0
  br i1 %t170, label %check.fault.103, label %check.cont.104
check.fault.103:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7008)
  unreachable
check.cont.104:
  %t171 = trunc i64 0 to i8
  %t172 = trunc i64 0 to i8
  %t173 = call i1 @Main__overlap_1_EQ_true(i8 %t171, i8 %t172)
  %t174 = icmp eq i1 %t173, 0
  br i1 %t174, label %check.fault.105, label %check.cont.106
check.fault.105:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7040)
  unreachable
check.cont.106:
  %t175 = trunc i64 0 to i8
  %t176 = trunc i64 0 to i8
  %t177 = call i1 @Main__overlap_1_NE_false(i8 %t175, i8 %t176)
  %t178 = icmp eq i1 %t177, 0
  %t179 = icmp eq i1 %t178, 0
  br i1 %t179, label %check.fault.107, label %check.cont.108
check.fault.107:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7072)
  unreachable
check.cont.108:
  %t180 = call i1 @Main__ord_enum_vs_negative_LT_false(i8 0)
  %t181 = icmp eq i1 %t180, 0
  %t182 = icmp eq i1 %t181, 0
  br i1 %t182, label %check.fault.109, label %check.cont.110
check.fault.109:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7104)
  unreachable
check.cont.110:
  %t183 = call i1 @Main__ord_enum_vs_negative_LE_false(i8 0)
  %t184 = icmp eq i1 %t183, 0
  %t185 = icmp eq i1 %t184, 0
  br i1 %t185, label %check.fault.111, label %check.cont.112
check.fault.111:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7136)
  unreachable
check.cont.112:
  %t186 = call i1 @Main__ord_enum_vs_negative_GT_true(i8 0)
  %t187 = icmp eq i1 %t186, 0
  br i1 %t187, label %check.fault.113, label %check.cont.114
check.fault.113:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7168)
  unreachable
check.cont.114:
  %t188 = call i1 @Main__ord_enum_vs_negative_GE_true(i8 0)
  %t189 = icmp eq i1 %t188, 0
  br i1 %t189, label %check.fault.115, label %check.cont.116
check.fault.115:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7200)
  unreachable
check.cont.116:
  %t190 = call i1 @Main__ord_enum_vs_negative_EQ_false(i8 0)
  %t191 = icmp eq i1 %t190, 0
  %t192 = icmp eq i1 %t191, 0
  br i1 %t192, label %check.fault.117, label %check.cont.118
check.fault.117:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7232)
  unreachable
check.cont.118:
  %t193 = call i1 @Main__ord_enum_vs_negative_NE_true(i8 0)
  %t194 = icmp eq i1 %t193, 0
  br i1 %t194, label %check.fault.119, label %check.cont.120
check.fault.119:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7264)
  unreachable
check.cont.120:
  %t195 = call i1 @Main__abs_vs_negative_LT_false(i64 0)
  %t196 = icmp eq i1 %t195, 0
  %t197 = icmp eq i1 %t196, 0
  br i1 %t197, label %check.fault.121, label %check.cont.122
check.fault.121:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7296)
  unreachable
check.cont.122:
  %t198 = call i1 @Main__abs_vs_negative_LE_false(i64 0)
  %t199 = icmp eq i1 %t198, 0
  %t200 = icmp eq i1 %t199, 0
  br i1 %t200, label %check.fault.123, label %check.cont.124
check.fault.123:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7328)
  unreachable
check.cont.124:
  %t201 = call i1 @Main__abs_vs_negative_GT_true(i64 0)
  %t202 = icmp eq i1 %t201, 0
  br i1 %t202, label %check.fault.125, label %check.cont.126
check.fault.125:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7360)
  unreachable
check.cont.126:
  %t203 = call i1 @Main__abs_vs_negative_GE_true(i64 0)
  %t204 = icmp eq i1 %t203, 0
  br i1 %t204, label %check.fault.127, label %check.cont.128
check.fault.127:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7392)
  unreachable
check.cont.128:
  %t205 = call i1 @Main__abs_vs_negative_EQ_false(i64 0)
  %t206 = icmp eq i1 %t205, 0
  %t207 = icmp eq i1 %t206, 0
  br i1 %t207, label %check.fault.129, label %check.cont.130
check.fault.129:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7424)
  unreachable
check.cont.130:
  %t208 = call i1 @Main__abs_vs_negative_NE_true(i64 0)
  %t209 = icmp eq i1 %t208, 0
  br i1 %t209, label %check.fault.131, label %check.cont.132
check.fault.131:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7456)
  unreachable
check.cont.132:
  %t210 = call i1 @Main__abs_vs_zero_LT_false(i64 0)
  %t211 = icmp eq i1 %t210, 0
  %t212 = icmp eq i1 %t211, 0
  br i1 %t212, label %check.fault.133, label %check.cont.134
check.fault.133:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7488)
  unreachable
check.cont.134:
  %t213 = call i1 @Main__abs_vs_zero_GE_true(i64 0)
  %t214 = icmp eq i1 %t213, 0
  br i1 %t214, label %check.fault.135, label %check.cont.136
check.fault.135:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7520)
  unreachable
check.cont.136:
  %t215 = call i1 @Main__neg_abs_vs_zero_LE_true(i64 0)
  %t216 = icmp eq i1 %t215, 0
  br i1 %t216, label %check.fault.137, label %check.cont.138
check.fault.137:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7552)
  unreachable
check.cont.138:
  %t217 = call i1 @Main__neg_abs_vs_zero_GT_false(i64 0)
  %t218 = icmp eq i1 %t217, 0
  %t219 = icmp eq i1 %t218, 0
  br i1 %t219, label %check.fault.139, label %check.cont.140
check.fault.139:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7584)
  unreachable
check.cont.140:
  %t220 = call i1 @Main__neg_abs_vs_one_LT_true(i64 0)
  %t221 = icmp eq i1 %t220, 0
  br i1 %t221, label %check.fault.141, label %check.cont.142
check.fault.141:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7616)
  unreachable
check.cont.142:
  %t222 = call i1 @Main__neg_abs_vs_one_LE_true(i64 0)
  %t223 = icmp eq i1 %t222, 0
  br i1 %t223, label %check.fault.143, label %check.cont.144
check.fault.143:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7648)
  unreachable
check.cont.144:
  %t224 = call i1 @Main__neg_abs_vs_one_GT_false(i64 0)
  %t225 = icmp eq i1 %t224, 0
  %t226 = icmp eq i1 %t225, 0
  br i1 %t226, label %check.fault.145, label %check.cont.146
check.fault.145:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7680)
  unreachable
check.cont.146:
  %t227 = call i1 @Main__neg_abs_vs_one_GE_false(i64 0)
  %t228 = icmp eq i1 %t227, 0
  %t229 = icmp eq i1 %t228, 0
  br i1 %t229, label %check.fault.147, label %check.cont.148
check.fault.147:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7712)
  unreachable
check.cont.148:
  %t230 = call i1 @Main__neg_abs_vs_one_EQ_false(i64 0)
  %t231 = icmp eq i1 %t230, 0
  %t232 = icmp eq i1 %t231, 0
  br i1 %t232, label %check.fault.149, label %check.cont.150
check.fault.149:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7744)
  unreachable
check.cont.150:
  %t233 = call i1 @Main__neg_abs_vs_one_NE_true(i64 0)
  %t234 = icmp eq i1 %t233, 0
  br i1 %t234, label %check.fault.151, label %check.cont.152
check.fault.151:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7776)
  unreachable
check.cont.152:
  ret void
}

; TEXT literal globals
@textlit_methods = internal constant [5 x ptr] [
  ptr @RTHooks__TextLitInfo,
  ptr @RTHooks__TextLitGetChar,
  ptr @RTHooks__TextLitGetWideChar,
  ptr @RTHooks__TextLitGetChars,
  ptr @RTHooks__TextLitGetWideChars
]
@textlit_0 = internal constant { i64, ptr, i64, [26 x i8] } { i64 2, ptr @textlit_methods, i64 25, [26 x i8] c"NOT CardinalLT0_false(0) \00" }
@textlit_1 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c"CardinalGE0_true(0) \00" }
@textlit_2 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalLTNeg1_false(0) \00" }
@textlit_3 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalLENeg1_false(0) \00" }
@textlit_4 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalGTNeg1_true(0) \00" }
@textlit_5 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalGENeg1_true(0) \00" }
@textlit_6 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalNENeg1_true(0) \00" }
@textlit_7 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalEQNeg1_false(0) \00" }
@textlit_8 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalLTNeg2_false(0) \00" }
@textlit_9 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalLENeg2_false(0) \00" }
@textlit_10 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalGTNeg2_true(0) \00" }
@textlit_11 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalGENeg2_true(0) \00" }
@textlit_12 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"CardinalNENeg2_true(0) \00" }
@textlit_13 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT CardinalEQNeg2_false(0) \00" }
@textlit_14 = internal constant { i64, ptr, i64, [27 x i8] } { i64 2, ptr @textlit_methods, i64 26, [27 x i8] c"NOT LongcardLT0_false(0L) \00" }
@textlit_15 = internal constant { i64, ptr, i64, [22 x i8] } { i64 2, ptr @textlit_methods, i64 21, [22 x i8] c"LongcardGE0_true(0L) \00" }
@textlit_16 = internal constant { i64, ptr, i64, [30 x i8] } { i64 2, ptr @textlit_methods, i64 29, [30 x i8] c"no_overlap_less_LT_true(0,2) \00" }
@textlit_17 = internal constant { i64, ptr, i64, [30 x i8] } { i64 2, ptr @textlit_methods, i64 29, [30 x i8] c"no_overlap_less_LE_true(0,2) \00" }
@textlit_18 = internal constant { i64, ptr, i64, [35 x i8] } { i64 2, ptr @textlit_methods, i64 34, [35 x i8] c"NOT no_overlap_less_GT_false(0,2) \00" }
@textlit_19 = internal constant { i64, ptr, i64, [35 x i8] } { i64 2, ptr @textlit_methods, i64 34, [35 x i8] c"NOT no_overlap_less_GE_false(0,2) \00" }
@textlit_20 = internal constant { i64, ptr, i64, [35 x i8] } { i64 2, ptr @textlit_methods, i64 34, [35 x i8] c"NOT no_overlap_less_EQ_false(0,2) \00" }
@textlit_21 = internal constant { i64, ptr, i64, [30 x i8] } { i64 2, ptr @textlit_methods, i64 29, [30 x i8] c"no_overlap_less_NE_true(0,2) \00" }
@textlit_22 = internal constant { i64, ptr, i64, [35 x i8] } { i64 2, ptr @textlit_methods, i64 34, [35 x i8] c"minimum_overlap_less_LE_true(0,1) \00" }
@textlit_23 = internal constant { i64, ptr, i64, [40 x i8] } { i64 2, ptr @textlit_methods, i64 39, [40 x i8] c"NOT minimum_overlap_less_GT_false(0,1) \00" }
@textlit_24 = internal constant { i64, ptr, i64, [38 x i8] } { i64 2, ptr @textlit_methods, i64 37, [38 x i8] c"NOT no_overlap_greater_LT_false(2,0) \00" }
@textlit_25 = internal constant { i64, ptr, i64, [38 x i8] } { i64 2, ptr @textlit_methods, i64 37, [38 x i8] c"NOT no_overlap_greater_LE_false(2,0) \00" }
@textlit_26 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"no_overlap_greater_GT_true(2,0) \00" }
@textlit_27 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"no_overlap_greater_GE_true(2,0) \00" }
@textlit_28 = internal constant { i64, ptr, i64, [38 x i8] } { i64 2, ptr @textlit_methods, i64 37, [38 x i8] c"NOT no_overlap_greater_EQ_false(2,0) \00" }
@textlit_29 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"no_overlap_greater_NE_true(2,0) \00" }
@textlit_30 = internal constant { i64, ptr, i64, [43 x i8] } { i64 2, ptr @textlit_methods, i64 42, [43 x i8] c"NOT minimum_overlap_greater_LT_false(1,0) \00" }
@textlit_31 = internal constant { i64, ptr, i64, [38 x i8] } { i64 2, ptr @textlit_methods, i64 37, [38 x i8] c"minimum_overlap_greater_GE_true(1,0) \00" }
@textlit_32 = internal constant { i64, ptr, i64, [56 x i8] } { i64 2, ptr @textlit_methods, i64 55, [56 x i8] c"no_overlap_less_enum_LT_true(Number.Zero, Number.Four) \00" }
@textlit_33 = internal constant { i64, ptr, i64, [56 x i8] } { i64 2, ptr @textlit_methods, i64 55, [56 x i8] c"no_overlap_less_enum_LE_true(Number.Zero, Number.Four) \00" }
@textlit_34 = internal constant { i64, ptr, i64, [61 x i8] } { i64 2, ptr @textlit_methods, i64 60, [61 x i8] c"NOT no_overlap_less_enum_GT_false(Number.Zero, Number.Four) \00" }
@textlit_35 = internal constant { i64, ptr, i64, [61 x i8] } { i64 2, ptr @textlit_methods, i64 60, [61 x i8] c"NOT no_overlap_less_enum_GE_false(Number.Zero, Number.Four) \00" }
@textlit_36 = internal constant { i64, ptr, i64, [61 x i8] } { i64 2, ptr @textlit_methods, i64 60, [61 x i8] c"NOT no_overlap_less_enum_EQ_false(Number.Zero, Number.Four) \00" }
@textlit_37 = internal constant { i64, ptr, i64, [56 x i8] } { i64 2, ptr @textlit_methods, i64 55, [56 x i8] c"no_overlap_less_enum_NE_true(Number.Zero, Number.Four) \00" }
@textlit_38 = internal constant { i64, ptr, i64, [60 x i8] } { i64 2, ptr @textlit_methods, i64 59, [60 x i8] c"minimum_overlap_less_enum_LE_true(Number.Zero, Number.One) \00" }
@textlit_39 = internal constant { i64, ptr, i64, [65 x i8] } { i64 2, ptr @textlit_methods, i64 64, [65 x i8] c"NOT minimum_overlap_less_enum_GT_false(Number.Zero, Number.One) \00" }
@textlit_40 = internal constant { i64, ptr, i64, [64 x i8] } { i64 2, ptr @textlit_methods, i64 63, [64 x i8] c"NOT no_overlap_greater_enum_LT_false(Number.Four, Number.Zero) \00" }
@textlit_41 = internal constant { i64, ptr, i64, [64 x i8] } { i64 2, ptr @textlit_methods, i64 63, [64 x i8] c"NOT no_overlap_greater_enum_LE_false(Number.Four, Number.Zero) \00" }
@textlit_42 = internal constant { i64, ptr, i64, [59 x i8] } { i64 2, ptr @textlit_methods, i64 58, [59 x i8] c"no_overlap_greater_enum_GT_true(Number.Four, Number.Zero) \00" }
@textlit_43 = internal constant { i64, ptr, i64, [59 x i8] } { i64 2, ptr @textlit_methods, i64 58, [59 x i8] c"no_overlap_greater_enum_GE_true(Number.Four, Number.Zero) \00" }
@textlit_44 = internal constant { i64, ptr, i64, [64 x i8] } { i64 2, ptr @textlit_methods, i64 63, [64 x i8] c"NOT no_overlap_greater_enum_EQ_false(Number.Four, Number.Zero) \00" }
@textlit_45 = internal constant { i64, ptr, i64, [59 x i8] } { i64 2, ptr @textlit_methods, i64 58, [59 x i8] c"no_overlap_greater_enum_NE_true(Number.Four, Number.Zero) \00" }
@textlit_46 = internal constant { i64, ptr, i64, [68 x i8] } { i64 2, ptr @textlit_methods, i64 67, [68 x i8] c"NOT minimum_overlap_greater_enum_LT_false(Number.One, Number.Zero) \00" }
@textlit_47 = internal constant { i64, ptr, i64, [63 x i8] } { i64 2, ptr @textlit_methods, i64 62, [63 x i8] c"minimum_overlap_greater_enum_GE_true(Number.One, Number.Zero) \00" }
@textlit_48 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT overlap_1_LT_false(0,0) \00" }
@textlit_49 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"overlap_1_LE_true(0,0) \00" }
@textlit_50 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT overlap_1_GT_false(0,0) \00" }
@textlit_51 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"overlap_1_GE_true(0,0) \00" }
@textlit_52 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"overlap_1_EQ_true(0,0) \00" }
@textlit_53 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT overlap_1_NE_false(0,0) \00" }
@textlit_54 = internal constant { i64, ptr, i64, [48 x i8] } { i64 2, ptr @textlit_methods, i64 47, [48 x i8] c"NOT ord_enum_vs_negative_LT_false(Number.Zero) \00" }
@textlit_55 = internal constant { i64, ptr, i64, [48 x i8] } { i64 2, ptr @textlit_methods, i64 47, [48 x i8] c"NOT ord_enum_vs_negative_LE_false(Number.Zero) \00" }
@textlit_56 = internal constant { i64, ptr, i64, [43 x i8] } { i64 2, ptr @textlit_methods, i64 42, [43 x i8] c"ord_enum_vs_negative_GT_true(Number.Zero) \00" }
@textlit_57 = internal constant { i64, ptr, i64, [43 x i8] } { i64 2, ptr @textlit_methods, i64 42, [43 x i8] c"ord_enum_vs_negative_GE_true(Number.Zero) \00" }
@textlit_58 = internal constant { i64, ptr, i64, [48 x i8] } { i64 2, ptr @textlit_methods, i64 47, [48 x i8] c"NOT ord_enum_vs_negative_EQ_false(Number.Zero) \00" }
@textlit_59 = internal constant { i64, ptr, i64, [43 x i8] } { i64 2, ptr @textlit_methods, i64 42, [43 x i8] c"ord_enum_vs_negative_NE_true(Number.Zero) \00" }
@textlit_60 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"NOT abs_vs_negative_LT_false(0) \00" }
@textlit_61 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"NOT abs_vs_negative_LE_false(0) \00" }
@textlit_62 = internal constant { i64, ptr, i64, [28 x i8] } { i64 2, ptr @textlit_methods, i64 27, [28 x i8] c"abs_vs_negative_GT_true(0) \00" }
@textlit_63 = internal constant { i64, ptr, i64, [28 x i8] } { i64 2, ptr @textlit_methods, i64 27, [28 x i8] c"abs_vs_negative_GE_true(0) \00" }
@textlit_64 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"NOT abs_vs_negative_EQ_false(0) \00" }
@textlit_65 = internal constant { i64, ptr, i64, [28 x i8] } { i64 2, ptr @textlit_methods, i64 27, [28 x i8] c"abs_vs_negative_NE_true(0) \00" }
@textlit_66 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"NOT abs_vs_zero_LT_false(0) \00" }
@textlit_67 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"abs_vs_zero_GE_true(0) \00" }
@textlit_68 = internal constant { i64, ptr, i64, [28 x i8] } { i64 2, ptr @textlit_methods, i64 27, [28 x i8] c"neg_abs_vs_zero_LE_true(0) \00" }
@textlit_69 = internal constant { i64, ptr, i64, [33 x i8] } { i64 2, ptr @textlit_methods, i64 32, [33 x i8] c"NOT neg_abs_vs_zero_GT_false(0) \00" }
@textlit_70 = internal constant { i64, ptr, i64, [27 x i8] } { i64 2, ptr @textlit_methods, i64 26, [27 x i8] c"neg_abs_vs_one_LT_true(0) \00" }
@textlit_71 = internal constant { i64, ptr, i64, [27 x i8] } { i64 2, ptr @textlit_methods, i64 26, [27 x i8] c"neg_abs_vs_one_LE_true(0) \00" }
@textlit_72 = internal constant { i64, ptr, i64, [32 x i8] } { i64 2, ptr @textlit_methods, i64 31, [32 x i8] c"NOT neg_abs_vs_one_GT_false(0) \00" }
@textlit_73 = internal constant { i64, ptr, i64, [32 x i8] } { i64 2, ptr @textlit_methods, i64 31, [32 x i8] c"NOT neg_abs_vs_one_GE_false(0) \00" }
@textlit_74 = internal constant { i64, ptr, i64, [32 x i8] } { i64 2, ptr @textlit_methods, i64 31, [32 x i8] c"NOT neg_abs_vs_one_EQ_false(0) \00" }
@textlit_75 = internal constant { i64, ptr, i64, [27 x i8] } { i64 2, ptr @textlit_methods, i64 26, [27 x i8] c"neg_abs_vs_one_NE_true(0) \00" }

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Main_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Main_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @Main_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @Main__Main_M3()
  br label %done
done:
  ret ptr @Main_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "Main__CardinalLT0_false", linkageName: "Main__CardinalLT0_false", scope: !4, file: !3, line: 13, type: !6, scopeLine: 13, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__CardinalGE0_true", linkageName: "Main__CardinalGE0_true", scope: !4, file: !3, line: 14, type: !6, scopeLine: 14, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__CardinalLTNeg1_false", linkageName: "Main__CardinalLTNeg1_false", scope: !4, file: !3, line: 19, type: !6, scopeLine: 19, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__CardinalLENeg1_false", linkageName: "Main__CardinalLENeg1_false", scope: !4, file: !3, line: 20, type: !6, scopeLine: 20, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Main__CardinalGTNeg1_true", linkageName: "Main__CardinalGTNeg1_true", scope: !4, file: !3, line: 21, type: !6, scopeLine: 21, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Main__CardinalGENeg1_true", linkageName: "Main__CardinalGENeg1_true", scope: !4, file: !3, line: 22, type: !6, scopeLine: 22, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Main__CardinalNENeg1_true", linkageName: "Main__CardinalNENeg1_true", scope: !4, file: !3, line: 23, type: !6, scopeLine: 23, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Main__CardinalEQNeg1_false", linkageName: "Main__CardinalEQNeg1_false", scope: !4, file: !3, line: 24, type: !6, scopeLine: 24, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Main__CardinalLTNeg2_false", linkageName: "Main__CardinalLTNeg2_false", scope: !4, file: !3, line: 29, type: !6, scopeLine: 29, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Main__CardinalLENeg2_false", linkageName: "Main__CardinalLENeg2_false", scope: !4, file: !3, line: 30, type: !6, scopeLine: 30, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Main__CardinalGTNeg2_true", linkageName: "Main__CardinalGTNeg2_true", scope: !4, file: !3, line: 31, type: !6, scopeLine: 31, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Main__CardinalGENeg2_true", linkageName: "Main__CardinalGENeg2_true", scope: !4, file: !3, line: 32, type: !6, scopeLine: 32, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "Main__CardinalNENeg2_true", linkageName: "Main__CardinalNENeg2_true", scope: !4, file: !3, line: 33, type: !6, scopeLine: 33, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "Main__CardinalEQNeg2_false", linkageName: "Main__CardinalEQNeg2_false", scope: !4, file: !3, line: 34, type: !6, scopeLine: 34, unit: !2, spFlags: DISPFlagDefinition)
!44 = distinct !DISubprogram(name: "Main__LongcardLT0_false", linkageName: "Main__LongcardLT0_false", scope: !4, file: !3, line: 39, type: !6, scopeLine: 39, unit: !2, spFlags: DISPFlagDefinition)
!46 = distinct !DISubprogram(name: "Main__LongcardGE0_true", linkageName: "Main__LongcardGE0_true", scope: !4, file: !3, line: 40, type: !6, scopeLine: 40, unit: !2, spFlags: DISPFlagDefinition)
!48 = distinct !DISubprogram(name: "Main__no_overlap_less_LT_true", linkageName: "Main__no_overlap_less_LT_true", scope: !4, file: !3, line: 45, type: !6, scopeLine: 45, unit: !2, spFlags: DISPFlagDefinition)
!50 = distinct !DISubprogram(name: "Main__no_overlap_less_LE_true", linkageName: "Main__no_overlap_less_LE_true", scope: !4, file: !3, line: 46, type: !6, scopeLine: 46, unit: !2, spFlags: DISPFlagDefinition)
!52 = distinct !DISubprogram(name: "Main__no_overlap_less_GT_false", linkageName: "Main__no_overlap_less_GT_false", scope: !4, file: !3, line: 47, type: !6, scopeLine: 47, unit: !2, spFlags: DISPFlagDefinition)
!54 = distinct !DISubprogram(name: "Main__no_overlap_less_GE_false", linkageName: "Main__no_overlap_less_GE_false", scope: !4, file: !3, line: 48, type: !6, scopeLine: 48, unit: !2, spFlags: DISPFlagDefinition)
!56 = distinct !DISubprogram(name: "Main__no_overlap_less_EQ_false", linkageName: "Main__no_overlap_less_EQ_false", scope: !4, file: !3, line: 49, type: !6, scopeLine: 49, unit: !2, spFlags: DISPFlagDefinition)
!58 = distinct !DISubprogram(name: "Main__no_overlap_less_NE_true", linkageName: "Main__no_overlap_less_NE_true", scope: !4, file: !3, line: 50, type: !6, scopeLine: 50, unit: !2, spFlags: DISPFlagDefinition)
!60 = distinct !DISubprogram(name: "Main__minimum_overlap_less_LE_true", linkageName: "Main__minimum_overlap_less_LE_true", scope: !4, file: !3, line: 55, type: !6, scopeLine: 55, unit: !2, spFlags: DISPFlagDefinition)
!62 = distinct !DISubprogram(name: "Main__minimum_overlap_less_GT_false", linkageName: "Main__minimum_overlap_less_GT_false", scope: !4, file: !3, line: 56, type: !6, scopeLine: 56, unit: !2, spFlags: DISPFlagDefinition)
!64 = distinct !DISubprogram(name: "Main__no_overlap_greater_LT_false", linkageName: "Main__no_overlap_greater_LT_false", scope: !4, file: !3, line: 61, type: !6, scopeLine: 61, unit: !2, spFlags: DISPFlagDefinition)
!66 = distinct !DISubprogram(name: "Main__no_overlap_greater_LE_false", linkageName: "Main__no_overlap_greater_LE_false", scope: !4, file: !3, line: 62, type: !6, scopeLine: 62, unit: !2, spFlags: DISPFlagDefinition)
!68 = distinct !DISubprogram(name: "Main__no_overlap_greater_GT_true", linkageName: "Main__no_overlap_greater_GT_true", scope: !4, file: !3, line: 63, type: !6, scopeLine: 63, unit: !2, spFlags: DISPFlagDefinition)
!70 = distinct !DISubprogram(name: "Main__no_overlap_greater_GE_true", linkageName: "Main__no_overlap_greater_GE_true", scope: !4, file: !3, line: 64, type: !6, scopeLine: 64, unit: !2, spFlags: DISPFlagDefinition)
!72 = distinct !DISubprogram(name: "Main__no_overlap_greater_EQ_false", linkageName: "Main__no_overlap_greater_EQ_false", scope: !4, file: !3, line: 65, type: !6, scopeLine: 65, unit: !2, spFlags: DISPFlagDefinition)
!74 = distinct !DISubprogram(name: "Main__no_overlap_greater_NE_true", linkageName: "Main__no_overlap_greater_NE_true", scope: !4, file: !3, line: 66, type: !6, scopeLine: 66, unit: !2, spFlags: DISPFlagDefinition)
!76 = distinct !DISubprogram(name: "Main__minimum_overlap_greater_LT_false", linkageName: "Main__minimum_overlap_greater_LT_false", scope: !4, file: !3, line: 71, type: !6, scopeLine: 71, unit: !2, spFlags: DISPFlagDefinition)
!78 = distinct !DISubprogram(name: "Main__minimum_overlap_greater_GE_true", linkageName: "Main__minimum_overlap_greater_GE_true", scope: !4, file: !3, line: 72, type: !6, scopeLine: 72, unit: !2, spFlags: DISPFlagDefinition)
!80 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_LT_true", linkageName: "Main__no_overlap_less_enum_LT_true", scope: !4, file: !3, line: 86, type: !6, scopeLine: 86, unit: !2, spFlags: DISPFlagDefinition)
!82 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_LE_true", linkageName: "Main__no_overlap_less_enum_LE_true", scope: !4, file: !3, line: 87, type: !6, scopeLine: 87, unit: !2, spFlags: DISPFlagDefinition)
!84 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_GT_false", linkageName: "Main__no_overlap_less_enum_GT_false", scope: !4, file: !3, line: 88, type: !6, scopeLine: 88, unit: !2, spFlags: DISPFlagDefinition)
!86 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_GE_false", linkageName: "Main__no_overlap_less_enum_GE_false", scope: !4, file: !3, line: 89, type: !6, scopeLine: 89, unit: !2, spFlags: DISPFlagDefinition)
!88 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_EQ_false", linkageName: "Main__no_overlap_less_enum_EQ_false", scope: !4, file: !3, line: 90, type: !6, scopeLine: 90, unit: !2, spFlags: DISPFlagDefinition)
!90 = distinct !DISubprogram(name: "Main__no_overlap_less_enum_NE_true", linkageName: "Main__no_overlap_less_enum_NE_true", scope: !4, file: !3, line: 91, type: !6, scopeLine: 91, unit: !2, spFlags: DISPFlagDefinition)
!92 = distinct !DISubprogram(name: "Main__minimum_overlap_less_enum_LE_true", linkageName: "Main__minimum_overlap_less_enum_LE_true", scope: !4, file: !3, line: 96, type: !6, scopeLine: 96, unit: !2, spFlags: DISPFlagDefinition)
!94 = distinct !DISubprogram(name: "Main__minimum_overlap_less_enum_GT_false", linkageName: "Main__minimum_overlap_less_enum_GT_false", scope: !4, file: !3, line: 97, type: !6, scopeLine: 97, unit: !2, spFlags: DISPFlagDefinition)
!96 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_LT_false", linkageName: "Main__no_overlap_greater_enum_LT_false", scope: !4, file: !3, line: 102, type: !6, scopeLine: 102, unit: !2, spFlags: DISPFlagDefinition)
!98 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_LE_false", linkageName: "Main__no_overlap_greater_enum_LE_false", scope: !4, file: !3, line: 103, type: !6, scopeLine: 103, unit: !2, spFlags: DISPFlagDefinition)
!100 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_GT_true", linkageName: "Main__no_overlap_greater_enum_GT_true", scope: !4, file: !3, line: 104, type: !6, scopeLine: 104, unit: !2, spFlags: DISPFlagDefinition)
!102 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_GE_true", linkageName: "Main__no_overlap_greater_enum_GE_true", scope: !4, file: !3, line: 105, type: !6, scopeLine: 105, unit: !2, spFlags: DISPFlagDefinition)
!104 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_EQ_false", linkageName: "Main__no_overlap_greater_enum_EQ_false", scope: !4, file: !3, line: 106, type: !6, scopeLine: 106, unit: !2, spFlags: DISPFlagDefinition)
!106 = distinct !DISubprogram(name: "Main__no_overlap_greater_enum_NE_true", linkageName: "Main__no_overlap_greater_enum_NE_true", scope: !4, file: !3, line: 107, type: !6, scopeLine: 107, unit: !2, spFlags: DISPFlagDefinition)
!108 = distinct !DISubprogram(name: "Main__minimum_overlap_greater_enum_LT_false", linkageName: "Main__minimum_overlap_greater_enum_LT_false", scope: !4, file: !3, line: 112, type: !6, scopeLine: 112, unit: !2, spFlags: DISPFlagDefinition)
!110 = distinct !DISubprogram(name: "Main__minimum_overlap_greater_enum_GE_true", linkageName: "Main__minimum_overlap_greater_enum_GE_true", scope: !4, file: !3, line: 113, type: !6, scopeLine: 113, unit: !2, spFlags: DISPFlagDefinition)
!112 = distinct !DISubprogram(name: "Main__overlap_1_LT_false", linkageName: "Main__overlap_1_LT_false", scope: !4, file: !3, line: 118, type: !6, scopeLine: 118, unit: !2, spFlags: DISPFlagDefinition)
!114 = distinct !DISubprogram(name: "Main__overlap_1_LE_true", linkageName: "Main__overlap_1_LE_true", scope: !4, file: !3, line: 119, type: !6, scopeLine: 119, unit: !2, spFlags: DISPFlagDefinition)
!116 = distinct !DISubprogram(name: "Main__overlap_1_GT_false", linkageName: "Main__overlap_1_GT_false", scope: !4, file: !3, line: 120, type: !6, scopeLine: 120, unit: !2, spFlags: DISPFlagDefinition)
!118 = distinct !DISubprogram(name: "Main__overlap_1_GE_true", linkageName: "Main__overlap_1_GE_true", scope: !4, file: !3, line: 121, type: !6, scopeLine: 121, unit: !2, spFlags: DISPFlagDefinition)
!120 = distinct !DISubprogram(name: "Main__overlap_1_EQ_true", linkageName: "Main__overlap_1_EQ_true", scope: !4, file: !3, line: 122, type: !6, scopeLine: 122, unit: !2, spFlags: DISPFlagDefinition)
!122 = distinct !DISubprogram(name: "Main__overlap_1_NE_false", linkageName: "Main__overlap_1_NE_false", scope: !4, file: !3, line: 123, type: !6, scopeLine: 123, unit: !2, spFlags: DISPFlagDefinition)
!124 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_LT_false", linkageName: "Main__ord_enum_vs_negative_LT_false", scope: !4, file: !3, line: 128, type: !6, scopeLine: 128, unit: !2, spFlags: DISPFlagDefinition)
!126 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_LE_false", linkageName: "Main__ord_enum_vs_negative_LE_false", scope: !4, file: !3, line: 129, type: !6, scopeLine: 129, unit: !2, spFlags: DISPFlagDefinition)
!128 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_GT_true", linkageName: "Main__ord_enum_vs_negative_GT_true", scope: !4, file: !3, line: 130, type: !6, scopeLine: 130, unit: !2, spFlags: DISPFlagDefinition)
!130 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_GE_true", linkageName: "Main__ord_enum_vs_negative_GE_true", scope: !4, file: !3, line: 131, type: !6, scopeLine: 131, unit: !2, spFlags: DISPFlagDefinition)
!132 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_EQ_false", linkageName: "Main__ord_enum_vs_negative_EQ_false", scope: !4, file: !3, line: 132, type: !6, scopeLine: 132, unit: !2, spFlags: DISPFlagDefinition)
!134 = distinct !DISubprogram(name: "Main__ord_enum_vs_negative_NE_true", linkageName: "Main__ord_enum_vs_negative_NE_true", scope: !4, file: !3, line: 133, type: !6, scopeLine: 133, unit: !2, spFlags: DISPFlagDefinition)
!136 = distinct !DISubprogram(name: "Main__abs_vs_negative_LT_false", linkageName: "Main__abs_vs_negative_LT_false", scope: !4, file: !3, line: 138, type: !6, scopeLine: 138, unit: !2, spFlags: DISPFlagDefinition)
!138 = distinct !DISubprogram(name: "Main__abs_vs_negative_LE_false", linkageName: "Main__abs_vs_negative_LE_false", scope: !4, file: !3, line: 139, type: !6, scopeLine: 139, unit: !2, spFlags: DISPFlagDefinition)
!140 = distinct !DISubprogram(name: "Main__abs_vs_negative_GT_true", linkageName: "Main__abs_vs_negative_GT_true", scope: !4, file: !3, line: 140, type: !6, scopeLine: 140, unit: !2, spFlags: DISPFlagDefinition)
!142 = distinct !DISubprogram(name: "Main__abs_vs_negative_GE_true", linkageName: "Main__abs_vs_negative_GE_true", scope: !4, file: !3, line: 141, type: !6, scopeLine: 141, unit: !2, spFlags: DISPFlagDefinition)
!144 = distinct !DISubprogram(name: "Main__abs_vs_negative_EQ_false", linkageName: "Main__abs_vs_negative_EQ_false", scope: !4, file: !3, line: 142, type: !6, scopeLine: 142, unit: !2, spFlags: DISPFlagDefinition)
!146 = distinct !DISubprogram(name: "Main__abs_vs_negative_NE_true", linkageName: "Main__abs_vs_negative_NE_true", scope: !4, file: !3, line: 143, type: !6, scopeLine: 143, unit: !2, spFlags: DISPFlagDefinition)
!148 = distinct !DISubprogram(name: "Main__abs_vs_zero_LT_false", linkageName: "Main__abs_vs_zero_LT_false", scope: !4, file: !3, line: 148, type: !6, scopeLine: 148, unit: !2, spFlags: DISPFlagDefinition)
!150 = distinct !DISubprogram(name: "Main__abs_vs_zero_GE_true", linkageName: "Main__abs_vs_zero_GE_true", scope: !4, file: !3, line: 149, type: !6, scopeLine: 149, unit: !2, spFlags: DISPFlagDefinition)
!152 = distinct !DISubprogram(name: "Main__neg_abs_vs_zero_LE_true", linkageName: "Main__neg_abs_vs_zero_LE_true", scope: !4, file: !3, line: 154, type: !6, scopeLine: 154, unit: !2, spFlags: DISPFlagDefinition)
!154 = distinct !DISubprogram(name: "Main__neg_abs_vs_zero_GT_false", linkageName: "Main__neg_abs_vs_zero_GT_false", scope: !4, file: !3, line: 155, type: !6, scopeLine: 155, unit: !2, spFlags: DISPFlagDefinition)
!156 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_LT_true", linkageName: "Main__neg_abs_vs_one_LT_true", scope: !4, file: !3, line: 160, type: !6, scopeLine: 160, unit: !2, spFlags: DISPFlagDefinition)
!158 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_LE_true", linkageName: "Main__neg_abs_vs_one_LE_true", scope: !4, file: !3, line: 161, type: !6, scopeLine: 161, unit: !2, spFlags: DISPFlagDefinition)
!160 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_GT_false", linkageName: "Main__neg_abs_vs_one_GT_false", scope: !4, file: !3, line: 162, type: !6, scopeLine: 162, unit: !2, spFlags: DISPFlagDefinition)
!162 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_GE_false", linkageName: "Main__neg_abs_vs_one_GE_false", scope: !4, file: !3, line: 163, type: !6, scopeLine: 163, unit: !2, spFlags: DISPFlagDefinition)
!164 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_EQ_false", linkageName: "Main__neg_abs_vs_one_EQ_false", scope: !4, file: !3, line: 164, type: !6, scopeLine: 164, unit: !2, spFlags: DISPFlagDefinition)
!166 = distinct !DISubprogram(name: "Main__neg_abs_vs_one_NE_true", linkageName: "Main__neg_abs_vs_one_NE_true", scope: !4, file: !3, line: 165, type: !6, scopeLine: 165, unit: !2, spFlags: DISPFlagDefinition)
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
!168 = !DILocalVariable(name: "_result", scope: !16, file: !3, line: 13, type: !7)
!169 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 13, type: !7)
!170 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 14, type: !7)
!171 = !DILocalVariable(name: "a", scope: !18, file: !3, line: 14, type: !7)
!172 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 19, type: !7)
!173 = !DILocalVariable(name: "a", scope: !20, file: !3, line: 19, type: !7)
!174 = !DILocalVariable(name: "_result", scope: !22, file: !3, line: 20, type: !7)
!175 = !DILocalVariable(name: "a", scope: !22, file: !3, line: 20, type: !7)
!176 = !DILocalVariable(name: "_result", scope: !24, file: !3, line: 21, type: !7)
!177 = !DILocalVariable(name: "a", scope: !24, file: !3, line: 21, type: !7)
!178 = !DILocalVariable(name: "_result", scope: !26, file: !3, line: 22, type: !7)
!179 = !DILocalVariable(name: "a", scope: !26, file: !3, line: 22, type: !7)
!180 = !DILocalVariable(name: "_result", scope: !28, file: !3, line: 23, type: !7)
!181 = !DILocalVariable(name: "a", scope: !28, file: !3, line: 23, type: !7)
!182 = !DILocalVariable(name: "_result", scope: !30, file: !3, line: 24, type: !7)
!183 = !DILocalVariable(name: "a", scope: !30, file: !3, line: 24, type: !7)
!184 = !DILocalVariable(name: "_result", scope: !32, file: !3, line: 29, type: !7)
!185 = !DILocalVariable(name: "a", scope: !32, file: !3, line: 29, type: !7)
!186 = !DILocalVariable(name: "_result", scope: !34, file: !3, line: 30, type: !7)
!187 = !DILocalVariable(name: "a", scope: !34, file: !3, line: 30, type: !7)
!188 = !DILocalVariable(name: "_result", scope: !36, file: !3, line: 31, type: !7)
!189 = !DILocalVariable(name: "a", scope: !36, file: !3, line: 31, type: !7)
!190 = !DILocalVariable(name: "_result", scope: !38, file: !3, line: 32, type: !7)
!191 = !DILocalVariable(name: "a", scope: !38, file: !3, line: 32, type: !7)
!192 = !DILocalVariable(name: "_result", scope: !40, file: !3, line: 33, type: !7)
!193 = !DILocalVariable(name: "a", scope: !40, file: !3, line: 33, type: !7)
!194 = !DILocalVariable(name: "_result", scope: !42, file: !3, line: 34, type: !7)
!195 = !DILocalVariable(name: "a", scope: !42, file: !3, line: 34, type: !7)
!196 = !DILocalVariable(name: "_result", scope: !44, file: !3, line: 39, type: !7)
!197 = !DILocalVariable(name: "a", scope: !44, file: !3, line: 39, type: !7)
!198 = !DILocalVariable(name: "_result", scope: !46, file: !3, line: 40, type: !7)
!199 = !DILocalVariable(name: "a", scope: !46, file: !3, line: 40, type: !7)
!200 = !DILocalVariable(name: "_result", scope: !48, file: !3, line: 45, type: !7)
!201 = !DILocalVariable(name: "b", scope: !48, file: !3, line: 45, type: !7)
!202 = !DILocalVariable(name: "a", scope: !48, file: !3, line: 45, type: !7)
!203 = !DILocalVariable(name: "_result", scope: !50, file: !3, line: 46, type: !7)
!204 = !DILocalVariable(name: "b", scope: !50, file: !3, line: 46, type: !7)
!205 = !DILocalVariable(name: "a", scope: !50, file: !3, line: 46, type: !7)
!206 = !DILocalVariable(name: "_result", scope: !52, file: !3, line: 47, type: !7)
!207 = !DILocalVariable(name: "b", scope: !52, file: !3, line: 47, type: !7)
!208 = !DILocalVariable(name: "a", scope: !52, file: !3, line: 47, type: !7)
!209 = !DILocalVariable(name: "_result", scope: !54, file: !3, line: 48, type: !7)
!210 = !DILocalVariable(name: "b", scope: !54, file: !3, line: 48, type: !7)
!211 = !DILocalVariable(name: "a", scope: !54, file: !3, line: 48, type: !7)
!212 = !DILocalVariable(name: "_result", scope: !56, file: !3, line: 49, type: !7)
!213 = !DILocalVariable(name: "b", scope: !56, file: !3, line: 49, type: !7)
!214 = !DILocalVariable(name: "a", scope: !56, file: !3, line: 49, type: !7)
!215 = !DILocalVariable(name: "_result", scope: !58, file: !3, line: 50, type: !7)
!216 = !DILocalVariable(name: "b", scope: !58, file: !3, line: 50, type: !7)
!217 = !DILocalVariable(name: "a", scope: !58, file: !3, line: 50, type: !7)
!218 = !DILocalVariable(name: "_result", scope: !60, file: !3, line: 55, type: !7)
!219 = !DILocalVariable(name: "b", scope: !60, file: !3, line: 55, type: !7)
!220 = !DILocalVariable(name: "a", scope: !60, file: !3, line: 55, type: !7)
!221 = !DILocalVariable(name: "_result", scope: !62, file: !3, line: 56, type: !7)
!222 = !DILocalVariable(name: "b", scope: !62, file: !3, line: 56, type: !7)
!223 = !DILocalVariable(name: "a", scope: !62, file: !3, line: 56, type: !7)
!224 = !DILocalVariable(name: "_result", scope: !64, file: !3, line: 61, type: !7)
!225 = !DILocalVariable(name: "b", scope: !64, file: !3, line: 61, type: !7)
!226 = !DILocalVariable(name: "a", scope: !64, file: !3, line: 61, type: !7)
!227 = !DILocalVariable(name: "_result", scope: !66, file: !3, line: 62, type: !7)
!228 = !DILocalVariable(name: "b", scope: !66, file: !3, line: 62, type: !7)
!229 = !DILocalVariable(name: "a", scope: !66, file: !3, line: 62, type: !7)
!230 = !DILocalVariable(name: "_result", scope: !68, file: !3, line: 63, type: !7)
!231 = !DILocalVariable(name: "b", scope: !68, file: !3, line: 63, type: !7)
!232 = !DILocalVariable(name: "a", scope: !68, file: !3, line: 63, type: !7)
!233 = !DILocalVariable(name: "_result", scope: !70, file: !3, line: 64, type: !7)
!234 = !DILocalVariable(name: "b", scope: !70, file: !3, line: 64, type: !7)
!235 = !DILocalVariable(name: "a", scope: !70, file: !3, line: 64, type: !7)
!236 = !DILocalVariable(name: "_result", scope: !72, file: !3, line: 65, type: !7)
!237 = !DILocalVariable(name: "b", scope: !72, file: !3, line: 65, type: !7)
!238 = !DILocalVariable(name: "a", scope: !72, file: !3, line: 65, type: !7)
!239 = !DILocalVariable(name: "_result", scope: !74, file: !3, line: 66, type: !7)
!240 = !DILocalVariable(name: "b", scope: !74, file: !3, line: 66, type: !7)
!241 = !DILocalVariable(name: "a", scope: !74, file: !3, line: 66, type: !7)
!242 = !DILocalVariable(name: "_result", scope: !76, file: !3, line: 71, type: !7)
!243 = !DILocalVariable(name: "b", scope: !76, file: !3, line: 71, type: !7)
!244 = !DILocalVariable(name: "a", scope: !76, file: !3, line: 71, type: !7)
!245 = !DILocalVariable(name: "_result", scope: !78, file: !3, line: 72, type: !7)
!246 = !DILocalVariable(name: "b", scope: !78, file: !3, line: 72, type: !7)
!247 = !DILocalVariable(name: "a", scope: !78, file: !3, line: 72, type: !7)
!248 = !DILocalVariable(name: "_result", scope: !80, file: !3, line: 86, type: !7)
!249 = !DILocalVariable(name: "b", scope: !80, file: !3, line: 86, type: !7)
!250 = !DILocalVariable(name: "a", scope: !80, file: !3, line: 86, type: !7)
!251 = !DILocalVariable(name: "_result", scope: !82, file: !3, line: 87, type: !7)
!252 = !DILocalVariable(name: "b", scope: !82, file: !3, line: 87, type: !7)
!253 = !DILocalVariable(name: "a", scope: !82, file: !3, line: 87, type: !7)
!254 = !DILocalVariable(name: "_result", scope: !84, file: !3, line: 88, type: !7)
!255 = !DILocalVariable(name: "b", scope: !84, file: !3, line: 88, type: !7)
!256 = !DILocalVariable(name: "a", scope: !84, file: !3, line: 88, type: !7)
!257 = !DILocalVariable(name: "_result", scope: !86, file: !3, line: 89, type: !7)
!258 = !DILocalVariable(name: "b", scope: !86, file: !3, line: 89, type: !7)
!259 = !DILocalVariable(name: "a", scope: !86, file: !3, line: 89, type: !7)
!260 = !DILocalVariable(name: "_result", scope: !88, file: !3, line: 90, type: !7)
!261 = !DILocalVariable(name: "b", scope: !88, file: !3, line: 90, type: !7)
!262 = !DILocalVariable(name: "a", scope: !88, file: !3, line: 90, type: !7)
!263 = !DILocalVariable(name: "_result", scope: !90, file: !3, line: 91, type: !7)
!264 = !DILocalVariable(name: "b", scope: !90, file: !3, line: 91, type: !7)
!265 = !DILocalVariable(name: "a", scope: !90, file: !3, line: 91, type: !7)
!266 = !DILocalVariable(name: "_result", scope: !92, file: !3, line: 96, type: !7)
!267 = !DILocalVariable(name: "b", scope: !92, file: !3, line: 96, type: !7)
!268 = !DILocalVariable(name: "a", scope: !92, file: !3, line: 96, type: !7)
!269 = !DILocalVariable(name: "_result", scope: !94, file: !3, line: 97, type: !7)
!270 = !DILocalVariable(name: "b", scope: !94, file: !3, line: 97, type: !7)
!271 = !DILocalVariable(name: "a", scope: !94, file: !3, line: 97, type: !7)
!272 = !DILocalVariable(name: "_result", scope: !96, file: !3, line: 102, type: !7)
!273 = !DILocalVariable(name: "b", scope: !96, file: !3, line: 102, type: !7)
!274 = !DILocalVariable(name: "a", scope: !96, file: !3, line: 102, type: !7)
!275 = !DILocalVariable(name: "_result", scope: !98, file: !3, line: 103, type: !7)
!276 = !DILocalVariable(name: "b", scope: !98, file: !3, line: 103, type: !7)
!277 = !DILocalVariable(name: "a", scope: !98, file: !3, line: 103, type: !7)
!278 = !DILocalVariable(name: "_result", scope: !100, file: !3, line: 104, type: !7)
!279 = !DILocalVariable(name: "b", scope: !100, file: !3, line: 104, type: !7)
!280 = !DILocalVariable(name: "a", scope: !100, file: !3, line: 104, type: !7)
!281 = !DILocalVariable(name: "_result", scope: !102, file: !3, line: 105, type: !7)
!282 = !DILocalVariable(name: "b", scope: !102, file: !3, line: 105, type: !7)
!283 = !DILocalVariable(name: "a", scope: !102, file: !3, line: 105, type: !7)
!284 = !DILocalVariable(name: "_result", scope: !104, file: !3, line: 106, type: !7)
!285 = !DILocalVariable(name: "b", scope: !104, file: !3, line: 106, type: !7)
!286 = !DILocalVariable(name: "a", scope: !104, file: !3, line: 106, type: !7)
!287 = !DILocalVariable(name: "_result", scope: !106, file: !3, line: 107, type: !7)
!288 = !DILocalVariable(name: "b", scope: !106, file: !3, line: 107, type: !7)
!289 = !DILocalVariable(name: "a", scope: !106, file: !3, line: 107, type: !7)
!290 = !DILocalVariable(name: "_result", scope: !108, file: !3, line: 112, type: !7)
!291 = !DILocalVariable(name: "b", scope: !108, file: !3, line: 112, type: !7)
!292 = !DILocalVariable(name: "a", scope: !108, file: !3, line: 112, type: !7)
!293 = !DILocalVariable(name: "_result", scope: !110, file: !3, line: 113, type: !7)
!294 = !DILocalVariable(name: "b", scope: !110, file: !3, line: 113, type: !7)
!295 = !DILocalVariable(name: "a", scope: !110, file: !3, line: 113, type: !7)
!296 = !DILocalVariable(name: "_result", scope: !112, file: !3, line: 118, type: !7)
!297 = !DILocalVariable(name: "b", scope: !112, file: !3, line: 118, type: !7)
!298 = !DILocalVariable(name: "a", scope: !112, file: !3, line: 118, type: !7)
!299 = !DILocalVariable(name: "_result", scope: !114, file: !3, line: 119, type: !7)
!300 = !DILocalVariable(name: "b", scope: !114, file: !3, line: 119, type: !7)
!301 = !DILocalVariable(name: "a", scope: !114, file: !3, line: 119, type: !7)
!302 = !DILocalVariable(name: "_result", scope: !116, file: !3, line: 120, type: !7)
!303 = !DILocalVariable(name: "b", scope: !116, file: !3, line: 120, type: !7)
!304 = !DILocalVariable(name: "a", scope: !116, file: !3, line: 120, type: !7)
!305 = !DILocalVariable(name: "_result", scope: !118, file: !3, line: 121, type: !7)
!306 = !DILocalVariable(name: "b", scope: !118, file: !3, line: 121, type: !7)
!307 = !DILocalVariable(name: "a", scope: !118, file: !3, line: 121, type: !7)
!308 = !DILocalVariable(name: "_result", scope: !120, file: !3, line: 122, type: !7)
!309 = !DILocalVariable(name: "b", scope: !120, file: !3, line: 122, type: !7)
!310 = !DILocalVariable(name: "a", scope: !120, file: !3, line: 122, type: !7)
!311 = !DILocalVariable(name: "_result", scope: !122, file: !3, line: 123, type: !7)
!312 = !DILocalVariable(name: "b", scope: !122, file: !3, line: 123, type: !7)
!313 = !DILocalVariable(name: "a", scope: !122, file: !3, line: 123, type: !7)
!314 = !DILocalVariable(name: "_result", scope: !124, file: !3, line: 128, type: !7)
!315 = !DILocalVariable(name: "a", scope: !124, file: !3, line: 128, type: !7)
!316 = !DILocalVariable(name: "_result", scope: !126, file: !3, line: 129, type: !7)
!317 = !DILocalVariable(name: "a", scope: !126, file: !3, line: 129, type: !7)
!318 = !DILocalVariable(name: "_result", scope: !128, file: !3, line: 130, type: !7)
!319 = !DILocalVariable(name: "a", scope: !128, file: !3, line: 130, type: !7)
!320 = !DILocalVariable(name: "_result", scope: !130, file: !3, line: 131, type: !7)
!321 = !DILocalVariable(name: "a", scope: !130, file: !3, line: 131, type: !7)
!322 = !DILocalVariable(name: "_result", scope: !132, file: !3, line: 132, type: !7)
!323 = !DILocalVariable(name: "a", scope: !132, file: !3, line: 132, type: !7)
!324 = !DILocalVariable(name: "_result", scope: !134, file: !3, line: 133, type: !7)
!325 = !DILocalVariable(name: "a", scope: !134, file: !3, line: 133, type: !7)
!326 = !DILocalVariable(name: "_result", scope: !136, file: !3, line: 138, type: !7)
!327 = !DILocalVariable(name: "a", scope: !136, file: !3, line: 138, type: !7)
!328 = !DILocalVariable(name: "_result", scope: !138, file: !3, line: 139, type: !7)
!329 = !DILocalVariable(name: "a", scope: !138, file: !3, line: 139, type: !7)
!330 = !DILocalVariable(name: "_result", scope: !140, file: !3, line: 140, type: !7)
!331 = !DILocalVariable(name: "a", scope: !140, file: !3, line: 140, type: !7)
!332 = !DILocalVariable(name: "_result", scope: !142, file: !3, line: 141, type: !7)
!333 = !DILocalVariable(name: "a", scope: !142, file: !3, line: 141, type: !7)
!334 = !DILocalVariable(name: "_result", scope: !144, file: !3, line: 142, type: !7)
!335 = !DILocalVariable(name: "a", scope: !144, file: !3, line: 142, type: !7)
!336 = !DILocalVariable(name: "_result", scope: !146, file: !3, line: 143, type: !7)
!337 = !DILocalVariable(name: "a", scope: !146, file: !3, line: 143, type: !7)
!338 = !DILocalVariable(name: "_result", scope: !148, file: !3, line: 148, type: !7)
!339 = !DILocalVariable(name: "a", scope: !148, file: !3, line: 148, type: !7)
!340 = !DILocalVariable(name: "_result", scope: !150, file: !3, line: 149, type: !7)
!341 = !DILocalVariable(name: "a", scope: !150, file: !3, line: 149, type: !7)
!342 = !DILocalVariable(name: "_result", scope: !152, file: !3, line: 154, type: !7)
!343 = !DILocalVariable(name: "a", scope: !152, file: !3, line: 154, type: !7)
!344 = !DILocalVariable(name: "_result", scope: !154, file: !3, line: 155, type: !7)
!345 = !DILocalVariable(name: "a", scope: !154, file: !3, line: 155, type: !7)
!346 = !DILocalVariable(name: "_result", scope: !156, file: !3, line: 160, type: !7)
!347 = !DILocalVariable(name: "a", scope: !156, file: !3, line: 160, type: !7)
!348 = !DILocalVariable(name: "_result", scope: !158, file: !3, line: 161, type: !7)
!349 = !DILocalVariable(name: "a", scope: !158, file: !3, line: 161, type: !7)
!350 = !DILocalVariable(name: "_result", scope: !160, file: !3, line: 162, type: !7)
!351 = !DILocalVariable(name: "a", scope: !160, file: !3, line: 162, type: !7)
!352 = !DILocalVariable(name: "_result", scope: !162, file: !3, line: 163, type: !7)
!353 = !DILocalVariable(name: "a", scope: !162, file: !3, line: 163, type: !7)
!354 = !DILocalVariable(name: "_result", scope: !164, file: !3, line: 164, type: !7)
!355 = !DILocalVariable(name: "a", scope: !164, file: !3, line: 164, type: !7)
!356 = !DILocalVariable(name: "_result", scope: !166, file: !3, line: 165, type: !7)
!357 = !DILocalVariable(name: "a", scope: !166, file: !3, line: 165, type: !7)
!358 = !DILocation(line: 13, column: 0, scope: !16)
!359 = !DILocation(line: 14, column: 0, scope: !18)
!360 = !DILocation(line: 19, column: 0, scope: !20)
!361 = !DILocation(line: 20, column: 0, scope: !22)
!362 = !DILocation(line: 21, column: 0, scope: !24)
!363 = !DILocation(line: 22, column: 0, scope: !26)
!364 = !DILocation(line: 23, column: 0, scope: !28)
!365 = !DILocation(line: 24, column: 0, scope: !30)
!366 = !DILocation(line: 29, column: 0, scope: !32)
!367 = !DILocation(line: 30, column: 0, scope: !34)
!368 = !DILocation(line: 31, column: 0, scope: !36)
!369 = !DILocation(line: 32, column: 0, scope: !38)
!370 = !DILocation(line: 33, column: 0, scope: !40)
!371 = !DILocation(line: 34, column: 0, scope: !42)
!372 = !DILocation(line: 39, column: 0, scope: !44)
!373 = !DILocation(line: 40, column: 0, scope: !46)
!374 = !DILocation(line: 45, column: 0, scope: !48)
!375 = !DILocation(line: 46, column: 0, scope: !50)
!376 = !DILocation(line: 47, column: 0, scope: !52)
!377 = !DILocation(line: 48, column: 0, scope: !54)
!378 = !DILocation(line: 49, column: 0, scope: !56)
!379 = !DILocation(line: 50, column: 0, scope: !58)
!380 = !DILocation(line: 55, column: 0, scope: !60)
!381 = !DILocation(line: 56, column: 0, scope: !62)
!382 = !DILocation(line: 61, column: 0, scope: !64)
!383 = !DILocation(line: 62, column: 0, scope: !66)
!384 = !DILocation(line: 63, column: 0, scope: !68)
!385 = !DILocation(line: 64, column: 0, scope: !70)
!386 = !DILocation(line: 65, column: 0, scope: !72)
!387 = !DILocation(line: 66, column: 0, scope: !74)
!388 = !DILocation(line: 71, column: 0, scope: !76)
!389 = !DILocation(line: 72, column: 0, scope: !78)
!390 = !DILocation(line: 86, column: 0, scope: !80)
!391 = !DILocation(line: 87, column: 0, scope: !82)
!392 = !DILocation(line: 88, column: 0, scope: !84)
!393 = !DILocation(line: 89, column: 0, scope: !86)
!394 = !DILocation(line: 90, column: 0, scope: !88)
!395 = !DILocation(line: 91, column: 0, scope: !90)
!396 = !DILocation(line: 96, column: 0, scope: !92)
!397 = !DILocation(line: 97, column: 0, scope: !94)
!398 = !DILocation(line: 102, column: 0, scope: !96)
!399 = !DILocation(line: 103, column: 0, scope: !98)
!400 = !DILocation(line: 104, column: 0, scope: !100)
!401 = !DILocation(line: 105, column: 0, scope: !102)
!402 = !DILocation(line: 106, column: 0, scope: !104)
!403 = !DILocation(line: 107, column: 0, scope: !106)
!404 = !DILocation(line: 112, column: 0, scope: !108)
!405 = !DILocation(line: 113, column: 0, scope: !110)
!406 = !DILocation(line: 118, column: 0, scope: !112)
!407 = !DILocation(line: 119, column: 0, scope: !114)
!408 = !DILocation(line: 120, column: 0, scope: !116)
!409 = !DILocation(line: 121, column: 0, scope: !118)
!410 = !DILocation(line: 122, column: 0, scope: !120)
!411 = !DILocation(line: 123, column: 0, scope: !122)
!412 = !DILocation(line: 128, column: 0, scope: !124)
!413 = !DILocation(line: 129, column: 0, scope: !126)
!414 = !DILocation(line: 130, column: 0, scope: !128)
!415 = !DILocation(line: 131, column: 0, scope: !130)
!416 = !DILocation(line: 132, column: 0, scope: !132)
!417 = !DILocation(line: 133, column: 0, scope: !134)
!418 = !DILocation(line: 138, column: 0, scope: !136)
!419 = !DILocation(line: 139, column: 0, scope: !138)
!420 = !DILocation(line: 140, column: 0, scope: !140)
!421 = !DILocation(line: 141, column: 0, scope: !142)
!422 = !DILocation(line: 142, column: 0, scope: !144)
!423 = !DILocation(line: 143, column: 0, scope: !146)
!424 = !DILocation(line: 148, column: 0, scope: !148)
!425 = !DILocation(line: 149, column: 0, scope: !150)
!426 = !DILocation(line: 154, column: 0, scope: !152)
!427 = !DILocation(line: 155, column: 0, scope: !154)
!428 = !DILocation(line: 160, column: 0, scope: !156)
!429 = !DILocation(line: 161, column: 0, scope: !158)
!430 = !DILocation(line: 162, column: 0, scope: !160)
!431 = !DILocation(line: 163, column: 0, scope: !162)
!432 = !DILocation(line: 164, column: 0, scope: !164)
!433 = !DILocation(line: 165, column: 0, scope: !166)
!3 = !DIFile(filename: "Main.m3", directory: "..")
!4 = !DINamespace(name: "Main", scope: !2)
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
