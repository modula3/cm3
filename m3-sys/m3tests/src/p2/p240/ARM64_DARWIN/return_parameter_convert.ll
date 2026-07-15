; ModuleID = 'return_parameter_convert'
source_filename = "return_parameter_convert"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define i64 @return_parameter_convert__ret_u64_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !304, metadata !DIExpression()), !dbg !592
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !305, metadata !DIExpression()), !dbg !592
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !306, metadata !DIExpression()), !dbg !593
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !307, metadata !DIExpression()), !dbg !593
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !308, metadata !DIExpression()), !dbg !594
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !309, metadata !DIExpression()), !dbg !594
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !310, metadata !DIExpression()), !dbg !595
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !311, metadata !DIExpression()), !dbg !595
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !312, metadata !DIExpression()), !dbg !596
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !313, metadata !DIExpression()), !dbg !596
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !314, metadata !DIExpression()), !dbg !597
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !315, metadata !DIExpression()), !dbg !597
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !316, metadata !DIExpression()), !dbg !598
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !317, metadata !DIExpression()), !dbg !598
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !318, metadata !DIExpression()), !dbg !599
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !319, metadata !DIExpression()), !dbg !599
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !320, metadata !DIExpression()), !dbg !600
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !321, metadata !DIExpression()), !dbg !600
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !322, metadata !DIExpression()), !dbg !601
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !323, metadata !DIExpression()), !dbg !601
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !324, metadata !DIExpression()), !dbg !602
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !325, metadata !DIExpression()), !dbg !602
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u64_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !326, metadata !DIExpression()), !dbg !603
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !327, metadata !DIExpression()), !dbg !603
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i8_u64(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !328, metadata !DIExpression()), !dbg !604
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !329, metadata !DIExpression()), !dbg !604
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_i8(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !330, metadata !DIExpression()), !dbg !605
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !331, metadata !DIExpression()), !dbg !605
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_i32(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !44 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !332, metadata !DIExpression()), !dbg !606
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !333, metadata !DIExpression()), !dbg !606
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_LC(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !46 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !334, metadata !DIExpression()), !dbg !607
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !335, metadata !DIExpression()), !dbg !607
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_u16(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !48 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !336, metadata !DIExpression()), !dbg !608
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !337, metadata !DIExpression()), !dbg !608
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_I(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !50 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !338, metadata !DIExpression()), !dbg !609
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !339, metadata !DIExpression()), !dbg !609
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_i64(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !52 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !340, metadata !DIExpression()), !dbg !610
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !341, metadata !DIExpression()), !dbg !610
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_C(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !54 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !342, metadata !DIExpression()), !dbg !611
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !343, metadata !DIExpression()), !dbg !611
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_i16(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !56 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !344, metadata !DIExpression()), !dbg !612
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !345, metadata !DIExpression()), !dbg !612
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_u32(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !58 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !346, metadata !DIExpression()), !dbg !613
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !347, metadata !DIExpression()), !dbg !613
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_u8(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !60 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !348, metadata !DIExpression()), !dbg !614
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !349, metadata !DIExpression()), !dbg !614
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i8_L(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !62 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !350, metadata !DIExpression()), !dbg !615
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !351, metadata !DIExpression()), !dbg !615
  %t1 = sext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_u64(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !64 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !352, metadata !DIExpression()), !dbg !616
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !353, metadata !DIExpression()), !dbg !616
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_i8(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !66 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !354, metadata !DIExpression()), !dbg !617
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !355, metadata !DIExpression()), !dbg !617
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_i32(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !68 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !356, metadata !DIExpression()), !dbg !618
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !357, metadata !DIExpression()), !dbg !618
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_LC(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !70 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !358, metadata !DIExpression()), !dbg !619
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !359, metadata !DIExpression()), !dbg !619
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_u16(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !72 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !360, metadata !DIExpression()), !dbg !620
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !361, metadata !DIExpression()), !dbg !620
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_I(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !74 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !362, metadata !DIExpression()), !dbg !621
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !363, metadata !DIExpression()), !dbg !621
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_i64(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !76 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !364, metadata !DIExpression()), !dbg !622
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !365, metadata !DIExpression()), !dbg !622
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_C(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !78 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !366, metadata !DIExpression()), !dbg !623
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !367, metadata !DIExpression()), !dbg !623
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_i16(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !80 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !368, metadata !DIExpression()), !dbg !624
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !369, metadata !DIExpression()), !dbg !624
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_u32(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !82 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !370, metadata !DIExpression()), !dbg !625
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !371, metadata !DIExpression()), !dbg !625
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_u8(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !84 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !372, metadata !DIExpression()), !dbg !626
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !373, metadata !DIExpression()), !dbg !626
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i32_L(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !86 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !374, metadata !DIExpression()), !dbg !627
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !375, metadata !DIExpression()), !dbg !627
  %t1 = sext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_LC_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !88 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !376, metadata !DIExpression()), !dbg !628
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !377, metadata !DIExpression()), !dbg !628
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !90 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !378, metadata !DIExpression()), !dbg !629
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !379, metadata !DIExpression()), !dbg !629
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !92 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !380, metadata !DIExpression()), !dbg !630
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !381, metadata !DIExpression()), !dbg !630
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !94 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !382, metadata !DIExpression()), !dbg !631
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !383, metadata !DIExpression()), !dbg !631
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !96 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !384, metadata !DIExpression()), !dbg !632
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !385, metadata !DIExpression()), !dbg !632
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !98 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !386, metadata !DIExpression()), !dbg !633
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !387, metadata !DIExpression()), !dbg !633
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !100 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !388, metadata !DIExpression()), !dbg !634
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !389, metadata !DIExpression()), !dbg !634
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !102 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !390, metadata !DIExpression()), !dbg !635
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !391, metadata !DIExpression()), !dbg !635
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !104 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !392, metadata !DIExpression()), !dbg !636
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !393, metadata !DIExpression()), !dbg !636
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !106 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !394, metadata !DIExpression()), !dbg !637
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !395, metadata !DIExpression()), !dbg !637
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !108 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !396, metadata !DIExpression()), !dbg !638
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !397, metadata !DIExpression()), !dbg !638
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_LC_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !110 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !398, metadata !DIExpression()), !dbg !639
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !399, metadata !DIExpression()), !dbg !639
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_u16_u64(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !112 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !400, metadata !DIExpression()), !dbg !640
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !401, metadata !DIExpression()), !dbg !640
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_i8(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !114 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !402, metadata !DIExpression()), !dbg !641
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !403, metadata !DIExpression()), !dbg !641
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_i32(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !116 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !404, metadata !DIExpression()), !dbg !642
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !405, metadata !DIExpression()), !dbg !642
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_LC(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !118 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !406, metadata !DIExpression()), !dbg !643
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !407, metadata !DIExpression()), !dbg !643
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_u16(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !120 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !408, metadata !DIExpression()), !dbg !644
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !409, metadata !DIExpression()), !dbg !644
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_I(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !122 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !410, metadata !DIExpression()), !dbg !645
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !411, metadata !DIExpression()), !dbg !645
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_i64(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !124 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !412, metadata !DIExpression()), !dbg !646
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !413, metadata !DIExpression()), !dbg !646
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_C(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !126 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !414, metadata !DIExpression()), !dbg !647
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !415, metadata !DIExpression()), !dbg !647
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_i16(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !128 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !416, metadata !DIExpression()), !dbg !648
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !417, metadata !DIExpression()), !dbg !648
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_u32(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !130 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !418, metadata !DIExpression()), !dbg !649
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !419, metadata !DIExpression()), !dbg !649
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_u8(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !132 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !420, metadata !DIExpression()), !dbg !650
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !421, metadata !DIExpression()), !dbg !650
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u16_L(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !134 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !422, metadata !DIExpression()), !dbg !651
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !423, metadata !DIExpression()), !dbg !651
  %t1 = zext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_I_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !136 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !424, metadata !DIExpression()), !dbg !652
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !425, metadata !DIExpression()), !dbg !652
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !138 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !426, metadata !DIExpression()), !dbg !653
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !427, metadata !DIExpression()), !dbg !653
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !140 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !428, metadata !DIExpression()), !dbg !654
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !429, metadata !DIExpression()), !dbg !654
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !142 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !430, metadata !DIExpression()), !dbg !655
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !431, metadata !DIExpression()), !dbg !655
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !144 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !432, metadata !DIExpression()), !dbg !656
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !433, metadata !DIExpression()), !dbg !656
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !146 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !434, metadata !DIExpression()), !dbg !657
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !435, metadata !DIExpression()), !dbg !657
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !148 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !436, metadata !DIExpression()), !dbg !658
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !437, metadata !DIExpression()), !dbg !658
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !150 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !438, metadata !DIExpression()), !dbg !659
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !439, metadata !DIExpression()), !dbg !659
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !152 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !440, metadata !DIExpression()), !dbg !660
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !441, metadata !DIExpression()), !dbg !660
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !154 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !442, metadata !DIExpression()), !dbg !661
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !443, metadata !DIExpression()), !dbg !661
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !156 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !444, metadata !DIExpression()), !dbg !662
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !445, metadata !DIExpression()), !dbg !662
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_I_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !158 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !446, metadata !DIExpression()), !dbg !663
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !447, metadata !DIExpression()), !dbg !663
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !160 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !448, metadata !DIExpression()), !dbg !664
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !449, metadata !DIExpression()), !dbg !664
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !162 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !450, metadata !DIExpression()), !dbg !665
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !451, metadata !DIExpression()), !dbg !665
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !164 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !452, metadata !DIExpression()), !dbg !666
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !453, metadata !DIExpression()), !dbg !666
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !166 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !454, metadata !DIExpression()), !dbg !667
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !455, metadata !DIExpression()), !dbg !667
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !168 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !456, metadata !DIExpression()), !dbg !668
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !457, metadata !DIExpression()), !dbg !668
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !170 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !458, metadata !DIExpression()), !dbg !669
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !459, metadata !DIExpression()), !dbg !669
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !172 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !460, metadata !DIExpression()), !dbg !670
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !461, metadata !DIExpression()), !dbg !670
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !174 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !462, metadata !DIExpression()), !dbg !671
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !463, metadata !DIExpression()), !dbg !671
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !176 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !464, metadata !DIExpression()), !dbg !672
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !465, metadata !DIExpression()), !dbg !672
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !178 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !466, metadata !DIExpression()), !dbg !673
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !467, metadata !DIExpression()), !dbg !673
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !180 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !468, metadata !DIExpression()), !dbg !674
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !469, metadata !DIExpression()), !dbg !674
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i64_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !182 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !470, metadata !DIExpression()), !dbg !675
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !471, metadata !DIExpression()), !dbg !675
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !184 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !472, metadata !DIExpression()), !dbg !676
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !473, metadata !DIExpression()), !dbg !676
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !186 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !474, metadata !DIExpression()), !dbg !677
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !475, metadata !DIExpression()), !dbg !677
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !188 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !476, metadata !DIExpression()), !dbg !678
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !477, metadata !DIExpression()), !dbg !678
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !190 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !478, metadata !DIExpression()), !dbg !679
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !479, metadata !DIExpression()), !dbg !679
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !192 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !480, metadata !DIExpression()), !dbg !680
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !481, metadata !DIExpression()), !dbg !680
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !194 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !482, metadata !DIExpression()), !dbg !681
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !483, metadata !DIExpression()), !dbg !681
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !196 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !484, metadata !DIExpression()), !dbg !682
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !485, metadata !DIExpression()), !dbg !682
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !198 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !486, metadata !DIExpression()), !dbg !683
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !487, metadata !DIExpression()), !dbg !683
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !200 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !488, metadata !DIExpression()), !dbg !684
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !489, metadata !DIExpression()), !dbg !684
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !202 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !490, metadata !DIExpression()), !dbg !685
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !491, metadata !DIExpression()), !dbg !685
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !204 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !492, metadata !DIExpression()), !dbg !686
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !493, metadata !DIExpression()), !dbg !686
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_C_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !206 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !494, metadata !DIExpression()), !dbg !687
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !495, metadata !DIExpression()), !dbg !687
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_i16_u64(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !208 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !496, metadata !DIExpression()), !dbg !688
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !497, metadata !DIExpression()), !dbg !688
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_i8(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !210 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !498, metadata !DIExpression()), !dbg !689
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !499, metadata !DIExpression()), !dbg !689
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_i32(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !212 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !500, metadata !DIExpression()), !dbg !690
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !501, metadata !DIExpression()), !dbg !690
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_LC(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !214 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !502, metadata !DIExpression()), !dbg !691
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !503, metadata !DIExpression()), !dbg !691
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_u16(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !216 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !504, metadata !DIExpression()), !dbg !692
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !505, metadata !DIExpression()), !dbg !692
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_I(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !218 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !506, metadata !DIExpression()), !dbg !693
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !507, metadata !DIExpression()), !dbg !693
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_i64(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !220 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !508, metadata !DIExpression()), !dbg !694
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !509, metadata !DIExpression()), !dbg !694
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_C(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !222 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !510, metadata !DIExpression()), !dbg !695
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !511, metadata !DIExpression()), !dbg !695
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_i16(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !224 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !512, metadata !DIExpression()), !dbg !696
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !513, metadata !DIExpression()), !dbg !696
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_u32(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !226 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !514, metadata !DIExpression()), !dbg !697
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !515, metadata !DIExpression()), !dbg !697
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_u8(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !228 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !516, metadata !DIExpression()), !dbg !698
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !517, metadata !DIExpression()), !dbg !698
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_i16_L(i16 %a.a) personality ptr @__gxx_personality_v0 !dbg !230 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !518, metadata !DIExpression()), !dbg !699
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !519, metadata !DIExpression()), !dbg !699
  %t1 = sext i16 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_u64(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !232 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !520, metadata !DIExpression()), !dbg !700
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !521, metadata !DIExpression()), !dbg !700
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_i8(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !234 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !522, metadata !DIExpression()), !dbg !701
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !523, metadata !DIExpression()), !dbg !701
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_i32(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !236 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !524, metadata !DIExpression()), !dbg !702
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !525, metadata !DIExpression()), !dbg !702
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_LC(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !238 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !526, metadata !DIExpression()), !dbg !703
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !527, metadata !DIExpression()), !dbg !703
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_u16(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !240 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !528, metadata !DIExpression()), !dbg !704
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !529, metadata !DIExpression()), !dbg !704
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_I(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !242 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !530, metadata !DIExpression()), !dbg !705
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !531, metadata !DIExpression()), !dbg !705
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_i64(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !244 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !532, metadata !DIExpression()), !dbg !706
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !533, metadata !DIExpression()), !dbg !706
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_C(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !246 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !534, metadata !DIExpression()), !dbg !707
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !535, metadata !DIExpression()), !dbg !707
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_i16(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !248 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !536, metadata !DIExpression()), !dbg !708
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !537, metadata !DIExpression()), !dbg !708
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_u32(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !250 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !538, metadata !DIExpression()), !dbg !709
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !539, metadata !DIExpression()), !dbg !709
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_u8(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !252 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !540, metadata !DIExpression()), !dbg !710
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !541, metadata !DIExpression()), !dbg !710
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u32_L(i32 %a.a) personality ptr @__gxx_personality_v0 !dbg !254 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !542, metadata !DIExpression()), !dbg !711
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !543, metadata !DIExpression()), !dbg !711
  %t1 = zext i32 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_u64(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !256 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !544, metadata !DIExpression()), !dbg !712
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !545, metadata !DIExpression()), !dbg !712
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_i8(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !258 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !546, metadata !DIExpression()), !dbg !713
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !547, metadata !DIExpression()), !dbg !713
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_i32(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !260 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !548, metadata !DIExpression()), !dbg !714
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !549, metadata !DIExpression()), !dbg !714
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_LC(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !262 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !550, metadata !DIExpression()), !dbg !715
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !551, metadata !DIExpression()), !dbg !715
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_u16(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !264 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !552, metadata !DIExpression()), !dbg !716
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !553, metadata !DIExpression()), !dbg !716
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_I(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !266 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !554, metadata !DIExpression()), !dbg !717
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !555, metadata !DIExpression()), !dbg !717
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_i64(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !268 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !556, metadata !DIExpression()), !dbg !718
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !557, metadata !DIExpression()), !dbg !718
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_C(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !270 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !558, metadata !DIExpression()), !dbg !719
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !559, metadata !DIExpression()), !dbg !719
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_i16(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !272 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !560, metadata !DIExpression()), !dbg !720
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !561, metadata !DIExpression()), !dbg !720
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_u32(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !274 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !562, metadata !DIExpression()), !dbg !721
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !563, metadata !DIExpression()), !dbg !721
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_u8(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !276 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !564, metadata !DIExpression()), !dbg !722
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !565, metadata !DIExpression()), !dbg !722
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_u8_L(i8 %a.a) personality ptr @__gxx_personality_v0 !dbg !278 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !566, metadata !DIExpression()), !dbg !723
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !567, metadata !DIExpression()), !dbg !723
  %t1 = zext i8 %a.a to i64
  store i64 %t1, ptr %a.slot
  %t2 = load i64, ptr %a.slot
  ret i64 %t2
}

define i64 @return_parameter_convert__ret_L_u64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !280 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !568, metadata !DIExpression()), !dbg !724
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !569, metadata !DIExpression()), !dbg !724
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_i8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !282 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !570, metadata !DIExpression()), !dbg !725
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !571, metadata !DIExpression()), !dbg !725
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_i32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !284 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !572, metadata !DIExpression()), !dbg !726
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !573, metadata !DIExpression()), !dbg !726
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_LC(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !286 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !574, metadata !DIExpression()), !dbg !727
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !575, metadata !DIExpression()), !dbg !727
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_u16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !288 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !576, metadata !DIExpression()), !dbg !728
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !577, metadata !DIExpression()), !dbg !728
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_I(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !290 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !578, metadata !DIExpression()), !dbg !729
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !579, metadata !DIExpression()), !dbg !729
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_i64(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !292 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !580, metadata !DIExpression()), !dbg !730
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !581, metadata !DIExpression()), !dbg !730
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_C(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !294 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !582, metadata !DIExpression()), !dbg !731
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !583, metadata !DIExpression()), !dbg !731
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_i16(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !296 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !584, metadata !DIExpression()), !dbg !732
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !585, metadata !DIExpression()), !dbg !732
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_u32(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !298 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !586, metadata !DIExpression()), !dbg !733
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !587, metadata !DIExpression()), !dbg !733
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_u8(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !300 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !588, metadata !DIExpression()), !dbg !734
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !589, metadata !DIExpression()), !dbg !734
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define i64 @return_parameter_convert__ret_L_L(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !302 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !590, metadata !DIExpression()), !dbg !735
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !591, metadata !DIExpression()), !dbg !735
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  ret i64 %t1
}

define void @return_parameter_convert__return_parameter_convert_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = trunc i64 113 to i8
  store i8 %t1, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 104)
  store i64 114, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 112)
  store double 0x405cc76c8b439581, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 120)
  %t2 = trunc i64 117 to i32
  store i32 %t2, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 128)
  store i64 118, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 136)
  %t3 = trunc i64 119 to i16
  store i16 %t3, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 144)
  store i64 120, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 152)
  store i64 121, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 160)
  store float 0x405e87df40000000, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 168)
  %t4 = trunc i64 124 to i16
  store i16 %t4, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 172)
  store i64 125, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 176)
  %t5 = trunc i64 126 to i32
  store i32 %t5, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 184)
  %t6 = trunc i64 127 to i8
  store i8 %t6, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 188)
  store i64 128, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 192)
  ret void
}

define weak ptr @return_parameter_convert_I3(i64 %mode) {
entry:
  ret ptr @return_parameter_convert_M3_info
}

; RT0.ImportInfo chain for return_parameter_convert
declare ptr @Long_I3(i64)
declare ptr @Word_I3(i64)
declare ptr @Cstdint_I3(i64)
@return_parameter_convert_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @return_parameter_convert_I3, ptr @return_parameter_convert_M3_imp.1 }
@return_parameter_convert_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Long_I3, ptr @return_parameter_convert_M3_imp.2 }
@return_parameter_convert_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Word_I3, ptr @return_parameter_convert_M3_imp.3 }
@return_parameter_convert_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Cstdint_I3, ptr null }

; RT0.ModuleInfo for return_parameter_convert (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [112 x i8] }
@return_parameter_convert_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @return_parameter_convert_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @return_parameter_convert_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [112 x i8] zeroinitializer  ; user globals (112 bytes)
}
@return_parameter_convert__vi8 = alias i8, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 104)
@return_parameter_convert__vu64 = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 112)
@return_parameter_convert__vf64 = alias double, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 120)
@return_parameter_convert__vi32 = alias i32, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 128)
@return_parameter_convert__vLC = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 136)
@return_parameter_convert__vu16 = alias i16, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 144)
@return_parameter_convert__vI = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 152)
@return_parameter_convert__vi64 = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 160)
@return_parameter_convert__vf32 = alias float, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 168)
@return_parameter_convert__vi16 = alias i16, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 172)
@return_parameter_convert__vC = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 176)
@return_parameter_convert__vu32 = alias i32, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 184)
@return_parameter_convert__vu8 = alias i8, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 188)
@return_parameter_convert__vL = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 192)
@return_parameter_convert__offset = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 200)
@return_parameter_convert__count = alias i64, ptr getelementptr inbounds (i8, ptr @return_parameter_convert_M3_info, i64 208)

define ptr @return_parameter_convert_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @return_parameter_convert__return_parameter_convert_M3()
  br label %done
done:
  ret ptr @return_parameter_convert_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_u64", linkageName: "return_parameter_convert__ret_u64_u64", scope: !4, file: !3, line: 54, type: !6, scopeLine: 54, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_i8", linkageName: "return_parameter_convert__ret_u64_i8", scope: !4, file: !3, line: 55, type: !6, scopeLine: 55, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_i32", linkageName: "return_parameter_convert__ret_u64_i32", scope: !4, file: !3, line: 56, type: !6, scopeLine: 56, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_LC", linkageName: "return_parameter_convert__ret_u64_LC", scope: !4, file: !3, line: 57, type: !6, scopeLine: 57, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_u16", linkageName: "return_parameter_convert__ret_u64_u16", scope: !4, file: !3, line: 58, type: !6, scopeLine: 58, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_I", linkageName: "return_parameter_convert__ret_u64_I", scope: !4, file: !3, line: 59, type: !6, scopeLine: 59, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_i64", linkageName: "return_parameter_convert__ret_u64_i64", scope: !4, file: !3, line: 60, type: !6, scopeLine: 60, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_C", linkageName: "return_parameter_convert__ret_u64_C", scope: !4, file: !3, line: 61, type: !6, scopeLine: 61, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_i16", linkageName: "return_parameter_convert__ret_u64_i16", scope: !4, file: !3, line: 62, type: !6, scopeLine: 62, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_u32", linkageName: "return_parameter_convert__ret_u64_u32", scope: !4, file: !3, line: 63, type: !6, scopeLine: 63, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_u8", linkageName: "return_parameter_convert__ret_u64_u8", scope: !4, file: !3, line: 64, type: !6, scopeLine: 64, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "return_parameter_convert__ret_u64_L", linkageName: "return_parameter_convert__ret_u64_L", scope: !4, file: !3, line: 65, type: !6, scopeLine: 65, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_u64", linkageName: "return_parameter_convert__ret_i8_u64", scope: !4, file: !3, line: 66, type: !6, scopeLine: 66, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_i8", linkageName: "return_parameter_convert__ret_i8_i8", scope: !4, file: !3, line: 67, type: !6, scopeLine: 67, unit: !2, spFlags: DISPFlagDefinition)
!44 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_i32", linkageName: "return_parameter_convert__ret_i8_i32", scope: !4, file: !3, line: 68, type: !6, scopeLine: 68, unit: !2, spFlags: DISPFlagDefinition)
!46 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_LC", linkageName: "return_parameter_convert__ret_i8_LC", scope: !4, file: !3, line: 69, type: !6, scopeLine: 69, unit: !2, spFlags: DISPFlagDefinition)
!48 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_u16", linkageName: "return_parameter_convert__ret_i8_u16", scope: !4, file: !3, line: 70, type: !6, scopeLine: 70, unit: !2, spFlags: DISPFlagDefinition)
!50 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_I", linkageName: "return_parameter_convert__ret_i8_I", scope: !4, file: !3, line: 71, type: !6, scopeLine: 71, unit: !2, spFlags: DISPFlagDefinition)
!52 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_i64", linkageName: "return_parameter_convert__ret_i8_i64", scope: !4, file: !3, line: 72, type: !6, scopeLine: 72, unit: !2, spFlags: DISPFlagDefinition)
!54 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_C", linkageName: "return_parameter_convert__ret_i8_C", scope: !4, file: !3, line: 73, type: !6, scopeLine: 73, unit: !2, spFlags: DISPFlagDefinition)
!56 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_i16", linkageName: "return_parameter_convert__ret_i8_i16", scope: !4, file: !3, line: 74, type: !6, scopeLine: 74, unit: !2, spFlags: DISPFlagDefinition)
!58 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_u32", linkageName: "return_parameter_convert__ret_i8_u32", scope: !4, file: !3, line: 75, type: !6, scopeLine: 75, unit: !2, spFlags: DISPFlagDefinition)
!60 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_u8", linkageName: "return_parameter_convert__ret_i8_u8", scope: !4, file: !3, line: 76, type: !6, scopeLine: 76, unit: !2, spFlags: DISPFlagDefinition)
!62 = distinct !DISubprogram(name: "return_parameter_convert__ret_i8_L", linkageName: "return_parameter_convert__ret_i8_L", scope: !4, file: !3, line: 77, type: !6, scopeLine: 77, unit: !2, spFlags: DISPFlagDefinition)
!64 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_u64", linkageName: "return_parameter_convert__ret_i32_u64", scope: !4, file: !3, line: 78, type: !6, scopeLine: 78, unit: !2, spFlags: DISPFlagDefinition)
!66 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_i8", linkageName: "return_parameter_convert__ret_i32_i8", scope: !4, file: !3, line: 79, type: !6, scopeLine: 79, unit: !2, spFlags: DISPFlagDefinition)
!68 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_i32", linkageName: "return_parameter_convert__ret_i32_i32", scope: !4, file: !3, line: 80, type: !6, scopeLine: 80, unit: !2, spFlags: DISPFlagDefinition)
!70 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_LC", linkageName: "return_parameter_convert__ret_i32_LC", scope: !4, file: !3, line: 81, type: !6, scopeLine: 81, unit: !2, spFlags: DISPFlagDefinition)
!72 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_u16", linkageName: "return_parameter_convert__ret_i32_u16", scope: !4, file: !3, line: 82, type: !6, scopeLine: 82, unit: !2, spFlags: DISPFlagDefinition)
!74 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_I", linkageName: "return_parameter_convert__ret_i32_I", scope: !4, file: !3, line: 83, type: !6, scopeLine: 83, unit: !2, spFlags: DISPFlagDefinition)
!76 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_i64", linkageName: "return_parameter_convert__ret_i32_i64", scope: !4, file: !3, line: 84, type: !6, scopeLine: 84, unit: !2, spFlags: DISPFlagDefinition)
!78 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_C", linkageName: "return_parameter_convert__ret_i32_C", scope: !4, file: !3, line: 85, type: !6, scopeLine: 85, unit: !2, spFlags: DISPFlagDefinition)
!80 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_i16", linkageName: "return_parameter_convert__ret_i32_i16", scope: !4, file: !3, line: 86, type: !6, scopeLine: 86, unit: !2, spFlags: DISPFlagDefinition)
!82 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_u32", linkageName: "return_parameter_convert__ret_i32_u32", scope: !4, file: !3, line: 87, type: !6, scopeLine: 87, unit: !2, spFlags: DISPFlagDefinition)
!84 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_u8", linkageName: "return_parameter_convert__ret_i32_u8", scope: !4, file: !3, line: 88, type: !6, scopeLine: 88, unit: !2, spFlags: DISPFlagDefinition)
!86 = distinct !DISubprogram(name: "return_parameter_convert__ret_i32_L", linkageName: "return_parameter_convert__ret_i32_L", scope: !4, file: !3, line: 89, type: !6, scopeLine: 89, unit: !2, spFlags: DISPFlagDefinition)
!88 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_u64", linkageName: "return_parameter_convert__ret_LC_u64", scope: !4, file: !3, line: 90, type: !6, scopeLine: 90, unit: !2, spFlags: DISPFlagDefinition)
!90 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_i8", linkageName: "return_parameter_convert__ret_LC_i8", scope: !4, file: !3, line: 91, type: !6, scopeLine: 91, unit: !2, spFlags: DISPFlagDefinition)
!92 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_i32", linkageName: "return_parameter_convert__ret_LC_i32", scope: !4, file: !3, line: 92, type: !6, scopeLine: 92, unit: !2, spFlags: DISPFlagDefinition)
!94 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_LC", linkageName: "return_parameter_convert__ret_LC_LC", scope: !4, file: !3, line: 93, type: !6, scopeLine: 93, unit: !2, spFlags: DISPFlagDefinition)
!96 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_u16", linkageName: "return_parameter_convert__ret_LC_u16", scope: !4, file: !3, line: 94, type: !6, scopeLine: 94, unit: !2, spFlags: DISPFlagDefinition)
!98 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_I", linkageName: "return_parameter_convert__ret_LC_I", scope: !4, file: !3, line: 95, type: !6, scopeLine: 95, unit: !2, spFlags: DISPFlagDefinition)
!100 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_i64", linkageName: "return_parameter_convert__ret_LC_i64", scope: !4, file: !3, line: 96, type: !6, scopeLine: 96, unit: !2, spFlags: DISPFlagDefinition)
!102 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_C", linkageName: "return_parameter_convert__ret_LC_C", scope: !4, file: !3, line: 97, type: !6, scopeLine: 97, unit: !2, spFlags: DISPFlagDefinition)
!104 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_i16", linkageName: "return_parameter_convert__ret_LC_i16", scope: !4, file: !3, line: 98, type: !6, scopeLine: 98, unit: !2, spFlags: DISPFlagDefinition)
!106 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_u32", linkageName: "return_parameter_convert__ret_LC_u32", scope: !4, file: !3, line: 99, type: !6, scopeLine: 99, unit: !2, spFlags: DISPFlagDefinition)
!108 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_u8", linkageName: "return_parameter_convert__ret_LC_u8", scope: !4, file: !3, line: 100, type: !6, scopeLine: 100, unit: !2, spFlags: DISPFlagDefinition)
!110 = distinct !DISubprogram(name: "return_parameter_convert__ret_LC_L", linkageName: "return_parameter_convert__ret_LC_L", scope: !4, file: !3, line: 101, type: !6, scopeLine: 101, unit: !2, spFlags: DISPFlagDefinition)
!112 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_u64", linkageName: "return_parameter_convert__ret_u16_u64", scope: !4, file: !3, line: 102, type: !6, scopeLine: 102, unit: !2, spFlags: DISPFlagDefinition)
!114 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_i8", linkageName: "return_parameter_convert__ret_u16_i8", scope: !4, file: !3, line: 103, type: !6, scopeLine: 103, unit: !2, spFlags: DISPFlagDefinition)
!116 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_i32", linkageName: "return_parameter_convert__ret_u16_i32", scope: !4, file: !3, line: 104, type: !6, scopeLine: 104, unit: !2, spFlags: DISPFlagDefinition)
!118 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_LC", linkageName: "return_parameter_convert__ret_u16_LC", scope: !4, file: !3, line: 105, type: !6, scopeLine: 105, unit: !2, spFlags: DISPFlagDefinition)
!120 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_u16", linkageName: "return_parameter_convert__ret_u16_u16", scope: !4, file: !3, line: 106, type: !6, scopeLine: 106, unit: !2, spFlags: DISPFlagDefinition)
!122 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_I", linkageName: "return_parameter_convert__ret_u16_I", scope: !4, file: !3, line: 107, type: !6, scopeLine: 107, unit: !2, spFlags: DISPFlagDefinition)
!124 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_i64", linkageName: "return_parameter_convert__ret_u16_i64", scope: !4, file: !3, line: 108, type: !6, scopeLine: 108, unit: !2, spFlags: DISPFlagDefinition)
!126 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_C", linkageName: "return_parameter_convert__ret_u16_C", scope: !4, file: !3, line: 109, type: !6, scopeLine: 109, unit: !2, spFlags: DISPFlagDefinition)
!128 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_i16", linkageName: "return_parameter_convert__ret_u16_i16", scope: !4, file: !3, line: 110, type: !6, scopeLine: 110, unit: !2, spFlags: DISPFlagDefinition)
!130 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_u32", linkageName: "return_parameter_convert__ret_u16_u32", scope: !4, file: !3, line: 111, type: !6, scopeLine: 111, unit: !2, spFlags: DISPFlagDefinition)
!132 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_u8", linkageName: "return_parameter_convert__ret_u16_u8", scope: !4, file: !3, line: 112, type: !6, scopeLine: 112, unit: !2, spFlags: DISPFlagDefinition)
!134 = distinct !DISubprogram(name: "return_parameter_convert__ret_u16_L", linkageName: "return_parameter_convert__ret_u16_L", scope: !4, file: !3, line: 113, type: !6, scopeLine: 113, unit: !2, spFlags: DISPFlagDefinition)
!136 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_u64", linkageName: "return_parameter_convert__ret_I_u64", scope: !4, file: !3, line: 114, type: !6, scopeLine: 114, unit: !2, spFlags: DISPFlagDefinition)
!138 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_i8", linkageName: "return_parameter_convert__ret_I_i8", scope: !4, file: !3, line: 115, type: !6, scopeLine: 115, unit: !2, spFlags: DISPFlagDefinition)
!140 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_i32", linkageName: "return_parameter_convert__ret_I_i32", scope: !4, file: !3, line: 116, type: !6, scopeLine: 116, unit: !2, spFlags: DISPFlagDefinition)
!142 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_LC", linkageName: "return_parameter_convert__ret_I_LC", scope: !4, file: !3, line: 117, type: !6, scopeLine: 117, unit: !2, spFlags: DISPFlagDefinition)
!144 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_u16", linkageName: "return_parameter_convert__ret_I_u16", scope: !4, file: !3, line: 118, type: !6, scopeLine: 118, unit: !2, spFlags: DISPFlagDefinition)
!146 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_I", linkageName: "return_parameter_convert__ret_I_I", scope: !4, file: !3, line: 119, type: !6, scopeLine: 119, unit: !2, spFlags: DISPFlagDefinition)
!148 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_i64", linkageName: "return_parameter_convert__ret_I_i64", scope: !4, file: !3, line: 120, type: !6, scopeLine: 120, unit: !2, spFlags: DISPFlagDefinition)
!150 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_C", linkageName: "return_parameter_convert__ret_I_C", scope: !4, file: !3, line: 121, type: !6, scopeLine: 121, unit: !2, spFlags: DISPFlagDefinition)
!152 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_i16", linkageName: "return_parameter_convert__ret_I_i16", scope: !4, file: !3, line: 122, type: !6, scopeLine: 122, unit: !2, spFlags: DISPFlagDefinition)
!154 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_u32", linkageName: "return_parameter_convert__ret_I_u32", scope: !4, file: !3, line: 123, type: !6, scopeLine: 123, unit: !2, spFlags: DISPFlagDefinition)
!156 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_u8", linkageName: "return_parameter_convert__ret_I_u8", scope: !4, file: !3, line: 124, type: !6, scopeLine: 124, unit: !2, spFlags: DISPFlagDefinition)
!158 = distinct !DISubprogram(name: "return_parameter_convert__ret_I_L", linkageName: "return_parameter_convert__ret_I_L", scope: !4, file: !3, line: 125, type: !6, scopeLine: 125, unit: !2, spFlags: DISPFlagDefinition)
!160 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_u64", linkageName: "return_parameter_convert__ret_i64_u64", scope: !4, file: !3, line: 126, type: !6, scopeLine: 126, unit: !2, spFlags: DISPFlagDefinition)
!162 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_i8", linkageName: "return_parameter_convert__ret_i64_i8", scope: !4, file: !3, line: 127, type: !6, scopeLine: 127, unit: !2, spFlags: DISPFlagDefinition)
!164 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_i32", linkageName: "return_parameter_convert__ret_i64_i32", scope: !4, file: !3, line: 128, type: !6, scopeLine: 128, unit: !2, spFlags: DISPFlagDefinition)
!166 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_LC", linkageName: "return_parameter_convert__ret_i64_LC", scope: !4, file: !3, line: 129, type: !6, scopeLine: 129, unit: !2, spFlags: DISPFlagDefinition)
!168 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_u16", linkageName: "return_parameter_convert__ret_i64_u16", scope: !4, file: !3, line: 130, type: !6, scopeLine: 130, unit: !2, spFlags: DISPFlagDefinition)
!170 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_I", linkageName: "return_parameter_convert__ret_i64_I", scope: !4, file: !3, line: 131, type: !6, scopeLine: 131, unit: !2, spFlags: DISPFlagDefinition)
!172 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_i64", linkageName: "return_parameter_convert__ret_i64_i64", scope: !4, file: !3, line: 132, type: !6, scopeLine: 132, unit: !2, spFlags: DISPFlagDefinition)
!174 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_C", linkageName: "return_parameter_convert__ret_i64_C", scope: !4, file: !3, line: 133, type: !6, scopeLine: 133, unit: !2, spFlags: DISPFlagDefinition)
!176 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_i16", linkageName: "return_parameter_convert__ret_i64_i16", scope: !4, file: !3, line: 134, type: !6, scopeLine: 134, unit: !2, spFlags: DISPFlagDefinition)
!178 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_u32", linkageName: "return_parameter_convert__ret_i64_u32", scope: !4, file: !3, line: 135, type: !6, scopeLine: 135, unit: !2, spFlags: DISPFlagDefinition)
!180 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_u8", linkageName: "return_parameter_convert__ret_i64_u8", scope: !4, file: !3, line: 136, type: !6, scopeLine: 136, unit: !2, spFlags: DISPFlagDefinition)
!182 = distinct !DISubprogram(name: "return_parameter_convert__ret_i64_L", linkageName: "return_parameter_convert__ret_i64_L", scope: !4, file: !3, line: 137, type: !6, scopeLine: 137, unit: !2, spFlags: DISPFlagDefinition)
!184 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_u64", linkageName: "return_parameter_convert__ret_C_u64", scope: !4, file: !3, line: 138, type: !6, scopeLine: 138, unit: !2, spFlags: DISPFlagDefinition)
!186 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_i8", linkageName: "return_parameter_convert__ret_C_i8", scope: !4, file: !3, line: 139, type: !6, scopeLine: 139, unit: !2, spFlags: DISPFlagDefinition)
!188 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_i32", linkageName: "return_parameter_convert__ret_C_i32", scope: !4, file: !3, line: 140, type: !6, scopeLine: 140, unit: !2, spFlags: DISPFlagDefinition)
!190 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_LC", linkageName: "return_parameter_convert__ret_C_LC", scope: !4, file: !3, line: 141, type: !6, scopeLine: 141, unit: !2, spFlags: DISPFlagDefinition)
!192 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_u16", linkageName: "return_parameter_convert__ret_C_u16", scope: !4, file: !3, line: 142, type: !6, scopeLine: 142, unit: !2, spFlags: DISPFlagDefinition)
!194 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_I", linkageName: "return_parameter_convert__ret_C_I", scope: !4, file: !3, line: 143, type: !6, scopeLine: 143, unit: !2, spFlags: DISPFlagDefinition)
!196 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_i64", linkageName: "return_parameter_convert__ret_C_i64", scope: !4, file: !3, line: 144, type: !6, scopeLine: 144, unit: !2, spFlags: DISPFlagDefinition)
!198 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_C", linkageName: "return_parameter_convert__ret_C_C", scope: !4, file: !3, line: 145, type: !6, scopeLine: 145, unit: !2, spFlags: DISPFlagDefinition)
!200 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_i16", linkageName: "return_parameter_convert__ret_C_i16", scope: !4, file: !3, line: 146, type: !6, scopeLine: 146, unit: !2, spFlags: DISPFlagDefinition)
!202 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_u32", linkageName: "return_parameter_convert__ret_C_u32", scope: !4, file: !3, line: 147, type: !6, scopeLine: 147, unit: !2, spFlags: DISPFlagDefinition)
!204 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_u8", linkageName: "return_parameter_convert__ret_C_u8", scope: !4, file: !3, line: 148, type: !6, scopeLine: 148, unit: !2, spFlags: DISPFlagDefinition)
!206 = distinct !DISubprogram(name: "return_parameter_convert__ret_C_L", linkageName: "return_parameter_convert__ret_C_L", scope: !4, file: !3, line: 149, type: !6, scopeLine: 149, unit: !2, spFlags: DISPFlagDefinition)
!208 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_u64", linkageName: "return_parameter_convert__ret_i16_u64", scope: !4, file: !3, line: 150, type: !6, scopeLine: 150, unit: !2, spFlags: DISPFlagDefinition)
!210 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_i8", linkageName: "return_parameter_convert__ret_i16_i8", scope: !4, file: !3, line: 151, type: !6, scopeLine: 151, unit: !2, spFlags: DISPFlagDefinition)
!212 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_i32", linkageName: "return_parameter_convert__ret_i16_i32", scope: !4, file: !3, line: 152, type: !6, scopeLine: 152, unit: !2, spFlags: DISPFlagDefinition)
!214 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_LC", linkageName: "return_parameter_convert__ret_i16_LC", scope: !4, file: !3, line: 153, type: !6, scopeLine: 153, unit: !2, spFlags: DISPFlagDefinition)
!216 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_u16", linkageName: "return_parameter_convert__ret_i16_u16", scope: !4, file: !3, line: 154, type: !6, scopeLine: 154, unit: !2, spFlags: DISPFlagDefinition)
!218 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_I", linkageName: "return_parameter_convert__ret_i16_I", scope: !4, file: !3, line: 155, type: !6, scopeLine: 155, unit: !2, spFlags: DISPFlagDefinition)
!220 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_i64", linkageName: "return_parameter_convert__ret_i16_i64", scope: !4, file: !3, line: 156, type: !6, scopeLine: 156, unit: !2, spFlags: DISPFlagDefinition)
!222 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_C", linkageName: "return_parameter_convert__ret_i16_C", scope: !4, file: !3, line: 157, type: !6, scopeLine: 157, unit: !2, spFlags: DISPFlagDefinition)
!224 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_i16", linkageName: "return_parameter_convert__ret_i16_i16", scope: !4, file: !3, line: 158, type: !6, scopeLine: 158, unit: !2, spFlags: DISPFlagDefinition)
!226 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_u32", linkageName: "return_parameter_convert__ret_i16_u32", scope: !4, file: !3, line: 159, type: !6, scopeLine: 159, unit: !2, spFlags: DISPFlagDefinition)
!228 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_u8", linkageName: "return_parameter_convert__ret_i16_u8", scope: !4, file: !3, line: 160, type: !6, scopeLine: 160, unit: !2, spFlags: DISPFlagDefinition)
!230 = distinct !DISubprogram(name: "return_parameter_convert__ret_i16_L", linkageName: "return_parameter_convert__ret_i16_L", scope: !4, file: !3, line: 161, type: !6, scopeLine: 161, unit: !2, spFlags: DISPFlagDefinition)
!232 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_u64", linkageName: "return_parameter_convert__ret_u32_u64", scope: !4, file: !3, line: 162, type: !6, scopeLine: 162, unit: !2, spFlags: DISPFlagDefinition)
!234 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_i8", linkageName: "return_parameter_convert__ret_u32_i8", scope: !4, file: !3, line: 163, type: !6, scopeLine: 163, unit: !2, spFlags: DISPFlagDefinition)
!236 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_i32", linkageName: "return_parameter_convert__ret_u32_i32", scope: !4, file: !3, line: 164, type: !6, scopeLine: 164, unit: !2, spFlags: DISPFlagDefinition)
!238 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_LC", linkageName: "return_parameter_convert__ret_u32_LC", scope: !4, file: !3, line: 165, type: !6, scopeLine: 165, unit: !2, spFlags: DISPFlagDefinition)
!240 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_u16", linkageName: "return_parameter_convert__ret_u32_u16", scope: !4, file: !3, line: 166, type: !6, scopeLine: 166, unit: !2, spFlags: DISPFlagDefinition)
!242 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_I", linkageName: "return_parameter_convert__ret_u32_I", scope: !4, file: !3, line: 167, type: !6, scopeLine: 167, unit: !2, spFlags: DISPFlagDefinition)
!244 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_i64", linkageName: "return_parameter_convert__ret_u32_i64", scope: !4, file: !3, line: 168, type: !6, scopeLine: 168, unit: !2, spFlags: DISPFlagDefinition)
!246 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_C", linkageName: "return_parameter_convert__ret_u32_C", scope: !4, file: !3, line: 169, type: !6, scopeLine: 169, unit: !2, spFlags: DISPFlagDefinition)
!248 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_i16", linkageName: "return_parameter_convert__ret_u32_i16", scope: !4, file: !3, line: 170, type: !6, scopeLine: 170, unit: !2, spFlags: DISPFlagDefinition)
!250 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_u32", linkageName: "return_parameter_convert__ret_u32_u32", scope: !4, file: !3, line: 171, type: !6, scopeLine: 171, unit: !2, spFlags: DISPFlagDefinition)
!252 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_u8", linkageName: "return_parameter_convert__ret_u32_u8", scope: !4, file: !3, line: 172, type: !6, scopeLine: 172, unit: !2, spFlags: DISPFlagDefinition)
!254 = distinct !DISubprogram(name: "return_parameter_convert__ret_u32_L", linkageName: "return_parameter_convert__ret_u32_L", scope: !4, file: !3, line: 173, type: !6, scopeLine: 173, unit: !2, spFlags: DISPFlagDefinition)
!256 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_u64", linkageName: "return_parameter_convert__ret_u8_u64", scope: !4, file: !3, line: 174, type: !6, scopeLine: 174, unit: !2, spFlags: DISPFlagDefinition)
!258 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_i8", linkageName: "return_parameter_convert__ret_u8_i8", scope: !4, file: !3, line: 175, type: !6, scopeLine: 175, unit: !2, spFlags: DISPFlagDefinition)
!260 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_i32", linkageName: "return_parameter_convert__ret_u8_i32", scope: !4, file: !3, line: 176, type: !6, scopeLine: 176, unit: !2, spFlags: DISPFlagDefinition)
!262 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_LC", linkageName: "return_parameter_convert__ret_u8_LC", scope: !4, file: !3, line: 177, type: !6, scopeLine: 177, unit: !2, spFlags: DISPFlagDefinition)
!264 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_u16", linkageName: "return_parameter_convert__ret_u8_u16", scope: !4, file: !3, line: 178, type: !6, scopeLine: 178, unit: !2, spFlags: DISPFlagDefinition)
!266 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_I", linkageName: "return_parameter_convert__ret_u8_I", scope: !4, file: !3, line: 179, type: !6, scopeLine: 179, unit: !2, spFlags: DISPFlagDefinition)
!268 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_i64", linkageName: "return_parameter_convert__ret_u8_i64", scope: !4, file: !3, line: 180, type: !6, scopeLine: 180, unit: !2, spFlags: DISPFlagDefinition)
!270 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_C", linkageName: "return_parameter_convert__ret_u8_C", scope: !4, file: !3, line: 181, type: !6, scopeLine: 181, unit: !2, spFlags: DISPFlagDefinition)
!272 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_i16", linkageName: "return_parameter_convert__ret_u8_i16", scope: !4, file: !3, line: 182, type: !6, scopeLine: 182, unit: !2, spFlags: DISPFlagDefinition)
!274 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_u32", linkageName: "return_parameter_convert__ret_u8_u32", scope: !4, file: !3, line: 183, type: !6, scopeLine: 183, unit: !2, spFlags: DISPFlagDefinition)
!276 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_u8", linkageName: "return_parameter_convert__ret_u8_u8", scope: !4, file: !3, line: 184, type: !6, scopeLine: 184, unit: !2, spFlags: DISPFlagDefinition)
!278 = distinct !DISubprogram(name: "return_parameter_convert__ret_u8_L", linkageName: "return_parameter_convert__ret_u8_L", scope: !4, file: !3, line: 185, type: !6, scopeLine: 185, unit: !2, spFlags: DISPFlagDefinition)
!280 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_u64", linkageName: "return_parameter_convert__ret_L_u64", scope: !4, file: !3, line: 186, type: !6, scopeLine: 186, unit: !2, spFlags: DISPFlagDefinition)
!282 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_i8", linkageName: "return_parameter_convert__ret_L_i8", scope: !4, file: !3, line: 187, type: !6, scopeLine: 187, unit: !2, spFlags: DISPFlagDefinition)
!284 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_i32", linkageName: "return_parameter_convert__ret_L_i32", scope: !4, file: !3, line: 188, type: !6, scopeLine: 188, unit: !2, spFlags: DISPFlagDefinition)
!286 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_LC", linkageName: "return_parameter_convert__ret_L_LC", scope: !4, file: !3, line: 189, type: !6, scopeLine: 189, unit: !2, spFlags: DISPFlagDefinition)
!288 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_u16", linkageName: "return_parameter_convert__ret_L_u16", scope: !4, file: !3, line: 190, type: !6, scopeLine: 190, unit: !2, spFlags: DISPFlagDefinition)
!290 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_I", linkageName: "return_parameter_convert__ret_L_I", scope: !4, file: !3, line: 191, type: !6, scopeLine: 191, unit: !2, spFlags: DISPFlagDefinition)
!292 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_i64", linkageName: "return_parameter_convert__ret_L_i64", scope: !4, file: !3, line: 192, type: !6, scopeLine: 192, unit: !2, spFlags: DISPFlagDefinition)
!294 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_C", linkageName: "return_parameter_convert__ret_L_C", scope: !4, file: !3, line: 193, type: !6, scopeLine: 193, unit: !2, spFlags: DISPFlagDefinition)
!296 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_i16", linkageName: "return_parameter_convert__ret_L_i16", scope: !4, file: !3, line: 194, type: !6, scopeLine: 194, unit: !2, spFlags: DISPFlagDefinition)
!298 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_u32", linkageName: "return_parameter_convert__ret_L_u32", scope: !4, file: !3, line: 195, type: !6, scopeLine: 195, unit: !2, spFlags: DISPFlagDefinition)
!300 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_u8", linkageName: "return_parameter_convert__ret_L_u8", scope: !4, file: !3, line: 196, type: !6, scopeLine: 196, unit: !2, spFlags: DISPFlagDefinition)
!302 = distinct !DISubprogram(name: "return_parameter_convert__ret_L_L", linkageName: "return_parameter_convert__ret_L_L", scope: !4, file: !3, line: 197, type: !6, scopeLine: 197, unit: !2, spFlags: DISPFlagDefinition)
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
!304 = !DILocalVariable(name: "_result", scope: !16, file: !3, line: 54, type: !7)
!305 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 54, type: !7)
!306 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 55, type: !7)
!307 = !DILocalVariable(name: "a", scope: !18, file: !3, line: 55, type: !7)
!308 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 56, type: !7)
!309 = !DILocalVariable(name: "a", scope: !20, file: !3, line: 56, type: !7)
!310 = !DILocalVariable(name: "_result", scope: !22, file: !3, line: 57, type: !7)
!311 = !DILocalVariable(name: "a", scope: !22, file: !3, line: 57, type: !7)
!312 = !DILocalVariable(name: "_result", scope: !24, file: !3, line: 58, type: !7)
!313 = !DILocalVariable(name: "a", scope: !24, file: !3, line: 58, type: !7)
!314 = !DILocalVariable(name: "_result", scope: !26, file: !3, line: 59, type: !7)
!315 = !DILocalVariable(name: "a", scope: !26, file: !3, line: 59, type: !7)
!316 = !DILocalVariable(name: "_result", scope: !28, file: !3, line: 60, type: !7)
!317 = !DILocalVariable(name: "a", scope: !28, file: !3, line: 60, type: !7)
!318 = !DILocalVariable(name: "_result", scope: !30, file: !3, line: 61, type: !7)
!319 = !DILocalVariable(name: "a", scope: !30, file: !3, line: 61, type: !7)
!320 = !DILocalVariable(name: "_result", scope: !32, file: !3, line: 62, type: !7)
!321 = !DILocalVariable(name: "a", scope: !32, file: !3, line: 62, type: !7)
!322 = !DILocalVariable(name: "_result", scope: !34, file: !3, line: 63, type: !7)
!323 = !DILocalVariable(name: "a", scope: !34, file: !3, line: 63, type: !7)
!324 = !DILocalVariable(name: "_result", scope: !36, file: !3, line: 64, type: !7)
!325 = !DILocalVariable(name: "a", scope: !36, file: !3, line: 64, type: !7)
!326 = !DILocalVariable(name: "_result", scope: !38, file: !3, line: 65, type: !7)
!327 = !DILocalVariable(name: "a", scope: !38, file: !3, line: 65, type: !7)
!328 = !DILocalVariable(name: "_result", scope: !40, file: !3, line: 66, type: !7)
!329 = !DILocalVariable(name: "a", scope: !40, file: !3, line: 66, type: !7)
!330 = !DILocalVariable(name: "_result", scope: !42, file: !3, line: 67, type: !7)
!331 = !DILocalVariable(name: "a", scope: !42, file: !3, line: 67, type: !7)
!332 = !DILocalVariable(name: "_result", scope: !44, file: !3, line: 68, type: !7)
!333 = !DILocalVariable(name: "a", scope: !44, file: !3, line: 68, type: !7)
!334 = !DILocalVariable(name: "_result", scope: !46, file: !3, line: 69, type: !7)
!335 = !DILocalVariable(name: "a", scope: !46, file: !3, line: 69, type: !7)
!336 = !DILocalVariable(name: "_result", scope: !48, file: !3, line: 70, type: !7)
!337 = !DILocalVariable(name: "a", scope: !48, file: !3, line: 70, type: !7)
!338 = !DILocalVariable(name: "_result", scope: !50, file: !3, line: 71, type: !7)
!339 = !DILocalVariable(name: "a", scope: !50, file: !3, line: 71, type: !7)
!340 = !DILocalVariable(name: "_result", scope: !52, file: !3, line: 72, type: !7)
!341 = !DILocalVariable(name: "a", scope: !52, file: !3, line: 72, type: !7)
!342 = !DILocalVariable(name: "_result", scope: !54, file: !3, line: 73, type: !7)
!343 = !DILocalVariable(name: "a", scope: !54, file: !3, line: 73, type: !7)
!344 = !DILocalVariable(name: "_result", scope: !56, file: !3, line: 74, type: !7)
!345 = !DILocalVariable(name: "a", scope: !56, file: !3, line: 74, type: !7)
!346 = !DILocalVariable(name: "_result", scope: !58, file: !3, line: 75, type: !7)
!347 = !DILocalVariable(name: "a", scope: !58, file: !3, line: 75, type: !7)
!348 = !DILocalVariable(name: "_result", scope: !60, file: !3, line: 76, type: !7)
!349 = !DILocalVariable(name: "a", scope: !60, file: !3, line: 76, type: !7)
!350 = !DILocalVariable(name: "_result", scope: !62, file: !3, line: 77, type: !7)
!351 = !DILocalVariable(name: "a", scope: !62, file: !3, line: 77, type: !7)
!352 = !DILocalVariable(name: "_result", scope: !64, file: !3, line: 78, type: !7)
!353 = !DILocalVariable(name: "a", scope: !64, file: !3, line: 78, type: !7)
!354 = !DILocalVariable(name: "_result", scope: !66, file: !3, line: 79, type: !7)
!355 = !DILocalVariable(name: "a", scope: !66, file: !3, line: 79, type: !7)
!356 = !DILocalVariable(name: "_result", scope: !68, file: !3, line: 80, type: !7)
!357 = !DILocalVariable(name: "a", scope: !68, file: !3, line: 80, type: !7)
!358 = !DILocalVariable(name: "_result", scope: !70, file: !3, line: 81, type: !7)
!359 = !DILocalVariable(name: "a", scope: !70, file: !3, line: 81, type: !7)
!360 = !DILocalVariable(name: "_result", scope: !72, file: !3, line: 82, type: !7)
!361 = !DILocalVariable(name: "a", scope: !72, file: !3, line: 82, type: !7)
!362 = !DILocalVariable(name: "_result", scope: !74, file: !3, line: 83, type: !7)
!363 = !DILocalVariable(name: "a", scope: !74, file: !3, line: 83, type: !7)
!364 = !DILocalVariable(name: "_result", scope: !76, file: !3, line: 84, type: !7)
!365 = !DILocalVariable(name: "a", scope: !76, file: !3, line: 84, type: !7)
!366 = !DILocalVariable(name: "_result", scope: !78, file: !3, line: 85, type: !7)
!367 = !DILocalVariable(name: "a", scope: !78, file: !3, line: 85, type: !7)
!368 = !DILocalVariable(name: "_result", scope: !80, file: !3, line: 86, type: !7)
!369 = !DILocalVariable(name: "a", scope: !80, file: !3, line: 86, type: !7)
!370 = !DILocalVariable(name: "_result", scope: !82, file: !3, line: 87, type: !7)
!371 = !DILocalVariable(name: "a", scope: !82, file: !3, line: 87, type: !7)
!372 = !DILocalVariable(name: "_result", scope: !84, file: !3, line: 88, type: !7)
!373 = !DILocalVariable(name: "a", scope: !84, file: !3, line: 88, type: !7)
!374 = !DILocalVariable(name: "_result", scope: !86, file: !3, line: 89, type: !7)
!375 = !DILocalVariable(name: "a", scope: !86, file: !3, line: 89, type: !7)
!376 = !DILocalVariable(name: "_result", scope: !88, file: !3, line: 90, type: !7)
!377 = !DILocalVariable(name: "a", scope: !88, file: !3, line: 90, type: !7)
!378 = !DILocalVariable(name: "_result", scope: !90, file: !3, line: 91, type: !7)
!379 = !DILocalVariable(name: "a", scope: !90, file: !3, line: 91, type: !7)
!380 = !DILocalVariable(name: "_result", scope: !92, file: !3, line: 92, type: !7)
!381 = !DILocalVariable(name: "a", scope: !92, file: !3, line: 92, type: !7)
!382 = !DILocalVariable(name: "_result", scope: !94, file: !3, line: 93, type: !7)
!383 = !DILocalVariable(name: "a", scope: !94, file: !3, line: 93, type: !7)
!384 = !DILocalVariable(name: "_result", scope: !96, file: !3, line: 94, type: !7)
!385 = !DILocalVariable(name: "a", scope: !96, file: !3, line: 94, type: !7)
!386 = !DILocalVariable(name: "_result", scope: !98, file: !3, line: 95, type: !7)
!387 = !DILocalVariable(name: "a", scope: !98, file: !3, line: 95, type: !7)
!388 = !DILocalVariable(name: "_result", scope: !100, file: !3, line: 96, type: !7)
!389 = !DILocalVariable(name: "a", scope: !100, file: !3, line: 96, type: !7)
!390 = !DILocalVariable(name: "_result", scope: !102, file: !3, line: 97, type: !7)
!391 = !DILocalVariable(name: "a", scope: !102, file: !3, line: 97, type: !7)
!392 = !DILocalVariable(name: "_result", scope: !104, file: !3, line: 98, type: !7)
!393 = !DILocalVariable(name: "a", scope: !104, file: !3, line: 98, type: !7)
!394 = !DILocalVariable(name: "_result", scope: !106, file: !3, line: 99, type: !7)
!395 = !DILocalVariable(name: "a", scope: !106, file: !3, line: 99, type: !7)
!396 = !DILocalVariable(name: "_result", scope: !108, file: !3, line: 100, type: !7)
!397 = !DILocalVariable(name: "a", scope: !108, file: !3, line: 100, type: !7)
!398 = !DILocalVariable(name: "_result", scope: !110, file: !3, line: 101, type: !7)
!399 = !DILocalVariable(name: "a", scope: !110, file: !3, line: 101, type: !7)
!400 = !DILocalVariable(name: "_result", scope: !112, file: !3, line: 102, type: !7)
!401 = !DILocalVariable(name: "a", scope: !112, file: !3, line: 102, type: !7)
!402 = !DILocalVariable(name: "_result", scope: !114, file: !3, line: 103, type: !7)
!403 = !DILocalVariable(name: "a", scope: !114, file: !3, line: 103, type: !7)
!404 = !DILocalVariable(name: "_result", scope: !116, file: !3, line: 104, type: !7)
!405 = !DILocalVariable(name: "a", scope: !116, file: !3, line: 104, type: !7)
!406 = !DILocalVariable(name: "_result", scope: !118, file: !3, line: 105, type: !7)
!407 = !DILocalVariable(name: "a", scope: !118, file: !3, line: 105, type: !7)
!408 = !DILocalVariable(name: "_result", scope: !120, file: !3, line: 106, type: !7)
!409 = !DILocalVariable(name: "a", scope: !120, file: !3, line: 106, type: !7)
!410 = !DILocalVariable(name: "_result", scope: !122, file: !3, line: 107, type: !7)
!411 = !DILocalVariable(name: "a", scope: !122, file: !3, line: 107, type: !7)
!412 = !DILocalVariable(name: "_result", scope: !124, file: !3, line: 108, type: !7)
!413 = !DILocalVariable(name: "a", scope: !124, file: !3, line: 108, type: !7)
!414 = !DILocalVariable(name: "_result", scope: !126, file: !3, line: 109, type: !7)
!415 = !DILocalVariable(name: "a", scope: !126, file: !3, line: 109, type: !7)
!416 = !DILocalVariable(name: "_result", scope: !128, file: !3, line: 110, type: !7)
!417 = !DILocalVariable(name: "a", scope: !128, file: !3, line: 110, type: !7)
!418 = !DILocalVariable(name: "_result", scope: !130, file: !3, line: 111, type: !7)
!419 = !DILocalVariable(name: "a", scope: !130, file: !3, line: 111, type: !7)
!420 = !DILocalVariable(name: "_result", scope: !132, file: !3, line: 112, type: !7)
!421 = !DILocalVariable(name: "a", scope: !132, file: !3, line: 112, type: !7)
!422 = !DILocalVariable(name: "_result", scope: !134, file: !3, line: 113, type: !7)
!423 = !DILocalVariable(name: "a", scope: !134, file: !3, line: 113, type: !7)
!424 = !DILocalVariable(name: "_result", scope: !136, file: !3, line: 114, type: !7)
!425 = !DILocalVariable(name: "a", scope: !136, file: !3, line: 114, type: !7)
!426 = !DILocalVariable(name: "_result", scope: !138, file: !3, line: 115, type: !7)
!427 = !DILocalVariable(name: "a", scope: !138, file: !3, line: 115, type: !7)
!428 = !DILocalVariable(name: "_result", scope: !140, file: !3, line: 116, type: !7)
!429 = !DILocalVariable(name: "a", scope: !140, file: !3, line: 116, type: !7)
!430 = !DILocalVariable(name: "_result", scope: !142, file: !3, line: 117, type: !7)
!431 = !DILocalVariable(name: "a", scope: !142, file: !3, line: 117, type: !7)
!432 = !DILocalVariable(name: "_result", scope: !144, file: !3, line: 118, type: !7)
!433 = !DILocalVariable(name: "a", scope: !144, file: !3, line: 118, type: !7)
!434 = !DILocalVariable(name: "_result", scope: !146, file: !3, line: 119, type: !7)
!435 = !DILocalVariable(name: "a", scope: !146, file: !3, line: 119, type: !7)
!436 = !DILocalVariable(name: "_result", scope: !148, file: !3, line: 120, type: !7)
!437 = !DILocalVariable(name: "a", scope: !148, file: !3, line: 120, type: !7)
!438 = !DILocalVariable(name: "_result", scope: !150, file: !3, line: 121, type: !7)
!439 = !DILocalVariable(name: "a", scope: !150, file: !3, line: 121, type: !7)
!440 = !DILocalVariable(name: "_result", scope: !152, file: !3, line: 122, type: !7)
!441 = !DILocalVariable(name: "a", scope: !152, file: !3, line: 122, type: !7)
!442 = !DILocalVariable(name: "_result", scope: !154, file: !3, line: 123, type: !7)
!443 = !DILocalVariable(name: "a", scope: !154, file: !3, line: 123, type: !7)
!444 = !DILocalVariable(name: "_result", scope: !156, file: !3, line: 124, type: !7)
!445 = !DILocalVariable(name: "a", scope: !156, file: !3, line: 124, type: !7)
!446 = !DILocalVariable(name: "_result", scope: !158, file: !3, line: 125, type: !7)
!447 = !DILocalVariable(name: "a", scope: !158, file: !3, line: 125, type: !7)
!448 = !DILocalVariable(name: "_result", scope: !160, file: !3, line: 126, type: !7)
!449 = !DILocalVariable(name: "a", scope: !160, file: !3, line: 126, type: !7)
!450 = !DILocalVariable(name: "_result", scope: !162, file: !3, line: 127, type: !7)
!451 = !DILocalVariable(name: "a", scope: !162, file: !3, line: 127, type: !7)
!452 = !DILocalVariable(name: "_result", scope: !164, file: !3, line: 128, type: !7)
!453 = !DILocalVariable(name: "a", scope: !164, file: !3, line: 128, type: !7)
!454 = !DILocalVariable(name: "_result", scope: !166, file: !3, line: 129, type: !7)
!455 = !DILocalVariable(name: "a", scope: !166, file: !3, line: 129, type: !7)
!456 = !DILocalVariable(name: "_result", scope: !168, file: !3, line: 130, type: !7)
!457 = !DILocalVariable(name: "a", scope: !168, file: !3, line: 130, type: !7)
!458 = !DILocalVariable(name: "_result", scope: !170, file: !3, line: 131, type: !7)
!459 = !DILocalVariable(name: "a", scope: !170, file: !3, line: 131, type: !7)
!460 = !DILocalVariable(name: "_result", scope: !172, file: !3, line: 132, type: !7)
!461 = !DILocalVariable(name: "a", scope: !172, file: !3, line: 132, type: !7)
!462 = !DILocalVariable(name: "_result", scope: !174, file: !3, line: 133, type: !7)
!463 = !DILocalVariable(name: "a", scope: !174, file: !3, line: 133, type: !7)
!464 = !DILocalVariable(name: "_result", scope: !176, file: !3, line: 134, type: !7)
!465 = !DILocalVariable(name: "a", scope: !176, file: !3, line: 134, type: !7)
!466 = !DILocalVariable(name: "_result", scope: !178, file: !3, line: 135, type: !7)
!467 = !DILocalVariable(name: "a", scope: !178, file: !3, line: 135, type: !7)
!468 = !DILocalVariable(name: "_result", scope: !180, file: !3, line: 136, type: !7)
!469 = !DILocalVariable(name: "a", scope: !180, file: !3, line: 136, type: !7)
!470 = !DILocalVariable(name: "_result", scope: !182, file: !3, line: 137, type: !7)
!471 = !DILocalVariable(name: "a", scope: !182, file: !3, line: 137, type: !7)
!472 = !DILocalVariable(name: "_result", scope: !184, file: !3, line: 138, type: !7)
!473 = !DILocalVariable(name: "a", scope: !184, file: !3, line: 138, type: !7)
!474 = !DILocalVariable(name: "_result", scope: !186, file: !3, line: 139, type: !7)
!475 = !DILocalVariable(name: "a", scope: !186, file: !3, line: 139, type: !7)
!476 = !DILocalVariable(name: "_result", scope: !188, file: !3, line: 140, type: !7)
!477 = !DILocalVariable(name: "a", scope: !188, file: !3, line: 140, type: !7)
!478 = !DILocalVariable(name: "_result", scope: !190, file: !3, line: 141, type: !7)
!479 = !DILocalVariable(name: "a", scope: !190, file: !3, line: 141, type: !7)
!480 = !DILocalVariable(name: "_result", scope: !192, file: !3, line: 142, type: !7)
!481 = !DILocalVariable(name: "a", scope: !192, file: !3, line: 142, type: !7)
!482 = !DILocalVariable(name: "_result", scope: !194, file: !3, line: 143, type: !7)
!483 = !DILocalVariable(name: "a", scope: !194, file: !3, line: 143, type: !7)
!484 = !DILocalVariable(name: "_result", scope: !196, file: !3, line: 144, type: !7)
!485 = !DILocalVariable(name: "a", scope: !196, file: !3, line: 144, type: !7)
!486 = !DILocalVariable(name: "_result", scope: !198, file: !3, line: 145, type: !7)
!487 = !DILocalVariable(name: "a", scope: !198, file: !3, line: 145, type: !7)
!488 = !DILocalVariable(name: "_result", scope: !200, file: !3, line: 146, type: !7)
!489 = !DILocalVariable(name: "a", scope: !200, file: !3, line: 146, type: !7)
!490 = !DILocalVariable(name: "_result", scope: !202, file: !3, line: 147, type: !7)
!491 = !DILocalVariable(name: "a", scope: !202, file: !3, line: 147, type: !7)
!492 = !DILocalVariable(name: "_result", scope: !204, file: !3, line: 148, type: !7)
!493 = !DILocalVariable(name: "a", scope: !204, file: !3, line: 148, type: !7)
!494 = !DILocalVariable(name: "_result", scope: !206, file: !3, line: 149, type: !7)
!495 = !DILocalVariable(name: "a", scope: !206, file: !3, line: 149, type: !7)
!496 = !DILocalVariable(name: "_result", scope: !208, file: !3, line: 150, type: !7)
!497 = !DILocalVariable(name: "a", scope: !208, file: !3, line: 150, type: !7)
!498 = !DILocalVariable(name: "_result", scope: !210, file: !3, line: 151, type: !7)
!499 = !DILocalVariable(name: "a", scope: !210, file: !3, line: 151, type: !7)
!500 = !DILocalVariable(name: "_result", scope: !212, file: !3, line: 152, type: !7)
!501 = !DILocalVariable(name: "a", scope: !212, file: !3, line: 152, type: !7)
!502 = !DILocalVariable(name: "_result", scope: !214, file: !3, line: 153, type: !7)
!503 = !DILocalVariable(name: "a", scope: !214, file: !3, line: 153, type: !7)
!504 = !DILocalVariable(name: "_result", scope: !216, file: !3, line: 154, type: !7)
!505 = !DILocalVariable(name: "a", scope: !216, file: !3, line: 154, type: !7)
!506 = !DILocalVariable(name: "_result", scope: !218, file: !3, line: 155, type: !7)
!507 = !DILocalVariable(name: "a", scope: !218, file: !3, line: 155, type: !7)
!508 = !DILocalVariable(name: "_result", scope: !220, file: !3, line: 156, type: !7)
!509 = !DILocalVariable(name: "a", scope: !220, file: !3, line: 156, type: !7)
!510 = !DILocalVariable(name: "_result", scope: !222, file: !3, line: 157, type: !7)
!511 = !DILocalVariable(name: "a", scope: !222, file: !3, line: 157, type: !7)
!512 = !DILocalVariable(name: "_result", scope: !224, file: !3, line: 158, type: !7)
!513 = !DILocalVariable(name: "a", scope: !224, file: !3, line: 158, type: !7)
!514 = !DILocalVariable(name: "_result", scope: !226, file: !3, line: 159, type: !7)
!515 = !DILocalVariable(name: "a", scope: !226, file: !3, line: 159, type: !7)
!516 = !DILocalVariable(name: "_result", scope: !228, file: !3, line: 160, type: !7)
!517 = !DILocalVariable(name: "a", scope: !228, file: !3, line: 160, type: !7)
!518 = !DILocalVariable(name: "_result", scope: !230, file: !3, line: 161, type: !7)
!519 = !DILocalVariable(name: "a", scope: !230, file: !3, line: 161, type: !7)
!520 = !DILocalVariable(name: "_result", scope: !232, file: !3, line: 162, type: !7)
!521 = !DILocalVariable(name: "a", scope: !232, file: !3, line: 162, type: !7)
!522 = !DILocalVariable(name: "_result", scope: !234, file: !3, line: 163, type: !7)
!523 = !DILocalVariable(name: "a", scope: !234, file: !3, line: 163, type: !7)
!524 = !DILocalVariable(name: "_result", scope: !236, file: !3, line: 164, type: !7)
!525 = !DILocalVariable(name: "a", scope: !236, file: !3, line: 164, type: !7)
!526 = !DILocalVariable(name: "_result", scope: !238, file: !3, line: 165, type: !7)
!527 = !DILocalVariable(name: "a", scope: !238, file: !3, line: 165, type: !7)
!528 = !DILocalVariable(name: "_result", scope: !240, file: !3, line: 166, type: !7)
!529 = !DILocalVariable(name: "a", scope: !240, file: !3, line: 166, type: !7)
!530 = !DILocalVariable(name: "_result", scope: !242, file: !3, line: 167, type: !7)
!531 = !DILocalVariable(name: "a", scope: !242, file: !3, line: 167, type: !7)
!532 = !DILocalVariable(name: "_result", scope: !244, file: !3, line: 168, type: !7)
!533 = !DILocalVariable(name: "a", scope: !244, file: !3, line: 168, type: !7)
!534 = !DILocalVariable(name: "_result", scope: !246, file: !3, line: 169, type: !7)
!535 = !DILocalVariable(name: "a", scope: !246, file: !3, line: 169, type: !7)
!536 = !DILocalVariable(name: "_result", scope: !248, file: !3, line: 170, type: !7)
!537 = !DILocalVariable(name: "a", scope: !248, file: !3, line: 170, type: !7)
!538 = !DILocalVariable(name: "_result", scope: !250, file: !3, line: 171, type: !7)
!539 = !DILocalVariable(name: "a", scope: !250, file: !3, line: 171, type: !7)
!540 = !DILocalVariable(name: "_result", scope: !252, file: !3, line: 172, type: !7)
!541 = !DILocalVariable(name: "a", scope: !252, file: !3, line: 172, type: !7)
!542 = !DILocalVariable(name: "_result", scope: !254, file: !3, line: 173, type: !7)
!543 = !DILocalVariable(name: "a", scope: !254, file: !3, line: 173, type: !7)
!544 = !DILocalVariable(name: "_result", scope: !256, file: !3, line: 174, type: !7)
!545 = !DILocalVariable(name: "a", scope: !256, file: !3, line: 174, type: !7)
!546 = !DILocalVariable(name: "_result", scope: !258, file: !3, line: 175, type: !7)
!547 = !DILocalVariable(name: "a", scope: !258, file: !3, line: 175, type: !7)
!548 = !DILocalVariable(name: "_result", scope: !260, file: !3, line: 176, type: !7)
!549 = !DILocalVariable(name: "a", scope: !260, file: !3, line: 176, type: !7)
!550 = !DILocalVariable(name: "_result", scope: !262, file: !3, line: 177, type: !7)
!551 = !DILocalVariable(name: "a", scope: !262, file: !3, line: 177, type: !7)
!552 = !DILocalVariable(name: "_result", scope: !264, file: !3, line: 178, type: !7)
!553 = !DILocalVariable(name: "a", scope: !264, file: !3, line: 178, type: !7)
!554 = !DILocalVariable(name: "_result", scope: !266, file: !3, line: 179, type: !7)
!555 = !DILocalVariable(name: "a", scope: !266, file: !3, line: 179, type: !7)
!556 = !DILocalVariable(name: "_result", scope: !268, file: !3, line: 180, type: !7)
!557 = !DILocalVariable(name: "a", scope: !268, file: !3, line: 180, type: !7)
!558 = !DILocalVariable(name: "_result", scope: !270, file: !3, line: 181, type: !7)
!559 = !DILocalVariable(name: "a", scope: !270, file: !3, line: 181, type: !7)
!560 = !DILocalVariable(name: "_result", scope: !272, file: !3, line: 182, type: !7)
!561 = !DILocalVariable(name: "a", scope: !272, file: !3, line: 182, type: !7)
!562 = !DILocalVariable(name: "_result", scope: !274, file: !3, line: 183, type: !7)
!563 = !DILocalVariable(name: "a", scope: !274, file: !3, line: 183, type: !7)
!564 = !DILocalVariable(name: "_result", scope: !276, file: !3, line: 184, type: !7)
!565 = !DILocalVariable(name: "a", scope: !276, file: !3, line: 184, type: !7)
!566 = !DILocalVariable(name: "_result", scope: !278, file: !3, line: 185, type: !7)
!567 = !DILocalVariable(name: "a", scope: !278, file: !3, line: 185, type: !7)
!568 = !DILocalVariable(name: "_result", scope: !280, file: !3, line: 186, type: !7)
!569 = !DILocalVariable(name: "a", scope: !280, file: !3, line: 186, type: !7)
!570 = !DILocalVariable(name: "_result", scope: !282, file: !3, line: 187, type: !7)
!571 = !DILocalVariable(name: "a", scope: !282, file: !3, line: 187, type: !7)
!572 = !DILocalVariable(name: "_result", scope: !284, file: !3, line: 188, type: !7)
!573 = !DILocalVariable(name: "a", scope: !284, file: !3, line: 188, type: !7)
!574 = !DILocalVariable(name: "_result", scope: !286, file: !3, line: 189, type: !7)
!575 = !DILocalVariable(name: "a", scope: !286, file: !3, line: 189, type: !7)
!576 = !DILocalVariable(name: "_result", scope: !288, file: !3, line: 190, type: !7)
!577 = !DILocalVariable(name: "a", scope: !288, file: !3, line: 190, type: !7)
!578 = !DILocalVariable(name: "_result", scope: !290, file: !3, line: 191, type: !7)
!579 = !DILocalVariable(name: "a", scope: !290, file: !3, line: 191, type: !7)
!580 = !DILocalVariable(name: "_result", scope: !292, file: !3, line: 192, type: !7)
!581 = !DILocalVariable(name: "a", scope: !292, file: !3, line: 192, type: !7)
!582 = !DILocalVariable(name: "_result", scope: !294, file: !3, line: 193, type: !7)
!583 = !DILocalVariable(name: "a", scope: !294, file: !3, line: 193, type: !7)
!584 = !DILocalVariable(name: "_result", scope: !296, file: !3, line: 194, type: !7)
!585 = !DILocalVariable(name: "a", scope: !296, file: !3, line: 194, type: !7)
!586 = !DILocalVariable(name: "_result", scope: !298, file: !3, line: 195, type: !7)
!587 = !DILocalVariable(name: "a", scope: !298, file: !3, line: 195, type: !7)
!588 = !DILocalVariable(name: "_result", scope: !300, file: !3, line: 196, type: !7)
!589 = !DILocalVariable(name: "a", scope: !300, file: !3, line: 196, type: !7)
!590 = !DILocalVariable(name: "_result", scope: !302, file: !3, line: 197, type: !7)
!591 = !DILocalVariable(name: "a", scope: !302, file: !3, line: 197, type: !7)
!592 = !DILocation(line: 54, column: 0, scope: !16)
!593 = !DILocation(line: 55, column: 0, scope: !18)
!594 = !DILocation(line: 56, column: 0, scope: !20)
!595 = !DILocation(line: 57, column: 0, scope: !22)
!596 = !DILocation(line: 58, column: 0, scope: !24)
!597 = !DILocation(line: 59, column: 0, scope: !26)
!598 = !DILocation(line: 60, column: 0, scope: !28)
!599 = !DILocation(line: 61, column: 0, scope: !30)
!600 = !DILocation(line: 62, column: 0, scope: !32)
!601 = !DILocation(line: 63, column: 0, scope: !34)
!602 = !DILocation(line: 64, column: 0, scope: !36)
!603 = !DILocation(line: 65, column: 0, scope: !38)
!604 = !DILocation(line: 66, column: 0, scope: !40)
!605 = !DILocation(line: 67, column: 0, scope: !42)
!606 = !DILocation(line: 68, column: 0, scope: !44)
!607 = !DILocation(line: 69, column: 0, scope: !46)
!608 = !DILocation(line: 70, column: 0, scope: !48)
!609 = !DILocation(line: 71, column: 0, scope: !50)
!610 = !DILocation(line: 72, column: 0, scope: !52)
!611 = !DILocation(line: 73, column: 0, scope: !54)
!612 = !DILocation(line: 74, column: 0, scope: !56)
!613 = !DILocation(line: 75, column: 0, scope: !58)
!614 = !DILocation(line: 76, column: 0, scope: !60)
!615 = !DILocation(line: 77, column: 0, scope: !62)
!616 = !DILocation(line: 78, column: 0, scope: !64)
!617 = !DILocation(line: 79, column: 0, scope: !66)
!618 = !DILocation(line: 80, column: 0, scope: !68)
!619 = !DILocation(line: 81, column: 0, scope: !70)
!620 = !DILocation(line: 82, column: 0, scope: !72)
!621 = !DILocation(line: 83, column: 0, scope: !74)
!622 = !DILocation(line: 84, column: 0, scope: !76)
!623 = !DILocation(line: 85, column: 0, scope: !78)
!624 = !DILocation(line: 86, column: 0, scope: !80)
!625 = !DILocation(line: 87, column: 0, scope: !82)
!626 = !DILocation(line: 88, column: 0, scope: !84)
!627 = !DILocation(line: 89, column: 0, scope: !86)
!628 = !DILocation(line: 90, column: 0, scope: !88)
!629 = !DILocation(line: 91, column: 0, scope: !90)
!630 = !DILocation(line: 92, column: 0, scope: !92)
!631 = !DILocation(line: 93, column: 0, scope: !94)
!632 = !DILocation(line: 94, column: 0, scope: !96)
!633 = !DILocation(line: 95, column: 0, scope: !98)
!634 = !DILocation(line: 96, column: 0, scope: !100)
!635 = !DILocation(line: 97, column: 0, scope: !102)
!636 = !DILocation(line: 98, column: 0, scope: !104)
!637 = !DILocation(line: 99, column: 0, scope: !106)
!638 = !DILocation(line: 100, column: 0, scope: !108)
!639 = !DILocation(line: 101, column: 0, scope: !110)
!640 = !DILocation(line: 102, column: 0, scope: !112)
!641 = !DILocation(line: 103, column: 0, scope: !114)
!642 = !DILocation(line: 104, column: 0, scope: !116)
!643 = !DILocation(line: 105, column: 0, scope: !118)
!644 = !DILocation(line: 106, column: 0, scope: !120)
!645 = !DILocation(line: 107, column: 0, scope: !122)
!646 = !DILocation(line: 108, column: 0, scope: !124)
!647 = !DILocation(line: 109, column: 0, scope: !126)
!648 = !DILocation(line: 110, column: 0, scope: !128)
!649 = !DILocation(line: 111, column: 0, scope: !130)
!650 = !DILocation(line: 112, column: 0, scope: !132)
!651 = !DILocation(line: 113, column: 0, scope: !134)
!652 = !DILocation(line: 114, column: 0, scope: !136)
!653 = !DILocation(line: 115, column: 0, scope: !138)
!654 = !DILocation(line: 116, column: 0, scope: !140)
!655 = !DILocation(line: 117, column: 0, scope: !142)
!656 = !DILocation(line: 118, column: 0, scope: !144)
!657 = !DILocation(line: 119, column: 0, scope: !146)
!658 = !DILocation(line: 120, column: 0, scope: !148)
!659 = !DILocation(line: 121, column: 0, scope: !150)
!660 = !DILocation(line: 122, column: 0, scope: !152)
!661 = !DILocation(line: 123, column: 0, scope: !154)
!662 = !DILocation(line: 124, column: 0, scope: !156)
!663 = !DILocation(line: 125, column: 0, scope: !158)
!664 = !DILocation(line: 126, column: 0, scope: !160)
!665 = !DILocation(line: 127, column: 0, scope: !162)
!666 = !DILocation(line: 128, column: 0, scope: !164)
!667 = !DILocation(line: 129, column: 0, scope: !166)
!668 = !DILocation(line: 130, column: 0, scope: !168)
!669 = !DILocation(line: 131, column: 0, scope: !170)
!670 = !DILocation(line: 132, column: 0, scope: !172)
!671 = !DILocation(line: 133, column: 0, scope: !174)
!672 = !DILocation(line: 134, column: 0, scope: !176)
!673 = !DILocation(line: 135, column: 0, scope: !178)
!674 = !DILocation(line: 136, column: 0, scope: !180)
!675 = !DILocation(line: 137, column: 0, scope: !182)
!676 = !DILocation(line: 138, column: 0, scope: !184)
!677 = !DILocation(line: 139, column: 0, scope: !186)
!678 = !DILocation(line: 140, column: 0, scope: !188)
!679 = !DILocation(line: 141, column: 0, scope: !190)
!680 = !DILocation(line: 142, column: 0, scope: !192)
!681 = !DILocation(line: 143, column: 0, scope: !194)
!682 = !DILocation(line: 144, column: 0, scope: !196)
!683 = !DILocation(line: 145, column: 0, scope: !198)
!684 = !DILocation(line: 146, column: 0, scope: !200)
!685 = !DILocation(line: 147, column: 0, scope: !202)
!686 = !DILocation(line: 148, column: 0, scope: !204)
!687 = !DILocation(line: 149, column: 0, scope: !206)
!688 = !DILocation(line: 150, column: 0, scope: !208)
!689 = !DILocation(line: 151, column: 0, scope: !210)
!690 = !DILocation(line: 152, column: 0, scope: !212)
!691 = !DILocation(line: 153, column: 0, scope: !214)
!692 = !DILocation(line: 154, column: 0, scope: !216)
!693 = !DILocation(line: 155, column: 0, scope: !218)
!694 = !DILocation(line: 156, column: 0, scope: !220)
!695 = !DILocation(line: 157, column: 0, scope: !222)
!696 = !DILocation(line: 158, column: 0, scope: !224)
!697 = !DILocation(line: 159, column: 0, scope: !226)
!698 = !DILocation(line: 160, column: 0, scope: !228)
!699 = !DILocation(line: 161, column: 0, scope: !230)
!700 = !DILocation(line: 162, column: 0, scope: !232)
!701 = !DILocation(line: 163, column: 0, scope: !234)
!702 = !DILocation(line: 164, column: 0, scope: !236)
!703 = !DILocation(line: 165, column: 0, scope: !238)
!704 = !DILocation(line: 166, column: 0, scope: !240)
!705 = !DILocation(line: 167, column: 0, scope: !242)
!706 = !DILocation(line: 168, column: 0, scope: !244)
!707 = !DILocation(line: 169, column: 0, scope: !246)
!708 = !DILocation(line: 170, column: 0, scope: !248)
!709 = !DILocation(line: 171, column: 0, scope: !250)
!710 = !DILocation(line: 172, column: 0, scope: !252)
!711 = !DILocation(line: 173, column: 0, scope: !254)
!712 = !DILocation(line: 174, column: 0, scope: !256)
!713 = !DILocation(line: 175, column: 0, scope: !258)
!714 = !DILocation(line: 176, column: 0, scope: !260)
!715 = !DILocation(line: 177, column: 0, scope: !262)
!716 = !DILocation(line: 178, column: 0, scope: !264)
!717 = !DILocation(line: 179, column: 0, scope: !266)
!718 = !DILocation(line: 180, column: 0, scope: !268)
!719 = !DILocation(line: 181, column: 0, scope: !270)
!720 = !DILocation(line: 182, column: 0, scope: !272)
!721 = !DILocation(line: 183, column: 0, scope: !274)
!722 = !DILocation(line: 184, column: 0, scope: !276)
!723 = !DILocation(line: 185, column: 0, scope: !278)
!724 = !DILocation(line: 186, column: 0, scope: !280)
!725 = !DILocation(line: 187, column: 0, scope: !282)
!726 = !DILocation(line: 188, column: 0, scope: !284)
!727 = !DILocation(line: 189, column: 0, scope: !286)
!728 = !DILocation(line: 190, column: 0, scope: !288)
!729 = !DILocation(line: 191, column: 0, scope: !290)
!730 = !DILocation(line: 192, column: 0, scope: !292)
!731 = !DILocation(line: 193, column: 0, scope: !294)
!732 = !DILocation(line: 194, column: 0, scope: !296)
!733 = !DILocation(line: 195, column: 0, scope: !298)
!734 = !DILocation(line: 196, column: 0, scope: !300)
!735 = !DILocation(line: 197, column: 0, scope: !302)
!3 = !DIFile(filename: "return_parameter_convert.m3", directory: "../ARM64_DARWIN")
!4 = !DINamespace(name: "return_parameter_convert", scope: !2)
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
