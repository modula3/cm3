; ModuleID = 'return_variable'
source_filename = "return_variable"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define i8 @return_variable__ret_vi8() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !44, metadata !DIExpression()), !dbg !58
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 104)
  ret i8 %t1
}

define i64 @return_variable__ret_vu64() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !45, metadata !DIExpression()), !dbg !59
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 112)
  ret i64 %t1
}

define double @return_variable__ret_vf64() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca double
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !46, metadata !DIExpression()), !dbg !60
  %t1 = load double, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 120)
  ret double %t1
}

define i32 @return_variable__ret_vi32() personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !47, metadata !DIExpression()), !dbg !61
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 128)
  ret i32 %t1
}

define i64 @return_variable__ret_vLC() personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !48, metadata !DIExpression()), !dbg !62
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 136)
  ret i64 %t1
}

define i16 @return_variable__ret_vu16() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !49, metadata !DIExpression()), !dbg !63
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 144)
  ret i16 %t1
}

define i64 @return_variable__ret_vI() personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !50, metadata !DIExpression()), !dbg !64
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 152)
  ret i64 %t1
}

define i64 @return_variable__ret_vi64() personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !51, metadata !DIExpression()), !dbg !65
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 160)
  ret i64 %t1
}

define float @return_variable__ret_vf32() personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %_result.slot = alloca float
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !52, metadata !DIExpression()), !dbg !66
  %t1 = load float, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 168)
  ret float %t1
}

define i16 @return_variable__ret_vi16() personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !53, metadata !DIExpression()), !dbg !67
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 172)
  ret i16 %t1
}

define i64 @return_variable__ret_vC() personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !54, metadata !DIExpression()), !dbg !68
  store i64 0, ptr %_result.slot
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 176)
  ret i64 %t1
}

define i32 @return_variable__ret_vu32() personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !55, metadata !DIExpression()), !dbg !69
  %t1 = load i32, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 184)
  ret i32 %t1
}

define i8 @return_variable__ret_vu8() personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !56, metadata !DIExpression()), !dbg !70
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 188)
  ret i8 %t1
}

define i64 @return_variable__ret_vL() personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !57, metadata !DIExpression()), !dbg !71
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 192)
  ret i64 %t1
}

define void @return_variable__return_variable_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = trunc i64 49 to i8
  store i8 %t1, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 104)
  store i64 50, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 112)
  store double 0x4049c28f5c28f5c3, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 120)
  %t2 = trunc i64 53 to i32
  store i32 %t2, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 128)
  store i64 54, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 136)
  %t3 = trunc i64 55 to i16
  store i16 %t3, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 144)
  store i64 56, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 152)
  store i64 57, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 160)
  store float 0x404d4b8520000000, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 168)
  %t4 = trunc i64 60 to i16
  store i16 %t4, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 172)
  store i64 61, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 176)
  %t5 = trunc i64 62 to i32
  store i32 %t5, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 184)
  %t6 = trunc i64 63 to i8
  store i8 %t6, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 188)
  store i64 64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 192)
  ret void
}

define weak ptr @return_variable_I3(i64 %mode) {
entry:
  ret ptr @return_variable_M3_info
}

; RT0.ImportInfo chain for return_variable
declare ptr @Long_I3(i64)
declare ptr @Word_I3(i64)
declare ptr @Cstdint_I3(i64)
@return_variable_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @return_variable_I3, ptr @return_variable_M3_imp.1 }
@return_variable_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Long_I3, ptr @return_variable_M3_imp.2 }
@return_variable_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Word_I3, ptr @return_variable_M3_imp.3 }
@return_variable_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Cstdint_I3, ptr null }

; RT0.ModuleInfo for return_variable (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [112 x i8] }
@return_variable_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @return_variable_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @return_variable_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [112 x i8] zeroinitializer  ; user globals (112 bytes)
}
@return_variable__vi8 = alias i8, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 104)
@return_variable__vu64 = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 112)
@return_variable__vf64 = alias double, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 120)
@return_variable__vi32 = alias i32, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 128)
@return_variable__vLC = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 136)
@return_variable__vu16 = alias i16, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 144)
@return_variable__vI = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 152)
@return_variable__vi64 = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 160)
@return_variable__vf32 = alias float, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 168)
@return_variable__vi16 = alias i16, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 172)
@return_variable__vC = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 176)
@return_variable__vu32 = alias i32, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 184)
@return_variable__vu8 = alias i8, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 188)
@return_variable__vL = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 192)
@return_variable__offset = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 200)
@return_variable__count = alias i64, ptr getelementptr inbounds (i8, ptr @return_variable_M3_info, i64 208)

define ptr @return_variable_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @return_variable__return_variable_M3()
  br label %done
done:
  ret ptr @return_variable_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "return_variable__ret_vi8", linkageName: "return_variable__ret_vi8", scope: !4, file: !3, line: 54, type: !6, scopeLine: 54, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "return_variable__ret_vu64", linkageName: "return_variable__ret_vu64", scope: !4, file: !3, line: 55, type: !6, scopeLine: 55, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "return_variable__ret_vf64", linkageName: "return_variable__ret_vf64", scope: !4, file: !3, line: 56, type: !6, scopeLine: 56, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "return_variable__ret_vi32", linkageName: "return_variable__ret_vi32", scope: !4, file: !3, line: 57, type: !6, scopeLine: 57, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "return_variable__ret_vLC", linkageName: "return_variable__ret_vLC", scope: !4, file: !3, line: 58, type: !6, scopeLine: 58, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "return_variable__ret_vu16", linkageName: "return_variable__ret_vu16", scope: !4, file: !3, line: 59, type: !6, scopeLine: 59, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "return_variable__ret_vI", linkageName: "return_variable__ret_vI", scope: !4, file: !3, line: 60, type: !6, scopeLine: 60, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "return_variable__ret_vi64", linkageName: "return_variable__ret_vi64", scope: !4, file: !3, line: 61, type: !6, scopeLine: 61, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "return_variable__ret_vf32", linkageName: "return_variable__ret_vf32", scope: !4, file: !3, line: 62, type: !6, scopeLine: 62, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "return_variable__ret_vi16", linkageName: "return_variable__ret_vi16", scope: !4, file: !3, line: 63, type: !6, scopeLine: 63, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "return_variable__ret_vC", linkageName: "return_variable__ret_vC", scope: !4, file: !3, line: 64, type: !6, scopeLine: 64, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "return_variable__ret_vu32", linkageName: "return_variable__ret_vu32", scope: !4, file: !3, line: 65, type: !6, scopeLine: 65, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "return_variable__ret_vu8", linkageName: "return_variable__ret_vu8", scope: !4, file: !3, line: 66, type: !6, scopeLine: 66, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "return_variable__ret_vL", linkageName: "return_variable__ret_vL", scope: !4, file: !3, line: 67, type: !6, scopeLine: 67, unit: !2, spFlags: DISPFlagDefinition)
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
!44 = !DILocalVariable(name: "_result", scope: !16, file: !3, line: 54, type: !7)
!45 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 55, type: !7)
!46 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 56, type: !13)
!47 = !DILocalVariable(name: "_result", scope: !22, file: !3, line: 57, type: !7)
!48 = !DILocalVariable(name: "_result", scope: !24, file: !3, line: 58, type: !7)
!49 = !DILocalVariable(name: "_result", scope: !26, file: !3, line: 59, type: !7)
!50 = !DILocalVariable(name: "_result", scope: !28, file: !3, line: 60, type: !7)
!51 = !DILocalVariable(name: "_result", scope: !30, file: !3, line: 61, type: !7)
!52 = !DILocalVariable(name: "_result", scope: !32, file: !3, line: 62, type: !12)
!53 = !DILocalVariable(name: "_result", scope: !34, file: !3, line: 63, type: !7)
!54 = !DILocalVariable(name: "_result", scope: !36, file: !3, line: 64, type: !7)
!55 = !DILocalVariable(name: "_result", scope: !38, file: !3, line: 65, type: !7)
!56 = !DILocalVariable(name: "_result", scope: !40, file: !3, line: 66, type: !7)
!57 = !DILocalVariable(name: "_result", scope: !42, file: !3, line: 67, type: !7)
!58 = !DILocation(line: 54, column: 0, scope: !16)
!59 = !DILocation(line: 55, column: 0, scope: !18)
!60 = !DILocation(line: 56, column: 0, scope: !20)
!61 = !DILocation(line: 57, column: 0, scope: !22)
!62 = !DILocation(line: 58, column: 0, scope: !24)
!63 = !DILocation(line: 59, column: 0, scope: !26)
!64 = !DILocation(line: 60, column: 0, scope: !28)
!65 = !DILocation(line: 61, column: 0, scope: !30)
!66 = !DILocation(line: 62, column: 0, scope: !32)
!67 = !DILocation(line: 63, column: 0, scope: !34)
!68 = !DILocation(line: 64, column: 0, scope: !36)
!69 = !DILocation(line: 65, column: 0, scope: !38)
!70 = !DILocation(line: 66, column: 0, scope: !40)
!71 = !DILocation(line: 67, column: 0, scope: !42)
!3 = !DIFile(filename: "return_variable.m3", directory: "../ARM64_DARWIN")
!4 = !DINamespace(name: "return_variable", scope: !2)
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
