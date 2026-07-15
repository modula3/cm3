; ModuleID = 'F4095'
source_filename = "F4095"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define i8 @F4095__F1(ptr %a.t) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !18, metadata !DIExpression()), !dbg !20
  %t.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %t.slot, metadata !19, metadata !DIExpression()), !dbg !20
  store ptr %a.t, ptr %t.slot
  %t1 = load ptr, ptr %t.slot
  %t2 = getelementptr i8, ptr %t1, i64 0
  %t3 = getelementptr inbounds [4096 x i8], ptr %t2, i64 0, i64 0
  %t4 = load i8, ptr %t3
  ret i8 %t4
}

define void @F4095__F4095_M3() personality ptr @__gxx_personality_v0 {
entry:
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_1139613435 = internal global %TC_t {
  i64 0,
  i64 1139613435,
  i64 u0x0f65e5244c88fbdf,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 4096,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null
}

define weak ptr @F4095_I3(i64 %mode) {
entry:
  ret ptr @F4095_M3_info
}

; RT0.ImportInfo chain for F4095
declare ptr @F0_I3(i64)
@F4095_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4095_I3, ptr @F4095_M3_imp.1 }
@F4095_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @F0_I3, ptr null }

; RT0.ModuleInfo for F4095 (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@F4095_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_1139613435,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @F4095_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @F4095_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @F4095_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @F4095__F4095_M3()
  br label %done
done:
  ret ptr @F4095_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "F4095__F1", linkageName: "F4095__F1", scope: !4, file: !3, line: 6, type: !6, scopeLine: 6, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocalVariable(name: "_result", scope: !16, file: !3, line: 6, type: !7)
!19 = !DILocalVariable(name: "t", scope: !16, file: !3, line: 6, type: !15)
!20 = !DILocation(line: 6, column: 0, scope: !16)
!21 = !DILocation(line: 8, column: 0, scope: !16)
!3 = !DIFile(filename: "F4095.m3", directory: "..")
!4 = !DINamespace(name: "F4095", scope: !2)
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
