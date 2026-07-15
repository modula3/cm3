; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__F1(i64 %a.x) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %x.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %x.slot, metadata !22, metadata !DIExpression()), !dbg !25
  store i64 %a.x, ptr %x.slot
  ret void
}

define void @Main__F2() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %x.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %x.slot, metadata !23, metadata !DIExpression()), !dbg !26
  store i64 0, ptr %x.slot
  ret void
}

define void @Main__F3() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %x.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %x.slot, metadata !24, metadata !DIExpression()), !dbg !28
  store i64 0, ptr %x.slot
  %t1 = load i64, ptr %x.slot
  call void @Main__F1(i64 %t1), !dbg !29
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__F1(i64 0)
  call void @Main__F2()
  call void @Main__F3()
  ret void
}

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
!16 = distinct !DISubprogram(name: "Main__F1", linkageName: "Main__F1", scope: !4, file: !3, line: 3, type: !6, scopeLine: 3, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__F2", linkageName: "Main__F2", scope: !4, file: !3, line: 5, type: !6, scopeLine: 5, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__F3", linkageName: "Main__F3", scope: !4, file: !3, line: 10, type: !6, scopeLine: 10, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!22 = !DILocalVariable(name: "x", scope: !16, file: !3, line: 3, type: !7)
!23 = !DILocalVariable(name: "x", scope: !18, file: !3, line: 5, type: !7)
!24 = !DILocalVariable(name: "x", scope: !20, file: !3, line: 10, type: !7)
!25 = !DILocation(line: 3, column: 0, scope: !16)
!26 = !DILocation(line: 5, column: 0, scope: !18)
!27 = !DILocation(line: 6, column: 0, scope: !18)
!28 = !DILocation(line: 10, column: 0, scope: !20)
!29 = !DILocation(line: 13, column: 0, scope: !20)
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
