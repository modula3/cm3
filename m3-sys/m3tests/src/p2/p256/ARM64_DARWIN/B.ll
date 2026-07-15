; ModuleID = 'B'
source_filename = "B"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @A__Exported2()
declare void @A__Exported1()

define void @B__Unused() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  call void @A__Exported2(), !dbg !22
  ret void
}

define void @B__Used() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  ret void
}

define void @B__Exported() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  call void @B__Used(), !dbg !24
  call void @A__Exported1(), !dbg !24
  ret void
}

define void @B__B_M3() personality ptr @__gxx_personality_v0 {
entry:
  ret void
}

define weak ptr @B_I3(i64 %mode) {
entry:
  ret ptr @B_M3_info
}

; RT0.ImportInfo chain for B
declare ptr @A_I3(i64)
@B_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @B_I3, ptr @B_M3_imp.1 }
@B_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @A_I3, ptr null }

; RT0.ModuleInfo for B (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@B_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @B_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @B_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @B_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @B__B_M3()
  br label %done
done:
  ret ptr @B_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "B__Unused", linkageName: "B__Unused", scope: !4, file: !3, line: 4, type: !6, scopeLine: 4, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "B__Used", linkageName: "B__Used", scope: !4, file: !3, line: 5, type: !6, scopeLine: 5, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "B__Exported", linkageName: "B__Exported", scope: !4, file: !3, line: 6, type: !6, scopeLine: 6, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!22 = !DILocation(line: 4, column: 0, scope: !16)
!23 = !DILocation(line: 5, column: 0, scope: !18)
!24 = !DILocation(line: 6, column: 0, scope: !20)
!3 = !DIFile(filename: "B.m3", directory: "..")
!4 = !DINamespace(name: "B", scope: !2)
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
