; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @Fmt__LongReal(double, i8, i64, i1)
declare void @Wr__PutText(ptr, ptr)
declare ptr @FileWr__Open(ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__DumpMatching__DumpOne(ptr %__cap_0, { double, { double, i64 } } %a.trade) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %trade.slot = alloca { double, { double, i64 } }
  call void @llvm.dbg.declare(metadata ptr %trade.slot, metadata !28, metadata !DIExpression()), !dbg !30
  store { double, { double, i64 } } %a.trade, ptr %trade.slot
  %t1 = load ptr, ptr %__cap_0
  %t2 = getelementptr i8, ptr %trade.slot, i64 0
  %t3 = load double, ptr %t2
  %t4 = call ptr @Fmt__LongReal(double %t3, i8 2, i64 16, i1 0), !dbg !31
  call void @Wr__PutText(ptr %t1, ptr %t4), !dbg !31
  ret void
}

define void @Main__DumpMatching() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %wr.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %wr.slot, metadata !29, metadata !DIExpression()), !dbg !32
  store ptr null, ptr %wr.slot
  %t1 = call ptr @FileWr__Open(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !33
  store ptr %t1, ptr %wr.slot
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
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
@textlit_0 = internal constant { i64, ptr, i64, [4 x i8] } { i64 2, ptr @textlit_methods, i64 3, [4 x i8] c"out\00" }

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Fmt_I3(i64)
declare ptr @Wr_I3(i64)
declare ptr @FileWr_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Fmt_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Wr_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @FileWr_I3, ptr null }

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
!16 = distinct !DISubprogram(name: "Main__DumpMatching__DumpOne", linkageName: "Main__DumpMatching__DumpOne", scope: !4, file: !3, line: 16, type: !6, scopeLine: 16, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__DumpMatching", linkageName: "Main__DumpMatching", scope: !4, file: !3, line: 15, type: !6, scopeLine: 15, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!28 = !DILocalVariable(name: "trade", scope: !16, file: !3, line: 16, type: !20)
!29 = !DILocalVariable(name: "wr", scope: !18, file: !3, line: 15, type: !15)
!20 = !DICompositeType(tag: DW_TAG_structure_type, name: "HFData_S", size: 192, elements: !21)
!21 = !{!26, !27}
!26 = !DIDerivedType(tag: DW_TAG_member, name: "time", baseType: !13, size: 64, offset: 0)
!27 = !DIDerivedType(tag: DW_TAG_member, name: "pv", baseType: !22, size: 128, offset: 64)
!22 = !DICompositeType(tag: DW_TAG_structure_type, name: "", size: 128, elements: !23)
!23 = !{!24, !25}
!24 = !DIDerivedType(tag: DW_TAG_member, name: "price", baseType: !13, size: 64, offset: 0)
!25 = !DIDerivedType(tag: DW_TAG_member, name: "volume", baseType: !8, size: 64, offset: 64)
!30 = !DILocation(line: 16, column: 0, scope: !16)
!31 = !DILocation(line: 18, column: 0, scope: !16)
!32 = !DILocation(line: 15, column: 0, scope: !18)
!33 = !DILocation(line: 18, column: 0, scope: !18)
!34 = !DILocation(line: 16, column: 0, scope: !18)
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
