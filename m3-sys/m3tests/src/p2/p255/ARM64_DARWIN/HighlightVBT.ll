; ModuleID = 'HighlightVBT'
source_filename = "HighlightVBT"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @HighlightVBT__Be(ptr %a.v, { i64 } %a.paintOpValue, ptr %a.paintOpReadOnly, { i64 } %a.pixmapValue, ptr %a.pixmapReadOnly, { i64 } %a.recordValue, ptr %a.recordReadOnly, [1 x i64] %a.fixedArrayValue, ptr %a.fixedArrayReadOnly, ptr %a.openArrayValue, ptr %a.openArrayReadOnly, i1088 %a.bigSetValue, ptr %a.bigSetReadOnly, i16 %a.smallSetValue, ptr %a.smallSetReadOnly) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  call void @llvm.dbg.declare(metadata ptr %a.openArrayReadOnly, metadata !45, metadata !DIExpression()), !dbg !17
  %smallSetValue.slot = alloca i16
  call void @llvm.dbg.declare(metadata ptr %smallSetValue.slot, metadata !18, metadata !DIExpression()), !dbg !46
  %bigSetValue.slot = alloca i1088
  call void @llvm.dbg.declare(metadata ptr %bigSetValue.slot, metadata !19, metadata !DIExpression()), !dbg !46
  %fixedArrayValue.slot = alloca [1 x i64]
  call void @llvm.dbg.declare(metadata ptr %fixedArrayValue.slot, metadata !23, metadata !DIExpression()), !dbg !46
  %recordValue.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %recordValue.slot, metadata !27, metadata !DIExpression()), !dbg !46
  %pixmapValue.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %pixmapValue.slot, metadata !31, metadata !DIExpression()), !dbg !46
  %paintOpValue.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %paintOpValue.slot, metadata !35, metadata !DIExpression()), !dbg !46
  %v.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %v.slot, metadata !40, metadata !DIExpression()), !dbg !46
  store ptr %a.v, ptr %v.slot
  store { i64 } %a.paintOpValue, ptr %paintOpValue.slot
  store { i64 } %a.pixmapValue, ptr %pixmapValue.slot
  store { i64 } %a.recordValue, ptr %recordValue.slot
  store [1 x i64] %a.fixedArrayValue, ptr %fixedArrayValue.slot
  store i1088 %a.bigSetValue, ptr %bigSetValue.slot
  store i16 %a.smallSetValue, ptr %smallSetValue.slot
  ret void
}

define void @HighlightVBT__HighlightVBT_M3() personality ptr @__gxx_personality_v0 {
entry:
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_712975378.tc_name = private unnamed_addr constant [28 x i8] c"HighlightVBT.HighlightVBT_T\00"
@tc_obj_712975378.methods = internal constant [1 x ptr] [ptr @HighlightVBT__Be]
@tc_obj_712975378 = internal global %OTC_t {
  i64 0,
  i64 712975378,
  i64 u0x153061ac3f4f45be,
  i8 1,
  i8 2,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 0,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_obj_712975378.tc_name,
  ptr null,
  i64 874473023,
  ptr null,
  i64 0,
  i64 0,
  i64 0,
  ptr @tc_obj_712975378.methods,
  ptr null
}

define weak ptr @HighlightVBT_I3(i64 %mode) {
entry:
  ret ptr @HighlightVBT_M3_info
}

; RT0.ImportInfo chain for HighlightVBT
declare ptr @VBT_I3(i64)
@HighlightVBT_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @HighlightVBT_I3, ptr @HighlightVBT_M3_imp.1 }
@HighlightVBT_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @VBT_I3, ptr null }

; full_rev — REVEAL records for RTLinker.NoteFullRevelation
%RT0_RV_t = type { i64, i64 }
@HighlightVBT_M3_full_rev = internal global [2 x %RT0_RV_t] [
  %RT0_RV_t { i64 -132573217, i64 712975378 },
  %RT0_RV_t { i64 0, i64 0 }
]

; RT0.ModuleInfo for HighlightVBT (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [16 x i8] }
@HighlightVBT_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_712975378,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr @HighlightVBT_M3_full_rev,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @HighlightVBT_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @HighlightVBT_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [16 x i8] zeroinitializer  ; user globals (16 bytes)
}
@HighlightVBT__HighlightVBT_var_a = alias i64, ptr getelementptr inbounds (i8, ptr @HighlightVBT_M3_info, i64 104)
@HighlightVBT__HighlightVBT_var_b = alias i64, ptr getelementptr inbounds (i8, ptr @HighlightVBT_M3_info, i64 112)

define ptr @HighlightVBT_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @HighlightVBT__HighlightVBT_M3()
  br label %done
done:
  ret ptr @HighlightVBT_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "HighlightVBT__Be", linkageName: "HighlightVBT__Be", scope: !4, file: !3, line: 13, type: !6, scopeLine: 13, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocalVariable(name: "smallSetValue", scope: !16, file: !3, line: 13, type: !15)
!19 = !DILocalVariable(name: "bigSetValue", scope: !16, file: !3, line: 13, type: !15)
!23 = !DILocalVariable(name: "fixedArrayValue", scope: !16, file: !3, line: 13, type: !20)
!27 = !DILocalVariable(name: "recordValue", scope: !16, file: !3, line: 13, type: !24)
!31 = !DILocalVariable(name: "pixmapValue", scope: !16, file: !3, line: 13, type: !28)
!35 = !DILocalVariable(name: "paintOpValue", scope: !16, file: !3, line: 13, type: !32)
!40 = !DILocalVariable(name: "v", scope: !16, file: !3, line: 13, type: !39)
!45 = !DILocalVariable(name: "a.openArrayReadOnly", scope: !16, file: !3, line: 13, type: !41)
!20 = !DICompositeType(tag: DW_TAG_array_type, baseType: !7, size: 64, elements: !21)
!21 = !{!22}
!22 = !DISubrange(count: 1)
!24 = !DICompositeType(tag: DW_TAG_structure_type, name: "Point_T", size: 64, elements: !25)
!25 = !{!26}
!26 = !DIDerivedType(tag: DW_TAG_member, name: "Point_T_field", baseType: !7, size: 64, offset: 0)
!28 = !DICompositeType(tag: DW_TAG_structure_type, name: "Pixmap_T", size: 64, elements: !29)
!29 = !{!30}
!30 = !DIDerivedType(tag: DW_TAG_member, name: "Pixmap_T_field", baseType: !7, size: 64, offset: 0)
!32 = !DICompositeType(tag: DW_TAG_structure_type, name: "PaintOp_T", size: 64, elements: !33)
!33 = !{!34}
!34 = !DIDerivedType(tag: DW_TAG_member, name: "PaintOp_T_field", baseType: !7, size: 64, offset: 0)
!36 = !DICompositeType(tag: DW_TAG_structure_type, name: "HighlightVBT_T", size: 64, elements: !37)
!37 = !{!38}
!38 = !DIDerivedType(tag: DW_TAG_member, name: "__vtable", baseType: !15, size: 64, offset: 0)
!39 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !36, size: 64)
!41 = !DICompositeType(tag: DW_TAG_structure_type, name: "__dope_1", size: 128, elements: !42)
!42 = !{!43, !44}
!43 = !DIDerivedType(tag: DW_TAG_member, name: "data", baseType: !15, size: 64, offset: 0)
!44 = !DIDerivedType(tag: DW_TAG_member, name: "count", baseType: !7, size: 64, offset: 64)
!46 = !DILocation(line: 13, column: 0, scope: !16)
!47 = !DILocation(line: 30, column: 0, scope: !16)
!3 = !DIFile(filename: "HighlightVBT.m3", directory: "..")
!4 = !DINamespace(name: "HighlightVBT", scope: !2)
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
