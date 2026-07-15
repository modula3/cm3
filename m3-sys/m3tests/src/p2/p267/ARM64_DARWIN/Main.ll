; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @RTIO__PutText(ptr)
declare void @RTIO__PutAddr(ptr, i64)
declare void @RTIO__Flush()
declare ptr @RTHooks__AllocateTracedObj(ptr)
declare void @RTHeapRep__RegisterFinalCleanup(ptr, ptr)
declare void @RTCollector__Collect()
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__Clean1(ptr %a.r) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %r.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %r.slot, metadata !22, metadata !DIExpression()), !dbg !29
  store ptr %a.r, ptr %r.slot
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !30
  %t1 = load ptr, ptr %r.slot
  call void @RTIO__PutAddr(ptr %t1, i64 0), !dbg !31
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8)), !dbg !32
  call void @RTIO__Flush(), !dbg !33
  ret void
}

define void @Main__Clean2(ptr %a.r) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %r.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %r.slot, metadata !23, metadata !DIExpression()), !dbg !34
  store ptr %a.r, ptr %r.slot
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8)), !dbg !35
  %t1 = load ptr, ptr %r.slot
  call void @RTIO__PutAddr(ptr %t1, i64 0), !dbg !36
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8)), !dbg !37
  call void @RTIO__Flush(), !dbg !38
  ret void
}

define void @Main__Test() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !28, metadata !DIExpression()), !dbg !39
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_obj_-1520182960
  %t2 = call ptr @RTHooks__AllocateTracedObj(ptr %t1), !dbg !39
  %t3 = bitcast ptr %t2 to ptr
  store ptr %t3, ptr %a.slot
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8)), !dbg !40
  %t4 = load ptr, ptr %a.slot
  call void @RTIO__PutAddr(ptr %t4, i64 0), !dbg !41
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8)), !dbg !42
  call void @RTIO__Flush(), !dbg !43
  %t5 = load ptr, ptr %a.slot
  call void @RTHeapRep__RegisterFinalCleanup(ptr %t5, ptr @Main__Clean1), !dbg !44
  %t6 = load ptr, ptr %a.slot
  call void @RTHeapRep__RegisterFinalCleanup(ptr %t6, ptr @Main__Clean2), !dbg !45
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  br label %while.header.1
while.header.1:
  br i1 1, label %while.body.2, label %while.exit.3
while.body.2:
  call void @Main__Test()
  call void @RTCollector__Collect()
  br label %while.header.1
while.exit.3:
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
@textlit_0 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"Clean1:\00" }
@textlit_1 = internal constant { i64, ptr, i64, [2 x i8] } { i64 2, ptr @textlit_methods, i64 1, [2 x i8] c"\0a\00" }
@textlit_2 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"Clean2:\00" }
@textlit_3 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"Test:\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_-1520182960.tc_name = private unnamed_addr constant [7 x i8] c"Main.A\00"
@tc_obj_-1520182960 = internal global %OTC_t {
  i64 0,
  i64 -1520182960,
  i64 u0x1464bbbfb10762ef,
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
  ptr @tc_obj_-1520182960.tc_name,
  ptr null,
  i64 -1651526519,
  ptr null,
  i64 0,
  i64 0,
  i64 0,
  ptr null,
  ptr null
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_obj_-1520182960 = internal global %TypeLink_t {
  ptr null,
  i64 -1520182960
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @RTCollector_I3(i64)
declare ptr @RTIO_I3(i64)
declare ptr @RTHeapRep_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTCollector_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTIO_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTHeapRep_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_-1520182960,  ; type_cells (+8)
  ptr @tl_obj_-1520182960,  ; type_cell_ptrs (+16)
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
!16 = distinct !DISubprogram(name: "Main__Clean1", linkageName: "Main__Clean1", scope: !4, file: !3, line: 7, type: !6, scopeLine: 7, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__Clean2", linkageName: "Main__Clean2", scope: !4, file: !3, line: 15, type: !6, scopeLine: 15, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__Test", linkageName: "Main__Test", scope: !4, file: !3, line: 25, type: !6, scopeLine: 25, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!22 = !DILocalVariable(name: "r", scope: !16, file: !3, line: 7, type: !15)
!23 = !DILocalVariable(name: "r", scope: !18, file: !3, line: 15, type: !15)
!28 = !DILocalVariable(name: "a", scope: !20, file: !3, line: 25, type: !27)
!24 = !DICompositeType(tag: DW_TAG_structure_type, name: "A", size: 64, elements: !25)
!25 = !{!26}
!26 = !DIDerivedType(tag: DW_TAG_member, name: "__vtable", baseType: !15, size: 64, offset: 0)
!27 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !24, size: 64)
!29 = !DILocation(line: 7, column: 0, scope: !16)
!30 = !DILocation(line: 9, column: 0, scope: !16)
!31 = !DILocation(line: 10, column: 0, scope: !16)
!32 = !DILocation(line: 11, column: 0, scope: !16)
!33 = !DILocation(line: 12, column: 0, scope: !16)
!34 = !DILocation(line: 15, column: 0, scope: !18)
!35 = !DILocation(line: 17, column: 0, scope: !18)
!36 = !DILocation(line: 18, column: 0, scope: !18)
!37 = !DILocation(line: 19, column: 0, scope: !18)
!38 = !DILocation(line: 20, column: 0, scope: !18)
!39 = !DILocation(line: 25, column: 0, scope: !20)
!40 = !DILocation(line: 28, column: 0, scope: !20)
!41 = !DILocation(line: 29, column: 0, scope: !20)
!42 = !DILocation(line: 30, column: 0, scope: !20)
!43 = !DILocation(line: 31, column: 0, scope: !20)
!44 = !DILocation(line: 32, column: 0, scope: !20)
!45 = !DILocation(line: 33, column: 0, scope: !20)
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
