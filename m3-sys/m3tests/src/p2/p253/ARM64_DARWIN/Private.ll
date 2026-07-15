; ModuleID = 'Private'
source_filename = "Private"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @M3toC__FlatTtoS(ptr)
declare i64 @RTHooks__CheckIsType(ptr, ptr)
declare void @put_adr(ptr, ptr)
declare void @RTHooks__ReportFault(ptr, i64)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define internal void @Private__Private_M3_te6e69551_INIT(ptr %obj) personality ptr @__gxx_personality_v0 {
entry:
  %t1 = getelementptr i8, ptr %obj, i64 8
  store i64 2, ptr %t1
  ret void
}

define void @Private__F2(ptr %a.a) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !23, metadata !DIExpression()), !dbg !24
  store ptr %a.a, ptr %a.slot
  %t1 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !25
  %t2 = load ptr, ptr %a.slot
  %t3 = load ptr, ptr @tl_obj_-1278203716
  %narrow.chk.0 = call i64 @RTHooks__CheckIsType(ptr %t2, ptr %t3), !dbg !25
  %narrow.cond.0 = icmp ne i64 %narrow.chk.0, 0
  br i1 %narrow.cond.0, label %narrow.ok.0.1, label %narrow.fail.0.2
narrow.ok.0.1:
  call void @put_adr(ptr %t1, ptr %t2), !dbg !25
  %t4 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8)), !dbg !26
  %t5 = load ptr, ptr %a.slot
  %t6 = load ptr, ptr @tl_obj_-1278203716
  %narrow.chk.1 = call i64 @RTHooks__CheckIsType(ptr %t5, ptr %t6), !dbg !26
  %narrow.cond.1 = icmp ne i64 %narrow.chk.1, 0
  br i1 %narrow.cond.1, label %narrow.ok.1.3, label %narrow.fail.1.4
narrow.fail.0.2:
  call void @RTHooks__ReportFault(ptr null, i64 5), !dbg !25
  unreachable
narrow.ok.1.3:
  %t7 = load ptr, ptr @tl_obj_-1278203716
  %t8 = getelementptr i8, ptr %t7, i64 112
  %t9 = load i64, ptr %t8
  %t10 = getelementptr inbounds i8, ptr %t5, i64 %t9
  %t11 = getelementptr i8, ptr %t10, i64 0
  call void @put_adr(ptr %t4, ptr %t11), !dbg !26
  %t12 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8)), !dbg !27
  %t13 = load ptr, ptr %a.slot
  %t14 = getelementptr i8, ptr %t13, i64 8
  call void @put_adr(ptr %t12, ptr %t14), !dbg !27
  ret void
narrow.fail.1.4:
  call void @RTHooks__ReportFault(ptr null, i64 5), !dbg !26
  unreachable
}

define void @Private__Private_M3() personality ptr @__gxx_personality_v0 {
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
@textlit_0 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"Private.F2:a\00" }
@textlit_1 = internal constant { i64, ptr, i64, [15 x i8] } { i64 2, ptr @textlit_methods, i64 14, [15 x i8] c"Private.F2:a.a\00" }
@textlit_2 = internal constant { i64, ptr, i64, [15 x i8] } { i64 2, ptr @textlit_methods, i64 14, [15 x i8] c"Private.F2:a.b\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_-421096111.tc_name = private unnamed_addr constant [15 x i8] c"Public.Private\00"
@tc_obj_-421096111.methods = internal constant [1 x ptr] [ptr @Private__F2]
@tc_obj_-421096111 = internal global %OTC_t {
  i64 0,
  i64 -421096111,
  i64 u0x1f1ebccef9f8299f,
  i8 1,
  i8 2,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 8,
  ptr null,
  ptr null,
  ptr null,
  ptr @Private__Private_M3_te6e69551_INIT,
  ptr null,
  ptr @tc_obj_-421096111.tc_name,
  ptr null,
  i64 -1651526519,
  ptr null,
  i64 0,
  i64 0,
  i64 8,
  ptr @tc_obj_-421096111.methods,
  ptr null
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_obj_-1278203716 = internal global %TypeLink_t {
  ptr null,
  i64 -1278203716
}

define weak ptr @Private_I3(i64 %mode) {
entry:
  ret ptr @Private_M3_info
}

; RT0.ImportInfo chain for Private
declare ptr @M3toC_I3(i64)
declare ptr @Public_I3(i64)
@Private_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Private_I3, ptr @Private_M3_imp.1 }
@Private_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @M3toC_I3, ptr @Private_M3_imp.2 }
@Private_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Public_I3, ptr null }

; full_rev — REVEAL records for RTLinker.NoteFullRevelation
%RT0_RV_t = type { i64, i64 }
@Private_M3_full_rev = internal global [2 x %RT0_RV_t] [
  %RT0_RV_t { i64 -215227970, i64 -421096111 },
  %RT0_RV_t { i64 0, i64 0 }
]

; RT0.ModuleInfo for Private (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Private_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_-421096111,  ; type_cells (+8)
  ptr @tl_obj_-1278203716,  ; type_cell_ptrs (+16)
  ptr @Private_M3_full_rev,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Private_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Private_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @Private_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @Private__Private_M3()
  br label %done
done:
  ret ptr @Private_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "Private__F2", linkageName: "Private__F2", scope: !4, file: !3, line: 5, type: !6, scopeLine: 5, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!23 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 5, type: !22)
!18 = !DICompositeType(tag: DW_TAG_structure_type, name: "Private", size: 128, elements: !19)
!19 = !{!20, !21}
!20 = !DIDerivedType(tag: DW_TAG_member, name: "__vtable", baseType: !15, size: 64, offset: 0)
!21 = !DIDerivedType(tag: DW_TAG_member, name: "b", baseType: !7, size: 64, offset: 64)
!22 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!24 = !DILocation(line: 5, column: 0, scope: !16)
!25 = !DILocation(line: 7, column: 0, scope: !16)
!26 = !DILocation(line: 8, column: 0, scope: !16)
!27 = !DILocation(line: 9, column: 0, scope: !16)
!3 = !DIFile(filename: "Private.m3", directory: "..")
!4 = !DINamespace(name: "Private", scope: !2)
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
