; ModuleID = 'Public'
source_filename = "Public"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @M3toC__FlatTtoS(ptr)
declare void @put_adr(ptr, ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define internal void @Public__Public_M3_tb3d028bc_INIT(ptr %obj) personality ptr @__gxx_personality_v0 {
entry:
  %t1 = load ptr, ptr @tl_obj_-1278203716
  %t2 = getelementptr i8, ptr %t1, i64 112
  %t3 = load i64, ptr %t2
  %t4 = add i64 %t3, 0
  %t5 = getelementptr inbounds i8, ptr %obj, i64 %t4
  store i64 1, ptr %t5
  %t6 = add i64 %t3, 8
  %t7 = getelementptr inbounds i8, ptr %obj, i64 %t6
  store i64 3, ptr %t7
  ret void
}

define void @Public__F1(ptr %a.a) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !18, metadata !DIExpression()), !dbg !19
  store ptr %a.a, ptr %a.slot
  %t1 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !20
  %t2 = load ptr, ptr %a.slot
  call void @put_adr(ptr %t1, ptr %t2), !dbg !20
  %t3 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8)), !dbg !21
  %t4 = load ptr, ptr %a.slot
  %t5 = load ptr, ptr @tl_obj_-1278203716
  %t6 = getelementptr i8, ptr %t5, i64 112
  %t7 = load i64, ptr %t6
  %t8 = getelementptr inbounds i8, ptr %t4, i64 %t7
  %t9 = getelementptr i8, ptr %t8, i64 0
  call void @put_adr(ptr %t3, ptr %t9), !dbg !21
  %t10 = call ptr @M3toC__FlatTtoS(ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8)), !dbg !22
  %t11 = load ptr, ptr %a.slot
  %t12 = load ptr, ptr @tl_obj_-1278203716
  %t13 = getelementptr i8, ptr %t12, i64 112
  %t14 = load i64, ptr %t13
  %t15 = getelementptr inbounds i8, ptr %t11, i64 %t14
  %t16 = getelementptr i8, ptr %t15, i64 8
  call void @put_adr(ptr %t10, ptr %t16), !dbg !22
  ret void
}

define void @Public__Public_M3() personality ptr @__gxx_personality_v0 {
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
@textlit_0 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"Public.F1.a\00" }
@textlit_1 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"Public.F1.a.a\00" }
@textlit_2 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"Public.F1.a.c\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_-1278203716.tc_name = private unnamed_addr constant [9 x i8] c"Public.T\00"
define internal void @tc_obj_-1278203716.linkproc(ptr %tp) personality ptr @__gxx_personality_v0 {
entry:
  %dm.ptr = getelementptr inbounds i8, ptr %tp, i64 136
  %dm = load ptr, ptr %dm.ptr
  %methoff.ptr = getelementptr inbounds i8, ptr %tp, i64 120
  %methoff = load i64, ptr %methoff.ptr
  %slot.abs.0 = sdiv i64 %methoff, 8
  %slot.0 = getelementptr ptr, ptr %dm, i64 %slot.abs.0
  store ptr @Public__F1, ptr %slot.0
  ret void
}
@tc_obj_-1278203716 = internal global %OTC_t {
  i64 0,
  i64 -1278203716,
  i64 u0x160bbd72a5db95ce,
  i8 1,
  i8 2,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr null,
  ptr null,
  ptr @Public__Public_M3_tb3d028bc_INIT,
  ptr null,
  ptr @tc_obj_-1278203716.tc_name,
  ptr null,
  i64 -215227970,
  ptr @tc_obj_-1278203716.linkproc,
  i64 0,
  i64 0,
  i64 8,
  ptr null,
  ptr null
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_obj_-1278203716 = internal global %TypeLink_t {
  ptr null,
  i64 -1278203716
}

define weak ptr @Public_I3(i64 %mode) {
entry:
  ret ptr @Public_M3_info
}

; RT0.ImportInfo chain for Public
declare ptr @M3toC_I3(i64)
@Public_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Public_I3, ptr @Public_M3_imp.1 }
@Public_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @M3toC_I3, ptr null }

; RT0.ModuleInfo for Public (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Public_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_-1278203716,  ; type_cells (+8)
  ptr @tl_obj_-1278203716,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Public_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Public_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @Public_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @Public__Public_M3()
  br label %done
done:
  ret ptr @Public_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "Public__F1", linkageName: "Public__F1", scope: !4, file: !3, line: 5, type: !6, scopeLine: 5, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 5, type: !15)
!19 = !DILocation(line: 5, column: 0, scope: !16)
!20 = !DILocation(line: 7, column: 0, scope: !16)
!21 = !DILocation(line: 8, column: 0, scope: !16)
!22 = !DILocation(line: 9, column: 0, scope: !16)
!3 = !DIFile(filename: "Public.m3", directory: "..")
!4 = !DINamespace(name: "Public", scope: !2)
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
