; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @RTHooks__AllocateTracedObj(ptr)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define internal void @Main__Main_M3_te6e69551_INIT(ptr %obj) personality ptr @__gxx_personality_v0 {
entry:
  %t1 = getelementptr i8, ptr %obj, i64 8
  store i64 2, ptr %t1
  ret void
}

define void @Main__Main() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !18, metadata !DIExpression()), !dbg !19
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_obj_-1278203716
  %t2 = call ptr @RTHooks__AllocateTracedObj(ptr %t1), !dbg !19
  %t3 = bitcast ptr %t2 to ptr
  store ptr %t3, ptr %a.slot
  %t4 = load ptr, ptr %a.slot
  %t5 = load ptr, ptr @tl_obj_-1278203716
  %t6 = getelementptr i8, ptr %t5, i64 120
  %t7 = load i64, ptr %t6
  %t8 = add i64 %t7, 0
  %t9 = bitcast ptr %t4 to ptr
  %t10 = load ptr, ptr %t9
  %t11 = getelementptr inbounds i8, ptr %t10, i64 %t8
  %t12 = load ptr, ptr %t11
  call void %t12(ptr %t4), !dbg !20
  %t13 = load ptr, ptr %a.slot
  %t14 = bitcast ptr %t13 to ptr
  %t15 = load ptr, ptr %t14
  %t16 = load ptr, ptr %t15
  call void %t16(ptr %t13), !dbg !21
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__Main()
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_-421096111.tc_name = private unnamed_addr constant [15 x i8] c"Public.Private\00"
declare void @Private__F2()
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
  ptr @Main__Main_M3_te6e69551_INIT,
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

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Private_I3(i64)
declare ptr @Public_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Private_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Public_I3, ptr null }

; full_rev — REVEAL records for RTLinker.NoteFullRevelation
%RT0_RV_t = type { i64, i64 }
@Main_M3_full_rev = internal global [2 x %RT0_RV_t] [
  %RT0_RV_t { i64 -215227970, i64 -421096111 },
  %RT0_RV_t { i64 0, i64 0 }
]

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_-421096111,  ; type_cells (+8)
  ptr @tl_obj_-1278203716,  ; type_cell_ptrs (+16)
  ptr @Main_M3_full_rev,  ; full_rev (+24)
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
!16 = distinct !DISubprogram(name: "Main__Main", linkageName: "Main__Main", scope: !4, file: !3, line: 4, type: !6, scopeLine: 4, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 4, type: !15)
!19 = !DILocation(line: 4, column: 0, scope: !16)
!20 = !DILocation(line: 7, column: 0, scope: !16)
!21 = !DILocation(line: 8, column: 0, scope: !16)
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
