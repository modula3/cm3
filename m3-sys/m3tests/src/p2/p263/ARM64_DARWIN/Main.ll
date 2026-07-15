; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @RTHooks__AllocateTracedRef(ptr)
declare i8 @F0__F1(ptr)
declare i8 @F1__F1(ptr)
declare i8 @F2__F1(ptr)
declare i8 @F4094__F1(ptr)
declare i8 @F4095__F1(ptr)
declare i8 @F4096__F1(ptr)
declare i8 @F4097__F1(ptr)
declare i8 @F4096x8m2__F1(ptr)
declare i8 @F4096x8m1__F1(ptr)
declare i8 @F4096x8__F1(ptr)
declare i8 @F4096x8p1__F1(ptr)
declare i8 @F4096x8p2__F1(ptr)
declare i1 @RTParams__IsPresent(ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)

define void @Main__F3() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %t1 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-1729626106), !dbg !18
  %t2 = bitcast ptr %t1 to ptr
  %t3 = getelementptr i8, ptr %t2, i64 0
  store [1 x i8] zeroinitializer, ptr %t3
  %t4 = call i8 @F0__F1(ptr %t2), !dbg !18
  %t5 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-2066005942), !dbg !19
  %t6 = bitcast ptr %t5 to ptr
  %t7 = getelementptr i8, ptr %t6, i64 0
  store [2 x i8] zeroinitializer, ptr %t7
  %t8 = call i8 @F1__F1(ptr %t6), !dbg !19
  %t9 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-2076233071), !dbg !20
  %t10 = bitcast ptr %t9 to ptr
  %t11 = getelementptr i8, ptr %t10, i64 0
  store [3 x i8] zeroinitializer, ptr %t11
  %t12 = call i8 @F2__F1(ptr %t10), !dbg !20
  %t13 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_1608408759), !dbg !21
  %t14 = bitcast ptr %t13 to ptr
  %t15 = getelementptr i8, ptr %t14, i64 0
  store [4095 x i8] zeroinitializer, ptr %t15
  %t16 = call i8 @F4094__F1(ptr %t14), !dbg !21
  %t17 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_1139613435), !dbg !22
  %t18 = bitcast ptr %t17 to ptr
  %t19 = getelementptr i8, ptr %t18, i64 0
  store [4096 x i8] zeroinitializer, ptr %t19
  %t20 = call i8 @F4095__F1(ptr %t18), !dbg !22
  %t21 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_1124690976), !dbg !23
  %t22 = bitcast ptr %t21 to ptr
  %t23 = getelementptr i8, ptr %t22, i64 0
  store [4097 x i8] zeroinitializer, ptr %t23
  %t24 = call i8 @F4096__F1(ptr %t22), !dbg !23
  %t25 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_1597647980), !dbg !24
  %t26 = bitcast ptr %t25 to ptr
  %t27 = getelementptr i8, ptr %t26, i64 0
  store [4098 x i8] zeroinitializer, ptr %t27
  %t28 = call i8 @F4097__F1(ptr %t26), !dbg !24
  %t29 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-2098750297), !dbg !25
  %t30 = bitcast ptr %t29 to ptr
  %t31 = getelementptr i8, ptr %t30, i64 0
  store [32767 x i8] zeroinitializer, ptr %t31
  %t32 = call i8 @F4096x8m2__F1(ptr %t30), !dbg !25
  %t33 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-1630216981), !dbg !26
  %t34 = bitcast ptr %t33 to ptr
  %t35 = getelementptr i8, ptr %t34, i64 0
  store [32768 x i8] zeroinitializer, ptr %t35
  %t36 = call i8 @F4096x8m1__F1(ptr %t34), !dbg !26
  %t37 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-470639269), !dbg !27
  %t38 = bitcast ptr %t37 to ptr
  %t39 = getelementptr i8, ptr %t38, i64 0
  store [32769 x i8] zeroinitializer, ptr %t39
  %t40 = call i8 @F4096x8__F1(ptr %t38), !dbg !27
  %t41 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-4072169), !dbg !28
  %t42 = bitcast ptr %t41 to ptr
  %t43 = getelementptr i8, ptr %t42, i64 0
  store [32770 x i8] zeroinitializer, ptr %t43
  %t44 = call i8 @F4096x8p1__F1(ptr %t42), !dbg !28
  %t45 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-491926834), !dbg !29
  %t46 = bitcast ptr %t45 to ptr
  %t47 = getelementptr i8, ptr %t46, i64 0
  store [32771 x i8] zeroinitializer, ptr %t47
  %t48 = call i8 @F4096x8p2__F1(ptr %t46), !dbg !29
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__F3()
  %t1 = call i1 @RTParams__IsPresent(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8))
  br i1 %t1, label %if.then.1, label %if.merge.2
if.then.1:
  %t2 = call i8 @F4096x8p2__F1(ptr null)
  br label %if.merge.2
if.merge.2:
  %t3 = call i1 @RTParams__IsPresent(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8))
  br i1 %t3, label %if.then.3, label %if.merge.4
if.then.3:
  %t4 = call i8 @F0__F1(ptr null)
  br label %if.merge.4
if.merge.4:
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
@textlit_0 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"checked\00" }
@textlit_1 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"unchecked\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_-1729626106 = internal global %TC_t {
  i64 0,
  i64 -1729626106,
  i64 u0x15fde9028d15e904,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 1,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-2066005942
}
@tc_ref_-2066005942 = internal global %TC_t {
  i64 0,
  i64 -2066005942,
  i64 u0x0303485b87d80811,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 2,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-2076233071
}
@tc_ref_-2076233071 = internal global %TC_t {
  i64 0,
  i64 -2076233071,
  i64 u0x18dd2e589ce21cc9,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 3,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_1608408759
}
@tc_ref_1608408759 = internal global %TC_t {
  i64 0,
  i64 1608408759,
  i64 u0x199b447d46451aca,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 4095,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_1139613435
}
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
  ptr @tc_ref_1124690976
}
@tc_ref_1124690976 = internal global %TC_t {
  i64 0,
  i64 1124690976,
  i64 u0x14bb832757b2ef07,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 4097,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_1597647980
}
@tc_ref_1597647980 = internal global %TC_t {
  i64 0,
  i64 1597647980,
  i64 u0x0245227e5d7f0e12,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 4098,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-2098750297
}
@tc_ref_-2098750297 = internal global %TC_t {
  i64 0,
  i64 -2098750297,
  i64 u0x0904a2528be33ef5,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 32767,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-1630216981
}
@tc_ref_-1630216981 = internal global %TC_t {
  i64 0,
  i64 -1630216981,
  i64 u0x1ffa030b812edfe0,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 32768,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-470639269
}
@tc_ref_-470639269 = internal global %TC_t {
  i64 0,
  i64 -470639269,
  i64 u0x0ec373bded31eee6,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 32769,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-4072169
}
@tc_ref_-4072169 = internal global %TC_t {
  i64 0,
  i64 -4072169,
  i64 u0x183dd2e4e7fc0ff3,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 32770,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-491926834
}
@tc_ref_-491926834 = internal global %TC_t {
  i64 0,
  i64 -491926834,
  i64 u0x0e51c79fecfc0d51,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 32771,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @RTParams_I3(i64)
declare ptr @F4096x8m2_I3(i64)
declare ptr @F4096x8m1_I3(i64)
declare ptr @F4096x8p2_I3(i64)
declare ptr @F4096x8p1_I3(i64)
declare ptr @F4096x8_I3(i64)
declare ptr @F4097_I3(i64)
declare ptr @F4096_I3(i64)
declare ptr @F4095_I3(i64)
declare ptr @F4094_I3(i64)
declare ptr @F2_I3(i64)
declare ptr @F1_I3(i64)
declare ptr @F0_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTParams_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096x8m2_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096x8m1_I3, ptr @Main_M3_imp.4 }
@Main_M3_imp.4 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096x8p2_I3, ptr @Main_M3_imp.5 }
@Main_M3_imp.5 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096x8p1_I3, ptr @Main_M3_imp.6 }
@Main_M3_imp.6 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096x8_I3, ptr @Main_M3_imp.7 }
@Main_M3_imp.7 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4097_I3, ptr @Main_M3_imp.8 }
@Main_M3_imp.8 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4096_I3, ptr @Main_M3_imp.9 }
@Main_M3_imp.9 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4095_I3, ptr @Main_M3_imp.10 }
@Main_M3_imp.10 = internal global { ptr, ptr, ptr } { ptr null, ptr @F4094_I3, ptr @Main_M3_imp.11 }
@Main_M3_imp.11 = internal global { ptr, ptr, ptr } { ptr null, ptr @F2_I3, ptr @Main_M3_imp.12 }
@Main_M3_imp.12 = internal global { ptr, ptr, ptr } { ptr null, ptr @F1_I3, ptr @Main_M3_imp.13 }
@Main_M3_imp.13 = internal global { ptr, ptr, ptr } { ptr null, ptr @F0_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_-1729626106,  ; type_cells (+8)
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
!16 = distinct !DISubprogram(name: "Main__F3", linkageName: "Main__F3", scope: !4, file: !3, line: 10, type: !6, scopeLine: 10, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocation(line: 12, column: 0, scope: !16)
!19 = !DILocation(line: 13, column: 0, scope: !16)
!20 = !DILocation(line: 14, column: 0, scope: !16)
!21 = !DILocation(line: 16, column: 0, scope: !16)
!22 = !DILocation(line: 17, column: 0, scope: !16)
!23 = !DILocation(line: 18, column: 0, scope: !16)
!24 = !DILocation(line: 19, column: 0, scope: !16)
!25 = !DILocation(line: 23, column: 0, scope: !16)
!26 = !DILocation(line: 24, column: 0, scope: !16)
!27 = !DILocation(line: 25, column: 0, scope: !16)
!28 = !DILocation(line: 26, column: 0, scope: !16)
!29 = !DILocation(line: 27, column: 0, scope: !16)
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
