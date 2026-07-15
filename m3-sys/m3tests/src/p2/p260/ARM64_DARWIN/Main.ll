; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @RTHooks__AllocateOpenArray(ptr, ptr)
declare ptr @memcpy(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__F2(ptr %a.b) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  ret void
}

define void @Main__F3(ptr %a.b) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  call void @llvm.dbg.declare(metadata ptr %a.b, metadata !82, metadata !DIExpression()), !dbg !19
  ret void
}

define ptr @Main__F4(ptr %a.b) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !40, metadata !DIExpression()), !dbg !85
  %b.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !41, metadata !DIExpression()), !dbg !85
  store ptr %a.b, ptr %b.slot
  store ptr null, ptr %_result.slot
  %t1 = load ptr, ptr %b.slot
  ret ptr %t1
}

define void @Main__F5() personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %t2 = alloca { ptr, i64, i64 }
  %t1 = load ptr, ptr @tl_arr_562018850
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 1, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 11 to i64
  store i64 %t7, ptr %t6
  %t8 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !86
  %t9 = bitcast ptr %t8 to ptr
  ret void
}

define void @Main__F6() personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %t2 = alloca { ptr, i64, i64, i64 }
  %t1 = load ptr, ptr @tl_arr_2039485594
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 2, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 11 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 12 to i64
  store i64 %t9, ptr %t8
  %t10 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !87
  %t11 = bitcast ptr %t10 to ptr
  ret void
}

define void @Main__F6v() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %t2 = alloca { ptr, i64, i64, i64 }
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !53, metadata !DIExpression()), !dbg !88
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_arr_2039485594
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 2, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 13 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 14 to i64
  store i64 %t9, ptr %t8
  %t10 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !88
  %t11 = bitcast ptr %t10 to ptr
  store ptr %t11, ptr %a.slot
  ret void
}

define void @Main__F6_3() personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %t2 = alloca { ptr, i64, i64, i64, i64 }
  %t1 = load ptr, ptr @tl_arr_-653524506
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 3, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 15 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 2 to i64
  store i64 %t9, ptr %t8
  %t10 = getelementptr i8, ptr %t2, i64 32
  %t11 = bitcast i64 3 to i64
  store i64 %t11, ptr %t10
  %t12 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !90
  %t13 = bitcast ptr %t12 to ptr
  ret void
}

define void @Main__F6_3v() personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %t2 = alloca { ptr, i64, i64, i64, i64 }
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !61, metadata !DIExpression()), !dbg !91
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_arr_-653524506
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 3, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 16 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 4 to i64
  store i64 %t9, ptr %t8
  %t10 = getelementptr i8, ptr %t2, i64 32
  %t11 = bitcast i64 5 to i64
  store i64 %t11, ptr %t10
  %t12 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !91
  %t13 = bitcast ptr %t12 to ptr
  store ptr %t13, ptr %a.slot
  ret void
}

define void @Main__F6_4v() personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %t2 = alloca { ptr, i64, i64, i64, i64, i64 }
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !70, metadata !DIExpression()), !dbg !93
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_arr_-940375170
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 4, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 17 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 7 to i64
  store i64 %t9, ptr %t8
  %t10 = getelementptr i8, ptr %t2, i64 32
  %t11 = bitcast i64 5 to i64
  store i64 %t11, ptr %t10
  %t12 = getelementptr i8, ptr %t2, i64 40
  %t13 = bitcast i64 2 to i64
  store i64 %t13, ptr %t12
  %t14 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !93
  %t15 = bitcast ptr %t14 to ptr
  store ptr %t15, ptr %a.slot
  ret void
}

define void @Main__F6_4() personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %t2 = alloca { ptr, i64, i64, i64, i64, i64 }
  %t1 = load ptr, ptr @tl_arr_-940375170
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 4, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 11 to i64
  store i64 %t7, ptr %t6
  %t8 = getelementptr i8, ptr %t2, i64 24
  %t9 = bitcast i64 21 to i64
  store i64 %t9, ptr %t8
  %t10 = getelementptr i8, ptr %t2, i64 32
  %t11 = bitcast i64 31 to i64
  store i64 %t11, ptr %t10
  %t12 = getelementptr i8, ptr %t2, i64 40
  %t13 = bitcast i64 24 to i64
  store i64 %t13, ptr %t12
  %t14 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !95
  %t15 = bitcast ptr %t14 to ptr
  ret void
}

define void @Main__F7() personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %t23 = alloca { ptr, i64 }
  %t15 = alloca { ptr, i64 }
  %t2 = alloca { ptr, i64, i64 }
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !75, metadata !DIExpression()), !dbg !97
  store ptr null, ptr %a.slot
  %t1 = load ptr, ptr @tl_arr_562018850
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 1, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 12 to i64
  store i64 %t7, ptr %t6
  %t8 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !97
  %t9 = bitcast ptr %t8 to ptr
  store ptr %t9, ptr %a.slot
  %t10 = load ptr, ptr %a.slot
  %t11 = getelementptr i8, ptr %t10, i64 0
  %t12 = load ptr, ptr %t11
  %t13 = mul i64 0, 8
  %t14 = getelementptr inbounds i8, ptr %t12, i64 %t13
  %t16 = getelementptr i8, ptr %t15, i64 0
  store ptr %t14, ptr %t16
  %t17 = getelementptr i8, ptr %t15, i64 8
  store i64 4, ptr %t17
  %t18 = load ptr, ptr %a.slot
  %t19 = getelementptr i8, ptr %t18, i64 0
  %t20 = load ptr, ptr %t19
  %t21 = mul i64 1, 8
  %t22 = getelementptr inbounds i8, ptr %t20, i64 %t21
  %t24 = getelementptr i8, ptr %t23, i64 0
  store ptr %t22, ptr %t24
  %t25 = getelementptr i8, ptr %t23, i64 8
  store i64 4, ptr %t25
  %t26 = load { ptr, i64 }, ptr %t23
  %t27 = load { ptr, i64 }, ptr %t15
  %__ll1 = extractvalue { ptr, i64 } %t27, 0
  %t28 = getelementptr inbounds i64, ptr %__ll1, i64 0
  %__ll2 = extractvalue { ptr, i64 } %t26, 0
  %t29 = getelementptr inbounds i64, ptr %__ll2, i64 0
  %t30 = extractvalue { ptr, i64 } %t26, 1
  %t31 = mul i64 %t30, 8
  %t32 = call ptr @memcpy(ptr %t28, ptr %t29, i64 %t31), !dbg !96
  ret void
}

define ptr @Main__F1() personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %t35 = alloca { ptr, i64 }
  %t27 = alloca { ptr, i64 }
  %t18 = alloca { ptr, i64 }
  %t2 = alloca { ptr, i64, i64 }
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !80, metadata !DIExpression()), !dbg !100
  %a.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !81, metadata !DIExpression()), !dbg !100
  store ptr null, ptr %a.slot
  store ptr null, ptr %_result.slot
  %t1 = load ptr, ptr @tl_arr_562018850
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 1, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = bitcast i64 10 to i64
  store i64 %t7, ptr %t6
  %t8 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !100
  %t9 = bitcast ptr %t8 to ptr
  store ptr %t9, ptr %a.slot
  %t10 = load ptr, ptr %a.slot
  %t11 = getelementptr i8, ptr %t10, i64 0
  %t12 = load ptr, ptr %t11
  %t13 = getelementptr i8, ptr %t10, i64 8
  %t14 = load i64, ptr %t13
  %t15 = mul i64 %t14, 8
  %t16 = alloca i8, i64 %t15
  %t17 = call ptr @memcpy(ptr %t16, ptr %t12, i64 %t15), !dbg !99
  %t19 = getelementptr i8, ptr %t18, i64 0
  store ptr %t16, ptr %t19
  %t20 = getelementptr i8, ptr %t18, i64 8
  store i64 %t14, ptr %t20
  call void @Main__F2(ptr %t18), !dbg !99
  %t21 = load ptr, ptr %a.slot
  call void @Main__F3(ptr %t21), !dbg !101
  %t22 = load ptr, ptr %a.slot
  %t23 = getelementptr i8, ptr %t22, i64 0
  %t24 = load ptr, ptr %t23
  %t25 = mul i64 0, 8
  %t26 = getelementptr inbounds i8, ptr %t24, i64 %t25
  %t28 = getelementptr i8, ptr %t27, i64 0
  store ptr %t26, ptr %t28
  %t29 = getelementptr i8, ptr %t27, i64 8
  store i64 4, ptr %t29
  %t30 = load ptr, ptr %a.slot
  %t31 = getelementptr i8, ptr %t30, i64 0
  %t32 = load ptr, ptr %t31
  %t33 = mul i64 0, 8
  %t34 = getelementptr inbounds i8, ptr %t32, i64 %t33
  %t36 = getelementptr i8, ptr %t35, i64 0
  store ptr %t34, ptr %t36
  %t37 = getelementptr i8, ptr %t35, i64 8
  store i64 4, ptr %t37
  %t38 = load { ptr, i64 }, ptr %t35
  %t39 = load { ptr, i64 }, ptr %t27
  %__ll1 = extractvalue { ptr, i64 } %t39, 0
  %t40 = getelementptr inbounds i64, ptr %__ll1, i64 0
  %__ll2 = extractvalue { ptr, i64 } %t38, 0
  %t41 = getelementptr inbounds i64, ptr %__ll2, i64 0
  %t42 = extractvalue { ptr, i64 } %t38, 1
  %t43 = mul i64 %t42, 8
  %t44 = call ptr @memcpy(ptr %t40, ptr %t41, i64 %t43), !dbg !98
  %t45 = load ptr, ptr %a.slot
  %t46 = call ptr @Main__F4(ptr %t45), !dbg !102
  ret ptr %t46
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__F5()
  call void @Main__F6()
  call void @Main__F6v()
  call void @Main__F6_3()
  call void @Main__F6_3v()
  call void @Main__F6_4()
  call void @Main__F6_4v()
  call void @Main__F7()
  %t1 = call ptr @Main__F1()
  %t2 = call ptr @Main__F4(ptr %t1)
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_arr_-940375170_gc_map = internal constant [4 x i8] [i8 24, i8 4, i8 4, i8 0]
@tc_arr_-940375170 = internal global %ATC_t {
  i64 0,
  i64 -940375170,
  i64 u0x03219373c4d2900d,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 40,
  ptr null,
  ptr @tc_arr_-940375170_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_-653524506,
  i64 4,
  i64 8
}
@tc_arr_-653524506_gc_map = internal constant [4 x i8] [i8 24, i8 3, i8 4, i8 0]
@tc_arr_-653524506 = internal global %ATC_t {
  i64 0,
  i64 -653524506,
  i64 u0x12e1340fcbed35e9,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 32,
  ptr null,
  ptr @tc_arr_-653524506_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_2039485594,
  i64 3,
  i64 8
}
@tc_arr_2039485594_gc_map = internal constant [4 x i8] [i8 24, i8 2, i8 4, i8 0]
@tc_arr_2039485594 = internal global %ATC_t {
  i64 0,
  i64 2039485594,
  i64 u0x02098d2f7b9999b5,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 24,
  ptr null,
  ptr @tc_arr_2039485594_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  i64 2,
  i64 8
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_arr_562018850 = internal global %TypeLink_t {
  ptr null,
  i64 562018850
}
@tl_arr_2039485594 = internal global %TypeLink_t {
  ptr @tl_arr_562018850,
  i64 2039485594
}
@tl_arr_-653524506 = internal global %TypeLink_t {
  ptr @tl_arr_2039485594,
  i64 -653524506
}
@tl_arr_-940375170 = internal global %TypeLink_t {
  ptr @tl_arr_-653524506,
  i64 -940375170
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
  ptr @tc_arr_-940375170,  ; type_cells (+8)
  ptr @tl_arr_-940375170,  ; type_cell_ptrs (+16)
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
!16 = distinct !DISubprogram(name: "Main__F2", linkageName: "Main__F2", scope: !4, file: !3, line: 5, type: !6, scopeLine: 5, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__F3", linkageName: "Main__F3", scope: !4, file: !3, line: 6, type: !6, scopeLine: 6, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__F4", linkageName: "Main__F4", scope: !4, file: !3, line: 7, type: !6, scopeLine: 7, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__F5", linkageName: "Main__F5", scope: !4, file: !3, line: 9, type: !6, scopeLine: 9, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Main__F6", linkageName: "Main__F6", scope: !4, file: !3, line: 13, type: !6, scopeLine: 13, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Main__F6v", linkageName: "Main__F6v", scope: !4, file: !3, line: 17, type: !6, scopeLine: 17, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Main__F6_3", linkageName: "Main__F6_3", scope: !4, file: !3, line: 21, type: !6, scopeLine: 21, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Main__F6_3v", linkageName: "Main__F6_3v", scope: !4, file: !3, line: 25, type: !6, scopeLine: 25, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Main__F6_4v", linkageName: "Main__F6_4v", scope: !4, file: !3, line: 30, type: !6, scopeLine: 30, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Main__F6_4", linkageName: "Main__F6_4", scope: !4, file: !3, line: 35, type: !6, scopeLine: 35, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Main__F7", linkageName: "Main__F7", scope: !4, file: !3, line: 39, type: !6, scopeLine: 39, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Main__F1", linkageName: "Main__F1", scope: !4, file: !3, line: 45, type: !6, scopeLine: 45, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!23 = !DILocation(line: 0, column: 0, scope: !22)
!25 = !DILocation(line: 0, column: 0, scope: !24)
!27 = !DILocation(line: 0, column: 0, scope: !26)
!29 = !DILocation(line: 0, column: 0, scope: !28)
!31 = !DILocation(line: 0, column: 0, scope: !30)
!33 = !DILocation(line: 0, column: 0, scope: !32)
!35 = !DILocation(line: 0, column: 0, scope: !34)
!37 = !DILocation(line: 0, column: 0, scope: !36)
!39 = !DILocation(line: 0, column: 0, scope: !38)
!40 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 7, type: !15)
!41 = !DILocalVariable(name: "b", scope: !20, file: !3, line: 7, type: !15)
!53 = !DILocalVariable(name: "a", scope: !26, file: !3, line: 17, type: !15)
!61 = !DILocalVariable(name: "a", scope: !30, file: !3, line: 25, type: !15)
!70 = !DILocalVariable(name: "a", scope: !32, file: !3, line: 30, type: !15)
!75 = !DILocalVariable(name: "a", scope: !36, file: !3, line: 39, type: !15)
!80 = !DILocalVariable(name: "_result", scope: !38, file: !3, line: 45, type: !15)
!81 = !DILocalVariable(name: "a", scope: !38, file: !3, line: 45, type: !15)
!82 = !DILocalVariable(name: "a.b", scope: !18, file: !3, line: 6, type: !71)
!42 = !DICompositeType(tag: DW_TAG_structure_type, name: "__oa_shape", size: 64, elements: !43)
!43 = !{!44, !45, !46}
!44 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!45 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!46 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!47 = !DICompositeType(tag: DW_TAG_structure_type, name: "__oa_shape", size: 64, elements: !48)
!48 = !{!49, !50, !51, !52}
!49 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!50 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!51 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!52 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!54 = !DICompositeType(tag: DW_TAG_structure_type, name: "__oa_shape", size: 64, elements: !55)
!55 = !{!56, !57, !58, !59, !60}
!56 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!57 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!58 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!59 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!62 = !DICompositeType(tag: DW_TAG_structure_type, name: "__oa_shape", size: 64, elements: !63)
!63 = !{!64, !65, !66, !67, !68, !69}
!64 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!65 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!66 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!67 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!68 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!69 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!71 = !DICompositeType(tag: DW_TAG_structure_type, name: "__dope_1", size: 128, elements: !72)
!72 = !{!73, !74}
!73 = !DIDerivedType(tag: DW_TAG_member, name: "data", baseType: !15, size: 64, offset: 0)
!74 = !DIDerivedType(tag: DW_TAG_member, name: "count", baseType: !7, size: 64, offset: 64)
!76 = !DICompositeType(tag: DW_TAG_structure_type, name: "", size: 64, elements: !77)
!77 = !{!78, !79}
!78 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!79 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!83 = !DILocation(line: 5, column: 0, scope: !16)
!84 = !DILocation(line: 6, column: 0, scope: !18)
!85 = !DILocation(line: 7, column: 0, scope: !20)
!86 = !DILocation(line: 10, column: 0, scope: !22)
!87 = !DILocation(line: 14, column: 0, scope: !24)
!88 = !DILocation(line: 17, column: 0, scope: !26)
!89 = !DILocation(line: 18, column: 0, scope: !26)
!90 = !DILocation(line: 22, column: 0, scope: !28)
!91 = !DILocation(line: 25, column: 0, scope: !30)
!92 = !DILocation(line: 26, column: 0, scope: !30)
!93 = !DILocation(line: 30, column: 0, scope: !32)
!94 = !DILocation(line: 31, column: 0, scope: !32)
!95 = !DILocation(line: 36, column: 0, scope: !34)
!96 = !DILocation(line: 42, column: 0, scope: !36)
!97 = !DILocation(line: 39, column: 0, scope: !36)
!98 = !DILocation(line: 50, column: 0, scope: !38)
!99 = !DILocation(line: 48, column: 0, scope: !38)
!100 = !DILocation(line: 45, column: 0, scope: !38)
!101 = !DILocation(line: 49, column: 0, scope: !38)
!102 = !DILocation(line: 51, column: 0, scope: !38)
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
