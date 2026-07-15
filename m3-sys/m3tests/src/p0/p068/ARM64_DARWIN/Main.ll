; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @RTHooks__CheckLoadTracedRef(ptr)
declare void @RTHooks__CheckStoreTraced(ptr)


declare ptr @RTHooks__AllocateTracedRef(ptr)
declare ptr @RTHooks__AllocateOpenArray(ptr, ptr)
declare void @_RTHeap__Print(ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t49 = alloca i64
  %i.slot = alloca i64
  %t35 = alloca { ptr, i64, i64 }
  %t26 = alloca { ptr, i64, i64 }
  %t17 = alloca { ptr, i64, i64 }
  %t8 = alloca { ptr, i64, i64 }
  %t1 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-1467451074)
  %t2 = bitcast ptr %t1 to ptr
  store ptr %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 840)
  %t3 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-1758373035)
  %t4 = bitcast ptr %t3 to ptr
  store ptr %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 848)
  %t5 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-318626117)
  %t6 = bitcast ptr %t5 to ptr
  store ptr %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 856)
  %t7 = load ptr, ptr @tl_arr_43642924
  %t9 = getelementptr i8, ptr %t8, i64 16
  %t10 = getelementptr i8, ptr %t8, i64 0
  store ptr %t9, ptr %t10
  %t11 = getelementptr i8, ptr %t8, i64 8
  store i64 1, ptr %t11
  %t12 = getelementptr i8, ptr %t8, i64 16
  %t13 = bitcast i64 11 to i64
  store i64 %t13, ptr %t12
  %t14 = call ptr @RTHooks__AllocateOpenArray(ptr %t7, ptr %t8)
  %t15 = bitcast ptr %t14 to ptr
  store ptr %t15, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 864)
  %t16 = load ptr, ptr @tl_arr_-797892043
  %t18 = getelementptr i8, ptr %t17, i64 16
  %t19 = getelementptr i8, ptr %t17, i64 0
  store ptr %t18, ptr %t19
  %t20 = getelementptr i8, ptr %t17, i64 8
  store i64 1, ptr %t20
  %t21 = getelementptr i8, ptr %t17, i64 16
  %t22 = bitcast i64 11 to i64
  store i64 %t22, ptr %t21
  %t23 = call ptr @RTHooks__AllocateOpenArray(ptr %t16, ptr %t17)
  %t24 = bitcast ptr %t23 to ptr
  store ptr %t24, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 872)
  %t25 = load ptr, ptr @tl_arr_-1649389748
  %t27 = getelementptr i8, ptr %t26, i64 16
  %t28 = getelementptr i8, ptr %t26, i64 0
  store ptr %t27, ptr %t28
  %t29 = getelementptr i8, ptr %t26, i64 8
  store i64 1, ptr %t29
  %t30 = getelementptr i8, ptr %t26, i64 16
  %t31 = bitcast i64 11 to i64
  store i64 %t31, ptr %t30
  %t32 = call ptr @RTHooks__AllocateOpenArray(ptr %t25, ptr %t26)
  %t33 = bitcast ptr %t32 to ptr
  store ptr %t33, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 880)
  store ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 888)
  %t34 = load ptr, ptr @tl_arr_839756992
  %t36 = getelementptr i8, ptr %t35, i64 16
  %t37 = getelementptr i8, ptr %t35, i64 0
  store ptr %t36, ptr %t37
  %t38 = getelementptr i8, ptr %t35, i64 8
  store i64 1, ptr %t38
  %t39 = getelementptr i8, ptr %t35, i64 16
  %t40 = bitcast i64 2 to i64
  store i64 %t40, ptr %t39
  %t41 = call ptr @RTHooks__AllocateOpenArray(ptr %t34, ptr %t35)
  %t42 = bitcast ptr %t41 to ptr
  store ptr %t42, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 896)
  %t43 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 896)
  %__gc_nil.1 = icmp eq ptr %t43, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t43 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t43, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t43)
  br label %gc.skip.1
gc.skip.1:
  %t44 = load { ptr, i64 }, ptr %t43
  %__ll2 = extractvalue { ptr, i64 } %t44, 0
  %t45 = getelementptr inbounds ptr, ptr %__ll2, i64 0
  %__gc_whptr.3 = getelementptr i8, ptr %t43, i64 -8
  %__gc_whdr.3 = load i64, ptr %__gc_whptr.3
  %__gc_wdb.3 = and i64 %__gc_whdr.3, 2097152
  %__gc_wdirty.3 = icmp ne i64 %__gc_wdb.3, 0
  br i1 %__gc_wdirty.3, label %gc.wskip.3, label %gc.wslow.3
gc.wslow.3:
  call void @RTHooks__CheckStoreTraced(ptr %t43)
  br label %gc.wskip.3
gc.wskip.3:
  store ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8), ptr %t45
  %t46 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 896)
  %__gc_nil.4 = icmp eq ptr %t46, null
  br i1 %__gc_nil.4, label %gc.skip.4, label %gc.check.4
gc.check.4:
  %__gc_int.4 = ptrtoint ptr %t46 to i64
  %__gc_low.4 = and i64 %__gc_int.4, 1
  %__gc_ma.4 = icmp ne i64 %__gc_low.4, 0
  br i1 %__gc_ma.4, label %gc.skip.4, label %gc.gray.4
gc.gray.4:
  %__gc_hptr.4 = getelementptr i8, ptr %t46, i64 -8
  %__gc_hdr.4 = load i64, ptr %__gc_hptr.4
  %__gc_gb.4 = and i64 %__gc_hdr.4, 4194304
  %__gc_gr.4 = icmp ne i64 %__gc_gb.4, 0
  br i1 %__gc_gr.4, label %gc.slow.4, label %gc.skip.4
gc.slow.4:
  call void @RTHooks__CheckLoadTracedRef(ptr %t46)
  br label %gc.skip.4
gc.skip.4:
  %t47 = load { ptr, i64 }, ptr %t46
  %__ll5 = extractvalue { ptr, i64 } %t47, 0
  %t48 = getelementptr inbounds ptr, ptr %__ll5, i64 1
  %__gc_whptr.6 = getelementptr i8, ptr %t46, i64 -8
  %__gc_whdr.6 = load i64, ptr %__gc_whptr.6
  %__gc_wdb.6 = and i64 %__gc_whdr.6, 2097152
  %__gc_wdirty.6 = icmp ne i64 %__gc_wdb.6, 0
  br i1 %__gc_wdirty.6, label %gc.wskip.6, label %gc.wslow.6
gc.wslow.6:
  call void @RTHooks__CheckStoreTraced(ptr %t46)
  br label %gc.wskip.6
gc.wskip.6:
  store ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8), ptr %t48
  store i64 0, ptr %i.slot
  store i64 10, ptr %t49
  br label %for.header.1
for.header.1:
  %t50 = load i64, ptr %i.slot
  %t51 = load i64, ptr %t49
  %t52 = icmp sle i64 %t50, %t51
  br i1 %t52, label %for.body.2, label %for.exit.3
for.body.2:
  %t53 = load i64, ptr %i.slot
  %t54 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 840)
  %__gc_nil.7 = icmp eq ptr %t54, null
  br i1 %__gc_nil.7, label %gc.skip.7, label %gc.check.7
gc.check.7:
  %__gc_int.7 = ptrtoint ptr %t54 to i64
  %__gc_low.7 = and i64 %__gc_int.7, 1
  %__gc_ma.7 = icmp ne i64 %__gc_low.7, 0
  br i1 %__gc_ma.7, label %gc.skip.7, label %gc.gray.7
gc.gray.7:
  %__gc_hptr.7 = getelementptr i8, ptr %t54, i64 -8
  %__gc_hdr.7 = load i64, ptr %__gc_hptr.7
  %__gc_gb.7 = and i64 %__gc_hdr.7, 4194304
  %__gc_gr.7 = icmp ne i64 %__gc_gb.7, 0
  br i1 %__gc_gr.7, label %gc.slow.7, label %gc.skip.7
gc.slow.7:
  call void @RTHooks__CheckLoadTracedRef(ptr %t54)
  br label %gc.skip.7
gc.skip.7:
  %t55 = getelementptr inbounds [11 x i64], ptr %t54, i64 0, i64 %t53
  %t56 = load i64, ptr %i.slot
  %t57 = sub i64 10, %t56
  store i64 %t57, ptr %t55
  %t58 = load i64, ptr %i.slot
  %t59 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 848)
  %__gc_nil.8 = icmp eq ptr %t59, null
  br i1 %__gc_nil.8, label %gc.skip.8, label %gc.check.8
gc.check.8:
  %__gc_int.8 = ptrtoint ptr %t59 to i64
  %__gc_low.8 = and i64 %__gc_int.8, 1
  %__gc_ma.8 = icmp ne i64 %__gc_low.8, 0
  br i1 %__gc_ma.8, label %gc.skip.8, label %gc.gray.8
gc.gray.8:
  %__gc_hptr.8 = getelementptr i8, ptr %t59, i64 -8
  %__gc_hdr.8 = load i64, ptr %__gc_hptr.8
  %__gc_gb.8 = and i64 %__gc_hdr.8, 4194304
  %__gc_gr.8 = icmp ne i64 %__gc_gb.8, 0
  br i1 %__gc_gr.8, label %gc.slow.8, label %gc.skip.8
gc.slow.8:
  call void @RTHooks__CheckLoadTracedRef(ptr %t59)
  br label %gc.skip.8
gc.skip.8:
  %t60 = getelementptr inbounds [11 x i8], ptr %t59, i64 0, i64 %t58
  %t61 = load i64, ptr %i.slot
  %t62 = add i64 65, %t61
  %t63 = trunc i64 %t62 to i8
  store i8 %t63, ptr %t60
  %t64 = load i64, ptr %i.slot
  %t65 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 856)
  %__gc_nil.9 = icmp eq ptr %t65, null
  br i1 %__gc_nil.9, label %gc.skip.9, label %gc.check.9
gc.check.9:
  %__gc_int.9 = ptrtoint ptr %t65 to i64
  %__gc_low.9 = and i64 %__gc_int.9, 1
  %__gc_ma.9 = icmp ne i64 %__gc_low.9, 0
  br i1 %__gc_ma.9, label %gc.skip.9, label %gc.gray.9
gc.gray.9:
  %__gc_hptr.9 = getelementptr i8, ptr %t65, i64 -8
  %__gc_hdr.9 = load i64, ptr %__gc_hptr.9
  %__gc_gb.9 = and i64 %__gc_hdr.9, 4194304
  %__gc_gr.9 = icmp ne i64 %__gc_gb.9, 0
  br i1 %__gc_gr.9, label %gc.slow.9, label %gc.skip.9
gc.slow.9:
  call void @RTHooks__CheckLoadTracedRef(ptr %t65)
  br label %gc.skip.9
gc.skip.9:
  %t66 = getelementptr inbounds [11 x i8], ptr %t65, i64 0, i64 %t64
  %t67 = load i64, ptr %i.slot
  %t68 = add i64 97, %t67
  %t69 = trunc i64 %t68 to i8
  store i8 %t69, ptr %t66
  %t70 = load i64, ptr %i.slot
  %t71 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 864)
  %__gc_nil.10 = icmp eq ptr %t71, null
  br i1 %__gc_nil.10, label %gc.skip.10, label %gc.check.10
gc.check.10:
  %__gc_int.10 = ptrtoint ptr %t71 to i64
  %__gc_low.10 = and i64 %__gc_int.10, 1
  %__gc_ma.10 = icmp ne i64 %__gc_low.10, 0
  br i1 %__gc_ma.10, label %gc.skip.10, label %gc.gray.10
gc.gray.10:
  %__gc_hptr.10 = getelementptr i8, ptr %t71, i64 -8
  %__gc_hdr.10 = load i64, ptr %__gc_hptr.10
  %__gc_gb.10 = and i64 %__gc_hdr.10, 4194304
  %__gc_gr.10 = icmp ne i64 %__gc_gb.10, 0
  br i1 %__gc_gr.10, label %gc.slow.10, label %gc.skip.10
gc.slow.10:
  call void @RTHooks__CheckLoadTracedRef(ptr %t71)
  br label %gc.skip.10
gc.skip.10:
  %t72 = load { ptr, i64 }, ptr %t71
  %__ll11 = extractvalue { ptr, i64 } %t72, 0
  %t73 = getelementptr inbounds i64, ptr %__ll11, i64 %t70
  %t74 = load i64, ptr %i.slot
  %t75 = sub i64 10, %t74
  store i64 %t75, ptr %t73
  %t76 = load i64, ptr %i.slot
  %t77 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 872)
  %__gc_nil.12 = icmp eq ptr %t77, null
  br i1 %__gc_nil.12, label %gc.skip.12, label %gc.check.12
gc.check.12:
  %__gc_int.12 = ptrtoint ptr %t77 to i64
  %__gc_low.12 = and i64 %__gc_int.12, 1
  %__gc_ma.12 = icmp ne i64 %__gc_low.12, 0
  br i1 %__gc_ma.12, label %gc.skip.12, label %gc.gray.12
gc.gray.12:
  %__gc_hptr.12 = getelementptr i8, ptr %t77, i64 -8
  %__gc_hdr.12 = load i64, ptr %__gc_hptr.12
  %__gc_gb.12 = and i64 %__gc_hdr.12, 4194304
  %__gc_gr.12 = icmp ne i64 %__gc_gb.12, 0
  br i1 %__gc_gr.12, label %gc.slow.12, label %gc.skip.12
gc.slow.12:
  call void @RTHooks__CheckLoadTracedRef(ptr %t77)
  br label %gc.skip.12
gc.skip.12:
  %t78 = load { ptr, i64 }, ptr %t77
  %__ll13 = extractvalue { ptr, i64 } %t78, 0
  %t79 = getelementptr inbounds i8, ptr %__ll13, i64 %t76
  %t80 = load i64, ptr %i.slot
  %t81 = add i64 65, %t80
  %t82 = trunc i64 %t81 to i8
  store i8 %t82, ptr %t79
  %t83 = load i64, ptr %i.slot
  %t84 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 880)
  %__gc_nil.14 = icmp eq ptr %t84, null
  br i1 %__gc_nil.14, label %gc.skip.14, label %gc.check.14
gc.check.14:
  %__gc_int.14 = ptrtoint ptr %t84 to i64
  %__gc_low.14 = and i64 %__gc_int.14, 1
  %__gc_ma.14 = icmp ne i64 %__gc_low.14, 0
  br i1 %__gc_ma.14, label %gc.skip.14, label %gc.gray.14
gc.gray.14:
  %__gc_hptr.14 = getelementptr i8, ptr %t84, i64 -8
  %__gc_hdr.14 = load i64, ptr %__gc_hptr.14
  %__gc_gb.14 = and i64 %__gc_hdr.14, 4194304
  %__gc_gr.14 = icmp ne i64 %__gc_gb.14, 0
  br i1 %__gc_gr.14, label %gc.slow.14, label %gc.skip.14
gc.slow.14:
  call void @RTHooks__CheckLoadTracedRef(ptr %t84)
  br label %gc.skip.14
gc.skip.14:
  %t85 = load { ptr, i64 }, ptr %t84
  %__ll15 = extractvalue { ptr, i64 } %t85, 0
  %t86 = getelementptr inbounds i8, ptr %__ll15, i64 %t83
  %t87 = load i64, ptr %i.slot
  %t88 = add i64 97, %t87
  %t89 = trunc i64 %t88 to i8
  store i8 %t89, ptr %t86
  %t90 = load i64, ptr %i.slot
  %t91 = add i64 %t90, 1
  store i64 %t91, ptr %i.slot
  br label %for.header.1
for.exit.3:
  %t92 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 840)
  %__gc_nil.16 = icmp eq ptr %t92, null
  br i1 %__gc_nil.16, label %gc.skip.16, label %gc.check.16
gc.check.16:
  %__gc_int.16 = ptrtoint ptr %t92 to i64
  %__gc_low.16 = and i64 %__gc_int.16, 1
  %__gc_ma.16 = icmp ne i64 %__gc_low.16, 0
  br i1 %__gc_ma.16, label %gc.skip.16, label %gc.gray.16
gc.gray.16:
  %__gc_hptr.16 = getelementptr i8, ptr %t92, i64 -8
  %__gc_hdr.16 = load i64, ptr %__gc_hptr.16
  %__gc_gb.16 = and i64 %__gc_hdr.16, 4194304
  %__gc_gr.16 = icmp ne i64 %__gc_gb.16, 0
  br i1 %__gc_gr.16, label %gc.slow.16, label %gc.skip.16
gc.slow.16:
  call void @RTHooks__CheckLoadTracedRef(ptr %t92)
  br label %gc.skip.16
gc.skip.16:
  call void @_RTHeap__Print(ptr %t92)
  %t93 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 848)
  %__gc_nil.17 = icmp eq ptr %t93, null
  br i1 %__gc_nil.17, label %gc.skip.17, label %gc.check.17
gc.check.17:
  %__gc_int.17 = ptrtoint ptr %t93 to i64
  %__gc_low.17 = and i64 %__gc_int.17, 1
  %__gc_ma.17 = icmp ne i64 %__gc_low.17, 0
  br i1 %__gc_ma.17, label %gc.skip.17, label %gc.gray.17
gc.gray.17:
  %__gc_hptr.17 = getelementptr i8, ptr %t93, i64 -8
  %__gc_hdr.17 = load i64, ptr %__gc_hptr.17
  %__gc_gb.17 = and i64 %__gc_hdr.17, 4194304
  %__gc_gr.17 = icmp ne i64 %__gc_gb.17, 0
  br i1 %__gc_gr.17, label %gc.slow.17, label %gc.skip.17
gc.slow.17:
  call void @RTHooks__CheckLoadTracedRef(ptr %t93)
  br label %gc.skip.17
gc.skip.17:
  call void @_RTHeap__Print(ptr %t93)
  %t94 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 856)
  %__gc_nil.18 = icmp eq ptr %t94, null
  br i1 %__gc_nil.18, label %gc.skip.18, label %gc.check.18
gc.check.18:
  %__gc_int.18 = ptrtoint ptr %t94 to i64
  %__gc_low.18 = and i64 %__gc_int.18, 1
  %__gc_ma.18 = icmp ne i64 %__gc_low.18, 0
  br i1 %__gc_ma.18, label %gc.skip.18, label %gc.gray.18
gc.gray.18:
  %__gc_hptr.18 = getelementptr i8, ptr %t94, i64 -8
  %__gc_hdr.18 = load i64, ptr %__gc_hptr.18
  %__gc_gb.18 = and i64 %__gc_hdr.18, 4194304
  %__gc_gr.18 = icmp ne i64 %__gc_gb.18, 0
  br i1 %__gc_gr.18, label %gc.slow.18, label %gc.skip.18
gc.slow.18:
  call void @RTHooks__CheckLoadTracedRef(ptr %t94)
  br label %gc.skip.18
gc.skip.18:
  call void @_RTHeap__Print(ptr %t94)
  %t95 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 864)
  %__gc_nil.19 = icmp eq ptr %t95, null
  br i1 %__gc_nil.19, label %gc.skip.19, label %gc.check.19
gc.check.19:
  %__gc_int.19 = ptrtoint ptr %t95 to i64
  %__gc_low.19 = and i64 %__gc_int.19, 1
  %__gc_ma.19 = icmp ne i64 %__gc_low.19, 0
  br i1 %__gc_ma.19, label %gc.skip.19, label %gc.gray.19
gc.gray.19:
  %__gc_hptr.19 = getelementptr i8, ptr %t95, i64 -8
  %__gc_hdr.19 = load i64, ptr %__gc_hptr.19
  %__gc_gb.19 = and i64 %__gc_hdr.19, 4194304
  %__gc_gr.19 = icmp ne i64 %__gc_gb.19, 0
  br i1 %__gc_gr.19, label %gc.slow.19, label %gc.skip.19
gc.slow.19:
  call void @RTHooks__CheckLoadTracedRef(ptr %t95)
  br label %gc.skip.19
gc.skip.19:
  call void @_RTHeap__Print(ptr %t95)
  %t96 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 872)
  %__gc_nil.20 = icmp eq ptr %t96, null
  br i1 %__gc_nil.20, label %gc.skip.20, label %gc.check.20
gc.check.20:
  %__gc_int.20 = ptrtoint ptr %t96 to i64
  %__gc_low.20 = and i64 %__gc_int.20, 1
  %__gc_ma.20 = icmp ne i64 %__gc_low.20, 0
  br i1 %__gc_ma.20, label %gc.skip.20, label %gc.gray.20
gc.gray.20:
  %__gc_hptr.20 = getelementptr i8, ptr %t96, i64 -8
  %__gc_hdr.20 = load i64, ptr %__gc_hptr.20
  %__gc_gb.20 = and i64 %__gc_hdr.20, 4194304
  %__gc_gr.20 = icmp ne i64 %__gc_gb.20, 0
  br i1 %__gc_gr.20, label %gc.slow.20, label %gc.skip.20
gc.slow.20:
  call void @RTHooks__CheckLoadTracedRef(ptr %t96)
  br label %gc.skip.20
gc.skip.20:
  call void @_RTHeap__Print(ptr %t96)
  %t97 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 880)
  %__gc_nil.21 = icmp eq ptr %t97, null
  br i1 %__gc_nil.21, label %gc.skip.21, label %gc.check.21
gc.check.21:
  %__gc_int.21 = ptrtoint ptr %t97 to i64
  %__gc_low.21 = and i64 %__gc_int.21, 1
  %__gc_ma.21 = icmp ne i64 %__gc_low.21, 0
  br i1 %__gc_ma.21, label %gc.skip.21, label %gc.gray.21
gc.gray.21:
  %__gc_hptr.21 = getelementptr i8, ptr %t97, i64 -8
  %__gc_hdr.21 = load i64, ptr %__gc_hptr.21
  %__gc_gb.21 = and i64 %__gc_hdr.21, 4194304
  %__gc_gr.21 = icmp ne i64 %__gc_gb.21, 0
  br i1 %__gc_gr.21, label %gc.slow.21, label %gc.skip.21
gc.slow.21:
  call void @RTHooks__CheckLoadTracedRef(ptr %t97)
  br label %gc.skip.21
gc.skip.21:
  call void @_RTHeap__Print(ptr %t97)
  %t98 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 888)
  %__gc_nil.22 = icmp eq ptr %t98, null
  br i1 %__gc_nil.22, label %gc.skip.22, label %gc.check.22
gc.check.22:
  %__gc_int.22 = ptrtoint ptr %t98 to i64
  %__gc_low.22 = and i64 %__gc_int.22, 1
  %__gc_ma.22 = icmp ne i64 %__gc_low.22, 0
  br i1 %__gc_ma.22, label %gc.skip.22, label %gc.gray.22
gc.gray.22:
  %__gc_hptr.22 = getelementptr i8, ptr %t98, i64 -8
  %__gc_hdr.22 = load i64, ptr %__gc_hptr.22
  %__gc_gb.22 = and i64 %__gc_hdr.22, 4194304
  %__gc_gr.22 = icmp ne i64 %__gc_gb.22, 0
  br i1 %__gc_gr.22, label %gc.slow.22, label %gc.skip.22
gc.slow.22:
  call void @RTHooks__CheckLoadTracedRef(ptr %t98)
  br label %gc.skip.22
gc.skip.22:
  call void @_RTHeap__Print(ptr %t98)
  %t99 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 896)
  %__gc_nil.23 = icmp eq ptr %t99, null
  br i1 %__gc_nil.23, label %gc.skip.23, label %gc.check.23
gc.check.23:
  %__gc_int.23 = ptrtoint ptr %t99 to i64
  %__gc_low.23 = and i64 %__gc_int.23, 1
  %__gc_ma.23 = icmp ne i64 %__gc_low.23, 0
  br i1 %__gc_ma.23, label %gc.skip.23, label %gc.gray.23
gc.gray.23:
  %__gc_hptr.23 = getelementptr i8, ptr %t99, i64 -8
  %__gc_hdr.23 = load i64, ptr %__gc_hptr.23
  %__gc_gb.23 = and i64 %__gc_hdr.23, 4194304
  %__gc_gr.23 = icmp ne i64 %__gc_gb.23, 0
  br i1 %__gc_gr.23, label %gc.slow.23, label %gc.skip.23
gc.slow.23:
  call void @RTHooks__CheckLoadTracedRef(ptr %t99)
  br label %gc.skip.23
gc.skip.23:
  call void @_RTHeap__Print(ptr %t99)
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
@textlit_0 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"Hello Eric\00" }
@textlit_1 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"hello\00" }
@textlit_2 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c"eric\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_-1467451074 = internal global %TC_t {
  i64 0,
  i64 -1467451074,
  i64 u0x1e84df1ab60ca624,
  i8 1,
  i8 1,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 88,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-1758373035
}
@tc_ref_-1758373035 = internal global %TC_t {
  i64 0,
  i64 -1758373035,
  i64 u0x00fe695c97cf3209,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 11,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-318626117
}
@tc_ref_-318626117 = internal global %TC_t {
  i64 0,
  i64 -318626117,
  i64 u0x0f3883b2e23aa509,
  i8 1,
  i8 1,
  i8 0,
  i8 1,
  [4 x i8] zeroinitializer,
  i64 11,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_839756992
}
@tc_arr_839756992_gc_map = internal constant [4 x i8] [i8 24, i8 1, i8 4, i8 0]
@tc_arr_839756992 = internal global %ATC_t {
  i64 0,
  i64 839756992,
  i64 u0x039d2ff731908337,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr @tc_arr_839756992_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_-1649389748,
  i64 1,
  i64 8
}
@tc_arr_-1649389748 = internal global %ATC_t {
  i64 0,
  i64 -1649389748,
  i64 u0x03ec74269e5c3b6a,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_-797892043,
  i64 1,
  i64 1
}
@tc_arr_-797892043 = internal global %ATC_t {
  i64 0,
  i64 -797892043,
  i64 u0x16cef62ec6bfd41b,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_43642924,
  i64 1,
  i64 1
}
@tc_arr_43642924_gc_map = internal constant [4 x i8] [i8 24, i8 1, i8 4, i8 0]
@tc_arr_43642924 = internal global %ATC_t {
  i64 0,
  i64 43642924,
  i64 u0x01179835038e6819,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr @tc_arr_43642924_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  i64 1,
  i64 8
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_arr_43642924 = internal global %TypeLink_t {
  ptr null,
  i64 43642924
}
@tl_arr_-797892043 = internal global %TypeLink_t {
  ptr @tl_arr_43642924,
  i64 -797892043
}
@tl_arr_-1649389748 = internal global %TypeLink_t {
  ptr @tl_arr_-797892043,
  i64 -1649389748
}
@tl_arr_839756992 = internal global %TypeLink_t {
  ptr @tl_arr_-1649389748,
  i64 839756992
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @print_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @print_I3, ptr null }
@Main_M3_gc_map = internal constant [14 x i8] c"\2a\68\2b\e0\02\04\04\04\04\04\04\04\04\00"

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [800 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_-1467451074,  ; type_cells (+8)
  ptr @tl_arr_839756992,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr @Main_M3_gc_map,  ; var_map (+56)
  ptr @Main_M3_gc_map,  ; gc_map (+64)
  ptr @Main_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Main_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [800 x i8] zeroinitializer  ; user globals (800 bytes)
}
@Main__t = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 840)
@Main__u = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 848)
@Main__v = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 856)
@Main__to = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 864)
@Main__uo = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 872)
@Main__vo = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 880)
@Main__te = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 888)
@Main__ta = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 896)

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
