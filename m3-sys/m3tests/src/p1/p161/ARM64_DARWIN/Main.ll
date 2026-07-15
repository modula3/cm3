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
declare void @RTHooks__ReportFault(ptr, i64)
declare void @Test__check(i1)
declare void @Test__done()
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__Sieve(ptr %a.s) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %t14 = alloca i64
  %i.slot.1 = alloca i64
  call void @llvm.dbg.declare(metadata ptr %i.slot.1, metadata !20, metadata !DIExpression()), !dbg !25
  %t.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %t.slot, metadata !21, metadata !DIExpression()), !dbg !26
  %j.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %j.slot, metadata !22, metadata !DIExpression()), !dbg !26
  %i.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %i.slot, metadata !23, metadata !DIExpression()), !dbg !26
  store ptr null, ptr %t.slot
  %t1 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-790884030), !dbg !26
  %t2 = bitcast ptr %t1 to ptr
  store ptr %t2, ptr %t.slot
  store i64 1, ptr %i.slot
  store i64 1000000, ptr %j.slot
  %t3 = load i64, ptr %i.slot
  %t4 = load i64, ptr %j.slot
  %t5 = zext i64 %t3 to i1000000
  %t6 = zext i64 %t4 to i1000000
  %t7 = sub i1000000 %t5, 1
  %t8 = sub i1000000 %t6, 1
  %t9 = sub i1000000 999999, %t8
  %t10 = lshr i1000000 -1, %t9
  %t11 = shl i1000000 -1, %t7
  %t12 = and i1000000 %t10, %t11
  %t13 = or i1000000 0, %t12
  store i1000000 %t13, ptr %a.s
  store i64 1, ptr %i.slot.1
  store i64 1000000, ptr %t14
  br label %for.header.1
for.header.1:
  %t15 = load i64, ptr %i.slot.1
  %t16 = load i64, ptr %t14
  %t17 = icmp sle i64 %t15, %t16
  br i1 %t17, label %for.body.2, label %for.exit.3
for.body.2:
  %t18 = load i64, ptr %i.slot.1
  store i64 %t18, ptr %j.slot
  br label %while.header.4
for.exit.3:
  ret void
while.header.4:
  %t19 = load i64, ptr %j.slot
  %t20 = icmp sle i64 %t19, 1000000
  br i1 %t20, label %while.body.5, label %while.exit.6
while.body.5:
  %t21 = load ptr, ptr %t.slot
  store i1000000 0, ptr %t21
  %t22 = load ptr, ptr %t.slot
  %t23 = load ptr, ptr %t.slot
  %t24 = load i1000000, ptr %t23
  %t25 = load i64, ptr %j.slot
  %t26 = zext i64 %t25 to i1000000
  %t27 = icmp slt i1000000 %t26, 1
  br i1 %t27, label %check.fault.7, label %check.cont.8
while.exit.6:
  %t40 = load i64, ptr %i.slot.1
  %t41 = add i64 %t40, 1
  store i64 %t41, ptr %i.slot.1
  br label %for.header.1
check.fault.7:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 1057), !dbg !32
  unreachable
check.cont.8:
  %t28 = icmp sgt i1000000 %t26, 1000000
  br i1 %t28, label %check.fault.9, label %check.cont.10
check.fault.9:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 1057), !dbg !32
  unreachable
check.cont.10:
  %t29 = sub i1000000 %t26, 1
  %t30 = shl i1000000 1, %t29
  %t31 = or i1000000 0, %t30
  %t32 = or i1000000 %t24, %t31
  store i1000000 %t32, ptr %t22
  %t33 = load i1000000, ptr %a.s
  %t34 = load ptr, ptr %t.slot
  %t35 = load i1000000, ptr %t34
  %t36 = xor i1000000 %t33, %t35
  store i1000000 %t36, ptr %a.s
  %t37 = load i64, ptr %j.slot
  %t38 = load i64, ptr %i.slot.1
  %t39 = add i64 %t37, %t38
  store i64 %t39, ptr %j.slot
  br label %while.header.4
}

define void @Main__Squares(ptr %a.t) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %i.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %i.slot, metadata !24, metadata !DIExpression()), !dbg !34
  store i64 1, ptr %i.slot
  store i64 1, ptr %i.slot
  store i1000000 0, ptr %a.t
  store i64 1, ptr %i.slot
  br label %while.header.1
while.header.1:
  %t1 = load i64, ptr %i.slot
  %t2 = load i64, ptr %i.slot
  %t3 = mul i64 %t1, %t2
  %t4 = icmp sle i64 %t3, 1000000
  br i1 %t4, label %while.body.2, label %while.exit.3
while.body.2:
  %t5 = load i1000000, ptr %a.t
  %t6 = load i64, ptr %i.slot
  %t7 = load i64, ptr %i.slot
  %t8 = mul i64 %t6, %t7
  %t9 = zext i64 %t8 to i1000000
  %t10 = icmp slt i1000000 %t9, 1
  br i1 %t10, label %check.fault.4, label %check.cont.5
while.exit.3:
  ret void
check.fault.4:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 1505), !dbg !38
  unreachable
check.cont.5:
  %t11 = icmp sgt i1000000 %t9, 1000000
  br i1 %t11, label %check.fault.6, label %check.cont.7
check.fault.6:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 1505), !dbg !38
  unreachable
check.cont.7:
  %t12 = sub i1000000 %t9, 1
  %t13 = shl i1000000 1, %t12
  %t14 = or i1000000 0, %t13
  %t15 = or i1000000 %t5, %t14
  store i1000000 %t15, ptr %a.t
  %t16 = load i32, ptr %i.slot
  %t17 = add i32 %t16, 1
  store i32 %t17, ptr %i.slot
  br label %while.header.1
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-790884030)
  %t2 = bitcast ptr %t1 to ptr
  store ptr %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %t3 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %__gc_nil.1 = icmp eq ptr %t3, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t3 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t3, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t3)
  br label %gc.skip.1
gc.skip.1:
  call void @Main__Sieve(ptr %t3)
  %t4 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %__gc_nil.2 = icmp eq ptr %t4, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t4 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t4, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t4)
  br label %gc.skip.2
gc.skip.2:
  %t5 = load i1000000, ptr %t4
  %t6 = sext i64 10 to i1000000
  %t7 = sub i1000000 %t6, 1
  %t8 = icmp ult i1000000 %t7, 1000000
  %t9 = select i1 %t8, i1000000 %t7, i1000000 0
  %t10 = lshr i1000000 %t5, %t9
  %t11 = and i1000000 %t10, 1
  %t12 = icmp ne i1000000 %t11, 0
  call void @Test__check(i1 %t12)
  %t13 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %__gc_nil.3 = icmp eq ptr %t13, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t13 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t13, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t13)
  br label %gc.skip.3
gc.skip.3:
  %t14 = load i1000000, ptr %t13
  %t15 = sext i64 100 to i1000000
  %t16 = sub i1000000 %t15, 1
  %t17 = icmp ult i1000000 %t16, 1000000
  %t18 = select i1 %t17, i1000000 %t16, i1000000 0
  %t19 = lshr i1000000 %t14, %t18
  %t20 = and i1000000 %t19, 1
  %t21 = icmp ne i1000000 %t20, 0
  %t22 = icmp eq i1 %t21, 0
  call void @Test__check(i1 %t22)
  %t23 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-790884030)
  %t24 = bitcast ptr %t23 to ptr
  store ptr %t24, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t25 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %__gc_nil.4 = icmp eq ptr %t25, null
  br i1 %__gc_nil.4, label %gc.skip.4, label %gc.check.4
gc.check.4:
  %__gc_int.4 = ptrtoint ptr %t25 to i64
  %__gc_low.4 = and i64 %__gc_int.4, 1
  %__gc_ma.4 = icmp ne i64 %__gc_low.4, 0
  br i1 %__gc_ma.4, label %gc.skip.4, label %gc.gray.4
gc.gray.4:
  %__gc_hptr.4 = getelementptr i8, ptr %t25, i64 -8
  %__gc_hdr.4 = load i64, ptr %__gc_hptr.4
  %__gc_gb.4 = and i64 %__gc_hdr.4, 4194304
  %__gc_gr.4 = icmp ne i64 %__gc_gb.4, 0
  br i1 %__gc_gr.4, label %gc.slow.4, label %gc.skip.4
gc.slow.4:
  call void @RTHooks__CheckLoadTracedRef(ptr %t25)
  br label %gc.skip.4
gc.skip.4:
  call void @Main__Squares(ptr %t25)
  %t26 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %__gc_nil.5 = icmp eq ptr %t26, null
  br i1 %__gc_nil.5, label %gc.skip.5, label %gc.check.5
gc.check.5:
  %__gc_int.5 = ptrtoint ptr %t26 to i64
  %__gc_low.5 = and i64 %__gc_int.5, 1
  %__gc_ma.5 = icmp ne i64 %__gc_low.5, 0
  br i1 %__gc_ma.5, label %gc.skip.5, label %gc.gray.5
gc.gray.5:
  %__gc_hptr.5 = getelementptr i8, ptr %t26, i64 -8
  %__gc_hdr.5 = load i64, ptr %__gc_hptr.5
  %__gc_gb.5 = and i64 %__gc_hdr.5, 4194304
  %__gc_gr.5 = icmp ne i64 %__gc_gb.5, 0
  br i1 %__gc_gr.5, label %gc.slow.5, label %gc.skip.5
gc.slow.5:
  call void @RTHooks__CheckLoadTracedRef(ptr %t26)
  br label %gc.skip.5
gc.skip.5:
  %t27 = load i1000000, ptr %t26
  %t28 = sext i64 10 to i1000000
  %t29 = sub i1000000 %t28, 1
  %t30 = icmp ult i1000000 %t29, 1000000
  %t31 = select i1 %t30, i1000000 %t29, i1000000 0
  %t32 = lshr i1000000 %t27, %t31
  %t33 = and i1000000 %t32, 1
  %t34 = icmp ne i1000000 %t33, 0
  %t35 = icmp eq i1 %t34, 0
  call void @Test__check(i1 %t35)
  %t36 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %__gc_nil.6 = icmp eq ptr %t36, null
  br i1 %__gc_nil.6, label %gc.skip.6, label %gc.check.6
gc.check.6:
  %__gc_int.6 = ptrtoint ptr %t36 to i64
  %__gc_low.6 = and i64 %__gc_int.6, 1
  %__gc_ma.6 = icmp ne i64 %__gc_low.6, 0
  br i1 %__gc_ma.6, label %gc.skip.6, label %gc.gray.6
gc.gray.6:
  %__gc_hptr.6 = getelementptr i8, ptr %t36, i64 -8
  %__gc_hdr.6 = load i64, ptr %__gc_hptr.6
  %__gc_gb.6 = and i64 %__gc_hdr.6, 4194304
  %__gc_gr.6 = icmp ne i64 %__gc_gb.6, 0
  br i1 %__gc_gr.6, label %gc.slow.6, label %gc.skip.6
gc.slow.6:
  call void @RTHooks__CheckLoadTracedRef(ptr %t36)
  br label %gc.skip.6
gc.skip.6:
  %t37 = load i1000000, ptr %t36
  %t38 = sext i64 100 to i1000000
  %t39 = sub i1000000 %t38, 1
  %t40 = icmp ult i1000000 %t39, 1000000
  %t41 = select i1 %t40, i1000000 %t39, i1000000 0
  %t42 = lshr i1000000 %t37, %t41
  %t43 = and i1000000 %t42, 1
  %t44 = icmp ne i1000000 %t43, 0
  call void @Test__check(i1 %t44)
  %t45 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_-790884030)
  %t46 = bitcast ptr %t45 to ptr
  store ptr %t46, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  store i64 1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  store i64 1000000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)
  %t47 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  %__gc_nil.7 = icmp eq ptr %t47, null
  br i1 %__gc_nil.7, label %gc.skip.7, label %gc.check.7
gc.check.7:
  %__gc_int.7 = ptrtoint ptr %t47 to i64
  %__gc_low.7 = and i64 %__gc_int.7, 1
  %__gc_ma.7 = icmp ne i64 %__gc_low.7, 0
  br i1 %__gc_ma.7, label %gc.skip.7, label %gc.gray.7
gc.gray.7:
  %__gc_hptr.7 = getelementptr i8, ptr %t47, i64 -8
  %__gc_hdr.7 = load i64, ptr %__gc_hptr.7
  %__gc_gb.7 = and i64 %__gc_hdr.7, 4194304
  %__gc_gr.7 = icmp ne i64 %__gc_gb.7, 0
  br i1 %__gc_gr.7, label %gc.slow.7, label %gc.skip.7
gc.slow.7:
  call void @RTHooks__CheckLoadTracedRef(ptr %t47)
  br label %gc.skip.7
gc.skip.7:
  %t48 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  %t49 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)
  %t50 = zext i64 %t48 to i1000000
  %t51 = zext i64 %t49 to i1000000
  %t52 = sub i1000000 %t50, 1
  %t53 = sub i1000000 %t51, 1
  %t54 = sub i1000000 999999, %t53
  %t55 = lshr i1000000 -1, %t54
  %t56 = shl i1000000 -1, %t52
  %t57 = and i1000000 %t55, %t56
  %t58 = or i1000000 0, %t57
  store i1000000 %t58, ptr %t47
  %t59 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
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
  %t60 = load i1000000, ptr %t59
  %t61 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %__gc_nil.9 = icmp eq ptr %t61, null
  br i1 %__gc_nil.9, label %gc.skip.9, label %gc.check.9
gc.check.9:
  %__gc_int.9 = ptrtoint ptr %t61 to i64
  %__gc_low.9 = and i64 %__gc_int.9, 1
  %__gc_ma.9 = icmp ne i64 %__gc_low.9, 0
  br i1 %__gc_ma.9, label %gc.skip.9, label %gc.gray.9
gc.gray.9:
  %__gc_hptr.9 = getelementptr i8, ptr %t61, i64 -8
  %__gc_hdr.9 = load i64, ptr %__gc_hptr.9
  %__gc_gb.9 = and i64 %__gc_hdr.9, 4194304
  %__gc_gr.9 = icmp ne i64 %__gc_gb.9, 0
  br i1 %__gc_gr.9, label %gc.slow.9, label %gc.skip.9
gc.slow.9:
  call void @RTHooks__CheckLoadTracedRef(ptr %t61)
  br label %gc.skip.9
gc.skip.9:
  %t62 = load i1000000, ptr %t61
  %t63 = and i1000000 %t60, %t62
  %t64 = icmp eq i1000000 %t63, 0
  call void @Test__check(i1 %t64)
  %t65 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %__gc_nil.10 = icmp eq ptr %t65, null
  br i1 %__gc_nil.10, label %gc.skip.10, label %gc.check.10
gc.check.10:
  %__gc_int.10 = ptrtoint ptr %t65 to i64
  %__gc_low.10 = and i64 %__gc_int.10, 1
  %__gc_ma.10 = icmp ne i64 %__gc_low.10, 0
  br i1 %__gc_ma.10, label %gc.skip.10, label %gc.gray.10
gc.gray.10:
  %__gc_hptr.10 = getelementptr i8, ptr %t65, i64 -8
  %__gc_hdr.10 = load i64, ptr %__gc_hptr.10
  %__gc_gb.10 = and i64 %__gc_hdr.10, 4194304
  %__gc_gr.10 = icmp ne i64 %__gc_gb.10, 0
  br i1 %__gc_gr.10, label %gc.slow.10, label %gc.skip.10
gc.slow.10:
  call void @RTHooks__CheckLoadTracedRef(ptr %t65)
  br label %gc.skip.10
gc.skip.10:
  %t66 = load i1000000, ptr %t65
  %t67 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %__gc_nil.11 = icmp eq ptr %t67, null
  br i1 %__gc_nil.11, label %gc.skip.11, label %gc.check.11
gc.check.11:
  %__gc_int.11 = ptrtoint ptr %t67 to i64
  %__gc_low.11 = and i64 %__gc_int.11, 1
  %__gc_ma.11 = icmp ne i64 %__gc_low.11, 0
  br i1 %__gc_ma.11, label %gc.skip.11, label %gc.gray.11
gc.gray.11:
  %__gc_hptr.11 = getelementptr i8, ptr %t67, i64 -8
  %__gc_hdr.11 = load i64, ptr %__gc_hptr.11
  %__gc_gb.11 = and i64 %__gc_hdr.11, 4194304
  %__gc_gr.11 = icmp ne i64 %__gc_gb.11, 0
  br i1 %__gc_gr.11, label %gc.slow.11, label %gc.skip.11
gc.slow.11:
  call void @RTHooks__CheckLoadTracedRef(ptr %t67)
  br label %gc.skip.11
gc.skip.11:
  %t68 = load i1000000, ptr %t67
  %t69 = or i1000000 %t66, %t68
  %t70 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  %__gc_nil.12 = icmp eq ptr %t70, null
  br i1 %__gc_nil.12, label %gc.skip.12, label %gc.check.12
gc.check.12:
  %__gc_int.12 = ptrtoint ptr %t70 to i64
  %__gc_low.12 = and i64 %__gc_int.12, 1
  %__gc_ma.12 = icmp ne i64 %__gc_low.12, 0
  br i1 %__gc_ma.12, label %gc.skip.12, label %gc.gray.12
gc.gray.12:
  %__gc_hptr.12 = getelementptr i8, ptr %t70, i64 -8
  %__gc_hdr.12 = load i64, ptr %__gc_hptr.12
  %__gc_gb.12 = and i64 %__gc_hdr.12, 4194304
  %__gc_gr.12 = icmp ne i64 %__gc_gb.12, 0
  br i1 %__gc_gr.12, label %gc.slow.12, label %gc.skip.12
gc.slow.12:
  call void @RTHooks__CheckLoadTracedRef(ptr %t70)
  br label %gc.skip.12
gc.skip.12:
  %t71 = load i1000000, ptr %t70
  %t72 = icmp eq i1000000 %t69, %t71
  call void @Test__check(i1 %t72)
  call void @Test__done()
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_-790884030 = internal global %TC_t {
  i64 0,
  i64 -790884030,
  i64 u0x0cfcdc54dc20cd16,
  i8 1,
  i8 1,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 125000,
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
declare ptr @Test_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Test_I3, ptr null }
@Main_M3_gc_map = internal constant [9 x i8] c"\2a\68\2a\60\04\04\04\04\00"

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [144 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_-790884030,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
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
  [144 x i8] zeroinitializer  ; user globals (144 bytes)
}
@Main__x = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
@Main__y = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
@Main__z = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
@Main__zz = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
@Main__a = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
@Main__b = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)

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
!16 = distinct !DISubprogram(name: "Main__Sieve", linkageName: "Main__Sieve", scope: !4, file: !3, line: 25, type: !6, scopeLine: 25, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__Squares", linkageName: "Main__Squares", scope: !4, file: !3, line: 40, type: !6, scopeLine: 40, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!20 = !DILocalVariable(name: "i.slot", scope: !16, file: !3, line: 25, type: !7)
!21 = !DILocalVariable(name: "t", scope: !16, file: !3, line: 25, type: !15)
!22 = !DILocalVariable(name: "j", scope: !16, file: !3, line: 25, type: !7)
!23 = !DILocalVariable(name: "i", scope: !16, file: !3, line: 25, type: !7)
!24 = !DILocalVariable(name: "i", scope: !18, file: !3, line: 40, type: !7)
!25 = !DILocation(line: 29, column: 0, scope: !16)
!26 = !DILocation(line: 25, column: 0, scope: !16)
!27 = !DILocation(line: 28, column: 0, scope: !16)
!28 = !DILocation(line: 30, column: 0, scope: !16)
!29 = !DILocation(line: 31, column: 0, scope: !16)
!30 = !DILocation(line: 35, column: 0, scope: !16)
!31 = !DILocation(line: 32, column: 0, scope: !16)
!32 = !DILocation(line: 33, column: 0, scope: !16)
!33 = !DILocation(line: 34, column: 0, scope: !16)
!34 = !DILocation(line: 40, column: 0, scope: !18)
!35 = !DILocation(line: 44, column: 0, scope: !18)
!36 = !DILocation(line: 45, column: 0, scope: !18)
!37 = !DILocation(line: 46, column: 0, scope: !18)
!38 = !DILocation(line: 47, column: 0, scope: !18)
!39 = !DILocation(line: 48, column: 0, scope: !18)
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
