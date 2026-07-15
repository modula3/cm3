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


declare void @Wr__PutText(ptr, ptr)
declare void @Wr__Flush(ptr)
declare ptr @Fmt__Int(i64, i8)
declare ptr @RTHooks__Concat(ptr, ptr)
declare ptr @__cxa_get_exception_ptr(ptr)
declare void @RTHooks__ResumeRaise(ptr)
declare void @RTHooks__ReportFault(ptr, i64)
declare void @Thread__Pause(double)
declare void @RTHooks__Raise(ptr, ptr, ptr, i64)
declare void @Thread__Acquire(ptr)
declare void @Thread__Release(ptr)
declare void @Thread__Wait(ptr, ptr)
declare void @Thread__Signal(ptr)
declare ptr @__cxa_begin_catch(ptr)
declare void @__cxa_end_catch()
declare ptr @RTHooks__AllocateTracedObj(ptr)
declare ptr @Thread__Fork(ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__W(ptr %a.Msg) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %Msg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %Msg.slot, metadata !52, metadata !DIExpression()), !dbg !97
  store ptr %a.Msg, ptr %Msg.slot
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t2 = getelementptr i8, ptr %t1, i64 112
  %t3 = load ptr, ptr %t2
  %t4 = load ptr, ptr %Msg.slot
  call void @Wr__PutText(ptr %t3, ptr %t4), !dbg !98
  %t5 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t6 = getelementptr i8, ptr %t5, i64 112
  %t7 = load ptr, ptr %t6
  call void @Wr__PutText(ptr %t7, ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !99
  %t8 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t9 = getelementptr i8, ptr %t8, i64 112
  %t10 = load ptr, ptr %t9
  call void @Wr__Flush(ptr %t10), !dbg !100
  ret void
}

define ptr @Main__ThImage(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !53, metadata !DIExpression()), !dbg !101
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !54, metadata !DIExpression()), !dbg !101
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store ptr null, ptr %_result.slot
  %t2 = load i64, ptr %ThN.slot
  %t3 = trunc i64 10 to i8
  %t4 = call ptr @Fmt__Int(i64 %t2, i8 %t3), !dbg !102
  %t5 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8), ptr %t4), !dbg !102
  ret ptr %t5
}

define ptr @Main__StateImage(i8 %a.St) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !55, metadata !DIExpression()), !dbg !103
  %LResult.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %LResult.slot, metadata !56, metadata !DIExpression()), !dbg !103
  %St.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %St.slot, metadata !57, metadata !DIExpression()), !dbg !103
  %t1 = zext i8 %a.St to i64
  store i64 %t1, ptr %St.slot
  store ptr null, ptr %LResult.slot
  store ptr null, ptr %_result.slot
  %t2 = load i64, ptr %St.slot
  %t3 = icmp slt i64 %t2, 0
  br i1 %t3, label %case.else.8, label %case.inrange.10
case.body.1:
  store ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.2:
  store ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.3:
  store ptr getelementptr inbounds (i8, ptr @textlit_4, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.4:
  store ptr getelementptr inbounds (i8, ptr @textlit_5, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.5:
  store ptr getelementptr inbounds (i8, ptr @textlit_6, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.6:
  store ptr getelementptr inbounds (i8, ptr @textlit_7, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.body.7:
  store ptr getelementptr inbounds (i8, ptr @textlit_8, i64 8), ptr %LResult.slot
  br label %case.merge.9
case.else.8:
  br label %case.merge.9
case.merge.9:
  %t17 = load ptr, ptr %LResult.slot
  ret ptr %t17
case.inrange.10:
  %t4 = icmp sle i64 %t2, 0
  br i1 %t4, label %case.body.1, label %case.next.11
case.next.11:
  %t5 = icmp slt i64 %t2, 1
  br i1 %t5, label %case.else.8, label %case.inrange.12
case.inrange.12:
  %t6 = icmp sle i64 %t2, 1
  br i1 %t6, label %case.body.2, label %case.next.13
case.next.13:
  %t7 = icmp slt i64 %t2, 2
  br i1 %t7, label %case.else.8, label %case.inrange.14
case.inrange.14:
  %t8 = icmp sle i64 %t2, 2
  br i1 %t8, label %case.body.3, label %case.next.15
case.next.15:
  %t9 = icmp slt i64 %t2, 3
  br i1 %t9, label %case.else.8, label %case.inrange.16
case.inrange.16:
  %t10 = icmp sle i64 %t2, 3
  br i1 %t10, label %case.body.4, label %case.next.17
case.next.17:
  %t11 = icmp slt i64 %t2, 4
  br i1 %t11, label %case.else.8, label %case.inrange.18
case.inrange.18:
  %t12 = icmp sle i64 %t2, 4
  br i1 %t12, label %case.body.5, label %case.next.19
case.next.19:
  %t13 = icmp slt i64 %t2, 5
  br i1 %t13, label %case.else.8, label %case.inrange.20
case.inrange.20:
  %t14 = icmp sle i64 %t2, 5
  br i1 %t14, label %case.body.6, label %case.next.21
case.next.21:
  %t15 = icmp slt i64 %t2, 6
  br i1 %t15, label %case.else.8, label %case.inrange.22
case.inrange.22:
  %t16 = icmp sle i64 %t2, 6
  br i1 %t16, label %case.body.7, label %case.else.8
}

define void @Main__Action(i8 %a.ThN, ptr %a.Apply) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %t30 = alloca i32
  %t29 = alloca ptr
  %t7 = alloca i32
  %t6 = alloca ptr
  %Apply.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %Apply.slot, metadata !58, metadata !DIExpression()), !dbg !115
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !59, metadata !DIExpression()), !dbg !115
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store ptr %a.Apply, ptr %Apply.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !114
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !114
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i64 0, i64 %t8
  %t10 = load ptr, ptr %t9
  %t11 = icmp eq ptr %t10, null
  %t12 = icmp eq i1 %t11, 0
  br i1 %t12, label %check.fault.6, label %check.cont.7
lock.lpad.1:
  %t14 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t15 = extractvalue { ptr, i32 } %t14, 0
  %t16 = call ptr @__cxa_get_exception_ptr(ptr %t15), !dbg !118
  %t17 = load ptr, ptr %t16
  store ptr %t17, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t18 = bitcast ptr %t2 to ptr
  %t19 = load ptr, ptr %t18
  %t20 = getelementptr i8, ptr %t19, i64 8
  %t21 = load ptr, ptr %t20
  call void %t21(ptr %t2), !dbg !118
  %t22 = load i32, ptr %t7
  %t23 = icmp eq i32 %t22, 1
  br i1 %t23, label %lock.resume.3, label %lock.fin.notexc.9
lock.resume.3:
  %t24 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t24), !dbg !118
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  br label %loop.header.10
check.fault.6:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 2656)
          to label %invoke.cont.8 unwind label %lock.lpad.1, !dbg !117
check.cont.7:
  %t13 = load ptr, ptr %Apply.slot
  store ptr %t13, ptr %t9
  br label %lock.fin.2
invoke.cont.8:
  unreachable
lock.fin.notexc.9:
  br label %lock.done.5
loop.header.10:
  call void @Thread__Pause(double 0x3fb999999999999a), !dbg !120
  %t25 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
  %__gc_nil.2 = icmp eq ptr %t25, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t25 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t25, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t25), !dbg !113
  br label %gc.skip.2
gc.skip.2:
  %t26 = bitcast ptr %t25 to ptr
  %t27 = load ptr, ptr %t26
  %t28 = load ptr, ptr %t27
  call void %t28(ptr %t25), !dbg !113
  store i32 0, ptr %t30
  %t31 = load i64, ptr %ThN.slot
  %t32 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i64 0, i64 %t31
  %t33 = load ptr, ptr %t32
  %t34 = icmp eq ptr %t33, null
  br i1 %t34, label %if.then.17, label %if.merge.18
loop.exit.11:
  ret void
lock.lpad.12:
  %t35 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t36 = extractvalue { ptr, i32 } %t35, 0
  %t37 = call ptr @__cxa_get_exception_ptr(ptr %t36), !dbg !121
  %t38 = load ptr, ptr %t37
  store ptr %t38, ptr %t29
  store i32 1, ptr %t30
  br label %lock.fin.13
lock.fin.13:
  %t39 = bitcast ptr %t25 to ptr
  %t40 = load ptr, ptr %t39
  %t41 = getelementptr i8, ptr %t40, i64 8
  %t42 = load ptr, ptr %t41
  call void %t42(ptr %t25), !dbg !121
  %t43 = load i32, ptr %t30
  %t44 = icmp eq i32 %t43, 1
  br i1 %t44, label %lock.resume.14, label %lock.fin.notexc.20
lock.resume.14:
  %t46 = load ptr, ptr %t29
  call void @RTHooks__ResumeRaise(ptr %t46), !dbg !121
  unreachable
lock.ret.15:
  unreachable
lock.done.16:
  br label %loop.header.10
if.then.17:
  store i32 2, ptr %t30
  br label %lock.fin.13
if.merge.18:
  br label %lock.fin.13
exit.dead.19:
  br label %if.merge.18
lock.fin.notexc.20:
  %t45 = icmp eq i32 %t43, 2
  br i1 %t45, label %lock.doexit.21, label %lock.done.16
lock.doexit.21:
  br label %loop.exit.11
}

define void @Main__ActionWait(i8 %a.ThN, ptr %a.Apply) personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %t16 = alloca i32
  %t15 = alloca ptr
  %t10 = alloca i32
  %t9 = alloca ptr
  %Apply.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %Apply.slot, metadata !60, metadata !DIExpression()), !dbg !124
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !61, metadata !DIExpression()), !dbg !124
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store ptr %a.Apply, ptr %Apply.slot
  %t2 = load i64, ptr %ThN.slot
  %t3 = trunc i64 %t2 to i8
  %t4 = load ptr, ptr %Apply.slot
  call void @Main__Action(i8 %t3, ptr %t4), !dbg !125
  br label %loop.header.1
loop.header.1:
  %t5 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %__gc_nil.1 = icmp eq ptr %t5, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t5 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t5, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t5), !dbg !123
  br label %gc.skip.1
gc.skip.1:
  %t6 = bitcast ptr %t5 to ptr
  %t7 = load ptr, ptr %t6
  %t8 = load ptr, ptr %t7
  call void %t8(ptr %t5), !dbg !123
  store i32 0, ptr %t10
  %t11 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.2 = icmp eq ptr %t11, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t11 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t11, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t11), !dbg !122
  br label %gc.skip.2
gc.skip.2:
  %t12 = bitcast ptr %t11 to ptr
  %t13 = load ptr, ptr %t12
  %t14 = load ptr, ptr %t13
  invoke void %t14(ptr %t11)
          to label %dispatch.cont.8 unwind label %lock.lpad.3, !dbg !122
loop.exit.2:
  ret void
lock.lpad.3:
  %t47 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t48 = extractvalue { ptr, i32 } %t47, 0
  %t49 = call ptr @__cxa_get_exception_ptr(ptr %t48), !dbg !127
  %t50 = load ptr, ptr %t49
  store ptr %t50, ptr %t9
  store i32 1, ptr %t10
  br label %lock.fin.4
lock.fin.4:
  %t51 = bitcast ptr %t5 to ptr
  %t52 = load ptr, ptr %t51
  %t53 = getelementptr i8, ptr %t52, i64 8
  %t54 = load ptr, ptr %t53
  call void %t54(ptr %t5), !dbg !127
  %t55 = load i32, ptr %t10
  %t56 = icmp eq i32 %t55, 1
  br i1 %t56, label %lock.resume.5, label %lock.fin.notexc.32
lock.resume.5:
  %t58 = load ptr, ptr %t9
  call void @RTHooks__ResumeRaise(ptr %t58), !dbg !127
  unreachable
lock.ret.6:
  unreachable
lock.done.7:
  br label %loop.header.1
dispatch.cont.8:
  store i32 0, ptr %t16
  %t17 = load i64, ptr %ThN.slot
  %t18 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t17
  %t19 = load i8, ptr %t18
  %t20 = icmp ne i8 %t19, 4
  br i1 %t20, label %if.then.14, label %if.next.15
lock.lpad.9:
  %t35 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t36 = extractvalue { ptr, i32 } %t35, 0
  %t37 = call ptr @__cxa_get_exception_ptr(ptr %t36), !dbg !127
  %t38 = load ptr, ptr %t37
  store ptr %t38, ptr %t15
  store i32 1, ptr %t16
  br label %lock.fin.10
lock.fin.10:
  %t39 = bitcast ptr %t11 to ptr
  %t40 = load ptr, ptr %t39
  %t41 = getelementptr i8, ptr %t40, i64 8
  %t42 = load ptr, ptr %t41
  invoke void %t42(ptr %t11)
          to label %dispatch.cont.28 unwind label %lock.lpad.3, !dbg !127
lock.resume.11:
  %t46 = load ptr, ptr %t15
  invoke void @RTHooks__ResumeRaise(ptr %t46)
          to label %lock.resume.cont.31 unwind label %lock.lpad.3, !dbg !127
lock.ret.12:
  unreachable
lock.done.13:
  br label %lock.fin.4
if.then.14:
  %t21 = load i64, ptr %ThN.slot
  %t22 = trunc i64 %t21 to i8
  %t23 = invoke ptr @Main__ThImage(i8 %t22)
          to label %invoke.cont.16 unwind label %lock.lpad.9, !dbg !130
if.next.15:
  %t25 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  %t26 = load i64, ptr %ThN.slot
  %t27 = zext i8 %t25 to i64
  %t28 = icmp eq i64 %t27, %t26
  %t29 = icmp eq i1 %t28, 0
  br i1 %t29, label %check.fault.20, label %check.cont.21
invoke.cont.16:
  %t24 = invoke ptr @RTHooks__Concat(ptr %t23, ptr getelementptr inbounds (i8, ptr @textlit_10, i64 8))
          to label %invoke.cont.17 unwind label %lock.lpad.9, !dbg !130
invoke.cont.17:
  invoke void @Main__W(ptr %t24)
          to label %invoke.cont.18 unwind label %lock.lpad.9, !dbg !130
invoke.cont.18:
  invoke void @RTHooks__Raise(ptr @Main__Failure_excptr, ptr null, ptr null, i64 0)
          to label %invoke.cont.19 unwind label %lock.lpad.9, !dbg !132
invoke.cont.19:
  unreachable
check.fault.20:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 3680)
          to label %invoke.cont.22 unwind label %lock.lpad.9, !dbg !131
check.cont.21:
  %t30 = trunc i64 0 to i8
  store i8 %t30, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  store i8 5, ptr %t18
  %t31 = load i64, ptr %ThN.slot
  %t32 = trunc i64 %t31 to i8
  %t33 = invoke ptr @Main__ThImage(i8 %t32)
          to label %invoke.cont.23 unwind label %lock.lpad.9, !dbg !135
invoke.cont.22:
  unreachable
invoke.cont.23:
  %t34 = invoke ptr @RTHooks__Concat(ptr %t33, ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8))
          to label %invoke.cont.24 unwind label %lock.lpad.9, !dbg !135
invoke.cont.24:
  invoke void @Main__W(ptr %t34)
          to label %invoke.cont.25 unwind label %lock.lpad.9, !dbg !135
invoke.cont.25:
  store i32 2, ptr %t16
  br label %lock.fin.10
exit.dead.26:
  br label %if.merge.27
if.merge.27:
  br label %lock.fin.10
dispatch.cont.28:
  %t43 = load i32, ptr %t16
  %t44 = icmp eq i32 %t43, 1
  br i1 %t44, label %lock.resume.11, label %lock.fin.notexc.29
lock.fin.notexc.29:
  %t45 = icmp eq i32 %t43, 2
  br i1 %t45, label %lock.doexit.30, label %lock.done.13
lock.doexit.30:
  store i32 2, ptr %t10
  br label %lock.fin.4
lock.resume.cont.31:
  unreachable
lock.fin.notexc.32:
  %t57 = icmp eq i32 %t55, 2
  br i1 %t57, label %lock.doexit.33, label %lock.done.7
lock.doexit.33:
  br label %loop.exit.2
}

define i8 @Main__WaitForHeld() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %t7 = alloca i8
  %t6 = alloca i32
  %t5 = alloca ptr
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !62, metadata !DIExpression()), !dbg !137
  %LHolder.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LHolder.slot, metadata !63, metadata !DIExpression()), !dbg !137
  store i64 0, ptr %LHolder.slot
  store i64 0, ptr %_result.slot
  br label %loop.header.1
loop.header.1:
  call void @Thread__Pause(double 0x3fb999999999999a), !dbg !139
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t1, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t1 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t1, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t1), !dbg !136
  br label %gc.skip.1
gc.skip.1:
  %t2 = bitcast ptr %t1 to ptr
  %t3 = load ptr, ptr %t2
  %t4 = load ptr, ptr %t3
  call void %t4(ptr %t1), !dbg !136
  store i32 0, ptr %t6
  %t8 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  %t9 = zext i8 %t8 to i64
  store i64 %t9, ptr %LHolder.slot
  br label %lock.fin.4
loop.exit.2:
  %t23 = load i64, ptr %LHolder.slot
  %t24 = trunc i64 %t23 to i8
  ret i8 %t24
lock.lpad.3:
  %t10 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t11 = extractvalue { ptr, i32 } %t10, 0
  %t12 = call ptr @__cxa_get_exception_ptr(ptr %t11), !dbg !136
  %t13 = load ptr, ptr %t12
  store ptr %t13, ptr %t5
  store i32 1, ptr %t6
  br label %lock.fin.4
lock.fin.4:
  %t14 = bitcast ptr %t1 to ptr
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr i8, ptr %t15, i64 8
  %t17 = load ptr, ptr %t16
  call void %t17(ptr %t1), !dbg !136
  %t18 = load i32, ptr %t6
  %t19 = icmp eq i32 %t18, 1
  br i1 %t19, label %lock.resume.5, label %lock.fin.notexc.8
lock.resume.5:
  %t20 = load ptr, ptr %t5
  call void @RTHooks__ResumeRaise(ptr %t20), !dbg !136
  unreachable
lock.ret.6:
  unreachable
lock.done.7:
  %t21 = load i64, ptr %LHolder.slot
  %t22 = icmp ne i64 %t21, 0
  br i1 %t22, label %if.then.9, label %if.merge.10
lock.fin.notexc.8:
  br label %lock.done.7
if.then.9:
  br label %loop.exit.2
if.merge.10:
  br label %loop.header.1
exit.dead.11:
  br label %if.merge.10
}

define void @Main__WaitForStateSet(i8 %a.ThN, i8 %a.Sts) personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %t7 = alloca i32
  %t6 = alloca ptr
  %Sts.slot = alloca i8
  call void @llvm.dbg.declare(metadata ptr %Sts.slot, metadata !64, metadata !DIExpression()), !dbg !143
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !65, metadata !DIExpression()), !dbg !143
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store i8 %a.Sts, ptr %Sts.slot
  br label %loop.header.1
loop.header.1:
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !142
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !142
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t8
  %t10 = load i8, ptr %Sts.slot
  %t11 = load i8, ptr %t9
  %t12 = icmp ult i8 %t11, 8
  %t13 = select i1 %t12, i8 %t11, i8 0
  %t14 = lshr i8 %t10, %t13
  %t15 = and i8 %t14, 1
  %t16 = icmp ne i8 %t15, 0
  br i1 %t16, label %if.then.8, label %if.merge.9
loop.exit.2:
  ret void
lock.lpad.3:
  %t17 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t18 = extractvalue { ptr, i32 } %t17, 0
  %t19 = call ptr @__cxa_get_exception_ptr(ptr %t18), !dbg !148
  %t20 = load ptr, ptr %t19
  store ptr %t20, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.4
lock.fin.4:
  %t21 = bitcast ptr %t2 to ptr
  %t22 = load ptr, ptr %t21
  %t23 = getelementptr i8, ptr %t22, i64 8
  %t24 = load ptr, ptr %t23
  call void %t24(ptr %t2), !dbg !148
  %t25 = load i32, ptr %t7
  %t26 = icmp eq i32 %t25, 1
  br i1 %t26, label %lock.resume.5, label %lock.fin.notexc.11
lock.resume.5:
  %t28 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t28), !dbg !148
  unreachable
lock.ret.6:
  unreachable
lock.done.7:
  call void @Thread__Pause(double 0x3fb999999999999a), !dbg !147
  br label %loop.header.1
if.then.8:
  store i32 2, ptr %t7
  br label %lock.fin.4
if.merge.9:
  br label %lock.fin.4
exit.dead.10:
  br label %if.merge.9
lock.fin.notexc.11:
  %t27 = icmp eq i32 %t25, 2
  br i1 %t27, label %lock.doexit.12, label %lock.done.7
lock.doexit.12:
  br label %loop.exit.2
}

define void @Main__WaitForState(i8 %a.ThN, i8 %a.St) personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %St.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %St.slot, metadata !66, metadata !DIExpression()), !dbg !149
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !67, metadata !DIExpression()), !dbg !149
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = zext i8 %a.St to i64
  store i64 %t2, ptr %St.slot
  %t3 = load i64, ptr %ThN.slot
  %t4 = trunc i64 %t3 to i8
  %t5 = load i64, ptr %St.slot
  %t6 = trunc i64 %t5 to i8
  %t7 = icmp slt i8 %t6, 0
  br i1 %t7, label %check.fault.1, label %check.cont.2
check.fault.1:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5121), !dbg !150
  unreachable
check.cont.2:
  %t8 = icmp sgt i8 %t6, 6
  br i1 %t8, label %check.fault.3, label %check.cont.4
check.fault.3:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 5121), !dbg !150
  unreachable
check.cont.4:
  %t9 = shl i8 1, %t6
  %t10 = or i8 0, %t9
  call void @Main__WaitForStateSet(i8 %t4, i8 %t10), !dbg !150
  ret void
}

define i1 @Main__NoteWhetherStateSet(i8 %a.ThN, i8 %a.Sts, ptr %a.YesMsg, ptr %a.NoMsg) personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %t8 = alloca i1
  %t7 = alloca i32
  %t6 = alloca ptr
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !68, metadata !DIExpression()), !dbg !152
  %LMsg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %LMsg.slot, metadata !69, metadata !DIExpression()), !dbg !152
  %LState.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LState.slot, metadata !70, metadata !DIExpression()), !dbg !152
  %LResult.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LResult.slot, metadata !71, metadata !DIExpression()), !dbg !152
  %NoMsg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %NoMsg.slot, metadata !72, metadata !DIExpression()), !dbg !152
  %YesMsg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %YesMsg.slot, metadata !73, metadata !DIExpression()), !dbg !152
  %Sts.slot = alloca i8
  call void @llvm.dbg.declare(metadata ptr %Sts.slot, metadata !74, metadata !DIExpression()), !dbg !152
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !75, metadata !DIExpression()), !dbg !152
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store i8 %a.Sts, ptr %Sts.slot
  store ptr %a.YesMsg, ptr %YesMsg.slot
  store ptr %a.NoMsg, ptr %NoMsg.slot
  store i64 0, ptr %LResult.slot
  store i64 0, ptr %LState.slot
  store ptr null, ptr %LMsg.slot
  store i64 0, ptr %_result.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !151
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !151
  store i32 0, ptr %t7
  %t9 = load i64, ptr %ThN.slot
  %t10 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t9
  %t11 = load i8, ptr %t10
  %t12 = zext i8 %t11 to i64
  store i64 %t12, ptr %LState.slot
  %t13 = load i8, ptr %Sts.slot
  %t14 = load i64, ptr %LState.slot
  %t15 = zext i8 %t13 to i64
  %t16 = icmp ult i64 %t14, 64
  %t17 = select i1 %t16, i64 %t14, i64 0
  %t18 = lshr i64 %t15, %t17
  %t19 = and i64 %t18, 1
  %t20 = icmp ne i64 %t19, 0
  %t21 = zext i1 %t20 to i64
  store i64 %t21, ptr %LResult.slot
  %t22 = load i64, ptr %ThN.slot
  %t23 = trunc i64 %t22 to i8
  %t24 = invoke ptr @Main__ThImage(i8 %t23)
          to label %invoke.cont.6 unwind label %lock.lpad.1, !dbg !155
lock.lpad.1:
  %t42 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t43 = extractvalue { ptr, i32 } %t42, 0
  %t44 = call ptr @__cxa_get_exception_ptr(ptr %t43), !dbg !156
  %t45 = load ptr, ptr %t44
  store ptr %t45, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t46 = bitcast ptr %t2 to ptr
  %t47 = load ptr, ptr %t46
  %t48 = getelementptr i8, ptr %t47, i64 8
  %t49 = load ptr, ptr %t48
  call void %t49(ptr %t2), !dbg !156
  %t50 = load i32, ptr %t7
  %t51 = icmp eq i32 %t50, 1
  br i1 %t51, label %lock.resume.3, label %lock.fin.notexc.20
lock.resume.3:
  %t52 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t52), !dbg !156
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  %t53 = load i64, ptr %LResult.slot
  %t54 = trunc i64 %t53 to i1
  ret i1 %t54
invoke.cont.6:
  %t25 = invoke ptr @RTHooks__Concat(ptr %t24, ptr getelementptr inbounds (i8, ptr @textlit_13, i64 8))
          to label %invoke.cont.7 unwind label %lock.lpad.1, !dbg !155
invoke.cont.7:
  %t26 = load i64, ptr %LState.slot
  %t27 = invoke ptr @Main__StateImage(i64 %t26)
          to label %invoke.cont.8 unwind label %lock.lpad.1, !dbg !155
invoke.cont.8:
  %t28 = invoke ptr @RTHooks__Concat(ptr %t25, ptr %t27)
          to label %invoke.cont.9 unwind label %lock.lpad.1, !dbg !155
invoke.cont.9:
  store ptr %t28, ptr %LMsg.slot
  %t29 = load i64, ptr %LResult.slot
  %t30 = trunc i64 %t29 to i1
  br i1 %t30, label %if.then.10, label %if.next.11
if.then.10:
  %t31 = load ptr, ptr %YesMsg.slot
  %t32 = icmp ne ptr %t31, null
  br i1 %t32, label %if.then.12, label %if.merge.13
if.next.11:
  %t36 = load ptr, ptr %NoMsg.slot
  %t37 = icmp ne ptr %t36, null
  br i1 %t37, label %if.then.16, label %if.merge.17
if.then.12:
  %t33 = load ptr, ptr %LMsg.slot
  %t34 = load ptr, ptr %YesMsg.slot
  %t35 = invoke ptr @RTHooks__Concat(ptr %t33, ptr %t34)
          to label %invoke.cont.14 unwind label %lock.lpad.1, !dbg !159
if.merge.13:
  br label %if.merge.15
invoke.cont.14:
  store ptr %t35, ptr %LMsg.slot
  br label %if.merge.13
if.merge.15:
  %t41 = load ptr, ptr %LMsg.slot
  invoke void @Main__W(ptr %t41)
          to label %invoke.cont.19 unwind label %lock.lpad.1, !dbg !156
if.then.16:
  %t38 = load ptr, ptr %LMsg.slot
  %t39 = load ptr, ptr %NoMsg.slot
  %t40 = invoke ptr @RTHooks__Concat(ptr %t38, ptr %t39)
          to label %invoke.cont.18 unwind label %lock.lpad.1, !dbg !160
if.merge.17:
  br label %if.merge.15
invoke.cont.18:
  store ptr %t40, ptr %LMsg.slot
  br label %if.merge.17
invoke.cont.19:
  br label %lock.fin.2
lock.fin.notexc.20:
  br label %lock.done.5
}

define i1 @Main__NoteWhetherState(i8 %a.ThN, i8 %a.St, ptr %a.YesMsg, ptr %a.NoMsg) personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !76, metadata !DIExpression()), !dbg !161
  %NoMsg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %NoMsg.slot, metadata !77, metadata !DIExpression()), !dbg !161
  %YesMsg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %YesMsg.slot, metadata !78, metadata !DIExpression()), !dbg !161
  %St.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %St.slot, metadata !79, metadata !DIExpression()), !dbg !161
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !80, metadata !DIExpression()), !dbg !161
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = zext i8 %a.St to i64
  store i64 %t2, ptr %St.slot
  store ptr %a.YesMsg, ptr %YesMsg.slot
  store ptr %a.NoMsg, ptr %NoMsg.slot
  store i64 0, ptr %_result.slot
  %t3 = load i64, ptr %ThN.slot
  %t4 = trunc i64 %t3 to i8
  %t5 = load i64, ptr %St.slot
  %t6 = trunc i64 %t5 to i8
  %t7 = icmp slt i8 %t6, 0
  br i1 %t7, label %check.fault.1, label %check.cont.2
check.fault.1:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6081), !dbg !162
  unreachable
check.cont.2:
  %t8 = icmp sgt i8 %t6, 6
  br i1 %t8, label %check.fault.3, label %check.cont.4
check.fault.3:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 6081), !dbg !162
  unreachable
check.cont.4:
  %t9 = shl i8 1, %t6
  %t10 = or i8 0, %t9
  %t11 = load ptr, ptr %YesMsg.slot
  %t12 = load ptr, ptr %NoMsg.slot
  %t13 = call i1 @Main__NoteWhetherStateSet(i8 %t4, i8 %t10, ptr %t11, ptr %t12), !dbg !162
  ret i1 %t13
}

define ptr @Main__TestApply(ptr %a.Self) personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %t13 = alloca ptr
  %t12 = alloca i32
  %t11 = alloca ptr
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !81, metadata !DIExpression()), !dbg !164
  %LProc.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %LProc.slot, metadata !82, metadata !DIExpression()), !dbg !164
  %Self.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %Self.slot, metadata !88, metadata !DIExpression()), !dbg !164
  store ptr %a.Self, ptr %Self.slot
  store ptr null, ptr %LProc.slot
  store ptr null, ptr %_result.slot
  br label %loop.header.1
loop.header.1:
  %t1 = load ptr, ptr %Self.slot
  %t2 = getelementptr i8, ptr %t1, i64 8
  %t3 = load i8, ptr %t2
  %t4 = sitofp i8 %t3 to double
  %t5 = fmul double %t4, 0x3f847ae147ae147b
  %t6 = fadd double 0x3fc999999999999a, %t5
  call void @Thread__Pause(double %t6), !dbg !166
  %t7 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
  %__gc_nil.1 = icmp eq ptr %t7, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t7 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t7, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t7), !dbg !163
  br label %gc.skip.1
gc.skip.1:
  %t8 = bitcast ptr %t7 to ptr
  %t9 = load ptr, ptr %t8
  %t10 = load ptr, ptr %t9
  call void %t10(ptr %t7), !dbg !163
  store i32 0, ptr %t12
  %t14 = load ptr, ptr %Self.slot
  %t15 = getelementptr i8, ptr %t14, i64 8
  %t16 = load i8, ptr %t15
  %t17 = zext i8 %t16 to i64
  %t18 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i64 0, i64 %t17
  %t19 = load ptr, ptr %t18
  %t20 = icmp ne ptr %t19, null
  br i1 %t20, label %if.then.8, label %if.merge.9
loop.exit.2:
  ret ptr null
lock.lpad.3:
  %t22 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t23 = extractvalue { ptr, i32 } %t22, 0
  %t24 = call ptr @__cxa_get_exception_ptr(ptr %t23), !dbg !170
  %t25 = load ptr, ptr %t24
  store ptr %t25, ptr %t11
  store i32 1, ptr %t12
  br label %lock.fin.4
lock.fin.4:
  %t26 = bitcast ptr %t7 to ptr
  %t27 = load ptr, ptr %t26
  %t28 = getelementptr i8, ptr %t27, i64 8
  %t29 = load ptr, ptr %t28
  call void %t29(ptr %t7), !dbg !170
  %t30 = load i32, ptr %t12
  %t31 = icmp eq i32 %t30, 1
  br i1 %t31, label %lock.resume.5, label %lock.fin.notexc.10
lock.resume.5:
  %t32 = load ptr, ptr %t11
  call void @RTHooks__ResumeRaise(ptr %t32), !dbg !170
  unreachable
lock.ret.6:
  unreachable
lock.done.7:
  %t33 = load ptr, ptr %LProc.slot
  %t34 = icmp ne ptr %t33, null
  br i1 %t34, label %if.then.11, label %if.merge.12
if.then.8:
  %t21 = load ptr, ptr %t18
  store ptr %t21, ptr %LProc.slot
  store ptr null, ptr %t18
  br label %if.merge.9
if.merge.9:
  br label %lock.fin.4
lock.fin.notexc.10:
  br label %lock.done.7
if.then.11:
  %t35 = load ptr, ptr %LProc.slot
  %t36 = load ptr, ptr %Self.slot
  %t37 = getelementptr i8, ptr %t36, i64 8
  %t38 = load i8, ptr %t37
  %t39 = load i64, ptr %t35
  %t40 = icmp eq i64 %t39, -1
  br i1 %t40, label %cl.closure.13, label %cl.direct.14
if.merge.12:
  br label %loop.header.1
cl.closure.13:
  %t41 = getelementptr i8, ptr %t35, i64 8
  %t42 = load ptr, ptr %t41
  %t43 = getelementptr i8, ptr %t35, i64 16
  %t44 = load ptr, ptr %t43
  call void %t42(ptr %t44, i8 %t38), !dbg !173
  br label %cl.merge.15
cl.direct.14:
  call void %t35(i8 %t38), !dbg !173
  br label %cl.merge.15
cl.merge.15:
  store ptr null, ptr %LProc.slot
  br label %if.merge.12
}

define void @Main__DoAcq(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %t35 = alloca i32
  %t34 = alloca ptr
  %t7 = alloca i32
  %t6 = alloca ptr
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !89, metadata !DIExpression()), !dbg !177
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !176
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !176
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t8
  store i8 2, ptr %t9
  br label %lock.fin.2
lock.lpad.1:
  %t10 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t11 = extractvalue { ptr, i32 } %t10, 0
  %t12 = call ptr @__cxa_get_exception_ptr(ptr %t11), !dbg !176
  %t13 = load ptr, ptr %t12
  store ptr %t13, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t14 = bitcast ptr %t2 to ptr
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr i8, ptr %t15, i64 8
  %t17 = load ptr, ptr %t16
  call void %t17(ptr %t2), !dbg !176
  %t18 = load i32, ptr %t7
  %t19 = icmp eq i32 %t18, 1
  br i1 %t19, label %lock.resume.3, label %lock.fin.notexc.6
lock.resume.3:
  %t20 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t20), !dbg !176
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  %t21 = load i64, ptr %ThN.slot
  %t22 = trunc i64 %t21 to i8
  %t23 = call ptr @Main__ThImage(i8 %t22), !dbg !178
  %t24 = call ptr @RTHooks__Concat(ptr %t23, ptr getelementptr inbounds (i8, ptr @textlit_14, i64 8)), !dbg !178
  call void @Main__W(ptr %t24), !dbg !178
  %t25 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %__gc_nil.2 = icmp eq ptr %t25, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t25 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t25, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t25), !dbg !179
  br label %gc.skip.2
gc.skip.2:
  call void @Thread__Acquire(ptr %t25), !dbg !179
  %t26 = load i64, ptr %ThN.slot
  %t27 = trunc i64 %t26 to i8
  %t28 = call ptr @Main__ThImage(i8 %t27), !dbg !180
  %t29 = call ptr @RTHooks__Concat(ptr %t28, ptr getelementptr inbounds (i8, ptr @textlit_15, i64 8)), !dbg !180
  call void @Main__W(ptr %t29), !dbg !180
  %t30 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.3 = icmp eq ptr %t30, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t30 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t30, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t30), !dbg !175
  br label %gc.skip.3
gc.skip.3:
  %t31 = bitcast ptr %t30 to ptr
  %t32 = load ptr, ptr %t31
  %t33 = load ptr, ptr %t32
  call void %t33(ptr %t30), !dbg !175
  store i32 0, ptr %t35
  %t36 = load i64, ptr %ThN.slot
  %t37 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t36
  %t38 = load i8, ptr %t37
  %t39 = icmp eq i8 %t38, 2
  %t40 = icmp eq i1 %t39, 0
  br i1 %t40, label %check.fault.12, label %check.cont.13
lock.fin.notexc.6:
  br label %lock.done.5
lock.lpad.7:
  %t47 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t48 = extractvalue { ptr, i32 } %t47, 0
  %t49 = call ptr @__cxa_get_exception_ptr(ptr %t48), !dbg !183
  %t50 = load ptr, ptr %t49
  store ptr %t50, ptr %t34
  store i32 1, ptr %t35
  br label %lock.fin.8
lock.fin.8:
  %t51 = bitcast ptr %t30 to ptr
  %t52 = load ptr, ptr %t51
  %t53 = getelementptr i8, ptr %t52, i64 8
  %t54 = load ptr, ptr %t53
  call void %t54(ptr %t30), !dbg !183
  %t55 = load i32, ptr %t35
  %t56 = icmp eq i32 %t55, 1
  br i1 %t56, label %lock.resume.9, label %lock.fin.notexc.18
lock.resume.9:
  %t57 = load ptr, ptr %t34
  call void @RTHooks__ResumeRaise(ptr %t57), !dbg !183
  unreachable
lock.ret.10:
  unreachable
lock.done.11:
  ret void
check.fault.12:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7744)
          to label %invoke.cont.14 unwind label %lock.lpad.7, !dbg !182
check.cont.13:
  store i8 1, ptr %t37
  %t41 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  %t42 = zext i8 %t41 to i64
  %t43 = icmp eq i64 %t42, 0
  %t44 = icmp eq i1 %t43, 0
  br i1 %t44, label %check.fault.15, label %check.cont.16
invoke.cont.14:
  unreachable
check.fault.15:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 7808)
          to label %invoke.cont.17 unwind label %lock.lpad.7, !dbg !185
check.cont.16:
  %t45 = load i64, ptr %ThN.slot
  %t46 = trunc i64 %t45 to i8
  store i8 %t46, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  br label %lock.fin.8
invoke.cont.17:
  unreachable
lock.fin.notexc.18:
  br label %lock.done.11
}

define void @Main__DoRel(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %t35 = alloca i32
  %t34 = alloca ptr
  %t7 = alloca i32
  %t6 = alloca ptr
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !90, metadata !DIExpression()), !dbg !188
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !187
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !187
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t8
  store i8 3, ptr %t9
  br label %lock.fin.2
lock.lpad.1:
  %t10 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t11 = extractvalue { ptr, i32 } %t10, 0
  %t12 = call ptr @__cxa_get_exception_ptr(ptr %t11), !dbg !187
  %t13 = load ptr, ptr %t12
  store ptr %t13, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t14 = bitcast ptr %t2 to ptr
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr i8, ptr %t15, i64 8
  %t17 = load ptr, ptr %t16
  call void %t17(ptr %t2), !dbg !187
  %t18 = load i32, ptr %t7
  %t19 = icmp eq i32 %t18, 1
  br i1 %t19, label %lock.resume.3, label %lock.fin.notexc.6
lock.resume.3:
  %t20 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t20), !dbg !187
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  %t21 = load i64, ptr %ThN.slot
  %t22 = trunc i64 %t21 to i8
  %t23 = call ptr @Main__ThImage(i8 %t22), !dbg !189
  %t24 = call ptr @RTHooks__Concat(ptr %t23, ptr getelementptr inbounds (i8, ptr @textlit_18, i64 8)), !dbg !189
  call void @Main__W(ptr %t24), !dbg !189
  %t25 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %__gc_nil.2 = icmp eq ptr %t25, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t25 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t25, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t25), !dbg !190
  br label %gc.skip.2
gc.skip.2:
  call void @Thread__Release(ptr %t25), !dbg !190
  %t26 = load i64, ptr %ThN.slot
  %t27 = trunc i64 %t26 to i8
  %t28 = call ptr @Main__ThImage(i8 %t27), !dbg !191
  %t29 = call ptr @RTHooks__Concat(ptr %t28, ptr getelementptr inbounds (i8, ptr @textlit_19, i64 8)), !dbg !191
  call void @Main__W(ptr %t29), !dbg !191
  %t30 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.3 = icmp eq ptr %t30, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t30 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t30, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t30), !dbg !186
  br label %gc.skip.3
gc.skip.3:
  %t31 = bitcast ptr %t30 to ptr
  %t32 = load ptr, ptr %t31
  %t33 = load ptr, ptr %t32
  call void %t33(ptr %t30), !dbg !186
  store i32 0, ptr %t35
  %t36 = load i64, ptr %ThN.slot
  %t37 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t36
  %t38 = load i8, ptr %t37
  %t39 = icmp eq i8 %t38, 3
  %t40 = icmp eq i1 %t39, 0
  br i1 %t40, label %check.fault.12, label %check.cont.13
lock.fin.notexc.6:
  br label %lock.done.5
lock.lpad.7:
  %t47 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t48 = extractvalue { ptr, i32 } %t47, 0
  %t49 = call ptr @__cxa_get_exception_ptr(ptr %t48), !dbg !194
  %t50 = load ptr, ptr %t49
  store ptr %t50, ptr %t34
  store i32 1, ptr %t35
  br label %lock.fin.8
lock.fin.8:
  %t51 = bitcast ptr %t30 to ptr
  %t52 = load ptr, ptr %t51
  %t53 = getelementptr i8, ptr %t52, i64 8
  %t54 = load ptr, ptr %t53
  call void %t54(ptr %t30), !dbg !194
  %t55 = load i32, ptr %t35
  %t56 = icmp eq i32 %t55, 1
  br i1 %t56, label %lock.resume.9, label %lock.fin.notexc.18
lock.resume.9:
  %t57 = load ptr, ptr %t34
  call void @RTHooks__ResumeRaise(ptr %t57), !dbg !194
  unreachable
lock.ret.10:
  unreachable
lock.done.11:
  ret void
check.fault.12:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 8288)
          to label %invoke.cont.14 unwind label %lock.lpad.7, !dbg !193
check.cont.13:
  store i8 1, ptr %t37
  %t41 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  %t42 = load i64, ptr %ThN.slot
  %t43 = zext i8 %t41 to i64
  %t44 = icmp eq i64 %t43, %t42
  %t45 = icmp eq i1 %t44, 0
  br i1 %t45, label %check.fault.15, label %check.cont.16
invoke.cont.14:
  unreachable
check.fault.15:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 8352)
          to label %invoke.cont.17 unwind label %lock.lpad.7, !dbg !196
check.cont.16:
  %t46 = trunc i64 0 to i8
  store i8 %t46, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  br label %lock.fin.8
invoke.cont.17:
  unreachable
lock.fin.notexc.18:
  br label %lock.done.11
}

define void @Main__DoWait(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %t44 = alloca i32
  %t43 = alloca ptr
  %t7 = alloca i32
  %t6 = alloca ptr
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !91, metadata !DIExpression()), !dbg !199
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !198
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !198
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t8
  %t10 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  %t11 = load i64, ptr %ThN.slot
  %t12 = zext i8 %t10 to i64
  %t13 = icmp eq i64 %t12, %t11
  %t14 = icmp eq i1 %t13, 0
  br i1 %t14, label %check.fault.6, label %check.cont.7
lock.lpad.1:
  %t18 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t19 = extractvalue { ptr, i32 } %t18, 0
  %t20 = call ptr @__cxa_get_exception_ptr(ptr %t19), !dbg !202
  %t21 = load ptr, ptr %t20
  store ptr %t21, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t22 = bitcast ptr %t2 to ptr
  %t23 = load ptr, ptr %t22
  %t24 = getelementptr i8, ptr %t23, i64 8
  %t25 = load ptr, ptr %t24
  call void %t25(ptr %t2), !dbg !202
  %t26 = load i32, ptr %t7
  %t27 = icmp eq i32 %t26, 1
  br i1 %t27, label %lock.resume.3, label %lock.fin.notexc.12
lock.resume.3:
  %t28 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t28), !dbg !202
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  %t29 = load i64, ptr %ThN.slot
  %t30 = trunc i64 %t29 to i8
  %t31 = call ptr @Main__ThImage(i8 %t30), !dbg !203
  %t32 = call ptr @RTHooks__Concat(ptr %t31, ptr getelementptr inbounds (i8, ptr @textlit_22, i64 8)), !dbg !203
  call void @Main__W(ptr %t32), !dbg !203
  %t33 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %__gc_nil.2 = icmp eq ptr %t33, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t33 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t33, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t33), !dbg !204
  br label %gc.skip.2
gc.skip.2:
  %t34 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  %__gc_nil.3 = icmp eq ptr %t34, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t34 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t34, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t34), !dbg !204
  br label %gc.skip.3
gc.skip.3:
  call void @Thread__Wait(ptr %t33, ptr %t34), !dbg !204
  %t35 = load i64, ptr %ThN.slot
  %t36 = trunc i64 %t35 to i8
  %t37 = call ptr @Main__ThImage(i8 %t36), !dbg !205
  %t38 = call ptr @RTHooks__Concat(ptr %t37, ptr getelementptr inbounds (i8, ptr @textlit_23, i64 8)), !dbg !205
  call void @Main__W(ptr %t38), !dbg !205
  %t39 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.4 = icmp eq ptr %t39, null
  br i1 %__gc_nil.4, label %gc.skip.4, label %gc.check.4
gc.check.4:
  %__gc_int.4 = ptrtoint ptr %t39 to i64
  %__gc_low.4 = and i64 %__gc_int.4, 1
  %__gc_ma.4 = icmp ne i64 %__gc_low.4, 0
  br i1 %__gc_ma.4, label %gc.skip.4, label %gc.gray.4
gc.gray.4:
  %__gc_hptr.4 = getelementptr i8, ptr %t39, i64 -8
  %__gc_hdr.4 = load i64, ptr %__gc_hptr.4
  %__gc_gb.4 = and i64 %__gc_hdr.4, 4194304
  %__gc_gr.4 = icmp ne i64 %__gc_gb.4, 0
  br i1 %__gc_gr.4, label %gc.slow.4, label %gc.skip.4
gc.slow.4:
  call void @RTHooks__CheckLoadTracedRef(ptr %t39), !dbg !197
  br label %gc.skip.4
gc.skip.4:
  %t40 = bitcast ptr %t39 to ptr
  %t41 = load ptr, ptr %t40
  %t42 = load ptr, ptr %t41
  call void %t42(ptr %t39), !dbg !197
  store i32 0, ptr %t44
  %t45 = load i64, ptr %ThN.slot
  %t46 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t45
  %t47 = load i8, ptr %t46
  %t48 = icmp eq i8 %t47, 5
  %t49 = icmp eq i1 %t48, 0
  br i1 %t49, label %check.fault.18, label %check.cont.19
check.fault.6:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 8704)
          to label %invoke.cont.8 unwind label %lock.lpad.1, !dbg !201
check.cont.7:
  %t15 = load i8, ptr %t9
  %t16 = icmp eq i8 %t15, 1
  %t17 = icmp eq i1 %t16, 0
  br i1 %t17, label %check.fault.9, label %check.cont.10
invoke.cont.8:
  unreachable
check.fault.9:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 8736)
          to label %invoke.cont.11 unwind label %lock.lpad.1, !dbg !208
check.cont.10:
  store i8 4, ptr %t9
  br label %lock.fin.2
invoke.cont.11:
  unreachable
lock.fin.notexc.12:
  br label %lock.done.5
lock.lpad.13:
  %t54 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t55 = extractvalue { ptr, i32 } %t54, 0
  %t56 = call ptr @__cxa_get_exception_ptr(ptr %t55), !dbg !209
  %t57 = load ptr, ptr %t56
  store ptr %t57, ptr %t43
  store i32 1, ptr %t44
  br label %lock.fin.14
lock.fin.14:
  %t58 = bitcast ptr %t39 to ptr
  %t59 = load ptr, ptr %t58
  %t60 = getelementptr i8, ptr %t59, i64 8
  %t61 = load ptr, ptr %t60
  call void %t61(ptr %t39), !dbg !209
  %t62 = load i32, ptr %t44
  %t63 = icmp eq i32 %t62, 1
  br i1 %t63, label %lock.resume.15, label %lock.fin.notexc.21
lock.resume.15:
  %t64 = load ptr, ptr %t43
  call void @RTHooks__ResumeRaise(ptr %t64), !dbg !209
  unreachable
lock.ret.16:
  unreachable
lock.done.17:
  ret void
check.fault.18:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 9120)
          to label %invoke.cont.20 unwind label %lock.lpad.13, !dbg !207
check.cont.19:
  %t50 = load i64, ptr %ThN.slot
  %t51 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t50
  store i8 1, ptr %t51
  %t52 = load i64, ptr %ThN.slot
  %t53 = trunc i64 %t52 to i8
  store i8 %t53, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
  br label %lock.fin.14
invoke.cont.20:
  unreachable
lock.fin.notexc.21:
  br label %lock.done.17
}

define void @Main__DoSignal(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !44 {
entry:
  %t35 = alloca i32
  %t34 = alloca ptr
  %t7 = alloca i32
  %t6 = alloca ptr
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !92, metadata !DIExpression()), !dbg !213
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.1 = icmp eq ptr %t2, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t2 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !212
  br label %gc.skip.1
gc.skip.1:
  %t3 = bitcast ptr %t2 to ptr
  %t4 = load ptr, ptr %t3
  %t5 = load ptr, ptr %t4
  call void %t5(ptr %t2), !dbg !212
  store i32 0, ptr %t7
  %t8 = load i64, ptr %ThN.slot
  %t9 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t8
  store i8 6, ptr %t9
  br label %lock.fin.2
lock.lpad.1:
  %t10 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t11 = extractvalue { ptr, i32 } %t10, 0
  %t12 = call ptr @__cxa_get_exception_ptr(ptr %t11), !dbg !212
  %t13 = load ptr, ptr %t12
  store ptr %t13, ptr %t6
  store i32 1, ptr %t7
  br label %lock.fin.2
lock.fin.2:
  %t14 = bitcast ptr %t2 to ptr
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr i8, ptr %t15, i64 8
  %t17 = load ptr, ptr %t16
  call void %t17(ptr %t2), !dbg !212
  %t18 = load i32, ptr %t7
  %t19 = icmp eq i32 %t18, 1
  br i1 %t19, label %lock.resume.3, label %lock.fin.notexc.6
lock.resume.3:
  %t20 = load ptr, ptr %t6
  call void @RTHooks__ResumeRaise(ptr %t20), !dbg !212
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  %t21 = load i64, ptr %ThN.slot
  %t22 = trunc i64 %t21 to i8
  %t23 = call ptr @Main__ThImage(i8 %t22), !dbg !214
  %t24 = call ptr @RTHooks__Concat(ptr %t23, ptr getelementptr inbounds (i8, ptr @textlit_25, i64 8)), !dbg !214
  call void @Main__W(ptr %t24), !dbg !214
  %t25 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  %__gc_nil.2 = icmp eq ptr %t25, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t25 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t25, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t25), !dbg !215
  br label %gc.skip.2
gc.skip.2:
  call void @Thread__Signal(ptr %t25), !dbg !215
  %t26 = load i64, ptr %ThN.slot
  %t27 = trunc i64 %t26 to i8
  %t28 = call ptr @Main__ThImage(i8 %t27), !dbg !216
  %t29 = call ptr @RTHooks__Concat(ptr %t28, ptr getelementptr inbounds (i8, ptr @textlit_26, i64 8)), !dbg !216
  call void @Main__W(ptr %t29), !dbg !216
  %t30 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %__gc_nil.3 = icmp eq ptr %t30, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t30 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t30, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t30), !dbg !211
  br label %gc.skip.3
gc.skip.3:
  %t31 = bitcast ptr %t30 to ptr
  %t32 = load ptr, ptr %t31
  %t33 = load ptr, ptr %t32
  call void %t33(ptr %t30), !dbg !211
  store i32 0, ptr %t35
  %t36 = load i64, ptr %ThN.slot
  %t37 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t36
  store i8 1, ptr %t37
  br label %lock.fin.8
lock.fin.notexc.6:
  br label %lock.done.5
lock.lpad.7:
  %t38 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t39 = extractvalue { ptr, i32 } %t38, 0
  %t40 = call ptr @__cxa_get_exception_ptr(ptr %t39), !dbg !211
  %t41 = load ptr, ptr %t40
  store ptr %t41, ptr %t34
  store i32 1, ptr %t35
  br label %lock.fin.8
lock.fin.8:
  %t42 = bitcast ptr %t30 to ptr
  %t43 = load ptr, ptr %t42
  %t44 = getelementptr i8, ptr %t43, i64 8
  %t45 = load ptr, ptr %t44
  call void %t45(ptr %t30), !dbg !211
  %t46 = load i32, ptr %t35
  %t47 = icmp eq i32 %t46, 1
  br i1 %t47, label %lock.resume.9, label %lock.fin.notexc.12
lock.resume.9:
  %t48 = load ptr, ptr %t34
  call void @RTHooks__ResumeRaise(ptr %t48), !dbg !211
  unreachable
lock.ret.10:
  unreachable
lock.done.11:
  ret void
lock.fin.notexc.12:
  br label %lock.done.11
}

define void @Main__ForceSignalled(i8 %a.ThN) personality ptr @__gxx_personality_v0 !dbg !46 {
entry:
  %LThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LThN.slot, metadata !93, metadata !DIExpression()), !dbg !217
  %ThN.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %ThN.slot, metadata !94, metadata !DIExpression()), !dbg !217
  %t1 = zext i8 %a.ThN to i64
  store i64 %t1, ptr %ThN.slot
  store i64 0, ptr %LThN.slot
  call void @Thread__Pause(double 0x4008000000000000), !dbg !218
  %t2 = load i64, ptr %ThN.slot
  %t3 = trunc i64 %t2 to i8
  %t4 = call i1 @Main__NoteWhetherState(i8 %t3, i8 5, ptr getelementptr inbounds (i8, ptr @textlit_27, i64 8), ptr getelementptr inbounds (i8, ptr @textlit_27, i64 8)), !dbg !219
  br i1 %t4, label %if.then.1, label %if.merge.2
if.then.1:
  %t5 = trunc i64 3 to i8
  call void @Main__Action(i8 %t5, ptr @Main__DoSignal), !dbg !220
  %t6 = trunc i64 3 to i8
  call void @Main__WaitForState(i8 %t6, i8 1), !dbg !221
  %t7 = call i8 @Main__WaitForHeld(), !dbg !222
  %t8 = zext i8 %t7 to i64
  store i64 %t8, ptr %LThN.slot
  %t9 = load i64, ptr %LThN.slot
  %t10 = load i64, ptr %ThN.slot
  %t11 = icmp eq i64 %t9, %t10
  %t12 = icmp eq i1 %t11, 0
  br i1 %t12, label %check.fault.3, label %check.cont.4
if.merge.2:
  %t13 = load i64, ptr %ThN.slot
  %t14 = trunc i64 %t13 to i8
  call void @Main__Action(i8 %t14, ptr @Main__DoRel), !dbg !224
  %t15 = load i64, ptr %ThN.slot
  %t16 = trunc i64 %t15 to i8
  call void @Main__WaitForState(i8 %t16, i8 1), !dbg !225
  ret void
check.fault.3:
  call void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 10112), !dbg !223
  unreachable
check.cont.4:
  br label %if.merge.2
}

define void @Main__TestSeq() personality ptr @__gxx_personality_v0 !dbg !48 {
entry:
  %LThNo.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LThNo.slot, metadata !95, metadata !DIExpression()), !dbg !226
  store i64 0, ptr %LThNo.slot
  %t1 = trunc i64 1 to i8
  invoke void @Main__Action(i8 %t1, ptr @Main__DoAcq)
          to label %invoke.cont.3 unwind label %lpad.1, !dbg !227
lpad.1:
  %t43 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t44 = extractvalue { ptr, i32 } %t43, 0
  %t45 = call ptr @__cxa_get_exception_ptr(ptr %t44), !dbg !228
  %t46 = load ptr, ptr %t45
  %t47 = load ptr, ptr %t46
  %t48 = load i64, ptr %t47
  %t49 = icmp eq i64 %t48, -1505293580
  br i1 %t49, label %h.body.51, label %exc.next.52
try.merge.2:
  ret void
invoke.cont.3:
  %t2 = trunc i64 1 to i8
  invoke void @Main__WaitForState(i8 %t2, i8 1)
          to label %invoke.cont.4 unwind label %lpad.1, !dbg !229
invoke.cont.4:
  %t3 = trunc i64 1 to i8
  invoke void @Main__ActionWait(i8 %t3, ptr @Main__DoWait)
          to label %invoke.cont.5 unwind label %lpad.1, !dbg !230
invoke.cont.5:
  %t4 = trunc i64 1 to i8
  invoke void @Main__WaitForState(i8 %t4, i8 5)
          to label %invoke.cont.6 unwind label %lpad.1, !dbg !231
invoke.cont.6:
  %t5 = trunc i64 2 to i8
  invoke void @Main__Action(i8 %t5, ptr @Main__DoAcq)
          to label %invoke.cont.7 unwind label %lpad.1, !dbg !232
invoke.cont.7:
  %t6 = trunc i64 2 to i8
  invoke void @Main__WaitForState(i8 %t6, i8 1)
          to label %invoke.cont.8 unwind label %lpad.1, !dbg !233
invoke.cont.8:
  %t7 = trunc i64 2 to i8
  invoke void @Main__ActionWait(i8 %t7, ptr @Main__DoWait)
          to label %invoke.cont.9 unwind label %lpad.1, !dbg !234
invoke.cont.9:
  %t8 = trunc i64 2 to i8
  invoke void @Main__WaitForState(i8 %t8, i8 5)
          to label %invoke.cont.10 unwind label %lpad.1, !dbg !235
invoke.cont.10:
  %t9 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t9, ptr @Main__DoAcq)
          to label %invoke.cont.11 unwind label %lpad.1, !dbg !236
invoke.cont.11:
  %t10 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t10, i8 1)
          to label %invoke.cont.12 unwind label %lpad.1, !dbg !237
invoke.cont.12:
  %t11 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t11, ptr @Main__DoSignal)
          to label %invoke.cont.13 unwind label %lpad.1, !dbg !238
invoke.cont.13:
  %t12 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t12, i8 1)
          to label %invoke.cont.14 unwind label %lpad.1, !dbg !239
invoke.cont.14:
  %t13 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t13, ptr @Main__DoRel)
          to label %invoke.cont.15 unwind label %lpad.1, !dbg !240
invoke.cont.15:
  %t14 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t14, i8 1)
          to label %invoke.cont.16 unwind label %lpad.1, !dbg !241
invoke.cont.16:
  %t15 = invoke i8 @Main__WaitForHeld()
          to label %invoke.cont.17 unwind label %lpad.1, !dbg !242
invoke.cont.17:
  %t16 = zext i8 %t15 to i64
  store i64 %t16, ptr %LThNo.slot
  %t17 = load i64, ptr %LThNo.slot
  %t18 = icmp eq i64 %t17, 1
  br i1 %t18, label %if.then.18, label %if.next.19
if.then.18:
  %t19 = trunc i64 1 to i8
  invoke void @Main__Action(i8 %t19, ptr @Main__DoRel)
          to label %invoke.cont.20 unwind label %lpad.1, !dbg !244
if.next.19:
  %t22 = load i64, ptr %LThNo.slot
  %t23 = icmp eq i64 %t22, 2
  %t24 = icmp eq i1 %t23, 0
  br i1 %t24, label %check.fault.24, label %check.cont.25
invoke.cont.20:
  %t20 = trunc i64 1 to i8
  invoke void @Main__WaitForState(i8 %t20, i8 1)
          to label %invoke.cont.21 unwind label %lpad.1, !dbg !246
invoke.cont.21:
  %t21 = trunc i64 2 to i8
  invoke void @Main__ForceSignalled(i8 %t21)
          to label %invoke.cont.22 unwind label %lpad.1, !dbg !247
invoke.cont.22:
  br label %if.merge.23
if.merge.23:
  %t28 = trunc i64 4 to i8
  invoke void @Main__Action(i8 %t28, ptr @Main__DoAcq)
          to label %invoke.cont.30 unwind label %lpad.1, !dbg !248
check.fault.24:
  invoke void @RTHooks__ReportFault(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 0), i64 11936)
          to label %invoke.cont.26 unwind label %lpad.1, !dbg !245
check.cont.25:
  %t25 = trunc i64 2 to i8
  invoke void @Main__Action(i8 %t25, ptr @Main__DoRel)
          to label %invoke.cont.27 unwind label %lpad.1, !dbg !249
invoke.cont.26:
  unreachable
invoke.cont.27:
  %t26 = trunc i64 2 to i8
  invoke void @Main__WaitForState(i8 %t26, i8 1)
          to label %invoke.cont.28 unwind label %lpad.1, !dbg !250
invoke.cont.28:
  %t27 = trunc i64 1 to i8
  invoke void @Main__ForceSignalled(i8 %t27)
          to label %invoke.cont.29 unwind label %lpad.1, !dbg !251
invoke.cont.29:
  br label %if.merge.23
invoke.cont.30:
  %t29 = trunc i64 4 to i8
  invoke void @Main__WaitForState(i8 %t29, i8 1)
          to label %invoke.cont.31 unwind label %lpad.1, !dbg !252
invoke.cont.31:
  %t30 = trunc i64 4 to i8
  invoke void @Main__ActionWait(i8 %t30, ptr @Main__DoWait)
          to label %invoke.cont.32 unwind label %lpad.1, !dbg !253
invoke.cont.32:
  %t31 = trunc i64 4 to i8
  invoke void @Main__WaitForState(i8 %t31, i8 5)
          to label %invoke.cont.33 unwind label %lpad.1, !dbg !254
invoke.cont.33:
  %t32 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t32, ptr @Main__DoAcq)
          to label %invoke.cont.34 unwind label %lpad.1, !dbg !255
invoke.cont.34:
  %t33 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t33, i8 1)
          to label %invoke.cont.35 unwind label %lpad.1, !dbg !256
invoke.cont.35:
  %t34 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t34, ptr @Main__DoSignal)
          to label %invoke.cont.36 unwind label %lpad.1, !dbg !257
invoke.cont.36:
  %t35 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t35, i8 1)
          to label %invoke.cont.37 unwind label %lpad.1, !dbg !258
invoke.cont.37:
  %t36 = trunc i64 3 to i8
  invoke void @Main__Action(i8 %t36, ptr @Main__DoRel)
          to label %invoke.cont.38 unwind label %lpad.1, !dbg !259
invoke.cont.38:
  %t37 = trunc i64 3 to i8
  invoke void @Main__WaitForState(i8 %t37, i8 1)
          to label %invoke.cont.39 unwind label %lpad.1, !dbg !260
invoke.cont.39:
  invoke void @Thread__Pause(double 0x4008000000000000)
          to label %invoke.cont.40 unwind label %lpad.1, !dbg !261
invoke.cont.40:
  %t38 = trunc i64 4 to i8
  invoke void @Main__WaitForStateSet(i8 %t38, i8 34)
          to label %invoke.cont.41 unwind label %lpad.1, !dbg !262
invoke.cont.41:
  %t39 = trunc i64 4 to i8
  %t40 = invoke i1 @Main__NoteWhetherState(i8 %t39, i8 1, ptr getelementptr inbounds (i8, ptr @textlit_27, i64 8), ptr getelementptr inbounds (i8, ptr @textlit_27, i64 8))
          to label %invoke.cont.42 unwind label %lpad.1, !dbg !263
invoke.cont.42:
  br i1 %t40, label %if.then.43, label %if.next.44
if.then.43:
  %t41 = trunc i64 4 to i8
  invoke void @Main__Action(i8 %t41, ptr @Main__DoRel)
          to label %invoke.cont.45 unwind label %lpad.1, !dbg !264
if.next.44:
  invoke void @Main__W(ptr getelementptr inbounds (i8, ptr @textlit_31, i64 8))
          to label %invoke.cont.49 unwind label %lpad.1, !dbg !265
invoke.cont.45:
  %t42 = trunc i64 4 to i8
  invoke void @Main__WaitForState(i8 %t42, i8 1)
          to label %invoke.cont.46 unwind label %lpad.1, !dbg !266
invoke.cont.46:
  invoke void @Main__W(ptr getelementptr inbounds (i8, ptr @textlit_30, i64 8))
          to label %invoke.cont.47 unwind label %lpad.1, !dbg !267
invoke.cont.47:
  br label %if.merge.48
if.merge.48:
  br label %try.merge.2
invoke.cont.49:
  invoke void @RTHooks__Raise(ptr @Main__Failure_excptr, ptr null, ptr null, i64 0)
          to label %invoke.cont.50 unwind label %lpad.1, !dbg !228
invoke.cont.50:
  unreachable
h.body.51:
  %t50 = call ptr @__cxa_begin_catch(ptr %t44), !dbg !228
  call void @__cxa_end_catch(), !dbg !228
  br label %try.merge.2
exc.next.52:
  resume { ptr, i32 } %t43
}

define void @Main__Init() personality ptr @__gxx_personality_v0 !dbg !50 {
entry:
  %t13 = alloca i64
  %LThNo.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %LThNo.slot, metadata !96, metadata !DIExpression()), !dbg !268
  %t1 = load ptr, ptr @tl_obj_356643957
  %t2 = call ptr @RTHooks__AllocateTracedObj(ptr %t1), !dbg !269
  %t3 = bitcast ptr %t2 to ptr
  store ptr %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
  %t4 = load ptr, ptr @tl_obj_356643957
  %t5 = call ptr @RTHooks__AllocateTracedObj(ptr %t4), !dbg !270
  %t6 = bitcast ptr %t5 to ptr
  store ptr %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t7 = load ptr, ptr @tl_obj_356643957
  %t8 = call ptr @RTHooks__AllocateTracedObj(ptr %t7), !dbg !271
  %t9 = bitcast ptr %t8 to ptr
  store ptr %t9, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %t10 = load ptr, ptr @tl_obj_1475387830
  %t11 = call ptr @RTHooks__AllocateTracedObj(ptr %t10), !dbg !272
  %t12 = bitcast ptr %t11 to ptr
  store ptr %t12, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  store i64 0, ptr %LThNo.slot
  store i64 4, ptr %t13
  br label %for.header.1
for.header.1:
  %t14 = load i64, ptr %LThNo.slot
  %t15 = load i64, ptr %t13
  %t16 = icmp sle i64 %t14, %t15
  br i1 %t16, label %for.body.2, label %for.exit.3
for.body.2:
  %t17 = load i64, ptr %LThNo.slot
  %t18 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384), i64 0, i64 %t17
  %t19 = load ptr, ptr @tl_obj_1940822287
  %t20 = call ptr @RTHooks__AllocateTracedObj(ptr %t19), !dbg !273
  %t21 = bitcast ptr %t20 to ptr
  %t22 = load i64, ptr %LThNo.slot
  %t23 = getelementptr i8, ptr %t21, i64 8
  store i64 %t22, ptr %t23
  store ptr %t21, ptr %t18
  %t24 = load i64, ptr %LThNo.slot
  %t25 = getelementptr inbounds [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 0, i64 %t24
  store i8 1, ptr %t25
  %t26 = load i64, ptr %LThNo.slot
  %t27 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192), i64 0, i64 %t26
  %t28 = load i64, ptr %LThNo.slot
  %t29 = getelementptr inbounds [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384), i64 0, i64 %t28
  %t30 = load ptr, ptr %t29
  %t31 = call ptr @Thread__Fork(ptr %t30), !dbg !275
  store ptr %t31, ptr %t27
  %t32 = load i64, ptr %LThNo.slot
  %t33 = add i64 %t32, 1
  store i64 %t33, ptr %LThNo.slot
  br label %for.header.1
for.exit.3:
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__Init()
  call void @Main__TestSeq()
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
@textlit_0 = internal constant { i64, ptr, i64, [2 x i8] } { i64 2, ptr @textlit_methods, i64 1, [2 x i8] c"\0a\00" }
@textlit_1 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"Thread \00" }
@textlit_2 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c"null\00" }
@textlit_3 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c"idle\00" }
@textlit_4 = internal constant { i64, ptr, i64, [17 x i8] } { i64 2, ptr @textlit_methods, i64 16, [17 x i8] c"entering Acquire\00" }
@textlit_5 = internal constant { i64, ptr, i64, [17 x i8] } { i64 2, ptr @textlit_methods, i64 16, [17 x i8] c"entering Release\00" }
@textlit_6 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"entering Wait\00" }
@textlit_7 = internal constant { i64, ptr, i64, [15 x i8] } { i64 2, ptr @textlit_methods, i64 14, [15 x i8] c"asleep in Wait\00" }
@textlit_8 = internal constant { i64, ptr, i64, [16 x i8] } { i64 2, ptr @textlit_methods, i64 15, [16 x i8] c"entering Signal\00" }
@textlit_9 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"WAct = NIL \00" }
@textlit_10 = internal constant { i64, ptr, i64, [25 x i8] } { i64 2, ptr @textlit_methods, i64 24, [25 x i8] c" Failed to wait in Wait.\00" }
@textlit_11 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"Holder = ThN \00" }
@textlit_12 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c" Waiting.\00" }
@textlit_13 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c" is \00" }
@textlit_14 = internal constant { i64, ptr, i64, [32 x i8] } { i64 2, ptr @textlit_methods, i64 31, [32 x i8] c" Entering Acquire of TestMutex.\00" }
@textlit_15 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c" Acquired TestMutex.\00" }
@textlit_16 = internal constant { i64, ptr, i64, [20 x i8] } { i64 2, ptr @textlit_methods, i64 19, [20 x i8] c"WThN = State . Acq \00" }
@textlit_17 = internal constant { i64, ptr, i64, [23 x i8] } { i64 2, ptr @textlit_methods, i64 22, [23 x i8] c"Holder = ThreadNoNull \00" }
@textlit_18 = internal constant { i64, ptr, i64, [32 x i8] } { i64 2, ptr @textlit_methods, i64 31, [32 x i8] c" Entering release of TestMutex.\00" }
@textlit_19 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c" Released TestMutex.\00" }
@textlit_20 = internal constant { i64, ptr, i64, [20 x i8] } { i64 2, ptr @textlit_methods, i64 19, [20 x i8] c"WThN = State . Rel \00" }
@textlit_21 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c"WThN = State . Idle \00" }
@textlit_22 = internal constant { i64, ptr, i64, [28 x i8] } { i64 2, ptr @textlit_methods, i64 27, [28 x i8] c" Entering Wait on TestCond.\00" }
@textlit_23 = internal constant { i64, ptr, i64, [53 x i8] } { i64 2, ptr @textlit_methods, i64 52, [53 x i8] c" Was Signalled in TestCond and reacquired TestMutex.\00" }
@textlit_24 = internal constant { i64, ptr, i64, [22 x i8] } { i64 2, ptr @textlit_methods, i64 21, [22 x i8] c"WThN = State . Wait2 \00" }
@textlit_25 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c" Entering Signal on TestCond\00" }
@textlit_26 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c" TestCond Signalled \00" }
@textlit_27 = internal constant { i64, ptr, i64, [1 x i8] } { i64 2, ptr @textlit_methods, i64 0, [1 x i8] c"\00" }
@textlit_28 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"LThN = ThN \00" }
@textlit_29 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"LThNo = 2 \00" }
@textlit_30 = internal constant { i64, ptr, i64, [26 x i8] } { i64 2, ptr @textlit_methods, i64 25, [26 x i8] c"SUCCESS: all as expected.\00" }
@textlit_31 = internal constant { i64, ptr, i64, [65 x i8] } { i64 2, ptr @textlit_methods, i64 64, [65 x i8] c"FAILURE: This is the \22twice used ticket\22 bug we are testing for.\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_obj_1940822287.tc_name = private unnamed_addr constant [8 x i8] c"Main.Cl\00"
@tc_obj_1940822287.methods = internal constant [1 x ptr] [ptr @Main__TestApply]
@tc_obj_1940822287 = internal global %OTC_t {
  i64 0,
  i64 1940822287,
  i64 u0x0cff159a7f518c95,
  i8 1,
  i8 2,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 8,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_obj_1940822287.tc_name,
  ptr null,
  i64 -448425059,
  ptr null,
  i64 0,
  i64 0,
  i64 0,
  ptr @tc_obj_1940822287.methods,
  ptr null
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_obj_356643957 = internal global %TypeLink_t {
  ptr null,
  i64 356643957
}
@tl_obj_1475387830 = internal global %TypeLink_t {
  ptr @tl_obj_356643957,
  i64 1475387830
}
@tl_obj_1940822287 = internal global %TypeLink_t {
  ptr @tl_obj_1475387830,
  i64 1940822287
}

@Main__Failure_excptr = internal global { i64, ptr, i64 } { i64 -1505293580, ptr null, i64 0 }

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Wr_I3(i64)
declare ptr @Thread_I3(i64)
declare ptr @Stdio_I3(i64)
declare ptr @Fmt_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Wr_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Thread_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Stdio_I3, ptr @Main_M3_imp.4 }
@Main_M3_imp.4 = internal global { ptr, ptr, ptr } { ptr null, ptr @Fmt_I3, ptr null }
@Main_M3_gc_map = internal constant [11 x i8] c"\2a\68\04\2a\10\04\2a\28\04\04\00"

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [320 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_obj_1940822287,  ; type_cells (+8)
  ptr @tl_obj_1940822287,  ; type_cell_ptrs (+16)
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
  [320 x i8] zeroinitializer  ; user globals (320 bytes)
}
@Main__StateMutex = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
@Main__States = alias [5 x i8], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112)
@Main__Holder = alias i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 117)
@Main__ActionMutex = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
@Main__ActionProcs = alias [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136)
@Main__TestMutex = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
@Main__TestCond = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
@Main__Threads = alias [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192)
@Main__Closures = alias [5 x ptr], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)

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
!16 = distinct !DISubprogram(name: "Main__W", linkageName: "Main__W", scope: !4, file: !3, line: 16, type: !6, scopeLine: 16, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__ThImage", linkageName: "Main__ThImage", scope: !4, file: !3, line: 29, type: !6, scopeLine: 29, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__StateImage", linkageName: "Main__StateImage", scope: !4, file: !3, line: 52, type: !6, scopeLine: 52, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__Action", linkageName: "Main__Action", scope: !4, file: !3, line: 76, type: !6, scopeLine: 76, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Main__ActionWait", linkageName: "Main__ActionWait", scope: !4, file: !3, line: 95, type: !6, scopeLine: 95, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Main__WaitForHeld", linkageName: "Main__WaitForHeld", scope: !4, file: !3, line: 127, type: !6, scopeLine: 127, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Main__WaitForStateSet", linkageName: "Main__WaitForStateSet", scope: !4, file: !3, line: 141, type: !6, scopeLine: 141, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Main__WaitForState", linkageName: "Main__WaitForState", scope: !4, file: !3, line: 157, type: !6, scopeLine: 157, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Main__NoteWhetherStateSet", linkageName: "Main__NoteWhetherStateSet", scope: !4, file: !3, line: 164, type: !6, scopeLine: 164, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Main__NoteWhetherState", linkageName: "Main__NoteWhetherState", scope: !4, file: !3, line: 187, type: !6, scopeLine: 187, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Main__TestApply", linkageName: "Main__TestApply", scope: !4, file: !3, line: 205, type: !6, scopeLine: 205, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Main__DoAcq", linkageName: "Main__DoAcq", scope: !4, file: !3, line: 233, type: !6, scopeLine: 233, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "Main__DoRel", linkageName: "Main__DoRel", scope: !4, file: !3, line: 250, type: !6, scopeLine: 250, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "Main__DoWait", linkageName: "Main__DoWait", scope: !4, file: !3, line: 267, type: !6, scopeLine: 267, unit: !2, spFlags: DISPFlagDefinition)
!44 = distinct !DISubprogram(name: "Main__DoSignal", linkageName: "Main__DoSignal", scope: !4, file: !3, line: 292, type: !6, scopeLine: 292, unit: !2, spFlags: DISPFlagDefinition)
!46 = distinct !DISubprogram(name: "Main__ForceSignalled", linkageName: "Main__ForceSignalled", scope: !4, file: !3, line: 301, type: !6, scopeLine: 301, unit: !2, spFlags: DISPFlagDefinition)
!48 = distinct !DISubprogram(name: "Main__TestSeq", linkageName: "Main__TestSeq", scope: !4, file: !3, line: 322, type: !6, scopeLine: 322, unit: !2, spFlags: DISPFlagDefinition)
!50 = distinct !DISubprogram(name: "Main__Init", linkageName: "Main__Init", scope: !4, file: !3, line: 412, type: !6, scopeLine: 412, unit: !2, spFlags: DISPFlagDefinition)
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
!41 = !DILocation(line: 0, column: 0, scope: !40)
!43 = !DILocation(line: 0, column: 0, scope: !42)
!45 = !DILocation(line: 0, column: 0, scope: !44)
!47 = !DILocation(line: 0, column: 0, scope: !46)
!49 = !DILocation(line: 0, column: 0, scope: !48)
!51 = !DILocation(line: 0, column: 0, scope: !50)
!52 = !DILocalVariable(name: "Msg", scope: !16, file: !3, line: 16, type: !15)
!53 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 29, type: !15)
!54 = !DILocalVariable(name: "ThN", scope: !18, file: !3, line: 29, type: !7)
!55 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 52, type: !15)
!56 = !DILocalVariable(name: "LResult", scope: !20, file: !3, line: 52, type: !15)
!57 = !DILocalVariable(name: "St", scope: !20, file: !3, line: 52, type: !7)
!58 = !DILocalVariable(name: "Apply", scope: !22, file: !3, line: 76, type: !15)
!59 = !DILocalVariable(name: "ThN", scope: !22, file: !3, line: 76, type: !7)
!60 = !DILocalVariable(name: "Apply", scope: !24, file: !3, line: 95, type: !15)
!61 = !DILocalVariable(name: "ThN", scope: !24, file: !3, line: 95, type: !7)
!62 = !DILocalVariable(name: "_result", scope: !26, file: !3, line: 127, type: !7)
!63 = !DILocalVariable(name: "LHolder", scope: !26, file: !3, line: 127, type: !7)
!64 = !DILocalVariable(name: "Sts", scope: !28, file: !3, line: 141, type: !15)
!65 = !DILocalVariable(name: "ThN", scope: !28, file: !3, line: 141, type: !7)
!66 = !DILocalVariable(name: "St", scope: !30, file: !3, line: 157, type: !7)
!67 = !DILocalVariable(name: "ThN", scope: !30, file: !3, line: 157, type: !7)
!68 = !DILocalVariable(name: "_result", scope: !32, file: !3, line: 164, type: !7)
!69 = !DILocalVariable(name: "LMsg", scope: !32, file: !3, line: 164, type: !15)
!70 = !DILocalVariable(name: "LState", scope: !32, file: !3, line: 164, type: !7)
!71 = !DILocalVariable(name: "LResult", scope: !32, file: !3, line: 164, type: !7)
!72 = !DILocalVariable(name: "NoMsg", scope: !32, file: !3, line: 164, type: !15)
!73 = !DILocalVariable(name: "YesMsg", scope: !32, file: !3, line: 164, type: !15)
!74 = !DILocalVariable(name: "Sts", scope: !32, file: !3, line: 164, type: !15)
!75 = !DILocalVariable(name: "ThN", scope: !32, file: !3, line: 164, type: !7)
!76 = !DILocalVariable(name: "_result", scope: !34, file: !3, line: 187, type: !7)
!77 = !DILocalVariable(name: "NoMsg", scope: !34, file: !3, line: 187, type: !15)
!78 = !DILocalVariable(name: "YesMsg", scope: !34, file: !3, line: 187, type: !15)
!79 = !DILocalVariable(name: "St", scope: !34, file: !3, line: 187, type: !7)
!80 = !DILocalVariable(name: "ThN", scope: !34, file: !3, line: 187, type: !7)
!81 = !DILocalVariable(name: "_result", scope: !36, file: !3, line: 205, type: !15)
!82 = !DILocalVariable(name: "LProc", scope: !36, file: !3, line: 205, type: !15)
!88 = !DILocalVariable(name: "Self", scope: !36, file: !3, line: 205, type: !87)
!89 = !DILocalVariable(name: "ThN", scope: !38, file: !3, line: 233, type: !7)
!90 = !DILocalVariable(name: "ThN", scope: !40, file: !3, line: 250, type: !7)
!91 = !DILocalVariable(name: "ThN", scope: !42, file: !3, line: 267, type: !7)
!92 = !DILocalVariable(name: "ThN", scope: !44, file: !3, line: 292, type: !7)
!93 = !DILocalVariable(name: "LThN", scope: !46, file: !3, line: 301, type: !7)
!94 = !DILocalVariable(name: "ThN", scope: !46, file: !3, line: 301, type: !7)
!95 = !DILocalVariable(name: "LThNo", scope: !48, file: !3, line: 322, type: !7)
!96 = !DILocalVariable(name: "LThNo", scope: !50, file: !3, line: 412, type: !7)
!83 = !DICompositeType(tag: DW_TAG_structure_type, name: "Cl", size: 72, elements: !84)
!84 = !{!85, !86}
!85 = !DIDerivedType(tag: DW_TAG_member, name: "__vtable", baseType: !15, size: 64, offset: 0)
!86 = !DIDerivedType(tag: DW_TAG_member, name: "ClThN", baseType: !14, size: 8, offset: 64)
!87 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !83, size: 64)
!97 = !DILocation(line: 16, column: 0, scope: !16)
!98 = !DILocation(line: 21, column: 0, scope: !16)
!99 = !DILocation(line: 22, column: 0, scope: !16)
!100 = !DILocation(line: 23, column: 0, scope: !16)
!101 = !DILocation(line: 29, column: 0, scope: !18)
!102 = !DILocation(line: 31, column: 0, scope: !18)
!103 = !DILocation(line: 52, column: 0, scope: !20)
!104 = !DILocation(line: 57, column: 0, scope: !20)
!105 = !DILocation(line: 58, column: 0, scope: !20)
!106 = !DILocation(line: 59, column: 0, scope: !20)
!107 = !DILocation(line: 60, column: 0, scope: !20)
!108 = !DILocation(line: 61, column: 0, scope: !20)
!109 = !DILocation(line: 62, column: 0, scope: !20)
!110 = !DILocation(line: 63, column: 0, scope: !20)
!111 = !DILocation(line: 64, column: 0, scope: !20)
!112 = !DILocation(line: 66, column: 0, scope: !20)
!113 = !DILocation(line: 89, column: 0, scope: !22)
!114 = !DILocation(line: 80, column: 0, scope: !22)
!115 = !DILocation(line: 76, column: 0, scope: !22)
!116 = !DILocation(line: 81, column: 0, scope: !22)
!117 = !DILocation(line: 83, column: 0, scope: !22)
!118 = !DILocation(line: 84, column: 0, scope: !22)
!119 = !DILocation(line: 87, column: 0, scope: !22)
!120 = !DILocation(line: 88, column: 0, scope: !22)
!121 = !DILocation(line: 90, column: 0, scope: !22)
!122 = !DILocation(line: 108, column: 0, scope: !24)
!123 = !DILocation(line: 107, column: 0, scope: !24)
!124 = !DILocation(line: 95, column: 0, scope: !24)
!125 = !DILocation(line: 103, column: 0, scope: !24)
!126 = !DILocation(line: 104, column: 0, scope: !24)
!127 = !DILocation(line: 119, column: 0, scope: !24)
!128 = !DILocation(line: 109, column: 0, scope: !24)
!129 = !DILocation(line: 110, column: 0, scope: !24)
!130 = !DILocation(line: 112, column: 0, scope: !24)
!131 = !DILocation(line: 115, column: 0, scope: !24)
!132 = !DILocation(line: 113, column: 0, scope: !24)
!133 = !DILocation(line: 116, column: 0, scope: !24)
!134 = !DILocation(line: 117, column: 0, scope: !24)
!135 = !DILocation(line: 118, column: 0, scope: !24)
!136 = !DILocation(line: 135, column: 0, scope: !26)
!137 = !DILocation(line: 127, column: 0, scope: !26)
!138 = !DILocation(line: 133, column: 0, scope: !26)
!139 = !DILocation(line: 134, column: 0, scope: !26)
!140 = !DILocation(line: 138, column: 0, scope: !26)
!141 = !DILocation(line: 136, column: 0, scope: !26)
!142 = !DILocation(line: 145, column: 0, scope: !28)
!143 = !DILocation(line: 141, column: 0, scope: !28)
!144 = !DILocation(line: 144, column: 0, scope: !28)
!145 = !DILocation(line: 146, column: 0, scope: !28)
!146 = !DILocation(line: 147, column: 0, scope: !28)
!147 = !DILocation(line: 153, column: 0, scope: !28)
!148 = !DILocation(line: 149, column: 0, scope: !28)
!149 = !DILocation(line: 157, column: 0, scope: !30)
!150 = !DILocation(line: 160, column: 0, scope: !30)
!151 = !DILocation(line: 172, column: 0, scope: !32)
!152 = !DILocation(line: 164, column: 0, scope: !32)
!153 = !DILocation(line: 174, column: 0, scope: !32)
!154 = !DILocation(line: 175, column: 0, scope: !32)
!155 = !DILocation(line: 176, column: 0, scope: !32)
!156 = !DILocation(line: 181, column: 0, scope: !32)
!157 = !DILocation(line: 183, column: 0, scope: !32)
!158 = !DILocation(line: 177, column: 0, scope: !32)
!159 = !DILocation(line: 178, column: 0, scope: !32)
!160 = !DILocation(line: 179, column: 0, scope: !32)
!161 = !DILocation(line: 187, column: 0, scope: !34)
!162 = !DILocation(line: 190, column: 0, scope: !34)
!163 = !DILocation(line: 215, column: 0, scope: !36)
!164 = !DILocation(line: 205, column: 0, scope: !36)
!165 = !DILocation(line: 209, column: 0, scope: !36)
!166 = !DILocation(line: 210, column: 0, scope: !36)
!167 = !DILocation(line: 216, column: 0, scope: !36)
!168 = !DILocation(line: 217, column: 0, scope: !36)
!169 = !DILocation(line: 230, column: 0, scope: !36)
!170 = !DILocation(line: 220, column: 0, scope: !36)
!171 = !DILocation(line: 224, column: 0, scope: !36)
!172 = !DILocation(line: 219, column: 0, scope: !36)
!173 = !DILocation(line: 225, column: 0, scope: !36)
!174 = !DILocation(line: 227, column: 0, scope: !36)
!175 = !DILocation(line: 239, column: 0, scope: !38)
!176 = !DILocation(line: 235, column: 0, scope: !38)
!177 = !DILocation(line: 233, column: 0, scope: !38)
!178 = !DILocation(line: 236, column: 0, scope: !38)
!179 = !DILocation(line: 237, column: 0, scope: !38)
!180 = !DILocation(line: 238, column: 0, scope: !38)
!181 = !DILocation(line: 240, column: 0, scope: !38)
!182 = !DILocation(line: 242, column: 0, scope: !38)
!183 = !DILocation(line: 245, column: 0, scope: !38)
!184 = !DILocation(line: 243, column: 0, scope: !38)
!185 = !DILocation(line: 244, column: 0, scope: !38)
!186 = !DILocation(line: 256, column: 0, scope: !40)
!187 = !DILocation(line: 252, column: 0, scope: !40)
!188 = !DILocation(line: 250, column: 0, scope: !40)
!189 = !DILocation(line: 253, column: 0, scope: !40)
!190 = !DILocation(line: 254, column: 0, scope: !40)
!191 = !DILocation(line: 255, column: 0, scope: !40)
!192 = !DILocation(line: 257, column: 0, scope: !40)
!193 = !DILocation(line: 259, column: 0, scope: !40)
!194 = !DILocation(line: 262, column: 0, scope: !40)
!195 = !DILocation(line: 260, column: 0, scope: !40)
!196 = !DILocation(line: 261, column: 0, scope: !40)
!197 = !DILocation(line: 282, column: 0, scope: !42)
!198 = !DILocation(line: 269, column: 0, scope: !42)
!199 = !DILocation(line: 267, column: 0, scope: !42)
!200 = !DILocation(line: 270, column: 0, scope: !42)
!201 = !DILocation(line: 272, column: 0, scope: !42)
!202 = !DILocation(line: 274, column: 0, scope: !42)
!203 = !DILocation(line: 277, column: 0, scope: !42)
!204 = !DILocation(line: 278, column: 0, scope: !42)
!205 = !DILocation(line: 279, column: 0, scope: !42)
!206 = !DILocation(line: 283, column: 0, scope: !42)
!207 = !DILocation(line: 285, column: 0, scope: !42)
!208 = !DILocation(line: 273, column: 0, scope: !42)
!209 = !DILocation(line: 287, column: 0, scope: !42)
!210 = !DILocation(line: 286, column: 0, scope: !42)
!211 = !DILocation(line: 298, column: 0, scope: !44)
!212 = !DILocation(line: 294, column: 0, scope: !44)
!213 = !DILocation(line: 292, column: 0, scope: !44)
!214 = !DILocation(line: 295, column: 0, scope: !44)
!215 = !DILocation(line: 296, column: 0, scope: !44)
!216 = !DILocation(line: 297, column: 0, scope: !44)
!217 = !DILocation(line: 301, column: 0, scope: !46)
!218 = !DILocation(line: 310, column: 0, scope: !46)
!219 = !DILocation(line: 311, column: 0, scope: !46)
!220 = !DILocation(line: 313, column: 0, scope: !46)
!221 = !DILocation(line: 314, column: 0, scope: !46)
!222 = !DILocation(line: 315, column: 0, scope: !46)
!223 = !DILocation(line: 316, column: 0, scope: !46)
!224 = !DILocation(line: 318, column: 0, scope: !46)
!225 = !DILocation(line: 319, column: 0, scope: !46)
!226 = !DILocation(line: 322, column: 0, scope: !48)
!227 = !DILocation(line: 329, column: 0, scope: !48)
!228 = !DILocation(line: 405, column: 0, scope: !48)
!229 = !DILocation(line: 330, column: 0, scope: !48)
!230 = !DILocation(line: 331, column: 0, scope: !48)
!231 = !DILocation(line: 332, column: 0, scope: !48)
!232 = !DILocation(line: 335, column: 0, scope: !48)
!233 = !DILocation(line: 336, column: 0, scope: !48)
!234 = !DILocation(line: 337, column: 0, scope: !48)
!235 = !DILocation(line: 338, column: 0, scope: !48)
!236 = !DILocation(line: 351, column: 0, scope: !48)
!237 = !DILocation(line: 352, column: 0, scope: !48)
!238 = !DILocation(line: 356, column: 0, scope: !48)
!239 = !DILocation(line: 357, column: 0, scope: !48)
!240 = !DILocation(line: 361, column: 0, scope: !48)
!241 = !DILocation(line: 362, column: 0, scope: !48)
!242 = !DILocation(line: 365, column: 0, scope: !48)
!243 = !DILocation(line: 366, column: 0, scope: !48)
!244 = !DILocation(line: 369, column: 0, scope: !48)
!245 = !DILocation(line: 373, column: 0, scope: !48)
!246 = !DILocation(line: 370, column: 0, scope: !48)
!247 = !DILocation(line: 371, column: 0, scope: !48)
!248 = !DILocation(line: 380, column: 0, scope: !48)
!249 = !DILocation(line: 374, column: 0, scope: !48)
!250 = !DILocation(line: 375, column: 0, scope: !48)
!251 = !DILocation(line: 376, column: 0, scope: !48)
!252 = !DILocation(line: 381, column: 0, scope: !48)
!253 = !DILocation(line: 382, column: 0, scope: !48)
!254 = !DILocation(line: 383, column: 0, scope: !48)
!255 = !DILocation(line: 386, column: 0, scope: !48)
!256 = !DILocation(line: 387, column: 0, scope: !48)
!257 = !DILocation(line: 388, column: 0, scope: !48)
!258 = !DILocation(line: 389, column: 0, scope: !48)
!259 = !DILocation(line: 390, column: 0, scope: !48)
!260 = !DILocation(line: 391, column: 0, scope: !48)
!261 = !DILocation(line: 394, column: 0, scope: !48)
!262 = !DILocation(line: 397, column: 0, scope: !48)
!263 = !DILocation(line: 398, column: 0, scope: !48)
!264 = !DILocation(line: 400, column: 0, scope: !48)
!265 = !DILocation(line: 404, column: 0, scope: !48)
!266 = !DILocation(line: 401, column: 0, scope: !48)
!267 = !DILocation(line: 402, column: 0, scope: !48)
!268 = !DILocation(line: 418, column: 0, scope: !50)
!269 = !DILocation(line: 414, column: 0, scope: !50)
!270 = !DILocation(line: 415, column: 0, scope: !50)
!271 = !DILocation(line: 416, column: 0, scope: !50)
!272 = !DILocation(line: 417, column: 0, scope: !50)
!273 = !DILocation(line: 420, column: 0, scope: !50)
!274 = !DILocation(line: 421, column: 0, scope: !50)
!275 = !DILocation(line: 422, column: 0, scope: !50)
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
