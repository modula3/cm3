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


declare ptr @RTHooks__Concat(ptr, ptr)
declare void @IO__Put(ptr, ptr)
declare ptr @__cxa_get_exception_ptr(ptr)
declare void @RTHooks__ResumeRaise(ptr)
declare void @Thread__Release(ptr)
declare void @Thread__Pause(double)
declare void @Thread__Acquire(ptr)
declare ptr @RTHooks__AllocateOpenArray(ptr, ptr)
declare ptr @Fmt__Int(i64, i8)
declare ptr @RTHooks__AllocateTracedObj(ptr)
declare ptr @Thread__Fork(ptr)
declare ptr @Thread__Join(ptr)
declare void @Process__Exit(i32)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__Print(ptr %a.msg) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %msg.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %msg.slot, metadata !22, metadata !DIExpression()), !dbg !45
  store ptr %a.msg, ptr %msg.slot
  %t1 = load ptr, ptr %msg.slot
  %t2 = call ptr @RTHooks__Concat(ptr %t1, ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !46
  call void @IO__Put(ptr %t2, ptr null), !dbg !46
  ret void
}

define ptr @Main__ChildApply(ptr %a.self) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %t23 = alloca ptr
  %t22 = alloca { ptr, i32 }
  %t21 = alloca ptr
  %t20 = alloca i32
  %t7 = alloca ptr
  %t6 = alloca i32
  %t5 = alloca ptr
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !27, metadata !DIExpression()), !dbg !49
  %numLoops.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %numLoops.slot, metadata !28, metadata !DIExpression()), !dbg !49
  %self.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %self.slot, metadata !34, metadata !DIExpression()), !dbg !49
  store ptr %a.self, ptr %self.slot
  store i64 0, ptr %numLoops.slot
  store ptr null, ptr %_result.slot
  store i64 0, ptr %numLoops.slot
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)
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
  call void @RTHooks__CheckLoadTracedRef(ptr %t1), !dbg !48
  br label %gc.skip.1
gc.skip.1:
  %t2 = bitcast ptr %t1 to ptr
  %t3 = load ptr, ptr %t2
  %t4 = load ptr, ptr %t3
  call void %t4(ptr %t1), !dbg !48
  store i32 0, ptr %t6
  br label %while.header.6
lock.lpad.1:
  %t34 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t35 = extractvalue { ptr, i32 } %t34, 0
  %t36 = call ptr @__cxa_get_exception_ptr(ptr %t35), !dbg !51
  %t37 = load ptr, ptr %t36
  store ptr %t37, ptr %t5
  store i32 1, ptr %t6
  br label %lock.fin.2
lock.fin.2:
  %t38 = bitcast ptr %t1 to ptr
  %t39 = load ptr, ptr %t38
  %t40 = getelementptr i8, ptr %t39, i64 8
  %t41 = load ptr, ptr %t40
  call void %t41(ptr %t1), !dbg !51
  %t42 = load i32, ptr %t6
  %t43 = icmp eq i32 %t42, 1
  br i1 %t43, label %lock.resume.3, label %lock.fin.notexc.18
lock.resume.3:
  %t44 = load ptr, ptr %t5
  call void @RTHooks__ResumeRaise(ptr %t44), !dbg !51
  unreachable
lock.ret.4:
  unreachable
lock.done.5:
  ret ptr null
while.header.6:
  %t8 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 408)
  %t9 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t10 = icmp slt i64 %t8, %t9
  br i1 %t10, label %while.body.7, label %while.exit.8
while.body.7:
  %t11 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 408)
  %t12 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 400)
  %__gc_nil.2 = icmp eq ptr %t12, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t12 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t12, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t12), !dbg !53
  br label %gc.skip.2
gc.skip.2:
  %t13 = load { ptr, i64 }, ptr %t12
  %__ll3 = extractvalue { ptr, i64 } %t13, 0
  %t14 = getelementptr inbounds i64, ptr %__ll3, i64 %t11
  %t15 = load i64, ptr %t14
  %t16 = add i64 %t15, 1
  store i64 %t16, ptr %t14
  %t17 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 408)
  %t18 = add i64 %t17, 1
  store i64 %t18, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 408)
  %t19 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)
  %__gc_nil.4 = icmp eq ptr %t19, null
  br i1 %__gc_nil.4, label %gc.skip.4, label %gc.check.4
gc.check.4:
  %__gc_int.4 = ptrtoint ptr %t19 to i64
  %__gc_low.4 = and i64 %__gc_int.4, 1
  %__gc_ma.4 = icmp ne i64 %__gc_low.4, 0
  br i1 %__gc_ma.4, label %gc.skip.4, label %gc.gray.4
gc.gray.4:
  %__gc_hptr.4 = getelementptr i8, ptr %t19, i64 -8
  %__gc_hdr.4 = load i64, ptr %__gc_hptr.4
  %__gc_gb.4 = and i64 %__gc_hdr.4, 4194304
  %__gc_gr.4 = icmp ne i64 %__gc_gb.4, 0
  br i1 %__gc_gr.4, label %gc.slow.4, label %gc.skip.4
gc.slow.4:
  call void @RTHooks__CheckLoadTracedRef(ptr %t19), !dbg !55
  br label %gc.skip.4
gc.skip.4:
  invoke void @Thread__Release(ptr %t19)
          to label %invoke.cont.9 unwind label %lock.lpad.1, !dbg !55
while.exit.8:
  br label %lock.fin.2
invoke.cont.9:
  store i32 0, ptr %t20
  %t24 = load i64, ptr %numLoops.slot
  %t25 = add i64 %t24, 1
  store i64 %t25, ptr %numLoops.slot
  invoke void @Thread__Pause(double 0x3fbc28f5c28f5c29)
          to label %invoke.cont.14 unwind label %fin.lpad.10, !dbg !57
fin.lpad.10:
  %t26 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t27 = extractvalue { ptr, i32 } %t26, 0
  %t28 = call ptr @__cxa_get_exception_ptr(ptr %t27), !dbg !57
  %t29 = load ptr, ptr %t28
  store ptr %t29, ptr %t21
  store i32 1, ptr %t20
  br label %fin.body.11
fin.body.11:
  %t30 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)
  %__gc_nil.5 = icmp eq ptr %t30, null
  br i1 %__gc_nil.5, label %gc.skip.5, label %gc.check.5
gc.check.5:
  %__gc_int.5 = ptrtoint ptr %t30 to i64
  %__gc_low.5 = and i64 %__gc_int.5, 1
  %__gc_ma.5 = icmp ne i64 %__gc_low.5, 0
  br i1 %__gc_ma.5, label %gc.skip.5, label %gc.gray.5
gc.gray.5:
  %__gc_hptr.5 = getelementptr i8, ptr %t30, i64 -8
  %__gc_hdr.5 = load i64, ptr %__gc_hptr.5
  %__gc_gb.5 = and i64 %__gc_hdr.5, 4194304
  %__gc_gr.5 = icmp ne i64 %__gc_gb.5, 0
  br i1 %__gc_gr.5, label %gc.slow.5, label %gc.skip.5
gc.slow.5:
  call void @RTHooks__CheckLoadTracedRef(ptr %t30), !dbg !51
  br label %gc.skip.5
gc.skip.5:
  invoke void @Thread__Acquire(ptr %t30)
          to label %invoke.cont.15 unwind label %lock.lpad.1, !dbg !51
fin.rethrow.12:
  %t33 = load ptr, ptr %t21
  invoke void @RTHooks__ResumeRaise(ptr %t33)
          to label %fin.rethrow.cont.17 unwind label %lock.lpad.1, !dbg !51
fin.done.13:
  br label %while.header.6
invoke.cont.14:
  br label %fin.body.11
invoke.cont.15:
  %t31 = load i32, ptr %t20
  %t32 = icmp eq i32 %t31, 1
  br i1 %t32, label %fin.rethrow.12, label %fin.notexc.16
fin.notexc.16:
  br label %fin.done.13
fin.rethrow.cont.17:
  unreachable
lock.fin.notexc.18:
  br label %lock.done.5
}

define void @Main__CheckResults() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %t88 = alloca i1
  %t81 = alloca i1
  %t61 = alloca i64
  %n.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %n.slot, metadata !35, metadata !DIExpression()), !dbg !60
  %t25 = alloca i64
  %i.slot.1 = alloca i64
  call void @llvm.dbg.declare(metadata ptr %i.slot.1, metadata !36, metadata !DIExpression()), !dbg !61
  %t13 = alloca i64
  %i.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %i.slot, metadata !37, metadata !DIExpression()), !dbg !62
  %t2 = alloca { ptr, i64, i64 }
  %error.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %error.slot, metadata !43, metadata !DIExpression()), !dbg !64
  %count.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %count.slot, metadata !44, metadata !DIExpression()), !dbg !64
  store ptr null, ptr %count.slot
  store i64 0, ptr %error.slot
  store i1 0, ptr %error.slot
  %t1 = load ptr, ptr @tl_arr_-476616643
  %t3 = getelementptr i8, ptr %t2, i64 16
  %t4 = getelementptr i8, ptr %t2, i64 0
  store ptr %t3, ptr %t4
  %t5 = getelementptr i8, ptr %t2, i64 8
  store i64 1, ptr %t5
  %t6 = getelementptr i8, ptr %t2, i64 16
  %t7 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t8 = add i64 %t7, 1
  %t9 = bitcast i64 %t8 to i64
  store i64 %t9, ptr %t6
  %t10 = call ptr @RTHooks__AllocateOpenArray(ptr %t1, ptr %t2), !dbg !63
  %t11 = bitcast ptr %t10 to ptr
  store ptr %t11, ptr %count.slot
  store i64 0, ptr %i.slot
  %t12 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  store i64 %t12, ptr %t13
  br label %for.header.1
for.header.1:
  %t14 = load i64, ptr %i.slot
  %t15 = load i64, ptr %t13
  %t16 = icmp sle i64 %t14, %t15
  br i1 %t16, label %for.body.2, label %for.exit.3
for.body.2:
  %t17 = load i64, ptr %i.slot
  %t18 = load ptr, ptr %count.slot
  %t19 = load { ptr, i64 }, ptr %t18
  %__ll1 = extractvalue { ptr, i64 } %t19, 0
  %t20 = getelementptr inbounds i64, ptr %__ll1, i64 %t17
  store i64 0, ptr %t20
  %t21 = load i64, ptr %i.slot
  %t22 = add i64 %t21, 1
  store i64 %t22, ptr %i.slot
  br label %for.header.1
for.exit.3:
  store i64 0, ptr %i.slot.1
  %t23 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t24 = sub i64 %t23, 1
  store i64 %t24, ptr %t25
  br label %for.header.4
for.header.4:
  %t26 = load i64, ptr %i.slot.1
  %t27 = load i64, ptr %t25
  %t28 = icmp sle i64 %t26, %t27
  br i1 %t28, label %for.body.5, label %for.exit.6
for.body.5:
  %t29 = load i64, ptr %i.slot.1
  %t30 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 400)
  %__gc_nil.2 = icmp eq ptr %t30, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t30 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t30, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t30), !dbg !66
  br label %gc.skip.2
gc.skip.2:
  %t31 = load { ptr, i64 }, ptr %t30
  %__ll3 = extractvalue { ptr, i64 } %t31, 0
  %t32 = getelementptr inbounds i64, ptr %__ll3, i64 %t29
  %t33 = load i64, ptr %t32
  %t34 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t35 = icmp sgt i64 %t33, %t34
  br i1 %t35, label %if.then.7, label %if.next.8
for.exit.6:
  %t55 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t56 = trunc i64 10 to i8
  %t57 = call ptr @Fmt__Int(i64 %t55, i8 %t56), !dbg !68
  %t58 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_4, i64 8), ptr %t57), !dbg !68
  %t59 = call ptr @RTHooks__Concat(ptr %t58, ptr getelementptr inbounds (i8, ptr @textlit_5, i64 8)), !dbg !68
  call void @Main__Print(ptr %t59), !dbg !68
  store i64 0, ptr %n.slot
  %t60 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  store i64 %t60, ptr %t61
  br label %for.header.10
if.then.7:
  %t36 = zext i1 1 to i64
  store i64 %t36, ptr %error.slot
  %t37 = load i64, ptr %i.slot.1
  %t38 = trunc i64 10 to i8
  %t39 = call ptr @Fmt__Int(i64 %t37, i8 %t38), !dbg !70
  %t40 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8), ptr %t39), !dbg !70
  %t41 = call ptr @RTHooks__Concat(ptr %t40, ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8)), !dbg !70
  %t42 = load i64, ptr %t32
  %t43 = trunc i64 10 to i8
  %t44 = call ptr @Fmt__Int(i64 %t42, i8 %t43), !dbg !70
  %t45 = call ptr @RTHooks__Concat(ptr %t41, ptr %t44), !dbg !70
  %t46 = call ptr @RTHooks__Concat(ptr %t45, ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8)), !dbg !70
  call void @Main__Print(ptr %t46), !dbg !70
  br label %if.merge.9
if.next.8:
  %t47 = load i64, ptr %t32
  %t48 = load ptr, ptr %count.slot
  %t49 = load { ptr, i64 }, ptr %t48
  %__ll4 = extractvalue { ptr, i64 } %t49, 0
  %t50 = getelementptr inbounds i64, ptr %__ll4, i64 %t47
  %t51 = load i64, ptr %t50
  %t52 = add i64 %t51, 1
  store i64 %t52, ptr %t50
  br label %if.merge.9
if.merge.9:
  %t53 = load i64, ptr %i.slot.1
  %t54 = add i64 %t53, 1
  store i64 %t54, ptr %i.slot.1
  br label %for.header.4
for.header.10:
  %t62 = load i64, ptr %n.slot
  %t63 = load i64, ptr %t61
  %t64 = icmp sle i64 %t62, %t63
  br i1 %t64, label %for.body.11, label %for.exit.12
for.body.11:
  %t65 = load i64, ptr %n.slot
  %t66 = load ptr, ptr %count.slot
  %t67 = load { ptr, i64 }, ptr %t66
  %__ll5 = extractvalue { ptr, i64 } %t67, 0
  %t68 = getelementptr inbounds i64, ptr %__ll5, i64 %t65
  %t69 = load i64, ptr %t68
  %t70 = icmp sgt i64 %t69, 0
  br i1 %t70, label %if.then.13, label %if.merge.14
for.exit.12:
  %t98 = load i64, ptr %error.slot
  %t99 = trunc i64 %t98 to i1
  br i1 %t99, label %if.then.23, label %if.next.24
if.then.13:
  %t71 = load i64, ptr %n.slot
  %t72 = trunc i64 10 to i8
  %t73 = call ptr @Fmt__Int(i64 %t71, i8 %t72), !dbg !75
  %t74 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_6, i64 8), ptr %t73), !dbg !75
  %t75 = call ptr @RTHooks__Concat(ptr %t74, ptr getelementptr inbounds (i8, ptr @textlit_7, i64 8)), !dbg !75
  %t76 = load i64, ptr %t68
  %t77 = trunc i64 10 to i8
  %t78 = call ptr @Fmt__Int(i64 %t76, i8 %t77), !dbg !75
  %t79 = call ptr @RTHooks__Concat(ptr %t75, ptr %t78), !dbg !75
  %t80 = call ptr @RTHooks__Concat(ptr %t79, ptr getelementptr inbounds (i8, ptr @textlit_8, i64 8)), !dbg !75
  call void @Main__Print(ptr %t80), !dbg !75
  br label %if.merge.14
if.merge.14:
  store i1 0, ptr %t81
  %t82 = load i64, ptr %n.slot
  %t83 = icmp ne i64 %t82, 1
  br i1 %t83, label %and.rhs.15, label %and.merge.16
and.rhs.15:
  %t84 = load i64, ptr %t68
  %t85 = icmp ne i64 %t84, 0
  store i1 %t85, ptr %t81
  br label %and.merge.16
and.merge.16:
  %t86 = load i1, ptr %t81
  br i1 %t86, label %if.then.17, label %if.next.18
if.then.17:
  %t87 = zext i1 1 to i64
  store i64 %t87, ptr %error.slot
  br label %if.merge.19
if.next.18:
  store i1 0, ptr %t88
  %t89 = load i64, ptr %n.slot
  %t90 = icmp eq i64 %t89, 1
  br i1 %t90, label %and.rhs.20, label %and.merge.21
if.merge.19:
  %t96 = load i64, ptr %n.slot
  %t97 = add i64 %t96, 1
  store i64 %t97, ptr %n.slot
  br label %for.header.10
and.rhs.20:
  %t91 = load i64, ptr %t68
  %t92 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t93 = icmp ne i64 %t91, %t92
  store i1 %t93, ptr %t88
  br label %and.merge.21
and.merge.21:
  %t94 = load i1, ptr %t88
  br i1 %t94, label %if.then.22, label %if.merge.19
if.then.22:
  %t95 = zext i1 1 to i64
  store i64 %t95, ptr %error.slot
  br label %if.merge.19
if.then.23:
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_9, i64 8)), !dbg !77
  store i64 1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 368)
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_10, i64 8)), !dbg !79
  br label %if.merge.25
if.next.24:
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_11, i64 8)), !dbg !80
  br label %if.merge.25
if.merge.25:
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t85 = alloca i64
  %i.slot.2 = alloca i64
  %t67 = alloca i64
  %i.slot.1 = alloca i64
  %minutes.slot = alloca i64
  %t26 = alloca i64
  %i.slot = alloca i64
  %t15 = alloca { ptr, i64, i64 }
  %t5 = alloca { ptr, i64, i64 }
  %child.slot = alloca ptr
  %t1 = load ptr, ptr @tl_obj_356643957
  %t2 = call ptr @RTHooks__AllocateTracedObj(ptr %t1)
  %t3 = bitcast ptr %t2 to ptr
  store ptr %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)
  store i64 10, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  store ptr null, ptr %child.slot
  %t4 = load ptr, ptr @tl_arr_-784487079
  %t6 = getelementptr i8, ptr %t5, i64 16
  %t7 = getelementptr i8, ptr %t5, i64 0
  store ptr %t6, ptr %t7
  %t8 = getelementptr i8, ptr %t5, i64 8
  store i64 1, ptr %t8
  %t9 = getelementptr i8, ptr %t5, i64 16
  %t10 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  %t11 = bitcast i64 %t10 to i64
  store i64 %t11, ptr %t9
  %t12 = call ptr @RTHooks__AllocateOpenArray(ptr %t4, ptr %t5)
  %t13 = bitcast ptr %t12 to ptr
  store ptr %t13, ptr %child.slot
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_13, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_14, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_15, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_16, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_17, i64 8))
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8))
  store i64 303, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  store i64 573741, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t14 = load ptr, ptr @tl_arr_-476616643
  %t16 = getelementptr i8, ptr %t15, i64 16
  %t17 = getelementptr i8, ptr %t15, i64 0
  store ptr %t16, ptr %t17
  %t18 = getelementptr i8, ptr %t15, i64 8
  store i64 1, ptr %t18
  %t19 = getelementptr i8, ptr %t15, i64 16
  %t20 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t21 = bitcast i64 %t20 to i64
  store i64 %t21, ptr %t19
  %t22 = call ptr @RTHooks__AllocateOpenArray(ptr %t14, ptr %t15)
  %t23 = bitcast ptr %t22 to ptr
  store ptr %t23, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 400)
  store i64 0, ptr %i.slot
  %t24 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t25 = sub i64 %t24, 1
  store i64 %t25, ptr %t26
  br label %for.header.1
for.header.1:
  %t27 = load i64, ptr %i.slot
  %t28 = load i64, ptr %t26
  %t29 = icmp sle i64 %t27, %t28
  br i1 %t29, label %for.body.2, label %for.exit.3
for.body.2:
  %t30 = load i64, ptr %i.slot
  %t31 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 400)
  %__gc_nil.1 = icmp eq ptr %t31, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t31 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t31, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t31)
  br label %gc.skip.1
gc.skip.1:
  %t32 = load { ptr, i64 }, ptr %t31
  %__ll2 = extractvalue { ptr, i64 } %t32, 0
  %t33 = getelementptr inbounds i64, ptr %__ll2, i64 %t30
  store i64 0, ptr %t33
  %t34 = load i64, ptr %i.slot
  %t35 = add i64 %t34, 1
  store i64 %t35, ptr %i.slot
  br label %for.header.1
for.exit.3:
  %t36 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  %t37 = trunc i64 10 to i8
  %t38 = call ptr @Fmt__Int(i64 %t36, i8 %t37)
  %t39 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_18, i64 8), ptr %t38)
  %t40 = call ptr @RTHooks__Concat(ptr %t39, ptr getelementptr inbounds (i8, ptr @textlit_19, i64 8))
  %t41 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t42 = trunc i64 10 to i8
  %t43 = call ptr @Fmt__Int(i64 %t41, i8 %t42)
  %t44 = call ptr @RTHooks__Concat(ptr %t40, ptr %t43)
  call void @Main__Print(ptr %t44)
  %t45 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
  %t46 = sitofp i64 %t45 to double
  %t47 = fmul double %t46, 0x3fbc28f5c28f5c29
  %t48 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  %t49 = sitofp i64 %t48 to double
  %t50 = fdiv double %t47, %t49
  %t51 = fdiv double %t50, 0x404e000000000000
  %t52 = call double @llvm.roundeven.f64(double %t51)
  %t53 = fptosi double %t52 to i64
  %t54 = add i64 %t53, 1
  store i64 %t54, ptr %minutes.slot
  %t55 = load i64, ptr %minutes.slot
  %t56 = trunc i64 10 to i8
  %t57 = call ptr @Fmt__Int(i64 %t55, i8 %t56)
  %t58 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_20, i64 8), ptr %t57)
  %t59 = call ptr @RTHooks__Concat(ptr %t58, ptr getelementptr inbounds (i8, ptr @textlit_21, i64 8))
  %t60 = load i64, ptr %minutes.slot
  %t61 = add i64 %t60, 1
  %t62 = trunc i64 10 to i8
  %t63 = call ptr @Fmt__Int(i64 %t61, i8 %t62)
  %t64 = call ptr @RTHooks__Concat(ptr %t59, ptr %t63)
  %t65 = call ptr @RTHooks__Concat(ptr %t64, ptr getelementptr inbounds (i8, ptr @textlit_22, i64 8))
  call void @Main__Print(ptr %t65)
  call void @Main__Print(ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8))
  store i64 1, ptr %i.slot.1
  %t66 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  store i64 %t66, ptr %t67
  br label %for.header.4
for.header.4:
  %t68 = load i64, ptr %i.slot.1
  %t69 = load i64, ptr %t67
  %t70 = icmp sle i64 %t68, %t69
  br i1 %t70, label %for.body.5, label %for.exit.6
for.body.5:
  %t71 = load i64, ptr %i.slot.1
  %t72 = sub i64 %t71, 1
  %t73 = load ptr, ptr %child.slot
  %t74 = load { ptr, i64 }, ptr %t73
  %__ll3 = extractvalue { ptr, i64 } %t74, 0
  %t75 = getelementptr inbounds ptr, ptr %__ll3, i64 %t72
  %t76 = load ptr, ptr @tl_obj_-1770097841
  %t77 = call ptr @RTHooks__AllocateTracedObj(ptr %t76)
  %t78 = bitcast ptr %t77 to ptr
  %t79 = load i64, ptr %i.slot.1
  %t80 = getelementptr i8, ptr %t78, i64 8
  store i64 %t79, ptr %t80
  %t81 = call ptr @Thread__Fork(ptr %t78)
  %__gc_whptr.4 = getelementptr i8, ptr %t73, i64 -8
  %__gc_whdr.4 = load i64, ptr %__gc_whptr.4
  %__gc_wdb.4 = and i64 %__gc_whdr.4, 2097152
  %__gc_wdirty.4 = icmp ne i64 %__gc_wdb.4, 0
  br i1 %__gc_wdirty.4, label %gc.wskip.4, label %gc.wslow.4
gc.wslow.4:
  call void @RTHooks__CheckStoreTraced(ptr %t73)
  br label %gc.wskip.4
gc.wskip.4:
  store ptr %t81, ptr %t75
  %t82 = load i64, ptr %i.slot.1
  %t83 = add i64 %t82, 1
  store i64 %t83, ptr %i.slot.1
  br label %for.header.4
for.exit.6:
  store i64 1, ptr %i.slot.2
  %t84 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
  store i64 %t84, ptr %t85
  br label %for.header.7
for.header.7:
  %t86 = load i64, ptr %i.slot.2
  %t87 = load i64, ptr %t85
  %t88 = icmp sle i64 %t86, %t87
  br i1 %t88, label %for.body.8, label %for.exit.9
for.body.8:
  %t89 = load i64, ptr %i.slot.2
  %t90 = sub i64 %t89, 1
  %t91 = load ptr, ptr %child.slot
  %t92 = load { ptr, i64 }, ptr %t91
  %__ll5 = extractvalue { ptr, i64 } %t92, 0
  %t93 = getelementptr inbounds ptr, ptr %__ll5, i64 %t90
  %t94 = load ptr, ptr %t93
  %__gc_nil.6 = icmp eq ptr %t94, null
  br i1 %__gc_nil.6, label %gc.skip.6, label %gc.check.6
gc.check.6:
  %__gc_int.6 = ptrtoint ptr %t94 to i64
  %__gc_low.6 = and i64 %__gc_int.6, 1
  %__gc_ma.6 = icmp ne i64 %__gc_low.6, 0
  br i1 %__gc_ma.6, label %gc.skip.6, label %gc.gray.6
gc.gray.6:
  %__gc_hptr.6 = getelementptr i8, ptr %t94, i64 -8
  %__gc_hdr.6 = load i64, ptr %__gc_hptr.6
  %__gc_gb.6 = and i64 %__gc_hdr.6, 4194304
  %__gc_gr.6 = icmp ne i64 %__gc_gb.6, 0
  br i1 %__gc_gr.6, label %gc.slow.6, label %gc.skip.6
gc.slow.6:
  call void @RTHooks__CheckLoadTracedRef(ptr %t94)
  br label %gc.skip.6
gc.skip.6:
  %t95 = call ptr @Thread__Join(ptr %t94)
  %t96 = load i64, ptr %i.slot.2
  %t97 = add i64 %t96, 1
  store i64 %t97, ptr %i.slot.2
  br label %for.header.7
for.exit.9:
  call void @Main__CheckResults()
  %t98 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 368)
  %t99 = trunc i64 %t98 to i32
  call void @Process__Exit(i32 %t99)
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
@textlit_1 = internal constant { i64, ptr, i64, [56 x i8] } { i64 2, ptr @textlit_methods, i64 55, [56 x i8] c"!!! Something really broken in CM3 because sharedArray[\00" }
@textlit_2 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c"] = \00" }
@textlit_3 = internal constant { i64, ptr, i64, [36 x i8] } { i64 2, ptr @textlit_methods, i64 35, [36 x i8] c" which is greater than maxCount !!!\00" }
@textlit_4 = internal constant { i64, ptr, i64, [36 x i8] } { i64 2, ptr @textlit_methods, i64 35, [36 x i8] c"\0aHISTOGRAM:  (result should be [1: \00" }
@textlit_5 = internal constant { i64, ptr, i64, [35 x i8] } { i64 2, ptr @textlit_methods, i64 34, [35 x i8] c"] with no other entries)\0a---------\00" }
@textlit_6 = internal constant { i64, ptr, i64, [2 x i8] } { i64 2, ptr @textlit_methods, i64 1, [2 x i8] c"[\00" }
@textlit_7 = internal constant { i64, ptr, i64, [3 x i8] } { i64 2, ptr @textlit_methods, i64 2, [3 x i8] c": \00" }
@textlit_8 = internal constant { i64, ptr, i64, [2 x i8] } { i64 2, ptr @textlit_methods, i64 1, [2 x i8] c"]\00" }
@textlit_9 = internal constant { i64, ptr, i64, [20 x i8] } { i64 2, ptr @textlit_methods, i64 19, [20 x i8] c"\0a! ERROR DETECTED !\00" }
@textlit_10 = internal constant { i64, ptr, i64, [69 x i8] } { i64 2, ptr @textlit_methods, i64 68, [69 x i8] c"\0a!!! Something is broken in the CM3 system and needs to be fixed !!!\00" }
@textlit_11 = internal constant { i64, ptr, i64, [3 x i8] } { i64 2, ptr @textlit_methods, i64 2, [3 x i8] c"OK\00" }
@textlit_12 = internal constant { i64, ptr, i64, [80 x i8] } { i64 2, ptr @textlit_methods, i64 79, [80 x i8] c"-------------------------------------------------------------------------------\00" }
@textlit_13 = internal constant { i64, ptr, i64, [80 x i8] } { i64 2, ptr @textlit_methods, i64 79, [80 x i8] c"This program designed to test if MUTEX working properly using multiple threads.\00" }
@textlit_14 = internal constant { i64, ptr, i64, [24 x i8] } { i64 2, ptr @textlit_methods, i64 23, [24 x i8] c"Author:  Randy Coleburn\00" }
@textlit_15 = internal constant { i64, ptr, i64, [63 x i8] } { i64 2, ptr @textlit_methods, i64 62, [63 x i8] c"Inspiration:  \22The Little Book of Semaphores\22, by Allen Downey\00" }
@textlit_16 = internal constant { i64, ptr, i64, [50 x i8] } { i64 2, ptr @textlit_methods, i64 49, [50 x i8] c"              Section 8.1: Mutex checker problem.\00" }
@textlit_17 = internal constant { i64, ptr, i64, [55 x i8] } { i64 2, ptr @textlit_methods, i64 54, [55 x i8] c"              http://www.greenteapress.com/semaphores/\00" }
@textlit_18 = internal constant { i64, ptr, i64, [7 x i8] } { i64 2, ptr @textlit_methods, i64 6, [7 x i8] c"Using \00" }
@textlit_19 = internal constant { i64, ptr, i64, [26 x i8] } { i64 2, ptr @textlit_methods, i64 25, [26 x i8] c" threads with maxCount = \00" }
@textlit_20 = internal constant { i64, ptr, i64, [29 x i8] } { i64 2, ptr @textlit_methods, i64 28, [29 x i8] c"Expected runtime is approx. \00" }
@textlit_21 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c" to \00" }
@textlit_22 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c" minutes.\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_arr_-784487079_gc_map = internal constant [4 x i8] [i8 24, i8 1, i8 4, i8 0]
@tc_arr_-784487079 = internal global %ATC_t {
  i64 0,
  i64 -784487079,
  i64 u0x12c5bdebc3f810b2,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr @tc_arr_-784487079_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_arr_-476616643,
  i64 1,
  i64 8
}
@tc_arr_-476616643_gc_map = internal constant [4 x i8] [i8 24, i8 1, i8 4, i8 0]
@tc_arr_-476616643 = internal global %ATC_t {
  i64 0,
  i64 -476616643,
  i64 u0x0cdd2331ef4a4b0c,
  i8 1,
  i8 3,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 16,
  ptr null,
  ptr @tc_arr_-476616643_gc_map,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_obj_-1770097841,
  i64 1,
  i64 8
}
@tc_obj_-1770097841.tc_name = private unnamed_addr constant [18 x i8] c"Main.ChildClosure\00"
@tc_obj_-1770097841.methods = internal constant [1 x ptr] [ptr @Main__ChildApply]
@tc_obj_-1770097841 = internal global %OTC_t {
  i64 0,
  i64 -1770097841,
  i64 u0x03a45bd795da2898,
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
  ptr @tc_obj_-1770097841.tc_name,
  ptr null,
  i64 -448425059,
  ptr null,
  i64 0,
  i64 0,
  i64 0,
  ptr @tc_obj_-1770097841.methods,
  ptr null
}

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_arr_-476616643 = internal global %TypeLink_t {
  ptr null,
  i64 -476616643
}
@tl_obj_356643957 = internal global %TypeLink_t {
  ptr @tl_arr_-476616643,
  i64 356643957
}
@tl_arr_-784487079 = internal global %TypeLink_t {
  ptr @tl_obj_356643957,
  i64 -784487079
}
@tl_obj_-1770097841 = internal global %TypeLink_t {
  ptr @tl_arr_-784487079,
  i64 -1770097841
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Thread_I3(i64)
declare ptr @Process_I3(i64)
declare ptr @IO_I3(i64)
declare ptr @Fmt_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Thread_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Process_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @IO_I3, ptr @Main_M3_imp.4 }
@Main_M3_imp.4 = internal global { ptr, ptr, ptr } { ptr null, ptr @Fmt_I3, ptr null }
@Main_M3_gc_map = internal constant [10 x i8] c"\2a\68\2b\18\01\04\2a\08\04\00"

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [312 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_arr_-784487079,  ; type_cells (+8)
  ptr @tl_obj_-1770097841,  ; type_cell_ptrs (+16)
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
  [312 x i8] zeroinitializer  ; user globals (312 bytes)
}
@Main__exitCode = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 368)
@Main__maxCount = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 376)
@Main__mutex = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 384)
@Main__numThreads = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 392)
@Main__sharedArray = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 400)
@Main__sharedCounter = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 408)

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
!16 = distinct !DISubprogram(name: "Main__Print", linkageName: "Main__Print", scope: !4, file: !3, line: 33, type: !6, scopeLine: 33, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__ChildApply", linkageName: "Main__ChildApply", scope: !4, file: !3, line: 43, type: !6, scopeLine: 43, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__CheckResults", linkageName: "Main__CheckResults", scope: !4, file: !3, line: 69, type: !6, scopeLine: 69, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!22 = !DILocalVariable(name: "msg", scope: !16, file: !3, line: 33, type: !15)
!27 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 43, type: !15)
!28 = !DILocalVariable(name: "numLoops", scope: !18, file: !3, line: 43, type: !7)
!34 = !DILocalVariable(name: "self", scope: !18, file: !3, line: 43, type: !33)
!35 = !DILocalVariable(name: "n", scope: !20, file: !3, line: 69, type: !7)
!36 = !DILocalVariable(name: "i.slot", scope: !20, file: !3, line: 69, type: !7)
!37 = !DILocalVariable(name: "i", scope: !20, file: !3, line: 69, type: !7)
!43 = !DILocalVariable(name: "error", scope: !20, file: !3, line: 69, type: !7)
!44 = !DILocalVariable(name: "count", scope: !20, file: !3, line: 69, type: !15)
!23 = !DICompositeType(tag: DW_TAG_structure_type, name: "lpad_t", size: 64, elements: !24)
!24 = !{!25, !26}
!25 = !DIDerivedType(tag: DW_TAG_member, name: "excobj", baseType: !15, size: 64, offset: 0)
!26 = !DIDerivedType(tag: DW_TAG_member, name: "sel", baseType: !9, size: 32, offset: 0)
!29 = !DICompositeType(tag: DW_TAG_structure_type, name: "ChildClosure", size: 128, elements: !30)
!30 = !{!31, !32}
!31 = !DIDerivedType(tag: DW_TAG_member, name: "__vtable", baseType: !15, size: 64, offset: 0)
!32 = !DIDerivedType(tag: DW_TAG_member, name: "id", baseType: !8, size: 64, offset: 64)
!33 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !29, size: 64)
!38 = !DICompositeType(tag: DW_TAG_structure_type, name: "__oa_shape", size: 64, elements: !39)
!39 = !{!40, !41, !42}
!40 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!41 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!42 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!45 = !DILocation(line: 33, column: 0, scope: !16)
!46 = !DILocation(line: 38, column: 0, scope: !16)
!47 = !DILocation(line: 56, column: 0, scope: !18)
!48 = !DILocation(line: 50, column: 0, scope: !18)
!49 = !DILocation(line: 43, column: 0, scope: !18)
!50 = !DILocation(line: 51, column: 0, scope: !18)
!51 = !DILocation(line: 60, column: 0, scope: !18)
!52 = !DILocation(line: 64, column: 0, scope: !18)
!53 = !DILocation(line: 53, column: 0, scope: !18)
!54 = !DILocation(line: 54, column: 0, scope: !18)
!55 = !DILocation(line: 55, column: 0, scope: !18)
!56 = !DILocation(line: 57, column: 0, scope: !18)
!57 = !DILocation(line: 58, column: 0, scope: !18)
!58 = !DILocation(line: 105, column: 0, scope: !20)
!59 = !DILocation(line: 103, column: 0, scope: !20)
!60 = !DILocation(line: 95, column: 0, scope: !20)
!61 = !DILocation(line: 81, column: 0, scope: !20)
!62 = !DILocation(line: 77, column: 0, scope: !20)
!63 = !DILocation(line: 76, column: 0, scope: !20)
!64 = !DILocation(line: 69, column: 0, scope: !20)
!65 = !DILocation(line: 79, column: 0, scope: !20)
!66 = !DILocation(line: 83, column: 0, scope: !20)
!67 = !DILocation(line: 85, column: 0, scope: !20)
!68 = !DILocation(line: 94, column: 0, scope: !20)
!69 = !DILocation(line: 87, column: 0, scope: !20)
!70 = !DILocation(line: 88, column: 0, scope: !20)
!71 = !DILocation(line: 90, column: 0, scope: !20)
!72 = !DILocation(line: 97, column: 0, scope: !20)
!73 = !DILocation(line: 99, column: 0, scope: !20)
!74 = !DILocation(line: 112, column: 0, scope: !20)
!75 = !DILocation(line: 101, column: 0, scope: !20)
!76 = !DILocation(line: 108, column: 0, scope: !20)
!77 = !DILocation(line: 114, column: 0, scope: !20)
!78 = !DILocation(line: 115, column: 0, scope: !20)
!79 = !DILocation(line: 116, column: 0, scope: !20)
!80 = !DILocation(line: 118, column: 0, scope: !20)
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
