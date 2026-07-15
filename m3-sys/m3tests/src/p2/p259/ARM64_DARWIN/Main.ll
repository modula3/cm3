; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @IO__Put(ptr, ptr)
declare void @IO__PutInt(i64, ptr)
declare ptr @__cxa_get_exception_ptr(ptr)
declare ptr @__cxa_begin_catch(ptr)
declare void @__cxa_end_catch()
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__NL() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8), ptr null), !dbg !44
  ret void
}

define ptr @Main__GetStack() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %_result.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !40, metadata !DIExpression()), !dbg !45
  %b.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !41, metadata !DIExpression()), !dbg !45
  store ptr null, ptr %b.slot
  store ptr null, ptr %_result.slot
  store ptr %b.slot, ptr %b.slot
  %t1 = load ptr, ptr %b.slot
  ret ptr %t1
}

define i64 @Main__GetStackHeight() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !42, metadata !DIExpression()), !dbg !47
  %b.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %b.slot, metadata !43, metadata !DIExpression()), !dbg !47
  store ptr null, ptr %b.slot
  %t1 = call ptr @Main__GetStack(), !dbg !47
  store ptr %t1, ptr %b.slot
  %t2 = load ptr, ptr %b.slot
  %t3 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t4 = icmp sgt ptr %t2, %t3
  br i1 %t4, label %if.then.1, label %if.merge.2
if.then.1:
  %t5 = load ptr, ptr %b.slot
  %t6 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t7 = ptrtoint ptr %t5 to i64
  %t8 = ptrtoint ptr %t6 to i64
  %t9 = sub i64 %t7, %t8
  ret i64 %t9
if.merge.2:
  %t10 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t11 = load ptr, ptr %b.slot
  %t12 = ptrtoint ptr %t10 to i64
  %t13 = ptrtoint ptr %t11 to i64
  %t14 = sub i64 %t12, %t13
  ret i64 %t14
}

define void @Main__PrintStackHeight() personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8), ptr null), !dbg !51
  %t1 = call i64 @Main__GetStackHeight(), !dbg !52
  call void @IO__PutInt(i64 %t1, ptr null), !dbg !52
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8), ptr null), !dbg !53
  call void @Main__NL(), !dbg !54
  ret void
}

define void @Main__Try1__Try1_Try2() personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8), ptr null), !dbg !56
  call void @Main__NL(), !dbg !56
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.7 unwind label %lpad.4, !dbg !55
lpad.1:
  %t14 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t15 = extractvalue { ptr, i32 } %t14, 0
  %t16 = call ptr @__cxa_get_exception_ptr(ptr %t15), !dbg !55
  %t17 = load ptr, ptr %t16
  %t18 = load ptr, ptr %t17
  %t19 = load i64, ptr %t18
  store ptr %t17, ptr %t2
  store i32 1, ptr %t1
  %t20 = call ptr @__cxa_begin_catch(ptr %t15), !dbg !55
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_4, i64 8), ptr null), !dbg !57
  call void @Main__NL(), !dbg !57
  ret void
else.dispatch.3:
  %t21 = load i32, ptr %t1
  %t22 = icmp eq i32 %t21, 1
  br i1 %t22, label %else.endcatch.10, label %else.skip.endcatch.11
lpad.4:
  %t5 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t6 = extractvalue { ptr, i32 } %t5, 0
  %t7 = call ptr @__cxa_get_exception_ptr(ptr %t6), !dbg !55
  %t8 = load ptr, ptr %t7
  %t9 = load ptr, ptr %t8
  %t10 = load i64, ptr %t9
  store ptr %t8, ptr %t4
  store i32 1, ptr %t3
  %t11 = call ptr @__cxa_begin_catch(ptr %t6), !dbg !55
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t12 = load i32, ptr %t3
  %t13 = icmp eq i32 %t12, 1
  br i1 %t13, label %else.endcatch.8, label %else.skip.endcatch.9
invoke.cont.7:
  br label %try.merge.5
else.endcatch.8:
  call void @__cxa_end_catch(), !dbg !55
  br label %try.merge.5
else.skip.endcatch.9:
  br label %try.merge.5
else.endcatch.10:
  call void @__cxa_end_catch(), !dbg !55
  br label %try.merge.2
else.skip.endcatch.11:
  br label %try.merge.2
}

define void @Main__Try1() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_5, i64 8), ptr null), !dbg !59
  call void @Main__NL(), !dbg !59
  call void @Main__Try1__Try1_Try2(), !dbg !60
  store i32 0, ptr %t1
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.4 unwind label %lpad.1, !dbg !58
lpad.1:
  %t3 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t4 = extractvalue { ptr, i32 } %t3, 0
  %t5 = call ptr @__cxa_get_exception_ptr(ptr %t4), !dbg !58
  %t6 = load ptr, ptr %t5
  %t7 = load ptr, ptr %t6
  %t8 = load i64, ptr %t7
  store ptr %t6, ptr %t2
  store i32 1, ptr %t1
  %t9 = call ptr @__cxa_begin_catch(ptr %t4), !dbg !58
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_6, i64 8), ptr null), !dbg !61
  call void @Main__NL(), !dbg !61
  ret void
else.dispatch.3:
  %t10 = load i32, ptr %t1
  %t11 = icmp eq i32 %t10, 1
  br i1 %t11, label %else.endcatch.5, label %else.skip.endcatch.6
invoke.cont.4:
  br label %try.merge.2
else.endcatch.5:
  call void @__cxa_end_catch(), !dbg !58
  br label %try.merge.2
else.skip.endcatch.6:
  br label %try.merge.2
}

define void @Main__Try2__Try2_Try2() personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_7, i64 8), ptr null), !dbg !63
  call void @Main__NL(), !dbg !63
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.7 unwind label %lpad.4, !dbg !62
lpad.1:
  %t14 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t15 = extractvalue { ptr, i32 } %t14, 0
  %t16 = call ptr @__cxa_get_exception_ptr(ptr %t15), !dbg !62
  %t17 = load ptr, ptr %t16
  %t18 = load ptr, ptr %t17
  %t19 = load i64, ptr %t18
  store ptr %t17, ptr %t2
  store i32 1, ptr %t1
  %t20 = call ptr @__cxa_begin_catch(ptr %t15), !dbg !62
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_8, i64 8), ptr null), !dbg !64
  call void @Main__NL(), !dbg !64
  ret void
else.dispatch.3:
  %t21 = load i32, ptr %t1
  %t22 = icmp eq i32 %t21, 1
  br i1 %t22, label %else.endcatch.10, label %else.skip.endcatch.11
lpad.4:
  %t5 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t6 = extractvalue { ptr, i32 } %t5, 0
  %t7 = call ptr @__cxa_get_exception_ptr(ptr %t6), !dbg !62
  %t8 = load ptr, ptr %t7
  %t9 = load ptr, ptr %t8
  %t10 = load i64, ptr %t9
  store ptr %t8, ptr %t4
  store i32 1, ptr %t3
  %t11 = call ptr @__cxa_begin_catch(ptr %t6), !dbg !62
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t12 = load i32, ptr %t3
  %t13 = icmp eq i32 %t12, 1
  br i1 %t13, label %else.endcatch.8, label %else.skip.endcatch.9
invoke.cont.7:
  br label %try.merge.5
else.endcatch.8:
  call void @__cxa_end_catch(), !dbg !62
  br label %try.merge.5
else.skip.endcatch.9:
  br label %try.merge.5
else.endcatch.10:
  call void @__cxa_end_catch(), !dbg !62
  br label %try.merge.2
else.skip.endcatch.11:
  br label %try.merge.2
}

define void @Main__Try2() personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_9, i64 8), ptr null), !dbg !66
  call void @Main__NL(), !dbg !66
  call void @Main__Try2__Try2_Try2(), !dbg !67
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.7 unwind label %lpad.4, !dbg !65
lpad.1:
  %t14 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t15 = extractvalue { ptr, i32 } %t14, 0
  %t16 = call ptr @__cxa_get_exception_ptr(ptr %t15), !dbg !65
  %t17 = load ptr, ptr %t16
  %t18 = load ptr, ptr %t17
  %t19 = load i64, ptr %t18
  store ptr %t17, ptr %t2
  store i32 1, ptr %t1
  %t20 = call ptr @__cxa_begin_catch(ptr %t15), !dbg !65
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_10, i64 8), ptr null), !dbg !68
  call void @Main__NL(), !dbg !68
  ret void
else.dispatch.3:
  %t21 = load i32, ptr %t1
  %t22 = icmp eq i32 %t21, 1
  br i1 %t22, label %else.endcatch.10, label %else.skip.endcatch.11
lpad.4:
  %t5 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t6 = extractvalue { ptr, i32 } %t5, 0
  %t7 = call ptr @__cxa_get_exception_ptr(ptr %t6), !dbg !65
  %t8 = load ptr, ptr %t7
  %t9 = load ptr, ptr %t8
  %t10 = load i64, ptr %t9
  store ptr %t8, ptr %t4
  store i32 1, ptr %t3
  %t11 = call ptr @__cxa_begin_catch(ptr %t6), !dbg !65
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t12 = load i32, ptr %t3
  %t13 = icmp eq i32 %t12, 1
  br i1 %t13, label %else.endcatch.8, label %else.skip.endcatch.9
invoke.cont.7:
  br label %try.merge.5
else.endcatch.8:
  call void @__cxa_end_catch(), !dbg !65
  br label %try.merge.5
else.skip.endcatch.9:
  br label %try.merge.5
else.endcatch.10:
  call void @__cxa_end_catch(), !dbg !65
  br label %try.merge.2
else.skip.endcatch.11:
  br label %try.merge.2
}

define void @Main__Try3__Try3_Try1() personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_11, i64 8), ptr null), !dbg !70
  call void @Main__NL(), !dbg !70
  store i32 0, ptr %t1
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.4 unwind label %lpad.1, !dbg !69
lpad.1:
  %t3 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t4 = extractvalue { ptr, i32 } %t3, 0
  %t5 = call ptr @__cxa_get_exception_ptr(ptr %t4), !dbg !69
  %t6 = load ptr, ptr %t5
  %t7 = load ptr, ptr %t6
  %t8 = load i64, ptr %t7
  store ptr %t6, ptr %t2
  store i32 1, ptr %t1
  %t9 = call ptr @__cxa_begin_catch(ptr %t4), !dbg !69
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8), ptr null), !dbg !71
  call void @Main__NL(), !dbg !71
  ret void
else.dispatch.3:
  %t10 = load i32, ptr %t1
  %t11 = icmp eq i32 %t10, 1
  br i1 %t11, label %else.endcatch.5, label %else.skip.endcatch.6
invoke.cont.4:
  br label %try.merge.2
else.endcatch.5:
  call void @__cxa_end_catch(), !dbg !69
  br label %try.merge.2
else.skip.endcatch.6:
  br label %try.merge.2
}

define void @Main__Try3__Try3_Try2() personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_13, i64 8), ptr null), !dbg !73
  call void @Main__NL(), !dbg !73
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.7 unwind label %lpad.4, !dbg !72
lpad.1:
  %t14 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t15 = extractvalue { ptr, i32 } %t14, 0
  %t16 = call ptr @__cxa_get_exception_ptr(ptr %t15), !dbg !72
  %t17 = load ptr, ptr %t16
  %t18 = load ptr, ptr %t17
  %t19 = load i64, ptr %t18
  store ptr %t17, ptr %t2
  store i32 1, ptr %t1
  %t20 = call ptr @__cxa_begin_catch(ptr %t15), !dbg !72
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_14, i64 8), ptr null), !dbg !74
  call void @Main__NL(), !dbg !74
  ret void
else.dispatch.3:
  %t21 = load i32, ptr %t1
  %t22 = icmp eq i32 %t21, 1
  br i1 %t22, label %else.endcatch.10, label %else.skip.endcatch.11
lpad.4:
  %t5 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t6 = extractvalue { ptr, i32 } %t5, 0
  %t7 = call ptr @__cxa_get_exception_ptr(ptr %t6), !dbg !72
  %t8 = load ptr, ptr %t7
  %t9 = load ptr, ptr %t8
  %t10 = load i64, ptr %t9
  store ptr %t8, ptr %t4
  store i32 1, ptr %t3
  %t11 = call ptr @__cxa_begin_catch(ptr %t6), !dbg !72
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t12 = load i32, ptr %t3
  %t13 = icmp eq i32 %t12, 1
  br i1 %t13, label %else.endcatch.8, label %else.skip.endcatch.9
invoke.cont.7:
  br label %try.merge.5
else.endcatch.8:
  call void @__cxa_end_catch(), !dbg !72
  br label %try.merge.5
else.skip.endcatch.9:
  br label %try.merge.5
else.endcatch.10:
  call void @__cxa_end_catch(), !dbg !72
  br label %try.merge.2
else.skip.endcatch.11:
  br label %try.merge.2
}

define void @Main__Try3__Try3_Try3() personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  %t6 = alloca ptr
  %t5 = alloca i32
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_15, i64 8), ptr null), !dbg !76
  call void @Main__NL(), !dbg !76
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  store i32 0, ptr %t5
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.10 unwind label %lpad.7, !dbg !75
lpad.1:
  %t25 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t26 = extractvalue { ptr, i32 } %t25, 0
  %t27 = call ptr @__cxa_get_exception_ptr(ptr %t26), !dbg !75
  %t28 = load ptr, ptr %t27
  %t29 = load ptr, ptr %t28
  %t30 = load i64, ptr %t29
  store ptr %t28, ptr %t2
  store i32 1, ptr %t1
  %t31 = call ptr @__cxa_begin_catch(ptr %t26), !dbg !75
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_16, i64 8), ptr null), !dbg !77
  call void @Main__NL(), !dbg !77
  ret void
else.dispatch.3:
  %t32 = load i32, ptr %t1
  %t33 = icmp eq i32 %t32, 1
  br i1 %t33, label %else.endcatch.15, label %else.skip.endcatch.16
lpad.4:
  %t16 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t17 = extractvalue { ptr, i32 } %t16, 0
  %t18 = call ptr @__cxa_get_exception_ptr(ptr %t17), !dbg !75
  %t19 = load ptr, ptr %t18
  %t20 = load ptr, ptr %t19
  %t21 = load i64, ptr %t20
  store ptr %t19, ptr %t4
  store i32 1, ptr %t3
  %t22 = call ptr @__cxa_begin_catch(ptr %t17), !dbg !75
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t23 = load i32, ptr %t3
  %t24 = icmp eq i32 %t23, 1
  br i1 %t24, label %else.endcatch.13, label %else.skip.endcatch.14
lpad.7:
  %t7 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t8 = extractvalue { ptr, i32 } %t7, 0
  %t9 = call ptr @__cxa_get_exception_ptr(ptr %t8), !dbg !75
  %t10 = load ptr, ptr %t9
  %t11 = load ptr, ptr %t10
  %t12 = load i64, ptr %t11
  store ptr %t10, ptr %t6
  store i32 1, ptr %t5
  %t13 = call ptr @__cxa_begin_catch(ptr %t8), !dbg !75
  br label %else.dispatch.9
try.merge.8:
  br label %try.merge.5
else.dispatch.9:
  %t14 = load i32, ptr %t5
  %t15 = icmp eq i32 %t14, 1
  br i1 %t15, label %else.endcatch.11, label %else.skip.endcatch.12
invoke.cont.10:
  br label %try.merge.8
else.endcatch.11:
  call void @__cxa_end_catch(), !dbg !75
  br label %try.merge.8
else.skip.endcatch.12:
  br label %try.merge.8
else.endcatch.13:
  call void @__cxa_end_catch(), !dbg !75
  br label %try.merge.5
else.skip.endcatch.14:
  br label %try.merge.5
else.endcatch.15:
  call void @__cxa_end_catch(), !dbg !75
  br label %try.merge.2
else.skip.endcatch.16:
  br label %try.merge.2
}

define void @Main__Try3() personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  %t6 = alloca ptr
  %t5 = alloca i32
  %t4 = alloca ptr
  %t3 = alloca i32
  %t2 = alloca ptr
  %t1 = alloca i32
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_17, i64 8), ptr null), !dbg !79
  call void @Main__NL(), !dbg !79
  call void @Main__Try3__Try3_Try1(), !dbg !80
  call void @Main__Try3__Try3_Try2(), !dbg !81
  call void @Main__Try3__Try3_Try3(), !dbg !82
  store i32 0, ptr %t1
  store i32 0, ptr %t3
  store i32 0, ptr %t5
  invoke void @Main__PrintStackHeight()
          to label %invoke.cont.10 unwind label %lpad.7, !dbg !78
lpad.1:
  %t25 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t26 = extractvalue { ptr, i32 } %t25, 0
  %t27 = call ptr @__cxa_get_exception_ptr(ptr %t26), !dbg !78
  %t28 = load ptr, ptr %t27
  %t29 = load ptr, ptr %t28
  %t30 = load i64, ptr %t29
  store ptr %t28, ptr %t2
  store i32 1, ptr %t1
  %t31 = call ptr @__cxa_begin_catch(ptr %t26), !dbg !78
  br label %else.dispatch.3
try.merge.2:
  call void @IO__Put(ptr getelementptr inbounds (i8, ptr @textlit_18, i64 8), ptr null), !dbg !83
  call void @Main__NL(), !dbg !83
  ret void
else.dispatch.3:
  %t32 = load i32, ptr %t1
  %t33 = icmp eq i32 %t32, 1
  br i1 %t33, label %else.endcatch.15, label %else.skip.endcatch.16
lpad.4:
  %t16 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t17 = extractvalue { ptr, i32 } %t16, 0
  %t18 = call ptr @__cxa_get_exception_ptr(ptr %t17), !dbg !78
  %t19 = load ptr, ptr %t18
  %t20 = load ptr, ptr %t19
  %t21 = load i64, ptr %t20
  store ptr %t19, ptr %t4
  store i32 1, ptr %t3
  %t22 = call ptr @__cxa_begin_catch(ptr %t17), !dbg !78
  br label %else.dispatch.6
try.merge.5:
  br label %try.merge.2
else.dispatch.6:
  %t23 = load i32, ptr %t3
  %t24 = icmp eq i32 %t23, 1
  br i1 %t24, label %else.endcatch.13, label %else.skip.endcatch.14
lpad.7:
  %t7 = landingpad { ptr, i32 }
          catch ptr @_ZTI6_M3Exc
  %t8 = extractvalue { ptr, i32 } %t7, 0
  %t9 = call ptr @__cxa_get_exception_ptr(ptr %t8), !dbg !78
  %t10 = load ptr, ptr %t9
  %t11 = load ptr, ptr %t10
  %t12 = load i64, ptr %t11
  store ptr %t10, ptr %t6
  store i32 1, ptr %t5
  %t13 = call ptr @__cxa_begin_catch(ptr %t8), !dbg !78
  br label %else.dispatch.9
try.merge.8:
  br label %try.merge.5
else.dispatch.9:
  %t14 = load i32, ptr %t5
  %t15 = icmp eq i32 %t14, 1
  br i1 %t15, label %else.endcatch.11, label %else.skip.endcatch.12
invoke.cont.10:
  br label %try.merge.8
else.endcatch.11:
  call void @__cxa_end_catch(), !dbg !78
  br label %try.merge.8
else.skip.endcatch.12:
  br label %try.merge.8
else.endcatch.13:
  call void @__cxa_end_catch(), !dbg !78
  br label %try.merge.5
else.skip.endcatch.14:
  br label %try.merge.5
else.endcatch.15:
  call void @__cxa_end_catch(), !dbg !78
  br label %try.merge.2
else.skip.endcatch.16:
  br label %try.merge.2
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t1 = call ptr @Main__GetStack()
  store ptr %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  call void @Main__Try1()
  call void @Main__NL()
  call void @Main__Try2()
  call void @Main__NL()
  call void @Main__Try3()
  call void @Main__NL()
  call void @Main__Try1()
  call void @Main__NL()
  call void @Main__Try2()
  call void @Main__NL()
  call void @Main__Try3()
  call void @Main__NL()
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
@textlit_1 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"stack_height:\00" }
@textlit_2 = internal constant { i64, ptr, i64, [2 x i8] } { i64 2, ptr @textlit_methods, i64 1, [2 x i8] c" \00" }
@textlit_3 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c">Try1_Try2\00" }
@textlit_4 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"<Try1_Try2\00" }
@textlit_5 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c">Try1\00" }
@textlit_6 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"<Try1\00" }
@textlit_7 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c">Try2_Try2\00" }
@textlit_8 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"<Try2_Try2\00" }
@textlit_9 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c">Try2\00" }
@textlit_10 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"<Try2\00" }
@textlit_11 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c">Try3_Try1\00" }
@textlit_12 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"<Try3_Try1\00" }
@textlit_13 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c">Try3_Try2\00" }
@textlit_14 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"<Try3_Try2\00" }
@textlit_15 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c">Try3_Try3\00" }
@textlit_16 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"<Try3_Try3\00" }
@textlit_17 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c">Try3\00" }
@textlit_18 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"<Try3\00" }

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @IO_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @IO_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [8 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
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
  i64 3,  ; gc_flags (+96)
  [8 x i8] zeroinitializer  ; user globals (8 bytes)
}
@Main__top_of_stack = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)

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
!16 = distinct !DISubprogram(name: "Main__NL", linkageName: "Main__NL", scope: !4, file: !3, line: 7, type: !6, scopeLine: 7, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__GetStack", linkageName: "Main__GetStack", scope: !4, file: !3, line: 11, type: !6, scopeLine: 11, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__GetStackHeight", linkageName: "Main__GetStackHeight", scope: !4, file: !3, line: 18, type: !6, scopeLine: 18, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__PrintStackHeight", linkageName: "Main__PrintStackHeight", scope: !4, file: !3, line: 27, type: !6, scopeLine: 27, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Main__Try1__Try1_Try2", linkageName: "Main__Try1__Try1_Try2", scope: !4, file: !3, line: 36, type: !6, scopeLine: 36, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Main__Try1", linkageName: "Main__Try1", scope: !4, file: !3, line: 35, type: !6, scopeLine: 35, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Main__Try2__Try2_Try2", linkageName: "Main__Try2__Try2_Try2", scope: !4, file: !3, line: 49, type: !6, scopeLine: 49, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Main__Try2", linkageName: "Main__Try2", scope: !4, file: !3, line: 48, type: !6, scopeLine: 48, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Main__Try3__Try3_Try1", linkageName: "Main__Try3__Try3_Try1", scope: !4, file: !3, line: 62, type: !6, scopeLine: 62, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Main__Try3__Try3_Try2", linkageName: "Main__Try3__Try3_Try2", scope: !4, file: !3, line: 68, type: !6, scopeLine: 68, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Main__Try3__Try3_Try3", linkageName: "Main__Try3__Try3_Try3", scope: !4, file: !3, line: 74, type: !6, scopeLine: 74, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Main__Try3", linkageName: "Main__Try3", scope: !4, file: !3, line: 61, type: !6, scopeLine: 61, unit: !2, spFlags: DISPFlagDefinition)
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
!40 = !DILocalVariable(name: "_result", scope: !18, file: !3, line: 11, type: !15)
!41 = !DILocalVariable(name: "b", scope: !18, file: !3, line: 11, type: !15)
!42 = !DILocalVariable(name: "_result", scope: !20, file: !3, line: 18, type: !7)
!43 = !DILocalVariable(name: "b", scope: !20, file: !3, line: 18, type: !15)
!44 = !DILocation(line: 7, column: 0, scope: !16)
!45 = !DILocation(line: 11, column: 0, scope: !18)
!46 = !DILocation(line: 15, column: 0, scope: !18)
!47 = !DILocation(line: 18, column: 0, scope: !20)
!48 = !DILocation(line: 21, column: 0, scope: !20)
!49 = !DILocation(line: 22, column: 0, scope: !20)
!50 = !DILocation(line: 24, column: 0, scope: !20)
!51 = !DILocation(line: 29, column: 0, scope: !22)
!52 = !DILocation(line: 30, column: 0, scope: !22)
!53 = !DILocation(line: 31, column: 0, scope: !22)
!54 = !DILocation(line: 32, column: 0, scope: !22)
!55 = !DILocation(line: 38, column: 0, scope: !24)
!56 = !DILocation(line: 37, column: 0, scope: !24)
!57 = !DILocation(line: 39, column: 0, scope: !24)
!58 = !DILocation(line: 44, column: 0, scope: !26)
!59 = !DILocation(line: 42, column: 0, scope: !26)
!60 = !DILocation(line: 43, column: 0, scope: !26)
!61 = !DILocation(line: 45, column: 0, scope: !26)
!62 = !DILocation(line: 51, column: 0, scope: !28)
!63 = !DILocation(line: 50, column: 0, scope: !28)
!64 = !DILocation(line: 52, column: 0, scope: !28)
!65 = !DILocation(line: 57, column: 0, scope: !30)
!66 = !DILocation(line: 55, column: 0, scope: !30)
!67 = !DILocation(line: 56, column: 0, scope: !30)
!68 = !DILocation(line: 58, column: 0, scope: !30)
!69 = !DILocation(line: 64, column: 0, scope: !32)
!70 = !DILocation(line: 63, column: 0, scope: !32)
!71 = !DILocation(line: 65, column: 0, scope: !32)
!72 = !DILocation(line: 70, column: 0, scope: !34)
!73 = !DILocation(line: 69, column: 0, scope: !34)
!74 = !DILocation(line: 71, column: 0, scope: !34)
!75 = !DILocation(line: 76, column: 0, scope: !36)
!76 = !DILocation(line: 75, column: 0, scope: !36)
!77 = !DILocation(line: 77, column: 0, scope: !36)
!78 = !DILocation(line: 84, column: 0, scope: !38)
!79 = !DILocation(line: 80, column: 0, scope: !38)
!80 = !DILocation(line: 81, column: 0, scope: !38)
!81 = !DILocation(line: 82, column: 0, scope: !38)
!82 = !DILocation(line: 83, column: 0, scope: !38)
!83 = !DILocation(line: 85, column: 0, scope: !38)
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
