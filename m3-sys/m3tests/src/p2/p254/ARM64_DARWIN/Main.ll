; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @RTIO__PutInt(i64, i64)
declare void @RTIO__PutText(ptr)
declare void @RTIO__Flush()
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__A(i64 %a.a) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !24, metadata !DIExpression()), !dbg !43
  store i64 %a.a, ptr %a.slot
  %t1 = load i64, ptr %a.slot
  call void @RTIO__PutInt(i64 %t1, i64 0), !dbg !44
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !45
  ret void
}

define void @Main__F2(ptr %a.p) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %p.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %p.slot, metadata !25, metadata !DIExpression()), !dbg !46
  store ptr %a.p, ptr %p.slot
  %t1 = load ptr, ptr %p.slot
  %t2 = load i64, ptr %t1
  %t3 = icmp eq i64 %t2, -1
  br i1 %t3, label %cl.closure.1, label %cl.direct.2
cl.closure.1:
  %t4 = getelementptr i8, ptr %t1, i64 8
  %t5 = load ptr, ptr %t4
  %t6 = getelementptr i8, ptr %t1, i64 16
  %t7 = load ptr, ptr %t6
  call void %t5(ptr %t7), !dbg !47
  br label %cl.merge.3
cl.direct.2:
  call void %t1(), !dbg !47
  br label %cl.merge.3
cl.merge.3:
  ret void
}

define void @Main__Main__F1(ptr %__cap_0, i64 %__cap_1, ptr %__cap_2, ptr %__cap_3, ptr %__cap_4, ptr %__cap_5, ptr %__cap_6, ptr %__cap_7) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %t1 = load i64, ptr %__cap_0
  %t2 = add i64 100, %__cap_1
  %t3 = add i64 %t1, %t2
  store i64 %t3, ptr %__cap_0
  %t4 = load i64, ptr %__cap_2
  %t5 = add i64 200, %__cap_1
  %t6 = add i64 %t4, %t5
  store i64 %t6, ptr %__cap_2
  %t7 = getelementptr i8, ptr %__cap_3, i64 0
  %t8 = load i64, ptr %t7
  %t9 = getelementptr i8, ptr %__cap_4, i64 0
  %t10 = load i64, ptr %t9
  %t11 = add i64 300, %t10
  %t12 = add i64 %t8, %t11
  store i64 %t12, ptr %t7
  %t13 = getelementptr i8, ptr %__cap_5, i64 0
  %t14 = load i64, ptr %t13
  %t15 = getelementptr i8, ptr %__cap_4, i64 0
  %t16 = load i64, ptr %t15
  %t17 = add i64 400, %t16
  %t18 = add i64 %t14, %t17
  store i64 %t18, ptr %t13
  %t19 = load i64, ptr %__cap_6
  %t20 = add i64 %t19, 600
  store i64 %t20, ptr %__cap_6
  %t21 = getelementptr i8, ptr %__cap_7, i64 0
  %t22 = load i64, ptr %t21
  %t23 = add i64 %t22, 700
  store i64 %t23, ptr %t21
  ret void
}

define internal void @Main__Main__F1__shim(ptr %__env) personality ptr @__gxx_personality_v0 {
entry:
  %t1 = getelementptr i8, ptr %__env, i64 0
  %t2 = load ptr, ptr %t1
  %t3 = getelementptr i8, ptr %__env, i64 8
  %t4 = load ptr, ptr %t3
  %t5 = load i64, ptr %t4
  %t6 = getelementptr i8, ptr %__env, i64 16
  %t7 = load ptr, ptr %t6
  %t8 = getelementptr i8, ptr %__env, i64 24
  %t9 = load ptr, ptr %t8
  %t10 = getelementptr i8, ptr %__env, i64 32
  %t11 = load ptr, ptr %t10
  %t12 = getelementptr i8, ptr %__env, i64 40
  %t13 = load ptr, ptr %t12
  %t14 = getelementptr i8, ptr %__env, i64 48
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr i8, ptr %__env, i64 56
  %t17 = load ptr, ptr %t16
  call void @Main__Main__F1(ptr %t2, i64 %t5, ptr %t7, ptr %t9, ptr %t11, ptr %t13, ptr %t15, ptr %t17)
  ret void
}

define void @Main__Main(ptr %_result_ptr, i64 %a.param_integer, i64 %a.param_integer_uplevel, ptr %a.var_param_integer, ptr %a.var_param_integer_uplevel, ptr %a.readonly_param_integer, ptr %a.readonly_param_integer_uplevel, { i64 } %a.param_record, { i64 } %a.param_record_uplevel, ptr %a.var_param_record, ptr %a.var_param_record_uplevel, ptr %a.readonly_param_record, ptr %a.readonly_param_record_uplevel) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %t98 = alloca { i64 }
  %t87 = alloca { i64 }
  %t65 = alloca [24 x i8]
  %t56 = alloca i64
  %t54 = alloca [8 x ptr]
  %local_record_uplevel.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %local_record_uplevel.slot, metadata !35, metadata !DIExpression()), !dbg !57
  %local_record.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %local_record.slot, metadata !36, metadata !DIExpression()), !dbg !57
  %local_integer_uplevel.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %local_integer_uplevel.slot, metadata !37, metadata !DIExpression()), !dbg !57
  %local_integer.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %local_integer.slot, metadata !38, metadata !DIExpression()), !dbg !57
  %param_record_uplevel.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %param_record_uplevel.slot, metadata !39, metadata !DIExpression()), !dbg !57
  %param_record.slot = alloca { i64 }
  call void @llvm.dbg.declare(metadata ptr %param_record.slot, metadata !40, metadata !DIExpression()), !dbg !57
  %param_integer_uplevel.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %param_integer_uplevel.slot, metadata !41, metadata !DIExpression()), !dbg !57
  %param_integer.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %param_integer.slot, metadata !42, metadata !DIExpression()), !dbg !57
  store i64 %a.param_integer, ptr %param_integer.slot
  store i64 %a.param_integer_uplevel, ptr %param_integer_uplevel.slot
  store { i64 } %a.param_record, ptr %param_record.slot
  store { i64 } %a.param_record_uplevel, ptr %param_record_uplevel.slot
  store { i64 } zeroinitializer, ptr %local_record.slot
  store { i64 } zeroinitializer, ptr %local_record_uplevel.slot
  %t1 = getelementptr i8, ptr %local_record.slot, i64 0
  store i64 999, ptr %t1
  %t2 = getelementptr i8, ptr %local_record_uplevel.slot, i64 0
  store i64 999, ptr %t2
  %t3 = load i64, ptr %param_integer_uplevel.slot
  %t4 = load i64, ptr %a.readonly_param_integer_uplevel
  %t5 = add i64 1, %t4
  %t6 = add i64 %t3, %t5
  store i64 %t6, ptr %param_integer_uplevel.slot
  %t7 = load i64, ptr %a.var_param_integer_uplevel
  %t8 = load i64, ptr %a.readonly_param_integer_uplevel
  %t9 = add i64 2, %t8
  %t10 = add i64 %t7, %t9
  store i64 %t10, ptr %a.var_param_integer_uplevel
  %t11 = getelementptr i8, ptr %param_record_uplevel.slot, i64 0
  %t12 = load i64, ptr %t11
  %t13 = getelementptr i8, ptr %a.readonly_param_record_uplevel, i64 0
  %t14 = load i64, ptr %t13
  %t15 = add i64 3, %t14
  %t16 = add i64 %t12, %t15
  store i64 %t16, ptr %t11
  %t17 = getelementptr i8, ptr %a.var_param_record_uplevel, i64 0
  %t18 = load i64, ptr %t17
  %t19 = getelementptr i8, ptr %a.readonly_param_record_uplevel, i64 0
  %t20 = load i64, ptr %t19
  %t21 = add i64 4, %t20
  %t22 = add i64 %t18, %t21
  store i64 %t22, ptr %t17
  %t23 = load i64, ptr %local_integer_uplevel.slot
  %t24 = add i64 %t23, 5
  store i64 %t24, ptr %local_integer_uplevel.slot
  %t25 = getelementptr i8, ptr %local_record_uplevel.slot, i64 0
  %t26 = load i64, ptr %t25
  %t27 = add i64 %t26, 6
  store i64 %t27, ptr %t25
  %t28 = load i64, ptr %param_integer.slot
  %t29 = load i64, ptr %a.readonly_param_integer
  %t30 = add i64 7, %t29
  %t31 = add i64 %t28, %t30
  store i64 %t31, ptr %param_integer.slot
  %t32 = load i64, ptr %a.var_param_integer
  %t33 = load i64, ptr %a.readonly_param_integer
  %t34 = add i64 8, %t33
  %t35 = add i64 %t32, %t34
  store i64 %t35, ptr %a.var_param_integer
  %t36 = getelementptr i8, ptr %param_record.slot, i64 0
  %t37 = load i64, ptr %t36
  %t38 = getelementptr i8, ptr %a.readonly_param_record, i64 0
  %t39 = load i64, ptr %t38
  %t40 = add i64 9, %t39
  %t41 = add i64 %t37, %t40
  store i64 %t41, ptr %t36
  %t42 = getelementptr i8, ptr %a.var_param_record, i64 0
  %t43 = load i64, ptr %t42
  %t44 = getelementptr i8, ptr %a.readonly_param_record, i64 0
  %t45 = load i64, ptr %t44
  %t46 = add i64 10, %t45
  %t47 = add i64 %t43, %t46
  store i64 %t47, ptr %t42
  %t48 = load i64, ptr %local_integer.slot
  %t49 = add i64 %t48, 11
  store i64 %t49, ptr %local_integer.slot
  %t50 = getelementptr i8, ptr %local_record.slot, i64 0
  %t51 = load i64, ptr %t50
  %t52 = add i64 %t51, 12
  store i64 %t52, ptr %t50
  %t53 = load i64, ptr %a.readonly_param_integer_uplevel
  call void @Main__Main__F1(ptr %param_integer_uplevel.slot, i64 %t53, ptr %a.var_param_integer_uplevel, ptr %param_record_uplevel.slot, ptr %a.readonly_param_record_uplevel, ptr %a.var_param_record_uplevel, ptr %local_integer_uplevel.slot, ptr %local_record_uplevel.slot), !dbg !70
  %t55 = getelementptr i8, ptr %t54, i64 0
  store ptr %param_integer_uplevel.slot, ptr %t55
  %t57 = load i64, ptr %a.readonly_param_integer_uplevel
  store i64 %t57, ptr %t56
  %t58 = getelementptr i8, ptr %t54, i64 8
  store ptr %t56, ptr %t58
  %t59 = getelementptr i8, ptr %t54, i64 16
  store ptr %a.var_param_integer_uplevel, ptr %t59
  %t60 = getelementptr i8, ptr %t54, i64 24
  store ptr %param_record_uplevel.slot, ptr %t60
  %t61 = getelementptr i8, ptr %t54, i64 32
  store ptr %a.readonly_param_record_uplevel, ptr %t61
  %t62 = getelementptr i8, ptr %t54, i64 40
  store ptr %a.var_param_record_uplevel, ptr %t62
  %t63 = getelementptr i8, ptr %t54, i64 48
  store ptr %local_integer_uplevel.slot, ptr %t63
  %t64 = getelementptr i8, ptr %t54, i64 56
  store ptr %local_record_uplevel.slot, ptr %t64
  %t66 = getelementptr i8, ptr %t65, i64 0
  store i64 -1, ptr %t66
  %t67 = getelementptr i8, ptr %t65, i64 8
  store ptr @Main__Main__F1__shim, ptr %t67
  %t68 = getelementptr i8, ptr %t65, i64 16
  store ptr %t54, ptr %t68
  call void @Main__F2(ptr %t65), !dbg !56
  %t69 = load i64, ptr %param_integer.slot
  call void @Main__A(i64 %t69), !dbg !71
  %t70 = load i64, ptr %param_integer_uplevel.slot
  call void @Main__A(i64 %t70), !dbg !72
  %t71 = load i64, ptr %a.var_param_integer
  call void @Main__A(i64 %t71), !dbg !73
  %t72 = load i64, ptr %a.var_param_integer_uplevel
  call void @Main__A(i64 %t72), !dbg !74
  %t73 = load i64, ptr %a.readonly_param_integer
  call void @Main__A(i64 %t73), !dbg !75
  %t74 = load i64, ptr %a.readonly_param_integer_uplevel
  call void @Main__A(i64 %t74), !dbg !76
  %t75 = getelementptr i8, ptr %param_record.slot, i64 0
  %t76 = load i64, ptr %t75
  call void @Main__A(i64 %t76), !dbg !77
  %t77 = getelementptr i8, ptr %param_record_uplevel.slot, i64 0
  %t78 = load i64, ptr %t77
  call void @Main__A(i64 %t78), !dbg !78
  %t79 = getelementptr i8, ptr %a.var_param_record, i64 0
  %t80 = load i64, ptr %t79
  call void @Main__A(i64 %t80), !dbg !79
  %t81 = getelementptr i8, ptr %a.var_param_record_uplevel, i64 0
  %t82 = load i64, ptr %t81
  call void @Main__A(i64 %t82), !dbg !80
  %t83 = getelementptr i8, ptr %a.readonly_param_record, i64 0
  %t84 = load i64, ptr %t83
  call void @Main__A(i64 %t84), !dbg !81
  %t85 = getelementptr i8, ptr %a.readonly_param_record_uplevel, i64 0
  %t86 = load i64, ptr %t85
  call void @Main__A(i64 %t86), !dbg !82
  br i1 0, label %if.then.1, label %if.merge.2
if.then.1:
  %t88 = getelementptr i8, ptr %t87, i64 0
  store i64 999, ptr %t88
  %t89 = load { i64 }, ptr %t87
  store { i64 } %t89, ptr %_result_ptr
  ret void
if.merge.2:
  br i1 0, label %if.then.3, label %if.merge.4
if.then.3:
  %t90 = load { i64 }, ptr %param_record.slot
  store { i64 } %t90, ptr %_result_ptr
  ret void
if.merge.4:
  br i1 0, label %if.then.5, label %if.merge.6
if.then.5:
  %t91 = load { i64 }, ptr %param_record_uplevel.slot
  store { i64 } %t91, ptr %_result_ptr
  ret void
if.merge.6:
  br i1 0, label %if.then.7, label %if.merge.8
if.then.7:
  %t92 = load { i64 }, ptr %a.var_param_record
  store { i64 } %t92, ptr %_result_ptr
  ret void
if.merge.8:
  br i1 0, label %if.then.9, label %if.merge.10
if.then.9:
  %t93 = load { i64 }, ptr %a.var_param_record_uplevel
  store { i64 } %t93, ptr %_result_ptr
  ret void
if.merge.10:
  br i1 0, label %if.then.11, label %if.merge.12
if.then.11:
  %t94 = load { i64 }, ptr %a.readonly_param_record
  store { i64 } %t94, ptr %_result_ptr
  ret void
if.merge.12:
  br i1 0, label %if.then.13, label %if.merge.14
if.then.13:
  %t95 = load { i64 }, ptr %a.readonly_param_record_uplevel
  store { i64 } %t95, ptr %_result_ptr
  ret void
if.merge.14:
  br i1 0, label %if.then.15, label %if.merge.16
if.then.15:
  %t96 = load { i64 }, ptr %local_record_uplevel.slot
  store { i64 } %t96, ptr %_result_ptr
  ret void
if.merge.16:
  br i1 0, label %if.then.17, label %if.merge.18
if.then.17:
  %t97 = load { i64 }, ptr %local_record.slot
  store { i64 } %t97, ptr %_result_ptr
  ret void
if.merge.18:
  %t99 = getelementptr i8, ptr %t98, i64 0
  store i64 999, ptr %t99
  %t100 = load { i64 }, ptr %t98
  store { i64 } %t100, ptr %_result_ptr
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t23 = alloca { i64 }
  %t16 = alloca { i64 }
  %t13 = alloca { i64 }
  %t10 = alloca { i64 }
  %t7 = alloca { i64 }
  %t4 = alloca { i64 }
  %t1 = alloca { i64 }
  store i64 1000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  store i64 2000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  store i64 3000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  store i64 4000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
  store i64 5000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  store i64 6000, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)
  %t2 = getelementptr i8, ptr %t1, i64 0
  store i64 7000, ptr %t2
  %t3 = load { i64 }, ptr %t1
  store { i64 } %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 248)
  %t5 = getelementptr i8, ptr %t4, i64 0
  store i64 8000, ptr %t5
  %t6 = load { i64 }, ptr %t4
  store { i64 } %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 256)
  %t8 = getelementptr i8, ptr %t7, i64 0
  store i64 9000, ptr %t8
  %t9 = load { i64 }, ptr %t7
  store { i64 } %t9, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 264)
  %t11 = getelementptr i8, ptr %t10, i64 0
  store i64 10000, ptr %t11
  %t12 = load { i64 }, ptr %t10
  store { i64 } %t12, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 272)
  %t14 = getelementptr i8, ptr %t13, i64 0
  store i64 11000, ptr %t14
  %t15 = load { i64 }, ptr %t13
  store { i64 } %t15, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 280)
  %t17 = getelementptr i8, ptr %t16, i64 0
  store i64 12000, ptr %t17
  %t18 = load { i64 }, ptr %t16
  store { i64 } %t18, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 288)
  %t19 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %t20 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t21 = load { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 248)
  %t22 = load { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 256)
  call void @Main__Main(ptr %t23, i64 %t19, i64 %t20, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240), { i64 } %t21, { i64 } %t22, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 264), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 272), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 280), ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 288))
  %t24 = load { i64 }, ptr %t23
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8))
  %t25 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  call void @Main__A(i64 %t25)
  %t26 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  call void @Main__A(i64 %t26)
  %t27 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  call void @Main__A(i64 %t27)
  %t28 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
  call void @Main__A(i64 %t28)
  %t29 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  call void @Main__A(i64 %t29)
  %t30 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)
  call void @Main__A(i64 %t30)
  %t31 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 248), i64 0
  %t32 = load i64, ptr %t31
  call void @Main__A(i64 %t32)
  %t33 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 256), i64 0
  %t34 = load i64, ptr %t33
  call void @Main__A(i64 %t34)
  %t35 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 264), i64 0
  %t36 = load i64, ptr %t35
  call void @Main__A(i64 %t36)
  %t37 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 272), i64 0
  %t38 = load i64, ptr %t37
  call void @Main__A(i64 %t38)
  %t39 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 280), i64 0
  %t40 = load i64, ptr %t39
  call void @Main__A(i64 %t40)
  %t41 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 288), i64 0
  %t42 = load i64, ptr %t41
  call void @Main__A(i64 %t42)
  call void @RTIO__Flush()
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

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_401812086 = internal global %TC_t {
  i64 0,
  i64 401812086,
  i64 u0x0319725f14ea5829,
  i8 1,
  i8 1,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 8,
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
declare ptr @RTIO_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTIO_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [192 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_401812086,  ; type_cells (+8)
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
  [192 x i8] zeroinitializer  ; user globals (192 bytes)
}
@Main__xparam_integer = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
@Main__xparam_integer_uplevel = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
@Main__xvar_param_integer = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
@Main__xvar_param_integer_uplevel = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
@Main__xreadonly_param_integer = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
@Main__xreadonly_param_integer_uplevel = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 240)
@Main__xparam_record = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 248)
@Main__xparam_record_uplevel = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 256)
@Main__xvar_param_record = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 264)
@Main__xvar_param_record_uplevel = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 272)
@Main__xreadonly_param_record = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 280)
@Main__xreadonly_param_record_uplevel = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 288)

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
!16 = distinct !DISubprogram(name: "Main__A", linkageName: "Main__A", scope: !4, file: !3, line: 7, type: !6, scopeLine: 7, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__F2", linkageName: "Main__F2", scope: !4, file: !3, line: 13, type: !6, scopeLine: 13, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__Main__F1", linkageName: "Main__Main__F1", scope: !4, file: !3, line: 37, type: !6, scopeLine: 37, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__Main", linkageName: "Main__Main", scope: !4, file: !3, line: 18, type: !6, scopeLine: 18, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!23 = !DILocation(line: 0, column: 0, scope: !22)
!24 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 7, type: !7)
!25 = !DILocalVariable(name: "p", scope: !18, file: !3, line: 13, type: !15)
!35 = !DILocalVariable(name: "local_record_uplevel", scope: !22, file: !3, line: 18, type: !26)
!36 = !DILocalVariable(name: "local_record", scope: !22, file: !3, line: 18, type: !26)
!37 = !DILocalVariable(name: "local_integer_uplevel", scope: !22, file: !3, line: 18, type: !7)
!38 = !DILocalVariable(name: "local_integer", scope: !22, file: !3, line: 18, type: !7)
!39 = !DILocalVariable(name: "param_record_uplevel", scope: !22, file: !3, line: 18, type: !26)
!40 = !DILocalVariable(name: "param_record", scope: !22, file: !3, line: 18, type: !26)
!41 = !DILocalVariable(name: "param_integer_uplevel", scope: !22, file: !3, line: 18, type: !7)
!42 = !DILocalVariable(name: "param_integer", scope: !22, file: !3, line: 18, type: !7)
!26 = !DICompositeType(tag: DW_TAG_structure_type, name: "", size: 64, elements: !27)
!27 = !{!28}
!28 = !DIDerivedType(tag: DW_TAG_member, name: "a", baseType: !7, size: 64, offset: 0)
!29 = !DICompositeType(tag: DW_TAG_array_type, baseType: !15, size: 192, elements: !30)
!30 = !{!31}
!31 = !DISubrange(count: 24)
!32 = !DICompositeType(tag: DW_TAG_array_type, baseType: !15, size: 512, elements: !33)
!33 = !{!34}
!34 = !DISubrange(count: 8)
!43 = !DILocation(line: 7, column: 0, scope: !16)
!44 = !DILocation(line: 9, column: 0, scope: !16)
!45 = !DILocation(line: 10, column: 0, scope: !16)
!46 = !DILocation(line: 13, column: 0, scope: !18)
!47 = !DILocation(line: 15, column: 0, scope: !18)
!48 = !DILocation(line: 39, column: 0, scope: !20)
!49 = !DILocation(line: 40, column: 0, scope: !20)
!50 = !DILocation(line: 41, column: 0, scope: !20)
!51 = !DILocation(line: 42, column: 0, scope: !20)
!52 = !DILocation(line: 43, column: 0, scope: !20)
!53 = !DILocation(line: 44, column: 0, scope: !20)
!54 = !DILocation(line: 84, column: 0, scope: !22)
!55 = !DILocation(line: 75, column: 0, scope: !22)
!56 = !DILocation(line: 60, column: 0, scope: !22)
!57 = !DILocation(line: 18, column: 0, scope: !22)
!58 = !DILocation(line: 47, column: 0, scope: !22)
!59 = !DILocation(line: 48, column: 0, scope: !22)
!60 = !DILocation(line: 49, column: 0, scope: !22)
!61 = !DILocation(line: 50, column: 0, scope: !22)
!62 = !DILocation(line: 51, column: 0, scope: !22)
!63 = !DILocation(line: 52, column: 0, scope: !22)
!64 = !DILocation(line: 53, column: 0, scope: !22)
!65 = !DILocation(line: 54, column: 0, scope: !22)
!66 = !DILocation(line: 55, column: 0, scope: !22)
!67 = !DILocation(line: 56, column: 0, scope: !22)
!68 = !DILocation(line: 57, column: 0, scope: !22)
!69 = !DILocation(line: 58, column: 0, scope: !22)
!70 = !DILocation(line: 59, column: 0, scope: !22)
!71 = !DILocation(line: 62, column: 0, scope: !22)
!72 = !DILocation(line: 63, column: 0, scope: !22)
!73 = !DILocation(line: 64, column: 0, scope: !22)
!74 = !DILocation(line: 65, column: 0, scope: !22)
!75 = !DILocation(line: 66, column: 0, scope: !22)
!76 = !DILocation(line: 67, column: 0, scope: !22)
!77 = !DILocation(line: 68, column: 0, scope: !22)
!78 = !DILocation(line: 69, column: 0, scope: !22)
!79 = !DILocation(line: 70, column: 0, scope: !22)
!80 = !DILocation(line: 71, column: 0, scope: !22)
!81 = !DILocation(line: 72, column: 0, scope: !22)
!82 = !DILocation(line: 73, column: 0, scope: !22)
!83 = !DILocation(line: 76, column: 0, scope: !22)
!84 = !DILocation(line: 77, column: 0, scope: !22)
!85 = !DILocation(line: 78, column: 0, scope: !22)
!86 = !DILocation(line: 79, column: 0, scope: !22)
!87 = !DILocation(line: 80, column: 0, scope: !22)
!88 = !DILocation(line: 81, column: 0, scope: !22)
!89 = !DILocation(line: 82, column: 0, scope: !22)
!90 = !DILocation(line: 83, column: 0, scope: !22)
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
