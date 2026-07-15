; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @RTIO__PutText(ptr)
declare void @RTIO__Flush()
declare void @RTIO__PutAddr(ptr, i64)
declare void @RTIO__PutInt(i64, i64)
declare double @Time__Now()
declare void @RTIO__PutF(double)
declare void @Date__FromTime(ptr, double, ptr)
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)

define void @Main__NL() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8)), !dbg !18
  call void @RTIO__Flush(), !dbg !19
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t56 = alloca { i64, i8, i8, i8, i8, i8, i64, ptr, i8 }
  %t26 = alloca { i64, i8, i8, i8, i8, i8, i64, ptr, i8 }
  %t1 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 0
  store i64 0, ptr %t1
  %t2 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 8
  store i8 0, ptr %t2
  %t3 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 9
  store i8 1, ptr %t3
  %t4 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 10
  store i8 0, ptr %t4
  %t5 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 11
  store i8 0, ptr %t5
  %t6 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 12
  store i8 0, ptr %t6
  %t7 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 16
  store i64 0, ptr %t7
  %t8 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 24
  store ptr null, ptr %t8
  %t9 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 32
  store i8 0, ptr %t9
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8))
  call void @RTIO__PutAddr(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8))
  %t10 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 0
  call void @RTIO__PutAddr(ptr %t10, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8))
  %t11 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 8
  call void @RTIO__PutAddr(ptr %t11, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_4, i64 8))
  %t12 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 9
  call void @RTIO__PutAddr(ptr %t12, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_5, i64 8))
  %t13 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 10
  call void @RTIO__PutAddr(ptr %t13, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_6, i64 8))
  %t14 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 11
  call void @RTIO__PutAddr(ptr %t14, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_7, i64 8))
  %t15 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 12
  call void @RTIO__PutAddr(ptr %t15, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_8, i64 8))
  %t16 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 16
  call void @RTIO__PutAddr(ptr %t16, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_9, i64 8))
  %t17 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 24
  call void @RTIO__PutAddr(ptr %t17, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_10, i64 8))
  %t18 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 32
  call void @RTIO__PutAddr(ptr %t18, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_11, i64 8))
  call void @RTIO__PutInt(i64 40, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_12, i64 8))
  call void @RTIO__PutInt(i64 8, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_13, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_14, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_15, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_16, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_17, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_18, i64 8))
  call void @RTIO__PutInt(i64 8, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_19, i64 8))
  call void @RTIO__PutInt(i64 8, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_20, i64 8))
  call void @RTIO__PutInt(i64 1, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_21, i64 8))
  %t19 = call double @Time__Now()
  call void @RTIO__PutF(double %t19)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_22, i64 8))
  %t20 = call double @Time__Now()
  %t21 = fsub double %t20, 0x4205b08488000000
  call void @RTIO__PutF(double %t21)
  call void @Main__NL()
  %t22 = call double @Time__Now()
  %t23 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t24 = getelementptr i8, ptr %t23, i64 104
  %t25 = load ptr, ptr %t24
  call void @Date__FromTime(ptr %t26, double %t22, ptr %t25)
  %t27 = load { i64, i8, i8, i8, i8, i8, i64, ptr, i8 }, ptr %t26
  store { i64, i8, i8, i8, i8, i8, i64, ptr, i8 } %t27, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_23, i64 8))
  %t28 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 0
  %t29 = load i64, ptr %t28
  call void @RTIO__PutInt(i64 %t29, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_24, i64 8))
  %t30 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 8
  %t31 = load i8, ptr %t30
  %t32 = sext i8 %t31 to i64
  call void @RTIO__PutInt(i64 %t32, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_25, i64 8))
  %t33 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 9
  %t34 = load i8, ptr %t33
  %t35 = zext i8 %t34 to i64
  call void @RTIO__PutInt(i64 %t35, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_26, i64 8))
  %t36 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 10
  %t37 = load i8, ptr %t36
  %t38 = zext i8 %t37 to i64
  call void @RTIO__PutInt(i64 %t38, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_27, i64 8))
  %t39 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 11
  %t40 = load i8, ptr %t39
  %t41 = zext i8 %t40 to i64
  call void @RTIO__PutInt(i64 %t41, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_28, i64 8))
  %t42 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 12
  %t43 = load i8, ptr %t42
  %t44 = zext i8 %t43 to i64
  call void @RTIO__PutInt(i64 %t44, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_29, i64 8))
  %t45 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 16
  %t46 = load i64, ptr %t45
  call void @RTIO__PutInt(i64 %t46, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_30, i64 8))
  %t47 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 24
  %t48 = load ptr, ptr %t47
  call void @RTIO__PutText(ptr %t48)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_31, i64 8))
  %t49 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 32
  %t50 = load i8, ptr %t49
  %t51 = sext i8 %t50 to i64
  call void @RTIO__PutInt(i64 %t51, i64 0)
  call void @Main__NL()
  %t52 = call double @Time__Now()
  %t53 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t54 = getelementptr i8, ptr %t53, i64 112
  %t55 = load ptr, ptr %t54
  call void @Date__FromTime(ptr %t56, double %t52, ptr %t55)
  %t57 = load { i64, i8, i8, i8, i8, i8, i64, ptr, i8 }, ptr %t56
  store { i64, i8, i8, i8, i8, i8, i64, ptr, i8 } %t57, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_32, i64 8))
  %t58 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 0
  %t59 = load i64, ptr %t58
  call void @RTIO__PutInt(i64 %t59, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_33, i64 8))
  %t60 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 8
  %t61 = load i8, ptr %t60
  %t62 = sext i8 %t61 to i64
  call void @RTIO__PutInt(i64 %t62, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_34, i64 8))
  %t63 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 9
  %t64 = load i8, ptr %t63
  %t65 = zext i8 %t64 to i64
  call void @RTIO__PutInt(i64 %t65, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_35, i64 8))
  %t66 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 10
  %t67 = load i8, ptr %t66
  %t68 = zext i8 %t67 to i64
  call void @RTIO__PutInt(i64 %t68, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_36, i64 8))
  %t69 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 11
  %t70 = load i8, ptr %t69
  %t71 = zext i8 %t70 to i64
  call void @RTIO__PutInt(i64 %t71, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_37, i64 8))
  %t72 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 12
  %t73 = load i8, ptr %t72
  %t74 = zext i8 %t73 to i64
  call void @RTIO__PutInt(i64 %t74, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_38, i64 8))
  %t75 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 16
  %t76 = load i64, ptr %t75
  call void @RTIO__PutInt(i64 %t76, i64 0)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_39, i64 8))
  %t77 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 24
  %t78 = load ptr, ptr %t77
  call void @RTIO__PutText(ptr %t78)
  call void @Main__NL()
  call void @RTIO__PutText(ptr getelementptr inbounds (i8, ptr @textlit_40, i64 8))
  %t79 = getelementptr i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i64 32
  %t80 = load i8, ptr %t79
  %t81 = sext i8 %t80 to i64
  call void @RTIO__PutInt(i64 %t81, i64 0)
  call void @Main__NL()
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
@textlit_1 = internal constant { i64, ptr, i64, [4 x i8] } { i64 2, ptr @textlit_methods, i64 3, [4 x i8] c"&d \00" }
@textlit_2 = internal constant { i64, ptr, i64, [7 x i8] } { i64 2, ptr @textlit_methods, i64 6, [7 x i8] c"&year \00" }
@textlit_3 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"&month \00" }
@textlit_4 = internal constant { i64, ptr, i64, [6 x i8] } { i64 2, ptr @textlit_methods, i64 5, [6 x i8] c"&day \00" }
@textlit_5 = internal constant { i64, ptr, i64, [7 x i8] } { i64 2, ptr @textlit_methods, i64 6, [7 x i8] c"&hour \00" }
@textlit_6 = internal constant { i64, ptr, i64, [9 x i8] } { i64 2, ptr @textlit_methods, i64 8, [9 x i8] c"&minute \00" }
@textlit_7 = internal constant { i64, ptr, i64, [9 x i8] } { i64 2, ptr @textlit_methods, i64 8, [9 x i8] c"&second \00" }
@textlit_8 = internal constant { i64, ptr, i64, [9 x i8] } { i64 2, ptr @textlit_methods, i64 8, [9 x i8] c"&offset \00" }
@textlit_9 = internal constant { i64, ptr, i64, [7 x i8] } { i64 2, ptr @textlit_methods, i64 6, [7 x i8] c"&zone \00" }
@textlit_10 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"&weekday \00" }
@textlit_11 = internal constant { i64, ptr, i64, [8 x i8] } { i64 2, ptr @textlit_methods, i64 7, [8 x i8] c"size T \00" }
@textlit_12 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"size year \00" }
@textlit_13 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"size month \00" }
@textlit_14 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"size day \00" }
@textlit_15 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"size hour \00" }
@textlit_16 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"size minute \00" }
@textlit_17 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"size second \00" }
@textlit_18 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"size offset \00" }
@textlit_19 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"size zone \00" }
@textlit_20 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"size weekday \00" }
@textlit_21 = internal constant { i64, ptr, i64, [5 x i8] } { i64 2, ptr @textlit_methods, i64 4, [5 x i8] c"now \00" }
@textlit_22 = internal constant { i64, ptr, i64, [48 x i8] } { i64 2, ptr @textlit_methods, i64 47, [48 x i8] c"or possibly the posix value printed from Win32 \00" }
@textlit_23 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"local year \00" }
@textlit_24 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"local month \00" }
@textlit_25 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"local day \00" }
@textlit_26 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"local hour \00" }
@textlit_27 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"local minute \00" }
@textlit_28 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"local second \00" }
@textlit_29 = internal constant { i64, ptr, i64, [14 x i8] } { i64 2, ptr @textlit_methods, i64 13, [14 x i8] c"local offset \00" }
@textlit_30 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"local zone \00" }
@textlit_31 = internal constant { i64, ptr, i64, [15 x i8] } { i64 2, ptr @textlit_methods, i64 14, [15 x i8] c"local weekday \00" }
@textlit_32 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"utc year \00" }
@textlit_33 = internal constant { i64, ptr, i64, [11 x i8] } { i64 2, ptr @textlit_methods, i64 10, [11 x i8] c"utc month \00" }
@textlit_34 = internal constant { i64, ptr, i64, [9 x i8] } { i64 2, ptr @textlit_methods, i64 8, [9 x i8] c"utc day \00" }
@textlit_35 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"utc hour \00" }
@textlit_36 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"utc minute \00" }
@textlit_37 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"utc second \00" }
@textlit_38 = internal constant { i64, ptr, i64, [12 x i8] } { i64 2, ptr @textlit_methods, i64 11, [12 x i8] c"utc offset \00" }
@textlit_39 = internal constant { i64, ptr, i64, [10 x i8] } { i64 2, ptr @textlit_methods, i64 9, [10 x i8] c"utc zone \00" }
@textlit_40 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c"utc weekday \00" }

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Time_I3(i64)
declare ptr @RTIO_I3(i64)
declare ptr @Date_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Time_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTIO_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Date_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [40 x i8] }
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
  [40 x i8] zeroinitializer  ; user globals (40 bytes)
}
@Main__d = alias { i64, i8, i8, i8, i8, i8, i64, ptr, i8 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)

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
!16 = distinct !DISubprogram(name: "Main__NL", linkageName: "Main__NL", scope: !4, file: !3, line: 12, type: !6, scopeLine: 12, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocation(line: 14, column: 0, scope: !16)
!19 = !DILocation(line: 15, column: 0, scope: !16)
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
