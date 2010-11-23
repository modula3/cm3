	.text
.no_dead_strip _Minus__uMinus_var_u16_u16
_Minus__uMinus_var_u16_u16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _Minus__Minus_var_u16_u16
_Minus__Minus_var_u16_u16:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L3
	movl	$1761, %edi
	call	__m3_fault
L3:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI5:
	ret
LFE1:
.no_dead_strip _Minus__uMinus_param_u16_u16
_Minus__uMinus_param_u16_u16:
LFB2:
	pushq	%rbp
LCFI6:
	movq	%rsp, %rbp
LCFI7:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI8:
	ret
LFE2:
.no_dead_strip _Minus__Minus_param_u16_u16
_Minus__Minus_param_u16_u16:
LFB3:
	pushq	%rbp
LCFI9:
	movq	%rsp, %rbp
LCFI10:
	subq	$16, %rsp
LCFI11:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L6
	movl	$1825, %edi
	call	__m3_fault
L6:
	movzwl	-2(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI12:
	ret
LFE3:
.no_dead_strip _Minus__uMinus_var_u16_u32
_Minus__uMinus_var_u16_u32:
LFB4:
	pushq	%rbp
LCFI13:
	movq	%rsp, %rbp
LCFI14:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI15:
	ret
LFE4:
.no_dead_strip _Minus__Minus_var_u16_u32
_Minus__Minus_var_u16_u32:
LFB5:
	pushq	%rbp
LCFI16:
	movq	%rsp, %rbp
LCFI17:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L9
	movl	$1889, %edi
	call	__m3_fault
L9:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI18:
	ret
LFE5:
.no_dead_strip _Minus__uMinus_param_u16_u32
_Minus__uMinus_param_u16_u32:
LFB6:
	pushq	%rbp
LCFI19:
	movq	%rsp, %rbp
LCFI20:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI21:
	ret
LFE6:
.no_dead_strip _Minus__Minus_param_u16_u32
_Minus__Minus_param_u16_u32:
LFB7:
	pushq	%rbp
LCFI22:
	movq	%rsp, %rbp
LCFI23:
	subq	$16, %rsp
LCFI24:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L12
	movl	$1953, %edi
	call	__m3_fault
L12:
	movzwl	-2(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI25:
	ret
LFE7:
.no_dead_strip _Minus__uMinus_var_u16_LC
_Minus__uMinus_var_u16_LC:
LFB8:
	pushq	%rbp
LCFI26:
	movq	%rsp, %rbp
LCFI27:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI28:
	ret
LFE8:
.no_dead_strip _Minus__Minus_var_u16_LC
_Minus__Minus_var_u16_LC:
LFB9:
	pushq	%rbp
LCFI29:
	movq	%rsp, %rbp
LCFI30:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI31:
	ret
LFE9:
.no_dead_strip _Minus__uMinus_param_u16_LC
_Minus__uMinus_param_u16_LC:
LFB10:
	pushq	%rbp
LCFI32:
	movq	%rsp, %rbp
LCFI33:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI34:
	ret
LFE10:
.no_dead_strip _Minus__Minus_param_u16_LC
_Minus__Minus_param_u16_LC:
LFB11:
	pushq	%rbp
LCFI35:
	movq	%rsp, %rbp
LCFI36:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI37:
	ret
LFE11:
.no_dead_strip _Minus__uMinus_var_u16_I
_Minus__uMinus_var_u16_I:
LFB12:
	pushq	%rbp
LCFI38:
	movq	%rsp, %rbp
LCFI39:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI40:
	ret
LFE12:
.no_dead_strip _Minus__Minus_var_u16_I
_Minus__Minus_var_u16_I:
LFB13:
	pushq	%rbp
LCFI41:
	movq	%rsp, %rbp
LCFI42:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L19
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L20
L19:
	movl	$2145, %edi
	call	__m3_fault
L20:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI43:
	ret
LFE13:
.no_dead_strip _Minus__uMinus_param_u16_I
_Minus__uMinus_param_u16_I:
LFB14:
	pushq	%rbp
LCFI44:
	movq	%rsp, %rbp
LCFI45:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI46:
	ret
LFE14:
.no_dead_strip _Minus__Minus_param_u16_I
_Minus__Minus_param_u16_I:
LFB15:
	pushq	%rbp
LCFI47:
	movq	%rsp, %rbp
LCFI48:
	subq	$16, %rsp
LCFI49:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L23
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	cmpq	$65535, %rax
	jle	L24
L23:
	movl	$2209, %edi
	call	__m3_fault
L24:
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI50:
	ret
LFE15:
.no_dead_strip _Minus__uMinus_var_u16_i16
_Minus__uMinus_var_u16_i16:
LFB16:
	pushq	%rbp
LCFI51:
	movq	%rsp, %rbp
LCFI52:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI53:
	ret
LFE16:
.no_dead_strip _Minus__Minus_var_u16_i16
_Minus__Minus_var_u16_i16:
LFB17:
	pushq	%rbp
LCFI54:
	movq	%rsp, %rbp
LCFI55:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L27
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L28
L27:
	movl	$2273, %edi
	call	__m3_fault
L28:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI56:
	ret
LFE17:
.no_dead_strip _Minus__uMinus_param_u16_i16
_Minus__uMinus_param_u16_i16:
LFB18:
	pushq	%rbp
LCFI57:
	movq	%rsp, %rbp
LCFI58:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI59:
	ret
LFE18:
.no_dead_strip _Minus__Minus_param_u16_i16
_Minus__Minus_param_u16_i16:
LFB19:
	pushq	%rbp
LCFI60:
	movq	%rsp, %rbp
LCFI61:
	subq	$16, %rsp
LCFI62:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L31
	movzwl	-2(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L32
L31:
	movl	$2337, %edi
	call	__m3_fault
L32:
	movzwl	-2(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI63:
	ret
LFE19:
.no_dead_strip _Minus__uMinus_var_u16_u64
_Minus__uMinus_var_u16_u64:
LFB20:
	pushq	%rbp
LCFI64:
	movq	%rsp, %rbp
LCFI65:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI66:
	ret
LFE20:
.no_dead_strip _Minus__Minus_var_u16_u64
_Minus__Minus_var_u16_u64:
LFB21:
	pushq	%rbp
LCFI67:
	movq	%rsp, %rbp
LCFI68:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI69:
	ret
LFE21:
.no_dead_strip _Minus__uMinus_param_u16_u64
_Minus__uMinus_param_u16_u64:
LFB22:
	pushq	%rbp
LCFI70:
	movq	%rsp, %rbp
LCFI71:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI72:
	ret
LFE22:
.no_dead_strip _Minus__Minus_param_u16_u64
_Minus__Minus_param_u16_u64:
LFB23:
	pushq	%rbp
LCFI73:
	movq	%rsp, %rbp
LCFI74:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI75:
	ret
LFE23:
.no_dead_strip _Minus__uMinus_var_u16_i32
_Minus__uMinus_var_u16_i32:
LFB24:
	pushq	%rbp
LCFI76:
	movq	%rsp, %rbp
LCFI77:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI78:
	ret
LFE24:
.no_dead_strip _Minus__Minus_var_u16_i32
_Minus__Minus_var_u16_i32:
LFB25:
	pushq	%rbp
LCFI79:
	movq	%rsp, %rbp
LCFI80:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L39
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L40
L39:
	movl	$2529, %edi
	call	__m3_fault
L40:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI81:
	ret
LFE25:
.no_dead_strip _Minus__uMinus_param_u16_i32
_Minus__uMinus_param_u16_i32:
LFB26:
	pushq	%rbp
LCFI82:
	movq	%rsp, %rbp
LCFI83:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI84:
	ret
LFE26:
.no_dead_strip _Minus__Minus_param_u16_i32
_Minus__Minus_param_u16_i32:
LFB27:
	pushq	%rbp
LCFI85:
	movq	%rsp, %rbp
LCFI86:
	subq	$16, %rsp
LCFI87:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L43
	movzwl	-2(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L44
L43:
	movl	$2593, %edi
	call	__m3_fault
L44:
	movzwl	-2(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI88:
	ret
LFE27:
.no_dead_strip _Minus__uMinus_var_u16_C
_Minus__uMinus_var_u16_C:
LFB28:
	pushq	%rbp
LCFI89:
	movq	%rsp, %rbp
LCFI90:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI91:
	ret
LFE28:
.no_dead_strip _Minus__Minus_var_u16_C
_Minus__Minus_var_u16_C:
LFB29:
	pushq	%rbp
LCFI92:
	movq	%rsp, %rbp
LCFI93:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L47
	movl	$2657, %edi
	call	__m3_fault
L47:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI94:
	ret
LFE29:
.no_dead_strip _Minus__uMinus_param_u16_C
_Minus__uMinus_param_u16_C:
LFB30:
	pushq	%rbp
LCFI95:
	movq	%rsp, %rbp
LCFI96:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI97:
	ret
LFE30:
.no_dead_strip _Minus__Minus_param_u16_C
_Minus__Minus_param_u16_C:
LFB31:
	pushq	%rbp
LCFI98:
	movq	%rsp, %rbp
LCFI99:
	subq	$16, %rsp
LCFI100:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L50
	movl	$2721, %edi
	call	__m3_fault
L50:
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI101:
	ret
LFE31:
.no_dead_strip _Minus__uMinus_var_u16_u8
_Minus__uMinus_var_u16_u8:
LFB32:
	pushq	%rbp
LCFI102:
	movq	%rsp, %rbp
LCFI103:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI104:
	ret
LFE32:
.no_dead_strip _Minus__Minus_var_u16_u8
_Minus__Minus_var_u16_u8:
LFB33:
	pushq	%rbp
LCFI105:
	movq	%rsp, %rbp
LCFI106:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L53
	movl	$2785, %edi
	call	__m3_fault
L53:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI107:
	ret
LFE33:
.no_dead_strip _Minus__uMinus_param_u16_u8
_Minus__uMinus_param_u16_u8:
LFB34:
	pushq	%rbp
LCFI108:
	movq	%rsp, %rbp
LCFI109:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI110:
	ret
LFE34:
.no_dead_strip _Minus__Minus_param_u16_u8
_Minus__Minus_param_u16_u8:
LFB35:
	pushq	%rbp
LCFI111:
	movq	%rsp, %rbp
LCFI112:
	subq	$16, %rsp
LCFI113:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L56
	movl	$2849, %edi
	call	__m3_fault
L56:
	movzwl	-2(%rbp), %edx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI114:
	ret
LFE35:
.no_dead_strip _Minus__uMinus_var_u16_i64
_Minus__uMinus_var_u16_i64:
LFB36:
	pushq	%rbp
LCFI115:
	movq	%rsp, %rbp
LCFI116:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI117:
	ret
LFE36:
.no_dead_strip _Minus__Minus_var_u16_i64
_Minus__Minus_var_u16_i64:
LFB37:
	pushq	%rbp
LCFI118:
	movq	%rsp, %rbp
LCFI119:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI120:
	ret
LFE37:
.no_dead_strip _Minus__uMinus_param_u16_i64
_Minus__uMinus_param_u16_i64:
LFB38:
	pushq	%rbp
LCFI121:
	movq	%rsp, %rbp
LCFI122:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI123:
	ret
LFE38:
.no_dead_strip _Minus__Minus_param_u16_i64
_Minus__Minus_param_u16_i64:
LFB39:
	pushq	%rbp
LCFI124:
	movq	%rsp, %rbp
LCFI125:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI126:
	ret
LFE39:
.no_dead_strip _Minus__uMinus_var_u16_L
_Minus__uMinus_var_u16_L:
LFB40:
	pushq	%rbp
LCFI127:
	movq	%rsp, %rbp
LCFI128:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI129:
	ret
LFE40:
.no_dead_strip _Minus__Minus_var_u16_L
_Minus__Minus_var_u16_L:
LFB41:
	pushq	%rbp
LCFI130:
	movq	%rsp, %rbp
LCFI131:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI132:
	ret
LFE41:
.no_dead_strip _Minus__uMinus_param_u16_L
_Minus__uMinus_param_u16_L:
LFB42:
	pushq	%rbp
LCFI133:
	movq	%rsp, %rbp
LCFI134:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI135:
	ret
LFE42:
.no_dead_strip _Minus__Minus_param_u16_L
_Minus__Minus_param_u16_L:
LFB43:
	pushq	%rbp
LCFI136:
	movq	%rsp, %rbp
LCFI137:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI138:
	ret
LFE43:
.no_dead_strip _Minus__uMinus_var_u16_i8
_Minus__uMinus_var_u16_i8:
LFB44:
	pushq	%rbp
LCFI139:
	movq	%rsp, %rbp
LCFI140:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI141:
	ret
LFE44:
.no_dead_strip _Minus__Minus_var_u16_i8
_Minus__Minus_var_u16_i8:
LFB45:
	pushq	%rbp
LCFI142:
	movq	%rsp, %rbp
LCFI143:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L67
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L68
L67:
	movl	$3169, %edi
	call	__m3_fault
L68:
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI144:
	ret
LFE45:
.no_dead_strip _Minus__uMinus_param_u16_i8
_Minus__uMinus_param_u16_i8:
LFB46:
	pushq	%rbp
LCFI145:
	movq	%rsp, %rbp
LCFI146:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI147:
	ret
LFE46:
.no_dead_strip _Minus__Minus_param_u16_i8
_Minus__Minus_param_u16_i8:
LFB47:
	pushq	%rbp
LCFI148:
	movq	%rsp, %rbp
LCFI149:
	subq	$16, %rsp
LCFI150:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L71
	movzwl	-2(%rbp), %edx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$65535, %rax
	jle	L72
L71:
	movl	$3233, %edi
	call	__m3_fault
L72:
	movzwl	-2(%rbp), %edx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI151:
	ret
LFE47:
.no_dead_strip _Minus__uMinus_var_u32_u16
_Minus__uMinus_var_u32_u16:
LFB48:
	pushq	%rbp
LCFI152:
	movq	%rsp, %rbp
LCFI153:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI154:
	ret
LFE48:
.no_dead_strip _Minus__Minus_var_u32_u16
_Minus__Minus_var_u32_u16:
LFB49:
	pushq	%rbp
LCFI155:
	movq	%rsp, %rbp
LCFI156:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L75
	movl	$3297, %edi
	call	__m3_fault
L75:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI157:
	ret
LFE49:
.no_dead_strip _Minus__uMinus_param_u32_u16
_Minus__uMinus_param_u32_u16:
LFB50:
	pushq	%rbp
LCFI158:
	movq	%rsp, %rbp
LCFI159:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI160:
	ret
LFE50:
.no_dead_strip _Minus__Minus_param_u32_u16
_Minus__Minus_param_u32_u16:
LFB51:
	pushq	%rbp
LCFI161:
	movq	%rsp, %rbp
LCFI162:
	subq	$16, %rsp
LCFI163:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L78
	movl	$3361, %edi
	call	__m3_fault
L78:
	mov	-4(%rbp), %edx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI164:
	ret
LFE51:
.no_dead_strip _Minus__uMinus_var_u32_u32
_Minus__uMinus_var_u32_u32:
LFB52:
	pushq	%rbp
LCFI165:
	movq	%rsp, %rbp
LCFI166:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI167:
	ret
LFE52:
.no_dead_strip _Minus__Minus_var_u32_u32
_Minus__Minus_var_u32_u32:
LFB53:
	pushq	%rbp
LCFI168:
	movq	%rsp, %rbp
LCFI169:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L81
	movl	$3425, %edi
	call	__m3_fault
L81:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI170:
	ret
LFE53:
.no_dead_strip _Minus__uMinus_param_u32_u32
_Minus__uMinus_param_u32_u32:
LFB54:
	pushq	%rbp
LCFI171:
	movq	%rsp, %rbp
LCFI172:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI173:
	ret
LFE54:
.no_dead_strip _Minus__Minus_param_u32_u32
_Minus__Minus_param_u32_u32:
LFB55:
	pushq	%rbp
LCFI174:
	movq	%rsp, %rbp
LCFI175:
	subq	$16, %rsp
LCFI176:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L84
	movl	$3489, %edi
	call	__m3_fault
L84:
	mov	-4(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI177:
	ret
LFE55:
.no_dead_strip _Minus__uMinus_var_u32_LC
_Minus__uMinus_var_u32_LC:
LFB56:
	pushq	%rbp
LCFI178:
	movq	%rsp, %rbp
LCFI179:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI180:
	ret
LFE56:
.no_dead_strip _Minus__Minus_var_u32_LC
_Minus__Minus_var_u32_LC:
LFB57:
	pushq	%rbp
LCFI181:
	movq	%rsp, %rbp
LCFI182:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI183:
	ret
LFE57:
.no_dead_strip _Minus__uMinus_param_u32_LC
_Minus__uMinus_param_u32_LC:
LFB58:
	pushq	%rbp
LCFI184:
	movq	%rsp, %rbp
LCFI185:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI186:
	ret
LFE58:
.no_dead_strip _Minus__Minus_param_u32_LC
_Minus__Minus_param_u32_LC:
LFB59:
	pushq	%rbp
LCFI187:
	movq	%rsp, %rbp
LCFI188:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI189:
	ret
LFE59:
.no_dead_strip _Minus__uMinus_var_u32_I
_Minus__uMinus_var_u32_I:
LFB60:
	pushq	%rbp
LCFI190:
	movq	%rsp, %rbp
LCFI191:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI192:
	ret
LFE60:
.no_dead_strip _Minus__Minus_var_u32_I
_Minus__Minus_var_u32_I:
LFB61:
	pushq	%rbp
LCFI193:
	movq	%rsp, %rbp
LCFI194:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L91
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L92
L91:
	movl	$3681, %edi
	call	__m3_fault
L92:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI195:
	ret
LFE61:
.no_dead_strip _Minus__uMinus_param_u32_I
_Minus__uMinus_param_u32_I:
LFB62:
	pushq	%rbp
LCFI196:
	movq	%rsp, %rbp
LCFI197:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI198:
	ret
LFE62:
.no_dead_strip _Minus__Minus_param_u32_I
_Minus__Minus_param_u32_I:
LFB63:
	pushq	%rbp
LCFI199:
	movq	%rsp, %rbp
LCFI200:
	subq	$16, %rsp
LCFI201:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L95
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	subq	-16(%rbp), %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L96
L95:
	movl	$3745, %edi
	call	__m3_fault
L96:
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI202:
	ret
LFE63:
.no_dead_strip _Minus__uMinus_var_u32_i16
_Minus__uMinus_var_u32_i16:
LFB64:
	pushq	%rbp
LCFI203:
	movq	%rsp, %rbp
LCFI204:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI205:
	ret
LFE64:
.no_dead_strip _Minus__Minus_var_u32_i16
_Minus__Minus_var_u32_i16:
LFB65:
	pushq	%rbp
LCFI206:
	movq	%rsp, %rbp
LCFI207:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L99
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L100
L99:
	movl	$3809, %edi
	call	__m3_fault
L100:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI208:
	ret
LFE65:
.no_dead_strip _Minus__uMinus_param_u32_i16
_Minus__uMinus_param_u32_i16:
LFB66:
	pushq	%rbp
LCFI209:
	movq	%rsp, %rbp
LCFI210:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI211:
	ret
LFE66:
.no_dead_strip _Minus__Minus_param_u32_i16
_Minus__Minus_param_u32_i16:
LFB67:
	pushq	%rbp
LCFI212:
	movq	%rsp, %rbp
LCFI213:
	subq	$16, %rsp
LCFI214:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L103
	mov	-4(%rbp), %edx
	movswq	-6(%rbp), %rax
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L104
L103:
	movl	$3873, %edi
	call	__m3_fault
L104:
	mov	-4(%rbp), %edx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI215:
	ret
LFE67:
.no_dead_strip _Minus__uMinus_var_u32_u64
_Minus__uMinus_var_u32_u64:
LFB68:
	pushq	%rbp
LCFI216:
	movq	%rsp, %rbp
LCFI217:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI218:
	ret
LFE68:
.no_dead_strip _Minus__Minus_var_u32_u64
_Minus__Minus_var_u32_u64:
LFB69:
	pushq	%rbp
LCFI219:
	movq	%rsp, %rbp
LCFI220:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI221:
	ret
LFE69:
.no_dead_strip _Minus__uMinus_param_u32_u64
_Minus__uMinus_param_u32_u64:
LFB70:
	pushq	%rbp
LCFI222:
	movq	%rsp, %rbp
LCFI223:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI224:
	ret
LFE70:
.no_dead_strip _Minus__Minus_param_u32_u64
_Minus__Minus_param_u32_u64:
LFB71:
	pushq	%rbp
LCFI225:
	movq	%rsp, %rbp
LCFI226:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI227:
	ret
LFE71:
.no_dead_strip _Minus__uMinus_var_u32_i32
_Minus__uMinus_var_u32_i32:
LFB72:
	pushq	%rbp
LCFI228:
	movq	%rsp, %rbp
LCFI229:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI230:
	ret
LFE72:
.no_dead_strip _Minus__Minus_var_u32_i32
_Minus__Minus_var_u32_i32:
LFB73:
	pushq	%rbp
LCFI231:
	movq	%rsp, %rbp
LCFI232:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L111
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L112
L111:
	movl	$4065, %edi
	call	__m3_fault
L112:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI233:
	ret
LFE73:
.no_dead_strip _Minus__uMinus_param_u32_i32
_Minus__uMinus_param_u32_i32:
LFB74:
	pushq	%rbp
LCFI234:
	movq	%rsp, %rbp
LCFI235:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI236:
	ret
LFE74:
.no_dead_strip _Minus__Minus_param_u32_i32
_Minus__Minus_param_u32_i32:
LFB75:
	pushq	%rbp
LCFI237:
	movq	%rsp, %rbp
LCFI238:
	subq	$16, %rsp
LCFI239:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L115
	mov	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L116
L115:
	movl	$4129, %edi
	call	__m3_fault
L116:
	mov	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI240:
	ret
LFE75:
.no_dead_strip _Minus__uMinus_var_u32_C
_Minus__uMinus_var_u32_C:
LFB76:
	pushq	%rbp
LCFI241:
	movq	%rsp, %rbp
LCFI242:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI243:
	ret
LFE76:
.no_dead_strip _Minus__Minus_var_u32_C
_Minus__Minus_var_u32_C:
LFB77:
	pushq	%rbp
LCFI244:
	movq	%rsp, %rbp
LCFI245:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L119
	movl	$4193, %edi
	call	__m3_fault
L119:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI246:
	ret
LFE77:
.no_dead_strip _Minus__uMinus_param_u32_C
_Minus__uMinus_param_u32_C:
LFB78:
	pushq	%rbp
LCFI247:
	movq	%rsp, %rbp
LCFI248:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI249:
	ret
LFE78:
.no_dead_strip _Minus__Minus_param_u32_C
_Minus__Minus_param_u32_C:
LFB79:
	pushq	%rbp
LCFI250:
	movq	%rsp, %rbp
LCFI251:
	subq	$16, %rsp
LCFI252:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L122
	movl	$4257, %edi
	call	__m3_fault
L122:
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI253:
	ret
LFE79:
.no_dead_strip _Minus__uMinus_var_u32_u8
_Minus__uMinus_var_u32_u8:
LFB80:
	pushq	%rbp
LCFI254:
	movq	%rsp, %rbp
LCFI255:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI256:
	ret
LFE80:
.no_dead_strip _Minus__Minus_var_u32_u8
_Minus__Minus_var_u32_u8:
LFB81:
	pushq	%rbp
LCFI257:
	movq	%rsp, %rbp
LCFI258:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L125
	movl	$4321, %edi
	call	__m3_fault
L125:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI259:
	ret
LFE81:
.no_dead_strip _Minus__uMinus_param_u32_u8
_Minus__uMinus_param_u32_u8:
LFB82:
	pushq	%rbp
LCFI260:
	movq	%rsp, %rbp
LCFI261:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI262:
	ret
LFE82:
.no_dead_strip _Minus__Minus_param_u32_u8
_Minus__Minus_param_u32_u8:
LFB83:
	pushq	%rbp
LCFI263:
	movq	%rsp, %rbp
LCFI264:
	subq	$16, %rsp
LCFI265:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L128
	movl	$4385, %edi
	call	__m3_fault
L128:
	mov	-4(%rbp), %edx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI266:
	ret
LFE83:
.no_dead_strip _Minus__uMinus_var_u32_i64
_Minus__uMinus_var_u32_i64:
LFB84:
	pushq	%rbp
LCFI267:
	movq	%rsp, %rbp
LCFI268:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI269:
	ret
LFE84:
.no_dead_strip _Minus__Minus_var_u32_i64
_Minus__Minus_var_u32_i64:
LFB85:
	pushq	%rbp
LCFI270:
	movq	%rsp, %rbp
LCFI271:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI272:
	ret
LFE85:
.no_dead_strip _Minus__uMinus_param_u32_i64
_Minus__uMinus_param_u32_i64:
LFB86:
	pushq	%rbp
LCFI273:
	movq	%rsp, %rbp
LCFI274:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI275:
	ret
LFE86:
.no_dead_strip _Minus__Minus_param_u32_i64
_Minus__Minus_param_u32_i64:
LFB87:
	pushq	%rbp
LCFI276:
	movq	%rsp, %rbp
LCFI277:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI278:
	ret
LFE87:
.no_dead_strip _Minus__uMinus_var_u32_L
_Minus__uMinus_var_u32_L:
LFB88:
	pushq	%rbp
LCFI279:
	movq	%rsp, %rbp
LCFI280:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI281:
	ret
LFE88:
.no_dead_strip _Minus__Minus_var_u32_L
_Minus__Minus_var_u32_L:
LFB89:
	pushq	%rbp
LCFI282:
	movq	%rsp, %rbp
LCFI283:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI284:
	ret
LFE89:
.no_dead_strip _Minus__uMinus_param_u32_L
_Minus__uMinus_param_u32_L:
LFB90:
	pushq	%rbp
LCFI285:
	movq	%rsp, %rbp
LCFI286:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI287:
	ret
LFE90:
.no_dead_strip _Minus__Minus_param_u32_L
_Minus__Minus_param_u32_L:
LFB91:
	pushq	%rbp
LCFI288:
	movq	%rsp, %rbp
LCFI289:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI290:
	ret
LFE91:
.no_dead_strip _Minus__uMinus_var_u32_i8
_Minus__uMinus_var_u32_i8:
LFB92:
	pushq	%rbp
LCFI291:
	movq	%rsp, %rbp
LCFI292:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI293:
	ret
LFE92:
.no_dead_strip _Minus__Minus_var_u32_i8
_Minus__Minus_var_u32_i8:
LFB93:
	pushq	%rbp
LCFI294:
	movq	%rsp, %rbp
LCFI295:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L139
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L140
L139:
	movl	$4705, %edi
	call	__m3_fault
L140:
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI296:
	ret
LFE93:
.no_dead_strip _Minus__uMinus_param_u32_i8
_Minus__uMinus_param_u32_i8:
LFB94:
	pushq	%rbp
LCFI297:
	movq	%rsp, %rbp
LCFI298:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI299:
	ret
LFE94:
.no_dead_strip _Minus__Minus_param_u32_i8
_Minus__Minus_param_u32_i8:
LFB95:
	pushq	%rbp
LCFI300:
	movq	%rsp, %rbp
LCFI301:
	subq	$16, %rsp
LCFI302:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L143
	mov	-4(%rbp), %edx
	movsbq	-5(%rbp), %rax
	subq	%rax, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L144
L143:
	movl	$4769, %edi
	call	__m3_fault
L144:
	mov	-4(%rbp), %edx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI303:
	ret
LFE95:
.no_dead_strip _Minus__uMinus_var_LC_u16
_Minus__uMinus_var_LC_u16:
LFB96:
	pushq	%rbp
LCFI304:
	movq	%rsp, %rbp
LCFI305:
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI306:
	ret
LFE96:
.no_dead_strip _Minus__Minus_var_LC_u16
_Minus__Minus_var_LC_u16:
LFB97:
	pushq	%rbp
LCFI307:
	movq	%rsp, %rbp
LCFI308:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI309:
	ret
LFE97:
.no_dead_strip _Minus__uMinus_param_LC_u16
_Minus__uMinus_param_LC_u16:
LFB98:
	pushq	%rbp
LCFI310:
	movq	%rsp, %rbp
LCFI311:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI312:
	ret
LFE98:
.no_dead_strip _Minus__Minus_param_LC_u16
_Minus__Minus_param_LC_u16:
LFB99:
	pushq	%rbp
LCFI313:
	movq	%rsp, %rbp
LCFI314:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI315:
	ret
LFE99:
.no_dead_strip _Minus__uMinus_var_LC_u32
_Minus__uMinus_var_LC_u32:
LFB100:
	pushq	%rbp
LCFI316:
	movq	%rsp, %rbp
LCFI317:
	movq	112+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI318:
	ret
LFE100:
.no_dead_strip _Minus__Minus_var_LC_u32
_Minus__Minus_var_LC_u32:
LFB101:
	pushq	%rbp
LCFI319:
	movq	%rsp, %rbp
LCFI320:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI321:
	ret
LFE101:
.no_dead_strip _Minus__uMinus_param_LC_u32
_Minus__uMinus_param_LC_u32:
LFB102:
	pushq	%rbp
LCFI322:
	movq	%rsp, %rbp
LCFI323:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI324:
	ret
LFE102:
.no_dead_strip _Minus__Minus_param_LC_u32
_Minus__Minus_param_LC_u32:
LFB103:
	pushq	%rbp
LCFI325:
	movq	%rsp, %rbp
LCFI326:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI327:
	ret
LFE103:
.no_dead_strip _Minus__uMinus_var_LC_LC
_Minus__uMinus_var_LC_LC:
LFB104:
	pushq	%rbp
LCFI328:
	movq	%rsp, %rbp
LCFI329:
	movq	112+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI330:
	ret
LFE104:
.no_dead_strip _Minus__Minus_var_LC_LC
_Minus__Minus_var_LC_LC:
LFB105:
	pushq	%rbp
LCFI331:
	movq	%rsp, %rbp
LCFI332:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI333:
	ret
LFE105:
.no_dead_strip _Minus__uMinus_param_LC_LC
_Minus__uMinus_param_LC_LC:
LFB106:
	pushq	%rbp
LCFI334:
	movq	%rsp, %rbp
LCFI335:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI336:
	ret
LFE106:
.no_dead_strip _Minus__Minus_param_LC_LC
_Minus__Minus_param_LC_LC:
LFB107:
	pushq	%rbp
LCFI337:
	movq	%rsp, %rbp
LCFI338:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI339:
	ret
LFE107:
.no_dead_strip _Minus__uMinus_var_LC_I
_Minus__uMinus_var_LC_I:
LFB108:
	pushq	%rbp
LCFI340:
	movq	%rsp, %rbp
LCFI341:
	movq	112+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI342:
	ret
LFE108:
.no_dead_strip _Minus__Minus_var_LC_I
_Minus__Minus_var_LC_I:
LFB109:
	pushq	%rbp
LCFI343:
	movq	%rsp, %rbp
LCFI344:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI345:
	ret
LFE109:
.no_dead_strip _Minus__uMinus_param_LC_I
_Minus__uMinus_param_LC_I:
LFB110:
	pushq	%rbp
LCFI346:
	movq	%rsp, %rbp
LCFI347:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI348:
	ret
LFE110:
.no_dead_strip _Minus__Minus_param_LC_I
_Minus__Minus_param_LC_I:
LFB111:
	pushq	%rbp
LCFI349:
	movq	%rsp, %rbp
LCFI350:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI351:
	ret
LFE111:
.no_dead_strip _Minus__uMinus_var_LC_i16
_Minus__uMinus_var_LC_i16:
LFB112:
	pushq	%rbp
LCFI352:
	movq	%rsp, %rbp
LCFI353:
	movq	112+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI354:
	ret
LFE112:
.no_dead_strip _Minus__Minus_var_LC_i16
_Minus__Minus_var_LC_i16:
LFB113:
	pushq	%rbp
LCFI355:
	movq	%rsp, %rbp
LCFI356:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI357:
	ret
LFE113:
.no_dead_strip _Minus__uMinus_param_LC_i16
_Minus__uMinus_param_LC_i16:
LFB114:
	pushq	%rbp
LCFI358:
	movq	%rsp, %rbp
LCFI359:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI360:
	ret
LFE114:
.no_dead_strip _Minus__Minus_param_LC_i16
_Minus__Minus_param_LC_i16:
LFB115:
	pushq	%rbp
LCFI361:
	movq	%rsp, %rbp
LCFI362:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI363:
	ret
LFE115:
.no_dead_strip _Minus__uMinus_var_LC_u64
_Minus__uMinus_var_LC_u64:
LFB116:
	pushq	%rbp
LCFI364:
	movq	%rsp, %rbp
LCFI365:
	movq	112+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI366:
	ret
LFE116:
.no_dead_strip _Minus__Minus_var_LC_u64
_Minus__Minus_var_LC_u64:
LFB117:
	pushq	%rbp
LCFI367:
	movq	%rsp, %rbp
LCFI368:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI369:
	ret
LFE117:
.no_dead_strip _Minus__uMinus_param_LC_u64
_Minus__uMinus_param_LC_u64:
LFB118:
	pushq	%rbp
LCFI370:
	movq	%rsp, %rbp
LCFI371:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI372:
	ret
LFE118:
.no_dead_strip _Minus__Minus_param_LC_u64
_Minus__Minus_param_LC_u64:
LFB119:
	pushq	%rbp
LCFI373:
	movq	%rsp, %rbp
LCFI374:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI375:
	ret
LFE119:
.no_dead_strip _Minus__uMinus_var_LC_i32
_Minus__uMinus_var_LC_i32:
LFB120:
	pushq	%rbp
LCFI376:
	movq	%rsp, %rbp
LCFI377:
	movq	112+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI378:
	ret
LFE120:
.no_dead_strip _Minus__Minus_var_LC_i32
_Minus__Minus_var_LC_i32:
LFB121:
	pushq	%rbp
LCFI379:
	movq	%rsp, %rbp
LCFI380:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI381:
	ret
LFE121:
.no_dead_strip _Minus__uMinus_param_LC_i32
_Minus__uMinus_param_LC_i32:
LFB122:
	pushq	%rbp
LCFI382:
	movq	%rsp, %rbp
LCFI383:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI384:
	ret
LFE122:
.no_dead_strip _Minus__Minus_param_LC_i32
_Minus__Minus_param_LC_i32:
LFB123:
	pushq	%rbp
LCFI385:
	movq	%rsp, %rbp
LCFI386:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI387:
	ret
LFE123:
.no_dead_strip _Minus__uMinus_var_LC_C
_Minus__uMinus_var_LC_C:
LFB124:
	pushq	%rbp
LCFI388:
	movq	%rsp, %rbp
LCFI389:
	movq	112+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI390:
	ret
LFE124:
.no_dead_strip _Minus__Minus_var_LC_C
_Minus__Minus_var_LC_C:
LFB125:
	pushq	%rbp
LCFI391:
	movq	%rsp, %rbp
LCFI392:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI393:
	ret
LFE125:
.no_dead_strip _Minus__uMinus_param_LC_C
_Minus__uMinus_param_LC_C:
LFB126:
	pushq	%rbp
LCFI394:
	movq	%rsp, %rbp
LCFI395:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI396:
	ret
LFE126:
.no_dead_strip _Minus__Minus_param_LC_C
_Minus__Minus_param_LC_C:
LFB127:
	pushq	%rbp
LCFI397:
	movq	%rsp, %rbp
LCFI398:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI399:
	ret
LFE127:
.no_dead_strip _Minus__uMinus_var_LC_u8
_Minus__uMinus_var_LC_u8:
LFB128:
	pushq	%rbp
LCFI400:
	movq	%rsp, %rbp
LCFI401:
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI402:
	ret
LFE128:
.no_dead_strip _Minus__Minus_var_LC_u8
_Minus__Minus_var_LC_u8:
LFB129:
	pushq	%rbp
LCFI403:
	movq	%rsp, %rbp
LCFI404:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI405:
	ret
LFE129:
.no_dead_strip _Minus__uMinus_param_LC_u8
_Minus__uMinus_param_LC_u8:
LFB130:
	pushq	%rbp
LCFI406:
	movq	%rsp, %rbp
LCFI407:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI408:
	ret
LFE130:
.no_dead_strip _Minus__Minus_param_LC_u8
_Minus__Minus_param_LC_u8:
LFB131:
	pushq	%rbp
LCFI409:
	movq	%rsp, %rbp
LCFI410:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI411:
	ret
LFE131:
.no_dead_strip _Minus__uMinus_var_LC_i64
_Minus__uMinus_var_LC_i64:
LFB132:
	pushq	%rbp
LCFI412:
	movq	%rsp, %rbp
LCFI413:
	movq	112+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI414:
	ret
LFE132:
.no_dead_strip _Minus__Minus_var_LC_i64
_Minus__Minus_var_LC_i64:
LFB133:
	pushq	%rbp
LCFI415:
	movq	%rsp, %rbp
LCFI416:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI417:
	ret
LFE133:
.no_dead_strip _Minus__uMinus_param_LC_i64
_Minus__uMinus_param_LC_i64:
LFB134:
	pushq	%rbp
LCFI418:
	movq	%rsp, %rbp
LCFI419:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI420:
	ret
LFE134:
.no_dead_strip _Minus__Minus_param_LC_i64
_Minus__Minus_param_LC_i64:
LFB135:
	pushq	%rbp
LCFI421:
	movq	%rsp, %rbp
LCFI422:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI423:
	ret
LFE135:
.no_dead_strip _Minus__uMinus_var_LC_L
_Minus__uMinus_var_LC_L:
LFB136:
	pushq	%rbp
LCFI424:
	movq	%rsp, %rbp
LCFI425:
	movq	112+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI426:
	ret
LFE136:
.no_dead_strip _Minus__Minus_var_LC_L
_Minus__Minus_var_LC_L:
LFB137:
	pushq	%rbp
LCFI427:
	movq	%rsp, %rbp
LCFI428:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI429:
	ret
LFE137:
.no_dead_strip _Minus__uMinus_param_LC_L
_Minus__uMinus_param_LC_L:
LFB138:
	pushq	%rbp
LCFI430:
	movq	%rsp, %rbp
LCFI431:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI432:
	ret
LFE138:
.no_dead_strip _Minus__Minus_param_LC_L
_Minus__Minus_param_LC_L:
LFB139:
	pushq	%rbp
LCFI433:
	movq	%rsp, %rbp
LCFI434:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI435:
	ret
LFE139:
.no_dead_strip _Minus__uMinus_var_LC_i8
_Minus__uMinus_var_LC_i8:
LFB140:
	pushq	%rbp
LCFI436:
	movq	%rsp, %rbp
LCFI437:
	movq	112+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI438:
	ret
LFE140:
.no_dead_strip _Minus__Minus_var_LC_i8
_Minus__Minus_var_LC_i8:
LFB141:
	pushq	%rbp
LCFI439:
	movq	%rsp, %rbp
LCFI440:
	movq	112+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI441:
	ret
LFE141:
.no_dead_strip _Minus__uMinus_param_LC_i8
_Minus__uMinus_param_LC_i8:
LFB142:
	pushq	%rbp
LCFI442:
	movq	%rsp, %rbp
LCFI443:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI444:
	ret
LFE142:
.no_dead_strip _Minus__Minus_param_LC_i8
_Minus__Minus_param_LC_i8:
LFB143:
	pushq	%rbp
LCFI445:
	movq	%rsp, %rbp
LCFI446:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI447:
	ret
LFE143:
.no_dead_strip _Minus__uMinus_var_I_u16
_Minus__uMinus_var_I_u16:
LFB144:
	pushq	%rbp
LCFI448:
	movq	%rsp, %rbp
LCFI449:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI450:
	ret
LFE144:
.no_dead_strip _Minus__Minus_var_I_u16
_Minus__Minus_var_I_u16:
LFB145:
	pushq	%rbp
LCFI451:
	movq	%rsp, %rbp
LCFI452:
	movq	120+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI453:
	ret
LFE145:
.no_dead_strip _Minus__uMinus_param_I_u16
_Minus__uMinus_param_I_u16:
LFB146:
	pushq	%rbp
LCFI454:
	movq	%rsp, %rbp
LCFI455:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI456:
	ret
LFE146:
.no_dead_strip _Minus__Minus_param_I_u16
_Minus__Minus_param_I_u16:
LFB147:
	pushq	%rbp
LCFI457:
	movq	%rsp, %rbp
LCFI458:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI459:
	ret
LFE147:
.no_dead_strip _Minus__uMinus_var_I_u32
_Minus__uMinus_var_I_u32:
LFB148:
	pushq	%rbp
LCFI460:
	movq	%rsp, %rbp
LCFI461:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI462:
	ret
LFE148:
.no_dead_strip _Minus__Minus_var_I_u32
_Minus__Minus_var_I_u32:
LFB149:
	pushq	%rbp
LCFI463:
	movq	%rsp, %rbp
LCFI464:
	movq	120+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI465:
	ret
LFE149:
.no_dead_strip _Minus__uMinus_param_I_u32
_Minus__uMinus_param_I_u32:
LFB150:
	pushq	%rbp
LCFI466:
	movq	%rsp, %rbp
LCFI467:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI468:
	ret
LFE150:
.no_dead_strip _Minus__Minus_param_I_u32
_Minus__Minus_param_I_u32:
LFB151:
	pushq	%rbp
LCFI469:
	movq	%rsp, %rbp
LCFI470:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI471:
	ret
LFE151:
.no_dead_strip _Minus__uMinus_var_I_LC
_Minus__uMinus_var_I_LC:
LFB152:
	pushq	%rbp
LCFI472:
	movq	%rsp, %rbp
LCFI473:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI474:
	ret
LFE152:
.no_dead_strip _Minus__Minus_var_I_LC
_Minus__Minus_var_I_LC:
LFB153:
	pushq	%rbp
LCFI475:
	movq	%rsp, %rbp
LCFI476:
	movq	120+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI477:
	ret
LFE153:
.no_dead_strip _Minus__uMinus_param_I_LC
_Minus__uMinus_param_I_LC:
LFB154:
	pushq	%rbp
LCFI478:
	movq	%rsp, %rbp
LCFI479:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI480:
	ret
LFE154:
.no_dead_strip _Minus__Minus_param_I_LC
_Minus__Minus_param_I_LC:
LFB155:
	pushq	%rbp
LCFI481:
	movq	%rsp, %rbp
LCFI482:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI483:
	ret
LFE155:
.no_dead_strip _Minus__uMinus_var_I_I
_Minus__uMinus_var_I_I:
LFB156:
	pushq	%rbp
LCFI484:
	movq	%rsp, %rbp
LCFI485:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI486:
	ret
LFE156:
.no_dead_strip _Minus__Minus_var_I_I
_Minus__Minus_var_I_I:
LFB157:
	pushq	%rbp
LCFI487:
	movq	%rsp, %rbp
LCFI488:
	movq	120+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI489:
	ret
LFE157:
.no_dead_strip _Minus__uMinus_param_I_I
_Minus__uMinus_param_I_I:
LFB158:
	pushq	%rbp
LCFI490:
	movq	%rsp, %rbp
LCFI491:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI492:
	ret
LFE158:
.no_dead_strip _Minus__Minus_param_I_I
_Minus__Minus_param_I_I:
LFB159:
	pushq	%rbp
LCFI493:
	movq	%rsp, %rbp
LCFI494:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI495:
	ret
LFE159:
.no_dead_strip _Minus__uMinus_var_I_i16
_Minus__uMinus_var_I_i16:
LFB160:
	pushq	%rbp
LCFI496:
	movq	%rsp, %rbp
LCFI497:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI498:
	ret
LFE160:
.no_dead_strip _Minus__Minus_var_I_i16
_Minus__Minus_var_I_i16:
LFB161:
	pushq	%rbp
LCFI499:
	movq	%rsp, %rbp
LCFI500:
	movq	120+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI501:
	ret
LFE161:
.no_dead_strip _Minus__uMinus_param_I_i16
_Minus__uMinus_param_I_i16:
LFB162:
	pushq	%rbp
LCFI502:
	movq	%rsp, %rbp
LCFI503:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI504:
	ret
LFE162:
.no_dead_strip _Minus__Minus_param_I_i16
_Minus__Minus_param_I_i16:
LFB163:
	pushq	%rbp
LCFI505:
	movq	%rsp, %rbp
LCFI506:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI507:
	ret
LFE163:
.no_dead_strip _Minus__uMinus_var_I_u64
_Minus__uMinus_var_I_u64:
LFB164:
	pushq	%rbp
LCFI508:
	movq	%rsp, %rbp
LCFI509:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI510:
	ret
LFE164:
.no_dead_strip _Minus__Minus_var_I_u64
_Minus__Minus_var_I_u64:
LFB165:
	pushq	%rbp
LCFI511:
	movq	%rsp, %rbp
LCFI512:
	movq	120+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI513:
	ret
LFE165:
.no_dead_strip _Minus__uMinus_param_I_u64
_Minus__uMinus_param_I_u64:
LFB166:
	pushq	%rbp
LCFI514:
	movq	%rsp, %rbp
LCFI515:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI516:
	ret
LFE166:
.no_dead_strip _Minus__Minus_param_I_u64
_Minus__Minus_param_I_u64:
LFB167:
	pushq	%rbp
LCFI517:
	movq	%rsp, %rbp
LCFI518:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI519:
	ret
LFE167:
.no_dead_strip _Minus__uMinus_var_I_i32
_Minus__uMinus_var_I_i32:
LFB168:
	pushq	%rbp
LCFI520:
	movq	%rsp, %rbp
LCFI521:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI522:
	ret
LFE168:
.no_dead_strip _Minus__Minus_var_I_i32
_Minus__Minus_var_I_i32:
LFB169:
	pushq	%rbp
LCFI523:
	movq	%rsp, %rbp
LCFI524:
	movq	120+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI525:
	ret
LFE169:
.no_dead_strip _Minus__uMinus_param_I_i32
_Minus__uMinus_param_I_i32:
LFB170:
	pushq	%rbp
LCFI526:
	movq	%rsp, %rbp
LCFI527:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI528:
	ret
LFE170:
.no_dead_strip _Minus__Minus_param_I_i32
_Minus__Minus_param_I_i32:
LFB171:
	pushq	%rbp
LCFI529:
	movq	%rsp, %rbp
LCFI530:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI531:
	ret
LFE171:
.no_dead_strip _Minus__uMinus_var_I_C
_Minus__uMinus_var_I_C:
LFB172:
	pushq	%rbp
LCFI532:
	movq	%rsp, %rbp
LCFI533:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI534:
	ret
LFE172:
.no_dead_strip _Minus__Minus_var_I_C
_Minus__Minus_var_I_C:
LFB173:
	pushq	%rbp
LCFI535:
	movq	%rsp, %rbp
LCFI536:
	movq	120+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI537:
	ret
LFE173:
.no_dead_strip _Minus__uMinus_param_I_C
_Minus__uMinus_param_I_C:
LFB174:
	pushq	%rbp
LCFI538:
	movq	%rsp, %rbp
LCFI539:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI540:
	ret
LFE174:
.no_dead_strip _Minus__Minus_param_I_C
_Minus__Minus_param_I_C:
LFB175:
	pushq	%rbp
LCFI541:
	movq	%rsp, %rbp
LCFI542:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI543:
	ret
LFE175:
.no_dead_strip _Minus__uMinus_var_I_u8
_Minus__uMinus_var_I_u8:
LFB176:
	pushq	%rbp
LCFI544:
	movq	%rsp, %rbp
LCFI545:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI546:
	ret
LFE176:
.no_dead_strip _Minus__Minus_var_I_u8
_Minus__Minus_var_I_u8:
LFB177:
	pushq	%rbp
LCFI547:
	movq	%rsp, %rbp
LCFI548:
	movq	120+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI549:
	ret
LFE177:
.no_dead_strip _Minus__uMinus_param_I_u8
_Minus__uMinus_param_I_u8:
LFB178:
	pushq	%rbp
LCFI550:
	movq	%rsp, %rbp
LCFI551:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI552:
	ret
LFE178:
.no_dead_strip _Minus__Minus_param_I_u8
_Minus__Minus_param_I_u8:
LFB179:
	pushq	%rbp
LCFI553:
	movq	%rsp, %rbp
LCFI554:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI555:
	ret
LFE179:
.no_dead_strip _Minus__uMinus_var_I_i64
_Minus__uMinus_var_I_i64:
LFB180:
	pushq	%rbp
LCFI556:
	movq	%rsp, %rbp
LCFI557:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI558:
	ret
LFE180:
.no_dead_strip _Minus__Minus_var_I_i64
_Minus__Minus_var_I_i64:
LFB181:
	pushq	%rbp
LCFI559:
	movq	%rsp, %rbp
LCFI560:
	movq	120+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI561:
	ret
LFE181:
.no_dead_strip _Minus__uMinus_param_I_i64
_Minus__uMinus_param_I_i64:
LFB182:
	pushq	%rbp
LCFI562:
	movq	%rsp, %rbp
LCFI563:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI564:
	ret
LFE182:
.no_dead_strip _Minus__Minus_param_I_i64
_Minus__Minus_param_I_i64:
LFB183:
	pushq	%rbp
LCFI565:
	movq	%rsp, %rbp
LCFI566:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI567:
	ret
LFE183:
.no_dead_strip _Minus__uMinus_var_I_L
_Minus__uMinus_var_I_L:
LFB184:
	pushq	%rbp
LCFI568:
	movq	%rsp, %rbp
LCFI569:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI570:
	ret
LFE184:
.no_dead_strip _Minus__Minus_var_I_L
_Minus__Minus_var_I_L:
LFB185:
	pushq	%rbp
LCFI571:
	movq	%rsp, %rbp
LCFI572:
	movq	120+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI573:
	ret
LFE185:
.no_dead_strip _Minus__uMinus_param_I_L
_Minus__uMinus_param_I_L:
LFB186:
	pushq	%rbp
LCFI574:
	movq	%rsp, %rbp
LCFI575:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI576:
	ret
LFE186:
.no_dead_strip _Minus__Minus_param_I_L
_Minus__Minus_param_I_L:
LFB187:
	pushq	%rbp
LCFI577:
	movq	%rsp, %rbp
LCFI578:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI579:
	ret
LFE187:
.no_dead_strip _Minus__uMinus_var_I_i8
_Minus__uMinus_var_I_i8:
LFB188:
	pushq	%rbp
LCFI580:
	movq	%rsp, %rbp
LCFI581:
	movq	120+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI582:
	ret
LFE188:
.no_dead_strip _Minus__Minus_var_I_i8
_Minus__Minus_var_I_i8:
LFB189:
	pushq	%rbp
LCFI583:
	movq	%rsp, %rbp
LCFI584:
	movq	120+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI585:
	ret
LFE189:
.no_dead_strip _Minus__uMinus_param_I_i8
_Minus__uMinus_param_I_i8:
LFB190:
	pushq	%rbp
LCFI586:
	movq	%rsp, %rbp
LCFI587:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI588:
	ret
LFE190:
.no_dead_strip _Minus__Minus_param_I_i8
_Minus__Minus_param_I_i8:
LFB191:
	pushq	%rbp
LCFI589:
	movq	%rsp, %rbp
LCFI590:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI591:
	ret
LFE191:
.no_dead_strip _Minus__uMinus_var_i16_u16
_Minus__uMinus_var_i16_u16:
LFB192:
	pushq	%rbp
LCFI592:
	movq	%rsp, %rbp
LCFI593:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI594:
	ret
LFE192:
.no_dead_strip _Minus__Minus_var_i16_u16
_Minus__Minus_var_i16_u16:
LFB193:
	pushq	%rbp
LCFI595:
	movq	%rsp, %rbp
LCFI596:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L243
	movl	$7905, %edi
	call	__m3_fault
L243:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI597:
	ret
LFE193:
.no_dead_strip _Minus__uMinus_param_i16_u16
_Minus__uMinus_param_i16_u16:
LFB194:
	pushq	%rbp
LCFI598:
	movq	%rsp, %rbp
LCFI599:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI600:
	ret
LFE194:
.no_dead_strip _Minus__Minus_param_i16_u16
_Minus__Minus_param_i16_u16:
LFB195:
	pushq	%rbp
LCFI601:
	movq	%rsp, %rbp
LCFI602:
	subq	$16, %rsp
LCFI603:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L246
	movl	$7969, %edi
	call	__m3_fault
L246:
	movswq	-2(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI604:
	ret
LFE195:
.no_dead_strip _Minus__uMinus_var_i16_u32
_Minus__uMinus_var_i16_u32:
LFB196:
	pushq	%rbp
LCFI605:
	movq	%rsp, %rbp
LCFI606:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI607:
	ret
LFE196:
.no_dead_strip _Minus__Minus_var_i16_u32
_Minus__Minus_var_i16_u32:
LFB197:
	pushq	%rbp
LCFI608:
	movq	%rsp, %rbp
LCFI609:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L249
	movl	$8033, %edi
	call	__m3_fault
L249:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI610:
	ret
LFE197:
.no_dead_strip _Minus__uMinus_param_i16_u32
_Minus__uMinus_param_i16_u32:
LFB198:
	pushq	%rbp
LCFI611:
	movq	%rsp, %rbp
LCFI612:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI613:
	ret
LFE198:
.no_dead_strip _Minus__Minus_param_i16_u32
_Minus__Minus_param_i16_u32:
LFB199:
	pushq	%rbp
LCFI614:
	movq	%rsp, %rbp
LCFI615:
	subq	$16, %rsp
LCFI616:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L252
	movl	$8097, %edi
	call	__m3_fault
L252:
	movswq	-2(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI617:
	ret
LFE199:
.no_dead_strip _Minus__uMinus_var_i16_LC
_Minus__uMinus_var_i16_LC:
LFB200:
	pushq	%rbp
LCFI618:
	movq	%rsp, %rbp
LCFI619:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI620:
	ret
LFE200:
.no_dead_strip _Minus__Minus_var_i16_LC
_Minus__Minus_var_i16_LC:
LFB201:
	pushq	%rbp
LCFI621:
	movq	%rsp, %rbp
LCFI622:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI623:
	ret
LFE201:
.no_dead_strip _Minus__uMinus_param_i16_LC
_Minus__uMinus_param_i16_LC:
LFB202:
	pushq	%rbp
LCFI624:
	movq	%rsp, %rbp
LCFI625:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI626:
	ret
LFE202:
.no_dead_strip _Minus__Minus_param_i16_LC
_Minus__Minus_param_i16_LC:
LFB203:
	pushq	%rbp
LCFI627:
	movq	%rsp, %rbp
LCFI628:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI629:
	ret
LFE203:
.no_dead_strip _Minus__uMinus_var_i16_I
_Minus__uMinus_var_i16_I:
LFB204:
	pushq	%rbp
LCFI630:
	movq	%rsp, %rbp
LCFI631:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI632:
	ret
LFE204:
.no_dead_strip _Minus__Minus_var_i16_I
_Minus__Minus_var_i16_I:
LFB205:
	pushq	%rbp
LCFI633:
	movq	%rsp, %rbp
LCFI634:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L259
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L260
L259:
	movl	$8289, %edi
	call	__m3_fault
L260:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI635:
	ret
LFE205:
.no_dead_strip _Minus__uMinus_param_i16_I
_Minus__uMinus_param_i16_I:
LFB206:
	pushq	%rbp
LCFI636:
	movq	%rsp, %rbp
LCFI637:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI638:
	ret
LFE206:
.no_dead_strip _Minus__Minus_param_i16_I
_Minus__Minus_param_i16_I:
LFB207:
	pushq	%rbp
LCFI639:
	movq	%rsp, %rbp
LCFI640:
	subq	$16, %rsp
LCFI641:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	cmpq	$-32768, %rax
	jl	L263
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	cmpq	$32767, %rax
	jle	L264
L263:
	movl	$8353, %edi
	call	__m3_fault
L264:
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI642:
	ret
LFE207:
.no_dead_strip _Minus__uMinus_var_i16_i16
_Minus__uMinus_var_i16_i16:
LFB208:
	pushq	%rbp
LCFI643:
	movq	%rsp, %rbp
LCFI644:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI645:
	ret
LFE208:
.no_dead_strip _Minus__Minus_var_i16_i16
_Minus__Minus_var_i16_i16:
LFB209:
	pushq	%rbp
LCFI646:
	movq	%rsp, %rbp
LCFI647:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L267
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L268
L267:
	movl	$8417, %edi
	call	__m3_fault
L268:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI648:
	ret
LFE209:
.no_dead_strip _Minus__uMinus_param_i16_i16
_Minus__uMinus_param_i16_i16:
LFB210:
	pushq	%rbp
LCFI649:
	movq	%rsp, %rbp
LCFI650:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI651:
	ret
LFE210:
.no_dead_strip _Minus__Minus_param_i16_i16
_Minus__Minus_param_i16_i16:
LFB211:
	pushq	%rbp
LCFI652:
	movq	%rsp, %rbp
LCFI653:
	subq	$16, %rsp
LCFI654:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L271
	movswq	-2(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L272
L271:
	movl	$8481, %edi
	call	__m3_fault
L272:
	movswq	-2(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI655:
	ret
LFE211:
.no_dead_strip _Minus__uMinus_var_i16_u64
_Minus__uMinus_var_i16_u64:
LFB212:
	pushq	%rbp
LCFI656:
	movq	%rsp, %rbp
LCFI657:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI658:
	ret
LFE212:
.no_dead_strip _Minus__Minus_var_i16_u64
_Minus__Minus_var_i16_u64:
LFB213:
	pushq	%rbp
LCFI659:
	movq	%rsp, %rbp
LCFI660:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI661:
	ret
LFE213:
.no_dead_strip _Minus__uMinus_param_i16_u64
_Minus__uMinus_param_i16_u64:
LFB214:
	pushq	%rbp
LCFI662:
	movq	%rsp, %rbp
LCFI663:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI664:
	ret
LFE214:
.no_dead_strip _Minus__Minus_param_i16_u64
_Minus__Minus_param_i16_u64:
LFB215:
	pushq	%rbp
LCFI665:
	movq	%rsp, %rbp
LCFI666:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI667:
	ret
LFE215:
.no_dead_strip _Minus__uMinus_var_i16_i32
_Minus__uMinus_var_i16_i32:
LFB216:
	pushq	%rbp
LCFI668:
	movq	%rsp, %rbp
LCFI669:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI670:
	ret
LFE216:
.no_dead_strip _Minus__Minus_var_i16_i32
_Minus__Minus_var_i16_i32:
LFB217:
	pushq	%rbp
LCFI671:
	movq	%rsp, %rbp
LCFI672:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L279
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L280
L279:
	movl	$8673, %edi
	call	__m3_fault
L280:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI673:
	ret
LFE217:
.no_dead_strip _Minus__uMinus_param_i16_i32
_Minus__uMinus_param_i16_i32:
LFB218:
	pushq	%rbp
LCFI674:
	movq	%rsp, %rbp
LCFI675:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI676:
	ret
LFE218:
.no_dead_strip _Minus__Minus_param_i16_i32
_Minus__Minus_param_i16_i32:
LFB219:
	pushq	%rbp
LCFI677:
	movq	%rsp, %rbp
LCFI678:
	subq	$16, %rsp
LCFI679:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L283
	movswq	-2(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L284
L283:
	movl	$8737, %edi
	call	__m3_fault
L284:
	movswq	-2(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI680:
	ret
LFE219:
.no_dead_strip _Minus__uMinus_var_i16_C
_Minus__uMinus_var_i16_C:
LFB220:
	pushq	%rbp
LCFI681:
	movq	%rsp, %rbp
LCFI682:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI683:
	ret
LFE220:
.no_dead_strip _Minus__Minus_var_i16_C
_Minus__Minus_var_i16_C:
LFB221:
	pushq	%rbp
LCFI684:
	movq	%rsp, %rbp
LCFI685:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L287
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L288
L287:
	movl	$8801, %edi
	call	__m3_fault
L288:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI686:
	ret
LFE221:
.no_dead_strip _Minus__uMinus_param_i16_C
_Minus__uMinus_param_i16_C:
LFB222:
	pushq	%rbp
LCFI687:
	movq	%rsp, %rbp
LCFI688:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI689:
	ret
LFE222:
.no_dead_strip _Minus__Minus_param_i16_C
_Minus__Minus_param_i16_C:
LFB223:
	pushq	%rbp
LCFI690:
	movq	%rsp, %rbp
LCFI691:
	subq	$16, %rsp
LCFI692:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L291
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L292
L291:
	movl	$8865, %edi
	call	__m3_fault
L292:
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI693:
	ret
LFE223:
.no_dead_strip _Minus__uMinus_var_i16_u8
_Minus__uMinus_var_i16_u8:
LFB224:
	pushq	%rbp
LCFI694:
	movq	%rsp, %rbp
LCFI695:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI696:
	ret
LFE224:
.no_dead_strip _Minus__Minus_var_i16_u8
_Minus__Minus_var_i16_u8:
LFB225:
	pushq	%rbp
LCFI697:
	movq	%rsp, %rbp
LCFI698:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L295
	movl	$8929, %edi
	call	__m3_fault
L295:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI699:
	ret
LFE225:
.no_dead_strip _Minus__uMinus_param_i16_u8
_Minus__uMinus_param_i16_u8:
LFB226:
	pushq	%rbp
LCFI700:
	movq	%rsp, %rbp
LCFI701:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI702:
	ret
LFE226:
.no_dead_strip _Minus__Minus_param_i16_u8
_Minus__Minus_param_i16_u8:
LFB227:
	pushq	%rbp
LCFI703:
	movq	%rsp, %rbp
LCFI704:
	subq	$16, %rsp
LCFI705:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jge	L298
	movl	$8993, %edi
	call	__m3_fault
L298:
	movswq	-2(%rbp), %rdx
	movzbl	-3(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI706:
	ret
LFE227:
.no_dead_strip _Minus__uMinus_var_i16_i64
_Minus__uMinus_var_i16_i64:
LFB228:
	pushq	%rbp
LCFI707:
	movq	%rsp, %rbp
LCFI708:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI709:
	ret
LFE228:
.no_dead_strip _Minus__Minus_var_i16_i64
_Minus__Minus_var_i16_i64:
LFB229:
	pushq	%rbp
LCFI710:
	movq	%rsp, %rbp
LCFI711:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI712:
	ret
LFE229:
.no_dead_strip _Minus__uMinus_param_i16_i64
_Minus__uMinus_param_i16_i64:
LFB230:
	pushq	%rbp
LCFI713:
	movq	%rsp, %rbp
LCFI714:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI715:
	ret
LFE230:
.no_dead_strip _Minus__Minus_param_i16_i64
_Minus__Minus_param_i16_i64:
LFB231:
	pushq	%rbp
LCFI716:
	movq	%rsp, %rbp
LCFI717:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI718:
	ret
LFE231:
.no_dead_strip _Minus__uMinus_var_i16_L
_Minus__uMinus_var_i16_L:
LFB232:
	pushq	%rbp
LCFI719:
	movq	%rsp, %rbp
LCFI720:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI721:
	ret
LFE232:
.no_dead_strip _Minus__Minus_var_i16_L
_Minus__Minus_var_i16_L:
LFB233:
	pushq	%rbp
LCFI722:
	movq	%rsp, %rbp
LCFI723:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI724:
	ret
LFE233:
.no_dead_strip _Minus__uMinus_param_i16_L
_Minus__uMinus_param_i16_L:
LFB234:
	pushq	%rbp
LCFI725:
	movq	%rsp, %rbp
LCFI726:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI727:
	ret
LFE234:
.no_dead_strip _Minus__Minus_param_i16_L
_Minus__Minus_param_i16_L:
LFB235:
	pushq	%rbp
LCFI728:
	movq	%rsp, %rbp
LCFI729:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI730:
	ret
LFE235:
.no_dead_strip _Minus__uMinus_var_i16_i8
_Minus__uMinus_var_i16_i8:
LFB236:
	pushq	%rbp
LCFI731:
	movq	%rsp, %rbp
LCFI732:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI733:
	ret
LFE236:
.no_dead_strip _Minus__Minus_var_i16_i8
_Minus__Minus_var_i16_i8:
LFB237:
	pushq	%rbp
LCFI734:
	movq	%rsp, %rbp
LCFI735:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L309
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L310
L309:
	movl	$9313, %edi
	call	__m3_fault
L310:
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI736:
	ret
LFE237:
.no_dead_strip _Minus__uMinus_param_i16_i8
_Minus__uMinus_param_i16_i8:
LFB238:
	pushq	%rbp
LCFI737:
	movq	%rsp, %rbp
LCFI738:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI739:
	ret
LFE238:
.no_dead_strip _Minus__Minus_param_i16_i8
_Minus__Minus_param_i16_i8:
LFB239:
	pushq	%rbp
LCFI740:
	movq	%rsp, %rbp
LCFI741:
	subq	$16, %rsp
LCFI742:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-32768, %rax
	jl	L313
	movswq	-2(%rbp), %rdx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$32767, %rax
	jle	L314
L313:
	movl	$9377, %edi
	call	__m3_fault
L314:
	movswq	-2(%rbp), %rdx
	movsbq	-3(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI743:
	ret
LFE239:
.no_dead_strip _Minus__uMinus_var_u64_u16
_Minus__uMinus_var_u64_u16:
LFB240:
	pushq	%rbp
LCFI744:
	movq	%rsp, %rbp
LCFI745:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI746:
	ret
LFE240:
.no_dead_strip _Minus__Minus_var_u64_u16
_Minus__Minus_var_u64_u16:
LFB241:
	pushq	%rbp
LCFI747:
	movq	%rsp, %rbp
LCFI748:
	movq	136+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI749:
	ret
LFE241:
.no_dead_strip _Minus__uMinus_param_u64_u16
_Minus__uMinus_param_u64_u16:
LFB242:
	pushq	%rbp
LCFI750:
	movq	%rsp, %rbp
LCFI751:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI752:
	ret
LFE242:
.no_dead_strip _Minus__Minus_param_u64_u16
_Minus__Minus_param_u64_u16:
LFB243:
	pushq	%rbp
LCFI753:
	movq	%rsp, %rbp
LCFI754:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI755:
	ret
LFE243:
.no_dead_strip _Minus__uMinus_var_u64_u32
_Minus__uMinus_var_u64_u32:
LFB244:
	pushq	%rbp
LCFI756:
	movq	%rsp, %rbp
LCFI757:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI758:
	ret
LFE244:
.no_dead_strip _Minus__Minus_var_u64_u32
_Minus__Minus_var_u64_u32:
LFB245:
	pushq	%rbp
LCFI759:
	movq	%rsp, %rbp
LCFI760:
	movq	136+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI761:
	ret
LFE245:
.no_dead_strip _Minus__uMinus_param_u64_u32
_Minus__uMinus_param_u64_u32:
LFB246:
	pushq	%rbp
LCFI762:
	movq	%rsp, %rbp
LCFI763:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI764:
	ret
LFE246:
.no_dead_strip _Minus__Minus_param_u64_u32
_Minus__Minus_param_u64_u32:
LFB247:
	pushq	%rbp
LCFI765:
	movq	%rsp, %rbp
LCFI766:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI767:
	ret
LFE247:
.no_dead_strip _Minus__uMinus_var_u64_LC
_Minus__uMinus_var_u64_LC:
LFB248:
	pushq	%rbp
LCFI768:
	movq	%rsp, %rbp
LCFI769:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI770:
	ret
LFE248:
.no_dead_strip _Minus__Minus_var_u64_LC
_Minus__Minus_var_u64_LC:
LFB249:
	pushq	%rbp
LCFI771:
	movq	%rsp, %rbp
LCFI772:
	movq	136+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI773:
	ret
LFE249:
.no_dead_strip _Minus__uMinus_param_u64_LC
_Minus__uMinus_param_u64_LC:
LFB250:
	pushq	%rbp
LCFI774:
	movq	%rsp, %rbp
LCFI775:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI776:
	ret
LFE250:
.no_dead_strip _Minus__Minus_param_u64_LC
_Minus__Minus_param_u64_LC:
LFB251:
	pushq	%rbp
LCFI777:
	movq	%rsp, %rbp
LCFI778:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI779:
	ret
LFE251:
.no_dead_strip _Minus__uMinus_var_u64_I
_Minus__uMinus_var_u64_I:
LFB252:
	pushq	%rbp
LCFI780:
	movq	%rsp, %rbp
LCFI781:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI782:
	ret
LFE252:
.no_dead_strip _Minus__Minus_var_u64_I
_Minus__Minus_var_u64_I:
LFB253:
	pushq	%rbp
LCFI783:
	movq	%rsp, %rbp
LCFI784:
	movq	136+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI785:
	ret
LFE253:
.no_dead_strip _Minus__uMinus_param_u64_I
_Minus__uMinus_param_u64_I:
LFB254:
	pushq	%rbp
LCFI786:
	movq	%rsp, %rbp
LCFI787:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI788:
	ret
LFE254:
.no_dead_strip _Minus__Minus_param_u64_I
_Minus__Minus_param_u64_I:
LFB255:
	pushq	%rbp
LCFI789:
	movq	%rsp, %rbp
LCFI790:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI791:
	ret
LFE255:
.no_dead_strip _Minus__uMinus_var_u64_i16
_Minus__uMinus_var_u64_i16:
LFB256:
	pushq	%rbp
LCFI792:
	movq	%rsp, %rbp
LCFI793:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI794:
	ret
LFE256:
.no_dead_strip _Minus__Minus_var_u64_i16
_Minus__Minus_var_u64_i16:
LFB257:
	pushq	%rbp
LCFI795:
	movq	%rsp, %rbp
LCFI796:
	movq	136+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI797:
	ret
LFE257:
.no_dead_strip _Minus__uMinus_param_u64_i16
_Minus__uMinus_param_u64_i16:
LFB258:
	pushq	%rbp
LCFI798:
	movq	%rsp, %rbp
LCFI799:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI800:
	ret
LFE258:
.no_dead_strip _Minus__Minus_param_u64_i16
_Minus__Minus_param_u64_i16:
LFB259:
	pushq	%rbp
LCFI801:
	movq	%rsp, %rbp
LCFI802:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI803:
	ret
LFE259:
.no_dead_strip _Minus__uMinus_var_u64_u64
_Minus__uMinus_var_u64_u64:
LFB260:
	pushq	%rbp
LCFI804:
	movq	%rsp, %rbp
LCFI805:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI806:
	ret
LFE260:
.no_dead_strip _Minus__Minus_var_u64_u64
_Minus__Minus_var_u64_u64:
LFB261:
	pushq	%rbp
LCFI807:
	movq	%rsp, %rbp
LCFI808:
	movq	136+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI809:
	ret
LFE261:
.no_dead_strip _Minus__uMinus_param_u64_u64
_Minus__uMinus_param_u64_u64:
LFB262:
	pushq	%rbp
LCFI810:
	movq	%rsp, %rbp
LCFI811:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI812:
	ret
LFE262:
.no_dead_strip _Minus__Minus_param_u64_u64
_Minus__Minus_param_u64_u64:
LFB263:
	pushq	%rbp
LCFI813:
	movq	%rsp, %rbp
LCFI814:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI815:
	ret
LFE263:
.no_dead_strip _Minus__uMinus_var_u64_i32
_Minus__uMinus_var_u64_i32:
LFB264:
	pushq	%rbp
LCFI816:
	movq	%rsp, %rbp
LCFI817:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI818:
	ret
LFE264:
.no_dead_strip _Minus__Minus_var_u64_i32
_Minus__Minus_var_u64_i32:
LFB265:
	pushq	%rbp
LCFI819:
	movq	%rsp, %rbp
LCFI820:
	movq	136+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI821:
	ret
LFE265:
.no_dead_strip _Minus__uMinus_param_u64_i32
_Minus__uMinus_param_u64_i32:
LFB266:
	pushq	%rbp
LCFI822:
	movq	%rsp, %rbp
LCFI823:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI824:
	ret
LFE266:
.no_dead_strip _Minus__Minus_param_u64_i32
_Minus__Minus_param_u64_i32:
LFB267:
	pushq	%rbp
LCFI825:
	movq	%rsp, %rbp
LCFI826:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI827:
	ret
LFE267:
.no_dead_strip _Minus__uMinus_var_u64_C
_Minus__uMinus_var_u64_C:
LFB268:
	pushq	%rbp
LCFI828:
	movq	%rsp, %rbp
LCFI829:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI830:
	ret
LFE268:
.no_dead_strip _Minus__Minus_var_u64_C
_Minus__Minus_var_u64_C:
LFB269:
	pushq	%rbp
LCFI831:
	movq	%rsp, %rbp
LCFI832:
	movq	136+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI833:
	ret
LFE269:
.no_dead_strip _Minus__uMinus_param_u64_C
_Minus__uMinus_param_u64_C:
LFB270:
	pushq	%rbp
LCFI834:
	movq	%rsp, %rbp
LCFI835:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI836:
	ret
LFE270:
.no_dead_strip _Minus__Minus_param_u64_C
_Minus__Minus_param_u64_C:
LFB271:
	pushq	%rbp
LCFI837:
	movq	%rsp, %rbp
LCFI838:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI839:
	ret
LFE271:
.no_dead_strip _Minus__uMinus_var_u64_u8
_Minus__uMinus_var_u64_u8:
LFB272:
	pushq	%rbp
LCFI840:
	movq	%rsp, %rbp
LCFI841:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI842:
	ret
LFE272:
.no_dead_strip _Minus__Minus_var_u64_u8
_Minus__Minus_var_u64_u8:
LFB273:
	pushq	%rbp
LCFI843:
	movq	%rsp, %rbp
LCFI844:
	movq	136+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI845:
	ret
LFE273:
.no_dead_strip _Minus__uMinus_param_u64_u8
_Minus__uMinus_param_u64_u8:
LFB274:
	pushq	%rbp
LCFI846:
	movq	%rsp, %rbp
LCFI847:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI848:
	ret
LFE274:
.no_dead_strip _Minus__Minus_param_u64_u8
_Minus__Minus_param_u64_u8:
LFB275:
	pushq	%rbp
LCFI849:
	movq	%rsp, %rbp
LCFI850:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI851:
	ret
LFE275:
.no_dead_strip _Minus__uMinus_var_u64_i64
_Minus__uMinus_var_u64_i64:
LFB276:
	pushq	%rbp
LCFI852:
	movq	%rsp, %rbp
LCFI853:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI854:
	ret
LFE276:
.no_dead_strip _Minus__Minus_var_u64_i64
_Minus__Minus_var_u64_i64:
LFB277:
	pushq	%rbp
LCFI855:
	movq	%rsp, %rbp
LCFI856:
	movq	136+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI857:
	ret
LFE277:
.no_dead_strip _Minus__uMinus_param_u64_i64
_Minus__uMinus_param_u64_i64:
LFB278:
	pushq	%rbp
LCFI858:
	movq	%rsp, %rbp
LCFI859:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI860:
	ret
LFE278:
.no_dead_strip _Minus__Minus_param_u64_i64
_Minus__Minus_param_u64_i64:
LFB279:
	pushq	%rbp
LCFI861:
	movq	%rsp, %rbp
LCFI862:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI863:
	ret
LFE279:
.no_dead_strip _Minus__uMinus_var_u64_L
_Minus__uMinus_var_u64_L:
LFB280:
	pushq	%rbp
LCFI864:
	movq	%rsp, %rbp
LCFI865:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI866:
	ret
LFE280:
.no_dead_strip _Minus__Minus_var_u64_L
_Minus__Minus_var_u64_L:
LFB281:
	pushq	%rbp
LCFI867:
	movq	%rsp, %rbp
LCFI868:
	movq	136+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI869:
	ret
LFE281:
.no_dead_strip _Minus__uMinus_param_u64_L
_Minus__uMinus_param_u64_L:
LFB282:
	pushq	%rbp
LCFI870:
	movq	%rsp, %rbp
LCFI871:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI872:
	ret
LFE282:
.no_dead_strip _Minus__Minus_param_u64_L
_Minus__Minus_param_u64_L:
LFB283:
	pushq	%rbp
LCFI873:
	movq	%rsp, %rbp
LCFI874:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI875:
	ret
LFE283:
.no_dead_strip _Minus__uMinus_var_u64_i8
_Minus__uMinus_var_u64_i8:
LFB284:
	pushq	%rbp
LCFI876:
	movq	%rsp, %rbp
LCFI877:
	movq	136+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI878:
	ret
LFE284:
.no_dead_strip _Minus__Minus_var_u64_i8
_Minus__Minus_var_u64_i8:
LFB285:
	pushq	%rbp
LCFI879:
	movq	%rsp, %rbp
LCFI880:
	movq	136+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI881:
	ret
LFE285:
.no_dead_strip _Minus__uMinus_param_u64_i8
_Minus__uMinus_param_u64_i8:
LFB286:
	pushq	%rbp
LCFI882:
	movq	%rsp, %rbp
LCFI883:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI884:
	ret
LFE286:
.no_dead_strip _Minus__Minus_param_u64_i8
_Minus__Minus_param_u64_i8:
LFB287:
	pushq	%rbp
LCFI885:
	movq	%rsp, %rbp
LCFI886:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI887:
	ret
LFE287:
.no_dead_strip _Minus__Minus_var_f64_f64
_Minus__Minus_var_f64_f64:
LFB288:
	pushq	%rbp
LCFI888:
	movq	%rsp, %rbp
LCFI889:
	leaq	_MM_Minus(%rip), %rax
	addq	$144, %rax
	movsd	(%rax), %xmm0
	leaq	_MM_Minus(%rip), %rax
	addq	$144, %rax
	movsd	(%rax), %xmm1
	subsd	%xmm1, %xmm0
	leave
LCFI890:
	ret
LFE288:
.no_dead_strip _Minus__Minus_param_f64_f64
_Minus__Minus_param_f64_f64:
LFB289:
	pushq	%rbp
LCFI891:
	movq	%rsp, %rbp
LCFI892:
	movsd	%xmm0, -8(%rbp)
	movsd	%xmm1, -16(%rbp)
	movsd	-8(%rbp), %xmm0
	subsd	-16(%rbp), %xmm0
	leave
LCFI893:
	ret
LFE289:
.no_dead_strip _Minus__uMinus_var_i32_u16
_Minus__uMinus_var_i32_u16:
LFB290:
	pushq	%rbp
LCFI894:
	movq	%rsp, %rbp
LCFI895:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI896:
	ret
LFE290:
.no_dead_strip _Minus__Minus_var_i32_u16
_Minus__Minus_var_i32_u16:
LFB291:
	pushq	%rbp
LCFI897:
	movq	%rsp, %rbp
LCFI898:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L367
	movl	$11041, %edi
	call	__m3_fault
L367:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI899:
	ret
LFE291:
.no_dead_strip _Minus__uMinus_param_i32_u16
_Minus__uMinus_param_i32_u16:
LFB292:
	pushq	%rbp
LCFI900:
	movq	%rsp, %rbp
LCFI901:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI902:
	ret
LFE292:
.no_dead_strip _Minus__Minus_param_i32_u16
_Minus__Minus_param_i32_u16:
LFB293:
	pushq	%rbp
LCFI903:
	movq	%rsp, %rbp
LCFI904:
	subq	$16, %rsp
LCFI905:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L370
	movl	$11105, %edi
	call	__m3_fault
L370:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzwl	-6(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI906:
	ret
LFE293:
.no_dead_strip _Minus__uMinus_var_i32_u32
_Minus__uMinus_var_i32_u32:
LFB294:
	pushq	%rbp
LCFI907:
	movq	%rsp, %rbp
LCFI908:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI909:
	ret
LFE294:
.no_dead_strip _Minus__Minus_var_i32_u32
_Minus__Minus_var_i32_u32:
LFB295:
	pushq	%rbp
LCFI910:
	movq	%rsp, %rbp
LCFI911:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L373
	movl	$11169, %edi
	call	__m3_fault
L373:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI912:
	ret
LFE295:
.no_dead_strip _Minus__uMinus_param_i32_u32
_Minus__uMinus_param_i32_u32:
LFB296:
	pushq	%rbp
LCFI913:
	movq	%rsp, %rbp
LCFI914:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI915:
	ret
LFE296:
.no_dead_strip _Minus__Minus_param_i32_u32
_Minus__Minus_param_i32_u32:
LFB297:
	pushq	%rbp
LCFI916:
	movq	%rsp, %rbp
LCFI917:
	subq	$16, %rsp
LCFI918:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L376
	movl	$11233, %edi
	call	__m3_fault
L376:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI919:
	ret
LFE297:
.no_dead_strip _Minus__uMinus_var_i32_LC
_Minus__uMinus_var_i32_LC:
LFB298:
	pushq	%rbp
LCFI920:
	movq	%rsp, %rbp
LCFI921:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI922:
	ret
LFE298:
.no_dead_strip _Minus__Minus_var_i32_LC
_Minus__Minus_var_i32_LC:
LFB299:
	pushq	%rbp
LCFI923:
	movq	%rsp, %rbp
LCFI924:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI925:
	ret
LFE299:
.no_dead_strip _Minus__uMinus_param_i32_LC
_Minus__uMinus_param_i32_LC:
LFB300:
	pushq	%rbp
LCFI926:
	movq	%rsp, %rbp
LCFI927:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI928:
	ret
LFE300:
.no_dead_strip _Minus__Minus_param_i32_LC
_Minus__Minus_param_i32_LC:
LFB301:
	pushq	%rbp
LCFI929:
	movq	%rsp, %rbp
LCFI930:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI931:
	ret
LFE301:
.no_dead_strip _Minus__uMinus_var_i32_I
_Minus__uMinus_var_i32_I:
LFB302:
	pushq	%rbp
LCFI932:
	movq	%rsp, %rbp
LCFI933:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI934:
	ret
LFE302:
.no_dead_strip _Minus__Minus_var_i32_I
_Minus__Minus_var_i32_I:
LFB303:
	pushq	%rbp
LCFI935:
	movq	%rsp, %rbp
LCFI936:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L383
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L384
L383:
	movl	$11425, %edi
	call	__m3_fault
L384:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI937:
	ret
LFE303:
.no_dead_strip _Minus__uMinus_param_i32_I
_Minus__uMinus_param_i32_I:
LFB304:
	pushq	%rbp
LCFI938:
	movq	%rsp, %rbp
LCFI939:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI940:
	ret
LFE304:
.no_dead_strip _Minus__Minus_param_i32_I
_Minus__Minus_param_i32_I:
LFB305:
	pushq	%rbp
LCFI941:
	movq	%rsp, %rbp
LCFI942:
	subq	$16, %rsp
LCFI943:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	cmpq	$-2147483648, %rax
	jl	L387
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	cmpq	$2147483647, %rax
	jle	L388
L387:
	movl	$11489, %edi
	call	__m3_fault
L388:
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI944:
	ret
LFE305:
.no_dead_strip _Minus__uMinus_var_i32_i16
_Minus__uMinus_var_i32_i16:
LFB306:
	pushq	%rbp
LCFI945:
	movq	%rsp, %rbp
LCFI946:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI947:
	ret
LFE306:
.no_dead_strip _Minus__Minus_var_i32_i16
_Minus__Minus_var_i32_i16:
LFB307:
	pushq	%rbp
LCFI948:
	movq	%rsp, %rbp
LCFI949:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L391
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L392
L391:
	movl	$11553, %edi
	call	__m3_fault
L392:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI950:
	ret
LFE307:
.no_dead_strip _Minus__uMinus_param_i32_i16
_Minus__uMinus_param_i32_i16:
LFB308:
	pushq	%rbp
LCFI951:
	movq	%rsp, %rbp
LCFI952:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI953:
	ret
LFE308:
.no_dead_strip _Minus__Minus_param_i32_i16
_Minus__Minus_param_i32_i16:
LFB309:
	pushq	%rbp
LCFI954:
	movq	%rsp, %rbp
LCFI955:
	subq	$16, %rsp
LCFI956:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L395
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L396
L395:
	movl	$11617, %edi
	call	__m3_fault
L396:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movswq	-6(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI957:
	ret
LFE309:
.no_dead_strip _Minus__uMinus_var_i32_u64
_Minus__uMinus_var_i32_u64:
LFB310:
	pushq	%rbp
LCFI958:
	movq	%rsp, %rbp
LCFI959:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI960:
	ret
LFE310:
.no_dead_strip _Minus__Minus_var_i32_u64
_Minus__Minus_var_i32_u64:
LFB311:
	pushq	%rbp
LCFI961:
	movq	%rsp, %rbp
LCFI962:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI963:
	ret
LFE311:
.no_dead_strip _Minus__uMinus_param_i32_u64
_Minus__uMinus_param_i32_u64:
LFB312:
	pushq	%rbp
LCFI964:
	movq	%rsp, %rbp
LCFI965:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI966:
	ret
LFE312:
.no_dead_strip _Minus__Minus_param_i32_u64
_Minus__Minus_param_i32_u64:
LFB313:
	pushq	%rbp
LCFI967:
	movq	%rsp, %rbp
LCFI968:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI969:
	ret
LFE313:
.no_dead_strip _Minus__uMinus_var_i32_i32
_Minus__uMinus_var_i32_i32:
LFB314:
	pushq	%rbp
LCFI970:
	movq	%rsp, %rbp
LCFI971:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI972:
	ret
LFE314:
.no_dead_strip _Minus__Minus_var_i32_i32
_Minus__Minus_var_i32_i32:
LFB315:
	pushq	%rbp
LCFI973:
	movq	%rsp, %rbp
LCFI974:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L403
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L404
L403:
	movl	$11809, %edi
	call	__m3_fault
L404:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI975:
	ret
LFE315:
.no_dead_strip _Minus__uMinus_param_i32_i32
_Minus__uMinus_param_i32_i32:
LFB316:
	pushq	%rbp
LCFI976:
	movq	%rsp, %rbp
LCFI977:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI978:
	ret
LFE316:
.no_dead_strip _Minus__Minus_param_i32_i32
_Minus__Minus_param_i32_i32:
LFB317:
	pushq	%rbp
LCFI979:
	movq	%rsp, %rbp
LCFI980:
	subq	$16, %rsp
LCFI981:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L407
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L408
L407:
	movl	$11873, %edi
	call	__m3_fault
L408:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI982:
	ret
LFE317:
.no_dead_strip _Minus__uMinus_var_i32_C
_Minus__uMinus_var_i32_C:
LFB318:
	pushq	%rbp
LCFI983:
	movq	%rsp, %rbp
LCFI984:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI985:
	ret
LFE318:
.no_dead_strip _Minus__Minus_var_i32_C
_Minus__Minus_var_i32_C:
LFB319:
	pushq	%rbp
LCFI986:
	movq	%rsp, %rbp
LCFI987:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L411
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L412
L411:
	movl	$11937, %edi
	call	__m3_fault
L412:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI988:
	ret
LFE319:
.no_dead_strip _Minus__uMinus_param_i32_C
_Minus__uMinus_param_i32_C:
LFB320:
	pushq	%rbp
LCFI989:
	movq	%rsp, %rbp
LCFI990:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI991:
	ret
LFE320:
.no_dead_strip _Minus__Minus_param_i32_C
_Minus__Minus_param_i32_C:
LFB321:
	pushq	%rbp
LCFI992:
	movq	%rsp, %rbp
LCFI993:
	subq	$16, %rsp
LCFI994:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L415
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L416
L415:
	movl	$12001, %edi
	call	__m3_fault
L416:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI995:
	ret
LFE321:
.no_dead_strip _Minus__uMinus_var_i32_u8
_Minus__uMinus_var_i32_u8:
LFB322:
	pushq	%rbp
LCFI996:
	movq	%rsp, %rbp
LCFI997:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI998:
	ret
LFE322:
.no_dead_strip _Minus__Minus_var_i32_u8
_Minus__Minus_var_i32_u8:
LFB323:
	pushq	%rbp
LCFI999:
	movq	%rsp, %rbp
LCFI1000:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L419
	movl	$12065, %edi
	call	__m3_fault
L419:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1001:
	ret
LFE323:
.no_dead_strip _Minus__uMinus_param_i32_u8
_Minus__uMinus_param_i32_u8:
LFB324:
	pushq	%rbp
LCFI1002:
	movq	%rsp, %rbp
LCFI1003:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1004:
	ret
LFE324:
.no_dead_strip _Minus__Minus_param_i32_u8
_Minus__Minus_param_i32_u8:
LFB325:
	pushq	%rbp
LCFI1005:
	movq	%rsp, %rbp
LCFI1006:
	subq	$16, %rsp
LCFI1007:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jge	L422
	movl	$12129, %edi
	call	__m3_fault
L422:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzbl	-5(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1008:
	ret
LFE325:
.no_dead_strip _Minus__uMinus_var_i32_i64
_Minus__uMinus_var_i32_i64:
LFB326:
	pushq	%rbp
LCFI1009:
	movq	%rsp, %rbp
LCFI1010:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1011:
	ret
LFE326:
.no_dead_strip _Minus__Minus_var_i32_i64
_Minus__Minus_var_i32_i64:
LFB327:
	pushq	%rbp
LCFI1012:
	movq	%rsp, %rbp
LCFI1013:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1014:
	ret
LFE327:
.no_dead_strip _Minus__uMinus_param_i32_i64
_Minus__uMinus_param_i32_i64:
LFB328:
	pushq	%rbp
LCFI1015:
	movq	%rsp, %rbp
LCFI1016:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1017:
	ret
LFE328:
.no_dead_strip _Minus__Minus_param_i32_i64
_Minus__Minus_param_i32_i64:
LFB329:
	pushq	%rbp
LCFI1018:
	movq	%rsp, %rbp
LCFI1019:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI1020:
	ret
LFE329:
.no_dead_strip _Minus__uMinus_var_i32_L
_Minus__uMinus_var_i32_L:
LFB330:
	pushq	%rbp
LCFI1021:
	movq	%rsp, %rbp
LCFI1022:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1023:
	ret
LFE330:
.no_dead_strip _Minus__Minus_var_i32_L
_Minus__Minus_var_i32_L:
LFB331:
	pushq	%rbp
LCFI1024:
	movq	%rsp, %rbp
LCFI1025:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1026:
	ret
LFE331:
.no_dead_strip _Minus__uMinus_param_i32_L
_Minus__uMinus_param_i32_L:
LFB332:
	pushq	%rbp
LCFI1027:
	movq	%rsp, %rbp
LCFI1028:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1029:
	ret
LFE332:
.no_dead_strip _Minus__Minus_param_i32_L
_Minus__Minus_param_i32_L:
LFB333:
	pushq	%rbp
LCFI1030:
	movq	%rsp, %rbp
LCFI1031:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	subq	-16(%rbp), %rax
	leave
LCFI1032:
	ret
LFE333:
.no_dead_strip _Minus__uMinus_var_i32_i8
_Minus__uMinus_var_i32_i8:
LFB334:
	pushq	%rbp
LCFI1033:
	movq	%rsp, %rbp
LCFI1034:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1035:
	ret
LFE334:
.no_dead_strip _Minus__Minus_var_i32_i8
_Minus__Minus_var_i32_i8:
LFB335:
	pushq	%rbp
LCFI1036:
	movq	%rsp, %rbp
LCFI1037:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L433
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L434
L433:
	movl	$12449, %edi
	call	__m3_fault
L434:
	movl	152+_MM_Minus(%rip), %eax
	movslq	%eax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1038:
	ret
LFE335:
.no_dead_strip _Minus__uMinus_param_i32_i8
_Minus__uMinus_param_i32_i8:
LFB336:
	pushq	%rbp
LCFI1039:
	movq	%rsp, %rbp
LCFI1040:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1041:
	ret
LFE336:
.no_dead_strip _Minus__Minus_param_i32_i8
_Minus__Minus_param_i32_i8:
LFB337:
	pushq	%rbp
LCFI1042:
	movq	%rsp, %rbp
LCFI1043:
	subq	$16, %rsp
LCFI1044:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-2147483648, %rax
	jl	L437
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$2147483647, %rax
	jle	L438
L437:
	movl	$12513, %edi
	call	__m3_fault
L438:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movsbq	-5(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1045:
	ret
LFE337:
.no_dead_strip _Minus__uMinus_var_C_u16
_Minus__uMinus_var_C_u16:
LFB338:
	pushq	%rbp
LCFI1046:
	movq	%rsp, %rbp
LCFI1047:
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1048:
	ret
LFE338:
.no_dead_strip _Minus__Minus_var_C_u16
_Minus__Minus_var_C_u16:
LFB339:
	pushq	%rbp
LCFI1049:
	movq	%rsp, %rbp
LCFI1050:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L441
	movl	$12577, %edi
	call	__m3_fault
L441:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1051:
	ret
LFE339:
.no_dead_strip _Minus__uMinus_param_C_u16
_Minus__uMinus_param_C_u16:
LFB340:
	pushq	%rbp
LCFI1052:
	movq	%rsp, %rbp
LCFI1053:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1054:
	ret
LFE340:
.no_dead_strip _Minus__Minus_param_C_u16
_Minus__Minus_param_C_u16:
LFB341:
	pushq	%rbp
LCFI1055:
	movq	%rsp, %rbp
LCFI1056:
	subq	$16, %rsp
LCFI1057:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L444
	movl	$12641, %edi
	call	__m3_fault
L444:
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1058:
	ret
LFE341:
.no_dead_strip _Minus__uMinus_var_C_u32
_Minus__uMinus_var_C_u32:
LFB342:
	pushq	%rbp
LCFI1059:
	movq	%rsp, %rbp
LCFI1060:
	movq	160+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1061:
	ret
LFE342:
.no_dead_strip _Minus__Minus_var_C_u32
_Minus__Minus_var_C_u32:
LFB343:
	pushq	%rbp
LCFI1062:
	movq	%rsp, %rbp
LCFI1063:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L447
	movl	$12705, %edi
	call	__m3_fault
L447:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1064:
	ret
LFE343:
.no_dead_strip _Minus__uMinus_param_C_u32
_Minus__uMinus_param_C_u32:
LFB344:
	pushq	%rbp
LCFI1065:
	movq	%rsp, %rbp
LCFI1066:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1067:
	ret
LFE344:
.no_dead_strip _Minus__Minus_param_C_u32
_Minus__Minus_param_C_u32:
LFB345:
	pushq	%rbp
LCFI1068:
	movq	%rsp, %rbp
LCFI1069:
	subq	$16, %rsp
LCFI1070:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L450
	movl	$12769, %edi
	call	__m3_fault
L450:
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1071:
	ret
LFE345:
.no_dead_strip _Minus__uMinus_var_C_LC
_Minus__uMinus_var_C_LC:
LFB346:
	pushq	%rbp
LCFI1072:
	movq	%rsp, %rbp
LCFI1073:
	movq	160+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1074:
	ret
LFE346:
.no_dead_strip _Minus__Minus_var_C_LC
_Minus__Minus_var_C_LC:
LFB347:
	pushq	%rbp
LCFI1075:
	movq	%rsp, %rbp
LCFI1076:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1077:
	ret
LFE347:
.no_dead_strip _Minus__uMinus_param_C_LC
_Minus__uMinus_param_C_LC:
LFB348:
	pushq	%rbp
LCFI1078:
	movq	%rsp, %rbp
LCFI1079:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1080:
	ret
LFE348:
.no_dead_strip _Minus__Minus_param_C_LC
_Minus__Minus_param_C_LC:
LFB349:
	pushq	%rbp
LCFI1081:
	movq	%rsp, %rbp
LCFI1082:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1083:
	ret
LFE349:
.no_dead_strip _Minus__uMinus_var_C_I
_Minus__uMinus_var_C_I:
LFB350:
	pushq	%rbp
LCFI1084:
	movq	%rsp, %rbp
LCFI1085:
	movq	160+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1086:
	ret
LFE350:
.no_dead_strip _Minus__Minus_var_C_I
_Minus__Minus_var_C_I:
LFB351:
	pushq	%rbp
LCFI1087:
	movq	%rsp, %rbp
LCFI1088:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L457
	movl	$12961, %edi
	call	__m3_fault
L457:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1089:
	ret
LFE351:
.no_dead_strip _Minus__uMinus_param_C_I
_Minus__uMinus_param_C_I:
LFB352:
	pushq	%rbp
LCFI1090:
	movq	%rsp, %rbp
LCFI1091:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1092:
	ret
LFE352:
.no_dead_strip _Minus__Minus_param_C_I
_Minus__Minus_param_C_I:
LFB353:
	pushq	%rbp
LCFI1093:
	movq	%rsp, %rbp
LCFI1094:
	subq	$16, %rsp
LCFI1095:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L460
	movl	$13025, %edi
	call	__m3_fault
L460:
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1096:
	ret
LFE353:
.no_dead_strip _Minus__uMinus_var_C_i16
_Minus__uMinus_var_C_i16:
LFB354:
	pushq	%rbp
LCFI1097:
	movq	%rsp, %rbp
LCFI1098:
	movq	160+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1099:
	ret
LFE354:
.no_dead_strip _Minus__Minus_var_C_i16
_Minus__Minus_var_C_i16:
LFB355:
	pushq	%rbp
LCFI1100:
	movq	%rsp, %rbp
LCFI1101:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L463
	movl	$13089, %edi
	call	__m3_fault
L463:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1102:
	ret
LFE355:
.no_dead_strip _Minus__uMinus_param_C_i16
_Minus__uMinus_param_C_i16:
LFB356:
	pushq	%rbp
LCFI1103:
	movq	%rsp, %rbp
LCFI1104:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1105:
	ret
LFE356:
.no_dead_strip _Minus__Minus_param_C_i16
_Minus__Minus_param_C_i16:
LFB357:
	pushq	%rbp
LCFI1106:
	movq	%rsp, %rbp
LCFI1107:
	subq	$16, %rsp
LCFI1108:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L466
	movl	$13153, %edi
	call	__m3_fault
L466:
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1109:
	ret
LFE357:
.no_dead_strip _Minus__uMinus_var_C_u64
_Minus__uMinus_var_C_u64:
LFB358:
	pushq	%rbp
LCFI1110:
	movq	%rsp, %rbp
LCFI1111:
	movq	160+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1112:
	ret
LFE358:
.no_dead_strip _Minus__Minus_var_C_u64
_Minus__Minus_var_C_u64:
LFB359:
	pushq	%rbp
LCFI1113:
	movq	%rsp, %rbp
LCFI1114:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1115:
	ret
LFE359:
.no_dead_strip _Minus__uMinus_param_C_u64
_Minus__uMinus_param_C_u64:
LFB360:
	pushq	%rbp
LCFI1116:
	movq	%rsp, %rbp
LCFI1117:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1118:
	ret
LFE360:
.no_dead_strip _Minus__Minus_param_C_u64
_Minus__Minus_param_C_u64:
LFB361:
	pushq	%rbp
LCFI1119:
	movq	%rsp, %rbp
LCFI1120:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1121:
	ret
LFE361:
.no_dead_strip _Minus__uMinus_var_C_i32
_Minus__uMinus_var_C_i32:
LFB362:
	pushq	%rbp
LCFI1122:
	movq	%rsp, %rbp
LCFI1123:
	movq	160+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1124:
	ret
LFE362:
.no_dead_strip _Minus__Minus_var_C_i32
_Minus__Minus_var_C_i32:
LFB363:
	pushq	%rbp
LCFI1125:
	movq	%rsp, %rbp
LCFI1126:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L473
	movl	$13345, %edi
	call	__m3_fault
L473:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1127:
	ret
LFE363:
.no_dead_strip _Minus__uMinus_param_C_i32
_Minus__uMinus_param_C_i32:
LFB364:
	pushq	%rbp
LCFI1128:
	movq	%rsp, %rbp
LCFI1129:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1130:
	ret
LFE364:
.no_dead_strip _Minus__Minus_param_C_i32
_Minus__Minus_param_C_i32:
LFB365:
	pushq	%rbp
LCFI1131:
	movq	%rsp, %rbp
LCFI1132:
	subq	$16, %rsp
LCFI1133:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L476
	movl	$13409, %edi
	call	__m3_fault
L476:
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1134:
	ret
LFE365:
.no_dead_strip _Minus__uMinus_var_C_C
_Minus__uMinus_var_C_C:
LFB366:
	pushq	%rbp
LCFI1135:
	movq	%rsp, %rbp
LCFI1136:
	movq	160+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1137:
	ret
LFE366:
.no_dead_strip _Minus__Minus_var_C_C
_Minus__Minus_var_C_C:
LFB367:
	pushq	%rbp
LCFI1138:
	movq	%rsp, %rbp
LCFI1139:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L479
	movl	$13473, %edi
	call	__m3_fault
L479:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1140:
	ret
LFE367:
.no_dead_strip _Minus__uMinus_param_C_C
_Minus__uMinus_param_C_C:
LFB368:
	pushq	%rbp
LCFI1141:
	movq	%rsp, %rbp
LCFI1142:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1143:
	ret
LFE368:
.no_dead_strip _Minus__Minus_param_C_C
_Minus__Minus_param_C_C:
LFB369:
	pushq	%rbp
LCFI1144:
	movq	%rsp, %rbp
LCFI1145:
	subq	$16, %rsp
LCFI1146:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L482
	movl	$13537, %edi
	call	__m3_fault
L482:
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1147:
	ret
LFE369:
.no_dead_strip _Minus__uMinus_var_C_u8
_Minus__uMinus_var_C_u8:
LFB370:
	pushq	%rbp
LCFI1148:
	movq	%rsp, %rbp
LCFI1149:
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1150:
	ret
LFE370:
.no_dead_strip _Minus__Minus_var_C_u8
_Minus__Minus_var_C_u8:
LFB371:
	pushq	%rbp
LCFI1151:
	movq	%rsp, %rbp
LCFI1152:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L485
	movl	$13601, %edi
	call	__m3_fault
L485:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1153:
	ret
LFE371:
.no_dead_strip _Minus__uMinus_param_C_u8
_Minus__uMinus_param_C_u8:
LFB372:
	pushq	%rbp
LCFI1154:
	movq	%rsp, %rbp
LCFI1155:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1156:
	ret
LFE372:
.no_dead_strip _Minus__Minus_param_C_u8
_Minus__Minus_param_C_u8:
LFB373:
	pushq	%rbp
LCFI1157:
	movq	%rsp, %rbp
LCFI1158:
	subq	$16, %rsp
LCFI1159:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L488
	movl	$13665, %edi
	call	__m3_fault
L488:
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1160:
	ret
LFE373:
.no_dead_strip _Minus__uMinus_var_C_i64
_Minus__uMinus_var_C_i64:
LFB374:
	pushq	%rbp
LCFI1161:
	movq	%rsp, %rbp
LCFI1162:
	movq	160+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1163:
	ret
LFE374:
.no_dead_strip _Minus__Minus_var_C_i64
_Minus__Minus_var_C_i64:
LFB375:
	pushq	%rbp
LCFI1164:
	movq	%rsp, %rbp
LCFI1165:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1166:
	ret
LFE375:
.no_dead_strip _Minus__uMinus_param_C_i64
_Minus__uMinus_param_C_i64:
LFB376:
	pushq	%rbp
LCFI1167:
	movq	%rsp, %rbp
LCFI1168:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1169:
	ret
LFE376:
.no_dead_strip _Minus__Minus_param_C_i64
_Minus__Minus_param_C_i64:
LFB377:
	pushq	%rbp
LCFI1170:
	movq	%rsp, %rbp
LCFI1171:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1172:
	ret
LFE377:
.no_dead_strip _Minus__uMinus_var_C_L
_Minus__uMinus_var_C_L:
LFB378:
	pushq	%rbp
LCFI1173:
	movq	%rsp, %rbp
LCFI1174:
	movq	160+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1175:
	ret
LFE378:
.no_dead_strip _Minus__Minus_var_C_L
_Minus__Minus_var_C_L:
LFB379:
	pushq	%rbp
LCFI1176:
	movq	%rsp, %rbp
LCFI1177:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1178:
	ret
LFE379:
.no_dead_strip _Minus__uMinus_param_C_L
_Minus__uMinus_param_C_L:
LFB380:
	pushq	%rbp
LCFI1179:
	movq	%rsp, %rbp
LCFI1180:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1181:
	ret
LFE380:
.no_dead_strip _Minus__Minus_param_C_L
_Minus__Minus_param_C_L:
LFB381:
	pushq	%rbp
LCFI1182:
	movq	%rsp, %rbp
LCFI1183:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1184:
	ret
LFE381:
.no_dead_strip _Minus__uMinus_var_C_i8
_Minus__uMinus_var_C_i8:
LFB382:
	pushq	%rbp
LCFI1185:
	movq	%rsp, %rbp
LCFI1186:
	movq	160+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1187:
	ret
LFE382:
.no_dead_strip _Minus__Minus_var_C_i8
_Minus__Minus_var_C_i8:
LFB383:
	pushq	%rbp
LCFI1188:
	movq	%rsp, %rbp
LCFI1189:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L499
	movl	$13985, %edi
	call	__m3_fault
L499:
	movq	160+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1190:
	ret
LFE383:
.no_dead_strip _Minus__uMinus_param_C_i8
_Minus__uMinus_param_C_i8:
LFB384:
	pushq	%rbp
LCFI1191:
	movq	%rsp, %rbp
LCFI1192:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1193:
	ret
LFE384:
.no_dead_strip _Minus__Minus_param_C_i8
_Minus__Minus_param_C_i8:
LFB385:
	pushq	%rbp
LCFI1194:
	movq	%rsp, %rbp
LCFI1195:
	subq	$16, %rsp
LCFI1196:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L502
	movl	$14049, %edi
	call	__m3_fault
L502:
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1197:
	ret
LFE385:
.no_dead_strip _Minus__uMinus_var_u8_u16
_Minus__uMinus_var_u8_u16:
LFB386:
	pushq	%rbp
LCFI1198:
	movq	%rsp, %rbp
LCFI1199:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1200:
	ret
LFE386:
.no_dead_strip _Minus__Minus_var_u8_u16
_Minus__Minus_var_u8_u16:
LFB387:
	pushq	%rbp
LCFI1201:
	movq	%rsp, %rbp
LCFI1202:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L505
	movl	$14113, %edi
	call	__m3_fault
L505:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1203:
	ret
LFE387:
.no_dead_strip _Minus__uMinus_param_u8_u16
_Minus__uMinus_param_u8_u16:
LFB388:
	pushq	%rbp
LCFI1204:
	movq	%rsp, %rbp
LCFI1205:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1206:
	ret
LFE388:
.no_dead_strip _Minus__Minus_param_u8_u16
_Minus__Minus_param_u8_u16:
LFB389:
	pushq	%rbp
LCFI1207:
	movq	%rsp, %rbp
LCFI1208:
	subq	$16, %rsp
LCFI1209:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L508
	movl	$14177, %edi
	call	__m3_fault
L508:
	movzbl	-1(%rbp), %edx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1210:
	ret
LFE389:
.no_dead_strip _Minus__uMinus_var_u8_u32
_Minus__uMinus_var_u8_u32:
LFB390:
	pushq	%rbp
LCFI1211:
	movq	%rsp, %rbp
LCFI1212:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1213:
	ret
LFE390:
.no_dead_strip _Minus__Minus_var_u8_u32
_Minus__Minus_var_u8_u32:
LFB391:
	pushq	%rbp
LCFI1214:
	movq	%rsp, %rbp
LCFI1215:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L511
	movl	$14241, %edi
	call	__m3_fault
L511:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1216:
	ret
LFE391:
.no_dead_strip _Minus__uMinus_param_u8_u32
_Minus__uMinus_param_u8_u32:
LFB392:
	pushq	%rbp
LCFI1217:
	movq	%rsp, %rbp
LCFI1218:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1219:
	ret
LFE392:
.no_dead_strip _Minus__Minus_param_u8_u32
_Minus__Minus_param_u8_u32:
LFB393:
	pushq	%rbp
LCFI1220:
	movq	%rsp, %rbp
LCFI1221:
	subq	$16, %rsp
LCFI1222:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L514
	movl	$14305, %edi
	call	__m3_fault
L514:
	movzbl	-1(%rbp), %edx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1223:
	ret
LFE393:
.no_dead_strip _Minus__uMinus_var_u8_LC
_Minus__uMinus_var_u8_LC:
LFB394:
	pushq	%rbp
LCFI1224:
	movq	%rsp, %rbp
LCFI1225:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1226:
	ret
LFE394:
.no_dead_strip _Minus__Minus_var_u8_LC
_Minus__Minus_var_u8_LC:
LFB395:
	pushq	%rbp
LCFI1227:
	movq	%rsp, %rbp
LCFI1228:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1229:
	ret
LFE395:
.no_dead_strip _Minus__uMinus_param_u8_LC
_Minus__uMinus_param_u8_LC:
LFB396:
	pushq	%rbp
LCFI1230:
	movq	%rsp, %rbp
LCFI1231:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1232:
	ret
LFE396:
.no_dead_strip _Minus__Minus_param_u8_LC
_Minus__Minus_param_u8_LC:
LFB397:
	pushq	%rbp
LCFI1233:
	movq	%rsp, %rbp
LCFI1234:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1235:
	ret
LFE397:
.no_dead_strip _Minus__uMinus_var_u8_I
_Minus__uMinus_var_u8_I:
LFB398:
	pushq	%rbp
LCFI1236:
	movq	%rsp, %rbp
LCFI1237:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1238:
	ret
LFE398:
.no_dead_strip _Minus__Minus_var_u8_I
_Minus__Minus_var_u8_I:
LFB399:
	pushq	%rbp
LCFI1239:
	movq	%rsp, %rbp
LCFI1240:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L521
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L522
L521:
	movl	$14497, %edi
	call	__m3_fault
L522:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1241:
	ret
LFE399:
.no_dead_strip _Minus__uMinus_param_u8_I
_Minus__uMinus_param_u8_I:
LFB400:
	pushq	%rbp
LCFI1242:
	movq	%rsp, %rbp
LCFI1243:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1244:
	ret
LFE400:
.no_dead_strip _Minus__Minus_param_u8_I
_Minus__Minus_param_u8_I:
LFB401:
	pushq	%rbp
LCFI1245:
	movq	%rsp, %rbp
LCFI1246:
	subq	$16, %rsp
LCFI1247:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	testq	%rax, %rax
	js	L525
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	cmpq	$255, %rax
	jle	L526
L525:
	movl	$14561, %edi
	call	__m3_fault
L526:
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1248:
	ret
LFE401:
.no_dead_strip _Minus__uMinus_var_u8_i16
_Minus__uMinus_var_u8_i16:
LFB402:
	pushq	%rbp
LCFI1249:
	movq	%rsp, %rbp
LCFI1250:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1251:
	ret
LFE402:
.no_dead_strip _Minus__Minus_var_u8_i16
_Minus__Minus_var_u8_i16:
LFB403:
	pushq	%rbp
LCFI1252:
	movq	%rsp, %rbp
LCFI1253:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L529
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L530
L529:
	movl	$14625, %edi
	call	__m3_fault
L530:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1254:
	ret
LFE403:
.no_dead_strip _Minus__uMinus_param_u8_i16
_Minus__uMinus_param_u8_i16:
LFB404:
	pushq	%rbp
LCFI1255:
	movq	%rsp, %rbp
LCFI1256:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1257:
	ret
LFE404:
.no_dead_strip _Minus__Minus_param_u8_i16
_Minus__Minus_param_u8_i16:
LFB405:
	pushq	%rbp
LCFI1258:
	movq	%rsp, %rbp
LCFI1259:
	subq	$16, %rsp
LCFI1260:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L533
	movzbl	-1(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L534
L533:
	movl	$14689, %edi
	call	__m3_fault
L534:
	movzbl	-1(%rbp), %edx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1261:
	ret
LFE405:
.no_dead_strip _Minus__uMinus_var_u8_u64
_Minus__uMinus_var_u8_u64:
LFB406:
	pushq	%rbp
LCFI1262:
	movq	%rsp, %rbp
LCFI1263:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1264:
	ret
LFE406:
.no_dead_strip _Minus__Minus_var_u8_u64
_Minus__Minus_var_u8_u64:
LFB407:
	pushq	%rbp
LCFI1265:
	movq	%rsp, %rbp
LCFI1266:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1267:
	ret
LFE407:
.no_dead_strip _Minus__uMinus_param_u8_u64
_Minus__uMinus_param_u8_u64:
LFB408:
	pushq	%rbp
LCFI1268:
	movq	%rsp, %rbp
LCFI1269:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1270:
	ret
LFE408:
.no_dead_strip _Minus__Minus_param_u8_u64
_Minus__Minus_param_u8_u64:
LFB409:
	pushq	%rbp
LCFI1271:
	movq	%rsp, %rbp
LCFI1272:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1273:
	ret
LFE409:
.no_dead_strip _Minus__uMinus_var_u8_i32
_Minus__uMinus_var_u8_i32:
LFB410:
	pushq	%rbp
LCFI1274:
	movq	%rsp, %rbp
LCFI1275:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1276:
	ret
LFE410:
.no_dead_strip _Minus__Minus_var_u8_i32
_Minus__Minus_var_u8_i32:
LFB411:
	pushq	%rbp
LCFI1277:
	movq	%rsp, %rbp
LCFI1278:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L541
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L542
L541:
	movl	$14881, %edi
	call	__m3_fault
L542:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1279:
	ret
LFE411:
.no_dead_strip _Minus__uMinus_param_u8_i32
_Minus__uMinus_param_u8_i32:
LFB412:
	pushq	%rbp
LCFI1280:
	movq	%rsp, %rbp
LCFI1281:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1282:
	ret
LFE412:
.no_dead_strip _Minus__Minus_param_u8_i32
_Minus__Minus_param_u8_i32:
LFB413:
	pushq	%rbp
LCFI1283:
	movq	%rsp, %rbp
LCFI1284:
	subq	$16, %rsp
LCFI1285:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L545
	movzbl	-1(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L546
L545:
	movl	$14945, %edi
	call	__m3_fault
L546:
	movzbl	-1(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1286:
	ret
LFE413:
.no_dead_strip _Minus__uMinus_var_u8_C
_Minus__uMinus_var_u8_C:
LFB414:
	pushq	%rbp
LCFI1287:
	movq	%rsp, %rbp
LCFI1288:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1289:
	ret
LFE414:
.no_dead_strip _Minus__Minus_var_u8_C
_Minus__Minus_var_u8_C:
LFB415:
	pushq	%rbp
LCFI1290:
	movq	%rsp, %rbp
LCFI1291:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L549
	movl	$15009, %edi
	call	__m3_fault
L549:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1292:
	ret
LFE415:
.no_dead_strip _Minus__uMinus_param_u8_C
_Minus__uMinus_param_u8_C:
LFB416:
	pushq	%rbp
LCFI1293:
	movq	%rsp, %rbp
LCFI1294:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1295:
	ret
LFE416:
.no_dead_strip _Minus__Minus_param_u8_C
_Minus__Minus_param_u8_C:
LFB417:
	pushq	%rbp
LCFI1296:
	movq	%rsp, %rbp
LCFI1297:
	subq	$16, %rsp
LCFI1298:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L552
	movl	$15073, %edi
	call	__m3_fault
L552:
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1299:
	ret
LFE417:
.no_dead_strip _Minus__uMinus_var_u8_u8
_Minus__uMinus_var_u8_u8:
LFB418:
	pushq	%rbp
LCFI1300:
	movq	%rsp, %rbp
LCFI1301:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1302:
	ret
LFE418:
.no_dead_strip _Minus__Minus_var_u8_u8
_Minus__Minus_var_u8_u8:
LFB419:
	pushq	%rbp
LCFI1303:
	movq	%rsp, %rbp
LCFI1304:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L555
	movl	$15137, %edi
	call	__m3_fault
L555:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1305:
	ret
LFE419:
.no_dead_strip _Minus__uMinus_param_u8_u8
_Minus__uMinus_param_u8_u8:
LFB420:
	pushq	%rbp
LCFI1306:
	movq	%rsp, %rbp
LCFI1307:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1308:
	ret
LFE420:
.no_dead_strip _Minus__Minus_param_u8_u8
_Minus__Minus_param_u8_u8:
LFB421:
	pushq	%rbp
LCFI1309:
	movq	%rsp, %rbp
LCFI1310:
	subq	$16, %rsp
LCFI1311:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	jns	L558
	movl	$15201, %edi
	call	__m3_fault
L558:
	movzbl	-1(%rbp), %edx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1312:
	ret
LFE421:
.no_dead_strip _Minus__uMinus_var_u8_i64
_Minus__uMinus_var_u8_i64:
LFB422:
	pushq	%rbp
LCFI1313:
	movq	%rsp, %rbp
LCFI1314:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1315:
	ret
LFE422:
.no_dead_strip _Minus__Minus_var_u8_i64
_Minus__Minus_var_u8_i64:
LFB423:
	pushq	%rbp
LCFI1316:
	movq	%rsp, %rbp
LCFI1317:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1318:
	ret
LFE423:
.no_dead_strip _Minus__uMinus_param_u8_i64
_Minus__uMinus_param_u8_i64:
LFB424:
	pushq	%rbp
LCFI1319:
	movq	%rsp, %rbp
LCFI1320:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1321:
	ret
LFE424:
.no_dead_strip _Minus__Minus_param_u8_i64
_Minus__Minus_param_u8_i64:
LFB425:
	pushq	%rbp
LCFI1322:
	movq	%rsp, %rbp
LCFI1323:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1324:
	ret
LFE425:
.no_dead_strip _Minus__uMinus_var_u8_L
_Minus__uMinus_var_u8_L:
LFB426:
	pushq	%rbp
LCFI1325:
	movq	%rsp, %rbp
LCFI1326:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1327:
	ret
LFE426:
.no_dead_strip _Minus__Minus_var_u8_L
_Minus__Minus_var_u8_L:
LFB427:
	pushq	%rbp
LCFI1328:
	movq	%rsp, %rbp
LCFI1329:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1330:
	ret
LFE427:
.no_dead_strip _Minus__uMinus_param_u8_L
_Minus__uMinus_param_u8_L:
LFB428:
	pushq	%rbp
LCFI1331:
	movq	%rsp, %rbp
LCFI1332:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1333:
	ret
LFE428:
.no_dead_strip _Minus__Minus_param_u8_L
_Minus__Minus_param_u8_L:
LFB429:
	pushq	%rbp
LCFI1334:
	movq	%rsp, %rbp
LCFI1335:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	subq	-16(%rbp), %rax
	leave
LCFI1336:
	ret
LFE429:
.no_dead_strip _Minus__uMinus_var_u8_i8
_Minus__uMinus_var_u8_i8:
LFB430:
	pushq	%rbp
LCFI1337:
	movq	%rsp, %rbp
LCFI1338:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1339:
	ret
LFE430:
.no_dead_strip _Minus__Minus_var_u8_i8
_Minus__Minus_var_u8_i8:
LFB431:
	pushq	%rbp
LCFI1340:
	movq	%rsp, %rbp
LCFI1341:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L569
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L570
L569:
	movl	$15521, %edi
	call	__m3_fault
L570:
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %edx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1342:
	ret
LFE431:
.no_dead_strip _Minus__uMinus_param_u8_i8
_Minus__uMinus_param_u8_i8:
LFB432:
	pushq	%rbp
LCFI1343:
	movq	%rsp, %rbp
LCFI1344:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1345:
	ret
LFE432:
.no_dead_strip _Minus__Minus_param_u8_i8
_Minus__Minus_param_u8_i8:
LFB433:
	pushq	%rbp
LCFI1346:
	movq	%rsp, %rbp
LCFI1347:
	subq	$16, %rsp
LCFI1348:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	testq	%rax, %rax
	js	L573
	movzbl	-1(%rbp), %edx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$255, %rax
	jle	L574
L573:
	movl	$15585, %edi
	call	__m3_fault
L574:
	movzbl	-1(%rbp), %edx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1349:
	ret
LFE433:
.no_dead_strip _Minus__uMinus_var_i64_u16
_Minus__uMinus_var_i64_u16:
LFB434:
	pushq	%rbp
LCFI1350:
	movq	%rsp, %rbp
LCFI1351:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1352:
	ret
LFE434:
.no_dead_strip _Minus__Minus_var_i64_u16
_Minus__Minus_var_i64_u16:
LFB435:
	pushq	%rbp
LCFI1353:
	movq	%rsp, %rbp
LCFI1354:
	movq	176+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1355:
	ret
LFE435:
.no_dead_strip _Minus__uMinus_param_i64_u16
_Minus__uMinus_param_i64_u16:
LFB436:
	pushq	%rbp
LCFI1356:
	movq	%rsp, %rbp
LCFI1357:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1358:
	ret
LFE436:
.no_dead_strip _Minus__Minus_param_i64_u16
_Minus__Minus_param_i64_u16:
LFB437:
	pushq	%rbp
LCFI1359:
	movq	%rsp, %rbp
LCFI1360:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1361:
	ret
LFE437:
.no_dead_strip _Minus__uMinus_var_i64_u32
_Minus__uMinus_var_i64_u32:
LFB438:
	pushq	%rbp
LCFI1362:
	movq	%rsp, %rbp
LCFI1363:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1364:
	ret
LFE438:
.no_dead_strip _Minus__Minus_var_i64_u32
_Minus__Minus_var_i64_u32:
LFB439:
	pushq	%rbp
LCFI1365:
	movq	%rsp, %rbp
LCFI1366:
	movq	176+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1367:
	ret
LFE439:
.no_dead_strip _Minus__uMinus_param_i64_u32
_Minus__uMinus_param_i64_u32:
LFB440:
	pushq	%rbp
LCFI1368:
	movq	%rsp, %rbp
LCFI1369:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1370:
	ret
LFE440:
.no_dead_strip _Minus__Minus_param_i64_u32
_Minus__Minus_param_i64_u32:
LFB441:
	pushq	%rbp
LCFI1371:
	movq	%rsp, %rbp
LCFI1372:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1373:
	ret
LFE441:
.no_dead_strip _Minus__uMinus_var_i64_LC
_Minus__uMinus_var_i64_LC:
LFB442:
	pushq	%rbp
LCFI1374:
	movq	%rsp, %rbp
LCFI1375:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1376:
	ret
LFE442:
.no_dead_strip _Minus__Minus_var_i64_LC
_Minus__Minus_var_i64_LC:
LFB443:
	pushq	%rbp
LCFI1377:
	movq	%rsp, %rbp
LCFI1378:
	movq	176+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1379:
	ret
LFE443:
.no_dead_strip _Minus__uMinus_param_i64_LC
_Minus__uMinus_param_i64_LC:
LFB444:
	pushq	%rbp
LCFI1380:
	movq	%rsp, %rbp
LCFI1381:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1382:
	ret
LFE444:
.no_dead_strip _Minus__Minus_param_i64_LC
_Minus__Minus_param_i64_LC:
LFB445:
	pushq	%rbp
LCFI1383:
	movq	%rsp, %rbp
LCFI1384:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1385:
	ret
LFE445:
.no_dead_strip _Minus__uMinus_var_i64_I
_Minus__uMinus_var_i64_I:
LFB446:
	pushq	%rbp
LCFI1386:
	movq	%rsp, %rbp
LCFI1387:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1388:
	ret
LFE446:
.no_dead_strip _Minus__Minus_var_i64_I
_Minus__Minus_var_i64_I:
LFB447:
	pushq	%rbp
LCFI1389:
	movq	%rsp, %rbp
LCFI1390:
	movq	176+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1391:
	ret
LFE447:
.no_dead_strip _Minus__uMinus_param_i64_I
_Minus__uMinus_param_i64_I:
LFB448:
	pushq	%rbp
LCFI1392:
	movq	%rsp, %rbp
LCFI1393:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1394:
	ret
LFE448:
.no_dead_strip _Minus__Minus_param_i64_I
_Minus__Minus_param_i64_I:
LFB449:
	pushq	%rbp
LCFI1395:
	movq	%rsp, %rbp
LCFI1396:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1397:
	ret
LFE449:
.no_dead_strip _Minus__uMinus_var_i64_i16
_Minus__uMinus_var_i64_i16:
LFB450:
	pushq	%rbp
LCFI1398:
	movq	%rsp, %rbp
LCFI1399:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1400:
	ret
LFE450:
.no_dead_strip _Minus__Minus_var_i64_i16
_Minus__Minus_var_i64_i16:
LFB451:
	pushq	%rbp
LCFI1401:
	movq	%rsp, %rbp
LCFI1402:
	movq	176+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1403:
	ret
LFE451:
.no_dead_strip _Minus__uMinus_param_i64_i16
_Minus__uMinus_param_i64_i16:
LFB452:
	pushq	%rbp
LCFI1404:
	movq	%rsp, %rbp
LCFI1405:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1406:
	ret
LFE452:
.no_dead_strip _Minus__Minus_param_i64_i16
_Minus__Minus_param_i64_i16:
LFB453:
	pushq	%rbp
LCFI1407:
	movq	%rsp, %rbp
LCFI1408:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1409:
	ret
LFE453:
.no_dead_strip _Minus__uMinus_var_i64_u64
_Minus__uMinus_var_i64_u64:
LFB454:
	pushq	%rbp
LCFI1410:
	movq	%rsp, %rbp
LCFI1411:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1412:
	ret
LFE454:
.no_dead_strip _Minus__Minus_var_i64_u64
_Minus__Minus_var_i64_u64:
LFB455:
	pushq	%rbp
LCFI1413:
	movq	%rsp, %rbp
LCFI1414:
	movq	176+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1415:
	ret
LFE455:
.no_dead_strip _Minus__uMinus_param_i64_u64
_Minus__uMinus_param_i64_u64:
LFB456:
	pushq	%rbp
LCFI1416:
	movq	%rsp, %rbp
LCFI1417:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1418:
	ret
LFE456:
.no_dead_strip _Minus__Minus_param_i64_u64
_Minus__Minus_param_i64_u64:
LFB457:
	pushq	%rbp
LCFI1419:
	movq	%rsp, %rbp
LCFI1420:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1421:
	ret
LFE457:
.no_dead_strip _Minus__uMinus_var_i64_i32
_Minus__uMinus_var_i64_i32:
LFB458:
	pushq	%rbp
LCFI1422:
	movq	%rsp, %rbp
LCFI1423:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1424:
	ret
LFE458:
.no_dead_strip _Minus__Minus_var_i64_i32
_Minus__Minus_var_i64_i32:
LFB459:
	pushq	%rbp
LCFI1425:
	movq	%rsp, %rbp
LCFI1426:
	movq	176+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1427:
	ret
LFE459:
.no_dead_strip _Minus__uMinus_param_i64_i32
_Minus__uMinus_param_i64_i32:
LFB460:
	pushq	%rbp
LCFI1428:
	movq	%rsp, %rbp
LCFI1429:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1430:
	ret
LFE460:
.no_dead_strip _Minus__Minus_param_i64_i32
_Minus__Minus_param_i64_i32:
LFB461:
	pushq	%rbp
LCFI1431:
	movq	%rsp, %rbp
LCFI1432:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1433:
	ret
LFE461:
.no_dead_strip _Minus__uMinus_var_i64_C
_Minus__uMinus_var_i64_C:
LFB462:
	pushq	%rbp
LCFI1434:
	movq	%rsp, %rbp
LCFI1435:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1436:
	ret
LFE462:
.no_dead_strip _Minus__Minus_var_i64_C
_Minus__Minus_var_i64_C:
LFB463:
	pushq	%rbp
LCFI1437:
	movq	%rsp, %rbp
LCFI1438:
	movq	176+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1439:
	ret
LFE463:
.no_dead_strip _Minus__uMinus_param_i64_C
_Minus__uMinus_param_i64_C:
LFB464:
	pushq	%rbp
LCFI1440:
	movq	%rsp, %rbp
LCFI1441:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1442:
	ret
LFE464:
.no_dead_strip _Minus__Minus_param_i64_C
_Minus__Minus_param_i64_C:
LFB465:
	pushq	%rbp
LCFI1443:
	movq	%rsp, %rbp
LCFI1444:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1445:
	ret
LFE465:
.no_dead_strip _Minus__uMinus_var_i64_u8
_Minus__uMinus_var_i64_u8:
LFB466:
	pushq	%rbp
LCFI1446:
	movq	%rsp, %rbp
LCFI1447:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1448:
	ret
LFE466:
.no_dead_strip _Minus__Minus_var_i64_u8
_Minus__Minus_var_i64_u8:
LFB467:
	pushq	%rbp
LCFI1449:
	movq	%rsp, %rbp
LCFI1450:
	movq	176+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1451:
	ret
LFE467:
.no_dead_strip _Minus__uMinus_param_i64_u8
_Minus__uMinus_param_i64_u8:
LFB468:
	pushq	%rbp
LCFI1452:
	movq	%rsp, %rbp
LCFI1453:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1454:
	ret
LFE468:
.no_dead_strip _Minus__Minus_param_i64_u8
_Minus__Minus_param_i64_u8:
LFB469:
	pushq	%rbp
LCFI1455:
	movq	%rsp, %rbp
LCFI1456:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1457:
	ret
LFE469:
.no_dead_strip _Minus__uMinus_var_i64_i64
_Minus__uMinus_var_i64_i64:
LFB470:
	pushq	%rbp
LCFI1458:
	movq	%rsp, %rbp
LCFI1459:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1460:
	ret
LFE470:
.no_dead_strip _Minus__Minus_var_i64_i64
_Minus__Minus_var_i64_i64:
LFB471:
	pushq	%rbp
LCFI1461:
	movq	%rsp, %rbp
LCFI1462:
	movq	176+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1463:
	ret
LFE471:
.no_dead_strip _Minus__uMinus_param_i64_i64
_Minus__uMinus_param_i64_i64:
LFB472:
	pushq	%rbp
LCFI1464:
	movq	%rsp, %rbp
LCFI1465:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1466:
	ret
LFE472:
.no_dead_strip _Minus__Minus_param_i64_i64
_Minus__Minus_param_i64_i64:
LFB473:
	pushq	%rbp
LCFI1467:
	movq	%rsp, %rbp
LCFI1468:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1469:
	ret
LFE473:
.no_dead_strip _Minus__uMinus_var_i64_L
_Minus__uMinus_var_i64_L:
LFB474:
	pushq	%rbp
LCFI1470:
	movq	%rsp, %rbp
LCFI1471:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1472:
	ret
LFE474:
.no_dead_strip _Minus__Minus_var_i64_L
_Minus__Minus_var_i64_L:
LFB475:
	pushq	%rbp
LCFI1473:
	movq	%rsp, %rbp
LCFI1474:
	movq	176+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1475:
	ret
LFE475:
.no_dead_strip _Minus__uMinus_param_i64_L
_Minus__uMinus_param_i64_L:
LFB476:
	pushq	%rbp
LCFI1476:
	movq	%rsp, %rbp
LCFI1477:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1478:
	ret
LFE476:
.no_dead_strip _Minus__Minus_param_i64_L
_Minus__Minus_param_i64_L:
LFB477:
	pushq	%rbp
LCFI1479:
	movq	%rsp, %rbp
LCFI1480:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1481:
	ret
LFE477:
.no_dead_strip _Minus__uMinus_var_i64_i8
_Minus__uMinus_var_i64_i8:
LFB478:
	pushq	%rbp
LCFI1482:
	movq	%rsp, %rbp
LCFI1483:
	movq	176+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1484:
	ret
LFE478:
.no_dead_strip _Minus__Minus_var_i64_i8
_Minus__Minus_var_i64_i8:
LFB479:
	pushq	%rbp
LCFI1485:
	movq	%rsp, %rbp
LCFI1486:
	movq	176+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1487:
	ret
LFE479:
.no_dead_strip _Minus__uMinus_param_i64_i8
_Minus__uMinus_param_i64_i8:
LFB480:
	pushq	%rbp
LCFI1488:
	movq	%rsp, %rbp
LCFI1489:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1490:
	ret
LFE480:
.no_dead_strip _Minus__Minus_param_i64_i8
_Minus__Minus_param_i64_i8:
LFB481:
	pushq	%rbp
LCFI1491:
	movq	%rsp, %rbp
LCFI1492:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1493:
	ret
LFE481:
.no_dead_strip _Minus__Minus_var_f32_f32
_Minus__Minus_var_f32_f32:
LFB482:
	pushq	%rbp
LCFI1494:
	movq	%rsp, %rbp
LCFI1495:
	leaq	_MM_Minus(%rip), %rax
	addq	$184, %rax
	movss	(%rax), %xmm0
	leaq	_MM_Minus(%rip), %rax
	addq	$184, %rax
	movss	(%rax), %xmm1
	subss	%xmm1, %xmm0
	leave
LCFI1496:
	ret
LFE482:
.no_dead_strip _Minus__Minus_param_f32_f32
_Minus__Minus_param_f32_f32:
LFB483:
	pushq	%rbp
LCFI1497:
	movq	%rsp, %rbp
LCFI1498:
	movss	%xmm0, -4(%rbp)
	movss	%xmm1, -8(%rbp)
	movss	-4(%rbp), %xmm0
	subss	-8(%rbp), %xmm0
	leave
LCFI1499:
	ret
LFE483:
.no_dead_strip _Minus__uMinus_var_L_u16
_Minus__uMinus_var_L_u16:
LFB484:
	pushq	%rbp
LCFI1500:
	movq	%rsp, %rbp
LCFI1501:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1502:
	ret
LFE484:
.no_dead_strip _Minus__Minus_var_L_u16
_Minus__Minus_var_L_u16:
LFB485:
	pushq	%rbp
LCFI1503:
	movq	%rsp, %rbp
LCFI1504:
	movq	192+_MM_Minus(%rip), %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1505:
	ret
LFE485:
.no_dead_strip _Minus__uMinus_param_L_u16
_Minus__uMinus_param_L_u16:
LFB486:
	pushq	%rbp
LCFI1506:
	movq	%rsp, %rbp
LCFI1507:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1508:
	ret
LFE486:
.no_dead_strip _Minus__Minus_param_L_u16
_Minus__Minus_param_L_u16:
LFB487:
	pushq	%rbp
LCFI1509:
	movq	%rsp, %rbp
LCFI1510:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1511:
	ret
LFE487:
.no_dead_strip _Minus__uMinus_var_L_u32
_Minus__uMinus_var_L_u32:
LFB488:
	pushq	%rbp
LCFI1512:
	movq	%rsp, %rbp
LCFI1513:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1514:
	ret
LFE488:
.no_dead_strip _Minus__Minus_var_L_u32
_Minus__Minus_var_L_u32:
LFB489:
	pushq	%rbp
LCFI1515:
	movq	%rsp, %rbp
LCFI1516:
	movq	192+_MM_Minus(%rip), %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1517:
	ret
LFE489:
.no_dead_strip _Minus__uMinus_param_L_u32
_Minus__uMinus_param_L_u32:
LFB490:
	pushq	%rbp
LCFI1518:
	movq	%rsp, %rbp
LCFI1519:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1520:
	ret
LFE490:
.no_dead_strip _Minus__Minus_param_L_u32
_Minus__Minus_param_L_u32:
LFB491:
	pushq	%rbp
LCFI1521:
	movq	%rsp, %rbp
LCFI1522:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1523:
	ret
LFE491:
.no_dead_strip _Minus__uMinus_var_L_LC
_Minus__uMinus_var_L_LC:
LFB492:
	pushq	%rbp
LCFI1524:
	movq	%rsp, %rbp
LCFI1525:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1526:
	ret
LFE492:
.no_dead_strip _Minus__Minus_var_L_LC
_Minus__Minus_var_L_LC:
LFB493:
	pushq	%rbp
LCFI1527:
	movq	%rsp, %rbp
LCFI1528:
	movq	192+_MM_Minus(%rip), %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1529:
	ret
LFE493:
.no_dead_strip _Minus__uMinus_param_L_LC
_Minus__uMinus_param_L_LC:
LFB494:
	pushq	%rbp
LCFI1530:
	movq	%rsp, %rbp
LCFI1531:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1532:
	ret
LFE494:
.no_dead_strip _Minus__Minus_param_L_LC
_Minus__Minus_param_L_LC:
LFB495:
	pushq	%rbp
LCFI1533:
	movq	%rsp, %rbp
LCFI1534:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1535:
	ret
LFE495:
.no_dead_strip _Minus__uMinus_var_L_I
_Minus__uMinus_var_L_I:
LFB496:
	pushq	%rbp
LCFI1536:
	movq	%rsp, %rbp
LCFI1537:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1538:
	ret
LFE496:
.no_dead_strip _Minus__Minus_var_L_I
_Minus__Minus_var_L_I:
LFB497:
	pushq	%rbp
LCFI1539:
	movq	%rsp, %rbp
LCFI1540:
	movq	192+_MM_Minus(%rip), %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1541:
	ret
LFE497:
.no_dead_strip _Minus__uMinus_param_L_I
_Minus__uMinus_param_L_I:
LFB498:
	pushq	%rbp
LCFI1542:
	movq	%rsp, %rbp
LCFI1543:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1544:
	ret
LFE498:
.no_dead_strip _Minus__Minus_param_L_I
_Minus__Minus_param_L_I:
LFB499:
	pushq	%rbp
LCFI1545:
	movq	%rsp, %rbp
LCFI1546:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1547:
	ret
LFE499:
.no_dead_strip _Minus__uMinus_var_L_i16
_Minus__uMinus_var_L_i16:
LFB500:
	pushq	%rbp
LCFI1548:
	movq	%rsp, %rbp
LCFI1549:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1550:
	ret
LFE500:
.no_dead_strip _Minus__Minus_var_L_i16
_Minus__Minus_var_L_i16:
LFB501:
	pushq	%rbp
LCFI1551:
	movq	%rsp, %rbp
LCFI1552:
	movq	192+_MM_Minus(%rip), %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1553:
	ret
LFE501:
.no_dead_strip _Minus__uMinus_param_L_i16
_Minus__uMinus_param_L_i16:
LFB502:
	pushq	%rbp
LCFI1554:
	movq	%rsp, %rbp
LCFI1555:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1556:
	ret
LFE502:
.no_dead_strip _Minus__Minus_param_L_i16
_Minus__Minus_param_L_i16:
LFB503:
	pushq	%rbp
LCFI1557:
	movq	%rsp, %rbp
LCFI1558:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1559:
	ret
LFE503:
.no_dead_strip _Minus__uMinus_var_L_u64
_Minus__uMinus_var_L_u64:
LFB504:
	pushq	%rbp
LCFI1560:
	movq	%rsp, %rbp
LCFI1561:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1562:
	ret
LFE504:
.no_dead_strip _Minus__Minus_var_L_u64
_Minus__Minus_var_L_u64:
LFB505:
	pushq	%rbp
LCFI1563:
	movq	%rsp, %rbp
LCFI1564:
	movq	192+_MM_Minus(%rip), %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1565:
	ret
LFE505:
.no_dead_strip _Minus__uMinus_param_L_u64
_Minus__uMinus_param_L_u64:
LFB506:
	pushq	%rbp
LCFI1566:
	movq	%rsp, %rbp
LCFI1567:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1568:
	ret
LFE506:
.no_dead_strip _Minus__Minus_param_L_u64
_Minus__Minus_param_L_u64:
LFB507:
	pushq	%rbp
LCFI1569:
	movq	%rsp, %rbp
LCFI1570:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1571:
	ret
LFE507:
.no_dead_strip _Minus__uMinus_var_L_i32
_Minus__uMinus_var_L_i32:
LFB508:
	pushq	%rbp
LCFI1572:
	movq	%rsp, %rbp
LCFI1573:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1574:
	ret
LFE508:
.no_dead_strip _Minus__Minus_var_L_i32
_Minus__Minus_var_L_i32:
LFB509:
	pushq	%rbp
LCFI1575:
	movq	%rsp, %rbp
LCFI1576:
	movq	192+_MM_Minus(%rip), %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1577:
	ret
LFE509:
.no_dead_strip _Minus__uMinus_param_L_i32
_Minus__uMinus_param_L_i32:
LFB510:
	pushq	%rbp
LCFI1578:
	movq	%rsp, %rbp
LCFI1579:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1580:
	ret
LFE510:
.no_dead_strip _Minus__Minus_param_L_i32
_Minus__Minus_param_L_i32:
LFB511:
	pushq	%rbp
LCFI1581:
	movq	%rsp, %rbp
LCFI1582:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1583:
	ret
LFE511:
.no_dead_strip _Minus__uMinus_var_L_C
_Minus__uMinus_var_L_C:
LFB512:
	pushq	%rbp
LCFI1584:
	movq	%rsp, %rbp
LCFI1585:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1586:
	ret
LFE512:
.no_dead_strip _Minus__Minus_var_L_C
_Minus__Minus_var_L_C:
LFB513:
	pushq	%rbp
LCFI1587:
	movq	%rsp, %rbp
LCFI1588:
	movq	192+_MM_Minus(%rip), %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1589:
	ret
LFE513:
.no_dead_strip _Minus__uMinus_param_L_C
_Minus__uMinus_param_L_C:
LFB514:
	pushq	%rbp
LCFI1590:
	movq	%rsp, %rbp
LCFI1591:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1592:
	ret
LFE514:
.no_dead_strip _Minus__Minus_param_L_C
_Minus__Minus_param_L_C:
LFB515:
	pushq	%rbp
LCFI1593:
	movq	%rsp, %rbp
LCFI1594:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1595:
	ret
LFE515:
.no_dead_strip _Minus__uMinus_var_L_u8
_Minus__uMinus_var_L_u8:
LFB516:
	pushq	%rbp
LCFI1596:
	movq	%rsp, %rbp
LCFI1597:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1598:
	ret
LFE516:
.no_dead_strip _Minus__Minus_var_L_u8
_Minus__Minus_var_L_u8:
LFB517:
	pushq	%rbp
LCFI1599:
	movq	%rsp, %rbp
LCFI1600:
	movq	192+_MM_Minus(%rip), %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1601:
	ret
LFE517:
.no_dead_strip _Minus__uMinus_param_L_u8
_Minus__uMinus_param_L_u8:
LFB518:
	pushq	%rbp
LCFI1602:
	movq	%rsp, %rbp
LCFI1603:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1604:
	ret
LFE518:
.no_dead_strip _Minus__Minus_param_L_u8
_Minus__Minus_param_L_u8:
LFB519:
	pushq	%rbp
LCFI1605:
	movq	%rsp, %rbp
LCFI1606:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1607:
	ret
LFE519:
.no_dead_strip _Minus__uMinus_var_L_i64
_Minus__uMinus_var_L_i64:
LFB520:
	pushq	%rbp
LCFI1608:
	movq	%rsp, %rbp
LCFI1609:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1610:
	ret
LFE520:
.no_dead_strip _Minus__Minus_var_L_i64
_Minus__Minus_var_L_i64:
LFB521:
	pushq	%rbp
LCFI1611:
	movq	%rsp, %rbp
LCFI1612:
	movq	192+_MM_Minus(%rip), %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1613:
	ret
LFE521:
.no_dead_strip _Minus__uMinus_param_L_i64
_Minus__uMinus_param_L_i64:
LFB522:
	pushq	%rbp
LCFI1614:
	movq	%rsp, %rbp
LCFI1615:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1616:
	ret
LFE522:
.no_dead_strip _Minus__Minus_param_L_i64
_Minus__Minus_param_L_i64:
LFB523:
	pushq	%rbp
LCFI1617:
	movq	%rsp, %rbp
LCFI1618:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1619:
	ret
LFE523:
.no_dead_strip _Minus__uMinus_var_L_L
_Minus__uMinus_var_L_L:
LFB524:
	pushq	%rbp
LCFI1620:
	movq	%rsp, %rbp
LCFI1621:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1622:
	ret
LFE524:
.no_dead_strip _Minus__Minus_var_L_L
_Minus__Minus_var_L_L:
LFB525:
	pushq	%rbp
LCFI1623:
	movq	%rsp, %rbp
LCFI1624:
	movq	192+_MM_Minus(%rip), %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1625:
	ret
LFE525:
.no_dead_strip _Minus__uMinus_param_L_L
_Minus__uMinus_param_L_L:
LFB526:
	pushq	%rbp
LCFI1626:
	movq	%rsp, %rbp
LCFI1627:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1628:
	ret
LFE526:
.no_dead_strip _Minus__Minus_param_L_L
_Minus__Minus_param_L_L:
LFB527:
	pushq	%rbp
LCFI1629:
	movq	%rsp, %rbp
LCFI1630:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1631:
	ret
LFE527:
.no_dead_strip _Minus__uMinus_var_L_i8
_Minus__uMinus_var_L_i8:
LFB528:
	pushq	%rbp
LCFI1632:
	movq	%rsp, %rbp
LCFI1633:
	movq	192+_MM_Minus(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1634:
	ret
LFE528:
.no_dead_strip _Minus__Minus_var_L_i8
_Minus__Minus_var_L_i8:
LFB529:
	pushq	%rbp
LCFI1635:
	movq	%rsp, %rbp
LCFI1636:
	movq	192+_MM_Minus(%rip), %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1637:
	ret
LFE529:
.no_dead_strip _Minus__uMinus_param_L_i8
_Minus__uMinus_param_L_i8:
LFB530:
	pushq	%rbp
LCFI1638:
	movq	%rsp, %rbp
LCFI1639:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1640:
	ret
LFE530:
.no_dead_strip _Minus__Minus_param_L_i8
_Minus__Minus_param_L_i8:
LFB531:
	pushq	%rbp
LCFI1641:
	movq	%rsp, %rbp
LCFI1642:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	movq	-8(%rbp), %rdx
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1643:
	ret
LFE531:
.no_dead_strip _Minus__uMinus_var_i8_u16
_Minus__uMinus_var_i8_u16:
LFB532:
	pushq	%rbp
LCFI1644:
	movq	%rsp, %rbp
LCFI1645:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1646:
	ret
LFE532:
.no_dead_strip _Minus__Minus_var_i8_u16
_Minus__Minus_var_i8_u16:
LFB533:
	pushq	%rbp
LCFI1647:
	movq	%rsp, %rbp
LCFI1648:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L675
	movl	$18785, %edi
	call	__m3_fault
L675:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	104+_MM_Minus(%rip), %eax
	movzwl	%ax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1649:
	ret
LFE533:
.no_dead_strip _Minus__uMinus_param_i8_u16
_Minus__uMinus_param_i8_u16:
LFB534:
	pushq	%rbp
LCFI1650:
	movq	%rsp, %rbp
LCFI1651:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1652:
	ret
LFE534:
.no_dead_strip _Minus__Minus_param_i8_u16
_Minus__Minus_param_i8_u16:
LFB535:
	pushq	%rbp
LCFI1653:
	movq	%rsp, %rbp
LCFI1654:
	subq	$16, %rsp
LCFI1655:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L678
	movl	$18849, %edi
	call	__m3_fault
L678:
	movsbq	-1(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1656:
	ret
LFE535:
.no_dead_strip _Minus__uMinus_var_i8_u32
_Minus__uMinus_var_i8_u32:
LFB536:
	pushq	%rbp
LCFI1657:
	movq	%rsp, %rbp
LCFI1658:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1659:
	ret
LFE536:
.no_dead_strip _Minus__Minus_var_i8_u32
_Minus__Minus_var_i8_u32:
LFB537:
	pushq	%rbp
LCFI1660:
	movq	%rsp, %rbp
LCFI1661:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L681
	movl	$18913, %edi
	call	__m3_fault
L681:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	108+_MM_Minus(%rip), %eax
	mov	%eax, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1662:
	ret
LFE537:
.no_dead_strip _Minus__uMinus_param_i8_u32
_Minus__uMinus_param_i8_u32:
LFB538:
	pushq	%rbp
LCFI1663:
	movq	%rsp, %rbp
LCFI1664:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1665:
	ret
LFE538:
.no_dead_strip _Minus__Minus_param_i8_u32
_Minus__Minus_param_i8_u32:
LFB539:
	pushq	%rbp
LCFI1666:
	movq	%rsp, %rbp
LCFI1667:
	subq	$16, %rsp
LCFI1668:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L684
	movl	$18977, %edi
	call	__m3_fault
L684:
	movsbq	-1(%rbp), %rdx
	mov	-8(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1669:
	ret
LFE539:
.no_dead_strip _Minus__uMinus_var_i8_LC
_Minus__uMinus_var_i8_LC:
LFB540:
	pushq	%rbp
LCFI1670:
	movq	%rsp, %rbp
LCFI1671:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1672:
	ret
LFE540:
.no_dead_strip _Minus__Minus_var_i8_LC
_Minus__Minus_var_i8_LC:
LFB541:
	pushq	%rbp
LCFI1673:
	movq	%rsp, %rbp
LCFI1674:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	112+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1675:
	ret
LFE541:
.no_dead_strip _Minus__uMinus_param_i8_LC
_Minus__uMinus_param_i8_LC:
LFB542:
	pushq	%rbp
LCFI1676:
	movq	%rsp, %rbp
LCFI1677:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1678:
	ret
LFE542:
.no_dead_strip _Minus__Minus_param_i8_LC
_Minus__Minus_param_i8_LC:
LFB543:
	pushq	%rbp
LCFI1679:
	movq	%rsp, %rbp
LCFI1680:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1681:
	ret
LFE543:
.no_dead_strip _Minus__uMinus_var_i8_I
_Minus__uMinus_var_i8_I:
LFB544:
	pushq	%rbp
LCFI1682:
	movq	%rsp, %rbp
LCFI1683:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1684:
	ret
LFE544:
.no_dead_strip _Minus__Minus_var_i8_I
_Minus__Minus_var_i8_I:
LFB545:
	pushq	%rbp
LCFI1685:
	movq	%rsp, %rbp
LCFI1686:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L691
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L692
L691:
	movl	$19169, %edi
	call	__m3_fault
L692:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1687:
	ret
LFE545:
.no_dead_strip _Minus__uMinus_param_i8_I
_Minus__uMinus_param_i8_I:
LFB546:
	pushq	%rbp
LCFI1688:
	movq	%rsp, %rbp
LCFI1689:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1690:
	ret
LFE546:
.no_dead_strip _Minus__Minus_param_i8_I
_Minus__Minus_param_i8_I:
LFB547:
	pushq	%rbp
LCFI1691:
	movq	%rsp, %rbp
LCFI1692:
	subq	$16, %rsp
LCFI1693:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	cmpq	$-128, %rax
	jl	L695
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	cmpq	$127, %rax
	jle	L696
L695:
	movl	$19233, %edi
	call	__m3_fault
L696:
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1694:
	ret
LFE547:
.no_dead_strip _Minus__uMinus_var_i8_i16
_Minus__uMinus_var_i8_i16:
LFB548:
	pushq	%rbp
LCFI1695:
	movq	%rsp, %rbp
LCFI1696:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1697:
	ret
LFE548:
.no_dead_strip _Minus__Minus_var_i8_i16
_Minus__Minus_var_i8_i16:
LFB549:
	pushq	%rbp
LCFI1698:
	movq	%rsp, %rbp
LCFI1699:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L699
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L700
L699:
	movl	$19297, %edi
	call	__m3_fault
L700:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzwl	128+_MM_Minus(%rip), %eax
	movswq	%ax, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1700:
	ret
LFE549:
.no_dead_strip _Minus__uMinus_param_i8_i16
_Minus__uMinus_param_i8_i16:
LFB550:
	pushq	%rbp
LCFI1701:
	movq	%rsp, %rbp
LCFI1702:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1703:
	ret
LFE550:
.no_dead_strip _Minus__Minus_param_i8_i16
_Minus__Minus_param_i8_i16:
LFB551:
	pushq	%rbp
LCFI1704:
	movq	%rsp, %rbp
LCFI1705:
	subq	$16, %rsp
LCFI1706:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L703
	movsbq	-1(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L704
L703:
	movl	$19361, %edi
	call	__m3_fault
L704:
	movsbq	-1(%rbp), %rdx
	movswq	-4(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1707:
	ret
LFE551:
.no_dead_strip _Minus__uMinus_var_i8_u64
_Minus__uMinus_var_i8_u64:
LFB552:
	pushq	%rbp
LCFI1708:
	movq	%rsp, %rbp
LCFI1709:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1710:
	ret
LFE552:
.no_dead_strip _Minus__Minus_var_i8_u64
_Minus__Minus_var_i8_u64:
LFB553:
	pushq	%rbp
LCFI1711:
	movq	%rsp, %rbp
LCFI1712:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	136+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1713:
	ret
LFE553:
.no_dead_strip _Minus__uMinus_param_i8_u64
_Minus__uMinus_param_i8_u64:
LFB554:
	pushq	%rbp
LCFI1714:
	movq	%rsp, %rbp
LCFI1715:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1716:
	ret
LFE554:
.no_dead_strip _Minus__Minus_param_i8_u64
_Minus__Minus_param_i8_u64:
LFB555:
	pushq	%rbp
LCFI1717:
	movq	%rsp, %rbp
LCFI1718:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1719:
	ret
LFE555:
.no_dead_strip _Minus__uMinus_var_i8_i32
_Minus__uMinus_var_i8_i32:
LFB556:
	pushq	%rbp
LCFI1720:
	movq	%rsp, %rbp
LCFI1721:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1722:
	ret
LFE556:
.no_dead_strip _Minus__Minus_var_i8_i32
_Minus__Minus_var_i8_i32:
LFB557:
	pushq	%rbp
LCFI1723:
	movq	%rsp, %rbp
LCFI1724:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L711
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L712
L711:
	movl	$19553, %edi
	call	__m3_fault
L712:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movl	152+_MM_Minus(%rip), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1725:
	ret
LFE557:
.no_dead_strip _Minus__uMinus_param_i8_i32
_Minus__uMinus_param_i8_i32:
LFB558:
	pushq	%rbp
LCFI1726:
	movq	%rsp, %rbp
LCFI1727:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1728:
	ret
LFE558:
.no_dead_strip _Minus__Minus_param_i8_i32
_Minus__Minus_param_i8_i32:
LFB559:
	pushq	%rbp
LCFI1729:
	movq	%rsp, %rbp
LCFI1730:
	subq	$16, %rsp
LCFI1731:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L715
	movsbq	-1(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L716
L715:
	movl	$19617, %edi
	call	__m3_fault
L716:
	movsbq	-1(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1732:
	ret
LFE559:
.no_dead_strip _Minus__uMinus_var_i8_C
_Minus__uMinus_var_i8_C:
LFB560:
	pushq	%rbp
LCFI1733:
	movq	%rsp, %rbp
LCFI1734:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1735:
	ret
LFE560:
.no_dead_strip _Minus__Minus_var_i8_C
_Minus__Minus_var_i8_C:
LFB561:
	pushq	%rbp
LCFI1736:
	movq	%rsp, %rbp
LCFI1737:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L719
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L720
L719:
	movl	$19681, %edi
	call	__m3_fault
L720:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1738:
	ret
LFE561:
.no_dead_strip _Minus__uMinus_param_i8_C
_Minus__uMinus_param_i8_C:
LFB562:
	pushq	%rbp
LCFI1739:
	movq	%rsp, %rbp
LCFI1740:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1741:
	ret
LFE562:
.no_dead_strip _Minus__Minus_param_i8_C
_Minus__Minus_param_i8_C:
LFB563:
	pushq	%rbp
LCFI1742:
	movq	%rsp, %rbp
LCFI1743:
	subq	$16, %rsp
LCFI1744:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L723
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L724
L723:
	movl	$19745, %edi
	call	__m3_fault
L724:
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1745:
	ret
LFE563:
.no_dead_strip _Minus__uMinus_var_i8_u8
_Minus__uMinus_var_i8_u8:
LFB564:
	pushq	%rbp
LCFI1746:
	movq	%rsp, %rbp
LCFI1747:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1748:
	ret
LFE564:
.no_dead_strip _Minus__Minus_var_i8_u8
_Minus__Minus_var_i8_u8:
LFB565:
	pushq	%rbp
LCFI1749:
	movq	%rsp, %rbp
LCFI1750:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L727
	movl	$19809, %edi
	call	__m3_fault
L727:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	168+_MM_Minus(%rip), %eax
	movzbl	%al, %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1751:
	ret
LFE565:
.no_dead_strip _Minus__uMinus_param_i8_u8
_Minus__uMinus_param_i8_u8:
LFB566:
	pushq	%rbp
LCFI1752:
	movq	%rsp, %rbp
LCFI1753:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1754:
	ret
LFE566:
.no_dead_strip _Minus__Minus_param_i8_u8
_Minus__Minus_param_i8_u8:
LFB567:
	pushq	%rbp
LCFI1755:
	movq	%rsp, %rbp
LCFI1756:
	subq	$16, %rsp
LCFI1757:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jge	L730
	movl	$19873, %edi
	call	__m3_fault
L730:
	movsbq	-1(%rbp), %rdx
	movzbl	-2(%rbp), %eax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1758:
	ret
LFE567:
.no_dead_strip _Minus__uMinus_var_i8_i64
_Minus__uMinus_var_i8_i64:
LFB568:
	pushq	%rbp
LCFI1759:
	movq	%rsp, %rbp
LCFI1760:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1761:
	ret
LFE568:
.no_dead_strip _Minus__Minus_var_i8_i64
_Minus__Minus_var_i8_i64:
LFB569:
	pushq	%rbp
LCFI1762:
	movq	%rsp, %rbp
LCFI1763:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	176+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1764:
	ret
LFE569:
.no_dead_strip _Minus__uMinus_param_i8_i64
_Minus__uMinus_param_i8_i64:
LFB570:
	pushq	%rbp
LCFI1765:
	movq	%rsp, %rbp
LCFI1766:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1767:
	ret
LFE570:
.no_dead_strip _Minus__Minus_param_i8_i64
_Minus__Minus_param_i8_i64:
LFB571:
	pushq	%rbp
LCFI1768:
	movq	%rsp, %rbp
LCFI1769:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1770:
	ret
LFE571:
.no_dead_strip _Minus__uMinus_var_i8_L
_Minus__uMinus_var_i8_L:
LFB572:
	pushq	%rbp
LCFI1771:
	movq	%rsp, %rbp
LCFI1772:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1773:
	ret
LFE572:
.no_dead_strip _Minus__Minus_var_i8_L
_Minus__Minus_var_i8_L:
LFB573:
	pushq	%rbp
LCFI1774:
	movq	%rsp, %rbp
LCFI1775:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movq	192+_MM_Minus(%rip), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1776:
	ret
LFE573:
.no_dead_strip _Minus__uMinus_param_i8_L
_Minus__uMinus_param_i8_L:
LFB574:
	pushq	%rbp
LCFI1777:
	movq	%rsp, %rbp
LCFI1778:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1779:
	ret
LFE574:
.no_dead_strip _Minus__Minus_param_i8_L
_Minus__Minus_param_i8_L:
LFB575:
	pushq	%rbp
LCFI1780:
	movq	%rsp, %rbp
LCFI1781:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	subq	-16(%rbp), %rax
	leave
LCFI1782:
	ret
LFE575:
.no_dead_strip _Minus__uMinus_var_i8_i8
_Minus__uMinus_var_i8_i8:
LFB576:
	pushq	%rbp
LCFI1783:
	movq	%rsp, %rbp
LCFI1784:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1785:
	ret
LFE576:
.no_dead_strip _Minus__Minus_var_i8_i8
_Minus__Minus_var_i8_i8:
LFB577:
	pushq	%rbp
LCFI1786:
	movq	%rsp, %rbp
LCFI1787:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L741
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L742
L741:
	movl	$20193, %edi
	call	__m3_fault
L742:
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rdx
	movzbl	200+_MM_Minus(%rip), %eax
	movsbq	%al, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1788:
	ret
LFE577:
.no_dead_strip _Minus__uMinus_param_i8_i8
_Minus__uMinus_param_i8_i8:
LFB578:
	pushq	%rbp
LCFI1789:
	movq	%rsp, %rbp
LCFI1790:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1791:
	ret
LFE578:
.no_dead_strip _Minus__Minus_param_i8_i8
_Minus__Minus_param_i8_i8:
LFB579:
	pushq	%rbp
LCFI1792:
	movq	%rsp, %rbp
LCFI1793:
	subq	$16, %rsp
LCFI1794:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$-128, %rax
	jl	L745
	movsbq	-1(%rbp), %rdx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	cmpq	$127, %rax
	jle	L746
L745:
	movl	$20257, %edi
	call	__m3_fault
L746:
	movsbq	-1(%rbp), %rdx
	movsbq	-2(%rbp), %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	leave
LCFI1795:
	ret
LFE579:
.globl _Minus_M3
.no_dead_strip _Minus_M3
_Minus_M3:
LFB580:
	pushq	%rbp
LCFI1796:
	movq	%rsp, %rbp
LCFI1797:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Minus(%rip), %rax
	leave
LCFI1798:
	ret
LFE580:
__m3_fault:
LFB581:
	pushq	%rbp
LCFI1799:
	movq	%rsp, %rbp
LCFI1800:
	subq	$16, %rsp
LCFI1801:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_Minus(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI1802:
	ret
LFE581:
	.const_data
	.align 5
_L_1:
	.ascii "Minus_M3"
	.space 1
	.ascii "Minus_param_i8_i8"
	.space 1
	.ascii "uMinus_param_i8_i8"
	.space 1
	.ascii "Minus_var_i8_i8"
	.space 1
	.ascii "uMinus_var_i8_i8"
	.space 1
	.ascii "Minus_param_i8_L"
	.space 1
	.ascii "uMinus_param_i8_L"
	.space 1
	.ascii "Minus_var_i8_L"
	.space 1
	.ascii "uMinus_var_i8_L"
	.space 1
	.ascii "Minus_param_i8_i64"
	.space 1
	.ascii "uMinus_param_i8_i64"
	.space 1
	.ascii "Minus_var_i8_i64"
	.space 1
	.ascii "uMinus_var_i8_i64"
	.space 1
	.ascii "Minus_param_i8_u8"
	.space 1
	.ascii "uMinus_param_i8_u8"
	.space 1
	.ascii "Minus_var_i8_u8"
	.space 1
	.ascii "uMinus_var_i8_u8"
	.space 1
	.ascii "Minus_param_i8_C"
	.space 1
	.ascii "uMinus_param_i8_C"
	.space 1
	.ascii "Minus_var_i8_C"
	.space 1
	.ascii "uMinus_var_i8_C"
	.space 1
	.ascii "Minus_param_i8_i32"
	.space 1
	.ascii "uMinus_param_i8_i32"
	.space 1
	.ascii "Minus_var_i8_i32"
	.space 1
	.ascii "uMinus_var_i8_i32"
	.space 1
	.ascii "Minus_param_i8_u64"
	.space 1
	.ascii "uMinus_param_i8_u64"
	.space 1
	.ascii "Minus_var_i8_u64"
	.space 1
	.ascii "uMinus_var_i8_u64"
	.space 1
	.ascii "Minus_param_i8_i16"
	.space 1
	.ascii "uMinus_param_i8_i16"
	.space 1
	.ascii "Minus_var_i8_i16"
	.space 1
	.ascii "uMinus_var_i8_i16"
	.space 1
	.ascii "Minus_param_i8_I"
	.space 1
	.ascii "uMinus_param_i8_I"
	.space 1
	.ascii "Minus_var_i8_I"
	.space 1
	.ascii "uMinus_var_i8_I"
	.space 1
	.ascii "Minus_param_i8_LC"
	.space 1
	.ascii "uMinus_param_i8_LC"
	.space 1
	.ascii "Minus_var_i8_LC"
	.space 1
	.ascii "uMinus_var_i8_LC"
	.space 1
	.ascii "Minus_param_i8_u32"
	.space 1
	.ascii "uMinus_param_i8_u32"
	.space 1
	.ascii "Minus_var_i8_u32"
	.space 1
	.ascii "uMinus_var_i8_u32"
	.space 1
	.ascii "Minus_param_i8_u16"
	.space 1
	.ascii "uMinus_param_i8_u16"
	.space 1
	.ascii "Minus_var_i8_u16"
	.space 1
	.ascii "uMinus_var_i8_u16"
	.space 1
	.ascii "Minus_param_L_i8"
	.space 1
	.ascii "uMinus_param_L_i8"
	.space 1
	.ascii "Minus_var_L_i8"
	.space 1
	.ascii "uMinus_var_L_i8"
	.space 1
	.ascii "Minus_param_L_L"
	.space 1
	.ascii "uMinus_param_L_L"
	.space 1
	.ascii "Minus_var_L_L"
	.space 1
	.ascii "uMinus_var_L_L"
	.space 1
	.ascii "Minus_param_L_i64"
	.space 1
	.ascii "uMinus_param_L_i64"
	.space 1
	.ascii "Minus_var_L_i64"
	.space 1
	.ascii "uMinus_var_L_i64"
	.space 1
	.ascii "Minus_param_L_u8"
	.space 1
	.ascii "uMinus_param_L_u8"
	.space 1
	.ascii "Minus_var_L_u8"
	.space 1
	.ascii "uMinus_var_L_u8"
	.space 1
	.ascii "Minus_param_L_C"
	.space 1
	.ascii "uMinus_param_L_C"
	.space 1
	.ascii "Minus_var_L_C"
	.space 1
	.ascii "uMinus_var_L_C"
	.space 1
	.ascii "Minus_param_L_i32"
	.space 1
	.ascii "uMinus_param_L_i32"
	.space 1
	.ascii "Minus_var_L_i32"
	.space 1
	.ascii "uMinus_var_L_i32"
	.space 1
	.ascii "Minus_param_L_u64"
	.space 1
	.ascii "uMinus_param_L_u64"
	.space 1
	.ascii "Minus_var_L_u64"
	.space 1
	.ascii "uMinus_var_L_u64"
	.space 1
	.ascii "Minus_param_L_i16"
	.space 1
	.ascii "uMinus_param_L_i16"
	.space 1
	.ascii "Minus_var_L_i16"
	.space 1
	.ascii "uMinus_var_L_i16"
	.space 1
	.ascii "Minus_param_L_I"
	.space 1
	.ascii "uMinus_param_L_I"
	.space 1
	.ascii "Minus_var_L_I"
	.space 1
	.ascii "uMinus_var_L_I"
	.space 1
	.ascii "Minus_param_L_LC"
	.space 1
	.ascii "uMinus_param_L_LC"
	.space 1
	.ascii "Minus_var_L_LC"
	.space 1
	.ascii "uMinus_var_L_LC"
	.space 1
	.ascii "Minus_param_L_u32"
	.space 1
	.ascii "uMinus_param_L_u32"
	.space 1
	.ascii "Minus_var_L_u32"
	.space 1
	.ascii "uMinus_var_L_u32"
	.space 1
	.ascii "Minus_param_L_u16"
	.space 1
	.ascii "uMinus_param_L_u16"
	.space 1
	.ascii "Minus_var_L_u16"
	.space 1
	.ascii "uMinus_var_L_u16"
	.space 1
	.ascii "Minus_param_f32_f32"
	.space 1
	.ascii "Minus_var_f32_f32"
	.space 1
	.ascii "Minus_param_i64_i8"
	.space 1
	.ascii "uMinus_param_i64_i8"
	.space 1
	.ascii "Minus_var_i64_i8"
	.space 1
	.ascii "uMinus_var_i64_i8"
	.space 1
	.ascii "Minus_param_i64_L"
	.space 1
	.ascii "uMinus_param_i64_L"
	.space 1
	.ascii "Minus_var_i64_L"
	.space 1
	.ascii "uMinus_var_i64_L"
	.space 1
	.ascii "Minus_param_i64_i64"
	.space 1
	.ascii "uMinus_param_i64_i64"
	.space 1
	.ascii "Minus_var_i64_i64"
	.space 1
	.ascii "uMinus_var_i64_i64"
	.space 1
	.ascii "Minus_param_i64_u8"
	.space 1
	.ascii "uMinus_param_i64_u8"
	.space 1
	.ascii "Minus_var_i64_u8"
	.space 1
	.ascii "uMinus_var_i64_u8"
	.space 1
	.ascii "Minus_param_i64_C"
	.space 1
	.ascii "uMinus_param_i64_C"
	.space 1
	.ascii "Minus_var_i64_C"
	.space 1
	.ascii "uMinus_var_i64_C"
	.space 1
	.ascii "Minus_param_i64_i32"
	.space 1
	.ascii "uMinus_param_i64_i32"
	.space 1
	.ascii "Minus_var_i64_i32"
	.space 1
	.ascii "uMinus_var_i64_i32"
	.space 1
	.ascii "Minus_param_i64_u64"
	.space 1
	.ascii "uMinus_param_i64_u64"
	.space 1
	.ascii "Minus_var_i64_u64"
	.space 1
	.ascii "uMinus_var_i64_u64"
	.space 1
	.ascii "Minus_param_i64_i16"
	.space 1
	.ascii "uMinus_param_i64_i16"
	.space 1
	.ascii "Minus_var_i64_i16"
	.space 1
	.ascii "uMinus_var_i64_i16"
	.space 1
	.ascii "Minus_param_i64_I"
	.space 1
	.ascii "uMinus_param_i64_I"
	.space 1
	.ascii "Minus_var_i64_I"
	.space 1
	.ascii "uMinus_var_i64_I"
	.space 1
	.ascii "Minus_param_i64_LC"
	.space 1
	.ascii "uMinus_param_i64_LC"
	.space 1
	.ascii "Minus_var_i64_LC"
	.space 1
	.ascii "uMinus_var_i64_LC"
	.space 1
	.ascii "Minus_param_i64_u32"
	.space 1
	.ascii "uMinus_param_i64_u32"
	.space 1
	.ascii "Minus_var_i64_u32"
	.space 1
	.ascii "uMinus_var_i64_u32"
	.space 1
	.ascii "Minus_param_i64_u16"
	.space 1
	.ascii "uMinus_param_i64_u16"
	.space 1
	.ascii "Minus_var_i64_u16"
	.space 1
	.ascii "uMinus_var_i64_u16"
	.space 1
	.ascii "Minus_param_u8_i8"
	.space 1
	.ascii "uMinus_param_u8_i8"
	.space 1
	.ascii "Minus_var_u8_i8"
	.space 1
	.ascii "uMinus_var_u8_i8"
	.space 1
	.ascii "Minus_param_u8_L"
	.space 1
	.ascii "uMinus_param_u8_L"
	.space 1
	.ascii "Minus_var_u8_L"
	.space 1
	.ascii "uMinus_var_u8_L"
	.space 1
	.ascii "Minus_param_u8_i64"
	.space 1
	.ascii "uMinus_param_u8_i64"
	.space 1
	.ascii "Minus_var_u8_i64"
	.space 1
	.ascii "uMinus_var_u8_i64"
	.space 1
	.ascii "Minus_param_u8_u8"
	.space 1
	.ascii "uMinus_param_u8_u8"
	.space 1
	.ascii "Minus_var_u8_u8"
	.space 1
	.ascii "uMinus_var_u8_u8"
	.space 1
	.ascii "Minus_param_u8_C"
	.space 1
	.ascii "uMinus_param_u8_C"
	.space 1
	.ascii "Minus_var_u8_C"
	.space 1
	.ascii "uMinus_var_u8_C"
	.space 1
	.ascii "Minus_param_u8_i32"
	.space 1
	.ascii "uMinus_param_u8_i32"
	.space 1
	.ascii "Minus_var_u8_i32"
	.space 1
	.ascii "uMinus_var_u8_i32"
	.space 1
	.ascii "Minus_param_u8_u64"
	.space 1
	.ascii "uMinus_param_u8_u64"
	.space 1
	.ascii "Minus_var_u8_u64"
	.space 1
	.ascii "uMinus_var_u8_u64"
	.space 1
	.ascii "Minus_param_u8_i16"
	.space 1
	.ascii "uMinus_param_u8_i16"
	.space 1
	.ascii "Minus_var_u8_i16"
	.space 1
	.ascii "uMinus_var_u8_i16"
	.space 1
	.ascii "Minus_param_u8_I"
	.space 1
	.ascii "uMinus_param_u8_I"
	.space 1
	.ascii "Minus_var_u8_I"
	.space 1
	.ascii "uMinus_var_u8_I"
	.space 1
	.ascii "Minus_param_u8_LC"
	.space 1
	.ascii "uMinus_param_u8_LC"
	.space 1
	.ascii "Minus_var_u8_LC"
	.space 1
	.ascii "uMinus_var_u8_LC"
	.space 1
	.ascii "Minus_param_u8_u32"
	.space 1
	.ascii "uMinus_param_u8_u32"
	.space 1
	.ascii "Minus_var_u8_u32"
	.space 1
	.ascii "uMinus_var_u8_u32"
	.space 1
	.ascii "Minus_param_u8_u16"
	.space 1
	.ascii "uMinus_param_u8_u16"
	.space 1
	.ascii "Minus_var_u8_u16"
	.space 1
	.ascii "uMinus_var_u8_u16"
	.space 1
	.ascii "Minus_param_C_i8"
	.space 1
	.ascii "uMinus_param_C_i8"
	.space 1
	.ascii "Minus_var_C_i8"
	.space 1
	.ascii "uMinus_var_C_i8"
	.space 1
	.ascii "Minus_param_C_L"
	.space 1
	.ascii "uMinus_param_C_L"
	.space 1
	.ascii "Minus_var_C_L"
	.space 1
	.ascii "uMinus_var_C_L"
	.space 1
	.ascii "Minus_param_C_i64"
	.space 1
	.ascii "uMinus_param_C_i64"
	.space 1
	.ascii "Minus_var_C_i64"
	.space 1
	.ascii "uMinus_var_C_i64"
	.space 1
	.ascii "Minus_param_C_u8"
	.space 1
	.ascii "uMinus_param_C_u8"
	.space 1
	.ascii "Minus_var_C_u8"
	.space 1
	.ascii "uMinus_var_C_u8"
	.space 1
	.ascii "Minus_param_C_C"
	.space 1
	.ascii "uMinus_param_C_C"
	.space 1
	.ascii "Minus_var_C_C"
	.space 1
	.ascii "uMinus_var_C_C"
	.space 1
	.ascii "Minus_param_C_i32"
	.space 1
	.ascii "uMinus_param_C_i32"
	.space 1
	.ascii "Minus_var_C_i32"
	.space 1
	.ascii "uMinus_var_C_i32"
	.space 1
	.ascii "Minus_param_C_u64"
	.space 1
	.ascii "uMinus_param_C_u64"
	.space 1
	.ascii "Minus_var_C_u64"
	.space 1
	.ascii "uMinus_var_C_u64"
	.space 1
	.ascii "Minus_param_C_i16"
	.space 1
	.ascii "uMinus_param_C_i16"
	.space 1
	.ascii "Minus_var_C_i16"
	.space 1
	.ascii "uMinus_var_C_i16"
	.space 1
	.ascii "Minus_param_C_I"
	.space 1
	.ascii "uMinus_param_C_I"
	.space 1
	.ascii "Minus_var_C_I"
	.space 1
	.ascii "uMinus_var_C_I"
	.space 1
	.ascii "Minus_param_C_LC"
	.space 1
	.ascii "uMinus_param_C_LC"
	.space 1
	.ascii "Minus_var_C_LC"
	.space 1
	.ascii "uMinus_var_C_LC"
	.space 1
	.ascii "Minus_param_C_u32"
	.space 1
	.ascii "uMinus_param_C_u32"
	.space 1
	.ascii "Minus_var_C_u32"
	.space 1
	.ascii "uMinus_var_C_u32"
	.space 1
	.ascii "Minus_param_C_u16"
	.space 1
	.ascii "uMinus_param_C_u16"
	.space 1
	.ascii "Minus_var_C_u16"
	.space 1
	.ascii "uMinus_var_C_u16"
	.space 1
	.ascii "Minus_param_i32_i8"
	.space 1
	.ascii "uMinus_param_i32_i8"
	.space 1
	.ascii "Minus_var_i32_i8"
	.space 1
	.ascii "uMinus_var_i32_i8"
	.space 1
	.ascii "Minus_param_i32_L"
	.space 1
	.ascii "uMinus_param_i32_L"
	.space 1
	.ascii "Minus_var_i32_L"
	.space 1
	.ascii "uMinus_var_i32_L"
	.space 1
	.ascii "Minus_param_i32_i64"
	.space 1
	.ascii "uMinus_param_i32_i64"
	.space 1
	.ascii "Minus_var_i32_i64"
	.space 1
	.ascii "uMinus_var_i32_i64"
	.space 1
	.ascii "Minus_param_i32_u8"
	.space 1
	.ascii "uMinus_param_i32_u8"
	.space 1
	.ascii "Minus_var_i32_u8"
	.space 1
	.ascii "uMinus_var_i32_u8"
	.space 1
	.ascii "Minus_param_i32_C"
	.space 1
	.ascii "uMinus_param_i32_C"
	.space 1
	.ascii "Minus_var_i32_C"
	.space 1
	.ascii "uMinus_var_i32_C"
	.space 1
	.ascii "Minus_param_i32_i32"
	.space 1
	.ascii "uMinus_param_i32_i32"
	.space 1
	.ascii "Minus_var_i32_i32"
	.space 1
	.ascii "uMinus_var_i32_i32"
	.space 1
	.ascii "Minus_param_i32_u64"
	.space 1
	.ascii "uMinus_param_i32_u64"
	.space 1
	.ascii "Minus_var_i32_u64"
	.space 1
	.ascii "uMinus_var_i32_u64"
	.space 1
	.ascii "Minus_param_i32_i16"
	.space 1
	.ascii "uMinus_param_i32_i16"
	.space 1
	.ascii "Minus_var_i32_i16"
	.space 1
	.ascii "uMinus_var_i32_i16"
	.space 1
	.ascii "Minus_param_i32_I"
	.space 1
	.ascii "uMinus_param_i32_I"
	.space 1
	.ascii "Minus_var_i32_I"
	.space 1
	.ascii "uMinus_var_i32_I"
	.space 1
	.ascii "Minus_param_i32_LC"
	.space 1
	.ascii "uMinus_param_i32_LC"
	.space 1
	.ascii "Minus_var_i32_LC"
	.space 1
	.ascii "uMinus_var_i32_LC"
	.space 1
	.ascii "Minus_param_i32_u32"
	.space 1
	.ascii "uMinus_param_i32_u32"
	.space 1
	.ascii "Minus_var_i32_u32"
	.space 1
	.ascii "uMinus_var_i32_u32"
	.space 1
	.ascii "Minus_param_i32_u16"
	.space 1
	.ascii "uMinus_param_i32_u16"
	.space 1
	.ascii "Minus_var_i32_u16"
	.space 1
	.ascii "uMinus_var_i32_u16"
	.space 1
	.ascii "Minus_param_f64_f64"
	.space 1
	.ascii "Minus_var_f64_f64"
	.space 1
	.ascii "Minus_param_u64_i8"
	.space 1
	.ascii "uMinus_param_u64_i8"
	.space 1
	.ascii "Minus_var_u64_i8"
	.space 1
	.ascii "uMinus_var_u64_i8"
	.space 1
	.ascii "Minus_param_u64_L"
	.space 1
	.ascii "uMinus_param_u64_L"
	.space 1
	.ascii "Minus_var_u64_L"
	.space 1
	.ascii "uMinus_var_u64_L"
	.space 1
	.ascii "Minus_param_u64_i64"
	.space 1
	.ascii "uMinus_param_u64_i64"
	.space 1
	.ascii "Minus_var_u64_i64"
	.space 1
	.ascii "uMinus_var_u64_i64"
	.space 1
	.ascii "Minus_param_u64_u8"
	.space 1
	.ascii "uMinus_param_u64_u8"
	.space 1
	.ascii "Minus_var_u64_u8"
	.space 1
	.ascii "uMinus_var_u64_u8"
	.space 1
	.ascii "Minus_param_u64_C"
	.space 1
	.ascii "uMinus_param_u64_C"
	.space 1
	.ascii "Minus_var_u64_C"
	.space 1
	.ascii "uMinus_var_u64_C"
	.space 1
	.ascii "Minus_param_u64_i32"
	.space 1
	.ascii "uMinus_param_u64_i32"
	.space 1
	.ascii "Minus_var_u64_i32"
	.space 1
	.ascii "uMinus_var_u64_i32"
	.space 1
	.ascii "Minus_param_u64_u64"
	.space 1
	.ascii "uMinus_param_u64_u64"
	.space 1
	.ascii "Minus_var_u64_u64"
	.space 1
	.ascii "uMinus_var_u64_u64"
	.space 1
	.ascii "Minus_param_u64_i16"
	.space 1
	.ascii "uMinus_param_u64_i16"
	.space 1
	.ascii "Minus_var_u64_i16"
	.space 1
	.ascii "uMinus_var_u64_i16"
	.space 1
	.ascii "Minus_param_u64_I"
	.space 1
	.ascii "uMinus_param_u64_I"
	.space 1
	.ascii "Minus_var_u64_I"
	.space 1
	.ascii "uMinus_var_u64_I"
	.space 1
	.ascii "Minus_param_u64_LC"
	.space 1
	.ascii "uMinus_param_u64_LC"
	.space 1
	.ascii "Minus_var_u64_LC"
	.space 1
	.ascii "uMinus_var_u64_LC"
	.space 1
	.ascii "Minus_param_u64_u32"
	.space 1
	.ascii "uMinus_param_u64_u32"
	.space 1
	.ascii "Minus_var_u64_u32"
	.space 1
	.ascii "uMinus_var_u64_u32"
	.space 1
	.ascii "Minus_param_u64_u16"
	.space 1
	.ascii "uMinus_param_u64_u16"
	.space 1
	.ascii "Minus_var_u64_u16"
	.space 1
	.ascii "uMinus_var_u64_u16"
	.space 1
	.ascii "Minus_param_i16_i8"
	.space 1
	.ascii "uMinus_param_i16_i8"
	.space 1
	.ascii "Minus_var_i16_i8"
	.space 1
	.ascii "uMinus_var_i16_i8"
	.space 1
	.ascii "Minus_param_i16_L"
	.space 1
	.ascii "uMinus_param_i16_L"
	.space 1
	.ascii "Minus_var_i16_L"
	.space 1
	.ascii "uMinus_var_i16_L"
	.space 1
	.ascii "Minus_param_i16_i64"
	.space 1
	.ascii "uMinus_param_i16_i64"
	.space 1
	.ascii "Minus_var_i16_i64"
	.space 1
	.ascii "uMinus_var_i16_i64"
	.space 1
	.ascii "Minus_param_i16_u8"
	.space 1
	.ascii "uMinus_param_i16_u8"
	.space 1
	.ascii "Minus_var_i16_u8"
	.space 1
	.ascii "uMinus_var_i16_u8"
	.space 1
	.ascii "Minus_param_i16_C"
	.space 1
	.ascii "uMinus_param_i16_C"
	.space 1
	.ascii "Minus_var_i16_C"
	.space 1
	.ascii "uMinus_var_i16_C"
	.space 1
	.ascii "Minus_param_i16_i32"
	.space 1
	.ascii "uMinus_param_i16_i32"
	.space 1
	.ascii "Minus_var_i16_i32"
	.space 1
	.ascii "uMinus_var_i16_i32"
	.space 1
	.ascii "Minus_param_i16_u64"
	.space 1
	.ascii "uMinus_param_i16_u64"
	.space 1
	.ascii "Minus_var_i16_u64"
	.space 1
	.ascii "uMinus_var_i16_u64"
	.space 1
	.ascii "Minus_param_i16_i16"
	.space 1
	.ascii "uMinus_param_i16_i16"
	.space 1
	.ascii "Minus_var_i16_i16"
	.space 1
	.ascii "uMinus_var_i16_i16"
	.space 1
	.ascii "Minus_param_i16_I"
	.space 1
	.ascii "uMinus_param_i16_I"
	.space 1
	.ascii "Minus_var_i16_I"
	.space 1
	.ascii "uMinus_var_i16_I"
	.space 1
	.ascii "Minus_param_i16_LC"
	.space 1
	.ascii "uMinus_param_i16_LC"
	.space 1
	.ascii "Minus_var_i16_LC"
	.space 1
	.ascii "uMinus_var_i16_LC"
	.space 1
	.ascii "Minus_param_i16_u32"
	.space 1
	.ascii "uMinus_param_i16_u32"
	.space 1
	.ascii "Minus_var_i16_u32"
	.space 1
	.ascii "uMinus_var_i16_u32"
	.space 1
	.ascii "Minus_param_i16_u16"
	.space 1
	.ascii "uMinus_param_i16_u16"
	.space 1
	.ascii "Minus_var_i16_u16"
	.space 1
	.ascii "uMinus_var_i16_u16"
	.space 1
	.ascii "Minus_param_I_i8"
	.space 1
	.ascii "uMinus_param_I_i8"
	.space 1
	.ascii "Minus_var_I_i8"
	.space 1
	.ascii "uMinus_var_I_i8"
	.space 1
	.ascii "Minus_param_I_L"
	.space 1
	.ascii "uMinus_param_I_L"
	.space 1
	.ascii "Minus_var_I_L"
	.space 1
	.ascii "uMinus_var_I_L"
	.space 1
	.ascii "Minus_param_I_i64"
	.space 1
	.ascii "uMinus_param_I_i64"
	.space 1
	.ascii "Minus_var_I_i64"
	.space 1
	.ascii "uMinus_var_I_i64"
	.space 1
	.ascii "Minus_param_I_u8"
	.space 1
	.ascii "uMinus_param_I_u8"
	.space 1
	.ascii "Minus_var_I_u8"
	.space 1
	.ascii "uMinus_var_I_u8"
	.space 1
	.ascii "Minus_param_I_C"
	.space 1
	.ascii "uMinus_param_I_C"
	.space 1
	.ascii "Minus_var_I_C"
	.space 1
	.ascii "uMinus_var_I_C"
	.space 1
	.ascii "Minus_param_I_i32"
	.space 1
	.ascii "uMinus_param_I_i32"
	.space 1
	.ascii "Minus_var_I_i32"
	.space 1
	.ascii "uMinus_var_I_i32"
	.space 1
	.ascii "Minus_param_I_u64"
	.space 1
	.ascii "uMinus_param_I_u64"
	.space 1
	.ascii "Minus_var_I_u64"
	.space 1
	.ascii "uMinus_var_I_u64"
	.space 1
	.ascii "Minus_param_I_i16"
	.space 1
	.ascii "uMinus_param_I_i16"
	.space 1
	.ascii "Minus_var_I_i16"
	.space 1
	.ascii "uMinus_var_I_i16"
	.space 1
	.ascii "Minus_param_I_I"
	.space 1
	.ascii "uMinus_param_I_I"
	.space 1
	.ascii "Minus_var_I_I"
	.space 1
	.ascii "uMinus_var_I_I"
	.space 1
	.ascii "Minus_param_I_LC"
	.space 1
	.ascii "uMinus_param_I_LC"
	.space 1
	.ascii "Minus_var_I_LC"
	.space 1
	.ascii "uMinus_var_I_LC"
	.space 1
	.ascii "Minus_param_I_u32"
	.space 1
	.ascii "uMinus_param_I_u32"
	.space 1
	.ascii "Minus_var_I_u32"
	.space 1
	.ascii "uMinus_var_I_u32"
	.space 1
	.ascii "Minus_param_I_u16"
	.space 1
	.ascii "uMinus_param_I_u16"
	.space 1
	.ascii "Minus_var_I_u16"
	.space 1
	.ascii "uMinus_var_I_u16"
	.space 1
	.ascii "Minus_param_LC_i8"
	.space 1
	.ascii "uMinus_param_LC_i8"
	.space 1
	.ascii "Minus_var_LC_i8"
	.space 1
	.ascii "uMinus_var_LC_i8"
	.space 1
	.ascii "Minus_param_LC_L"
	.space 1
	.ascii "uMinus_param_LC_L"
	.space 1
	.ascii "Minus_var_LC_L"
	.space 1
	.ascii "uMinus_var_LC_L"
	.space 1
	.ascii "Minus_param_LC_i64"
	.space 1
	.ascii "uMinus_param_LC_i64"
	.space 1
	.ascii "Minus_var_LC_i64"
	.space 1
	.ascii "uMinus_var_LC_i64"
	.space 1
	.ascii "Minus_param_LC_u8"
	.space 1
	.ascii "uMinus_param_LC_u8"
	.space 1
	.ascii "Minus_var_LC_u8"
	.space 1
	.ascii "uMinus_var_LC_u8"
	.space 1
	.ascii "Minus_param_LC_C"
	.space 1
	.ascii "uMinus_param_LC_C"
	.space 1
	.ascii "Minus_var_LC_C"
	.space 1
	.ascii "uMinus_var_LC_C"
	.space 1
	.ascii "Minus_param_LC_i32"
	.space 1
	.ascii "uMinus_param_LC_i32"
	.space 1
	.ascii "Minus_var_LC_i32"
	.space 1
	.ascii "uMinus_var_LC_i32"
	.space 1
	.ascii "Minus_param_LC_u64"
	.space 1
	.ascii "uMinus_param_LC_u64"
	.space 1
	.ascii "Minus_var_LC_u64"
	.space 1
	.ascii "uMinus_var_LC_u64"
	.space 1
	.ascii "Minus_param_LC_i16"
	.space 1
	.ascii "uMinus_param_LC_i16"
	.space 1
	.ascii "Minus_var_LC_i16"
	.space 1
	.ascii "uMinus_var_LC_i16"
	.space 1
	.ascii "Minus_param_LC_I"
	.space 1
	.ascii "uMinus_param_LC_I"
	.space 1
	.ascii "Minus_var_LC_I"
	.space 1
	.ascii "uMinus_var_LC_I"
	.space 1
	.ascii "Minus_param_LC_LC"
	.space 1
	.ascii "uMinus_param_LC_LC"
	.space 1
	.ascii "Minus_var_LC_LC"
	.space 1
	.ascii "uMinus_var_LC_LC"
	.space 1
	.ascii "Minus_param_LC_u32"
	.space 1
	.ascii "uMinus_param_LC_u32"
	.space 1
	.ascii "Minus_var_LC_u32"
	.space 1
	.ascii "uMinus_var_LC_u32"
	.space 1
	.ascii "Minus_param_LC_u16"
	.space 1
	.ascii "uMinus_param_LC_u16"
	.space 1
	.ascii "Minus_var_LC_u16"
	.space 1
	.ascii "uMinus_var_LC_u16"
	.space 1
	.ascii "Minus_param_u32_i8"
	.space 1
	.ascii "uMinus_param_u32_i8"
	.space 1
	.ascii "Minus_var_u32_i8"
	.space 1
	.ascii "uMinus_var_u32_i8"
	.space 1
	.ascii "Minus_param_u32_L"
	.space 1
	.ascii "uMinus_param_u32_L"
	.space 1
	.ascii "Minus_var_u32_L"
	.space 1
	.ascii "uMinus_var_u32_L"
	.space 1
	.ascii "Minus_param_u32_i64"
	.space 1
	.ascii "uMinus_param_u32_i64"
	.space 1
	.ascii "Minus_var_u32_i64"
	.space 1
	.ascii "uMinus_var_u32_i64"
	.space 1
	.ascii "Minus_param_u32_u8"
	.space 1
	.ascii "uMinus_param_u32_u8"
	.space 1
	.ascii "Minus_var_u32_u8"
	.space 1
	.ascii "uMinus_var_u32_u8"
	.space 1
	.ascii "Minus_param_u32_C"
	.space 1
	.ascii "uMinus_param_u32_C"
	.space 1
	.ascii "Minus_var_u32_C"
	.space 1
	.ascii "uMinus_var_u32_C"
	.space 1
	.ascii "Minus_param_u32_i32"
	.space 1
	.ascii "uMinus_param_u32_i32"
	.space 1
	.ascii "Minus_var_u32_i32"
	.space 1
	.ascii "uMinus_var_u32_i32"
	.space 1
	.ascii "Minus_param_u32_u64"
	.space 1
	.ascii "uMinus_param_u32_u64"
	.space 1
	.ascii "Minus_var_u32_u64"
	.space 1
	.ascii "uMinus_var_u32_u64"
	.space 1
	.ascii "Minus_param_u32_i16"
	.space 1
	.ascii "uMinus_param_u32_i16"
	.space 1
	.ascii "Minus_var_u32_i16"
	.space 1
	.ascii "uMinus_var_u32_i16"
	.space 1
	.ascii "Minus_param_u32_I"
	.space 1
	.ascii "uMinus_param_u32_I"
	.space 1
	.ascii "Minus_var_u32_I"
	.space 1
	.ascii "uMinus_var_u32_I"
	.space 1
	.ascii "Minus_param_u32_LC"
	.space 1
	.ascii "uMinus_param_u32_LC"
	.space 1
	.ascii "Minus_var_u32_LC"
	.space 1
	.ascii "uMinus_var_u32_LC"
	.space 1
	.ascii "Minus_param_u32_u32"
	.space 1
	.ascii "uMinus_param_u32_u32"
	.space 1
	.ascii "Minus_var_u32_u32"
	.space 1
	.ascii "uMinus_var_u32_u32"
	.space 1
	.ascii "Minus_param_u32_u16"
	.space 1
	.ascii "uMinus_param_u32_u16"
	.space 1
	.ascii "Minus_var_u32_u16"
	.space 1
	.ascii "uMinus_var_u32_u16"
	.space 1
	.ascii "Minus_param_u16_i8"
	.space 1
	.ascii "uMinus_param_u16_i8"
	.space 1
	.ascii "Minus_var_u16_i8"
	.space 1
	.ascii "uMinus_var_u16_i8"
	.space 1
	.ascii "Minus_param_u16_L"
	.space 1
	.ascii "uMinus_param_u16_L"
	.space 1
	.ascii "Minus_var_u16_L"
	.space 1
	.ascii "uMinus_var_u16_L"
	.space 1
	.ascii "Minus_param_u16_i64"
	.space 1
	.ascii "uMinus_param_u16_i64"
	.space 1
	.ascii "Minus_var_u16_i64"
	.space 1
	.ascii "uMinus_var_u16_i64"
	.space 1
	.ascii "Minus_param_u16_u8"
	.space 1
	.ascii "uMinus_param_u16_u8"
	.space 1
	.ascii "Minus_var_u16_u8"
	.space 1
	.ascii "uMinus_var_u16_u8"
	.space 1
	.ascii "Minus_param_u16_C"
	.space 1
	.ascii "uMinus_param_u16_C"
	.space 1
	.ascii "Minus_var_u16_C"
	.space 1
	.ascii "uMinus_var_u16_C"
	.space 1
	.ascii "Minus_param_u16_i32"
	.space 1
	.ascii "uMinus_param_u16_i32"
	.space 1
	.ascii "Minus_var_u16_i32"
	.space 1
	.ascii "uMinus_var_u16_i32"
	.space 1
	.ascii "Minus_param_u16_u64"
	.space 1
	.ascii "uMinus_param_u16_u64"
	.space 1
	.ascii "Minus_var_u16_u64"
	.space 1
	.ascii "uMinus_var_u16_u64"
	.space 1
	.ascii "Minus_param_u16_i16"
	.space 1
	.ascii "uMinus_param_u16_i16"
	.space 1
	.ascii "Minus_var_u16_i16"
	.space 1
	.ascii "uMinus_var_u16_i16"
	.space 1
	.ascii "Minus_param_u16_I"
	.space 1
	.ascii "uMinus_param_u16_I"
	.space 1
	.ascii "Minus_var_u16_I"
	.space 1
	.ascii "uMinus_var_u16_I"
	.space 1
	.ascii "Minus_param_u16_LC"
	.space 1
	.ascii "uMinus_param_u16_LC"
	.space 1
	.ascii "Minus_var_u16_LC"
	.space 1
	.ascii "uMinus_var_u16_LC"
	.space 1
	.ascii "Minus_param_u16_u32"
	.space 1
	.ascii "uMinus_param_u16_u32"
	.space 1
	.ascii "Minus_var_u16_u32"
	.space 1
	.ascii "uMinus_var_u16_u32"
	.space 1
	.ascii "Minus_param_u16_u16"
	.space 1
	.ascii "uMinus_param_u16_u16"
	.space 1
	.ascii "Minus_var_u16_u16"
	.space 1
	.ascii "uMinus_var_u16_u16"
	.space 4
	.quad	_Minus_M3
	.quad	_L_1
	.quad	_Minus__Minus_param_i8_i8
	.quad	_L_1+9
	.quad	_Minus__uMinus_param_i8_i8
	.quad	_L_1+27
	.quad	_Minus__Minus_var_i8_i8
	.quad	_L_1+46
	.quad	_Minus__uMinus_var_i8_i8
	.quad	_L_1+62
	.quad	_Minus__Minus_param_i8_L
	.quad	_L_1+79
	.quad	_Minus__uMinus_param_i8_L
	.quad	_L_1+96
	.quad	_Minus__Minus_var_i8_L
	.quad	_L_1+114
	.quad	_Minus__uMinus_var_i8_L
	.quad	_L_1+129
	.quad	_Minus__Minus_param_i8_i64
	.quad	_L_1+145
	.quad	_Minus__uMinus_param_i8_i64
	.quad	_L_1+164
	.quad	_Minus__Minus_var_i8_i64
	.quad	_L_1+184
	.quad	_Minus__uMinus_var_i8_i64
	.quad	_L_1+201
	.quad	_Minus__Minus_param_i8_u8
	.quad	_L_1+219
	.quad	_Minus__uMinus_param_i8_u8
	.quad	_L_1+237
	.quad	_Minus__Minus_var_i8_u8
	.quad	_L_1+256
	.quad	_Minus__uMinus_var_i8_u8
	.quad	_L_1+272
	.quad	_Minus__Minus_param_i8_C
	.quad	_L_1+289
	.quad	_Minus__uMinus_param_i8_C
	.quad	_L_1+306
	.quad	_Minus__Minus_var_i8_C
	.quad	_L_1+324
	.quad	_Minus__uMinus_var_i8_C
	.quad	_L_1+339
	.quad	_Minus__Minus_param_i8_i32
	.quad	_L_1+355
	.quad	_Minus__uMinus_param_i8_i32
	.quad	_L_1+374
	.quad	_Minus__Minus_var_i8_i32
	.quad	_L_1+394
	.quad	_Minus__uMinus_var_i8_i32
	.quad	_L_1+411
	.quad	_Minus__Minus_param_i8_u64
	.quad	_L_1+429
	.quad	_Minus__uMinus_param_i8_u64
	.quad	_L_1+448
	.quad	_Minus__Minus_var_i8_u64
	.quad	_L_1+468
	.quad	_Minus__uMinus_var_i8_u64
	.quad	_L_1+485
	.quad	_Minus__Minus_param_i8_i16
	.quad	_L_1+503
	.quad	_Minus__uMinus_param_i8_i16
	.quad	_L_1+522
	.quad	_Minus__Minus_var_i8_i16
	.quad	_L_1+542
	.quad	_Minus__uMinus_var_i8_i16
	.quad	_L_1+559
	.quad	_Minus__Minus_param_i8_I
	.quad	_L_1+577
	.quad	_Minus__uMinus_param_i8_I
	.quad	_L_1+594
	.quad	_Minus__Minus_var_i8_I
	.quad	_L_1+612
	.quad	_Minus__uMinus_var_i8_I
	.quad	_L_1+627
	.quad	_Minus__Minus_param_i8_LC
	.quad	_L_1+643
	.quad	_Minus__uMinus_param_i8_LC
	.quad	_L_1+661
	.quad	_Minus__Minus_var_i8_LC
	.quad	_L_1+680
	.quad	_Minus__uMinus_var_i8_LC
	.quad	_L_1+696
	.quad	_Minus__Minus_param_i8_u32
	.quad	_L_1+713
	.quad	_Minus__uMinus_param_i8_u32
	.quad	_L_1+732
	.quad	_Minus__Minus_var_i8_u32
	.quad	_L_1+752
	.quad	_Minus__uMinus_var_i8_u32
	.quad	_L_1+769
	.quad	_Minus__Minus_param_i8_u16
	.quad	_L_1+787
	.quad	_Minus__uMinus_param_i8_u16
	.quad	_L_1+806
	.quad	_Minus__Minus_var_i8_u16
	.quad	_L_1+826
	.quad	_Minus__uMinus_var_i8_u16
	.quad	_L_1+843
	.quad	_Minus__Minus_param_L_i8
	.quad	_L_1+861
	.quad	_Minus__uMinus_param_L_i8
	.quad	_L_1+878
	.quad	_Minus__Minus_var_L_i8
	.quad	_L_1+896
	.quad	_Minus__uMinus_var_L_i8
	.quad	_L_1+911
	.quad	_Minus__Minus_param_L_L
	.quad	_L_1+927
	.quad	_Minus__uMinus_param_L_L
	.quad	_L_1+943
	.quad	_Minus__Minus_var_L_L
	.quad	_L_1+960
	.quad	_Minus__uMinus_var_L_L
	.quad	_L_1+974
	.quad	_Minus__Minus_param_L_i64
	.quad	_L_1+989
	.quad	_Minus__uMinus_param_L_i64
	.quad	_L_1+1007
	.quad	_Minus__Minus_var_L_i64
	.quad	_L_1+1026
	.quad	_Minus__uMinus_var_L_i64
	.quad	_L_1+1042
	.quad	_Minus__Minus_param_L_u8
	.quad	_L_1+1059
	.quad	_Minus__uMinus_param_L_u8
	.quad	_L_1+1076
	.quad	_Minus__Minus_var_L_u8
	.quad	_L_1+1094
	.quad	_Minus__uMinus_var_L_u8
	.quad	_L_1+1109
	.quad	_Minus__Minus_param_L_C
	.quad	_L_1+1125
	.quad	_Minus__uMinus_param_L_C
	.quad	_L_1+1141
	.quad	_Minus__Minus_var_L_C
	.quad	_L_1+1158
	.quad	_Minus__uMinus_var_L_C
	.quad	_L_1+1172
	.quad	_Minus__Minus_param_L_i32
	.quad	_L_1+1187
	.quad	_Minus__uMinus_param_L_i32
	.quad	_L_1+1205
	.quad	_Minus__Minus_var_L_i32
	.quad	_L_1+1224
	.quad	_Minus__uMinus_var_L_i32
	.quad	_L_1+1240
	.quad	_Minus__Minus_param_L_u64
	.quad	_L_1+1257
	.quad	_Minus__uMinus_param_L_u64
	.quad	_L_1+1275
	.quad	_Minus__Minus_var_L_u64
	.quad	_L_1+1294
	.quad	_Minus__uMinus_var_L_u64
	.quad	_L_1+1310
	.quad	_Minus__Minus_param_L_i16
	.quad	_L_1+1327
	.quad	_Minus__uMinus_param_L_i16
	.quad	_L_1+1345
	.quad	_Minus__Minus_var_L_i16
	.quad	_L_1+1364
	.quad	_Minus__uMinus_var_L_i16
	.quad	_L_1+1380
	.quad	_Minus__Minus_param_L_I
	.quad	_L_1+1397
	.quad	_Minus__uMinus_param_L_I
	.quad	_L_1+1413
	.quad	_Minus__Minus_var_L_I
	.quad	_L_1+1430
	.quad	_Minus__uMinus_var_L_I
	.quad	_L_1+1444
	.quad	_Minus__Minus_param_L_LC
	.quad	_L_1+1459
	.quad	_Minus__uMinus_param_L_LC
	.quad	_L_1+1476
	.quad	_Minus__Minus_var_L_LC
	.quad	_L_1+1494
	.quad	_Minus__uMinus_var_L_LC
	.quad	_L_1+1509
	.quad	_Minus__Minus_param_L_u32
	.quad	_L_1+1525
	.quad	_Minus__uMinus_param_L_u32
	.quad	_L_1+1543
	.quad	_Minus__Minus_var_L_u32
	.quad	_L_1+1562
	.quad	_Minus__uMinus_var_L_u32
	.quad	_L_1+1578
	.quad	_Minus__Minus_param_L_u16
	.quad	_L_1+1595
	.quad	_Minus__uMinus_param_L_u16
	.quad	_L_1+1613
	.quad	_Minus__Minus_var_L_u16
	.quad	_L_1+1632
	.quad	_Minus__uMinus_var_L_u16
	.quad	_L_1+1648
	.quad	_Minus__Minus_param_f32_f32
	.quad	_L_1+1665
	.quad	_Minus__Minus_var_f32_f32
	.quad	_L_1+1685
	.quad	_Minus__Minus_param_i64_i8
	.quad	_L_1+1703
	.quad	_Minus__uMinus_param_i64_i8
	.quad	_L_1+1722
	.quad	_Minus__Minus_var_i64_i8
	.quad	_L_1+1742
	.quad	_Minus__uMinus_var_i64_i8
	.quad	_L_1+1759
	.quad	_Minus__Minus_param_i64_L
	.quad	_L_1+1777
	.quad	_Minus__uMinus_param_i64_L
	.quad	_L_1+1795
	.quad	_Minus__Minus_var_i64_L
	.quad	_L_1+1814
	.quad	_Minus__uMinus_var_i64_L
	.quad	_L_1+1830
	.quad	_Minus__Minus_param_i64_i64
	.quad	_L_1+1847
	.quad	_Minus__uMinus_param_i64_i64
	.quad	_L_1+1867
	.quad	_Minus__Minus_var_i64_i64
	.quad	_L_1+1888
	.quad	_Minus__uMinus_var_i64_i64
	.quad	_L_1+1906
	.quad	_Minus__Minus_param_i64_u8
	.quad	_L_1+1925
	.quad	_Minus__uMinus_param_i64_u8
	.quad	_L_1+1944
	.quad	_Minus__Minus_var_i64_u8
	.quad	_L_1+1964
	.quad	_Minus__uMinus_var_i64_u8
	.quad	_L_1+1981
	.quad	_Minus__Minus_param_i64_C
	.quad	_L_1+1999
	.quad	_Minus__uMinus_param_i64_C
	.quad	_L_1+2017
	.quad	_Minus__Minus_var_i64_C
	.quad	_L_1+2036
	.quad	_Minus__uMinus_var_i64_C
	.quad	_L_1+2052
	.quad	_Minus__Minus_param_i64_i32
	.quad	_L_1+2069
	.quad	_Minus__uMinus_param_i64_i32
	.quad	_L_1+2089
	.quad	_Minus__Minus_var_i64_i32
	.quad	_L_1+2110
	.quad	_Minus__uMinus_var_i64_i32
	.quad	_L_1+2128
	.quad	_Minus__Minus_param_i64_u64
	.quad	_L_1+2147
	.quad	_Minus__uMinus_param_i64_u64
	.quad	_L_1+2167
	.quad	_Minus__Minus_var_i64_u64
	.quad	_L_1+2188
	.quad	_Minus__uMinus_var_i64_u64
	.quad	_L_1+2206
	.quad	_Minus__Minus_param_i64_i16
	.quad	_L_1+2225
	.quad	_Minus__uMinus_param_i64_i16
	.quad	_L_1+2245
	.quad	_Minus__Minus_var_i64_i16
	.quad	_L_1+2266
	.quad	_Minus__uMinus_var_i64_i16
	.quad	_L_1+2284
	.quad	_Minus__Minus_param_i64_I
	.quad	_L_1+2303
	.quad	_Minus__uMinus_param_i64_I
	.quad	_L_1+2321
	.quad	_Minus__Minus_var_i64_I
	.quad	_L_1+2340
	.quad	_Minus__uMinus_var_i64_I
	.quad	_L_1+2356
	.quad	_Minus__Minus_param_i64_LC
	.quad	_L_1+2373
	.quad	_Minus__uMinus_param_i64_LC
	.quad	_L_1+2392
	.quad	_Minus__Minus_var_i64_LC
	.quad	_L_1+2412
	.quad	_Minus__uMinus_var_i64_LC
	.quad	_L_1+2429
	.quad	_Minus__Minus_param_i64_u32
	.quad	_L_1+2447
	.quad	_Minus__uMinus_param_i64_u32
	.quad	_L_1+2467
	.quad	_Minus__Minus_var_i64_u32
	.quad	_L_1+2488
	.quad	_Minus__uMinus_var_i64_u32
	.quad	_L_1+2506
	.quad	_Minus__Minus_param_i64_u16
	.quad	_L_1+2525
	.quad	_Minus__uMinus_param_i64_u16
	.quad	_L_1+2545
	.quad	_Minus__Minus_var_i64_u16
	.quad	_L_1+2566
	.quad	_Minus__uMinus_var_i64_u16
	.quad	_L_1+2584
	.quad	_Minus__Minus_param_u8_i8
	.quad	_L_1+2603
	.quad	_Minus__uMinus_param_u8_i8
	.quad	_L_1+2621
	.quad	_Minus__Minus_var_u8_i8
	.quad	_L_1+2640
	.quad	_Minus__uMinus_var_u8_i8
	.quad	_L_1+2656
	.quad	_Minus__Minus_param_u8_L
	.quad	_L_1+2673
	.quad	_Minus__uMinus_param_u8_L
	.quad	_L_1+2690
	.quad	_Minus__Minus_var_u8_L
	.quad	_L_1+2708
	.quad	_Minus__uMinus_var_u8_L
	.quad	_L_1+2723
	.quad	_Minus__Minus_param_u8_i64
	.quad	_L_1+2739
	.quad	_Minus__uMinus_param_u8_i64
	.quad	_L_1+2758
	.quad	_Minus__Minus_var_u8_i64
	.quad	_L_1+2778
	.quad	_Minus__uMinus_var_u8_i64
	.quad	_L_1+2795
	.quad	_Minus__Minus_param_u8_u8
	.quad	_L_1+2813
	.quad	_Minus__uMinus_param_u8_u8
	.quad	_L_1+2831
	.quad	_Minus__Minus_var_u8_u8
	.quad	_L_1+2850
	.quad	_Minus__uMinus_var_u8_u8
	.quad	_L_1+2866
	.quad	_Minus__Minus_param_u8_C
	.quad	_L_1+2883
	.quad	_Minus__uMinus_param_u8_C
	.quad	_L_1+2900
	.quad	_Minus__Minus_var_u8_C
	.quad	_L_1+2918
	.quad	_Minus__uMinus_var_u8_C
	.quad	_L_1+2933
	.quad	_Minus__Minus_param_u8_i32
	.quad	_L_1+2949
	.quad	_Minus__uMinus_param_u8_i32
	.quad	_L_1+2968
	.quad	_Minus__Minus_var_u8_i32
	.quad	_L_1+2988
	.quad	_Minus__uMinus_var_u8_i32
	.quad	_L_1+3005
	.quad	_Minus__Minus_param_u8_u64
	.quad	_L_1+3023
	.quad	_Minus__uMinus_param_u8_u64
	.quad	_L_1+3042
	.quad	_Minus__Minus_var_u8_u64
	.quad	_L_1+3062
	.quad	_Minus__uMinus_var_u8_u64
	.quad	_L_1+3079
	.quad	_Minus__Minus_param_u8_i16
	.quad	_L_1+3097
	.quad	_Minus__uMinus_param_u8_i16
	.quad	_L_1+3116
	.quad	_Minus__Minus_var_u8_i16
	.quad	_L_1+3136
	.quad	_Minus__uMinus_var_u8_i16
	.quad	_L_1+3153
	.quad	_Minus__Minus_param_u8_I
	.quad	_L_1+3171
	.quad	_Minus__uMinus_param_u8_I
	.quad	_L_1+3188
	.quad	_Minus__Minus_var_u8_I
	.quad	_L_1+3206
	.quad	_Minus__uMinus_var_u8_I
	.quad	_L_1+3221
	.quad	_Minus__Minus_param_u8_LC
	.quad	_L_1+3237
	.quad	_Minus__uMinus_param_u8_LC
	.quad	_L_1+3255
	.quad	_Minus__Minus_var_u8_LC
	.quad	_L_1+3274
	.quad	_Minus__uMinus_var_u8_LC
	.quad	_L_1+3290
	.quad	_Minus__Minus_param_u8_u32
	.quad	_L_1+3307
	.quad	_Minus__uMinus_param_u8_u32
	.quad	_L_1+3326
	.quad	_Minus__Minus_var_u8_u32
	.quad	_L_1+3346
	.quad	_Minus__uMinus_var_u8_u32
	.quad	_L_1+3363
	.quad	_Minus__Minus_param_u8_u16
	.quad	_L_1+3381
	.quad	_Minus__uMinus_param_u8_u16
	.quad	_L_1+3400
	.quad	_Minus__Minus_var_u8_u16
	.quad	_L_1+3420
	.quad	_Minus__uMinus_var_u8_u16
	.quad	_L_1+3437
	.quad	_Minus__Minus_param_C_i8
	.quad	_L_1+3455
	.quad	_Minus__uMinus_param_C_i8
	.quad	_L_1+3472
	.quad	_Minus__Minus_var_C_i8
	.quad	_L_1+3490
	.quad	_Minus__uMinus_var_C_i8
	.quad	_L_1+3505
	.quad	_Minus__Minus_param_C_L
	.quad	_L_1+3521
	.quad	_Minus__uMinus_param_C_L
	.quad	_L_1+3537
	.quad	_Minus__Minus_var_C_L
	.quad	_L_1+3554
	.quad	_Minus__uMinus_var_C_L
	.quad	_L_1+3568
	.quad	_Minus__Minus_param_C_i64
	.quad	_L_1+3583
	.quad	_Minus__uMinus_param_C_i64
	.quad	_L_1+3601
	.quad	_Minus__Minus_var_C_i64
	.quad	_L_1+3620
	.quad	_Minus__uMinus_var_C_i64
	.quad	_L_1+3636
	.quad	_Minus__Minus_param_C_u8
	.quad	_L_1+3653
	.quad	_Minus__uMinus_param_C_u8
	.quad	_L_1+3670
	.quad	_Minus__Minus_var_C_u8
	.quad	_L_1+3688
	.quad	_Minus__uMinus_var_C_u8
	.quad	_L_1+3703
	.quad	_Minus__Minus_param_C_C
	.quad	_L_1+3719
	.quad	_Minus__uMinus_param_C_C
	.quad	_L_1+3735
	.quad	_Minus__Minus_var_C_C
	.quad	_L_1+3752
	.quad	_Minus__uMinus_var_C_C
	.quad	_L_1+3766
	.quad	_Minus__Minus_param_C_i32
	.quad	_L_1+3781
	.quad	_Minus__uMinus_param_C_i32
	.quad	_L_1+3799
	.quad	_Minus__Minus_var_C_i32
	.quad	_L_1+3818
	.quad	_Minus__uMinus_var_C_i32
	.quad	_L_1+3834
	.quad	_Minus__Minus_param_C_u64
	.quad	_L_1+3851
	.quad	_Minus__uMinus_param_C_u64
	.quad	_L_1+3869
	.quad	_Minus__Minus_var_C_u64
	.quad	_L_1+3888
	.quad	_Minus__uMinus_var_C_u64
	.quad	_L_1+3904
	.quad	_Minus__Minus_param_C_i16
	.quad	_L_1+3921
	.quad	_Minus__uMinus_param_C_i16
	.quad	_L_1+3939
	.quad	_Minus__Minus_var_C_i16
	.quad	_L_1+3958
	.quad	_Minus__uMinus_var_C_i16
	.quad	_L_1+3974
	.quad	_Minus__Minus_param_C_I
	.quad	_L_1+3991
	.quad	_Minus__uMinus_param_C_I
	.quad	_L_1+4007
	.quad	_Minus__Minus_var_C_I
	.quad	_L_1+4024
	.quad	_Minus__uMinus_var_C_I
	.quad	_L_1+4038
	.quad	_Minus__Minus_param_C_LC
	.quad	_L_1+4053
	.quad	_Minus__uMinus_param_C_LC
	.quad	_L_1+4070
	.quad	_Minus__Minus_var_C_LC
	.quad	_L_1+4088
	.quad	_Minus__uMinus_var_C_LC
	.quad	_L_1+4103
	.quad	_Minus__Minus_param_C_u32
	.quad	_L_1+4119
	.quad	_Minus__uMinus_param_C_u32
	.quad	_L_1+4137
	.quad	_Minus__Minus_var_C_u32
	.quad	_L_1+4156
	.quad	_Minus__uMinus_var_C_u32
	.quad	_L_1+4172
	.quad	_Minus__Minus_param_C_u16
	.quad	_L_1+4189
	.quad	_Minus__uMinus_param_C_u16
	.quad	_L_1+4207
	.quad	_Minus__Minus_var_C_u16
	.quad	_L_1+4226
	.quad	_Minus__uMinus_var_C_u16
	.quad	_L_1+4242
	.quad	_Minus__Minus_param_i32_i8
	.quad	_L_1+4259
	.quad	_Minus__uMinus_param_i32_i8
	.quad	_L_1+4278
	.quad	_Minus__Minus_var_i32_i8
	.quad	_L_1+4298
	.quad	_Minus__uMinus_var_i32_i8
	.quad	_L_1+4315
	.quad	_Minus__Minus_param_i32_L
	.quad	_L_1+4333
	.quad	_Minus__uMinus_param_i32_L
	.quad	_L_1+4351
	.quad	_Minus__Minus_var_i32_L
	.quad	_L_1+4370
	.quad	_Minus__uMinus_var_i32_L
	.quad	_L_1+4386
	.quad	_Minus__Minus_param_i32_i64
	.quad	_L_1+4403
	.quad	_Minus__uMinus_param_i32_i64
	.quad	_L_1+4423
	.quad	_Minus__Minus_var_i32_i64
	.quad	_L_1+4444
	.quad	_Minus__uMinus_var_i32_i64
	.quad	_L_1+4462
	.quad	_Minus__Minus_param_i32_u8
	.quad	_L_1+4481
	.quad	_Minus__uMinus_param_i32_u8
	.quad	_L_1+4500
	.quad	_Minus__Minus_var_i32_u8
	.quad	_L_1+4520
	.quad	_Minus__uMinus_var_i32_u8
	.quad	_L_1+4537
	.quad	_Minus__Minus_param_i32_C
	.quad	_L_1+4555
	.quad	_Minus__uMinus_param_i32_C
	.quad	_L_1+4573
	.quad	_Minus__Minus_var_i32_C
	.quad	_L_1+4592
	.quad	_Minus__uMinus_var_i32_C
	.quad	_L_1+4608
	.quad	_Minus__Minus_param_i32_i32
	.quad	_L_1+4625
	.quad	_Minus__uMinus_param_i32_i32
	.quad	_L_1+4645
	.quad	_Minus__Minus_var_i32_i32
	.quad	_L_1+4666
	.quad	_Minus__uMinus_var_i32_i32
	.quad	_L_1+4684
	.quad	_Minus__Minus_param_i32_u64
	.quad	_L_1+4703
	.quad	_Minus__uMinus_param_i32_u64
	.quad	_L_1+4723
	.quad	_Minus__Minus_var_i32_u64
	.quad	_L_1+4744
	.quad	_Minus__uMinus_var_i32_u64
	.quad	_L_1+4762
	.quad	_Minus__Minus_param_i32_i16
	.quad	_L_1+4781
	.quad	_Minus__uMinus_param_i32_i16
	.quad	_L_1+4801
	.quad	_Minus__Minus_var_i32_i16
	.quad	_L_1+4822
	.quad	_Minus__uMinus_var_i32_i16
	.quad	_L_1+4840
	.quad	_Minus__Minus_param_i32_I
	.quad	_L_1+4859
	.quad	_Minus__uMinus_param_i32_I
	.quad	_L_1+4877
	.quad	_Minus__Minus_var_i32_I
	.quad	_L_1+4896
	.quad	_Minus__uMinus_var_i32_I
	.quad	_L_1+4912
	.quad	_Minus__Minus_param_i32_LC
	.quad	_L_1+4929
	.quad	_Minus__uMinus_param_i32_LC
	.quad	_L_1+4948
	.quad	_Minus__Minus_var_i32_LC
	.quad	_L_1+4968
	.quad	_Minus__uMinus_var_i32_LC
	.quad	_L_1+4985
	.quad	_Minus__Minus_param_i32_u32
	.quad	_L_1+5003
	.quad	_Minus__uMinus_param_i32_u32
	.quad	_L_1+5023
	.quad	_Minus__Minus_var_i32_u32
	.quad	_L_1+5044
	.quad	_Minus__uMinus_var_i32_u32
	.quad	_L_1+5062
	.quad	_Minus__Minus_param_i32_u16
	.quad	_L_1+5081
	.quad	_Minus__uMinus_param_i32_u16
	.quad	_L_1+5101
	.quad	_Minus__Minus_var_i32_u16
	.quad	_L_1+5122
	.quad	_Minus__uMinus_var_i32_u16
	.quad	_L_1+5140
	.quad	_Minus__Minus_param_f64_f64
	.quad	_L_1+5159
	.quad	_Minus__Minus_var_f64_f64
	.quad	_L_1+5179
	.quad	_Minus__Minus_param_u64_i8
	.quad	_L_1+5197
	.quad	_Minus__uMinus_param_u64_i8
	.quad	_L_1+5216
	.quad	_Minus__Minus_var_u64_i8
	.quad	_L_1+5236
	.quad	_Minus__uMinus_var_u64_i8
	.quad	_L_1+5253
	.quad	_Minus__Minus_param_u64_L
	.quad	_L_1+5271
	.quad	_Minus__uMinus_param_u64_L
	.quad	_L_1+5289
	.quad	_Minus__Minus_var_u64_L
	.quad	_L_1+5308
	.quad	_Minus__uMinus_var_u64_L
	.quad	_L_1+5324
	.quad	_Minus__Minus_param_u64_i64
	.quad	_L_1+5341
	.quad	_Minus__uMinus_param_u64_i64
	.quad	_L_1+5361
	.quad	_Minus__Minus_var_u64_i64
	.quad	_L_1+5382
	.quad	_Minus__uMinus_var_u64_i64
	.quad	_L_1+5400
	.quad	_Minus__Minus_param_u64_u8
	.quad	_L_1+5419
	.quad	_Minus__uMinus_param_u64_u8
	.quad	_L_1+5438
	.quad	_Minus__Minus_var_u64_u8
	.quad	_L_1+5458
	.quad	_Minus__uMinus_var_u64_u8
	.quad	_L_1+5475
	.quad	_Minus__Minus_param_u64_C
	.quad	_L_1+5493
	.quad	_Minus__uMinus_param_u64_C
	.quad	_L_1+5511
	.quad	_Minus__Minus_var_u64_C
	.quad	_L_1+5530
	.quad	_Minus__uMinus_var_u64_C
	.quad	_L_1+5546
	.quad	_Minus__Minus_param_u64_i32
	.quad	_L_1+5563
	.quad	_Minus__uMinus_param_u64_i32
	.quad	_L_1+5583
	.quad	_Minus__Minus_var_u64_i32
	.quad	_L_1+5604
	.quad	_Minus__uMinus_var_u64_i32
	.quad	_L_1+5622
	.quad	_Minus__Minus_param_u64_u64
	.quad	_L_1+5641
	.quad	_Minus__uMinus_param_u64_u64
	.quad	_L_1+5661
	.quad	_Minus__Minus_var_u64_u64
	.quad	_L_1+5682
	.quad	_Minus__uMinus_var_u64_u64
	.quad	_L_1+5700
	.quad	_Minus__Minus_param_u64_i16
	.quad	_L_1+5719
	.quad	_Minus__uMinus_param_u64_i16
	.quad	_L_1+5739
	.quad	_Minus__Minus_var_u64_i16
	.quad	_L_1+5760
	.quad	_Minus__uMinus_var_u64_i16
	.quad	_L_1+5778
	.quad	_Minus__Minus_param_u64_I
	.quad	_L_1+5797
	.quad	_Minus__uMinus_param_u64_I
	.quad	_L_1+5815
	.quad	_Minus__Minus_var_u64_I
	.quad	_L_1+5834
	.quad	_Minus__uMinus_var_u64_I
	.quad	_L_1+5850
	.quad	_Minus__Minus_param_u64_LC
	.quad	_L_1+5867
	.quad	_Minus__uMinus_param_u64_LC
	.quad	_L_1+5886
	.quad	_Minus__Minus_var_u64_LC
	.quad	_L_1+5906
	.quad	_Minus__uMinus_var_u64_LC
	.quad	_L_1+5923
	.quad	_Minus__Minus_param_u64_u32
	.quad	_L_1+5941
	.quad	_Minus__uMinus_param_u64_u32
	.quad	_L_1+5961
	.quad	_Minus__Minus_var_u64_u32
	.quad	_L_1+5982
	.quad	_Minus__uMinus_var_u64_u32
	.quad	_L_1+6000
	.quad	_Minus__Minus_param_u64_u16
	.quad	_L_1+6019
	.quad	_Minus__uMinus_param_u64_u16
	.quad	_L_1+6039
	.quad	_Minus__Minus_var_u64_u16
	.quad	_L_1+6060
	.quad	_Minus__uMinus_var_u64_u16
	.quad	_L_1+6078
	.quad	_Minus__Minus_param_i16_i8
	.quad	_L_1+6097
	.quad	_Minus__uMinus_param_i16_i8
	.quad	_L_1+6116
	.quad	_Minus__Minus_var_i16_i8
	.quad	_L_1+6136
	.quad	_Minus__uMinus_var_i16_i8
	.quad	_L_1+6153
	.quad	_Minus__Minus_param_i16_L
	.quad	_L_1+6171
	.quad	_Minus__uMinus_param_i16_L
	.quad	_L_1+6189
	.quad	_Minus__Minus_var_i16_L
	.quad	_L_1+6208
	.quad	_Minus__uMinus_var_i16_L
	.quad	_L_1+6224
	.quad	_Minus__Minus_param_i16_i64
	.quad	_L_1+6241
	.quad	_Minus__uMinus_param_i16_i64
	.quad	_L_1+6261
	.quad	_Minus__Minus_var_i16_i64
	.quad	_L_1+6282
	.quad	_Minus__uMinus_var_i16_i64
	.quad	_L_1+6300
	.quad	_Minus__Minus_param_i16_u8
	.quad	_L_1+6319
	.quad	_Minus__uMinus_param_i16_u8
	.quad	_L_1+6338
	.quad	_Minus__Minus_var_i16_u8
	.quad	_L_1+6358
	.quad	_Minus__uMinus_var_i16_u8
	.quad	_L_1+6375
	.quad	_Minus__Minus_param_i16_C
	.quad	_L_1+6393
	.quad	_Minus__uMinus_param_i16_C
	.quad	_L_1+6411
	.quad	_Minus__Minus_var_i16_C
	.quad	_L_1+6430
	.quad	_Minus__uMinus_var_i16_C
	.quad	_L_1+6446
	.quad	_Minus__Minus_param_i16_i32
	.quad	_L_1+6463
	.quad	_Minus__uMinus_param_i16_i32
	.quad	_L_1+6483
	.quad	_Minus__Minus_var_i16_i32
	.quad	_L_1+6504
	.quad	_Minus__uMinus_var_i16_i32
	.quad	_L_1+6522
	.quad	_Minus__Minus_param_i16_u64
	.quad	_L_1+6541
	.quad	_Minus__uMinus_param_i16_u64
	.quad	_L_1+6561
	.quad	_Minus__Minus_var_i16_u64
	.quad	_L_1+6582
	.quad	_Minus__uMinus_var_i16_u64
	.quad	_L_1+6600
	.quad	_Minus__Minus_param_i16_i16
	.quad	_L_1+6619
	.quad	_Minus__uMinus_param_i16_i16
	.quad	_L_1+6639
	.quad	_Minus__Minus_var_i16_i16
	.quad	_L_1+6660
	.quad	_Minus__uMinus_var_i16_i16
	.quad	_L_1+6678
	.quad	_Minus__Minus_param_i16_I
	.quad	_L_1+6697
	.quad	_Minus__uMinus_param_i16_I
	.quad	_L_1+6715
	.quad	_Minus__Minus_var_i16_I
	.quad	_L_1+6734
	.quad	_Minus__uMinus_var_i16_I
	.quad	_L_1+6750
	.quad	_Minus__Minus_param_i16_LC
	.quad	_L_1+6767
	.quad	_Minus__uMinus_param_i16_LC
	.quad	_L_1+6786
	.quad	_Minus__Minus_var_i16_LC
	.quad	_L_1+6806
	.quad	_Minus__uMinus_var_i16_LC
	.quad	_L_1+6823
	.quad	_Minus__Minus_param_i16_u32
	.quad	_L_1+6841
	.quad	_Minus__uMinus_param_i16_u32
	.quad	_L_1+6861
	.quad	_Minus__Minus_var_i16_u32
	.quad	_L_1+6882
	.quad	_Minus__uMinus_var_i16_u32
	.quad	_L_1+6900
	.quad	_Minus__Minus_param_i16_u16
	.quad	_L_1+6919
	.quad	_Minus__uMinus_param_i16_u16
	.quad	_L_1+6939
	.quad	_Minus__Minus_var_i16_u16
	.quad	_L_1+6960
	.quad	_Minus__uMinus_var_i16_u16
	.quad	_L_1+6978
	.quad	_Minus__Minus_param_I_i8
	.quad	_L_1+6997
	.quad	_Minus__uMinus_param_I_i8
	.quad	_L_1+7014
	.quad	_Minus__Minus_var_I_i8
	.quad	_L_1+7032
	.quad	_Minus__uMinus_var_I_i8
	.quad	_L_1+7047
	.quad	_Minus__Minus_param_I_L
	.quad	_L_1+7063
	.quad	_Minus__uMinus_param_I_L
	.quad	_L_1+7079
	.quad	_Minus__Minus_var_I_L
	.quad	_L_1+7096
	.quad	_Minus__uMinus_var_I_L
	.quad	_L_1+7110
	.quad	_Minus__Minus_param_I_i64
	.quad	_L_1+7125
	.quad	_Minus__uMinus_param_I_i64
	.quad	_L_1+7143
	.quad	_Minus__Minus_var_I_i64
	.quad	_L_1+7162
	.quad	_Minus__uMinus_var_I_i64
	.quad	_L_1+7178
	.quad	_Minus__Minus_param_I_u8
	.quad	_L_1+7195
	.quad	_Minus__uMinus_param_I_u8
	.quad	_L_1+7212
	.quad	_Minus__Minus_var_I_u8
	.quad	_L_1+7230
	.quad	_Minus__uMinus_var_I_u8
	.quad	_L_1+7245
	.quad	_Minus__Minus_param_I_C
	.quad	_L_1+7261
	.quad	_Minus__uMinus_param_I_C
	.quad	_L_1+7277
	.quad	_Minus__Minus_var_I_C
	.quad	_L_1+7294
	.quad	_Minus__uMinus_var_I_C
	.quad	_L_1+7308
	.quad	_Minus__Minus_param_I_i32
	.quad	_L_1+7323
	.quad	_Minus__uMinus_param_I_i32
	.quad	_L_1+7341
	.quad	_Minus__Minus_var_I_i32
	.quad	_L_1+7360
	.quad	_Minus__uMinus_var_I_i32
	.quad	_L_1+7376
	.quad	_Minus__Minus_param_I_u64
	.quad	_L_1+7393
	.quad	_Minus__uMinus_param_I_u64
	.quad	_L_1+7411
	.quad	_Minus__Minus_var_I_u64
	.quad	_L_1+7430
	.quad	_Minus__uMinus_var_I_u64
	.quad	_L_1+7446
	.quad	_Minus__Minus_param_I_i16
	.quad	_L_1+7463
	.quad	_Minus__uMinus_param_I_i16
	.quad	_L_1+7481
	.quad	_Minus__Minus_var_I_i16
	.quad	_L_1+7500
	.quad	_Minus__uMinus_var_I_i16
	.quad	_L_1+7516
	.quad	_Minus__Minus_param_I_I
	.quad	_L_1+7533
	.quad	_Minus__uMinus_param_I_I
	.quad	_L_1+7549
	.quad	_Minus__Minus_var_I_I
	.quad	_L_1+7566
	.quad	_Minus__uMinus_var_I_I
	.quad	_L_1+7580
	.quad	_Minus__Minus_param_I_LC
	.quad	_L_1+7595
	.quad	_Minus__uMinus_param_I_LC
	.quad	_L_1+7612
	.quad	_Minus__Minus_var_I_LC
	.quad	_L_1+7630
	.quad	_Minus__uMinus_var_I_LC
	.quad	_L_1+7645
	.quad	_Minus__Minus_param_I_u32
	.quad	_L_1+7661
	.quad	_Minus__uMinus_param_I_u32
	.quad	_L_1+7679
	.quad	_Minus__Minus_var_I_u32
	.quad	_L_1+7698
	.quad	_Minus__uMinus_var_I_u32
	.quad	_L_1+7714
	.quad	_Minus__Minus_param_I_u16
	.quad	_L_1+7731
	.quad	_Minus__uMinus_param_I_u16
	.quad	_L_1+7749
	.quad	_Minus__Minus_var_I_u16
	.quad	_L_1+7768
	.quad	_Minus__uMinus_var_I_u16
	.quad	_L_1+7784
	.quad	_Minus__Minus_param_LC_i8
	.quad	_L_1+7801
	.quad	_Minus__uMinus_param_LC_i8
	.quad	_L_1+7819
	.quad	_Minus__Minus_var_LC_i8
	.quad	_L_1+7838
	.quad	_Minus__uMinus_var_LC_i8
	.quad	_L_1+7854
	.quad	_Minus__Minus_param_LC_L
	.quad	_L_1+7871
	.quad	_Minus__uMinus_param_LC_L
	.quad	_L_1+7888
	.quad	_Minus__Minus_var_LC_L
	.quad	_L_1+7906
	.quad	_Minus__uMinus_var_LC_L
	.quad	_L_1+7921
	.quad	_Minus__Minus_param_LC_i64
	.quad	_L_1+7937
	.quad	_Minus__uMinus_param_LC_i64
	.quad	_L_1+7956
	.quad	_Minus__Minus_var_LC_i64
	.quad	_L_1+7976
	.quad	_Minus__uMinus_var_LC_i64
	.quad	_L_1+7993
	.quad	_Minus__Minus_param_LC_u8
	.quad	_L_1+8011
	.quad	_Minus__uMinus_param_LC_u8
	.quad	_L_1+8029
	.quad	_Minus__Minus_var_LC_u8
	.quad	_L_1+8048
	.quad	_Minus__uMinus_var_LC_u8
	.quad	_L_1+8064
	.quad	_Minus__Minus_param_LC_C
	.quad	_L_1+8081
	.quad	_Minus__uMinus_param_LC_C
	.quad	_L_1+8098
	.quad	_Minus__Minus_var_LC_C
	.quad	_L_1+8116
	.quad	_Minus__uMinus_var_LC_C
	.quad	_L_1+8131
	.quad	_Minus__Minus_param_LC_i32
	.quad	_L_1+8147
	.quad	_Minus__uMinus_param_LC_i32
	.quad	_L_1+8166
	.quad	_Minus__Minus_var_LC_i32
	.quad	_L_1+8186
	.quad	_Minus__uMinus_var_LC_i32
	.quad	_L_1+8203
	.quad	_Minus__Minus_param_LC_u64
	.quad	_L_1+8221
	.quad	_Minus__uMinus_param_LC_u64
	.quad	_L_1+8240
	.quad	_Minus__Minus_var_LC_u64
	.quad	_L_1+8260
	.quad	_Minus__uMinus_var_LC_u64
	.quad	_L_1+8277
	.quad	_Minus__Minus_param_LC_i16
	.quad	_L_1+8295
	.quad	_Minus__uMinus_param_LC_i16
	.quad	_L_1+8314
	.quad	_Minus__Minus_var_LC_i16
	.quad	_L_1+8334
	.quad	_Minus__uMinus_var_LC_i16
	.quad	_L_1+8351
	.quad	_Minus__Minus_param_LC_I
	.quad	_L_1+8369
	.quad	_Minus__uMinus_param_LC_I
	.quad	_L_1+8386
	.quad	_Minus__Minus_var_LC_I
	.quad	_L_1+8404
	.quad	_Minus__uMinus_var_LC_I
	.quad	_L_1+8419
	.quad	_Minus__Minus_param_LC_LC
	.quad	_L_1+8435
	.quad	_Minus__uMinus_param_LC_LC
	.quad	_L_1+8453
	.quad	_Minus__Minus_var_LC_LC
	.quad	_L_1+8472
	.quad	_Minus__uMinus_var_LC_LC
	.quad	_L_1+8488
	.quad	_Minus__Minus_param_LC_u32
	.quad	_L_1+8505
	.quad	_Minus__uMinus_param_LC_u32
	.quad	_L_1+8524
	.quad	_Minus__Minus_var_LC_u32
	.quad	_L_1+8544
	.quad	_Minus__uMinus_var_LC_u32
	.quad	_L_1+8561
	.quad	_Minus__Minus_param_LC_u16
	.quad	_L_1+8579
	.quad	_Minus__uMinus_param_LC_u16
	.quad	_L_1+8598
	.quad	_Minus__Minus_var_LC_u16
	.quad	_L_1+8618
	.quad	_Minus__uMinus_var_LC_u16
	.quad	_L_1+8635
	.quad	_Minus__Minus_param_u32_i8
	.quad	_L_1+8653
	.quad	_Minus__uMinus_param_u32_i8
	.quad	_L_1+8672
	.quad	_Minus__Minus_var_u32_i8
	.quad	_L_1+8692
	.quad	_Minus__uMinus_var_u32_i8
	.quad	_L_1+8709
	.quad	_Minus__Minus_param_u32_L
	.quad	_L_1+8727
	.quad	_Minus__uMinus_param_u32_L
	.quad	_L_1+8745
	.quad	_Minus__Minus_var_u32_L
	.quad	_L_1+8764
	.quad	_Minus__uMinus_var_u32_L
	.quad	_L_1+8780
	.quad	_Minus__Minus_param_u32_i64
	.quad	_L_1+8797
	.quad	_Minus__uMinus_param_u32_i64
	.quad	_L_1+8817
	.quad	_Minus__Minus_var_u32_i64
	.quad	_L_1+8838
	.quad	_Minus__uMinus_var_u32_i64
	.quad	_L_1+8856
	.quad	_Minus__Minus_param_u32_u8
	.quad	_L_1+8875
	.quad	_Minus__uMinus_param_u32_u8
	.quad	_L_1+8894
	.quad	_Minus__Minus_var_u32_u8
	.quad	_L_1+8914
	.quad	_Minus__uMinus_var_u32_u8
	.quad	_L_1+8931
	.quad	_Minus__Minus_param_u32_C
	.quad	_L_1+8949
	.quad	_Minus__uMinus_param_u32_C
	.quad	_L_1+8967
	.quad	_Minus__Minus_var_u32_C
	.quad	_L_1+8986
	.quad	_Minus__uMinus_var_u32_C
	.quad	_L_1+9002
	.quad	_Minus__Minus_param_u32_i32
	.quad	_L_1+9019
	.quad	_Minus__uMinus_param_u32_i32
	.quad	_L_1+9039
	.quad	_Minus__Minus_var_u32_i32
	.quad	_L_1+9060
	.quad	_Minus__uMinus_var_u32_i32
	.quad	_L_1+9078
	.quad	_Minus__Minus_param_u32_u64
	.quad	_L_1+9097
	.quad	_Minus__uMinus_param_u32_u64
	.quad	_L_1+9117
	.quad	_Minus__Minus_var_u32_u64
	.quad	_L_1+9138
	.quad	_Minus__uMinus_var_u32_u64
	.quad	_L_1+9156
	.quad	_Minus__Minus_param_u32_i16
	.quad	_L_1+9175
	.quad	_Minus__uMinus_param_u32_i16
	.quad	_L_1+9195
	.quad	_Minus__Minus_var_u32_i16
	.quad	_L_1+9216
	.quad	_Minus__uMinus_var_u32_i16
	.quad	_L_1+9234
	.quad	_Minus__Minus_param_u32_I
	.quad	_L_1+9253
	.quad	_Minus__uMinus_param_u32_I
	.quad	_L_1+9271
	.quad	_Minus__Minus_var_u32_I
	.quad	_L_1+9290
	.quad	_Minus__uMinus_var_u32_I
	.quad	_L_1+9306
	.quad	_Minus__Minus_param_u32_LC
	.quad	_L_1+9323
	.quad	_Minus__uMinus_param_u32_LC
	.quad	_L_1+9342
	.quad	_Minus__Minus_var_u32_LC
	.quad	_L_1+9362
	.quad	_Minus__uMinus_var_u32_LC
	.quad	_L_1+9379
	.quad	_Minus__Minus_param_u32_u32
	.quad	_L_1+9397
	.quad	_Minus__uMinus_param_u32_u32
	.quad	_L_1+9417
	.quad	_Minus__Minus_var_u32_u32
	.quad	_L_1+9438
	.quad	_Minus__uMinus_var_u32_u32
	.quad	_L_1+9456
	.quad	_Minus__Minus_param_u32_u16
	.quad	_L_1+9475
	.quad	_Minus__uMinus_param_u32_u16
	.quad	_L_1+9495
	.quad	_Minus__Minus_var_u32_u16
	.quad	_L_1+9516
	.quad	_Minus__uMinus_var_u32_u16
	.quad	_L_1+9534
	.quad	_Minus__Minus_param_u16_i8
	.quad	_L_1+9553
	.quad	_Minus__uMinus_param_u16_i8
	.quad	_L_1+9572
	.quad	_Minus__Minus_var_u16_i8
	.quad	_L_1+9592
	.quad	_Minus__uMinus_var_u16_i8
	.quad	_L_1+9609
	.quad	_Minus__Minus_param_u16_L
	.quad	_L_1+9627
	.quad	_Minus__uMinus_param_u16_L
	.quad	_L_1+9645
	.quad	_Minus__Minus_var_u16_L
	.quad	_L_1+9664
	.quad	_Minus__uMinus_var_u16_L
	.quad	_L_1+9680
	.quad	_Minus__Minus_param_u16_i64
	.quad	_L_1+9697
	.quad	_Minus__uMinus_param_u16_i64
	.quad	_L_1+9717
	.quad	_Minus__Minus_var_u16_i64
	.quad	_L_1+9738
	.quad	_Minus__uMinus_var_u16_i64
	.quad	_L_1+9756
	.quad	_Minus__Minus_param_u16_u8
	.quad	_L_1+9775
	.quad	_Minus__uMinus_param_u16_u8
	.quad	_L_1+9794
	.quad	_Minus__Minus_var_u16_u8
	.quad	_L_1+9814
	.quad	_Minus__uMinus_var_u16_u8
	.quad	_L_1+9831
	.quad	_Minus__Minus_param_u16_C
	.quad	_L_1+9849
	.quad	_Minus__uMinus_param_u16_C
	.quad	_L_1+9867
	.quad	_Minus__Minus_var_u16_C
	.quad	_L_1+9886
	.quad	_Minus__uMinus_var_u16_C
	.quad	_L_1+9902
	.quad	_Minus__Minus_param_u16_i32
	.quad	_L_1+9919
	.quad	_Minus__uMinus_param_u16_i32
	.quad	_L_1+9939
	.quad	_Minus__Minus_var_u16_i32
	.quad	_L_1+9960
	.quad	_Minus__uMinus_var_u16_i32
	.quad	_L_1+9978
	.quad	_Minus__Minus_param_u16_u64
	.quad	_L_1+9997
	.quad	_Minus__uMinus_param_u16_u64
	.quad	_L_1+10017
	.quad	_Minus__Minus_var_u16_u64
	.quad	_L_1+10038
	.quad	_Minus__uMinus_var_u16_u64
	.quad	_L_1+10056
	.quad	_Minus__Minus_param_u16_i16
	.quad	_L_1+10075
	.quad	_Minus__uMinus_param_u16_i16
	.quad	_L_1+10095
	.quad	_Minus__Minus_var_u16_i16
	.quad	_L_1+10116
	.quad	_Minus__uMinus_var_u16_i16
	.quad	_L_1+10134
	.quad	_Minus__Minus_param_u16_I
	.quad	_L_1+10153
	.quad	_Minus__uMinus_param_u16_I
	.quad	_L_1+10171
	.quad	_Minus__Minus_var_u16_I
	.quad	_L_1+10190
	.quad	_Minus__uMinus_var_u16_I
	.quad	_L_1+10206
	.quad	_Minus__Minus_param_u16_LC
	.quad	_L_1+10223
	.quad	_Minus__uMinus_param_u16_LC
	.quad	_L_1+10242
	.quad	_Minus__Minus_var_u16_LC
	.quad	_L_1+10262
	.quad	_Minus__uMinus_var_u16_LC
	.quad	_L_1+10279
	.quad	_Minus__Minus_param_u16_u32
	.quad	_L_1+10297
	.quad	_Minus__uMinus_param_u16_u32
	.quad	_L_1+10317
	.quad	_Minus__Minus_var_u16_u32
	.quad	_L_1+10338
	.quad	_Minus__uMinus_var_u16_u32
	.quad	_L_1+10356
	.quad	_Minus__Minus_param_u16_u16
	.quad	_L_1+10375
	.quad	_Minus__uMinus_param_u16_u16
	.quad	_L_1+10395
	.quad	_Minus__Minus_var_u16_u16
	.quad	_L_1+10416
	.quad	_Minus__uMinus_var_u16_u16
	.quad	_L_1+10434
	.space 8
	.ascii "../AMD64_DARWIN/Minus.m3"
	.space 8
	.data
	.align 5
_MM_Minus:
	.quad	_L_1+19760
	.space 32
	.quad	_L_1+10456
	.space 24
	.quad	_MM_Minus+224
	.space 8
	.quad	_Minus_M3
	.quad	3
	.word	209
	.space 2
	.long	210
	.quad	211
	.quad	212
	.word	213
	.space 6
	.quad	214
	.long	2027224564
	.long	1080747753
	.long	217
	.space 4
	.quad	218
	.byte	-37
	.space 7
	.quad	220
	.long	1130182869
	.space 4
	.quad	223
	.byte	-32
	.space 31
	.quad	_Minus_I3
	.quad	_MM_Minus+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Minus+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Minus+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Minus+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Minus__uMinus_var_u16_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_u16.eh
_Minus__uMinus_param_u16_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_u16.eh
_Minus__uMinus_var_u16_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_u32.eh
_Minus__uMinus_param_u16_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_u32.eh
_Minus__uMinus_var_u16_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_LC.eh
_Minus__Minus_var_u16_LC.eh = 0
.no_dead_strip _Minus__Minus_var_u16_LC.eh
_Minus__uMinus_param_u16_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_LC.eh
_Minus__Minus_param_u16_LC.eh = 0
.no_dead_strip _Minus__Minus_param_u16_LC.eh
_Minus__uMinus_var_u16_I.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_I.eh
_Minus__uMinus_param_u16_I.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_I.eh
_Minus__uMinus_var_u16_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_i16.eh
_Minus__uMinus_param_u16_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_i16.eh
_Minus__uMinus_var_u16_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_u64.eh
_Minus__Minus_var_u16_u64.eh = 0
.no_dead_strip _Minus__Minus_var_u16_u64.eh
_Minus__uMinus_param_u16_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_u64.eh
_Minus__Minus_param_u16_u64.eh = 0
.no_dead_strip _Minus__Minus_param_u16_u64.eh
_Minus__uMinus_var_u16_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_i32.eh
_Minus__uMinus_param_u16_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_i32.eh
_Minus__uMinus_var_u16_C.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_C.eh
_Minus__uMinus_param_u16_C.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_C.eh
_Minus__uMinus_var_u16_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_u8.eh
_Minus__uMinus_param_u16_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_u8.eh
_Minus__uMinus_var_u16_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_i64.eh
_Minus__Minus_var_u16_i64.eh = 0
.no_dead_strip _Minus__Minus_var_u16_i64.eh
_Minus__uMinus_param_u16_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_i64.eh
_Minus__Minus_param_u16_i64.eh = 0
.no_dead_strip _Minus__Minus_param_u16_i64.eh
_Minus__uMinus_var_u16_L.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_L.eh
_Minus__Minus_var_u16_L.eh = 0
.no_dead_strip _Minus__Minus_var_u16_L.eh
_Minus__uMinus_param_u16_L.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_L.eh
_Minus__Minus_param_u16_L.eh = 0
.no_dead_strip _Minus__Minus_param_u16_L.eh
_Minus__uMinus_var_u16_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_u16_i8.eh
_Minus__uMinus_param_u16_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_u16_i8.eh
_Minus__uMinus_var_u32_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_u16.eh
_Minus__uMinus_param_u32_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_u16.eh
_Minus__uMinus_var_u32_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_u32.eh
_Minus__uMinus_param_u32_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_u32.eh
_Minus__uMinus_var_u32_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_LC.eh
_Minus__Minus_var_u32_LC.eh = 0
.no_dead_strip _Minus__Minus_var_u32_LC.eh
_Minus__uMinus_param_u32_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_LC.eh
_Minus__Minus_param_u32_LC.eh = 0
.no_dead_strip _Minus__Minus_param_u32_LC.eh
_Minus__uMinus_var_u32_I.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_I.eh
_Minus__uMinus_param_u32_I.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_I.eh
_Minus__uMinus_var_u32_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_i16.eh
_Minus__uMinus_param_u32_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_i16.eh
_Minus__uMinus_var_u32_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_u64.eh
_Minus__Minus_var_u32_u64.eh = 0
.no_dead_strip _Minus__Minus_var_u32_u64.eh
_Minus__uMinus_param_u32_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_u64.eh
_Minus__Minus_param_u32_u64.eh = 0
.no_dead_strip _Minus__Minus_param_u32_u64.eh
_Minus__uMinus_var_u32_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_i32.eh
_Minus__uMinus_param_u32_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_i32.eh
_Minus__uMinus_var_u32_C.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_C.eh
_Minus__uMinus_param_u32_C.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_C.eh
_Minus__uMinus_var_u32_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_u8.eh
_Minus__uMinus_param_u32_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_u8.eh
_Minus__uMinus_var_u32_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_i64.eh
_Minus__Minus_var_u32_i64.eh = 0
.no_dead_strip _Minus__Minus_var_u32_i64.eh
_Minus__uMinus_param_u32_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_i64.eh
_Minus__Minus_param_u32_i64.eh = 0
.no_dead_strip _Minus__Minus_param_u32_i64.eh
_Minus__uMinus_var_u32_L.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_L.eh
_Minus__Minus_var_u32_L.eh = 0
.no_dead_strip _Minus__Minus_var_u32_L.eh
_Minus__uMinus_param_u32_L.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_L.eh
_Minus__Minus_param_u32_L.eh = 0
.no_dead_strip _Minus__Minus_param_u32_L.eh
_Minus__uMinus_var_u32_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_u32_i8.eh
_Minus__uMinus_param_u32_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_u32_i8.eh
_Minus__uMinus_var_LC_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_u16.eh
_Minus__Minus_var_LC_u16.eh = 0
.no_dead_strip _Minus__Minus_var_LC_u16.eh
_Minus__uMinus_param_LC_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_u16.eh
_Minus__Minus_param_LC_u16.eh = 0
.no_dead_strip _Minus__Minus_param_LC_u16.eh
_Minus__uMinus_var_LC_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_u32.eh
_Minus__Minus_var_LC_u32.eh = 0
.no_dead_strip _Minus__Minus_var_LC_u32.eh
_Minus__uMinus_param_LC_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_u32.eh
_Minus__Minus_param_LC_u32.eh = 0
.no_dead_strip _Minus__Minus_param_LC_u32.eh
_Minus__uMinus_var_LC_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_LC.eh
_Minus__Minus_var_LC_LC.eh = 0
.no_dead_strip _Minus__Minus_var_LC_LC.eh
_Minus__uMinus_param_LC_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_LC.eh
_Minus__Minus_param_LC_LC.eh = 0
.no_dead_strip _Minus__Minus_param_LC_LC.eh
_Minus__uMinus_var_LC_I.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_I.eh
_Minus__Minus_var_LC_I.eh = 0
.no_dead_strip _Minus__Minus_var_LC_I.eh
_Minus__uMinus_param_LC_I.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_I.eh
_Minus__Minus_param_LC_I.eh = 0
.no_dead_strip _Minus__Minus_param_LC_I.eh
_Minus__uMinus_var_LC_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_i16.eh
_Minus__Minus_var_LC_i16.eh = 0
.no_dead_strip _Minus__Minus_var_LC_i16.eh
_Minus__uMinus_param_LC_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_i16.eh
_Minus__Minus_param_LC_i16.eh = 0
.no_dead_strip _Minus__Minus_param_LC_i16.eh
_Minus__uMinus_var_LC_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_u64.eh
_Minus__Minus_var_LC_u64.eh = 0
.no_dead_strip _Minus__Minus_var_LC_u64.eh
_Minus__uMinus_param_LC_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_u64.eh
_Minus__Minus_param_LC_u64.eh = 0
.no_dead_strip _Minus__Minus_param_LC_u64.eh
_Minus__uMinus_var_LC_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_i32.eh
_Minus__Minus_var_LC_i32.eh = 0
.no_dead_strip _Minus__Minus_var_LC_i32.eh
_Minus__uMinus_param_LC_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_i32.eh
_Minus__Minus_param_LC_i32.eh = 0
.no_dead_strip _Minus__Minus_param_LC_i32.eh
_Minus__uMinus_var_LC_C.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_C.eh
_Minus__Minus_var_LC_C.eh = 0
.no_dead_strip _Minus__Minus_var_LC_C.eh
_Minus__uMinus_param_LC_C.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_C.eh
_Minus__Minus_param_LC_C.eh = 0
.no_dead_strip _Minus__Minus_param_LC_C.eh
_Minus__uMinus_var_LC_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_u8.eh
_Minus__Minus_var_LC_u8.eh = 0
.no_dead_strip _Minus__Minus_var_LC_u8.eh
_Minus__uMinus_param_LC_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_u8.eh
_Minus__Minus_param_LC_u8.eh = 0
.no_dead_strip _Minus__Minus_param_LC_u8.eh
_Minus__uMinus_var_LC_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_i64.eh
_Minus__Minus_var_LC_i64.eh = 0
.no_dead_strip _Minus__Minus_var_LC_i64.eh
_Minus__uMinus_param_LC_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_i64.eh
_Minus__Minus_param_LC_i64.eh = 0
.no_dead_strip _Minus__Minus_param_LC_i64.eh
_Minus__uMinus_var_LC_L.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_L.eh
_Minus__Minus_var_LC_L.eh = 0
.no_dead_strip _Minus__Minus_var_LC_L.eh
_Minus__uMinus_param_LC_L.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_L.eh
_Minus__Minus_param_LC_L.eh = 0
.no_dead_strip _Minus__Minus_param_LC_L.eh
_Minus__uMinus_var_LC_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_LC_i8.eh
_Minus__Minus_var_LC_i8.eh = 0
.no_dead_strip _Minus__Minus_var_LC_i8.eh
_Minus__uMinus_param_LC_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_LC_i8.eh
_Minus__Minus_param_LC_i8.eh = 0
.no_dead_strip _Minus__Minus_param_LC_i8.eh
_Minus__uMinus_var_I_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_I_u16.eh
_Minus__Minus_var_I_u16.eh = 0
.no_dead_strip _Minus__Minus_var_I_u16.eh
_Minus__uMinus_param_I_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_I_u16.eh
_Minus__Minus_param_I_u16.eh = 0
.no_dead_strip _Minus__Minus_param_I_u16.eh
_Minus__uMinus_var_I_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_I_u32.eh
_Minus__Minus_var_I_u32.eh = 0
.no_dead_strip _Minus__Minus_var_I_u32.eh
_Minus__uMinus_param_I_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_I_u32.eh
_Minus__Minus_param_I_u32.eh = 0
.no_dead_strip _Minus__Minus_param_I_u32.eh
_Minus__uMinus_var_I_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_I_LC.eh
_Minus__Minus_var_I_LC.eh = 0
.no_dead_strip _Minus__Minus_var_I_LC.eh
_Minus__uMinus_param_I_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_I_LC.eh
_Minus__Minus_param_I_LC.eh = 0
.no_dead_strip _Minus__Minus_param_I_LC.eh
_Minus__uMinus_var_I_I.eh = 0
.no_dead_strip _Minus__uMinus_var_I_I.eh
_Minus__Minus_var_I_I.eh = 0
.no_dead_strip _Minus__Minus_var_I_I.eh
_Minus__uMinus_param_I_I.eh = 0
.no_dead_strip _Minus__uMinus_param_I_I.eh
_Minus__Minus_param_I_I.eh = 0
.no_dead_strip _Minus__Minus_param_I_I.eh
_Minus__uMinus_var_I_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_I_i16.eh
_Minus__Minus_var_I_i16.eh = 0
.no_dead_strip _Minus__Minus_var_I_i16.eh
_Minus__uMinus_param_I_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_I_i16.eh
_Minus__Minus_param_I_i16.eh = 0
.no_dead_strip _Minus__Minus_param_I_i16.eh
_Minus__uMinus_var_I_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_I_u64.eh
_Minus__Minus_var_I_u64.eh = 0
.no_dead_strip _Minus__Minus_var_I_u64.eh
_Minus__uMinus_param_I_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_I_u64.eh
_Minus__Minus_param_I_u64.eh = 0
.no_dead_strip _Minus__Minus_param_I_u64.eh
_Minus__uMinus_var_I_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_I_i32.eh
_Minus__Minus_var_I_i32.eh = 0
.no_dead_strip _Minus__Minus_var_I_i32.eh
_Minus__uMinus_param_I_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_I_i32.eh
_Minus__Minus_param_I_i32.eh = 0
.no_dead_strip _Minus__Minus_param_I_i32.eh
_Minus__uMinus_var_I_C.eh = 0
.no_dead_strip _Minus__uMinus_var_I_C.eh
_Minus__Minus_var_I_C.eh = 0
.no_dead_strip _Minus__Minus_var_I_C.eh
_Minus__uMinus_param_I_C.eh = 0
.no_dead_strip _Minus__uMinus_param_I_C.eh
_Minus__Minus_param_I_C.eh = 0
.no_dead_strip _Minus__Minus_param_I_C.eh
_Minus__uMinus_var_I_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_I_u8.eh
_Minus__Minus_var_I_u8.eh = 0
.no_dead_strip _Minus__Minus_var_I_u8.eh
_Minus__uMinus_param_I_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_I_u8.eh
_Minus__Minus_param_I_u8.eh = 0
.no_dead_strip _Minus__Minus_param_I_u8.eh
_Minus__uMinus_var_I_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_I_i64.eh
_Minus__Minus_var_I_i64.eh = 0
.no_dead_strip _Minus__Minus_var_I_i64.eh
_Minus__uMinus_param_I_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_I_i64.eh
_Minus__Minus_param_I_i64.eh = 0
.no_dead_strip _Minus__Minus_param_I_i64.eh
_Minus__uMinus_var_I_L.eh = 0
.no_dead_strip _Minus__uMinus_var_I_L.eh
_Minus__Minus_var_I_L.eh = 0
.no_dead_strip _Minus__Minus_var_I_L.eh
_Minus__uMinus_param_I_L.eh = 0
.no_dead_strip _Minus__uMinus_param_I_L.eh
_Minus__Minus_param_I_L.eh = 0
.no_dead_strip _Minus__Minus_param_I_L.eh
_Minus__uMinus_var_I_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_I_i8.eh
_Minus__Minus_var_I_i8.eh = 0
.no_dead_strip _Minus__Minus_var_I_i8.eh
_Minus__uMinus_param_I_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_I_i8.eh
_Minus__Minus_param_I_i8.eh = 0
.no_dead_strip _Minus__Minus_param_I_i8.eh
_Minus__uMinus_var_i16_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_u16.eh
_Minus__uMinus_param_i16_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_u16.eh
_Minus__uMinus_var_i16_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_u32.eh
_Minus__uMinus_param_i16_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_u32.eh
_Minus__uMinus_var_i16_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_LC.eh
_Minus__Minus_var_i16_LC.eh = 0
.no_dead_strip _Minus__Minus_var_i16_LC.eh
_Minus__uMinus_param_i16_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_LC.eh
_Minus__Minus_param_i16_LC.eh = 0
.no_dead_strip _Minus__Minus_param_i16_LC.eh
_Minus__uMinus_var_i16_I.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_I.eh
_Minus__uMinus_param_i16_I.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_I.eh
_Minus__uMinus_var_i16_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_i16.eh
_Minus__uMinus_param_i16_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_i16.eh
_Minus__uMinus_var_i16_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_u64.eh
_Minus__Minus_var_i16_u64.eh = 0
.no_dead_strip _Minus__Minus_var_i16_u64.eh
_Minus__uMinus_param_i16_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_u64.eh
_Minus__Minus_param_i16_u64.eh = 0
.no_dead_strip _Minus__Minus_param_i16_u64.eh
_Minus__uMinus_var_i16_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_i32.eh
_Minus__uMinus_param_i16_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_i32.eh
_Minus__uMinus_var_i16_C.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_C.eh
_Minus__uMinus_param_i16_C.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_C.eh
_Minus__uMinus_var_i16_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_u8.eh
_Minus__uMinus_param_i16_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_u8.eh
_Minus__uMinus_var_i16_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_i64.eh
_Minus__Minus_var_i16_i64.eh = 0
.no_dead_strip _Minus__Minus_var_i16_i64.eh
_Minus__uMinus_param_i16_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_i64.eh
_Minus__Minus_param_i16_i64.eh = 0
.no_dead_strip _Minus__Minus_param_i16_i64.eh
_Minus__uMinus_var_i16_L.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_L.eh
_Minus__Minus_var_i16_L.eh = 0
.no_dead_strip _Minus__Minus_var_i16_L.eh
_Minus__uMinus_param_i16_L.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_L.eh
_Minus__Minus_param_i16_L.eh = 0
.no_dead_strip _Minus__Minus_param_i16_L.eh
_Minus__uMinus_var_i16_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_i16_i8.eh
_Minus__uMinus_param_i16_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_i16_i8.eh
_Minus__uMinus_var_u64_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_u16.eh
_Minus__Minus_var_u64_u16.eh = 0
.no_dead_strip _Minus__Minus_var_u64_u16.eh
_Minus__uMinus_param_u64_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_u16.eh
_Minus__Minus_param_u64_u16.eh = 0
.no_dead_strip _Minus__Minus_param_u64_u16.eh
_Minus__uMinus_var_u64_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_u32.eh
_Minus__Minus_var_u64_u32.eh = 0
.no_dead_strip _Minus__Minus_var_u64_u32.eh
_Minus__uMinus_param_u64_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_u32.eh
_Minus__Minus_param_u64_u32.eh = 0
.no_dead_strip _Minus__Minus_param_u64_u32.eh
_Minus__uMinus_var_u64_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_LC.eh
_Minus__Minus_var_u64_LC.eh = 0
.no_dead_strip _Minus__Minus_var_u64_LC.eh
_Minus__uMinus_param_u64_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_LC.eh
_Minus__Minus_param_u64_LC.eh = 0
.no_dead_strip _Minus__Minus_param_u64_LC.eh
_Minus__uMinus_var_u64_I.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_I.eh
_Minus__Minus_var_u64_I.eh = 0
.no_dead_strip _Minus__Minus_var_u64_I.eh
_Minus__uMinus_param_u64_I.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_I.eh
_Minus__Minus_param_u64_I.eh = 0
.no_dead_strip _Minus__Minus_param_u64_I.eh
_Minus__uMinus_var_u64_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_i16.eh
_Minus__Minus_var_u64_i16.eh = 0
.no_dead_strip _Minus__Minus_var_u64_i16.eh
_Minus__uMinus_param_u64_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_i16.eh
_Minus__Minus_param_u64_i16.eh = 0
.no_dead_strip _Minus__Minus_param_u64_i16.eh
_Minus__uMinus_var_u64_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_u64.eh
_Minus__Minus_var_u64_u64.eh = 0
.no_dead_strip _Minus__Minus_var_u64_u64.eh
_Minus__uMinus_param_u64_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_u64.eh
_Minus__Minus_param_u64_u64.eh = 0
.no_dead_strip _Minus__Minus_param_u64_u64.eh
_Minus__uMinus_var_u64_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_i32.eh
_Minus__Minus_var_u64_i32.eh = 0
.no_dead_strip _Minus__Minus_var_u64_i32.eh
_Minus__uMinus_param_u64_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_i32.eh
_Minus__Minus_param_u64_i32.eh = 0
.no_dead_strip _Minus__Minus_param_u64_i32.eh
_Minus__uMinus_var_u64_C.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_C.eh
_Minus__Minus_var_u64_C.eh = 0
.no_dead_strip _Minus__Minus_var_u64_C.eh
_Minus__uMinus_param_u64_C.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_C.eh
_Minus__Minus_param_u64_C.eh = 0
.no_dead_strip _Minus__Minus_param_u64_C.eh
_Minus__uMinus_var_u64_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_u8.eh
_Minus__Minus_var_u64_u8.eh = 0
.no_dead_strip _Minus__Minus_var_u64_u8.eh
_Minus__uMinus_param_u64_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_u8.eh
_Minus__Minus_param_u64_u8.eh = 0
.no_dead_strip _Minus__Minus_param_u64_u8.eh
_Minus__uMinus_var_u64_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_i64.eh
_Minus__Minus_var_u64_i64.eh = 0
.no_dead_strip _Minus__Minus_var_u64_i64.eh
_Minus__uMinus_param_u64_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_i64.eh
_Minus__Minus_param_u64_i64.eh = 0
.no_dead_strip _Minus__Minus_param_u64_i64.eh
_Minus__uMinus_var_u64_L.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_L.eh
_Minus__Minus_var_u64_L.eh = 0
.no_dead_strip _Minus__Minus_var_u64_L.eh
_Minus__uMinus_param_u64_L.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_L.eh
_Minus__Minus_param_u64_L.eh = 0
.no_dead_strip _Minus__Minus_param_u64_L.eh
_Minus__uMinus_var_u64_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_u64_i8.eh
_Minus__Minus_var_u64_i8.eh = 0
.no_dead_strip _Minus__Minus_var_u64_i8.eh
_Minus__uMinus_param_u64_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_u64_i8.eh
_Minus__Minus_param_u64_i8.eh = 0
.no_dead_strip _Minus__Minus_param_u64_i8.eh
_Minus__Minus_var_f64_f64.eh = 0
.no_dead_strip _Minus__Minus_var_f64_f64.eh
_Minus__Minus_param_f64_f64.eh = 0
.no_dead_strip _Minus__Minus_param_f64_f64.eh
_Minus__uMinus_var_i32_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_u16.eh
_Minus__uMinus_param_i32_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_u16.eh
_Minus__uMinus_var_i32_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_u32.eh
_Minus__uMinus_param_i32_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_u32.eh
_Minus__uMinus_var_i32_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_LC.eh
_Minus__Minus_var_i32_LC.eh = 0
.no_dead_strip _Minus__Minus_var_i32_LC.eh
_Minus__uMinus_param_i32_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_LC.eh
_Minus__Minus_param_i32_LC.eh = 0
.no_dead_strip _Minus__Minus_param_i32_LC.eh
_Minus__uMinus_var_i32_I.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_I.eh
_Minus__uMinus_param_i32_I.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_I.eh
_Minus__uMinus_var_i32_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_i16.eh
_Minus__uMinus_param_i32_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_i16.eh
_Minus__uMinus_var_i32_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_u64.eh
_Minus__Minus_var_i32_u64.eh = 0
.no_dead_strip _Minus__Minus_var_i32_u64.eh
_Minus__uMinus_param_i32_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_u64.eh
_Minus__Minus_param_i32_u64.eh = 0
.no_dead_strip _Minus__Minus_param_i32_u64.eh
_Minus__uMinus_var_i32_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_i32.eh
_Minus__uMinus_param_i32_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_i32.eh
_Minus__uMinus_var_i32_C.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_C.eh
_Minus__uMinus_param_i32_C.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_C.eh
_Minus__uMinus_var_i32_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_u8.eh
_Minus__uMinus_param_i32_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_u8.eh
_Minus__uMinus_var_i32_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_i64.eh
_Minus__Minus_var_i32_i64.eh = 0
.no_dead_strip _Minus__Minus_var_i32_i64.eh
_Minus__uMinus_param_i32_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_i64.eh
_Minus__Minus_param_i32_i64.eh = 0
.no_dead_strip _Minus__Minus_param_i32_i64.eh
_Minus__uMinus_var_i32_L.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_L.eh
_Minus__Minus_var_i32_L.eh = 0
.no_dead_strip _Minus__Minus_var_i32_L.eh
_Minus__uMinus_param_i32_L.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_L.eh
_Minus__Minus_param_i32_L.eh = 0
.no_dead_strip _Minus__Minus_param_i32_L.eh
_Minus__uMinus_var_i32_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_i32_i8.eh
_Minus__uMinus_param_i32_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_i32_i8.eh
_Minus__uMinus_var_C_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_C_u16.eh
_Minus__uMinus_param_C_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_C_u16.eh
_Minus__uMinus_var_C_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_C_u32.eh
_Minus__uMinus_param_C_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_C_u32.eh
_Minus__uMinus_var_C_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_C_LC.eh
_Minus__Minus_var_C_LC.eh = 0
.no_dead_strip _Minus__Minus_var_C_LC.eh
_Minus__uMinus_param_C_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_C_LC.eh
_Minus__Minus_param_C_LC.eh = 0
.no_dead_strip _Minus__Minus_param_C_LC.eh
_Minus__uMinus_var_C_I.eh = 0
.no_dead_strip _Minus__uMinus_var_C_I.eh
_Minus__uMinus_param_C_I.eh = 0
.no_dead_strip _Minus__uMinus_param_C_I.eh
_Minus__uMinus_var_C_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_C_i16.eh
_Minus__uMinus_param_C_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_C_i16.eh
_Minus__uMinus_var_C_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_C_u64.eh
_Minus__Minus_var_C_u64.eh = 0
.no_dead_strip _Minus__Minus_var_C_u64.eh
_Minus__uMinus_param_C_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_C_u64.eh
_Minus__Minus_param_C_u64.eh = 0
.no_dead_strip _Minus__Minus_param_C_u64.eh
_Minus__uMinus_var_C_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_C_i32.eh
_Minus__uMinus_param_C_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_C_i32.eh
_Minus__uMinus_var_C_C.eh = 0
.no_dead_strip _Minus__uMinus_var_C_C.eh
_Minus__uMinus_param_C_C.eh = 0
.no_dead_strip _Minus__uMinus_param_C_C.eh
_Minus__uMinus_var_C_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_C_u8.eh
_Minus__uMinus_param_C_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_C_u8.eh
_Minus__uMinus_var_C_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_C_i64.eh
_Minus__Minus_var_C_i64.eh = 0
.no_dead_strip _Minus__Minus_var_C_i64.eh
_Minus__uMinus_param_C_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_C_i64.eh
_Minus__Minus_param_C_i64.eh = 0
.no_dead_strip _Minus__Minus_param_C_i64.eh
_Minus__uMinus_var_C_L.eh = 0
.no_dead_strip _Minus__uMinus_var_C_L.eh
_Minus__Minus_var_C_L.eh = 0
.no_dead_strip _Minus__Minus_var_C_L.eh
_Minus__uMinus_param_C_L.eh = 0
.no_dead_strip _Minus__uMinus_param_C_L.eh
_Minus__Minus_param_C_L.eh = 0
.no_dead_strip _Minus__Minus_param_C_L.eh
_Minus__uMinus_var_C_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_C_i8.eh
_Minus__uMinus_param_C_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_C_i8.eh
_Minus__uMinus_var_u8_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_u16.eh
_Minus__uMinus_param_u8_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_u16.eh
_Minus__uMinus_var_u8_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_u32.eh
_Minus__uMinus_param_u8_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_u32.eh
_Minus__uMinus_var_u8_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_LC.eh
_Minus__Minus_var_u8_LC.eh = 0
.no_dead_strip _Minus__Minus_var_u8_LC.eh
_Minus__uMinus_param_u8_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_LC.eh
_Minus__Minus_param_u8_LC.eh = 0
.no_dead_strip _Minus__Minus_param_u8_LC.eh
_Minus__uMinus_var_u8_I.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_I.eh
_Minus__uMinus_param_u8_I.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_I.eh
_Minus__uMinus_var_u8_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_i16.eh
_Minus__uMinus_param_u8_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_i16.eh
_Minus__uMinus_var_u8_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_u64.eh
_Minus__Minus_var_u8_u64.eh = 0
.no_dead_strip _Minus__Minus_var_u8_u64.eh
_Minus__uMinus_param_u8_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_u64.eh
_Minus__Minus_param_u8_u64.eh = 0
.no_dead_strip _Minus__Minus_param_u8_u64.eh
_Minus__uMinus_var_u8_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_i32.eh
_Minus__uMinus_param_u8_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_i32.eh
_Minus__uMinus_var_u8_C.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_C.eh
_Minus__uMinus_param_u8_C.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_C.eh
_Minus__uMinus_var_u8_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_u8.eh
_Minus__uMinus_param_u8_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_u8.eh
_Minus__uMinus_var_u8_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_i64.eh
_Minus__Minus_var_u8_i64.eh = 0
.no_dead_strip _Minus__Minus_var_u8_i64.eh
_Minus__uMinus_param_u8_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_i64.eh
_Minus__Minus_param_u8_i64.eh = 0
.no_dead_strip _Minus__Minus_param_u8_i64.eh
_Minus__uMinus_var_u8_L.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_L.eh
_Minus__Minus_var_u8_L.eh = 0
.no_dead_strip _Minus__Minus_var_u8_L.eh
_Minus__uMinus_param_u8_L.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_L.eh
_Minus__Minus_param_u8_L.eh = 0
.no_dead_strip _Minus__Minus_param_u8_L.eh
_Minus__uMinus_var_u8_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_u8_i8.eh
_Minus__uMinus_param_u8_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_u8_i8.eh
_Minus__uMinus_var_i64_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_u16.eh
_Minus__Minus_var_i64_u16.eh = 0
.no_dead_strip _Minus__Minus_var_i64_u16.eh
_Minus__uMinus_param_i64_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_u16.eh
_Minus__Minus_param_i64_u16.eh = 0
.no_dead_strip _Minus__Minus_param_i64_u16.eh
_Minus__uMinus_var_i64_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_u32.eh
_Minus__Minus_var_i64_u32.eh = 0
.no_dead_strip _Minus__Minus_var_i64_u32.eh
_Minus__uMinus_param_i64_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_u32.eh
_Minus__Minus_param_i64_u32.eh = 0
.no_dead_strip _Minus__Minus_param_i64_u32.eh
_Minus__uMinus_var_i64_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_LC.eh
_Minus__Minus_var_i64_LC.eh = 0
.no_dead_strip _Minus__Minus_var_i64_LC.eh
_Minus__uMinus_param_i64_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_LC.eh
_Minus__Minus_param_i64_LC.eh = 0
.no_dead_strip _Minus__Minus_param_i64_LC.eh
_Minus__uMinus_var_i64_I.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_I.eh
_Minus__Minus_var_i64_I.eh = 0
.no_dead_strip _Minus__Minus_var_i64_I.eh
_Minus__uMinus_param_i64_I.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_I.eh
_Minus__Minus_param_i64_I.eh = 0
.no_dead_strip _Minus__Minus_param_i64_I.eh
_Minus__uMinus_var_i64_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_i16.eh
_Minus__Minus_var_i64_i16.eh = 0
.no_dead_strip _Minus__Minus_var_i64_i16.eh
_Minus__uMinus_param_i64_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_i16.eh
_Minus__Minus_param_i64_i16.eh = 0
.no_dead_strip _Minus__Minus_param_i64_i16.eh
_Minus__uMinus_var_i64_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_u64.eh
_Minus__Minus_var_i64_u64.eh = 0
.no_dead_strip _Minus__Minus_var_i64_u64.eh
_Minus__uMinus_param_i64_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_u64.eh
_Minus__Minus_param_i64_u64.eh = 0
.no_dead_strip _Minus__Minus_param_i64_u64.eh
_Minus__uMinus_var_i64_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_i32.eh
_Minus__Minus_var_i64_i32.eh = 0
.no_dead_strip _Minus__Minus_var_i64_i32.eh
_Minus__uMinus_param_i64_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_i32.eh
_Minus__Minus_param_i64_i32.eh = 0
.no_dead_strip _Minus__Minus_param_i64_i32.eh
_Minus__uMinus_var_i64_C.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_C.eh
_Minus__Minus_var_i64_C.eh = 0
.no_dead_strip _Minus__Minus_var_i64_C.eh
_Minus__uMinus_param_i64_C.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_C.eh
_Minus__Minus_param_i64_C.eh = 0
.no_dead_strip _Minus__Minus_param_i64_C.eh
_Minus__uMinus_var_i64_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_u8.eh
_Minus__Minus_var_i64_u8.eh = 0
.no_dead_strip _Minus__Minus_var_i64_u8.eh
_Minus__uMinus_param_i64_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_u8.eh
_Minus__Minus_param_i64_u8.eh = 0
.no_dead_strip _Minus__Minus_param_i64_u8.eh
_Minus__uMinus_var_i64_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_i64.eh
_Minus__Minus_var_i64_i64.eh = 0
.no_dead_strip _Minus__Minus_var_i64_i64.eh
_Minus__uMinus_param_i64_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_i64.eh
_Minus__Minus_param_i64_i64.eh = 0
.no_dead_strip _Minus__Minus_param_i64_i64.eh
_Minus__uMinus_var_i64_L.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_L.eh
_Minus__Minus_var_i64_L.eh = 0
.no_dead_strip _Minus__Minus_var_i64_L.eh
_Minus__uMinus_param_i64_L.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_L.eh
_Minus__Minus_param_i64_L.eh = 0
.no_dead_strip _Minus__Minus_param_i64_L.eh
_Minus__uMinus_var_i64_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_i64_i8.eh
_Minus__Minus_var_i64_i8.eh = 0
.no_dead_strip _Minus__Minus_var_i64_i8.eh
_Minus__uMinus_param_i64_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_i64_i8.eh
_Minus__Minus_param_i64_i8.eh = 0
.no_dead_strip _Minus__Minus_param_i64_i8.eh
_Minus__Minus_var_f32_f32.eh = 0
.no_dead_strip _Minus__Minus_var_f32_f32.eh
_Minus__Minus_param_f32_f32.eh = 0
.no_dead_strip _Minus__Minus_param_f32_f32.eh
_Minus__uMinus_var_L_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_L_u16.eh
_Minus__Minus_var_L_u16.eh = 0
.no_dead_strip _Minus__Minus_var_L_u16.eh
_Minus__uMinus_param_L_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_L_u16.eh
_Minus__Minus_param_L_u16.eh = 0
.no_dead_strip _Minus__Minus_param_L_u16.eh
_Minus__uMinus_var_L_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_L_u32.eh
_Minus__Minus_var_L_u32.eh = 0
.no_dead_strip _Minus__Minus_var_L_u32.eh
_Minus__uMinus_param_L_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_L_u32.eh
_Minus__Minus_param_L_u32.eh = 0
.no_dead_strip _Minus__Minus_param_L_u32.eh
_Minus__uMinus_var_L_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_L_LC.eh
_Minus__Minus_var_L_LC.eh = 0
.no_dead_strip _Minus__Minus_var_L_LC.eh
_Minus__uMinus_param_L_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_L_LC.eh
_Minus__Minus_param_L_LC.eh = 0
.no_dead_strip _Minus__Minus_param_L_LC.eh
_Minus__uMinus_var_L_I.eh = 0
.no_dead_strip _Minus__uMinus_var_L_I.eh
_Minus__Minus_var_L_I.eh = 0
.no_dead_strip _Minus__Minus_var_L_I.eh
_Minus__uMinus_param_L_I.eh = 0
.no_dead_strip _Minus__uMinus_param_L_I.eh
_Minus__Minus_param_L_I.eh = 0
.no_dead_strip _Minus__Minus_param_L_I.eh
_Minus__uMinus_var_L_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_L_i16.eh
_Minus__Minus_var_L_i16.eh = 0
.no_dead_strip _Minus__Minus_var_L_i16.eh
_Minus__uMinus_param_L_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_L_i16.eh
_Minus__Minus_param_L_i16.eh = 0
.no_dead_strip _Minus__Minus_param_L_i16.eh
_Minus__uMinus_var_L_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_L_u64.eh
_Minus__Minus_var_L_u64.eh = 0
.no_dead_strip _Minus__Minus_var_L_u64.eh
_Minus__uMinus_param_L_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_L_u64.eh
_Minus__Minus_param_L_u64.eh = 0
.no_dead_strip _Minus__Minus_param_L_u64.eh
_Minus__uMinus_var_L_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_L_i32.eh
_Minus__Minus_var_L_i32.eh = 0
.no_dead_strip _Minus__Minus_var_L_i32.eh
_Minus__uMinus_param_L_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_L_i32.eh
_Minus__Minus_param_L_i32.eh = 0
.no_dead_strip _Minus__Minus_param_L_i32.eh
_Minus__uMinus_var_L_C.eh = 0
.no_dead_strip _Minus__uMinus_var_L_C.eh
_Minus__Minus_var_L_C.eh = 0
.no_dead_strip _Minus__Minus_var_L_C.eh
_Minus__uMinus_param_L_C.eh = 0
.no_dead_strip _Minus__uMinus_param_L_C.eh
_Minus__Minus_param_L_C.eh = 0
.no_dead_strip _Minus__Minus_param_L_C.eh
_Minus__uMinus_var_L_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_L_u8.eh
_Minus__Minus_var_L_u8.eh = 0
.no_dead_strip _Minus__Minus_var_L_u8.eh
_Minus__uMinus_param_L_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_L_u8.eh
_Minus__Minus_param_L_u8.eh = 0
.no_dead_strip _Minus__Minus_param_L_u8.eh
_Minus__uMinus_var_L_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_L_i64.eh
_Minus__Minus_var_L_i64.eh = 0
.no_dead_strip _Minus__Minus_var_L_i64.eh
_Minus__uMinus_param_L_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_L_i64.eh
_Minus__Minus_param_L_i64.eh = 0
.no_dead_strip _Minus__Minus_param_L_i64.eh
_Minus__uMinus_var_L_L.eh = 0
.no_dead_strip _Minus__uMinus_var_L_L.eh
_Minus__Minus_var_L_L.eh = 0
.no_dead_strip _Minus__Minus_var_L_L.eh
_Minus__uMinus_param_L_L.eh = 0
.no_dead_strip _Minus__uMinus_param_L_L.eh
_Minus__Minus_param_L_L.eh = 0
.no_dead_strip _Minus__Minus_param_L_L.eh
_Minus__uMinus_var_L_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_L_i8.eh
_Minus__Minus_var_L_i8.eh = 0
.no_dead_strip _Minus__Minus_var_L_i8.eh
_Minus__uMinus_param_L_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_L_i8.eh
_Minus__Minus_param_L_i8.eh = 0
.no_dead_strip _Minus__Minus_param_L_i8.eh
_Minus__uMinus_var_i8_u16.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_u16.eh
_Minus__uMinus_param_i8_u16.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_u16.eh
_Minus__uMinus_var_i8_u32.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_u32.eh
_Minus__uMinus_param_i8_u32.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_u32.eh
_Minus__uMinus_var_i8_LC.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_LC.eh
_Minus__Minus_var_i8_LC.eh = 0
.no_dead_strip _Minus__Minus_var_i8_LC.eh
_Minus__uMinus_param_i8_LC.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_LC.eh
_Minus__Minus_param_i8_LC.eh = 0
.no_dead_strip _Minus__Minus_param_i8_LC.eh
_Minus__uMinus_var_i8_I.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_I.eh
_Minus__uMinus_param_i8_I.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_I.eh
_Minus__uMinus_var_i8_i16.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_i16.eh
_Minus__uMinus_param_i8_i16.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_i16.eh
_Minus__uMinus_var_i8_u64.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_u64.eh
_Minus__Minus_var_i8_u64.eh = 0
.no_dead_strip _Minus__Minus_var_i8_u64.eh
_Minus__uMinus_param_i8_u64.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_u64.eh
_Minus__Minus_param_i8_u64.eh = 0
.no_dead_strip _Minus__Minus_param_i8_u64.eh
_Minus__uMinus_var_i8_i32.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_i32.eh
_Minus__uMinus_param_i8_i32.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_i32.eh
_Minus__uMinus_var_i8_C.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_C.eh
_Minus__uMinus_param_i8_C.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_C.eh
_Minus__uMinus_var_i8_u8.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_u8.eh
_Minus__uMinus_param_i8_u8.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_u8.eh
_Minus__uMinus_var_i8_i64.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_i64.eh
_Minus__Minus_var_i8_i64.eh = 0
.no_dead_strip _Minus__Minus_var_i8_i64.eh
_Minus__uMinus_param_i8_i64.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_i64.eh
_Minus__Minus_param_i8_i64.eh = 0
.no_dead_strip _Minus__Minus_param_i8_i64.eh
_Minus__uMinus_var_i8_L.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_L.eh
_Minus__Minus_var_i8_L.eh = 0
.no_dead_strip _Minus__Minus_var_i8_L.eh
_Minus__uMinus_param_i8_L.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_L.eh
_Minus__Minus_param_i8_L.eh = 0
.no_dead_strip _Minus__Minus_param_i8_L.eh
_Minus__uMinus_var_i8_i8.eh = 0
.no_dead_strip _Minus__uMinus_var_i8_i8.eh
_Minus__uMinus_param_i8_i8.eh = 0
.no_dead_strip _Minus__uMinus_param_i8_i8.eh
.globl _Minus_M3.eh
_Minus_M3.eh = 0
.no_dead_strip _Minus_M3.eh
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0x0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
_Minus__Minus_var_u16_u16.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB1-.
	.set L$set$2,LFE1-LFB1
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI3-LFB1
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI4-LCFI3
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$5,LCFI5-LCFI4
	.long L$set$5
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE1:
_Minus__Minus_param_u16_u16.eh:
LSFDE3:
	.set L$set$6,LEFDE3-LASFDE3
	.long L$set$6
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$7,LFE3-LFB3
	.quad L$set$7
	.byte	0x0
	.byte	0x4
	.set L$set$8,LCFI9-LFB3
	.long L$set$8
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$9,LCFI10-LCFI9
	.long L$set$9
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$10,LCFI12-LCFI10
	.long L$set$10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE3:
_Minus__Minus_var_u16_u32.eh:
LSFDE5:
	.set L$set$11,LEFDE5-LASFDE5
	.long L$set$11
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB5-.
	.set L$set$12,LFE5-LFB5
	.quad L$set$12
	.byte	0x0
	.byte	0x4
	.set L$set$13,LCFI16-LFB5
	.long L$set$13
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$14,LCFI17-LCFI16
	.long L$set$14
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$15,LCFI18-LCFI17
	.long L$set$15
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE5:
_Minus__Minus_param_u16_u32.eh:
LSFDE7:
	.set L$set$16,LEFDE7-LASFDE7
	.long L$set$16
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB7-.
	.set L$set$17,LFE7-LFB7
	.quad L$set$17
	.byte	0x0
	.byte	0x4
	.set L$set$18,LCFI22-LFB7
	.long L$set$18
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$19,LCFI23-LCFI22
	.long L$set$19
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$20,LCFI25-LCFI23
	.long L$set$20
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE7:
_Minus__Minus_var_u16_I.eh:
LSFDE9:
	.set L$set$21,LEFDE9-LASFDE9
	.long L$set$21
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB13-.
	.set L$set$22,LFE13-LFB13
	.quad L$set$22
	.byte	0x0
	.byte	0x4
	.set L$set$23,LCFI41-LFB13
	.long L$set$23
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$24,LCFI42-LCFI41
	.long L$set$24
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$25,LCFI43-LCFI42
	.long L$set$25
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE9:
_Minus__Minus_param_u16_I.eh:
LSFDE11:
	.set L$set$26,LEFDE11-LASFDE11
	.long L$set$26
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB15-.
	.set L$set$27,LFE15-LFB15
	.quad L$set$27
	.byte	0x0
	.byte	0x4
	.set L$set$28,LCFI47-LFB15
	.long L$set$28
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$29,LCFI48-LCFI47
	.long L$set$29
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$30,LCFI50-LCFI48
	.long L$set$30
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE11:
_Minus__Minus_var_u16_i16.eh:
LSFDE13:
	.set L$set$31,LEFDE13-LASFDE13
	.long L$set$31
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB17-.
	.set L$set$32,LFE17-LFB17
	.quad L$set$32
	.byte	0x0
	.byte	0x4
	.set L$set$33,LCFI54-LFB17
	.long L$set$33
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$34,LCFI55-LCFI54
	.long L$set$34
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$35,LCFI56-LCFI55
	.long L$set$35
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE13:
_Minus__Minus_param_u16_i16.eh:
LSFDE15:
	.set L$set$36,LEFDE15-LASFDE15
	.long L$set$36
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB19-.
	.set L$set$37,LFE19-LFB19
	.quad L$set$37
	.byte	0x0
	.byte	0x4
	.set L$set$38,LCFI60-LFB19
	.long L$set$38
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$39,LCFI61-LCFI60
	.long L$set$39
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$40,LCFI63-LCFI61
	.long L$set$40
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE15:
_Minus__Minus_var_u16_i32.eh:
LSFDE17:
	.set L$set$41,LEFDE17-LASFDE17
	.long L$set$41
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB25-.
	.set L$set$42,LFE25-LFB25
	.quad L$set$42
	.byte	0x0
	.byte	0x4
	.set L$set$43,LCFI79-LFB25
	.long L$set$43
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$44,LCFI80-LCFI79
	.long L$set$44
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$45,LCFI81-LCFI80
	.long L$set$45
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE17:
_Minus__Minus_param_u16_i32.eh:
LSFDE19:
	.set L$set$46,LEFDE19-LASFDE19
	.long L$set$46
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB27-.
	.set L$set$47,LFE27-LFB27
	.quad L$set$47
	.byte	0x0
	.byte	0x4
	.set L$set$48,LCFI85-LFB27
	.long L$set$48
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$49,LCFI86-LCFI85
	.long L$set$49
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$50,LCFI88-LCFI86
	.long L$set$50
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE19:
_Minus__Minus_var_u16_C.eh:
LSFDE21:
	.set L$set$51,LEFDE21-LASFDE21
	.long L$set$51
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB29-.
	.set L$set$52,LFE29-LFB29
	.quad L$set$52
	.byte	0x0
	.byte	0x4
	.set L$set$53,LCFI92-LFB29
	.long L$set$53
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$54,LCFI93-LCFI92
	.long L$set$54
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$55,LCFI94-LCFI93
	.long L$set$55
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE21:
_Minus__Minus_param_u16_C.eh:
LSFDE23:
	.set L$set$56,LEFDE23-LASFDE23
	.long L$set$56
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB31-.
	.set L$set$57,LFE31-LFB31
	.quad L$set$57
	.byte	0x0
	.byte	0x4
	.set L$set$58,LCFI98-LFB31
	.long L$set$58
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$59,LCFI99-LCFI98
	.long L$set$59
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$60,LCFI101-LCFI99
	.long L$set$60
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE23:
_Minus__Minus_var_u16_u8.eh:
LSFDE25:
	.set L$set$61,LEFDE25-LASFDE25
	.long L$set$61
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB33-.
	.set L$set$62,LFE33-LFB33
	.quad L$set$62
	.byte	0x0
	.byte	0x4
	.set L$set$63,LCFI105-LFB33
	.long L$set$63
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$64,LCFI106-LCFI105
	.long L$set$64
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$65,LCFI107-LCFI106
	.long L$set$65
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE25:
_Minus__Minus_param_u16_u8.eh:
LSFDE27:
	.set L$set$66,LEFDE27-LASFDE27
	.long L$set$66
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB35-.
	.set L$set$67,LFE35-LFB35
	.quad L$set$67
	.byte	0x0
	.byte	0x4
	.set L$set$68,LCFI111-LFB35
	.long L$set$68
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$69,LCFI112-LCFI111
	.long L$set$69
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$70,LCFI114-LCFI112
	.long L$set$70
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE27:
_Minus__Minus_var_u16_i8.eh:
LSFDE29:
	.set L$set$71,LEFDE29-LASFDE29
	.long L$set$71
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB45-.
	.set L$set$72,LFE45-LFB45
	.quad L$set$72
	.byte	0x0
	.byte	0x4
	.set L$set$73,LCFI142-LFB45
	.long L$set$73
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$74,LCFI143-LCFI142
	.long L$set$74
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$75,LCFI144-LCFI143
	.long L$set$75
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE29:
_Minus__Minus_param_u16_i8.eh:
LSFDE31:
	.set L$set$76,LEFDE31-LASFDE31
	.long L$set$76
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB47-.
	.set L$set$77,LFE47-LFB47
	.quad L$set$77
	.byte	0x0
	.byte	0x4
	.set L$set$78,LCFI148-LFB47
	.long L$set$78
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$79,LCFI149-LCFI148
	.long L$set$79
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$80,LCFI151-LCFI149
	.long L$set$80
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE31:
_Minus__Minus_var_u32_u16.eh:
LSFDE33:
	.set L$set$81,LEFDE33-LASFDE33
	.long L$set$81
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB49-.
	.set L$set$82,LFE49-LFB49
	.quad L$set$82
	.byte	0x0
	.byte	0x4
	.set L$set$83,LCFI155-LFB49
	.long L$set$83
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$84,LCFI156-LCFI155
	.long L$set$84
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$85,LCFI157-LCFI156
	.long L$set$85
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE33:
_Minus__Minus_param_u32_u16.eh:
LSFDE35:
	.set L$set$86,LEFDE35-LASFDE35
	.long L$set$86
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB51-.
	.set L$set$87,LFE51-LFB51
	.quad L$set$87
	.byte	0x0
	.byte	0x4
	.set L$set$88,LCFI161-LFB51
	.long L$set$88
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$89,LCFI162-LCFI161
	.long L$set$89
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$90,LCFI164-LCFI162
	.long L$set$90
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE35:
_Minus__Minus_var_u32_u32.eh:
LSFDE37:
	.set L$set$91,LEFDE37-LASFDE37
	.long L$set$91
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB53-.
	.set L$set$92,LFE53-LFB53
	.quad L$set$92
	.byte	0x0
	.byte	0x4
	.set L$set$93,LCFI168-LFB53
	.long L$set$93
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$94,LCFI169-LCFI168
	.long L$set$94
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$95,LCFI170-LCFI169
	.long L$set$95
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE37:
_Minus__Minus_param_u32_u32.eh:
LSFDE39:
	.set L$set$96,LEFDE39-LASFDE39
	.long L$set$96
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB55-.
	.set L$set$97,LFE55-LFB55
	.quad L$set$97
	.byte	0x0
	.byte	0x4
	.set L$set$98,LCFI174-LFB55
	.long L$set$98
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$99,LCFI175-LCFI174
	.long L$set$99
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$100,LCFI177-LCFI175
	.long L$set$100
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE39:
_Minus__Minus_var_u32_I.eh:
LSFDE41:
	.set L$set$101,LEFDE41-LASFDE41
	.long L$set$101
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB61-.
	.set L$set$102,LFE61-LFB61
	.quad L$set$102
	.byte	0x0
	.byte	0x4
	.set L$set$103,LCFI193-LFB61
	.long L$set$103
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$104,LCFI194-LCFI193
	.long L$set$104
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$105,LCFI195-LCFI194
	.long L$set$105
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE41:
_Minus__Minus_param_u32_I.eh:
LSFDE43:
	.set L$set$106,LEFDE43-LASFDE43
	.long L$set$106
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB63-.
	.set L$set$107,LFE63-LFB63
	.quad L$set$107
	.byte	0x0
	.byte	0x4
	.set L$set$108,LCFI199-LFB63
	.long L$set$108
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$109,LCFI200-LCFI199
	.long L$set$109
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$110,LCFI202-LCFI200
	.long L$set$110
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE43:
_Minus__Minus_var_u32_i16.eh:
LSFDE45:
	.set L$set$111,LEFDE45-LASFDE45
	.long L$set$111
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB65-.
	.set L$set$112,LFE65-LFB65
	.quad L$set$112
	.byte	0x0
	.byte	0x4
	.set L$set$113,LCFI206-LFB65
	.long L$set$113
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$114,LCFI207-LCFI206
	.long L$set$114
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$115,LCFI208-LCFI207
	.long L$set$115
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE45:
_Minus__Minus_param_u32_i16.eh:
LSFDE47:
	.set L$set$116,LEFDE47-LASFDE47
	.long L$set$116
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB67-.
	.set L$set$117,LFE67-LFB67
	.quad L$set$117
	.byte	0x0
	.byte	0x4
	.set L$set$118,LCFI212-LFB67
	.long L$set$118
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$119,LCFI213-LCFI212
	.long L$set$119
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$120,LCFI215-LCFI213
	.long L$set$120
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE47:
_Minus__Minus_var_u32_i32.eh:
LSFDE49:
	.set L$set$121,LEFDE49-LASFDE49
	.long L$set$121
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB73-.
	.set L$set$122,LFE73-LFB73
	.quad L$set$122
	.byte	0x0
	.byte	0x4
	.set L$set$123,LCFI231-LFB73
	.long L$set$123
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$124,LCFI232-LCFI231
	.long L$set$124
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$125,LCFI233-LCFI232
	.long L$set$125
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
_Minus__Minus_param_u32_i32.eh:
LSFDE51:
	.set L$set$126,LEFDE51-LASFDE51
	.long L$set$126
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB75-.
	.set L$set$127,LFE75-LFB75
	.quad L$set$127
	.byte	0x0
	.byte	0x4
	.set L$set$128,LCFI237-LFB75
	.long L$set$128
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$129,LCFI238-LCFI237
	.long L$set$129
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$130,LCFI240-LCFI238
	.long L$set$130
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE51:
_Minus__Minus_var_u32_C.eh:
LSFDE53:
	.set L$set$131,LEFDE53-LASFDE53
	.long L$set$131
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB77-.
	.set L$set$132,LFE77-LFB77
	.quad L$set$132
	.byte	0x0
	.byte	0x4
	.set L$set$133,LCFI244-LFB77
	.long L$set$133
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$134,LCFI245-LCFI244
	.long L$set$134
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$135,LCFI246-LCFI245
	.long L$set$135
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE53:
_Minus__Minus_param_u32_C.eh:
LSFDE55:
	.set L$set$136,LEFDE55-LASFDE55
	.long L$set$136
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB79-.
	.set L$set$137,LFE79-LFB79
	.quad L$set$137
	.byte	0x0
	.byte	0x4
	.set L$set$138,LCFI250-LFB79
	.long L$set$138
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$139,LCFI251-LCFI250
	.long L$set$139
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$140,LCFI253-LCFI251
	.long L$set$140
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE55:
_Minus__Minus_var_u32_u8.eh:
LSFDE57:
	.set L$set$141,LEFDE57-LASFDE57
	.long L$set$141
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB81-.
	.set L$set$142,LFE81-LFB81
	.quad L$set$142
	.byte	0x0
	.byte	0x4
	.set L$set$143,LCFI257-LFB81
	.long L$set$143
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$144,LCFI258-LCFI257
	.long L$set$144
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$145,LCFI259-LCFI258
	.long L$set$145
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE57:
_Minus__Minus_param_u32_u8.eh:
LSFDE59:
	.set L$set$146,LEFDE59-LASFDE59
	.long L$set$146
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB83-.
	.set L$set$147,LFE83-LFB83
	.quad L$set$147
	.byte	0x0
	.byte	0x4
	.set L$set$148,LCFI263-LFB83
	.long L$set$148
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$149,LCFI264-LCFI263
	.long L$set$149
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$150,LCFI266-LCFI264
	.long L$set$150
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE59:
_Minus__Minus_var_u32_i8.eh:
LSFDE61:
	.set L$set$151,LEFDE61-LASFDE61
	.long L$set$151
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB93-.
	.set L$set$152,LFE93-LFB93
	.quad L$set$152
	.byte	0x0
	.byte	0x4
	.set L$set$153,LCFI294-LFB93
	.long L$set$153
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$154,LCFI295-LCFI294
	.long L$set$154
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$155,LCFI296-LCFI295
	.long L$set$155
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE61:
_Minus__Minus_param_u32_i8.eh:
LSFDE63:
	.set L$set$156,LEFDE63-LASFDE63
	.long L$set$156
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB95-.
	.set L$set$157,LFE95-LFB95
	.quad L$set$157
	.byte	0x0
	.byte	0x4
	.set L$set$158,LCFI300-LFB95
	.long L$set$158
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$159,LCFI301-LCFI300
	.long L$set$159
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$160,LCFI303-LCFI301
	.long L$set$160
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE63:
_Minus__Minus_var_i16_u16.eh:
LSFDE65:
	.set L$set$161,LEFDE65-LASFDE65
	.long L$set$161
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB193-.
	.set L$set$162,LFE193-LFB193
	.quad L$set$162
	.byte	0x0
	.byte	0x4
	.set L$set$163,LCFI595-LFB193
	.long L$set$163
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$164,LCFI596-LCFI595
	.long L$set$164
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$165,LCFI597-LCFI596
	.long L$set$165
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE65:
_Minus__Minus_param_i16_u16.eh:
LSFDE67:
	.set L$set$166,LEFDE67-LASFDE67
	.long L$set$166
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB195-.
	.set L$set$167,LFE195-LFB195
	.quad L$set$167
	.byte	0x0
	.byte	0x4
	.set L$set$168,LCFI601-LFB195
	.long L$set$168
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$169,LCFI602-LCFI601
	.long L$set$169
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$170,LCFI604-LCFI602
	.long L$set$170
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE67:
_Minus__Minus_var_i16_u32.eh:
LSFDE69:
	.set L$set$171,LEFDE69-LASFDE69
	.long L$set$171
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB197-.
	.set L$set$172,LFE197-LFB197
	.quad L$set$172
	.byte	0x0
	.byte	0x4
	.set L$set$173,LCFI608-LFB197
	.long L$set$173
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$174,LCFI609-LCFI608
	.long L$set$174
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$175,LCFI610-LCFI609
	.long L$set$175
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE69:
_Minus__Minus_param_i16_u32.eh:
LSFDE71:
	.set L$set$176,LEFDE71-LASFDE71
	.long L$set$176
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB199-.
	.set L$set$177,LFE199-LFB199
	.quad L$set$177
	.byte	0x0
	.byte	0x4
	.set L$set$178,LCFI614-LFB199
	.long L$set$178
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$179,LCFI615-LCFI614
	.long L$set$179
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$180,LCFI617-LCFI615
	.long L$set$180
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE71:
_Minus__Minus_var_i16_I.eh:
LSFDE73:
	.set L$set$181,LEFDE73-LASFDE73
	.long L$set$181
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB205-.
	.set L$set$182,LFE205-LFB205
	.quad L$set$182
	.byte	0x0
	.byte	0x4
	.set L$set$183,LCFI633-LFB205
	.long L$set$183
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$184,LCFI634-LCFI633
	.long L$set$184
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$185,LCFI635-LCFI634
	.long L$set$185
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE73:
_Minus__Minus_param_i16_I.eh:
LSFDE75:
	.set L$set$186,LEFDE75-LASFDE75
	.long L$set$186
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB207-.
	.set L$set$187,LFE207-LFB207
	.quad L$set$187
	.byte	0x0
	.byte	0x4
	.set L$set$188,LCFI639-LFB207
	.long L$set$188
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$189,LCFI640-LCFI639
	.long L$set$189
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$190,LCFI642-LCFI640
	.long L$set$190
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE75:
_Minus__Minus_var_i16_i16.eh:
LSFDE77:
	.set L$set$191,LEFDE77-LASFDE77
	.long L$set$191
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB209-.
	.set L$set$192,LFE209-LFB209
	.quad L$set$192
	.byte	0x0
	.byte	0x4
	.set L$set$193,LCFI646-LFB209
	.long L$set$193
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$194,LCFI647-LCFI646
	.long L$set$194
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$195,LCFI648-LCFI647
	.long L$set$195
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE77:
_Minus__Minus_param_i16_i16.eh:
LSFDE79:
	.set L$set$196,LEFDE79-LASFDE79
	.long L$set$196
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB211-.
	.set L$set$197,LFE211-LFB211
	.quad L$set$197
	.byte	0x0
	.byte	0x4
	.set L$set$198,LCFI652-LFB211
	.long L$set$198
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$199,LCFI653-LCFI652
	.long L$set$199
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$200,LCFI655-LCFI653
	.long L$set$200
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE79:
_Minus__Minus_var_i16_i32.eh:
LSFDE81:
	.set L$set$201,LEFDE81-LASFDE81
	.long L$set$201
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB217-.
	.set L$set$202,LFE217-LFB217
	.quad L$set$202
	.byte	0x0
	.byte	0x4
	.set L$set$203,LCFI671-LFB217
	.long L$set$203
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$204,LCFI672-LCFI671
	.long L$set$204
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$205,LCFI673-LCFI672
	.long L$set$205
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE81:
_Minus__Minus_param_i16_i32.eh:
LSFDE83:
	.set L$set$206,LEFDE83-LASFDE83
	.long L$set$206
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB219-.
	.set L$set$207,LFE219-LFB219
	.quad L$set$207
	.byte	0x0
	.byte	0x4
	.set L$set$208,LCFI677-LFB219
	.long L$set$208
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$209,LCFI678-LCFI677
	.long L$set$209
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$210,LCFI680-LCFI678
	.long L$set$210
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE83:
_Minus__Minus_var_i16_C.eh:
LSFDE85:
	.set L$set$211,LEFDE85-LASFDE85
	.long L$set$211
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB221-.
	.set L$set$212,LFE221-LFB221
	.quad L$set$212
	.byte	0x0
	.byte	0x4
	.set L$set$213,LCFI684-LFB221
	.long L$set$213
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$214,LCFI685-LCFI684
	.long L$set$214
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$215,LCFI686-LCFI685
	.long L$set$215
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE85:
_Minus__Minus_param_i16_C.eh:
LSFDE87:
	.set L$set$216,LEFDE87-LASFDE87
	.long L$set$216
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB223-.
	.set L$set$217,LFE223-LFB223
	.quad L$set$217
	.byte	0x0
	.byte	0x4
	.set L$set$218,LCFI690-LFB223
	.long L$set$218
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$219,LCFI691-LCFI690
	.long L$set$219
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$220,LCFI693-LCFI691
	.long L$set$220
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE87:
_Minus__Minus_var_i16_u8.eh:
LSFDE89:
	.set L$set$221,LEFDE89-LASFDE89
	.long L$set$221
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB225-.
	.set L$set$222,LFE225-LFB225
	.quad L$set$222
	.byte	0x0
	.byte	0x4
	.set L$set$223,LCFI697-LFB225
	.long L$set$223
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$224,LCFI698-LCFI697
	.long L$set$224
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$225,LCFI699-LCFI698
	.long L$set$225
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE89:
_Minus__Minus_param_i16_u8.eh:
LSFDE91:
	.set L$set$226,LEFDE91-LASFDE91
	.long L$set$226
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB227-.
	.set L$set$227,LFE227-LFB227
	.quad L$set$227
	.byte	0x0
	.byte	0x4
	.set L$set$228,LCFI703-LFB227
	.long L$set$228
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$229,LCFI704-LCFI703
	.long L$set$229
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$230,LCFI706-LCFI704
	.long L$set$230
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE91:
_Minus__Minus_var_i16_i8.eh:
LSFDE93:
	.set L$set$231,LEFDE93-LASFDE93
	.long L$set$231
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB237-.
	.set L$set$232,LFE237-LFB237
	.quad L$set$232
	.byte	0x0
	.byte	0x4
	.set L$set$233,LCFI734-LFB237
	.long L$set$233
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$234,LCFI735-LCFI734
	.long L$set$234
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$235,LCFI736-LCFI735
	.long L$set$235
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE93:
_Minus__Minus_param_i16_i8.eh:
LSFDE95:
	.set L$set$236,LEFDE95-LASFDE95
	.long L$set$236
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB239-.
	.set L$set$237,LFE239-LFB239
	.quad L$set$237
	.byte	0x0
	.byte	0x4
	.set L$set$238,LCFI740-LFB239
	.long L$set$238
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$239,LCFI741-LCFI740
	.long L$set$239
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$240,LCFI743-LCFI741
	.long L$set$240
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE95:
_Minus__Minus_var_i32_u16.eh:
LSFDE97:
	.set L$set$241,LEFDE97-LASFDE97
	.long L$set$241
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB291-.
	.set L$set$242,LFE291-LFB291
	.quad L$set$242
	.byte	0x0
	.byte	0x4
	.set L$set$243,LCFI897-LFB291
	.long L$set$243
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$244,LCFI898-LCFI897
	.long L$set$244
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$245,LCFI899-LCFI898
	.long L$set$245
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE97:
_Minus__Minus_param_i32_u16.eh:
LSFDE99:
	.set L$set$246,LEFDE99-LASFDE99
	.long L$set$246
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB293-.
	.set L$set$247,LFE293-LFB293
	.quad L$set$247
	.byte	0x0
	.byte	0x4
	.set L$set$248,LCFI903-LFB293
	.long L$set$248
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$249,LCFI904-LCFI903
	.long L$set$249
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$250,LCFI906-LCFI904
	.long L$set$250
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE99:
_Minus__Minus_var_i32_u32.eh:
LSFDE101:
	.set L$set$251,LEFDE101-LASFDE101
	.long L$set$251
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB295-.
	.set L$set$252,LFE295-LFB295
	.quad L$set$252
	.byte	0x0
	.byte	0x4
	.set L$set$253,LCFI910-LFB295
	.long L$set$253
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$254,LCFI911-LCFI910
	.long L$set$254
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$255,LCFI912-LCFI911
	.long L$set$255
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE101:
_Minus__Minus_param_i32_u32.eh:
LSFDE103:
	.set L$set$256,LEFDE103-LASFDE103
	.long L$set$256
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB297-.
	.set L$set$257,LFE297-LFB297
	.quad L$set$257
	.byte	0x0
	.byte	0x4
	.set L$set$258,LCFI916-LFB297
	.long L$set$258
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$259,LCFI917-LCFI916
	.long L$set$259
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$260,LCFI919-LCFI917
	.long L$set$260
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE103:
_Minus__Minus_var_i32_I.eh:
LSFDE105:
	.set L$set$261,LEFDE105-LASFDE105
	.long L$set$261
LASFDE105:
	.long	LASFDE105-EH_frame1
	.quad	LFB303-.
	.set L$set$262,LFE303-LFB303
	.quad L$set$262
	.byte	0x0
	.byte	0x4
	.set L$set$263,LCFI935-LFB303
	.long L$set$263
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$264,LCFI936-LCFI935
	.long L$set$264
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$265,LCFI937-LCFI936
	.long L$set$265
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE105:
_Minus__Minus_param_i32_I.eh:
LSFDE107:
	.set L$set$266,LEFDE107-LASFDE107
	.long L$set$266
LASFDE107:
	.long	LASFDE107-EH_frame1
	.quad	LFB305-.
	.set L$set$267,LFE305-LFB305
	.quad L$set$267
	.byte	0x0
	.byte	0x4
	.set L$set$268,LCFI941-LFB305
	.long L$set$268
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$269,LCFI942-LCFI941
	.long L$set$269
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$270,LCFI944-LCFI942
	.long L$set$270
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE107:
_Minus__Minus_var_i32_i16.eh:
LSFDE109:
	.set L$set$271,LEFDE109-LASFDE109
	.long L$set$271
LASFDE109:
	.long	LASFDE109-EH_frame1
	.quad	LFB307-.
	.set L$set$272,LFE307-LFB307
	.quad L$set$272
	.byte	0x0
	.byte	0x4
	.set L$set$273,LCFI948-LFB307
	.long L$set$273
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$274,LCFI949-LCFI948
	.long L$set$274
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$275,LCFI950-LCFI949
	.long L$set$275
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE109:
_Minus__Minus_param_i32_i16.eh:
LSFDE111:
	.set L$set$276,LEFDE111-LASFDE111
	.long L$set$276
LASFDE111:
	.long	LASFDE111-EH_frame1
	.quad	LFB309-.
	.set L$set$277,LFE309-LFB309
	.quad L$set$277
	.byte	0x0
	.byte	0x4
	.set L$set$278,LCFI954-LFB309
	.long L$set$278
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$279,LCFI955-LCFI954
	.long L$set$279
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$280,LCFI957-LCFI955
	.long L$set$280
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE111:
_Minus__Minus_var_i32_i32.eh:
LSFDE113:
	.set L$set$281,LEFDE113-LASFDE113
	.long L$set$281
LASFDE113:
	.long	LASFDE113-EH_frame1
	.quad	LFB315-.
	.set L$set$282,LFE315-LFB315
	.quad L$set$282
	.byte	0x0
	.byte	0x4
	.set L$set$283,LCFI973-LFB315
	.long L$set$283
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$284,LCFI974-LCFI973
	.long L$set$284
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$285,LCFI975-LCFI974
	.long L$set$285
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE113:
_Minus__Minus_param_i32_i32.eh:
LSFDE115:
	.set L$set$286,LEFDE115-LASFDE115
	.long L$set$286
LASFDE115:
	.long	LASFDE115-EH_frame1
	.quad	LFB317-.
	.set L$set$287,LFE317-LFB317
	.quad L$set$287
	.byte	0x0
	.byte	0x4
	.set L$set$288,LCFI979-LFB317
	.long L$set$288
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$289,LCFI980-LCFI979
	.long L$set$289
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$290,LCFI982-LCFI980
	.long L$set$290
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE115:
_Minus__Minus_var_i32_C.eh:
LSFDE117:
	.set L$set$291,LEFDE117-LASFDE117
	.long L$set$291
LASFDE117:
	.long	LASFDE117-EH_frame1
	.quad	LFB319-.
	.set L$set$292,LFE319-LFB319
	.quad L$set$292
	.byte	0x0
	.byte	0x4
	.set L$set$293,LCFI986-LFB319
	.long L$set$293
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$294,LCFI987-LCFI986
	.long L$set$294
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$295,LCFI988-LCFI987
	.long L$set$295
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE117:
_Minus__Minus_param_i32_C.eh:
LSFDE119:
	.set L$set$296,LEFDE119-LASFDE119
	.long L$set$296
LASFDE119:
	.long	LASFDE119-EH_frame1
	.quad	LFB321-.
	.set L$set$297,LFE321-LFB321
	.quad L$set$297
	.byte	0x0
	.byte	0x4
	.set L$set$298,LCFI992-LFB321
	.long L$set$298
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$299,LCFI993-LCFI992
	.long L$set$299
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$300,LCFI995-LCFI993
	.long L$set$300
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE119:
_Minus__Minus_var_i32_u8.eh:
LSFDE121:
	.set L$set$301,LEFDE121-LASFDE121
	.long L$set$301
LASFDE121:
	.long	LASFDE121-EH_frame1
	.quad	LFB323-.
	.set L$set$302,LFE323-LFB323
	.quad L$set$302
	.byte	0x0
	.byte	0x4
	.set L$set$303,LCFI999-LFB323
	.long L$set$303
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$304,LCFI1000-LCFI999
	.long L$set$304
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$305,LCFI1001-LCFI1000
	.long L$set$305
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE121:
_Minus__Minus_param_i32_u8.eh:
LSFDE123:
	.set L$set$306,LEFDE123-LASFDE123
	.long L$set$306
LASFDE123:
	.long	LASFDE123-EH_frame1
	.quad	LFB325-.
	.set L$set$307,LFE325-LFB325
	.quad L$set$307
	.byte	0x0
	.byte	0x4
	.set L$set$308,LCFI1005-LFB325
	.long L$set$308
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$309,LCFI1006-LCFI1005
	.long L$set$309
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$310,LCFI1008-LCFI1006
	.long L$set$310
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE123:
_Minus__Minus_var_i32_i8.eh:
LSFDE125:
	.set L$set$311,LEFDE125-LASFDE125
	.long L$set$311
LASFDE125:
	.long	LASFDE125-EH_frame1
	.quad	LFB335-.
	.set L$set$312,LFE335-LFB335
	.quad L$set$312
	.byte	0x0
	.byte	0x4
	.set L$set$313,LCFI1036-LFB335
	.long L$set$313
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$314,LCFI1037-LCFI1036
	.long L$set$314
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$315,LCFI1038-LCFI1037
	.long L$set$315
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE125:
_Minus__Minus_param_i32_i8.eh:
LSFDE127:
	.set L$set$316,LEFDE127-LASFDE127
	.long L$set$316
LASFDE127:
	.long	LASFDE127-EH_frame1
	.quad	LFB337-.
	.set L$set$317,LFE337-LFB337
	.quad L$set$317
	.byte	0x0
	.byte	0x4
	.set L$set$318,LCFI1042-LFB337
	.long L$set$318
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$319,LCFI1043-LCFI1042
	.long L$set$319
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$320,LCFI1045-LCFI1043
	.long L$set$320
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE127:
_Minus__Minus_var_C_u16.eh:
LSFDE129:
	.set L$set$321,LEFDE129-LASFDE129
	.long L$set$321
LASFDE129:
	.long	LASFDE129-EH_frame1
	.quad	LFB339-.
	.set L$set$322,LFE339-LFB339
	.quad L$set$322
	.byte	0x0
	.byte	0x4
	.set L$set$323,LCFI1049-LFB339
	.long L$set$323
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$324,LCFI1050-LCFI1049
	.long L$set$324
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$325,LCFI1051-LCFI1050
	.long L$set$325
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE129:
_Minus__Minus_param_C_u16.eh:
LSFDE131:
	.set L$set$326,LEFDE131-LASFDE131
	.long L$set$326
LASFDE131:
	.long	LASFDE131-EH_frame1
	.quad	LFB341-.
	.set L$set$327,LFE341-LFB341
	.quad L$set$327
	.byte	0x0
	.byte	0x4
	.set L$set$328,LCFI1055-LFB341
	.long L$set$328
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$329,LCFI1056-LCFI1055
	.long L$set$329
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$330,LCFI1058-LCFI1056
	.long L$set$330
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE131:
_Minus__Minus_var_C_u32.eh:
LSFDE133:
	.set L$set$331,LEFDE133-LASFDE133
	.long L$set$331
LASFDE133:
	.long	LASFDE133-EH_frame1
	.quad	LFB343-.
	.set L$set$332,LFE343-LFB343
	.quad L$set$332
	.byte	0x0
	.byte	0x4
	.set L$set$333,LCFI1062-LFB343
	.long L$set$333
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$334,LCFI1063-LCFI1062
	.long L$set$334
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$335,LCFI1064-LCFI1063
	.long L$set$335
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE133:
_Minus__Minus_param_C_u32.eh:
LSFDE135:
	.set L$set$336,LEFDE135-LASFDE135
	.long L$set$336
LASFDE135:
	.long	LASFDE135-EH_frame1
	.quad	LFB345-.
	.set L$set$337,LFE345-LFB345
	.quad L$set$337
	.byte	0x0
	.byte	0x4
	.set L$set$338,LCFI1068-LFB345
	.long L$set$338
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$339,LCFI1069-LCFI1068
	.long L$set$339
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$340,LCFI1071-LCFI1069
	.long L$set$340
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE135:
_Minus__Minus_var_C_I.eh:
LSFDE137:
	.set L$set$341,LEFDE137-LASFDE137
	.long L$set$341
LASFDE137:
	.long	LASFDE137-EH_frame1
	.quad	LFB351-.
	.set L$set$342,LFE351-LFB351
	.quad L$set$342
	.byte	0x0
	.byte	0x4
	.set L$set$343,LCFI1087-LFB351
	.long L$set$343
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$344,LCFI1088-LCFI1087
	.long L$set$344
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$345,LCFI1089-LCFI1088
	.long L$set$345
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE137:
_Minus__Minus_param_C_I.eh:
LSFDE139:
	.set L$set$346,LEFDE139-LASFDE139
	.long L$set$346
LASFDE139:
	.long	LASFDE139-EH_frame1
	.quad	LFB353-.
	.set L$set$347,LFE353-LFB353
	.quad L$set$347
	.byte	0x0
	.byte	0x4
	.set L$set$348,LCFI1093-LFB353
	.long L$set$348
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$349,LCFI1094-LCFI1093
	.long L$set$349
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$350,LCFI1096-LCFI1094
	.long L$set$350
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE139:
_Minus__Minus_var_C_i16.eh:
LSFDE141:
	.set L$set$351,LEFDE141-LASFDE141
	.long L$set$351
LASFDE141:
	.long	LASFDE141-EH_frame1
	.quad	LFB355-.
	.set L$set$352,LFE355-LFB355
	.quad L$set$352
	.byte	0x0
	.byte	0x4
	.set L$set$353,LCFI1100-LFB355
	.long L$set$353
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$354,LCFI1101-LCFI1100
	.long L$set$354
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$355,LCFI1102-LCFI1101
	.long L$set$355
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE141:
_Minus__Minus_param_C_i16.eh:
LSFDE143:
	.set L$set$356,LEFDE143-LASFDE143
	.long L$set$356
LASFDE143:
	.long	LASFDE143-EH_frame1
	.quad	LFB357-.
	.set L$set$357,LFE357-LFB357
	.quad L$set$357
	.byte	0x0
	.byte	0x4
	.set L$set$358,LCFI1106-LFB357
	.long L$set$358
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$359,LCFI1107-LCFI1106
	.long L$set$359
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$360,LCFI1109-LCFI1107
	.long L$set$360
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE143:
_Minus__Minus_var_C_i32.eh:
LSFDE145:
	.set L$set$361,LEFDE145-LASFDE145
	.long L$set$361
LASFDE145:
	.long	LASFDE145-EH_frame1
	.quad	LFB363-.
	.set L$set$362,LFE363-LFB363
	.quad L$set$362
	.byte	0x0
	.byte	0x4
	.set L$set$363,LCFI1125-LFB363
	.long L$set$363
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$364,LCFI1126-LCFI1125
	.long L$set$364
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$365,LCFI1127-LCFI1126
	.long L$set$365
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE145:
_Minus__Minus_param_C_i32.eh:
LSFDE147:
	.set L$set$366,LEFDE147-LASFDE147
	.long L$set$366
LASFDE147:
	.long	LASFDE147-EH_frame1
	.quad	LFB365-.
	.set L$set$367,LFE365-LFB365
	.quad L$set$367
	.byte	0x0
	.byte	0x4
	.set L$set$368,LCFI1131-LFB365
	.long L$set$368
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$369,LCFI1132-LCFI1131
	.long L$set$369
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$370,LCFI1134-LCFI1132
	.long L$set$370
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE147:
_Minus__Minus_var_C_C.eh:
LSFDE149:
	.set L$set$371,LEFDE149-LASFDE149
	.long L$set$371
LASFDE149:
	.long	LASFDE149-EH_frame1
	.quad	LFB367-.
	.set L$set$372,LFE367-LFB367
	.quad L$set$372
	.byte	0x0
	.byte	0x4
	.set L$set$373,LCFI1138-LFB367
	.long L$set$373
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$374,LCFI1139-LCFI1138
	.long L$set$374
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$375,LCFI1140-LCFI1139
	.long L$set$375
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE149:
_Minus__Minus_param_C_C.eh:
LSFDE151:
	.set L$set$376,LEFDE151-LASFDE151
	.long L$set$376
LASFDE151:
	.long	LASFDE151-EH_frame1
	.quad	LFB369-.
	.set L$set$377,LFE369-LFB369
	.quad L$set$377
	.byte	0x0
	.byte	0x4
	.set L$set$378,LCFI1144-LFB369
	.long L$set$378
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$379,LCFI1145-LCFI1144
	.long L$set$379
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$380,LCFI1147-LCFI1145
	.long L$set$380
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE151:
_Minus__Minus_var_C_u8.eh:
LSFDE153:
	.set L$set$381,LEFDE153-LASFDE153
	.long L$set$381
LASFDE153:
	.long	LASFDE153-EH_frame1
	.quad	LFB371-.
	.set L$set$382,LFE371-LFB371
	.quad L$set$382
	.byte	0x0
	.byte	0x4
	.set L$set$383,LCFI1151-LFB371
	.long L$set$383
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$384,LCFI1152-LCFI1151
	.long L$set$384
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$385,LCFI1153-LCFI1152
	.long L$set$385
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE153:
_Minus__Minus_param_C_u8.eh:
LSFDE155:
	.set L$set$386,LEFDE155-LASFDE155
	.long L$set$386
LASFDE155:
	.long	LASFDE155-EH_frame1
	.quad	LFB373-.
	.set L$set$387,LFE373-LFB373
	.quad L$set$387
	.byte	0x0
	.byte	0x4
	.set L$set$388,LCFI1157-LFB373
	.long L$set$388
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$389,LCFI1158-LCFI1157
	.long L$set$389
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$390,LCFI1160-LCFI1158
	.long L$set$390
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE155:
_Minus__Minus_var_C_i8.eh:
LSFDE157:
	.set L$set$391,LEFDE157-LASFDE157
	.long L$set$391
LASFDE157:
	.long	LASFDE157-EH_frame1
	.quad	LFB383-.
	.set L$set$392,LFE383-LFB383
	.quad L$set$392
	.byte	0x0
	.byte	0x4
	.set L$set$393,LCFI1188-LFB383
	.long L$set$393
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$394,LCFI1189-LCFI1188
	.long L$set$394
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$395,LCFI1190-LCFI1189
	.long L$set$395
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE157:
_Minus__Minus_param_C_i8.eh:
LSFDE159:
	.set L$set$396,LEFDE159-LASFDE159
	.long L$set$396
LASFDE159:
	.long	LASFDE159-EH_frame1
	.quad	LFB385-.
	.set L$set$397,LFE385-LFB385
	.quad L$set$397
	.byte	0x0
	.byte	0x4
	.set L$set$398,LCFI1194-LFB385
	.long L$set$398
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$399,LCFI1195-LCFI1194
	.long L$set$399
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$400,LCFI1197-LCFI1195
	.long L$set$400
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE159:
_Minus__Minus_var_u8_u16.eh:
LSFDE161:
	.set L$set$401,LEFDE161-LASFDE161
	.long L$set$401
LASFDE161:
	.long	LASFDE161-EH_frame1
	.quad	LFB387-.
	.set L$set$402,LFE387-LFB387
	.quad L$set$402
	.byte	0x0
	.byte	0x4
	.set L$set$403,LCFI1201-LFB387
	.long L$set$403
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$404,LCFI1202-LCFI1201
	.long L$set$404
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$405,LCFI1203-LCFI1202
	.long L$set$405
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE161:
_Minus__Minus_param_u8_u16.eh:
LSFDE163:
	.set L$set$406,LEFDE163-LASFDE163
	.long L$set$406
LASFDE163:
	.long	LASFDE163-EH_frame1
	.quad	LFB389-.
	.set L$set$407,LFE389-LFB389
	.quad L$set$407
	.byte	0x0
	.byte	0x4
	.set L$set$408,LCFI1207-LFB389
	.long L$set$408
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$409,LCFI1208-LCFI1207
	.long L$set$409
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$410,LCFI1210-LCFI1208
	.long L$set$410
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE163:
_Minus__Minus_var_u8_u32.eh:
LSFDE165:
	.set L$set$411,LEFDE165-LASFDE165
	.long L$set$411
LASFDE165:
	.long	LASFDE165-EH_frame1
	.quad	LFB391-.
	.set L$set$412,LFE391-LFB391
	.quad L$set$412
	.byte	0x0
	.byte	0x4
	.set L$set$413,LCFI1214-LFB391
	.long L$set$413
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$414,LCFI1215-LCFI1214
	.long L$set$414
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$415,LCFI1216-LCFI1215
	.long L$set$415
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE165:
_Minus__Minus_param_u8_u32.eh:
LSFDE167:
	.set L$set$416,LEFDE167-LASFDE167
	.long L$set$416
LASFDE167:
	.long	LASFDE167-EH_frame1
	.quad	LFB393-.
	.set L$set$417,LFE393-LFB393
	.quad L$set$417
	.byte	0x0
	.byte	0x4
	.set L$set$418,LCFI1220-LFB393
	.long L$set$418
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$419,LCFI1221-LCFI1220
	.long L$set$419
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$420,LCFI1223-LCFI1221
	.long L$set$420
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE167:
_Minus__Minus_var_u8_I.eh:
LSFDE169:
	.set L$set$421,LEFDE169-LASFDE169
	.long L$set$421
LASFDE169:
	.long	LASFDE169-EH_frame1
	.quad	LFB399-.
	.set L$set$422,LFE399-LFB399
	.quad L$set$422
	.byte	0x0
	.byte	0x4
	.set L$set$423,LCFI1239-LFB399
	.long L$set$423
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$424,LCFI1240-LCFI1239
	.long L$set$424
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$425,LCFI1241-LCFI1240
	.long L$set$425
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE169:
_Minus__Minus_param_u8_I.eh:
LSFDE171:
	.set L$set$426,LEFDE171-LASFDE171
	.long L$set$426
LASFDE171:
	.long	LASFDE171-EH_frame1
	.quad	LFB401-.
	.set L$set$427,LFE401-LFB401
	.quad L$set$427
	.byte	0x0
	.byte	0x4
	.set L$set$428,LCFI1245-LFB401
	.long L$set$428
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$429,LCFI1246-LCFI1245
	.long L$set$429
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$430,LCFI1248-LCFI1246
	.long L$set$430
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE171:
_Minus__Minus_var_u8_i16.eh:
LSFDE173:
	.set L$set$431,LEFDE173-LASFDE173
	.long L$set$431
LASFDE173:
	.long	LASFDE173-EH_frame1
	.quad	LFB403-.
	.set L$set$432,LFE403-LFB403
	.quad L$set$432
	.byte	0x0
	.byte	0x4
	.set L$set$433,LCFI1252-LFB403
	.long L$set$433
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$434,LCFI1253-LCFI1252
	.long L$set$434
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$435,LCFI1254-LCFI1253
	.long L$set$435
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE173:
_Minus__Minus_param_u8_i16.eh:
LSFDE175:
	.set L$set$436,LEFDE175-LASFDE175
	.long L$set$436
LASFDE175:
	.long	LASFDE175-EH_frame1
	.quad	LFB405-.
	.set L$set$437,LFE405-LFB405
	.quad L$set$437
	.byte	0x0
	.byte	0x4
	.set L$set$438,LCFI1258-LFB405
	.long L$set$438
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$439,LCFI1259-LCFI1258
	.long L$set$439
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$440,LCFI1261-LCFI1259
	.long L$set$440
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE175:
_Minus__Minus_var_u8_i32.eh:
LSFDE177:
	.set L$set$441,LEFDE177-LASFDE177
	.long L$set$441
LASFDE177:
	.long	LASFDE177-EH_frame1
	.quad	LFB411-.
	.set L$set$442,LFE411-LFB411
	.quad L$set$442
	.byte	0x0
	.byte	0x4
	.set L$set$443,LCFI1277-LFB411
	.long L$set$443
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$444,LCFI1278-LCFI1277
	.long L$set$444
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$445,LCFI1279-LCFI1278
	.long L$set$445
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE177:
_Minus__Minus_param_u8_i32.eh:
LSFDE179:
	.set L$set$446,LEFDE179-LASFDE179
	.long L$set$446
LASFDE179:
	.long	LASFDE179-EH_frame1
	.quad	LFB413-.
	.set L$set$447,LFE413-LFB413
	.quad L$set$447
	.byte	0x0
	.byte	0x4
	.set L$set$448,LCFI1283-LFB413
	.long L$set$448
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$449,LCFI1284-LCFI1283
	.long L$set$449
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$450,LCFI1286-LCFI1284
	.long L$set$450
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE179:
_Minus__Minus_var_u8_C.eh:
LSFDE181:
	.set L$set$451,LEFDE181-LASFDE181
	.long L$set$451
LASFDE181:
	.long	LASFDE181-EH_frame1
	.quad	LFB415-.
	.set L$set$452,LFE415-LFB415
	.quad L$set$452
	.byte	0x0
	.byte	0x4
	.set L$set$453,LCFI1290-LFB415
	.long L$set$453
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$454,LCFI1291-LCFI1290
	.long L$set$454
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$455,LCFI1292-LCFI1291
	.long L$set$455
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE181:
_Minus__Minus_param_u8_C.eh:
LSFDE183:
	.set L$set$456,LEFDE183-LASFDE183
	.long L$set$456
LASFDE183:
	.long	LASFDE183-EH_frame1
	.quad	LFB417-.
	.set L$set$457,LFE417-LFB417
	.quad L$set$457
	.byte	0x0
	.byte	0x4
	.set L$set$458,LCFI1296-LFB417
	.long L$set$458
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$459,LCFI1297-LCFI1296
	.long L$set$459
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$460,LCFI1299-LCFI1297
	.long L$set$460
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE183:
_Minus__Minus_var_u8_u8.eh:
LSFDE185:
	.set L$set$461,LEFDE185-LASFDE185
	.long L$set$461
LASFDE185:
	.long	LASFDE185-EH_frame1
	.quad	LFB419-.
	.set L$set$462,LFE419-LFB419
	.quad L$set$462
	.byte	0x0
	.byte	0x4
	.set L$set$463,LCFI1303-LFB419
	.long L$set$463
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$464,LCFI1304-LCFI1303
	.long L$set$464
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$465,LCFI1305-LCFI1304
	.long L$set$465
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE185:
_Minus__Minus_param_u8_u8.eh:
LSFDE187:
	.set L$set$466,LEFDE187-LASFDE187
	.long L$set$466
LASFDE187:
	.long	LASFDE187-EH_frame1
	.quad	LFB421-.
	.set L$set$467,LFE421-LFB421
	.quad L$set$467
	.byte	0x0
	.byte	0x4
	.set L$set$468,LCFI1309-LFB421
	.long L$set$468
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$469,LCFI1310-LCFI1309
	.long L$set$469
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$470,LCFI1312-LCFI1310
	.long L$set$470
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE187:
_Minus__Minus_var_u8_i8.eh:
LSFDE189:
	.set L$set$471,LEFDE189-LASFDE189
	.long L$set$471
LASFDE189:
	.long	LASFDE189-EH_frame1
	.quad	LFB431-.
	.set L$set$472,LFE431-LFB431
	.quad L$set$472
	.byte	0x0
	.byte	0x4
	.set L$set$473,LCFI1340-LFB431
	.long L$set$473
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$474,LCFI1341-LCFI1340
	.long L$set$474
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$475,LCFI1342-LCFI1341
	.long L$set$475
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE189:
_Minus__Minus_param_u8_i8.eh:
LSFDE191:
	.set L$set$476,LEFDE191-LASFDE191
	.long L$set$476
LASFDE191:
	.long	LASFDE191-EH_frame1
	.quad	LFB433-.
	.set L$set$477,LFE433-LFB433
	.quad L$set$477
	.byte	0x0
	.byte	0x4
	.set L$set$478,LCFI1346-LFB433
	.long L$set$478
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$479,LCFI1347-LCFI1346
	.long L$set$479
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$480,LCFI1349-LCFI1347
	.long L$set$480
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE191:
_Minus__Minus_var_i8_u16.eh:
LSFDE193:
	.set L$set$481,LEFDE193-LASFDE193
	.long L$set$481
LASFDE193:
	.long	LASFDE193-EH_frame1
	.quad	LFB533-.
	.set L$set$482,LFE533-LFB533
	.quad L$set$482
	.byte	0x0
	.byte	0x4
	.set L$set$483,LCFI1647-LFB533
	.long L$set$483
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$484,LCFI1648-LCFI1647
	.long L$set$484
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$485,LCFI1649-LCFI1648
	.long L$set$485
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE193:
_Minus__Minus_param_i8_u16.eh:
LSFDE195:
	.set L$set$486,LEFDE195-LASFDE195
	.long L$set$486
LASFDE195:
	.long	LASFDE195-EH_frame1
	.quad	LFB535-.
	.set L$set$487,LFE535-LFB535
	.quad L$set$487
	.byte	0x0
	.byte	0x4
	.set L$set$488,LCFI1653-LFB535
	.long L$set$488
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$489,LCFI1654-LCFI1653
	.long L$set$489
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$490,LCFI1656-LCFI1654
	.long L$set$490
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE195:
_Minus__Minus_var_i8_u32.eh:
LSFDE197:
	.set L$set$491,LEFDE197-LASFDE197
	.long L$set$491
LASFDE197:
	.long	LASFDE197-EH_frame1
	.quad	LFB537-.
	.set L$set$492,LFE537-LFB537
	.quad L$set$492
	.byte	0x0
	.byte	0x4
	.set L$set$493,LCFI1660-LFB537
	.long L$set$493
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$494,LCFI1661-LCFI1660
	.long L$set$494
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$495,LCFI1662-LCFI1661
	.long L$set$495
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE197:
_Minus__Minus_param_i8_u32.eh:
LSFDE199:
	.set L$set$496,LEFDE199-LASFDE199
	.long L$set$496
LASFDE199:
	.long	LASFDE199-EH_frame1
	.quad	LFB539-.
	.set L$set$497,LFE539-LFB539
	.quad L$set$497
	.byte	0x0
	.byte	0x4
	.set L$set$498,LCFI1666-LFB539
	.long L$set$498
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$499,LCFI1667-LCFI1666
	.long L$set$499
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$500,LCFI1669-LCFI1667
	.long L$set$500
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE199:
_Minus__Minus_var_i8_I.eh:
LSFDE201:
	.set L$set$501,LEFDE201-LASFDE201
	.long L$set$501
LASFDE201:
	.long	LASFDE201-EH_frame1
	.quad	LFB545-.
	.set L$set$502,LFE545-LFB545
	.quad L$set$502
	.byte	0x0
	.byte	0x4
	.set L$set$503,LCFI1685-LFB545
	.long L$set$503
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$504,LCFI1686-LCFI1685
	.long L$set$504
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$505,LCFI1687-LCFI1686
	.long L$set$505
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE201:
_Minus__Minus_param_i8_I.eh:
LSFDE203:
	.set L$set$506,LEFDE203-LASFDE203
	.long L$set$506
LASFDE203:
	.long	LASFDE203-EH_frame1
	.quad	LFB547-.
	.set L$set$507,LFE547-LFB547
	.quad L$set$507
	.byte	0x0
	.byte	0x4
	.set L$set$508,LCFI1691-LFB547
	.long L$set$508
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$509,LCFI1692-LCFI1691
	.long L$set$509
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$510,LCFI1694-LCFI1692
	.long L$set$510
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE203:
_Minus__Minus_var_i8_i16.eh:
LSFDE205:
	.set L$set$511,LEFDE205-LASFDE205
	.long L$set$511
LASFDE205:
	.long	LASFDE205-EH_frame1
	.quad	LFB549-.
	.set L$set$512,LFE549-LFB549
	.quad L$set$512
	.byte	0x0
	.byte	0x4
	.set L$set$513,LCFI1698-LFB549
	.long L$set$513
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$514,LCFI1699-LCFI1698
	.long L$set$514
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$515,LCFI1700-LCFI1699
	.long L$set$515
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE205:
_Minus__Minus_param_i8_i16.eh:
LSFDE207:
	.set L$set$516,LEFDE207-LASFDE207
	.long L$set$516
LASFDE207:
	.long	LASFDE207-EH_frame1
	.quad	LFB551-.
	.set L$set$517,LFE551-LFB551
	.quad L$set$517
	.byte	0x0
	.byte	0x4
	.set L$set$518,LCFI1704-LFB551
	.long L$set$518
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$519,LCFI1705-LCFI1704
	.long L$set$519
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$520,LCFI1707-LCFI1705
	.long L$set$520
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE207:
_Minus__Minus_var_i8_i32.eh:
LSFDE209:
	.set L$set$521,LEFDE209-LASFDE209
	.long L$set$521
LASFDE209:
	.long	LASFDE209-EH_frame1
	.quad	LFB557-.
	.set L$set$522,LFE557-LFB557
	.quad L$set$522
	.byte	0x0
	.byte	0x4
	.set L$set$523,LCFI1723-LFB557
	.long L$set$523
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$524,LCFI1724-LCFI1723
	.long L$set$524
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$525,LCFI1725-LCFI1724
	.long L$set$525
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE209:
_Minus__Minus_param_i8_i32.eh:
LSFDE211:
	.set L$set$526,LEFDE211-LASFDE211
	.long L$set$526
LASFDE211:
	.long	LASFDE211-EH_frame1
	.quad	LFB559-.
	.set L$set$527,LFE559-LFB559
	.quad L$set$527
	.byte	0x0
	.byte	0x4
	.set L$set$528,LCFI1729-LFB559
	.long L$set$528
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$529,LCFI1730-LCFI1729
	.long L$set$529
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$530,LCFI1732-LCFI1730
	.long L$set$530
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE211:
_Minus__Minus_var_i8_C.eh:
LSFDE213:
	.set L$set$531,LEFDE213-LASFDE213
	.long L$set$531
LASFDE213:
	.long	LASFDE213-EH_frame1
	.quad	LFB561-.
	.set L$set$532,LFE561-LFB561
	.quad L$set$532
	.byte	0x0
	.byte	0x4
	.set L$set$533,LCFI1736-LFB561
	.long L$set$533
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$534,LCFI1737-LCFI1736
	.long L$set$534
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$535,LCFI1738-LCFI1737
	.long L$set$535
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE213:
_Minus__Minus_param_i8_C.eh:
LSFDE215:
	.set L$set$536,LEFDE215-LASFDE215
	.long L$set$536
LASFDE215:
	.long	LASFDE215-EH_frame1
	.quad	LFB563-.
	.set L$set$537,LFE563-LFB563
	.quad L$set$537
	.byte	0x0
	.byte	0x4
	.set L$set$538,LCFI1742-LFB563
	.long L$set$538
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$539,LCFI1743-LCFI1742
	.long L$set$539
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$540,LCFI1745-LCFI1743
	.long L$set$540
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE215:
_Minus__Minus_var_i8_u8.eh:
LSFDE217:
	.set L$set$541,LEFDE217-LASFDE217
	.long L$set$541
LASFDE217:
	.long	LASFDE217-EH_frame1
	.quad	LFB565-.
	.set L$set$542,LFE565-LFB565
	.quad L$set$542
	.byte	0x0
	.byte	0x4
	.set L$set$543,LCFI1749-LFB565
	.long L$set$543
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$544,LCFI1750-LCFI1749
	.long L$set$544
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$545,LCFI1751-LCFI1750
	.long L$set$545
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE217:
_Minus__Minus_param_i8_u8.eh:
LSFDE219:
	.set L$set$546,LEFDE219-LASFDE219
	.long L$set$546
LASFDE219:
	.long	LASFDE219-EH_frame1
	.quad	LFB567-.
	.set L$set$547,LFE567-LFB567
	.quad L$set$547
	.byte	0x0
	.byte	0x4
	.set L$set$548,LCFI1755-LFB567
	.long L$set$548
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$549,LCFI1756-LCFI1755
	.long L$set$549
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$550,LCFI1758-LCFI1756
	.long L$set$550
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE219:
_Minus__Minus_var_i8_i8.eh:
LSFDE221:
	.set L$set$551,LEFDE221-LASFDE221
	.long L$set$551
LASFDE221:
	.long	LASFDE221-EH_frame1
	.quad	LFB577-.
	.set L$set$552,LFE577-LFB577
	.quad L$set$552
	.byte	0x0
	.byte	0x4
	.set L$set$553,LCFI1786-LFB577
	.long L$set$553
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$554,LCFI1787-LCFI1786
	.long L$set$554
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$555,LCFI1788-LCFI1787
	.long L$set$555
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE221:
_Minus__Minus_param_i8_i8.eh:
LSFDE223:
	.set L$set$556,LEFDE223-LASFDE223
	.long L$set$556
LASFDE223:
	.long	LASFDE223-EH_frame1
	.quad	LFB579-.
	.set L$set$557,LFE579-LFB579
	.quad L$set$557
	.byte	0x0
	.byte	0x4
	.set L$set$558,LCFI1792-LFB579
	.long L$set$558
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$559,LCFI1793-LCFI1792
	.long L$set$559
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$560,LCFI1795-LCFI1793
	.long L$set$560
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE223:
__m3_fault.eh:
LSFDE225:
	.set L$set$561,LEFDE225-LASFDE225
	.long L$set$561
LASFDE225:
	.long	LASFDE225-EH_frame1
	.quad	LFB581-.
	.set L$set$562,LFE581-LFB581
	.quad L$set$562
	.byte	0x0
	.byte	0x4
	.set L$set$563,LCFI1799-LFB581
	.long L$set$563
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$564,LCFI1800-LCFI1799
	.long L$set$564
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$565,LCFI1802-LCFI1800
	.long L$set$565
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE225:
	.subsections_via_symbols
