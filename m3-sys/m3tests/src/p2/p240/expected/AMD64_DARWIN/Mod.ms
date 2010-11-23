	.text
.no_dead_strip _Mod__uMod_var_u16_u16
_Mod__uMod_var_u16_u16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _Mod__Mod_var_u16_u16
_Mod__Mod_var_u16_u16:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	pushq	%rbx
LCFI5:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L3
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L3
	addq	%rcx, %rdx
L3:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI6:
	ret
LFE1:
.no_dead_strip _Mod__uMod_param_u16_u16
_Mod__uMod_param_u16_u16:
LFB2:
	pushq	%rbp
LCFI7:
	movq	%rsp, %rbp
LCFI8:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %eax
	movzwl	-4(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI9:
	ret
LFE2:
.no_dead_strip _Mod__Mod_param_u16_u16
_Mod__Mod_param_u16_u16:
LFB3:
	pushq	%rbp
LCFI10:
	movq	%rsp, %rbp
LCFI11:
	pushq	%rbx
LCFI12:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L6
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L6
	addq	%rcx, %rdx
L6:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI13:
	ret
LFE3:
.no_dead_strip _Mod__uMod_var_u16_u32
_Mod__uMod_var_u16_u32:
LFB4:
	pushq	%rbp
LCFI14:
	movq	%rsp, %rbp
LCFI15:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI16:
	ret
LFE4:
.no_dead_strip _Mod__Mod_var_u16_u32
_Mod__Mod_var_u16_u32:
LFB5:
	pushq	%rbp
LCFI17:
	movq	%rsp, %rbp
LCFI18:
	pushq	%rbx
LCFI19:
	subq	$8, %rsp
LCFI20:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L9
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L9
	addq	%rcx, %rdx
L9:
	cmpq	$65535, %rdx
	jle	L10
	movl	$1889, %edi
	call	__m3_fault
L10:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L11
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L11
	addq	%rcx, %rdx
L11:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI21:
	ret
LFE5:
.no_dead_strip _Mod__uMod_param_u16_u32
_Mod__uMod_param_u16_u32:
LFB6:
	pushq	%rbp
LCFI22:
	movq	%rsp, %rbp
LCFI23:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %eax
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI24:
	ret
LFE6:
.no_dead_strip _Mod__Mod_param_u16_u32
_Mod__Mod_param_u16_u32:
LFB7:
	pushq	%rbp
LCFI25:
	movq	%rsp, %rbp
LCFI26:
	pushq	%rbx
LCFI27:
	subq	$24, %rsp
LCFI28:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L14
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L14
	addq	%rcx, %rdx
L14:
	cmpq	$65535, %rdx
	jle	L15
	movl	$1953, %edi
	call	__m3_fault
L15:
	movzwl	-18(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L16
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L16
	addq	%rcx, %rdx
L16:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI29:
	ret
LFE7:
.no_dead_strip _Mod__uMod_var_u16_LC
_Mod__uMod_var_u16_LC:
LFB8:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI32:
	ret
LFE8:
.no_dead_strip _Mod__Mod_var_u16_LC
_Mod__Mod_var_u16_LC:
LFB9:
	pushq	%rbp
LCFI33:
	movq	%rsp, %rbp
LCFI34:
	pushq	%rbx
LCFI35:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L19
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L19
	addq	%rcx, %rdx
L19:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI36:
	ret
LFE9:
.no_dead_strip _Mod__uMod_param_u16_LC
_Mod__uMod_param_u16_LC:
LFB10:
	pushq	%rbp
LCFI37:
	movq	%rsp, %rbp
LCFI38:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI39:
	ret
LFE10:
.no_dead_strip _Mod__Mod_param_u16_LC
_Mod__Mod_param_u16_LC:
LFB11:
	pushq	%rbp
LCFI40:
	movq	%rsp, %rbp
LCFI41:
	pushq	%rbx
LCFI42:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L22
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L22
	addq	%rcx, %rdx
L22:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI43:
	ret
LFE11:
.no_dead_strip _Mod__uMod_var_u16_I
_Mod__uMod_var_u16_I:
LFB12:
	pushq	%rbp
LCFI44:
	movq	%rsp, %rbp
LCFI45:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI46:
	ret
LFE12:
.no_dead_strip _Mod__Mod_var_u16_I
_Mod__Mod_var_u16_I:
LFB13:
	pushq	%rbp
LCFI47:
	movq	%rsp, %rbp
LCFI48:
	pushq	%rbx
LCFI49:
	subq	$8, %rsp
LCFI50:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L25
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L25
	addq	%rcx, %rdx
L25:
	cmpq	$65535, %rdx
	jle	L26
	movl	$2145, %edi
	call	__m3_fault
L26:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L27
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L27
	addq	%rcx, %rdx
L27:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI51:
	ret
LFE13:
.no_dead_strip _Mod__uMod_param_u16_I
_Mod__uMod_param_u16_I:
LFB14:
	pushq	%rbp
LCFI52:
	movq	%rsp, %rbp
LCFI53:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI54:
	ret
LFE14:
.no_dead_strip _Mod__Mod_param_u16_I
_Mod__Mod_param_u16_I:
LFB15:
	pushq	%rbp
LCFI55:
	movq	%rsp, %rbp
LCFI56:
	subq	$16, %rsp
LCFI57:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L30
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L30
	addq	-16(%rbp), %rdx
L30:
	cmpq	$65535, %rdx
	jle	L31
	movl	$2209, %edi
	call	__m3_fault
L31:
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L32
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L32
	addq	-16(%rbp), %rdx
L32:
	movl	%edx, %eax
	leave
LCFI58:
	ret
LFE15:
.no_dead_strip _Mod__uMod_var_u16_i16
_Mod__uMod_var_u16_i16:
LFB16:
	pushq	%rbp
LCFI59:
	movq	%rsp, %rbp
LCFI60:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI61:
	ret
LFE16:
.no_dead_strip _Mod__Mod_var_u16_i16
_Mod__Mod_var_u16_i16:
LFB17:
	pushq	%rbp
LCFI62:
	movq	%rsp, %rbp
LCFI63:
	pushq	%rbx
LCFI64:
	subq	$8, %rsp
LCFI65:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L35
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L35
	addq	%rcx, %rdx
L35:
	cmpq	$65535, %rdx
	jle	L36
	movl	$2273, %edi
	call	__m3_fault
L36:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L37
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L37
	addq	%rcx, %rdx
L37:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI66:
	ret
LFE17:
.no_dead_strip _Mod__uMod_param_u16_i16
_Mod__uMod_param_u16_i16:
LFB18:
	pushq	%rbp
LCFI67:
	movq	%rsp, %rbp
LCFI68:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %eax
	movswq	-4(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI69:
	ret
LFE18:
.no_dead_strip _Mod__Mod_param_u16_i16
_Mod__Mod_param_u16_i16:
LFB19:
	pushq	%rbp
LCFI70:
	movq	%rsp, %rbp
LCFI71:
	pushq	%rbx
LCFI72:
	subq	$24, %rsp
LCFI73:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movzwl	-18(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L40
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L40
	addq	%rcx, %rdx
L40:
	cmpq	$65535, %rdx
	jle	L41
	movl	$2337, %edi
	call	__m3_fault
L41:
	movzwl	-18(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L42
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L42
	addq	%rcx, %rdx
L42:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI74:
	ret
LFE19:
.no_dead_strip _Mod__uMod_var_u16_u64
_Mod__uMod_var_u16_u64:
LFB20:
	pushq	%rbp
LCFI75:
	movq	%rsp, %rbp
LCFI76:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI77:
	ret
LFE20:
.no_dead_strip _Mod__Mod_var_u16_u64
_Mod__Mod_var_u16_u64:
LFB21:
	pushq	%rbp
LCFI78:
	movq	%rsp, %rbp
LCFI79:
	pushq	%rbx
LCFI80:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L45
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L45
	addq	%rcx, %rdx
L45:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI81:
	ret
LFE21:
.no_dead_strip _Mod__uMod_param_u16_u64
_Mod__uMod_param_u16_u64:
LFB22:
	pushq	%rbp
LCFI82:
	movq	%rsp, %rbp
LCFI83:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI84:
	ret
LFE22:
.no_dead_strip _Mod__Mod_param_u16_u64
_Mod__Mod_param_u16_u64:
LFB23:
	pushq	%rbp
LCFI85:
	movq	%rsp, %rbp
LCFI86:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L48
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L48
	addq	-16(%rbp), %rdx
L48:
	movq	%rdx, %rax
	leave
LCFI87:
	ret
LFE23:
.no_dead_strip _Mod__uMod_var_u16_i32
_Mod__uMod_var_u16_i32:
LFB24:
	pushq	%rbp
LCFI88:
	movq	%rsp, %rbp
LCFI89:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI90:
	ret
LFE24:
.no_dead_strip _Mod__Mod_var_u16_i32
_Mod__Mod_var_u16_i32:
LFB25:
	pushq	%rbp
LCFI91:
	movq	%rsp, %rbp
LCFI92:
	pushq	%rbx
LCFI93:
	subq	$8, %rsp
LCFI94:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L51
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L51
	addq	%rcx, %rdx
L51:
	cmpq	$65535, %rdx
	jle	L52
	movl	$2529, %edi
	call	__m3_fault
L52:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L53
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L53
	addq	%rcx, %rdx
L53:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI95:
	ret
LFE25:
.no_dead_strip _Mod__uMod_param_u16_i32
_Mod__uMod_param_u16_i32:
LFB26:
	pushq	%rbp
LCFI96:
	movq	%rsp, %rbp
LCFI97:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI98:
	ret
LFE26:
.no_dead_strip _Mod__Mod_param_u16_i32
_Mod__Mod_param_u16_i32:
LFB27:
	pushq	%rbp
LCFI99:
	movq	%rsp, %rbp
LCFI100:
	pushq	%rbx
LCFI101:
	subq	$24, %rsp
LCFI102:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movzwl	-18(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L56
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L56
	addq	%rcx, %rdx
L56:
	cmpq	$65535, %rdx
	jle	L57
	movl	$2593, %edi
	call	__m3_fault
L57:
	movzwl	-18(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L58
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L58
	addq	%rcx, %rdx
L58:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI103:
	ret
LFE27:
.no_dead_strip _Mod__uMod_var_u16_C
_Mod__uMod_var_u16_C:
LFB28:
	pushq	%rbp
LCFI104:
	movq	%rsp, %rbp
LCFI105:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI106:
	ret
LFE28:
.no_dead_strip _Mod__Mod_var_u16_C
_Mod__Mod_var_u16_C:
LFB29:
	pushq	%rbp
LCFI107:
	movq	%rsp, %rbp
LCFI108:
	pushq	%rbx
LCFI109:
	subq	$8, %rsp
LCFI110:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L61
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L61
	addq	%rcx, %rdx
L61:
	cmpq	$65535, %rdx
	jle	L62
	movl	$2657, %edi
	call	__m3_fault
L62:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L63
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L63
	addq	%rcx, %rdx
L63:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI111:
	ret
LFE29:
.no_dead_strip _Mod__uMod_param_u16_C
_Mod__uMod_param_u16_C:
LFB30:
	pushq	%rbp
LCFI112:
	movq	%rsp, %rbp
LCFI113:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI114:
	ret
LFE30:
.no_dead_strip _Mod__Mod_param_u16_C
_Mod__Mod_param_u16_C:
LFB31:
	pushq	%rbp
LCFI115:
	movq	%rsp, %rbp
LCFI116:
	pushq	%rbx
LCFI117:
	subq	$24, %rsp
LCFI118:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L66
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L66
	addq	%rcx, %rdx
L66:
	cmpq	$65535, %rdx
	jle	L67
	movl	$2721, %edi
	call	__m3_fault
L67:
	movzwl	-18(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L68
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L68
	addq	%rcx, %rdx
L68:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI119:
	ret
LFE31:
.no_dead_strip _Mod__uMod_var_u16_u8
_Mod__uMod_var_u16_u8:
LFB32:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI122:
	ret
LFE32:
.no_dead_strip _Mod__Mod_var_u16_u8
_Mod__Mod_var_u16_u8:
LFB33:
	pushq	%rbp
LCFI123:
	movq	%rsp, %rbp
LCFI124:
	pushq	%rbx
LCFI125:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L71
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L71
	addq	%rcx, %rdx
L71:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI126:
	ret
LFE33:
.no_dead_strip _Mod__uMod_param_u16_u8
_Mod__uMod_param_u16_u8:
LFB34:
	pushq	%rbp
LCFI127:
	movq	%rsp, %rbp
LCFI128:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %eax
	movzbl	-3(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI129:
	ret
LFE34:
.no_dead_strip _Mod__Mod_param_u16_u8
_Mod__Mod_param_u16_u8:
LFB35:
	pushq	%rbp
LCFI130:
	movq	%rsp, %rbp
LCFI131:
	pushq	%rbx
LCFI132:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %ebx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L74
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L74
	addq	%rcx, %rdx
L74:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI133:
	ret
LFE35:
.no_dead_strip _Mod__uMod_var_u16_i64
_Mod__uMod_var_u16_i64:
LFB36:
	pushq	%rbp
LCFI134:
	movq	%rsp, %rbp
LCFI135:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI136:
	ret
LFE36:
.no_dead_strip _Mod__Mod_var_u16_i64
_Mod__Mod_var_u16_i64:
LFB37:
	pushq	%rbp
LCFI137:
	movq	%rsp, %rbp
LCFI138:
	pushq	%rbx
LCFI139:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L77
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L77
	addq	%rcx, %rdx
L77:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI140:
	ret
LFE37:
.no_dead_strip _Mod__uMod_param_u16_i64
_Mod__uMod_param_u16_i64:
LFB38:
	pushq	%rbp
LCFI141:
	movq	%rsp, %rbp
LCFI142:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI143:
	ret
LFE38:
.no_dead_strip _Mod__Mod_param_u16_i64
_Mod__Mod_param_u16_i64:
LFB39:
	pushq	%rbp
LCFI144:
	movq	%rsp, %rbp
LCFI145:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L80
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L80
	addq	-16(%rbp), %rdx
L80:
	movq	%rdx, %rax
	leave
LCFI146:
	ret
LFE39:
.no_dead_strip _Mod__uMod_var_u16_L
_Mod__uMod_var_u16_L:
LFB40:
	pushq	%rbp
LCFI147:
	movq	%rsp, %rbp
LCFI148:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI149:
	ret
LFE40:
.no_dead_strip _Mod__Mod_var_u16_L
_Mod__Mod_var_u16_L:
LFB41:
	pushq	%rbp
LCFI150:
	movq	%rsp, %rbp
LCFI151:
	pushq	%rbx
LCFI152:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L83
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L83
	addq	%rcx, %rdx
L83:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI153:
	ret
LFE41:
.no_dead_strip _Mod__uMod_param_u16_L
_Mod__uMod_param_u16_L:
LFB42:
	pushq	%rbp
LCFI154:
	movq	%rsp, %rbp
LCFI155:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI156:
	ret
LFE42:
.no_dead_strip _Mod__Mod_param_u16_L
_Mod__Mod_param_u16_L:
LFB43:
	pushq	%rbp
LCFI157:
	movq	%rsp, %rbp
LCFI158:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L86
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L86
	addq	-16(%rbp), %rdx
L86:
	movq	%rdx, %rax
	leave
LCFI159:
	ret
LFE43:
.no_dead_strip _Mod__uMod_var_u16_i8
_Mod__uMod_var_u16_i8:
LFB44:
	pushq	%rbp
LCFI160:
	movq	%rsp, %rbp
LCFI161:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %eax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI162:
	ret
LFE44:
.no_dead_strip _Mod__Mod_var_u16_i8
_Mod__Mod_var_u16_i8:
LFB45:
	pushq	%rbp
LCFI163:
	movq	%rsp, %rbp
LCFI164:
	pushq	%rbx
LCFI165:
	subq	$8, %rsp
LCFI166:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L89
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L89
	addq	%rcx, %rdx
L89:
	cmpq	$65535, %rdx
	jle	L90
	movl	$3169, %edi
	call	__m3_fault
L90:
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L91
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L91
	addq	%rcx, %rdx
L91:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI167:
	ret
LFE45:
.no_dead_strip _Mod__uMod_param_u16_i8
_Mod__uMod_param_u16_i8:
LFB46:
	pushq	%rbp
LCFI168:
	movq	%rsp, %rbp
LCFI169:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %eax
	movsbq	-3(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI170:
	ret
LFE46:
.no_dead_strip _Mod__Mod_param_u16_i8
_Mod__Mod_param_u16_i8:
LFB47:
	pushq	%rbp
LCFI171:
	movq	%rsp, %rbp
LCFI172:
	pushq	%rbx
LCFI173:
	subq	$24, %rsp
LCFI174:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movzwl	-18(%rbp), %ebx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L94
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L94
	addq	%rcx, %rdx
L94:
	cmpq	$65535, %rdx
	jle	L95
	movl	$3233, %edi
	call	__m3_fault
L95:
	movzwl	-18(%rbp), %ebx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L96
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L96
	addq	%rcx, %rdx
L96:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI175:
	ret
LFE47:
.no_dead_strip _Mod__uMod_var_u32_u16
_Mod__uMod_var_u32_u16:
LFB48:
	pushq	%rbp
LCFI176:
	movq	%rsp, %rbp
LCFI177:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI178:
	ret
LFE48:
.no_dead_strip _Mod__Mod_var_u32_u16
_Mod__Mod_var_u32_u16:
LFB49:
	pushq	%rbp
LCFI179:
	movq	%rsp, %rbp
LCFI180:
	pushq	%rbx
LCFI181:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L99
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L99
	addq	%rcx, %rdx
L99:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI182:
	ret
LFE49:
.no_dead_strip _Mod__uMod_param_u32_u16
_Mod__uMod_param_u32_u16:
LFB50:
	pushq	%rbp
LCFI183:
	movq	%rsp, %rbp
LCFI184:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %eax
	movzwl	-6(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI185:
	ret
LFE50:
.no_dead_strip _Mod__Mod_param_u32_u16
_Mod__Mod_param_u32_u16:
LFB51:
	pushq	%rbp
LCFI186:
	movq	%rsp, %rbp
LCFI187:
	pushq	%rbx
LCFI188:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %ebx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L102
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L102
	addq	%rcx, %rdx
L102:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI189:
	ret
LFE51:
.no_dead_strip _Mod__uMod_var_u32_u32
_Mod__uMod_var_u32_u32:
LFB52:
	pushq	%rbp
LCFI190:
	movq	%rsp, %rbp
LCFI191:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI192:
	ret
LFE52:
.no_dead_strip _Mod__Mod_var_u32_u32
_Mod__Mod_var_u32_u32:
LFB53:
	pushq	%rbp
LCFI193:
	movq	%rsp, %rbp
LCFI194:
	pushq	%rbx
LCFI195:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L105
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L105
	addq	%rcx, %rdx
L105:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI196:
	ret
LFE53:
.no_dead_strip _Mod__uMod_param_u32_u32
_Mod__uMod_param_u32_u32:
LFB54:
	pushq	%rbp
LCFI197:
	movq	%rsp, %rbp
LCFI198:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %eax
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI199:
	ret
LFE54:
.no_dead_strip _Mod__Mod_param_u32_u32
_Mod__Mod_param_u32_u32:
LFB55:
	pushq	%rbp
LCFI200:
	movq	%rsp, %rbp
LCFI201:
	pushq	%rbx
LCFI202:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L108
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L108
	addq	%rcx, %rdx
L108:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI203:
	ret
LFE55:
.no_dead_strip _Mod__uMod_var_u32_LC
_Mod__uMod_var_u32_LC:
LFB56:
	pushq	%rbp
LCFI204:
	movq	%rsp, %rbp
LCFI205:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI206:
	ret
LFE56:
.no_dead_strip _Mod__Mod_var_u32_LC
_Mod__Mod_var_u32_LC:
LFB57:
	pushq	%rbp
LCFI207:
	movq	%rsp, %rbp
LCFI208:
	pushq	%rbx
LCFI209:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L111
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L111
	addq	%rcx, %rdx
L111:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI210:
	ret
LFE57:
.no_dead_strip _Mod__uMod_param_u32_LC
_Mod__uMod_param_u32_LC:
LFB58:
	pushq	%rbp
LCFI211:
	movq	%rsp, %rbp
LCFI212:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI213:
	ret
LFE58:
.no_dead_strip _Mod__Mod_param_u32_LC
_Mod__Mod_param_u32_LC:
LFB59:
	pushq	%rbp
LCFI214:
	movq	%rsp, %rbp
LCFI215:
	pushq	%rbx
LCFI216:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L114
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L114
	addq	%rcx, %rdx
L114:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI217:
	ret
LFE59:
.no_dead_strip _Mod__uMod_var_u32_I
_Mod__uMod_var_u32_I:
LFB60:
	pushq	%rbp
LCFI218:
	movq	%rsp, %rbp
LCFI219:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI220:
	ret
LFE60:
.no_dead_strip _Mod__Mod_var_u32_I
_Mod__Mod_var_u32_I:
LFB61:
	pushq	%rbp
LCFI221:
	movq	%rsp, %rbp
LCFI222:
	pushq	%rbx
LCFI223:
	subq	$8, %rsp
LCFI224:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L117
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L117
	addq	%rcx, %rdx
L117:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L118
	movl	$3681, %edi
	call	__m3_fault
L118:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L119
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L119
	addq	%rcx, %rdx
L119:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI225:
	ret
LFE61:
.no_dead_strip _Mod__uMod_param_u32_I
_Mod__uMod_param_u32_I:
LFB62:
	pushq	%rbp
LCFI226:
	movq	%rsp, %rbp
LCFI227:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI228:
	ret
LFE62:
.no_dead_strip _Mod__Mod_param_u32_I
_Mod__Mod_param_u32_I:
LFB63:
	pushq	%rbp
LCFI229:
	movq	%rsp, %rbp
LCFI230:
	subq	$16, %rsp
LCFI231:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L122
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L122
	addq	-16(%rbp), %rdx
L122:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L123
	movl	$3745, %edi
	call	__m3_fault
L123:
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L124
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L124
	addq	-16(%rbp), %rdx
L124:
	movl	%edx, %eax
	leave
LCFI232:
	ret
LFE63:
.no_dead_strip _Mod__uMod_var_u32_i16
_Mod__uMod_var_u32_i16:
LFB64:
	pushq	%rbp
LCFI233:
	movq	%rsp, %rbp
LCFI234:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI235:
	ret
LFE64:
.no_dead_strip _Mod__Mod_var_u32_i16
_Mod__Mod_var_u32_i16:
LFB65:
	pushq	%rbp
LCFI236:
	movq	%rsp, %rbp
LCFI237:
	pushq	%rbx
LCFI238:
	subq	$8, %rsp
LCFI239:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L127
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L127
	addq	%rcx, %rdx
L127:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L128
	movl	$3809, %edi
	call	__m3_fault
L128:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L129
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L129
	addq	%rcx, %rdx
L129:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI240:
	ret
LFE65:
.no_dead_strip _Mod__uMod_param_u32_i16
_Mod__uMod_param_u32_i16:
LFB66:
	pushq	%rbp
LCFI241:
	movq	%rsp, %rbp
LCFI242:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %eax
	movswq	-6(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI243:
	ret
LFE66:
.no_dead_strip _Mod__Mod_param_u32_i16
_Mod__Mod_param_u32_i16:
LFB67:
	pushq	%rbp
LCFI244:
	movq	%rsp, %rbp
LCFI245:
	pushq	%rbx
LCFI246:
	subq	$24, %rsp
LCFI247:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	mov	-20(%rbp), %ebx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L132
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L132
	addq	%rcx, %rdx
L132:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L133
	movl	$3873, %edi
	call	__m3_fault
L133:
	mov	-20(%rbp), %ebx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L134
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L134
	addq	%rcx, %rdx
L134:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI248:
	ret
LFE67:
.no_dead_strip _Mod__uMod_var_u32_u64
_Mod__uMod_var_u32_u64:
LFB68:
	pushq	%rbp
LCFI249:
	movq	%rsp, %rbp
LCFI250:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI251:
	ret
LFE68:
.no_dead_strip _Mod__Mod_var_u32_u64
_Mod__Mod_var_u32_u64:
LFB69:
	pushq	%rbp
LCFI252:
	movq	%rsp, %rbp
LCFI253:
	pushq	%rbx
LCFI254:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L137
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L137
	addq	%rcx, %rdx
L137:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI255:
	ret
LFE69:
.no_dead_strip _Mod__uMod_param_u32_u64
_Mod__uMod_param_u32_u64:
LFB70:
	pushq	%rbp
LCFI256:
	movq	%rsp, %rbp
LCFI257:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI258:
	ret
LFE70:
.no_dead_strip _Mod__Mod_param_u32_u64
_Mod__Mod_param_u32_u64:
LFB71:
	pushq	%rbp
LCFI259:
	movq	%rsp, %rbp
LCFI260:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L140
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L140
	addq	-16(%rbp), %rdx
L140:
	movq	%rdx, %rax
	leave
LCFI261:
	ret
LFE71:
.no_dead_strip _Mod__uMod_var_u32_i32
_Mod__uMod_var_u32_i32:
LFB72:
	pushq	%rbp
LCFI262:
	movq	%rsp, %rbp
LCFI263:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI264:
	ret
LFE72:
.no_dead_strip _Mod__Mod_var_u32_i32
_Mod__Mod_var_u32_i32:
LFB73:
	pushq	%rbp
LCFI265:
	movq	%rsp, %rbp
LCFI266:
	pushq	%rbx
LCFI267:
	subq	$8, %rsp
LCFI268:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L143
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L143
	addq	%rcx, %rdx
L143:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L144
	movl	$4065, %edi
	call	__m3_fault
L144:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L145
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L145
	addq	%rcx, %rdx
L145:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI269:
	ret
LFE73:
.no_dead_strip _Mod__uMod_param_u32_i32
_Mod__uMod_param_u32_i32:
LFB74:
	pushq	%rbp
LCFI270:
	movq	%rsp, %rbp
LCFI271:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI272:
	ret
LFE74:
.no_dead_strip _Mod__Mod_param_u32_i32
_Mod__Mod_param_u32_i32:
LFB75:
	pushq	%rbp
LCFI273:
	movq	%rsp, %rbp
LCFI274:
	pushq	%rbx
LCFI275:
	subq	$24, %rsp
LCFI276:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	mov	-20(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L148
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L148
	addq	%rcx, %rdx
L148:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L149
	movl	$4129, %edi
	call	__m3_fault
L149:
	mov	-20(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L150
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L150
	addq	%rcx, %rdx
L150:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI277:
	ret
LFE75:
.no_dead_strip _Mod__uMod_var_u32_C
_Mod__uMod_var_u32_C:
LFB76:
	pushq	%rbp
LCFI278:
	movq	%rsp, %rbp
LCFI279:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI280:
	ret
LFE76:
.no_dead_strip _Mod__Mod_var_u32_C
_Mod__Mod_var_u32_C:
LFB77:
	pushq	%rbp
LCFI281:
	movq	%rsp, %rbp
LCFI282:
	pushq	%rbx
LCFI283:
	subq	$8, %rsp
LCFI284:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L153
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L153
	addq	%rcx, %rdx
L153:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L154
	movl	$4193, %edi
	call	__m3_fault
L154:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L155
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L155
	addq	%rcx, %rdx
L155:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI285:
	ret
LFE77:
.no_dead_strip _Mod__uMod_param_u32_C
_Mod__uMod_param_u32_C:
LFB78:
	pushq	%rbp
LCFI286:
	movq	%rsp, %rbp
LCFI287:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI288:
	ret
LFE78:
.no_dead_strip _Mod__Mod_param_u32_C
_Mod__Mod_param_u32_C:
LFB79:
	pushq	%rbp
LCFI289:
	movq	%rsp, %rbp
LCFI290:
	pushq	%rbx
LCFI291:
	subq	$24, %rsp
LCFI292:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L158
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L158
	addq	%rcx, %rdx
L158:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L159
	movl	$4257, %edi
	call	__m3_fault
L159:
	mov	-20(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L160
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L160
	addq	%rcx, %rdx
L160:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI293:
	ret
LFE79:
.no_dead_strip _Mod__uMod_var_u32_u8
_Mod__uMod_var_u32_u8:
LFB80:
	pushq	%rbp
LCFI294:
	movq	%rsp, %rbp
LCFI295:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI296:
	ret
LFE80:
.no_dead_strip _Mod__Mod_var_u32_u8
_Mod__Mod_var_u32_u8:
LFB81:
	pushq	%rbp
LCFI297:
	movq	%rsp, %rbp
LCFI298:
	pushq	%rbx
LCFI299:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L163
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L163
	addq	%rcx, %rdx
L163:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI300:
	ret
LFE81:
.no_dead_strip _Mod__uMod_param_u32_u8
_Mod__uMod_param_u32_u8:
LFB82:
	pushq	%rbp
LCFI301:
	movq	%rsp, %rbp
LCFI302:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %eax
	movzbl	-5(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI303:
	ret
LFE82:
.no_dead_strip _Mod__Mod_param_u32_u8
_Mod__Mod_param_u32_u8:
LFB83:
	pushq	%rbp
LCFI304:
	movq	%rsp, %rbp
LCFI305:
	pushq	%rbx
LCFI306:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %ebx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L166
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L166
	addq	%rcx, %rdx
L166:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI307:
	ret
LFE83:
.no_dead_strip _Mod__uMod_var_u32_i64
_Mod__uMod_var_u32_i64:
LFB84:
	pushq	%rbp
LCFI308:
	movq	%rsp, %rbp
LCFI309:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI310:
	ret
LFE84:
.no_dead_strip _Mod__Mod_var_u32_i64
_Mod__Mod_var_u32_i64:
LFB85:
	pushq	%rbp
LCFI311:
	movq	%rsp, %rbp
LCFI312:
	pushq	%rbx
LCFI313:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L169
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L169
	addq	%rcx, %rdx
L169:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI314:
	ret
LFE85:
.no_dead_strip _Mod__uMod_param_u32_i64
_Mod__uMod_param_u32_i64:
LFB86:
	pushq	%rbp
LCFI315:
	movq	%rsp, %rbp
LCFI316:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI317:
	ret
LFE86:
.no_dead_strip _Mod__Mod_param_u32_i64
_Mod__Mod_param_u32_i64:
LFB87:
	pushq	%rbp
LCFI318:
	movq	%rsp, %rbp
LCFI319:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L172
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L172
	addq	-16(%rbp), %rdx
L172:
	movq	%rdx, %rax
	leave
LCFI320:
	ret
LFE87:
.no_dead_strip _Mod__uMod_var_u32_L
_Mod__uMod_var_u32_L:
LFB88:
	pushq	%rbp
LCFI321:
	movq	%rsp, %rbp
LCFI322:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI323:
	ret
LFE88:
.no_dead_strip _Mod__Mod_var_u32_L
_Mod__Mod_var_u32_L:
LFB89:
	pushq	%rbp
LCFI324:
	movq	%rsp, %rbp
LCFI325:
	pushq	%rbx
LCFI326:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L175
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L175
	addq	%rcx, %rdx
L175:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI327:
	ret
LFE89:
.no_dead_strip _Mod__uMod_param_u32_L
_Mod__uMod_param_u32_L:
LFB90:
	pushq	%rbp
LCFI328:
	movq	%rsp, %rbp
LCFI329:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI330:
	ret
LFE90:
.no_dead_strip _Mod__Mod_param_u32_L
_Mod__Mod_param_u32_L:
LFB91:
	pushq	%rbp
LCFI331:
	movq	%rsp, %rbp
LCFI332:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L178
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L178
	addq	-16(%rbp), %rdx
L178:
	movq	%rdx, %rax
	leave
LCFI333:
	ret
LFE91:
.no_dead_strip _Mod__uMod_var_u32_i8
_Mod__uMod_var_u32_i8:
LFB92:
	pushq	%rbp
LCFI334:
	movq	%rsp, %rbp
LCFI335:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %eax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI336:
	ret
LFE92:
.no_dead_strip _Mod__Mod_var_u32_i8
_Mod__Mod_var_u32_i8:
LFB93:
	pushq	%rbp
LCFI337:
	movq	%rsp, %rbp
LCFI338:
	pushq	%rbx
LCFI339:
	subq	$8, %rsp
LCFI340:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L181
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L181
	addq	%rcx, %rdx
L181:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L182
	movl	$4705, %edi
	call	__m3_fault
L182:
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L183
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L183
	addq	%rcx, %rdx
L183:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI341:
	ret
LFE93:
.no_dead_strip _Mod__uMod_param_u32_i8
_Mod__uMod_param_u32_i8:
LFB94:
	pushq	%rbp
LCFI342:
	movq	%rsp, %rbp
LCFI343:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %eax
	movsbq	-5(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI344:
	ret
LFE94:
.no_dead_strip _Mod__Mod_param_u32_i8
_Mod__Mod_param_u32_i8:
LFB95:
	pushq	%rbp
LCFI345:
	movq	%rsp, %rbp
LCFI346:
	pushq	%rbx
LCFI347:
	subq	$24, %rsp
LCFI348:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	mov	-20(%rbp), %ebx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L186
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L186
	addq	%rcx, %rdx
L186:
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L187
	movl	$4769, %edi
	call	__m3_fault
L187:
	mov	-20(%rbp), %ebx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L188
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L188
	addq	%rcx, %rdx
L188:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI349:
	ret
LFE95:
.no_dead_strip _Mod__uMod_var_LC_u16
_Mod__uMod_var_LC_u16:
LFB96:
	pushq	%rbp
LCFI350:
	movq	%rsp, %rbp
LCFI351:
	movq	112+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI352:
	ret
LFE96:
.no_dead_strip _Mod__Mod_var_LC_u16
_Mod__Mod_var_LC_u16:
LFB97:
	pushq	%rbp
LCFI353:
	movq	%rsp, %rbp
LCFI354:
	pushq	%rbx
LCFI355:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L191
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L191
	addq	%rcx, %rdx
L191:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI356:
	ret
LFE97:
.no_dead_strip _Mod__uMod_param_LC_u16
_Mod__uMod_param_LC_u16:
LFB98:
	pushq	%rbp
LCFI357:
	movq	%rsp, %rbp
LCFI358:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI359:
	ret
LFE98:
.no_dead_strip _Mod__Mod_param_LC_u16
_Mod__Mod_param_LC_u16:
LFB99:
	pushq	%rbp
LCFI360:
	movq	%rsp, %rbp
LCFI361:
	pushq	%rbx
LCFI362:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movzwl	-26(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L194
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L194
	addq	%rcx, %rdx
L194:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI363:
	ret
LFE99:
.no_dead_strip _Mod__uMod_var_LC_u32
_Mod__uMod_var_LC_u32:
LFB100:
	pushq	%rbp
LCFI364:
	movq	%rsp, %rbp
LCFI365:
	movq	112+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI366:
	ret
LFE100:
.no_dead_strip _Mod__Mod_var_LC_u32
_Mod__Mod_var_LC_u32:
LFB101:
	pushq	%rbp
LCFI367:
	movq	%rsp, %rbp
LCFI368:
	pushq	%rbx
LCFI369:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L197
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L197
	addq	%rcx, %rdx
L197:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI370:
	ret
LFE101:
.no_dead_strip _Mod__uMod_param_LC_u32
_Mod__uMod_param_LC_u32:
LFB102:
	pushq	%rbp
LCFI371:
	movq	%rsp, %rbp
LCFI372:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI373:
	ret
LFE102:
.no_dead_strip _Mod__Mod_param_LC_u32
_Mod__Mod_param_LC_u32:
LFB103:
	pushq	%rbp
LCFI374:
	movq	%rsp, %rbp
LCFI375:
	pushq	%rbx
LCFI376:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	mov	-28(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L200
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L200
	addq	%rcx, %rdx
L200:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI377:
	ret
LFE103:
.no_dead_strip _Mod__uMod_var_LC_LC
_Mod__uMod_var_LC_LC:
LFB104:
	pushq	%rbp
LCFI378:
	movq	%rsp, %rbp
LCFI379:
	movq	112+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI380:
	ret
LFE104:
.no_dead_strip _Mod__Mod_var_LC_LC
_Mod__Mod_var_LC_LC:
LFB105:
	pushq	%rbp
LCFI381:
	movq	%rsp, %rbp
LCFI382:
	pushq	%rbx
LCFI383:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L203
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L203
	addq	%rcx, %rdx
L203:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI384:
	ret
LFE105:
.no_dead_strip _Mod__uMod_param_LC_LC
_Mod__uMod_param_LC_LC:
LFB106:
	pushq	%rbp
LCFI385:
	movq	%rsp, %rbp
LCFI386:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI387:
	ret
LFE106:
.no_dead_strip _Mod__Mod_param_LC_LC
_Mod__Mod_param_LC_LC:
LFB107:
	pushq	%rbp
LCFI388:
	movq	%rsp, %rbp
LCFI389:
	pushq	%rbx
LCFI390:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L206
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L206
	addq	%rcx, %rdx
L206:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI391:
	ret
LFE107:
.no_dead_strip _Mod__uMod_var_LC_I
_Mod__uMod_var_LC_I:
LFB108:
	pushq	%rbp
LCFI392:
	movq	%rsp, %rbp
LCFI393:
	movq	112+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI394:
	ret
LFE108:
.no_dead_strip _Mod__Mod_var_LC_I
_Mod__Mod_var_LC_I:
LFB109:
	pushq	%rbp
LCFI395:
	movq	%rsp, %rbp
LCFI396:
	pushq	%rbx
LCFI397:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L209
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L209
	addq	%rcx, %rdx
L209:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI398:
	ret
LFE109:
.no_dead_strip _Mod__uMod_param_LC_I
_Mod__uMod_param_LC_I:
LFB110:
	pushq	%rbp
LCFI399:
	movq	%rsp, %rbp
LCFI400:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI401:
	ret
LFE110:
.no_dead_strip _Mod__Mod_param_LC_I
_Mod__Mod_param_LC_I:
LFB111:
	pushq	%rbp
LCFI402:
	movq	%rsp, %rbp
LCFI403:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L212
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L212
	addq	-16(%rbp), %rdx
L212:
	movq	%rdx, %rax
	leave
LCFI404:
	ret
LFE111:
.no_dead_strip _Mod__uMod_var_LC_i16
_Mod__uMod_var_LC_i16:
LFB112:
	pushq	%rbp
LCFI405:
	movq	%rsp, %rbp
LCFI406:
	movq	112+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI407:
	ret
LFE112:
.no_dead_strip _Mod__Mod_var_LC_i16
_Mod__Mod_var_LC_i16:
LFB113:
	pushq	%rbp
LCFI408:
	movq	%rsp, %rbp
LCFI409:
	pushq	%rbx
LCFI410:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L215
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L215
	addq	%rcx, %rdx
L215:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI411:
	ret
LFE113:
.no_dead_strip _Mod__uMod_param_LC_i16
_Mod__uMod_param_LC_i16:
LFB114:
	pushq	%rbp
LCFI412:
	movq	%rsp, %rbp
LCFI413:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI414:
	ret
LFE114:
.no_dead_strip _Mod__Mod_param_LC_i16
_Mod__Mod_param_LC_i16:
LFB115:
	pushq	%rbp
LCFI415:
	movq	%rsp, %rbp
LCFI416:
	pushq	%rbx
LCFI417:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L218
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L218
	addq	%rcx, %rdx
L218:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI418:
	ret
LFE115:
.no_dead_strip _Mod__uMod_var_LC_u64
_Mod__uMod_var_LC_u64:
LFB116:
	pushq	%rbp
LCFI419:
	movq	%rsp, %rbp
LCFI420:
	movq	112+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI421:
	ret
LFE116:
.no_dead_strip _Mod__Mod_var_LC_u64
_Mod__Mod_var_LC_u64:
LFB117:
	pushq	%rbp
LCFI422:
	movq	%rsp, %rbp
LCFI423:
	pushq	%rbx
LCFI424:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L221
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L221
	addq	%rcx, %rdx
L221:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI425:
	ret
LFE117:
.no_dead_strip _Mod__uMod_param_LC_u64
_Mod__uMod_param_LC_u64:
LFB118:
	pushq	%rbp
LCFI426:
	movq	%rsp, %rbp
LCFI427:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI428:
	ret
LFE118:
.no_dead_strip _Mod__Mod_param_LC_u64
_Mod__Mod_param_LC_u64:
LFB119:
	pushq	%rbp
LCFI429:
	movq	%rsp, %rbp
LCFI430:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L224
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L224
	addq	-16(%rbp), %rdx
L224:
	movq	%rdx, %rax
	leave
LCFI431:
	ret
LFE119:
.no_dead_strip _Mod__uMod_var_LC_i32
_Mod__uMod_var_LC_i32:
LFB120:
	pushq	%rbp
LCFI432:
	movq	%rsp, %rbp
LCFI433:
	movq	112+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI434:
	ret
LFE120:
.no_dead_strip _Mod__Mod_var_LC_i32
_Mod__Mod_var_LC_i32:
LFB121:
	pushq	%rbp
LCFI435:
	movq	%rsp, %rbp
LCFI436:
	pushq	%rbx
LCFI437:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L227
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L227
	addq	%rcx, %rdx
L227:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI438:
	ret
LFE121:
.no_dead_strip _Mod__uMod_param_LC_i32
_Mod__uMod_param_LC_i32:
LFB122:
	pushq	%rbp
LCFI439:
	movq	%rsp, %rbp
LCFI440:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movslq	%eax, %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI441:
	ret
LFE122:
.no_dead_strip _Mod__Mod_param_LC_i32
_Mod__Mod_param_LC_i32:
LFB123:
	pushq	%rbp
LCFI442:
	movq	%rsp, %rbp
LCFI443:
	pushq	%rbx
LCFI444:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L230
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L230
	addq	%rcx, %rdx
L230:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI445:
	ret
LFE123:
.no_dead_strip _Mod__uMod_var_LC_C
_Mod__uMod_var_LC_C:
LFB124:
	pushq	%rbp
LCFI446:
	movq	%rsp, %rbp
LCFI447:
	movq	112+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI448:
	ret
LFE124:
.no_dead_strip _Mod__Mod_var_LC_C
_Mod__Mod_var_LC_C:
LFB125:
	pushq	%rbp
LCFI449:
	movq	%rsp, %rbp
LCFI450:
	pushq	%rbx
LCFI451:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L233
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L233
	addq	%rcx, %rdx
L233:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI452:
	ret
LFE125:
.no_dead_strip _Mod__uMod_param_LC_C
_Mod__uMod_param_LC_C:
LFB126:
	pushq	%rbp
LCFI453:
	movq	%rsp, %rbp
LCFI454:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI455:
	ret
LFE126:
.no_dead_strip _Mod__Mod_param_LC_C
_Mod__Mod_param_LC_C:
LFB127:
	pushq	%rbp
LCFI456:
	movq	%rsp, %rbp
LCFI457:
	pushq	%rbx
LCFI458:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L236
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L236
	addq	%rcx, %rdx
L236:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI459:
	ret
LFE127:
.no_dead_strip _Mod__uMod_var_LC_u8
_Mod__uMod_var_LC_u8:
LFB128:
	pushq	%rbp
LCFI460:
	movq	%rsp, %rbp
LCFI461:
	movq	112+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI462:
	ret
LFE128:
.no_dead_strip _Mod__Mod_var_LC_u8
_Mod__Mod_var_LC_u8:
LFB129:
	pushq	%rbp
LCFI463:
	movq	%rsp, %rbp
LCFI464:
	pushq	%rbx
LCFI465:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L239
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L239
	addq	%rcx, %rdx
L239:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI466:
	ret
LFE129:
.no_dead_strip _Mod__uMod_param_LC_u8
_Mod__uMod_param_LC_u8:
LFB130:
	pushq	%rbp
LCFI467:
	movq	%rsp, %rbp
LCFI468:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI469:
	ret
LFE130:
.no_dead_strip _Mod__Mod_param_LC_u8
_Mod__Mod_param_LC_u8:
LFB131:
	pushq	%rbp
LCFI470:
	movq	%rsp, %rbp
LCFI471:
	pushq	%rbx
LCFI472:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movzbl	-25(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L242
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L242
	addq	%rcx, %rdx
L242:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI473:
	ret
LFE131:
.no_dead_strip _Mod__uMod_var_LC_i64
_Mod__uMod_var_LC_i64:
LFB132:
	pushq	%rbp
LCFI474:
	movq	%rsp, %rbp
LCFI475:
	movq	112+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI476:
	ret
LFE132:
.no_dead_strip _Mod__Mod_var_LC_i64
_Mod__Mod_var_LC_i64:
LFB133:
	pushq	%rbp
LCFI477:
	movq	%rsp, %rbp
LCFI478:
	pushq	%rbx
LCFI479:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L245
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L245
	addq	%rcx, %rdx
L245:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI480:
	ret
LFE133:
.no_dead_strip _Mod__uMod_param_LC_i64
_Mod__uMod_param_LC_i64:
LFB134:
	pushq	%rbp
LCFI481:
	movq	%rsp, %rbp
LCFI482:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI483:
	ret
LFE134:
.no_dead_strip _Mod__Mod_param_LC_i64
_Mod__Mod_param_LC_i64:
LFB135:
	pushq	%rbp
LCFI484:
	movq	%rsp, %rbp
LCFI485:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L248
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L248
	addq	-16(%rbp), %rdx
L248:
	movq	%rdx, %rax
	leave
LCFI486:
	ret
LFE135:
.no_dead_strip _Mod__uMod_var_LC_L
_Mod__uMod_var_LC_L:
LFB136:
	pushq	%rbp
LCFI487:
	movq	%rsp, %rbp
LCFI488:
	movq	112+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI489:
	ret
LFE136:
.no_dead_strip _Mod__Mod_var_LC_L
_Mod__Mod_var_LC_L:
LFB137:
	pushq	%rbp
LCFI490:
	movq	%rsp, %rbp
LCFI491:
	pushq	%rbx
LCFI492:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L251
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L251
	addq	%rcx, %rdx
L251:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI493:
	ret
LFE137:
.no_dead_strip _Mod__uMod_param_LC_L
_Mod__uMod_param_LC_L:
LFB138:
	pushq	%rbp
LCFI494:
	movq	%rsp, %rbp
LCFI495:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI496:
	ret
LFE138:
.no_dead_strip _Mod__Mod_param_LC_L
_Mod__Mod_param_LC_L:
LFB139:
	pushq	%rbp
LCFI497:
	movq	%rsp, %rbp
LCFI498:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L254
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L254
	addq	-16(%rbp), %rdx
L254:
	movq	%rdx, %rax
	leave
LCFI499:
	ret
LFE139:
.no_dead_strip _Mod__uMod_var_LC_i8
_Mod__uMod_var_LC_i8:
LFB140:
	pushq	%rbp
LCFI500:
	movq	%rsp, %rbp
LCFI501:
	movq	112+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI502:
	ret
LFE140:
.no_dead_strip _Mod__Mod_var_LC_i8
_Mod__Mod_var_LC_i8:
LFB141:
	pushq	%rbp
LCFI503:
	movq	%rsp, %rbp
LCFI504:
	pushq	%rbx
LCFI505:
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L257
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L257
	addq	%rcx, %rdx
L257:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI506:
	ret
LFE141:
.no_dead_strip _Mod__uMod_param_LC_i8
_Mod__uMod_param_LC_i8:
LFB142:
	pushq	%rbp
LCFI507:
	movq	%rsp, %rbp
LCFI508:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI509:
	ret
LFE142:
.no_dead_strip _Mod__Mod_param_LC_i8
_Mod__Mod_param_LC_i8:
LFB143:
	pushq	%rbp
LCFI510:
	movq	%rsp, %rbp
LCFI511:
	pushq	%rbx
LCFI512:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L260
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L260
	addq	%rcx, %rdx
L260:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI513:
	ret
LFE143:
.no_dead_strip _Mod__uMod_var_I_u16
_Mod__uMod_var_I_u16:
LFB144:
	pushq	%rbp
LCFI514:
	movq	%rsp, %rbp
LCFI515:
	movq	120+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI516:
	ret
LFE144:
.no_dead_strip _Mod__Mod_var_I_u16
_Mod__Mod_var_I_u16:
LFB145:
	pushq	%rbp
LCFI517:
	movq	%rsp, %rbp
LCFI518:
	pushq	%rbx
LCFI519:
	movq	120+_MM_Mod(%rip), %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L263
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L263
	addq	%rcx, %rdx
L263:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI520:
	ret
LFE145:
.no_dead_strip _Mod__uMod_param_I_u16
_Mod__uMod_param_I_u16:
LFB146:
	pushq	%rbp
LCFI521:
	movq	%rsp, %rbp
LCFI522:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI523:
	ret
LFE146:
.no_dead_strip _Mod__Mod_param_I_u16
_Mod__Mod_param_I_u16:
LFB147:
	pushq	%rbp
LCFI524:
	movq	%rsp, %rbp
LCFI525:
	pushq	%rbx
LCFI526:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L266
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L266
	addq	%rcx, %rdx
L266:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI527:
	ret
LFE147:
.no_dead_strip _Mod__uMod_var_I_u32
_Mod__uMod_var_I_u32:
LFB148:
	pushq	%rbp
LCFI528:
	movq	%rsp, %rbp
LCFI529:
	movq	120+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI530:
	ret
LFE148:
.no_dead_strip _Mod__Mod_var_I_u32
_Mod__Mod_var_I_u32:
LFB149:
	pushq	%rbp
LCFI531:
	movq	%rsp, %rbp
LCFI532:
	pushq	%rbx
LCFI533:
	movq	120+_MM_Mod(%rip), %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L269
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L269
	addq	%rcx, %rdx
L269:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI534:
	ret
LFE149:
.no_dead_strip _Mod__uMod_param_I_u32
_Mod__uMod_param_I_u32:
LFB150:
	pushq	%rbp
LCFI535:
	movq	%rsp, %rbp
LCFI536:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI537:
	ret
LFE150:
.no_dead_strip _Mod__Mod_param_I_u32
_Mod__Mod_param_I_u32:
LFB151:
	pushq	%rbp
LCFI538:
	movq	%rsp, %rbp
LCFI539:
	pushq	%rbx
LCFI540:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L272
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L272
	addq	%rcx, %rdx
L272:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI541:
	ret
LFE151:
.no_dead_strip _Mod__uMod_var_I_LC
_Mod__uMod_var_I_LC:
LFB152:
	pushq	%rbp
LCFI542:
	movq	%rsp, %rbp
LCFI543:
	movq	120+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI544:
	ret
LFE152:
.no_dead_strip _Mod__Mod_var_I_LC
_Mod__Mod_var_I_LC:
LFB153:
	pushq	%rbp
LCFI545:
	movq	%rsp, %rbp
LCFI546:
	pushq	%rbx
LCFI547:
	movq	120+_MM_Mod(%rip), %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L275
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L275
	addq	%rcx, %rdx
L275:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI548:
	ret
LFE153:
.no_dead_strip _Mod__uMod_param_I_LC
_Mod__uMod_param_I_LC:
LFB154:
	pushq	%rbp
LCFI549:
	movq	%rsp, %rbp
LCFI550:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI551:
	ret
LFE154:
.no_dead_strip _Mod__Mod_param_I_LC
_Mod__Mod_param_I_LC:
LFB155:
	pushq	%rbp
LCFI552:
	movq	%rsp, %rbp
LCFI553:
	pushq	%rbx
LCFI554:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L278
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L278
	addq	%rcx, %rdx
L278:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI555:
	ret
LFE155:
.no_dead_strip _Mod__uMod_var_I_I
_Mod__uMod_var_I_I:
LFB156:
	pushq	%rbp
LCFI556:
	movq	%rsp, %rbp
LCFI557:
	movq	120+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI558:
	ret
LFE156:
.no_dead_strip _Mod__Mod_var_I_I
_Mod__Mod_var_I_I:
LFB157:
	pushq	%rbp
LCFI559:
	movq	%rsp, %rbp
LCFI560:
	pushq	%rbx
LCFI561:
	movq	120+_MM_Mod(%rip), %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L281
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L281
	addq	%rcx, %rdx
L281:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI562:
	ret
LFE157:
.no_dead_strip _Mod__uMod_param_I_I
_Mod__uMod_param_I_I:
LFB158:
	pushq	%rbp
LCFI563:
	movq	%rsp, %rbp
LCFI564:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI565:
	ret
LFE158:
.no_dead_strip _Mod__Mod_param_I_I
_Mod__Mod_param_I_I:
LFB159:
	pushq	%rbp
LCFI566:
	movq	%rsp, %rbp
LCFI567:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L284
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L284
	addq	-16(%rbp), %rdx
L284:
	movq	%rdx, %rax
	leave
LCFI568:
	ret
LFE159:
.no_dead_strip _Mod__uMod_var_I_i16
_Mod__uMod_var_I_i16:
LFB160:
	pushq	%rbp
LCFI569:
	movq	%rsp, %rbp
LCFI570:
	movq	120+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI571:
	ret
LFE160:
.no_dead_strip _Mod__Mod_var_I_i16
_Mod__Mod_var_I_i16:
LFB161:
	pushq	%rbp
LCFI572:
	movq	%rsp, %rbp
LCFI573:
	pushq	%rbx
LCFI574:
	movq	120+_MM_Mod(%rip), %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L287
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L287
	addq	%rcx, %rdx
L287:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI575:
	ret
LFE161:
.no_dead_strip _Mod__uMod_param_I_i16
_Mod__uMod_param_I_i16:
LFB162:
	pushq	%rbp
LCFI576:
	movq	%rsp, %rbp
LCFI577:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI578:
	ret
LFE162:
.no_dead_strip _Mod__Mod_param_I_i16
_Mod__Mod_param_I_i16:
LFB163:
	pushq	%rbp
LCFI579:
	movq	%rsp, %rbp
LCFI580:
	pushq	%rbx
LCFI581:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L290
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L290
	addq	%rcx, %rdx
L290:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI582:
	ret
LFE163:
.no_dead_strip _Mod__uMod_var_I_u64
_Mod__uMod_var_I_u64:
LFB164:
	pushq	%rbp
LCFI583:
	movq	%rsp, %rbp
LCFI584:
	movq	120+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI585:
	ret
LFE164:
.no_dead_strip _Mod__Mod_var_I_u64
_Mod__Mod_var_I_u64:
LFB165:
	pushq	%rbp
LCFI586:
	movq	%rsp, %rbp
LCFI587:
	pushq	%rbx
LCFI588:
	movq	120+_MM_Mod(%rip), %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L293
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L293
	addq	%rcx, %rdx
L293:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI589:
	ret
LFE165:
.no_dead_strip _Mod__uMod_param_I_u64
_Mod__uMod_param_I_u64:
LFB166:
	pushq	%rbp
LCFI590:
	movq	%rsp, %rbp
LCFI591:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI592:
	ret
LFE166:
.no_dead_strip _Mod__Mod_param_I_u64
_Mod__Mod_param_I_u64:
LFB167:
	pushq	%rbp
LCFI593:
	movq	%rsp, %rbp
LCFI594:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L296
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L296
	addq	-16(%rbp), %rdx
L296:
	movq	%rdx, %rax
	leave
LCFI595:
	ret
LFE167:
.no_dead_strip _Mod__uMod_var_I_i32
_Mod__uMod_var_I_i32:
LFB168:
	pushq	%rbp
LCFI596:
	movq	%rsp, %rbp
LCFI597:
	movq	120+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI598:
	ret
LFE168:
.no_dead_strip _Mod__Mod_var_I_i32
_Mod__Mod_var_I_i32:
LFB169:
	pushq	%rbp
LCFI599:
	movq	%rsp, %rbp
LCFI600:
	pushq	%rbx
LCFI601:
	movq	120+_MM_Mod(%rip), %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L299
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L299
	addq	%rcx, %rdx
L299:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI602:
	ret
LFE169:
.no_dead_strip _Mod__uMod_param_I_i32
_Mod__uMod_param_I_i32:
LFB170:
	pushq	%rbp
LCFI603:
	movq	%rsp, %rbp
LCFI604:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI605:
	ret
LFE170:
.no_dead_strip _Mod__Mod_param_I_i32
_Mod__Mod_param_I_i32:
LFB171:
	pushq	%rbp
LCFI606:
	movq	%rsp, %rbp
LCFI607:
	pushq	%rbx
LCFI608:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L302
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L302
	addq	%rcx, %rdx
L302:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI609:
	ret
LFE171:
.no_dead_strip _Mod__uMod_var_I_C
_Mod__uMod_var_I_C:
LFB172:
	pushq	%rbp
LCFI610:
	movq	%rsp, %rbp
LCFI611:
	movq	120+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI612:
	ret
LFE172:
.no_dead_strip _Mod__Mod_var_I_C
_Mod__Mod_var_I_C:
LFB173:
	pushq	%rbp
LCFI613:
	movq	%rsp, %rbp
LCFI614:
	pushq	%rbx
LCFI615:
	movq	120+_MM_Mod(%rip), %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L305
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L305
	addq	%rcx, %rdx
L305:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI616:
	ret
LFE173:
.no_dead_strip _Mod__uMod_param_I_C
_Mod__uMod_param_I_C:
LFB174:
	pushq	%rbp
LCFI617:
	movq	%rsp, %rbp
LCFI618:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI619:
	ret
LFE174:
.no_dead_strip _Mod__Mod_param_I_C
_Mod__Mod_param_I_C:
LFB175:
	pushq	%rbp
LCFI620:
	movq	%rsp, %rbp
LCFI621:
	pushq	%rbx
LCFI622:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L308
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L308
	addq	%rcx, %rdx
L308:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI623:
	ret
LFE175:
.no_dead_strip _Mod__uMod_var_I_u8
_Mod__uMod_var_I_u8:
LFB176:
	pushq	%rbp
LCFI624:
	movq	%rsp, %rbp
LCFI625:
	movq	120+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI626:
	ret
LFE176:
.no_dead_strip _Mod__Mod_var_I_u8
_Mod__Mod_var_I_u8:
LFB177:
	pushq	%rbp
LCFI627:
	movq	%rsp, %rbp
LCFI628:
	pushq	%rbx
LCFI629:
	movq	120+_MM_Mod(%rip), %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L311
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L311
	addq	%rcx, %rdx
L311:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI630:
	ret
LFE177:
.no_dead_strip _Mod__uMod_param_I_u8
_Mod__uMod_param_I_u8:
LFB178:
	pushq	%rbp
LCFI631:
	movq	%rsp, %rbp
LCFI632:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI633:
	ret
LFE178:
.no_dead_strip _Mod__Mod_param_I_u8
_Mod__Mod_param_I_u8:
LFB179:
	pushq	%rbp
LCFI634:
	movq	%rsp, %rbp
LCFI635:
	pushq	%rbx
LCFI636:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L314
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L314
	addq	%rcx, %rdx
L314:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI637:
	ret
LFE179:
.no_dead_strip _Mod__uMod_var_I_i64
_Mod__uMod_var_I_i64:
LFB180:
	pushq	%rbp
LCFI638:
	movq	%rsp, %rbp
LCFI639:
	movq	120+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI640:
	ret
LFE180:
.no_dead_strip _Mod__Mod_var_I_i64
_Mod__Mod_var_I_i64:
LFB181:
	pushq	%rbp
LCFI641:
	movq	%rsp, %rbp
LCFI642:
	pushq	%rbx
LCFI643:
	movq	120+_MM_Mod(%rip), %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L317
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L317
	addq	%rcx, %rdx
L317:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI644:
	ret
LFE181:
.no_dead_strip _Mod__uMod_param_I_i64
_Mod__uMod_param_I_i64:
LFB182:
	pushq	%rbp
LCFI645:
	movq	%rsp, %rbp
LCFI646:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI647:
	ret
LFE182:
.no_dead_strip _Mod__Mod_param_I_i64
_Mod__Mod_param_I_i64:
LFB183:
	pushq	%rbp
LCFI648:
	movq	%rsp, %rbp
LCFI649:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L320
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L320
	addq	-16(%rbp), %rdx
L320:
	movq	%rdx, %rax
	leave
LCFI650:
	ret
LFE183:
.no_dead_strip _Mod__uMod_var_I_L
_Mod__uMod_var_I_L:
LFB184:
	pushq	%rbp
LCFI651:
	movq	%rsp, %rbp
LCFI652:
	movq	120+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI653:
	ret
LFE184:
.no_dead_strip _Mod__Mod_var_I_L
_Mod__Mod_var_I_L:
LFB185:
	pushq	%rbp
LCFI654:
	movq	%rsp, %rbp
LCFI655:
	pushq	%rbx
LCFI656:
	movq	120+_MM_Mod(%rip), %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L323
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L323
	addq	%rcx, %rdx
L323:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI657:
	ret
LFE185:
.no_dead_strip _Mod__uMod_param_I_L
_Mod__uMod_param_I_L:
LFB186:
	pushq	%rbp
LCFI658:
	movq	%rsp, %rbp
LCFI659:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI660:
	ret
LFE186:
.no_dead_strip _Mod__Mod_param_I_L
_Mod__Mod_param_I_L:
LFB187:
	pushq	%rbp
LCFI661:
	movq	%rsp, %rbp
LCFI662:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L326
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L326
	addq	-16(%rbp), %rdx
L326:
	movq	%rdx, %rax
	leave
LCFI663:
	ret
LFE187:
.no_dead_strip _Mod__uMod_var_I_i8
_Mod__uMod_var_I_i8:
LFB188:
	pushq	%rbp
LCFI664:
	movq	%rsp, %rbp
LCFI665:
	movq	120+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI666:
	ret
LFE188:
.no_dead_strip _Mod__Mod_var_I_i8
_Mod__Mod_var_I_i8:
LFB189:
	pushq	%rbp
LCFI667:
	movq	%rsp, %rbp
LCFI668:
	pushq	%rbx
LCFI669:
	movq	120+_MM_Mod(%rip), %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L329
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L329
	addq	%rcx, %rdx
L329:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI670:
	ret
LFE189:
.no_dead_strip _Mod__uMod_param_I_i8
_Mod__uMod_param_I_i8:
LFB190:
	pushq	%rbp
LCFI671:
	movq	%rsp, %rbp
LCFI672:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI673:
	ret
LFE190:
.no_dead_strip _Mod__Mod_param_I_i8
_Mod__Mod_param_I_i8:
LFB191:
	pushq	%rbp
LCFI674:
	movq	%rsp, %rbp
LCFI675:
	pushq	%rbx
LCFI676:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L332
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L332
	addq	%rcx, %rdx
L332:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI677:
	ret
LFE191:
.no_dead_strip _Mod__uMod_var_i16_u16
_Mod__uMod_var_i16_u16:
LFB192:
	pushq	%rbp
LCFI678:
	movq	%rsp, %rbp
LCFI679:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI680:
	ret
LFE192:
.no_dead_strip _Mod__Mod_var_i16_u16
_Mod__Mod_var_i16_u16:
LFB193:
	pushq	%rbp
LCFI681:
	movq	%rsp, %rbp
LCFI682:
	pushq	%rbx
LCFI683:
	subq	$8, %rsp
LCFI684:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L335
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L335
	addq	%rcx, %rdx
L335:
	cmpq	$32767, %rdx
	jle	L336
	movl	$7905, %edi
	call	__m3_fault
L336:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L337
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L337
	addq	%rcx, %rdx
L337:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI685:
	ret
LFE193:
.no_dead_strip _Mod__uMod_param_i16_u16
_Mod__uMod_param_i16_u16:
LFB194:
	pushq	%rbp
LCFI686:
	movq	%rsp, %rbp
LCFI687:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rax
	movzwl	-4(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI688:
	ret
LFE194:
.no_dead_strip _Mod__Mod_param_i16_u16
_Mod__Mod_param_i16_u16:
LFB195:
	pushq	%rbp
LCFI689:
	movq	%rsp, %rbp
LCFI690:
	pushq	%rbx
LCFI691:
	subq	$24, %rsp
LCFI692:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L340
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L340
	addq	%rcx, %rdx
L340:
	cmpq	$32767, %rdx
	jle	L341
	movl	$7969, %edi
	call	__m3_fault
L341:
	movswq	-18(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L342
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L342
	addq	%rcx, %rdx
L342:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI693:
	ret
LFE195:
.no_dead_strip _Mod__uMod_var_i16_u32
_Mod__uMod_var_i16_u32:
LFB196:
	pushq	%rbp
LCFI694:
	movq	%rsp, %rbp
LCFI695:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI696:
	ret
LFE196:
.no_dead_strip _Mod__Mod_var_i16_u32
_Mod__Mod_var_i16_u32:
LFB197:
	pushq	%rbp
LCFI697:
	movq	%rsp, %rbp
LCFI698:
	pushq	%rbx
LCFI699:
	subq	$8, %rsp
LCFI700:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L345
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L345
	addq	%rcx, %rdx
L345:
	cmpq	$32767, %rdx
	jle	L346
	movl	$8033, %edi
	call	__m3_fault
L346:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L347
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L347
	addq	%rcx, %rdx
L347:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI701:
	ret
LFE197:
.no_dead_strip _Mod__uMod_param_i16_u32
_Mod__uMod_param_i16_u32:
LFB198:
	pushq	%rbp
LCFI702:
	movq	%rsp, %rbp
LCFI703:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rax
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI704:
	ret
LFE198:
.no_dead_strip _Mod__Mod_param_i16_u32
_Mod__Mod_param_i16_u32:
LFB199:
	pushq	%rbp
LCFI705:
	movq	%rsp, %rbp
LCFI706:
	pushq	%rbx
LCFI707:
	subq	$24, %rsp
LCFI708:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L350
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L350
	addq	%rcx, %rdx
L350:
	cmpq	$32767, %rdx
	jle	L351
	movl	$8097, %edi
	call	__m3_fault
L351:
	movswq	-18(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L352
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L352
	addq	%rcx, %rdx
L352:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI709:
	ret
LFE199:
.no_dead_strip _Mod__uMod_var_i16_LC
_Mod__uMod_var_i16_LC:
LFB200:
	pushq	%rbp
LCFI710:
	movq	%rsp, %rbp
LCFI711:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI712:
	ret
LFE200:
.no_dead_strip _Mod__Mod_var_i16_LC
_Mod__Mod_var_i16_LC:
LFB201:
	pushq	%rbp
LCFI713:
	movq	%rsp, %rbp
LCFI714:
	pushq	%rbx
LCFI715:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L355
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L355
	addq	%rcx, %rdx
L355:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI716:
	ret
LFE201:
.no_dead_strip _Mod__uMod_param_i16_LC
_Mod__uMod_param_i16_LC:
LFB202:
	pushq	%rbp
LCFI717:
	movq	%rsp, %rbp
LCFI718:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI719:
	ret
LFE202:
.no_dead_strip _Mod__Mod_param_i16_LC
_Mod__Mod_param_i16_LC:
LFB203:
	pushq	%rbp
LCFI720:
	movq	%rsp, %rbp
LCFI721:
	pushq	%rbx
LCFI722:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L358
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L358
	addq	%rcx, %rdx
L358:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI723:
	ret
LFE203:
.no_dead_strip _Mod__uMod_var_i16_I
_Mod__uMod_var_i16_I:
LFB204:
	pushq	%rbp
LCFI724:
	movq	%rsp, %rbp
LCFI725:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI726:
	ret
LFE204:
.no_dead_strip _Mod__Mod_var_i16_I
_Mod__Mod_var_i16_I:
LFB205:
	pushq	%rbp
LCFI727:
	movq	%rsp, %rbp
LCFI728:
	pushq	%rbx
LCFI729:
	subq	$8, %rsp
LCFI730:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L361
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L361
	addq	%rcx, %rdx
L361:
	cmpq	$-32768, %rdx
	jl	L362
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L363
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L363
	addq	%rcx, %rdx
L363:
	cmpq	$32767, %rdx
	jle	L364
L362:
	movl	$8289, %edi
	call	__m3_fault
L364:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L365
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L365
	addq	%rcx, %rdx
L365:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI731:
	ret
LFE205:
.no_dead_strip _Mod__uMod_param_i16_I
_Mod__uMod_param_i16_I:
LFB206:
	pushq	%rbp
LCFI732:
	movq	%rsp, %rbp
LCFI733:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI734:
	ret
LFE206:
.no_dead_strip _Mod__Mod_param_i16_I
_Mod__Mod_param_i16_I:
LFB207:
	pushq	%rbp
LCFI735:
	movq	%rsp, %rbp
LCFI736:
	subq	$16, %rsp
LCFI737:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L368
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L368
	addq	-16(%rbp), %rdx
L368:
	cmpq	$-32768, %rdx
	jl	L369
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L370
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L370
	addq	-16(%rbp), %rdx
L370:
	cmpq	$32767, %rdx
	jle	L371
L369:
	movl	$8353, %edi
	call	__m3_fault
L371:
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L372
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L372
	addq	-16(%rbp), %rdx
L372:
	movl	%edx, %eax
	leave
LCFI738:
	ret
LFE207:
.no_dead_strip _Mod__uMod_var_i16_i16
_Mod__uMod_var_i16_i16:
LFB208:
	pushq	%rbp
LCFI739:
	movq	%rsp, %rbp
LCFI740:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI741:
	ret
LFE208:
.no_dead_strip _Mod__Mod_var_i16_i16
_Mod__Mod_var_i16_i16:
LFB209:
	pushq	%rbp
LCFI742:
	movq	%rsp, %rbp
LCFI743:
	pushq	%rbx
LCFI744:
	subq	$8, %rsp
LCFI745:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L375
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L375
	addq	%rcx, %rdx
L375:
	cmpq	$-32768, %rdx
	jl	L376
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L377
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L377
	addq	%rcx, %rdx
L377:
	cmpq	$32767, %rdx
	jle	L378
L376:
	movl	$8417, %edi
	call	__m3_fault
L378:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L379
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L379
	addq	%rcx, %rdx
L379:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI746:
	ret
LFE209:
.no_dead_strip _Mod__uMod_param_i16_i16
_Mod__uMod_param_i16_i16:
LFB210:
	pushq	%rbp
LCFI747:
	movq	%rsp, %rbp
LCFI748:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rax
	movswq	-4(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI749:
	ret
LFE210:
.no_dead_strip _Mod__Mod_param_i16_i16
_Mod__Mod_param_i16_i16:
LFB211:
	pushq	%rbp
LCFI750:
	movq	%rsp, %rbp
LCFI751:
	pushq	%rbx
LCFI752:
	subq	$24, %rsp
LCFI753:
	movw	%di, -18(%rbp)
	movw	%si, -20(%rbp)
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L382
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L382
	addq	%rcx, %rdx
L382:
	cmpq	$-32768, %rdx
	jl	L383
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L384
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L384
	addq	%rcx, %rdx
L384:
	cmpq	$32767, %rdx
	jle	L385
L383:
	movl	$8481, %edi
	call	__m3_fault
L385:
	movswq	-18(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L386
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L386
	addq	%rcx, %rdx
L386:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI754:
	ret
LFE211:
.no_dead_strip _Mod__uMod_var_i16_u64
_Mod__uMod_var_i16_u64:
LFB212:
	pushq	%rbp
LCFI755:
	movq	%rsp, %rbp
LCFI756:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI757:
	ret
LFE212:
.no_dead_strip _Mod__Mod_var_i16_u64
_Mod__Mod_var_i16_u64:
LFB213:
	pushq	%rbp
LCFI758:
	movq	%rsp, %rbp
LCFI759:
	pushq	%rbx
LCFI760:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L389
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L389
	addq	%rcx, %rdx
L389:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI761:
	ret
LFE213:
.no_dead_strip _Mod__uMod_param_i16_u64
_Mod__uMod_param_i16_u64:
LFB214:
	pushq	%rbp
LCFI762:
	movq	%rsp, %rbp
LCFI763:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI764:
	ret
LFE214:
.no_dead_strip _Mod__Mod_param_i16_u64
_Mod__Mod_param_i16_u64:
LFB215:
	pushq	%rbp
LCFI765:
	movq	%rsp, %rbp
LCFI766:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L392
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L392
	addq	-16(%rbp), %rdx
L392:
	movq	%rdx, %rax
	leave
LCFI767:
	ret
LFE215:
.no_dead_strip _Mod__uMod_var_i16_i32
_Mod__uMod_var_i16_i32:
LFB216:
	pushq	%rbp
LCFI768:
	movq	%rsp, %rbp
LCFI769:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI770:
	ret
LFE216:
.no_dead_strip _Mod__Mod_var_i16_i32
_Mod__Mod_var_i16_i32:
LFB217:
	pushq	%rbp
LCFI771:
	movq	%rsp, %rbp
LCFI772:
	pushq	%rbx
LCFI773:
	subq	$8, %rsp
LCFI774:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L395
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L395
	addq	%rcx, %rdx
L395:
	cmpq	$-32768, %rdx
	jl	L396
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L397
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L397
	addq	%rcx, %rdx
L397:
	cmpq	$32767, %rdx
	jle	L398
L396:
	movl	$8673, %edi
	call	__m3_fault
L398:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L399
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L399
	addq	%rcx, %rdx
L399:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI775:
	ret
LFE217:
.no_dead_strip _Mod__uMod_param_i16_i32
_Mod__uMod_param_i16_i32:
LFB218:
	pushq	%rbp
LCFI776:
	movq	%rsp, %rbp
LCFI777:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rax
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI778:
	ret
LFE218:
.no_dead_strip _Mod__Mod_param_i16_i32
_Mod__Mod_param_i16_i32:
LFB219:
	pushq	%rbp
LCFI779:
	movq	%rsp, %rbp
LCFI780:
	pushq	%rbx
LCFI781:
	subq	$24, %rsp
LCFI782:
	movw	%di, -18(%rbp)
	movl	%esi, -24(%rbp)
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L402
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L402
	addq	%rcx, %rdx
L402:
	cmpq	$-32768, %rdx
	jl	L403
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L404
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L404
	addq	%rcx, %rdx
L404:
	cmpq	$32767, %rdx
	jle	L405
L403:
	movl	$8737, %edi
	call	__m3_fault
L405:
	movswq	-18(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L406
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L406
	addq	%rcx, %rdx
L406:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI783:
	ret
LFE219:
.no_dead_strip _Mod__uMod_var_i16_C
_Mod__uMod_var_i16_C:
LFB220:
	pushq	%rbp
LCFI784:
	movq	%rsp, %rbp
LCFI785:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI786:
	ret
LFE220:
.no_dead_strip _Mod__Mod_var_i16_C
_Mod__Mod_var_i16_C:
LFB221:
	pushq	%rbp
LCFI787:
	movq	%rsp, %rbp
LCFI788:
	pushq	%rbx
LCFI789:
	subq	$8, %rsp
LCFI790:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L409
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L409
	addq	%rcx, %rdx
L409:
	cmpq	$32767, %rdx
	jle	L410
	movl	$8801, %edi
	call	__m3_fault
L410:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L411
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L411
	addq	%rcx, %rdx
L411:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI791:
	ret
LFE221:
.no_dead_strip _Mod__uMod_param_i16_C
_Mod__uMod_param_i16_C:
LFB222:
	pushq	%rbp
LCFI792:
	movq	%rsp, %rbp
LCFI793:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI794:
	ret
LFE222:
.no_dead_strip _Mod__Mod_param_i16_C
_Mod__Mod_param_i16_C:
LFB223:
	pushq	%rbp
LCFI795:
	movq	%rsp, %rbp
LCFI796:
	pushq	%rbx
LCFI797:
	subq	$24, %rsp
LCFI798:
	movw	%di, -18(%rbp)
	movq	%rsi, -32(%rbp)
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L414
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L414
	addq	%rcx, %rdx
L414:
	cmpq	$32767, %rdx
	jle	L415
	movl	$8865, %edi
	call	__m3_fault
L415:
	movswq	-18(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L416
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L416
	addq	%rcx, %rdx
L416:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI799:
	ret
LFE223:
.no_dead_strip _Mod__uMod_var_i16_u8
_Mod__uMod_var_i16_u8:
LFB224:
	pushq	%rbp
LCFI800:
	movq	%rsp, %rbp
LCFI801:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI802:
	ret
LFE224:
.no_dead_strip _Mod__Mod_var_i16_u8
_Mod__Mod_var_i16_u8:
LFB225:
	pushq	%rbp
LCFI803:
	movq	%rsp, %rbp
LCFI804:
	pushq	%rbx
LCFI805:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L419
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L419
	addq	%rcx, %rdx
L419:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI806:
	ret
LFE225:
.no_dead_strip _Mod__uMod_param_i16_u8
_Mod__uMod_param_i16_u8:
LFB226:
	pushq	%rbp
LCFI807:
	movq	%rsp, %rbp
LCFI808:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rax
	movzbl	-3(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI809:
	ret
LFE226:
.no_dead_strip _Mod__Mod_param_i16_u8
_Mod__Mod_param_i16_u8:
LFB227:
	pushq	%rbp
LCFI810:
	movq	%rsp, %rbp
LCFI811:
	pushq	%rbx
LCFI812:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp), %rbx
	movzbl	-19(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L422
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L422
	addq	%rcx, %rdx
L422:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI813:
	ret
LFE227:
.no_dead_strip _Mod__uMod_var_i16_i64
_Mod__uMod_var_i16_i64:
LFB228:
	pushq	%rbp
LCFI814:
	movq	%rsp, %rbp
LCFI815:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI816:
	ret
LFE228:
.no_dead_strip _Mod__Mod_var_i16_i64
_Mod__Mod_var_i16_i64:
LFB229:
	pushq	%rbp
LCFI817:
	movq	%rsp, %rbp
LCFI818:
	pushq	%rbx
LCFI819:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L425
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L425
	addq	%rcx, %rdx
L425:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI820:
	ret
LFE229:
.no_dead_strip _Mod__uMod_param_i16_i64
_Mod__uMod_param_i16_i64:
LFB230:
	pushq	%rbp
LCFI821:
	movq	%rsp, %rbp
LCFI822:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI823:
	ret
LFE230:
.no_dead_strip _Mod__Mod_param_i16_i64
_Mod__Mod_param_i16_i64:
LFB231:
	pushq	%rbp
LCFI824:
	movq	%rsp, %rbp
LCFI825:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L428
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L428
	addq	-16(%rbp), %rdx
L428:
	movq	%rdx, %rax
	leave
LCFI826:
	ret
LFE231:
.no_dead_strip _Mod__uMod_var_i16_L
_Mod__uMod_var_i16_L:
LFB232:
	pushq	%rbp
LCFI827:
	movq	%rsp, %rbp
LCFI828:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI829:
	ret
LFE232:
.no_dead_strip _Mod__Mod_var_i16_L
_Mod__Mod_var_i16_L:
LFB233:
	pushq	%rbp
LCFI830:
	movq	%rsp, %rbp
LCFI831:
	pushq	%rbx
LCFI832:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L431
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L431
	addq	%rcx, %rdx
L431:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI833:
	ret
LFE233:
.no_dead_strip _Mod__uMod_param_i16_L
_Mod__uMod_param_i16_L:
LFB234:
	pushq	%rbp
LCFI834:
	movq	%rsp, %rbp
LCFI835:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI836:
	ret
LFE234:
.no_dead_strip _Mod__Mod_param_i16_L
_Mod__Mod_param_i16_L:
LFB235:
	pushq	%rbp
LCFI837:
	movq	%rsp, %rbp
LCFI838:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L434
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L434
	addq	-16(%rbp), %rdx
L434:
	movq	%rdx, %rax
	leave
LCFI839:
	ret
LFE235:
.no_dead_strip _Mod__uMod_var_i16_i8
_Mod__uMod_var_i16_i8:
LFB236:
	pushq	%rbp
LCFI840:
	movq	%rsp, %rbp
LCFI841:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI842:
	ret
LFE236:
.no_dead_strip _Mod__Mod_var_i16_i8
_Mod__Mod_var_i16_i8:
LFB237:
	pushq	%rbp
LCFI843:
	movq	%rsp, %rbp
LCFI844:
	pushq	%rbx
LCFI845:
	subq	$8, %rsp
LCFI846:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L437
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L437
	addq	%rcx, %rdx
L437:
	cmpq	$-32768, %rdx
	jl	L438
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L439
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L439
	addq	%rcx, %rdx
L439:
	cmpq	$32767, %rdx
	jle	L440
L438:
	movl	$9313, %edi
	call	__m3_fault
L440:
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L441
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L441
	addq	%rcx, %rdx
L441:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI847:
	ret
LFE237:
.no_dead_strip _Mod__uMod_param_i16_i8
_Mod__uMod_param_i16_i8:
LFB238:
	pushq	%rbp
LCFI848:
	movq	%rsp, %rbp
LCFI849:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rax
	movsbq	-3(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI850:
	ret
LFE238:
.no_dead_strip _Mod__Mod_param_i16_i8
_Mod__Mod_param_i16_i8:
LFB239:
	pushq	%rbp
LCFI851:
	movq	%rsp, %rbp
LCFI852:
	pushq	%rbx
LCFI853:
	subq	$24, %rsp
LCFI854:
	movw	%di, -18(%rbp)
	movb	%sil, -19(%rbp)
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L444
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L444
	addq	%rcx, %rdx
L444:
	cmpq	$-32768, %rdx
	jl	L445
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L446
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L446
	addq	%rcx, %rdx
L446:
	cmpq	$32767, %rdx
	jle	L447
L445:
	movl	$9377, %edi
	call	__m3_fault
L447:
	movswq	-18(%rbp), %rbx
	movsbq	-19(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L448
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L448
	addq	%rcx, %rdx
L448:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI855:
	ret
LFE239:
.no_dead_strip _Mod__uMod_var_u64_u16
_Mod__uMod_var_u64_u16:
LFB240:
	pushq	%rbp
LCFI856:
	movq	%rsp, %rbp
LCFI857:
	movq	136+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI858:
	ret
LFE240:
.no_dead_strip _Mod__Mod_var_u64_u16
_Mod__Mod_var_u64_u16:
LFB241:
	pushq	%rbp
LCFI859:
	movq	%rsp, %rbp
LCFI860:
	pushq	%rbx
LCFI861:
	movq	136+_MM_Mod(%rip), %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L451
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L451
	addq	%rcx, %rdx
L451:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI862:
	ret
LFE241:
.no_dead_strip _Mod__uMod_param_u64_u16
_Mod__uMod_param_u64_u16:
LFB242:
	pushq	%rbp
LCFI863:
	movq	%rsp, %rbp
LCFI864:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI865:
	ret
LFE242:
.no_dead_strip _Mod__Mod_param_u64_u16
_Mod__Mod_param_u64_u16:
LFB243:
	pushq	%rbp
LCFI866:
	movq	%rsp, %rbp
LCFI867:
	pushq	%rbx
LCFI868:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L454
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L454
	addq	%rcx, %rdx
L454:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI869:
	ret
LFE243:
.no_dead_strip _Mod__uMod_var_u64_u32
_Mod__uMod_var_u64_u32:
LFB244:
	pushq	%rbp
LCFI870:
	movq	%rsp, %rbp
LCFI871:
	movq	136+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI872:
	ret
LFE244:
.no_dead_strip _Mod__Mod_var_u64_u32
_Mod__Mod_var_u64_u32:
LFB245:
	pushq	%rbp
LCFI873:
	movq	%rsp, %rbp
LCFI874:
	pushq	%rbx
LCFI875:
	movq	136+_MM_Mod(%rip), %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L457
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L457
	addq	%rcx, %rdx
L457:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI876:
	ret
LFE245:
.no_dead_strip _Mod__uMod_param_u64_u32
_Mod__uMod_param_u64_u32:
LFB246:
	pushq	%rbp
LCFI877:
	movq	%rsp, %rbp
LCFI878:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI879:
	ret
LFE246:
.no_dead_strip _Mod__Mod_param_u64_u32
_Mod__Mod_param_u64_u32:
LFB247:
	pushq	%rbp
LCFI880:
	movq	%rsp, %rbp
LCFI881:
	pushq	%rbx
LCFI882:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L460
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L460
	addq	%rcx, %rdx
L460:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI883:
	ret
LFE247:
.no_dead_strip _Mod__uMod_var_u64_LC
_Mod__uMod_var_u64_LC:
LFB248:
	pushq	%rbp
LCFI884:
	movq	%rsp, %rbp
LCFI885:
	movq	136+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI886:
	ret
LFE248:
.no_dead_strip _Mod__Mod_var_u64_LC
_Mod__Mod_var_u64_LC:
LFB249:
	pushq	%rbp
LCFI887:
	movq	%rsp, %rbp
LCFI888:
	pushq	%rbx
LCFI889:
	movq	136+_MM_Mod(%rip), %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L463
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L463
	addq	%rcx, %rdx
L463:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI890:
	ret
LFE249:
.no_dead_strip _Mod__uMod_param_u64_LC
_Mod__uMod_param_u64_LC:
LFB250:
	pushq	%rbp
LCFI891:
	movq	%rsp, %rbp
LCFI892:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI893:
	ret
LFE250:
.no_dead_strip _Mod__Mod_param_u64_LC
_Mod__Mod_param_u64_LC:
LFB251:
	pushq	%rbp
LCFI894:
	movq	%rsp, %rbp
LCFI895:
	pushq	%rbx
LCFI896:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L466
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L466
	addq	%rcx, %rdx
L466:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI897:
	ret
LFE251:
.no_dead_strip _Mod__uMod_var_u64_I
_Mod__uMod_var_u64_I:
LFB252:
	pushq	%rbp
LCFI898:
	movq	%rsp, %rbp
LCFI899:
	movq	136+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI900:
	ret
LFE252:
.no_dead_strip _Mod__Mod_var_u64_I
_Mod__Mod_var_u64_I:
LFB253:
	pushq	%rbp
LCFI901:
	movq	%rsp, %rbp
LCFI902:
	pushq	%rbx
LCFI903:
	movq	136+_MM_Mod(%rip), %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L469
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L469
	addq	%rcx, %rdx
L469:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI904:
	ret
LFE253:
.no_dead_strip _Mod__uMod_param_u64_I
_Mod__uMod_param_u64_I:
LFB254:
	pushq	%rbp
LCFI905:
	movq	%rsp, %rbp
LCFI906:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI907:
	ret
LFE254:
.no_dead_strip _Mod__Mod_param_u64_I
_Mod__Mod_param_u64_I:
LFB255:
	pushq	%rbp
LCFI908:
	movq	%rsp, %rbp
LCFI909:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L472
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L472
	addq	-16(%rbp), %rdx
L472:
	movq	%rdx, %rax
	leave
LCFI910:
	ret
LFE255:
.no_dead_strip _Mod__uMod_var_u64_i16
_Mod__uMod_var_u64_i16:
LFB256:
	pushq	%rbp
LCFI911:
	movq	%rsp, %rbp
LCFI912:
	movq	136+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI913:
	ret
LFE256:
.no_dead_strip _Mod__Mod_var_u64_i16
_Mod__Mod_var_u64_i16:
LFB257:
	pushq	%rbp
LCFI914:
	movq	%rsp, %rbp
LCFI915:
	pushq	%rbx
LCFI916:
	movq	136+_MM_Mod(%rip), %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L475
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L475
	addq	%rcx, %rdx
L475:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI917:
	ret
LFE257:
.no_dead_strip _Mod__uMod_param_u64_i16
_Mod__uMod_param_u64_i16:
LFB258:
	pushq	%rbp
LCFI918:
	movq	%rsp, %rbp
LCFI919:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI920:
	ret
LFE258:
.no_dead_strip _Mod__Mod_param_u64_i16
_Mod__Mod_param_u64_i16:
LFB259:
	pushq	%rbp
LCFI921:
	movq	%rsp, %rbp
LCFI922:
	pushq	%rbx
LCFI923:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L478
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L478
	addq	%rcx, %rdx
L478:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI924:
	ret
LFE259:
.no_dead_strip _Mod__uMod_var_u64_u64
_Mod__uMod_var_u64_u64:
LFB260:
	pushq	%rbp
LCFI925:
	movq	%rsp, %rbp
LCFI926:
	movq	136+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI927:
	ret
LFE260:
.no_dead_strip _Mod__Mod_var_u64_u64
_Mod__Mod_var_u64_u64:
LFB261:
	pushq	%rbp
LCFI928:
	movq	%rsp, %rbp
LCFI929:
	pushq	%rbx
LCFI930:
	movq	136+_MM_Mod(%rip), %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L481
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L481
	addq	%rcx, %rdx
L481:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI931:
	ret
LFE261:
.no_dead_strip _Mod__uMod_param_u64_u64
_Mod__uMod_param_u64_u64:
LFB262:
	pushq	%rbp
LCFI932:
	movq	%rsp, %rbp
LCFI933:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI934:
	ret
LFE262:
.no_dead_strip _Mod__Mod_param_u64_u64
_Mod__Mod_param_u64_u64:
LFB263:
	pushq	%rbp
LCFI935:
	movq	%rsp, %rbp
LCFI936:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L484
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L484
	addq	-16(%rbp), %rdx
L484:
	movq	%rdx, %rax
	leave
LCFI937:
	ret
LFE263:
.no_dead_strip _Mod__uMod_var_u64_i32
_Mod__uMod_var_u64_i32:
LFB264:
	pushq	%rbp
LCFI938:
	movq	%rsp, %rbp
LCFI939:
	movq	136+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI940:
	ret
LFE264:
.no_dead_strip _Mod__Mod_var_u64_i32
_Mod__Mod_var_u64_i32:
LFB265:
	pushq	%rbp
LCFI941:
	movq	%rsp, %rbp
LCFI942:
	pushq	%rbx
LCFI943:
	movq	136+_MM_Mod(%rip), %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L487
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L487
	addq	%rcx, %rdx
L487:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI944:
	ret
LFE265:
.no_dead_strip _Mod__uMod_param_u64_i32
_Mod__uMod_param_u64_i32:
LFB266:
	pushq	%rbp
LCFI945:
	movq	%rsp, %rbp
LCFI946:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI947:
	ret
LFE266:
.no_dead_strip _Mod__Mod_param_u64_i32
_Mod__Mod_param_u64_i32:
LFB267:
	pushq	%rbp
LCFI948:
	movq	%rsp, %rbp
LCFI949:
	pushq	%rbx
LCFI950:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L490
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L490
	addq	%rcx, %rdx
L490:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI951:
	ret
LFE267:
.no_dead_strip _Mod__uMod_var_u64_C
_Mod__uMod_var_u64_C:
LFB268:
	pushq	%rbp
LCFI952:
	movq	%rsp, %rbp
LCFI953:
	movq	136+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI954:
	ret
LFE268:
.no_dead_strip _Mod__Mod_var_u64_C
_Mod__Mod_var_u64_C:
LFB269:
	pushq	%rbp
LCFI955:
	movq	%rsp, %rbp
LCFI956:
	pushq	%rbx
LCFI957:
	movq	136+_MM_Mod(%rip), %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L493
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L493
	addq	%rcx, %rdx
L493:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI958:
	ret
LFE269:
.no_dead_strip _Mod__uMod_param_u64_C
_Mod__uMod_param_u64_C:
LFB270:
	pushq	%rbp
LCFI959:
	movq	%rsp, %rbp
LCFI960:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI961:
	ret
LFE270:
.no_dead_strip _Mod__Mod_param_u64_C
_Mod__Mod_param_u64_C:
LFB271:
	pushq	%rbp
LCFI962:
	movq	%rsp, %rbp
LCFI963:
	pushq	%rbx
LCFI964:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L496
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L496
	addq	%rcx, %rdx
L496:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI965:
	ret
LFE271:
.no_dead_strip _Mod__uMod_var_u64_u8
_Mod__uMod_var_u64_u8:
LFB272:
	pushq	%rbp
LCFI966:
	movq	%rsp, %rbp
LCFI967:
	movq	136+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI968:
	ret
LFE272:
.no_dead_strip _Mod__Mod_var_u64_u8
_Mod__Mod_var_u64_u8:
LFB273:
	pushq	%rbp
LCFI969:
	movq	%rsp, %rbp
LCFI970:
	pushq	%rbx
LCFI971:
	movq	136+_MM_Mod(%rip), %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L499
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L499
	addq	%rcx, %rdx
L499:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI972:
	ret
LFE273:
.no_dead_strip _Mod__uMod_param_u64_u8
_Mod__uMod_param_u64_u8:
LFB274:
	pushq	%rbp
LCFI973:
	movq	%rsp, %rbp
LCFI974:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI975:
	ret
LFE274:
.no_dead_strip _Mod__Mod_param_u64_u8
_Mod__Mod_param_u64_u8:
LFB275:
	pushq	%rbp
LCFI976:
	movq	%rsp, %rbp
LCFI977:
	pushq	%rbx
LCFI978:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L502
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L502
	addq	%rcx, %rdx
L502:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI979:
	ret
LFE275:
.no_dead_strip _Mod__uMod_var_u64_i64
_Mod__uMod_var_u64_i64:
LFB276:
	pushq	%rbp
LCFI980:
	movq	%rsp, %rbp
LCFI981:
	movq	136+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI982:
	ret
LFE276:
.no_dead_strip _Mod__Mod_var_u64_i64
_Mod__Mod_var_u64_i64:
LFB277:
	pushq	%rbp
LCFI983:
	movq	%rsp, %rbp
LCFI984:
	pushq	%rbx
LCFI985:
	movq	136+_MM_Mod(%rip), %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L505
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L505
	addq	%rcx, %rdx
L505:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI986:
	ret
LFE277:
.no_dead_strip _Mod__uMod_param_u64_i64
_Mod__uMod_param_u64_i64:
LFB278:
	pushq	%rbp
LCFI987:
	movq	%rsp, %rbp
LCFI988:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI989:
	ret
LFE278:
.no_dead_strip _Mod__Mod_param_u64_i64
_Mod__Mod_param_u64_i64:
LFB279:
	pushq	%rbp
LCFI990:
	movq	%rsp, %rbp
LCFI991:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L508
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L508
	addq	-16(%rbp), %rdx
L508:
	movq	%rdx, %rax
	leave
LCFI992:
	ret
LFE279:
.no_dead_strip _Mod__uMod_var_u64_L
_Mod__uMod_var_u64_L:
LFB280:
	pushq	%rbp
LCFI993:
	movq	%rsp, %rbp
LCFI994:
	movq	136+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI995:
	ret
LFE280:
.no_dead_strip _Mod__Mod_var_u64_L
_Mod__Mod_var_u64_L:
LFB281:
	pushq	%rbp
LCFI996:
	movq	%rsp, %rbp
LCFI997:
	pushq	%rbx
LCFI998:
	movq	136+_MM_Mod(%rip), %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L511
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L511
	addq	%rcx, %rdx
L511:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI999:
	ret
LFE281:
.no_dead_strip _Mod__uMod_param_u64_L
_Mod__uMod_param_u64_L:
LFB282:
	pushq	%rbp
LCFI1000:
	movq	%rsp, %rbp
LCFI1001:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1002:
	ret
LFE282:
.no_dead_strip _Mod__Mod_param_u64_L
_Mod__Mod_param_u64_L:
LFB283:
	pushq	%rbp
LCFI1003:
	movq	%rsp, %rbp
LCFI1004:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L514
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L514
	addq	-16(%rbp), %rdx
L514:
	movq	%rdx, %rax
	leave
LCFI1005:
	ret
LFE283:
.no_dead_strip _Mod__uMod_var_u64_i8
_Mod__uMod_var_u64_i8:
LFB284:
	pushq	%rbp
LCFI1006:
	movq	%rsp, %rbp
LCFI1007:
	movq	136+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1008:
	ret
LFE284:
.no_dead_strip _Mod__Mod_var_u64_i8
_Mod__Mod_var_u64_i8:
LFB285:
	pushq	%rbp
LCFI1009:
	movq	%rsp, %rbp
LCFI1010:
	pushq	%rbx
LCFI1011:
	movq	136+_MM_Mod(%rip), %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L517
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L517
	addq	%rcx, %rdx
L517:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1012:
	ret
LFE285:
.no_dead_strip _Mod__uMod_param_u64_i8
_Mod__uMod_param_u64_i8:
LFB286:
	pushq	%rbp
LCFI1013:
	movq	%rsp, %rbp
LCFI1014:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1015:
	ret
LFE286:
.no_dead_strip _Mod__Mod_param_u64_i8
_Mod__Mod_param_u64_i8:
LFB287:
	pushq	%rbp
LCFI1016:
	movq	%rsp, %rbp
LCFI1017:
	pushq	%rbx
LCFI1018:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L520
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L520
	addq	%rcx, %rdx
L520:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1019:
	ret
LFE287:
.no_dead_strip _Mod__Mod_var_f64_f64
_Mod__Mod_var_f64_f64:
LFB288:
	pushq	%rbp
LCFI1020:
	movq	%rsp, %rbp
LCFI1021:
	leaq	_MM_Mod(%rip), %rax
	addq	$144, %rax
	movq	(%rax), %rax
	movq	%rax, -32(%rbp)
	leaq	_MM_Mod(%rip), %rax
	addq	$144, %rax
	movq	(%rax), %rax
	movq	%rax, -24(%rbp)
	movsd	-32(%rbp), %xmm0
	movsd	-24(%rbp), %xmm1
	divsd	%xmm1, %xmm0
	movsd	%xmm0, -8(%rbp)
	movsd	-8(%rbp), %xmm0
	cvttsd2siq	%xmm0, %rax
	movq	%rax, -16(%rbp)
	movsd	-32(%rbp), %xmm1
	cvtsi2sdq	-16(%rbp), %xmm0
	movsd	-8(%rbp), %xmm2
	ucomisd	%xmm0, %xmm2
	setae	%al
	testb	%al, %al
	je	L522
	cvtsi2sdq	-16(%rbp), %xmm0
	jmp	L523
L522:
	movq	-16(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
L523:
	movsd	-24(%rbp), %xmm2
	mulsd	%xmm2, %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
LCFI1022:
	ret
LFE288:
.no_dead_strip _Mod__Mod_param_f64_f64
_Mod__Mod_param_f64_f64:
LFB289:
	pushq	%rbp
LCFI1023:
	movq	%rsp, %rbp
LCFI1024:
	movsd	%xmm0, -40(%rbp)
	movsd	%xmm1, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -24(%rbp)
	movsd	-32(%rbp), %xmm0
	movsd	-24(%rbp), %xmm1
	divsd	%xmm1, %xmm0
	movsd	%xmm0, -8(%rbp)
	movsd	-8(%rbp), %xmm0
	cvttsd2siq	%xmm0, %rax
	movq	%rax, -16(%rbp)
	movsd	-32(%rbp), %xmm1
	cvtsi2sdq	-16(%rbp), %xmm0
	movsd	-8(%rbp), %xmm2
	ucomisd	%xmm0, %xmm2
	setae	%al
	testb	%al, %al
	je	L525
	cvtsi2sdq	-16(%rbp), %xmm0
	jmp	L526
L525:
	movq	-16(%rbp), %rax
	subq	$1, %rax
	cvtsi2sdq	%rax, %xmm0
L526:
	movsd	-24(%rbp), %xmm2
	mulsd	%xmm2, %xmm0
	movapd	%xmm1, %xmm2
	subsd	%xmm0, %xmm2
	movapd	%xmm2, %xmm0
	leave
LCFI1025:
	ret
LFE289:
.no_dead_strip _Mod__uMod_var_i32_u16
_Mod__uMod_var_i32_u16:
LFB290:
	pushq	%rbp
LCFI1026:
	movq	%rsp, %rbp
LCFI1027:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1028:
	ret
LFE290:
.no_dead_strip _Mod__Mod_var_i32_u16
_Mod__Mod_var_i32_u16:
LFB291:
	pushq	%rbp
LCFI1029:
	movq	%rsp, %rbp
LCFI1030:
	pushq	%rbx
LCFI1031:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L529
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L529
	addq	%rcx, %rdx
L529:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1032:
	ret
LFE291:
.no_dead_strip _Mod__uMod_param_i32_u16
_Mod__uMod_param_i32_u16:
LFB292:
	pushq	%rbp
LCFI1033:
	movq	%rsp, %rbp
LCFI1034:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movzwl	-6(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1035:
	ret
LFE292:
.no_dead_strip _Mod__Mod_param_i32_u16
_Mod__Mod_param_i32_u16:
LFB293:
	pushq	%rbp
LCFI1036:
	movq	%rsp, %rbp
LCFI1037:
	pushq	%rbx
LCFI1038:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzwl	-22(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L532
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L532
	addq	%rcx, %rdx
L532:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1039:
	ret
LFE293:
.no_dead_strip _Mod__uMod_var_i32_u32
_Mod__uMod_var_i32_u32:
LFB294:
	pushq	%rbp
LCFI1040:
	movq	%rsp, %rbp
LCFI1041:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1042:
	ret
LFE294:
.no_dead_strip _Mod__Mod_var_i32_u32
_Mod__Mod_var_i32_u32:
LFB295:
	pushq	%rbp
LCFI1043:
	movq	%rsp, %rbp
LCFI1044:
	pushq	%rbx
LCFI1045:
	subq	$8, %rsp
LCFI1046:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L535
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L535
	addq	%rcx, %rdx
L535:
	cmpq	$2147483647, %rdx
	jle	L536
	movl	$11169, %edi
	call	__m3_fault
L536:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L537
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L537
	addq	%rcx, %rdx
L537:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1047:
	ret
LFE295:
.no_dead_strip _Mod__uMod_param_i32_u32
_Mod__uMod_param_i32_u32:
LFB296:
	pushq	%rbp
LCFI1048:
	movq	%rsp, %rbp
LCFI1049:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cltq
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1050:
	ret
LFE296:
.no_dead_strip _Mod__Mod_param_i32_u32
_Mod__Mod_param_i32_u32:
LFB297:
	pushq	%rbp
LCFI1051:
	movq	%rsp, %rbp
LCFI1052:
	pushq	%rbx
LCFI1053:
	subq	$24, %rsp
LCFI1054:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L540
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L540
	addq	%rcx, %rdx
L540:
	cmpq	$2147483647, %rdx
	jle	L541
	movl	$11233, %edi
	call	__m3_fault
L541:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L542
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L542
	addq	%rcx, %rdx
L542:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1055:
	ret
LFE297:
.no_dead_strip _Mod__uMod_var_i32_LC
_Mod__uMod_var_i32_LC:
LFB298:
	pushq	%rbp
LCFI1056:
	movq	%rsp, %rbp
LCFI1057:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1058:
	ret
LFE298:
.no_dead_strip _Mod__Mod_var_i32_LC
_Mod__Mod_var_i32_LC:
LFB299:
	pushq	%rbp
LCFI1059:
	movq	%rsp, %rbp
LCFI1060:
	pushq	%rbx
LCFI1061:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L545
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L545
	addq	%rcx, %rdx
L545:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1062:
	ret
LFE299:
.no_dead_strip _Mod__uMod_param_i32_LC
_Mod__uMod_param_i32_LC:
LFB300:
	pushq	%rbp
LCFI1063:
	movq	%rsp, %rbp
LCFI1064:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1065:
	ret
LFE300:
.no_dead_strip _Mod__Mod_param_i32_LC
_Mod__Mod_param_i32_LC:
LFB301:
	pushq	%rbp
LCFI1066:
	movq	%rsp, %rbp
LCFI1067:
	pushq	%rbx
LCFI1068:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L548
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L548
	addq	%rcx, %rdx
L548:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1069:
	ret
LFE301:
.no_dead_strip _Mod__uMod_var_i32_I
_Mod__uMod_var_i32_I:
LFB302:
	pushq	%rbp
LCFI1070:
	movq	%rsp, %rbp
LCFI1071:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1072:
	ret
LFE302:
.no_dead_strip _Mod__Mod_var_i32_I
_Mod__Mod_var_i32_I:
LFB303:
	pushq	%rbp
LCFI1073:
	movq	%rsp, %rbp
LCFI1074:
	pushq	%rbx
LCFI1075:
	subq	$8, %rsp
LCFI1076:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L551
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L551
	addq	%rcx, %rdx
L551:
	cmpq	$-2147483648, %rdx
	jl	L552
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L553
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L553
	addq	%rcx, %rdx
L553:
	cmpq	$2147483647, %rdx
	jle	L554
L552:
	movl	$11425, %edi
	call	__m3_fault
L554:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L555
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L555
	addq	%rcx, %rdx
L555:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1077:
	ret
LFE303:
.no_dead_strip _Mod__uMod_param_i32_I
_Mod__uMod_param_i32_I:
LFB304:
	pushq	%rbp
LCFI1078:
	movq	%rsp, %rbp
LCFI1079:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1080:
	ret
LFE304:
.no_dead_strip _Mod__Mod_param_i32_I
_Mod__Mod_param_i32_I:
LFB305:
	pushq	%rbp
LCFI1081:
	movq	%rsp, %rbp
LCFI1082:
	subq	$16, %rsp
LCFI1083:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L558
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L558
	addq	-16(%rbp), %rdx
L558:
	cmpq	$-2147483648, %rdx
	jl	L559
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L560
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L560
	addq	-16(%rbp), %rdx
L560:
	cmpq	$2147483647, %rdx
	jle	L561
L559:
	movl	$11489, %edi
	call	__m3_fault
L561:
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L562
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L562
	addq	-16(%rbp), %rdx
L562:
	movl	%edx, %eax
	leave
LCFI1084:
	ret
LFE305:
.no_dead_strip _Mod__uMod_var_i32_i16
_Mod__uMod_var_i32_i16:
LFB306:
	pushq	%rbp
LCFI1085:
	movq	%rsp, %rbp
LCFI1086:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1087:
	ret
LFE306:
.no_dead_strip _Mod__Mod_var_i32_i16
_Mod__Mod_var_i32_i16:
LFB307:
	pushq	%rbp
LCFI1088:
	movq	%rsp, %rbp
LCFI1089:
	pushq	%rbx
LCFI1090:
	subq	$8, %rsp
LCFI1091:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L565
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L565
	addq	%rcx, %rdx
L565:
	cmpq	$-2147483648, %rdx
	jl	L566
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L567
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L567
	addq	%rcx, %rdx
L567:
	cmpq	$2147483647, %rdx
	jle	L568
L566:
	movl	$11553, %edi
	call	__m3_fault
L568:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L569
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L569
	addq	%rcx, %rdx
L569:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1092:
	ret
LFE307:
.no_dead_strip _Mod__uMod_param_i32_i16
_Mod__uMod_param_i32_i16:
LFB308:
	pushq	%rbp
LCFI1093:
	movq	%rsp, %rbp
LCFI1094:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movswq	-6(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1095:
	ret
LFE308:
.no_dead_strip _Mod__Mod_param_i32_i16
_Mod__Mod_param_i32_i16:
LFB309:
	pushq	%rbp
LCFI1096:
	movq	%rsp, %rbp
LCFI1097:
	pushq	%rbx
LCFI1098:
	subq	$24, %rsp
LCFI1099:
	movl	%edi, -20(%rbp)
	movw	%si, -22(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L572
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L572
	addq	%rcx, %rdx
L572:
	cmpq	$-2147483648, %rdx
	jl	L573
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L574
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L574
	addq	%rcx, %rdx
L574:
	cmpq	$2147483647, %rdx
	jle	L575
L573:
	movl	$11617, %edi
	call	__m3_fault
L575:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movswq	-22(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L576
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L576
	addq	%rcx, %rdx
L576:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1100:
	ret
LFE309:
.no_dead_strip _Mod__uMod_var_i32_u64
_Mod__uMod_var_i32_u64:
LFB310:
	pushq	%rbp
LCFI1101:
	movq	%rsp, %rbp
LCFI1102:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1103:
	ret
LFE310:
.no_dead_strip _Mod__Mod_var_i32_u64
_Mod__Mod_var_i32_u64:
LFB311:
	pushq	%rbp
LCFI1104:
	movq	%rsp, %rbp
LCFI1105:
	pushq	%rbx
LCFI1106:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L579
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L579
	addq	%rcx, %rdx
L579:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1107:
	ret
LFE311:
.no_dead_strip _Mod__uMod_param_i32_u64
_Mod__uMod_param_i32_u64:
LFB312:
	pushq	%rbp
LCFI1108:
	movq	%rsp, %rbp
LCFI1109:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1110:
	ret
LFE312:
.no_dead_strip _Mod__Mod_param_i32_u64
_Mod__Mod_param_i32_u64:
LFB313:
	pushq	%rbp
LCFI1111:
	movq	%rsp, %rbp
LCFI1112:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L582
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L582
	addq	-16(%rbp), %rdx
L582:
	movq	%rdx, %rax
	leave
LCFI1113:
	ret
LFE313:
.no_dead_strip _Mod__uMod_var_i32_i32
_Mod__uMod_var_i32_i32:
LFB314:
	pushq	%rbp
LCFI1114:
	movq	%rsp, %rbp
LCFI1115:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1116:
	ret
LFE314:
.no_dead_strip _Mod__Mod_var_i32_i32
_Mod__Mod_var_i32_i32:
LFB315:
	pushq	%rbp
LCFI1117:
	movq	%rsp, %rbp
LCFI1118:
	pushq	%rbx
LCFI1119:
	subq	$8, %rsp
LCFI1120:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L585
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L585
	addq	%rcx, %rdx
L585:
	cmpq	$-2147483648, %rdx
	jl	L586
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L587
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L587
	addq	%rcx, %rdx
L587:
	cmpq	$2147483647, %rdx
	jle	L588
L586:
	movl	$11809, %edi
	call	__m3_fault
L588:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L589
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L589
	addq	%rcx, %rdx
L589:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1121:
	ret
LFE315:
.no_dead_strip _Mod__uMod_param_i32_i32
_Mod__uMod_param_i32_i32:
LFB316:
	pushq	%rbp
LCFI1122:
	movq	%rsp, %rbp
LCFI1123:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1124:
	ret
LFE316:
.no_dead_strip _Mod__Mod_param_i32_i32
_Mod__Mod_param_i32_i32:
LFB317:
	pushq	%rbp
LCFI1125:
	movq	%rsp, %rbp
LCFI1126:
	pushq	%rbx
LCFI1127:
	subq	$24, %rsp
LCFI1128:
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L592
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L592
	addq	%rcx, %rdx
L592:
	cmpq	$-2147483648, %rdx
	jl	L593
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L594
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L594
	addq	%rcx, %rdx
L594:
	cmpq	$2147483647, %rdx
	jle	L595
L593:
	movl	$11873, %edi
	call	__m3_fault
L595:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L596
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L596
	addq	%rcx, %rdx
L596:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1129:
	ret
LFE317:
.no_dead_strip _Mod__uMod_var_i32_C
_Mod__uMod_var_i32_C:
LFB318:
	pushq	%rbp
LCFI1130:
	movq	%rsp, %rbp
LCFI1131:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1132:
	ret
LFE318:
.no_dead_strip _Mod__Mod_var_i32_C
_Mod__Mod_var_i32_C:
LFB319:
	pushq	%rbp
LCFI1133:
	movq	%rsp, %rbp
LCFI1134:
	pushq	%rbx
LCFI1135:
	subq	$8, %rsp
LCFI1136:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L599
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L599
	addq	%rcx, %rdx
L599:
	cmpq	$2147483647, %rdx
	jle	L600
	movl	$11937, %edi
	call	__m3_fault
L600:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L601
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L601
	addq	%rcx, %rdx
L601:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1137:
	ret
LFE319:
.no_dead_strip _Mod__uMod_param_i32_C
_Mod__uMod_param_i32_C:
LFB320:
	pushq	%rbp
LCFI1138:
	movq	%rsp, %rbp
LCFI1139:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1140:
	ret
LFE320:
.no_dead_strip _Mod__Mod_param_i32_C
_Mod__Mod_param_i32_C:
LFB321:
	pushq	%rbp
LCFI1141:
	movq	%rsp, %rbp
LCFI1142:
	pushq	%rbx
LCFI1143:
	subq	$24, %rsp
LCFI1144:
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L604
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L604
	addq	%rcx, %rdx
L604:
	cmpq	$2147483647, %rdx
	jle	L605
	movl	$12001, %edi
	call	__m3_fault
L605:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L606
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L606
	addq	%rcx, %rdx
L606:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1145:
	ret
LFE321:
.no_dead_strip _Mod__uMod_var_i32_u8
_Mod__uMod_var_i32_u8:
LFB322:
	pushq	%rbp
LCFI1146:
	movq	%rsp, %rbp
LCFI1147:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1148:
	ret
LFE322:
.no_dead_strip _Mod__Mod_var_i32_u8
_Mod__Mod_var_i32_u8:
LFB323:
	pushq	%rbp
LCFI1149:
	movq	%rsp, %rbp
LCFI1150:
	pushq	%rbx
LCFI1151:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L609
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L609
	addq	%rcx, %rdx
L609:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1152:
	ret
LFE323:
.no_dead_strip _Mod__uMod_param_i32_u8
_Mod__uMod_param_i32_u8:
LFB324:
	pushq	%rbp
LCFI1153:
	movq	%rsp, %rbp
LCFI1154:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movzbl	-5(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1155:
	ret
LFE324:
.no_dead_strip _Mod__Mod_param_i32_u8
_Mod__Mod_param_i32_u8:
LFB325:
	pushq	%rbp
LCFI1156:
	movq	%rsp, %rbp
LCFI1157:
	pushq	%rbx
LCFI1158:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movzbl	-21(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L612
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L612
	addq	%rcx, %rdx
L612:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1159:
	ret
LFE325:
.no_dead_strip _Mod__uMod_var_i32_i64
_Mod__uMod_var_i32_i64:
LFB326:
	pushq	%rbp
LCFI1160:
	movq	%rsp, %rbp
LCFI1161:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1162:
	ret
LFE326:
.no_dead_strip _Mod__Mod_var_i32_i64
_Mod__Mod_var_i32_i64:
LFB327:
	pushq	%rbp
LCFI1163:
	movq	%rsp, %rbp
LCFI1164:
	pushq	%rbx
LCFI1165:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L615
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L615
	addq	%rcx, %rdx
L615:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1166:
	ret
LFE327:
.no_dead_strip _Mod__uMod_param_i32_i64
_Mod__uMod_param_i32_i64:
LFB328:
	pushq	%rbp
LCFI1167:
	movq	%rsp, %rbp
LCFI1168:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1169:
	ret
LFE328:
.no_dead_strip _Mod__Mod_param_i32_i64
_Mod__Mod_param_i32_i64:
LFB329:
	pushq	%rbp
LCFI1170:
	movq	%rsp, %rbp
LCFI1171:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L618
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L618
	addq	-16(%rbp), %rdx
L618:
	movq	%rdx, %rax
	leave
LCFI1172:
	ret
LFE329:
.no_dead_strip _Mod__uMod_var_i32_L
_Mod__uMod_var_i32_L:
LFB330:
	pushq	%rbp
LCFI1173:
	movq	%rsp, %rbp
LCFI1174:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1175:
	ret
LFE330:
.no_dead_strip _Mod__Mod_var_i32_L
_Mod__Mod_var_i32_L:
LFB331:
	pushq	%rbp
LCFI1176:
	movq	%rsp, %rbp
LCFI1177:
	pushq	%rbx
LCFI1178:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L621
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L621
	addq	%rcx, %rdx
L621:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1179:
	ret
LFE331:
.no_dead_strip _Mod__uMod_param_i32_L
_Mod__uMod_param_i32_L:
LFB332:
	pushq	%rbp
LCFI1180:
	movq	%rsp, %rbp
LCFI1181:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1182:
	ret
LFE332:
.no_dead_strip _Mod__Mod_param_i32_L
_Mod__Mod_param_i32_L:
LFB333:
	pushq	%rbp
LCFI1183:
	movq	%rsp, %rbp
LCFI1184:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L624
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L624
	addq	-16(%rbp), %rdx
L624:
	movq	%rdx, %rax
	leave
LCFI1185:
	ret
LFE333:
.no_dead_strip _Mod__uMod_var_i32_i8
_Mod__uMod_var_i32_i8:
LFB334:
	pushq	%rbp
LCFI1186:
	movq	%rsp, %rbp
LCFI1187:
	movl	152+_MM_Mod(%rip), %eax
	cltq
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1188:
	ret
LFE334:
.no_dead_strip _Mod__Mod_var_i32_i8
_Mod__Mod_var_i32_i8:
LFB335:
	pushq	%rbp
LCFI1189:
	movq	%rsp, %rbp
LCFI1190:
	pushq	%rbx
LCFI1191:
	subq	$8, %rsp
LCFI1192:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L627
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L627
	addq	%rcx, %rdx
L627:
	cmpq	$-2147483648, %rdx
	jl	L628
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L629
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L629
	addq	%rcx, %rdx
L629:
	cmpq	$2147483647, %rdx
	jle	L630
L628:
	movl	$12449, %edi
	call	__m3_fault
L630:
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L631
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L631
	addq	%rcx, %rdx
L631:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1193:
	ret
LFE335:
.no_dead_strip _Mod__uMod_param_i32_i8
_Mod__uMod_param_i32_i8:
LFB336:
	pushq	%rbp
LCFI1194:
	movq	%rsp, %rbp
LCFI1195:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	cltq
	movsbq	-5(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1196:
	ret
LFE336:
.no_dead_strip _Mod__Mod_param_i32_i8
_Mod__Mod_param_i32_i8:
LFB337:
	pushq	%rbp
LCFI1197:
	movq	%rsp, %rbp
LCFI1198:
	pushq	%rbx
LCFI1199:
	subq	$24, %rsp
LCFI1200:
	movl	%edi, -20(%rbp)
	movb	%sil, -21(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L634
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L634
	addq	%rcx, %rdx
L634:
	cmpq	$-2147483648, %rdx
	jl	L635
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L636
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L636
	addq	%rcx, %rdx
L636:
	cmpq	$2147483647, %rdx
	jle	L637
L635:
	movl	$12513, %edi
	call	__m3_fault
L637:
	movl	-20(%rbp), %eax
	movslq	%eax, %rbx
	movsbq	-21(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L638
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L638
	addq	%rcx, %rdx
L638:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1201:
	ret
LFE337:
.no_dead_strip _Mod__uMod_var_C_u16
_Mod__uMod_var_C_u16:
LFB338:
	pushq	%rbp
LCFI1202:
	movq	%rsp, %rbp
LCFI1203:
	movq	160+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1204:
	ret
LFE338:
.no_dead_strip _Mod__Mod_var_C_u16
_Mod__Mod_var_C_u16:
LFB339:
	pushq	%rbp
LCFI1205:
	movq	%rsp, %rbp
LCFI1206:
	pushq	%rbx
LCFI1207:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L641
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L641
	addq	%rcx, %rdx
L641:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1208:
	ret
LFE339:
.no_dead_strip _Mod__uMod_param_C_u16
_Mod__uMod_param_C_u16:
LFB340:
	pushq	%rbp
LCFI1209:
	movq	%rsp, %rbp
LCFI1210:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1211:
	ret
LFE340:
.no_dead_strip _Mod__Mod_param_C_u16
_Mod__Mod_param_C_u16:
LFB341:
	pushq	%rbp
LCFI1212:
	movq	%rsp, %rbp
LCFI1213:
	pushq	%rbx
LCFI1214:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movzwl	-26(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L644
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L644
	addq	%rcx, %rdx
L644:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1215:
	ret
LFE341:
.no_dead_strip _Mod__uMod_var_C_u32
_Mod__uMod_var_C_u32:
LFB342:
	pushq	%rbp
LCFI1216:
	movq	%rsp, %rbp
LCFI1217:
	movq	160+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1218:
	ret
LFE342:
.no_dead_strip _Mod__Mod_var_C_u32
_Mod__Mod_var_C_u32:
LFB343:
	pushq	%rbp
LCFI1219:
	movq	%rsp, %rbp
LCFI1220:
	pushq	%rbx
LCFI1221:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L647
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L647
	addq	%rcx, %rdx
L647:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1222:
	ret
LFE343:
.no_dead_strip _Mod__uMod_param_C_u32
_Mod__uMod_param_C_u32:
LFB344:
	pushq	%rbp
LCFI1223:
	movq	%rsp, %rbp
LCFI1224:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1225:
	ret
LFE344:
.no_dead_strip _Mod__Mod_param_C_u32
_Mod__Mod_param_C_u32:
LFB345:
	pushq	%rbp
LCFI1226:
	movq	%rsp, %rbp
LCFI1227:
	pushq	%rbx
LCFI1228:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	mov	-28(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L650
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L650
	addq	%rcx, %rdx
L650:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1229:
	ret
LFE345:
.no_dead_strip _Mod__uMod_var_C_LC
_Mod__uMod_var_C_LC:
LFB346:
	pushq	%rbp
LCFI1230:
	movq	%rsp, %rbp
LCFI1231:
	movq	160+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1232:
	ret
LFE346:
.no_dead_strip _Mod__Mod_var_C_LC
_Mod__Mod_var_C_LC:
LFB347:
	pushq	%rbp
LCFI1233:
	movq	%rsp, %rbp
LCFI1234:
	pushq	%rbx
LCFI1235:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L653
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L653
	addq	%rcx, %rdx
L653:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1236:
	ret
LFE347:
.no_dead_strip _Mod__uMod_param_C_LC
_Mod__uMod_param_C_LC:
LFB348:
	pushq	%rbp
LCFI1237:
	movq	%rsp, %rbp
LCFI1238:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1239:
	ret
LFE348:
.no_dead_strip _Mod__Mod_param_C_LC
_Mod__Mod_param_C_LC:
LFB349:
	pushq	%rbp
LCFI1240:
	movq	%rsp, %rbp
LCFI1241:
	pushq	%rbx
LCFI1242:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L656
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L656
	addq	%rcx, %rdx
L656:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1243:
	ret
LFE349:
.no_dead_strip _Mod__uMod_var_C_I
_Mod__uMod_var_C_I:
LFB350:
	pushq	%rbp
LCFI1244:
	movq	%rsp, %rbp
LCFI1245:
	movq	160+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1246:
	ret
LFE350:
.no_dead_strip _Mod__Mod_var_C_I
_Mod__Mod_var_C_I:
LFB351:
	pushq	%rbp
LCFI1247:
	movq	%rsp, %rbp
LCFI1248:
	pushq	%rbx
LCFI1249:
	subq	$8, %rsp
LCFI1250:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L659
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L659
	addq	%rcx, %rdx
L659:
	testq	%rdx, %rdx
	jns	L660
	movl	$12961, %edi
	call	__m3_fault
L660:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L661
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L661
	addq	%rcx, %rdx
L661:
	movq	%rdx, %rax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1251:
	ret
LFE351:
.no_dead_strip _Mod__uMod_param_C_I
_Mod__uMod_param_C_I:
LFB352:
	pushq	%rbp
LCFI1252:
	movq	%rsp, %rbp
LCFI1253:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1254:
	ret
LFE352:
.no_dead_strip _Mod__Mod_param_C_I
_Mod__Mod_param_C_I:
LFB353:
	pushq	%rbp
LCFI1255:
	movq	%rsp, %rbp
LCFI1256:
	subq	$16, %rsp
LCFI1257:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L664
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L664
	addq	-16(%rbp), %rdx
L664:
	testq	%rdx, %rdx
	jns	L665
	movl	$13025, %edi
	call	__m3_fault
L665:
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L666
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L666
	addq	-16(%rbp), %rdx
L666:
	movq	%rdx, %rax
	leave
LCFI1258:
	ret
LFE353:
.no_dead_strip _Mod__uMod_var_C_i16
_Mod__uMod_var_C_i16:
LFB354:
	pushq	%rbp
LCFI1259:
	movq	%rsp, %rbp
LCFI1260:
	movq	160+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1261:
	ret
LFE354:
.no_dead_strip _Mod__Mod_var_C_i16
_Mod__Mod_var_C_i16:
LFB355:
	pushq	%rbp
LCFI1262:
	movq	%rsp, %rbp
LCFI1263:
	pushq	%rbx
LCFI1264:
	subq	$8, %rsp
LCFI1265:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L669
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L669
	addq	%rcx, %rdx
L669:
	testq	%rdx, %rdx
	jns	L670
	movl	$13089, %edi
	call	__m3_fault
L670:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L671
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L671
	addq	%rcx, %rdx
L671:
	movq	%rdx, %rax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1266:
	ret
LFE355:
.no_dead_strip _Mod__uMod_param_C_i16
_Mod__uMod_param_C_i16:
LFB356:
	pushq	%rbp
LCFI1267:
	movq	%rsp, %rbp
LCFI1268:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1269:
	ret
LFE356:
.no_dead_strip _Mod__Mod_param_C_i16
_Mod__Mod_param_C_i16:
LFB357:
	pushq	%rbp
LCFI1270:
	movq	%rsp, %rbp
LCFI1271:
	pushq	%rbx
LCFI1272:
	subq	$24, %rsp
LCFI1273:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L674
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L674
	addq	%rcx, %rdx
L674:
	testq	%rdx, %rdx
	jns	L675
	movl	$13153, %edi
	call	__m3_fault
L675:
	movq	-24(%rbp), %rbx
	movswq	-26(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L676
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L676
	addq	%rcx, %rdx
L676:
	movq	%rdx, %rax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1274:
	ret
LFE357:
.no_dead_strip _Mod__uMod_var_C_u64
_Mod__uMod_var_C_u64:
LFB358:
	pushq	%rbp
LCFI1275:
	movq	%rsp, %rbp
LCFI1276:
	movq	160+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1277:
	ret
LFE358:
.no_dead_strip _Mod__Mod_var_C_u64
_Mod__Mod_var_C_u64:
LFB359:
	pushq	%rbp
LCFI1278:
	movq	%rsp, %rbp
LCFI1279:
	pushq	%rbx
LCFI1280:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L679
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L679
	addq	%rcx, %rdx
L679:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1281:
	ret
LFE359:
.no_dead_strip _Mod__uMod_param_C_u64
_Mod__uMod_param_C_u64:
LFB360:
	pushq	%rbp
LCFI1282:
	movq	%rsp, %rbp
LCFI1283:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1284:
	ret
LFE360:
.no_dead_strip _Mod__Mod_param_C_u64
_Mod__Mod_param_C_u64:
LFB361:
	pushq	%rbp
LCFI1285:
	movq	%rsp, %rbp
LCFI1286:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L682
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L682
	addq	-16(%rbp), %rdx
L682:
	movq	%rdx, %rax
	leave
LCFI1287:
	ret
LFE361:
.no_dead_strip _Mod__uMod_var_C_i32
_Mod__uMod_var_C_i32:
LFB362:
	pushq	%rbp
LCFI1288:
	movq	%rsp, %rbp
LCFI1289:
	movq	160+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1290:
	ret
LFE362:
.no_dead_strip _Mod__Mod_var_C_i32
_Mod__Mod_var_C_i32:
LFB363:
	pushq	%rbp
LCFI1291:
	movq	%rsp, %rbp
LCFI1292:
	pushq	%rbx
LCFI1293:
	subq	$8, %rsp
LCFI1294:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L685
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L685
	addq	%rcx, %rdx
L685:
	testq	%rdx, %rdx
	jns	L686
	movl	$13345, %edi
	call	__m3_fault
L686:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L687
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L687
	addq	%rcx, %rdx
L687:
	movq	%rdx, %rax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1295:
	ret
LFE363:
.no_dead_strip _Mod__uMod_param_C_i32
_Mod__uMod_param_C_i32:
LFB364:
	pushq	%rbp
LCFI1296:
	movq	%rsp, %rbp
LCFI1297:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movslq	%eax, %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1298:
	ret
LFE364:
.no_dead_strip _Mod__Mod_param_C_i32
_Mod__Mod_param_C_i32:
LFB365:
	pushq	%rbp
LCFI1299:
	movq	%rsp, %rbp
LCFI1300:
	pushq	%rbx
LCFI1301:
	subq	$24, %rsp
LCFI1302:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L690
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L690
	addq	%rcx, %rdx
L690:
	testq	%rdx, %rdx
	jns	L691
	movl	$13409, %edi
	call	__m3_fault
L691:
	movq	-24(%rbp), %rbx
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L692
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L692
	addq	%rcx, %rdx
L692:
	movq	%rdx, %rax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1303:
	ret
LFE365:
.no_dead_strip _Mod__uMod_var_C_C
_Mod__uMod_var_C_C:
LFB366:
	pushq	%rbp
LCFI1304:
	movq	%rsp, %rbp
LCFI1305:
	movq	160+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1306:
	ret
LFE366:
.no_dead_strip _Mod__Mod_var_C_C
_Mod__Mod_var_C_C:
LFB367:
	pushq	%rbp
LCFI1307:
	movq	%rsp, %rbp
LCFI1308:
	pushq	%rbx
LCFI1309:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L695
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L695
	addq	%rcx, %rdx
L695:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1310:
	ret
LFE367:
.no_dead_strip _Mod__uMod_param_C_C
_Mod__uMod_param_C_C:
LFB368:
	pushq	%rbp
LCFI1311:
	movq	%rsp, %rbp
LCFI1312:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1313:
	ret
LFE368:
.no_dead_strip _Mod__Mod_param_C_C
_Mod__Mod_param_C_C:
LFB369:
	pushq	%rbp
LCFI1314:
	movq	%rsp, %rbp
LCFI1315:
	pushq	%rbx
LCFI1316:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L698
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L698
	addq	%rcx, %rdx
L698:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1317:
	ret
LFE369:
.no_dead_strip _Mod__uMod_var_C_u8
_Mod__uMod_var_C_u8:
LFB370:
	pushq	%rbp
LCFI1318:
	movq	%rsp, %rbp
LCFI1319:
	movq	160+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1320:
	ret
LFE370:
.no_dead_strip _Mod__Mod_var_C_u8
_Mod__Mod_var_C_u8:
LFB371:
	pushq	%rbp
LCFI1321:
	movq	%rsp, %rbp
LCFI1322:
	pushq	%rbx
LCFI1323:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L701
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L701
	addq	%rcx, %rdx
L701:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1324:
	ret
LFE371:
.no_dead_strip _Mod__uMod_param_C_u8
_Mod__uMod_param_C_u8:
LFB372:
	pushq	%rbp
LCFI1325:
	movq	%rsp, %rbp
LCFI1326:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %ecx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1327:
	ret
LFE372:
.no_dead_strip _Mod__Mod_param_C_u8
_Mod__Mod_param_C_u8:
LFB373:
	pushq	%rbp
LCFI1328:
	movq	%rsp, %rbp
LCFI1329:
	pushq	%rbx
LCFI1330:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movzbl	-25(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L704
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L704
	addq	%rcx, %rdx
L704:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1331:
	ret
LFE373:
.no_dead_strip _Mod__uMod_var_C_i64
_Mod__uMod_var_C_i64:
LFB374:
	pushq	%rbp
LCFI1332:
	movq	%rsp, %rbp
LCFI1333:
	movq	160+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1334:
	ret
LFE374:
.no_dead_strip _Mod__Mod_var_C_i64
_Mod__Mod_var_C_i64:
LFB375:
	pushq	%rbp
LCFI1335:
	movq	%rsp, %rbp
LCFI1336:
	pushq	%rbx
LCFI1337:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L707
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L707
	addq	%rcx, %rdx
L707:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1338:
	ret
LFE375:
.no_dead_strip _Mod__uMod_param_C_i64
_Mod__uMod_param_C_i64:
LFB376:
	pushq	%rbp
LCFI1339:
	movq	%rsp, %rbp
LCFI1340:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1341:
	ret
LFE376:
.no_dead_strip _Mod__Mod_param_C_i64
_Mod__Mod_param_C_i64:
LFB377:
	pushq	%rbp
LCFI1342:
	movq	%rsp, %rbp
LCFI1343:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L710
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L710
	addq	-16(%rbp), %rdx
L710:
	movq	%rdx, %rax
	leave
LCFI1344:
	ret
LFE377:
.no_dead_strip _Mod__uMod_var_C_L
_Mod__uMod_var_C_L:
LFB378:
	pushq	%rbp
LCFI1345:
	movq	%rsp, %rbp
LCFI1346:
	movq	160+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1347:
	ret
LFE378:
.no_dead_strip _Mod__Mod_var_C_L
_Mod__Mod_var_C_L:
LFB379:
	pushq	%rbp
LCFI1348:
	movq	%rsp, %rbp
LCFI1349:
	pushq	%rbx
LCFI1350:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L713
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L713
	addq	%rcx, %rdx
L713:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1351:
	ret
LFE379:
.no_dead_strip _Mod__uMod_param_C_L
_Mod__uMod_param_C_L:
LFB380:
	pushq	%rbp
LCFI1352:
	movq	%rsp, %rbp
LCFI1353:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1354:
	ret
LFE380:
.no_dead_strip _Mod__Mod_param_C_L
_Mod__Mod_param_C_L:
LFB381:
	pushq	%rbp
LCFI1355:
	movq	%rsp, %rbp
LCFI1356:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L716
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L716
	addq	-16(%rbp), %rdx
L716:
	movq	%rdx, %rax
	leave
LCFI1357:
	ret
LFE381:
.no_dead_strip _Mod__uMod_var_C_i8
_Mod__uMod_var_C_i8:
LFB382:
	pushq	%rbp
LCFI1358:
	movq	%rsp, %rbp
LCFI1359:
	movq	160+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1360:
	ret
LFE382:
.no_dead_strip _Mod__Mod_var_C_i8
_Mod__Mod_var_C_i8:
LFB383:
	pushq	%rbp
LCFI1361:
	movq	%rsp, %rbp
LCFI1362:
	pushq	%rbx
LCFI1363:
	subq	$8, %rsp
LCFI1364:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L719
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L719
	addq	%rcx, %rdx
L719:
	testq	%rdx, %rdx
	jns	L720
	movl	$13985, %edi
	call	__m3_fault
L720:
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L721
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L721
	addq	%rcx, %rdx
L721:
	movq	%rdx, %rax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1365:
	ret
LFE383:
.no_dead_strip _Mod__uMod_param_C_i8
_Mod__uMod_param_C_i8:
LFB384:
	pushq	%rbp
LCFI1366:
	movq	%rsp, %rbp
LCFI1367:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1368:
	ret
LFE384:
.no_dead_strip _Mod__Mod_param_C_i8
_Mod__Mod_param_C_i8:
LFB385:
	pushq	%rbp
LCFI1369:
	movq	%rsp, %rbp
LCFI1370:
	pushq	%rbx
LCFI1371:
	subq	$24, %rsp
LCFI1372:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L724
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L724
	addq	%rcx, %rdx
L724:
	testq	%rdx, %rdx
	jns	L725
	movl	$14049, %edi
	call	__m3_fault
L725:
	movq	-24(%rbp), %rbx
	movsbq	-25(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L726
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L726
	addq	%rcx, %rdx
L726:
	movq	%rdx, %rax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1373:
	ret
LFE385:
.no_dead_strip _Mod__uMod_var_u8_u16
_Mod__uMod_var_u8_u16:
LFB386:
	pushq	%rbp
LCFI1374:
	movq	%rsp, %rbp
LCFI1375:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1376:
	ret
LFE386:
.no_dead_strip _Mod__Mod_var_u8_u16
_Mod__Mod_var_u8_u16:
LFB387:
	pushq	%rbp
LCFI1377:
	movq	%rsp, %rbp
LCFI1378:
	pushq	%rbx
LCFI1379:
	subq	$8, %rsp
LCFI1380:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L729
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L729
	addq	%rcx, %rdx
L729:
	cmpq	$255, %rdx
	jle	L730
	movl	$14113, %edi
	call	__m3_fault
L730:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L731
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L731
	addq	%rcx, %rdx
L731:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1381:
	ret
LFE387:
.no_dead_strip _Mod__uMod_param_u8_u16
_Mod__uMod_param_u8_u16:
LFB388:
	pushq	%rbp
LCFI1382:
	movq	%rsp, %rbp
LCFI1383:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %eax
	movzwl	-4(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1384:
	ret
LFE388:
.no_dead_strip _Mod__Mod_param_u8_u16
_Mod__Mod_param_u8_u16:
LFB389:
	pushq	%rbp
LCFI1385:
	movq	%rsp, %rbp
LCFI1386:
	pushq	%rbx
LCFI1387:
	subq	$24, %rsp
LCFI1388:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L734
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L734
	addq	%rcx, %rdx
L734:
	cmpq	$255, %rdx
	jle	L735
	movl	$14177, %edi
	call	__m3_fault
L735:
	movzbl	-17(%rbp), %ebx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L736
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L736
	addq	%rcx, %rdx
L736:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1389:
	ret
LFE389:
.no_dead_strip _Mod__uMod_var_u8_u32
_Mod__uMod_var_u8_u32:
LFB390:
	pushq	%rbp
LCFI1390:
	movq	%rsp, %rbp
LCFI1391:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1392:
	ret
LFE390:
.no_dead_strip _Mod__Mod_var_u8_u32
_Mod__Mod_var_u8_u32:
LFB391:
	pushq	%rbp
LCFI1393:
	movq	%rsp, %rbp
LCFI1394:
	pushq	%rbx
LCFI1395:
	subq	$8, %rsp
LCFI1396:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L739
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L739
	addq	%rcx, %rdx
L739:
	cmpq	$255, %rdx
	jle	L740
	movl	$14241, %edi
	call	__m3_fault
L740:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L741
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L741
	addq	%rcx, %rdx
L741:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1397:
	ret
LFE391:
.no_dead_strip _Mod__uMod_param_u8_u32
_Mod__uMod_param_u8_u32:
LFB392:
	pushq	%rbp
LCFI1398:
	movq	%rsp, %rbp
LCFI1399:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %eax
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1400:
	ret
LFE392:
.no_dead_strip _Mod__Mod_param_u8_u32
_Mod__Mod_param_u8_u32:
LFB393:
	pushq	%rbp
LCFI1401:
	movq	%rsp, %rbp
LCFI1402:
	pushq	%rbx
LCFI1403:
	subq	$24, %rsp
LCFI1404:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L744
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L744
	addq	%rcx, %rdx
L744:
	cmpq	$255, %rdx
	jle	L745
	movl	$14305, %edi
	call	__m3_fault
L745:
	movzbl	-17(%rbp), %ebx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L746
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L746
	addq	%rcx, %rdx
L746:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1405:
	ret
LFE393:
.no_dead_strip _Mod__uMod_var_u8_LC
_Mod__uMod_var_u8_LC:
LFB394:
	pushq	%rbp
LCFI1406:
	movq	%rsp, %rbp
LCFI1407:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1408:
	ret
LFE394:
.no_dead_strip _Mod__Mod_var_u8_LC
_Mod__Mod_var_u8_LC:
LFB395:
	pushq	%rbp
LCFI1409:
	movq	%rsp, %rbp
LCFI1410:
	pushq	%rbx
LCFI1411:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L749
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L749
	addq	%rcx, %rdx
L749:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1412:
	ret
LFE395:
.no_dead_strip _Mod__uMod_param_u8_LC
_Mod__uMod_param_u8_LC:
LFB396:
	pushq	%rbp
LCFI1413:
	movq	%rsp, %rbp
LCFI1414:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1415:
	ret
LFE396:
.no_dead_strip _Mod__Mod_param_u8_LC
_Mod__Mod_param_u8_LC:
LFB397:
	pushq	%rbp
LCFI1416:
	movq	%rsp, %rbp
LCFI1417:
	pushq	%rbx
LCFI1418:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L752
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L752
	addq	%rcx, %rdx
L752:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1419:
	ret
LFE397:
.no_dead_strip _Mod__uMod_var_u8_I
_Mod__uMod_var_u8_I:
LFB398:
	pushq	%rbp
LCFI1420:
	movq	%rsp, %rbp
LCFI1421:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1422:
	ret
LFE398:
.no_dead_strip _Mod__Mod_var_u8_I
_Mod__Mod_var_u8_I:
LFB399:
	pushq	%rbp
LCFI1423:
	movq	%rsp, %rbp
LCFI1424:
	pushq	%rbx
LCFI1425:
	subq	$8, %rsp
LCFI1426:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L755
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L755
	addq	%rcx, %rdx
L755:
	cmpq	$255, %rdx
	jle	L756
	movl	$14497, %edi
	call	__m3_fault
L756:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L757
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L757
	addq	%rcx, %rdx
L757:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1427:
	ret
LFE399:
.no_dead_strip _Mod__uMod_param_u8_I
_Mod__uMod_param_u8_I:
LFB400:
	pushq	%rbp
LCFI1428:
	movq	%rsp, %rbp
LCFI1429:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1430:
	ret
LFE400:
.no_dead_strip _Mod__Mod_param_u8_I
_Mod__Mod_param_u8_I:
LFB401:
	pushq	%rbp
LCFI1431:
	movq	%rsp, %rbp
LCFI1432:
	subq	$16, %rsp
LCFI1433:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L760
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L760
	addq	-16(%rbp), %rdx
L760:
	cmpq	$255, %rdx
	jle	L761
	movl	$14561, %edi
	call	__m3_fault
L761:
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L762
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L762
	addq	-16(%rbp), %rdx
L762:
	movl	%edx, %eax
	leave
LCFI1434:
	ret
LFE401:
.no_dead_strip _Mod__uMod_var_u8_i16
_Mod__uMod_var_u8_i16:
LFB402:
	pushq	%rbp
LCFI1435:
	movq	%rsp, %rbp
LCFI1436:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1437:
	ret
LFE402:
.no_dead_strip _Mod__Mod_var_u8_i16
_Mod__Mod_var_u8_i16:
LFB403:
	pushq	%rbp
LCFI1438:
	movq	%rsp, %rbp
LCFI1439:
	pushq	%rbx
LCFI1440:
	subq	$8, %rsp
LCFI1441:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L765
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L765
	addq	%rcx, %rdx
L765:
	cmpq	$255, %rdx
	jle	L766
	movl	$14625, %edi
	call	__m3_fault
L766:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L767
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L767
	addq	%rcx, %rdx
L767:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1442:
	ret
LFE403:
.no_dead_strip _Mod__uMod_param_u8_i16
_Mod__uMod_param_u8_i16:
LFB404:
	pushq	%rbp
LCFI1443:
	movq	%rsp, %rbp
LCFI1444:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %eax
	movswq	-4(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1445:
	ret
LFE404:
.no_dead_strip _Mod__Mod_param_u8_i16
_Mod__Mod_param_u8_i16:
LFB405:
	pushq	%rbp
LCFI1446:
	movq	%rsp, %rbp
LCFI1447:
	pushq	%rbx
LCFI1448:
	subq	$24, %rsp
LCFI1449:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movzbl	-17(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L770
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L770
	addq	%rcx, %rdx
L770:
	cmpq	$255, %rdx
	jle	L771
	movl	$14689, %edi
	call	__m3_fault
L771:
	movzbl	-17(%rbp), %ebx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L772
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L772
	addq	%rcx, %rdx
L772:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1450:
	ret
LFE405:
.no_dead_strip _Mod__uMod_var_u8_u64
_Mod__uMod_var_u8_u64:
LFB406:
	pushq	%rbp
LCFI1451:
	movq	%rsp, %rbp
LCFI1452:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1453:
	ret
LFE406:
.no_dead_strip _Mod__Mod_var_u8_u64
_Mod__Mod_var_u8_u64:
LFB407:
	pushq	%rbp
LCFI1454:
	movq	%rsp, %rbp
LCFI1455:
	pushq	%rbx
LCFI1456:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L775
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L775
	addq	%rcx, %rdx
L775:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1457:
	ret
LFE407:
.no_dead_strip _Mod__uMod_param_u8_u64
_Mod__uMod_param_u8_u64:
LFB408:
	pushq	%rbp
LCFI1458:
	movq	%rsp, %rbp
LCFI1459:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1460:
	ret
LFE408:
.no_dead_strip _Mod__Mod_param_u8_u64
_Mod__Mod_param_u8_u64:
LFB409:
	pushq	%rbp
LCFI1461:
	movq	%rsp, %rbp
LCFI1462:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L778
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L778
	addq	-16(%rbp), %rdx
L778:
	movq	%rdx, %rax
	leave
LCFI1463:
	ret
LFE409:
.no_dead_strip _Mod__uMod_var_u8_i32
_Mod__uMod_var_u8_i32:
LFB410:
	pushq	%rbp
LCFI1464:
	movq	%rsp, %rbp
LCFI1465:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1466:
	ret
LFE410:
.no_dead_strip _Mod__Mod_var_u8_i32
_Mod__Mod_var_u8_i32:
LFB411:
	pushq	%rbp
LCFI1467:
	movq	%rsp, %rbp
LCFI1468:
	pushq	%rbx
LCFI1469:
	subq	$8, %rsp
LCFI1470:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L781
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L781
	addq	%rcx, %rdx
L781:
	cmpq	$255, %rdx
	jle	L782
	movl	$14881, %edi
	call	__m3_fault
L782:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L783
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L783
	addq	%rcx, %rdx
L783:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1471:
	ret
LFE411:
.no_dead_strip _Mod__uMod_param_u8_i32
_Mod__uMod_param_u8_i32:
LFB412:
	pushq	%rbp
LCFI1472:
	movq	%rsp, %rbp
LCFI1473:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %eax
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1474:
	ret
LFE412:
.no_dead_strip _Mod__Mod_param_u8_i32
_Mod__Mod_param_u8_i32:
LFB413:
	pushq	%rbp
LCFI1475:
	movq	%rsp, %rbp
LCFI1476:
	pushq	%rbx
LCFI1477:
	subq	$24, %rsp
LCFI1478:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movzbl	-17(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L786
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L786
	addq	%rcx, %rdx
L786:
	cmpq	$255, %rdx
	jle	L787
	movl	$14945, %edi
	call	__m3_fault
L787:
	movzbl	-17(%rbp), %ebx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L788
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L788
	addq	%rcx, %rdx
L788:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1479:
	ret
LFE413:
.no_dead_strip _Mod__uMod_var_u8_C
_Mod__uMod_var_u8_C:
LFB414:
	pushq	%rbp
LCFI1480:
	movq	%rsp, %rbp
LCFI1481:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1482:
	ret
LFE414:
.no_dead_strip _Mod__Mod_var_u8_C
_Mod__Mod_var_u8_C:
LFB415:
	pushq	%rbp
LCFI1483:
	movq	%rsp, %rbp
LCFI1484:
	pushq	%rbx
LCFI1485:
	subq	$8, %rsp
LCFI1486:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L791
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L791
	addq	%rcx, %rdx
L791:
	cmpq	$255, %rdx
	jle	L792
	movl	$15009, %edi
	call	__m3_fault
L792:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L793
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L793
	addq	%rcx, %rdx
L793:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1487:
	ret
LFE415:
.no_dead_strip _Mod__uMod_param_u8_C
_Mod__uMod_param_u8_C:
LFB416:
	pushq	%rbp
LCFI1488:
	movq	%rsp, %rbp
LCFI1489:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1490:
	ret
LFE416:
.no_dead_strip _Mod__Mod_param_u8_C
_Mod__Mod_param_u8_C:
LFB417:
	pushq	%rbp
LCFI1491:
	movq	%rsp, %rbp
LCFI1492:
	pushq	%rbx
LCFI1493:
	subq	$24, %rsp
LCFI1494:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L796
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L796
	addq	%rcx, %rdx
L796:
	cmpq	$255, %rdx
	jle	L797
	movl	$15073, %edi
	call	__m3_fault
L797:
	movzbl	-17(%rbp), %ebx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L798
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L798
	addq	%rcx, %rdx
L798:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1495:
	ret
LFE417:
.no_dead_strip _Mod__uMod_var_u8_u8
_Mod__uMod_var_u8_u8:
LFB418:
	pushq	%rbp
LCFI1496:
	movq	%rsp, %rbp
LCFI1497:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1498:
	ret
LFE418:
.no_dead_strip _Mod__Mod_var_u8_u8
_Mod__Mod_var_u8_u8:
LFB419:
	pushq	%rbp
LCFI1499:
	movq	%rsp, %rbp
LCFI1500:
	pushq	%rbx
LCFI1501:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L801
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L801
	addq	%rcx, %rdx
L801:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1502:
	ret
LFE419:
.no_dead_strip _Mod__uMod_param_u8_u8
_Mod__uMod_param_u8_u8:
LFB420:
	pushq	%rbp
LCFI1503:
	movq	%rsp, %rbp
LCFI1504:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %eax
	movzbl	-2(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1505:
	ret
LFE420:
.no_dead_strip _Mod__Mod_param_u8_u8
_Mod__Mod_param_u8_u8:
LFB421:
	pushq	%rbp
LCFI1506:
	movq	%rsp, %rbp
LCFI1507:
	pushq	%rbx
LCFI1508:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %ebx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L804
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L804
	addq	%rcx, %rdx
L804:
	movl	%edx, %eax
	popq	%rbx
	leave
LCFI1509:
	ret
LFE421:
.no_dead_strip _Mod__uMod_var_u8_i64
_Mod__uMod_var_u8_i64:
LFB422:
	pushq	%rbp
LCFI1510:
	movq	%rsp, %rbp
LCFI1511:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1512:
	ret
LFE422:
.no_dead_strip _Mod__Mod_var_u8_i64
_Mod__Mod_var_u8_i64:
LFB423:
	pushq	%rbp
LCFI1513:
	movq	%rsp, %rbp
LCFI1514:
	pushq	%rbx
LCFI1515:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L807
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L807
	addq	%rcx, %rdx
L807:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1516:
	ret
LFE423:
.no_dead_strip _Mod__uMod_param_u8_i64
_Mod__uMod_param_u8_i64:
LFB424:
	pushq	%rbp
LCFI1517:
	movq	%rsp, %rbp
LCFI1518:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1519:
	ret
LFE424:
.no_dead_strip _Mod__Mod_param_u8_i64
_Mod__Mod_param_u8_i64:
LFB425:
	pushq	%rbp
LCFI1520:
	movq	%rsp, %rbp
LCFI1521:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L810
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L810
	addq	-16(%rbp), %rdx
L810:
	movq	%rdx, %rax
	leave
LCFI1522:
	ret
LFE425:
.no_dead_strip _Mod__uMod_var_u8_L
_Mod__uMod_var_u8_L:
LFB426:
	pushq	%rbp
LCFI1523:
	movq	%rsp, %rbp
LCFI1524:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1525:
	ret
LFE426:
.no_dead_strip _Mod__Mod_var_u8_L
_Mod__Mod_var_u8_L:
LFB427:
	pushq	%rbp
LCFI1526:
	movq	%rsp, %rbp
LCFI1527:
	pushq	%rbx
LCFI1528:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L813
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L813
	addq	%rcx, %rdx
L813:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1529:
	ret
LFE427:
.no_dead_strip _Mod__uMod_param_u8_L
_Mod__uMod_param_u8_L:
LFB428:
	pushq	%rbp
LCFI1530:
	movq	%rsp, %rbp
LCFI1531:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1532:
	ret
LFE428:
.no_dead_strip _Mod__Mod_param_u8_L
_Mod__Mod_param_u8_L:
LFB429:
	pushq	%rbp
LCFI1533:
	movq	%rsp, %rbp
LCFI1534:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %ecx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L816
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L816
	addq	-16(%rbp), %rdx
L816:
	movq	%rdx, %rax
	leave
LCFI1535:
	ret
LFE429:
.no_dead_strip _Mod__uMod_var_u8_i8
_Mod__uMod_var_u8_i8:
LFB430:
	pushq	%rbp
LCFI1536:
	movq	%rsp, %rbp
LCFI1537:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %eax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1538:
	ret
LFE430:
.no_dead_strip _Mod__Mod_var_u8_i8
_Mod__Mod_var_u8_i8:
LFB431:
	pushq	%rbp
LCFI1539:
	movq	%rsp, %rbp
LCFI1540:
	pushq	%rbx
LCFI1541:
	subq	$8, %rsp
LCFI1542:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L819
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L819
	addq	%rcx, %rdx
L819:
	cmpq	$255, %rdx
	jle	L820
	movl	$15521, %edi
	call	__m3_fault
L820:
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ebx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L821
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L821
	addq	%rcx, %rdx
L821:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1543:
	ret
LFE431:
.no_dead_strip _Mod__uMod_param_u8_i8
_Mod__uMod_param_u8_i8:
LFB432:
	pushq	%rbp
LCFI1544:
	movq	%rsp, %rbp
LCFI1545:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %eax
	movsbq	-2(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1546:
	ret
LFE432:
.no_dead_strip _Mod__Mod_param_u8_i8
_Mod__Mod_param_u8_i8:
LFB433:
	pushq	%rbp
LCFI1547:
	movq	%rsp, %rbp
LCFI1548:
	pushq	%rbx
LCFI1549:
	subq	$24, %rsp
LCFI1550:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movzbl	-17(%rbp), %ebx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L824
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L824
	addq	%rcx, %rdx
L824:
	cmpq	$255, %rdx
	jle	L825
	movl	$15585, %edi
	call	__m3_fault
L825:
	movzbl	-17(%rbp), %ebx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L826
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L826
	addq	%rcx, %rdx
L826:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1551:
	ret
LFE433:
.no_dead_strip _Mod__uMod_var_i64_u16
_Mod__uMod_var_i64_u16:
LFB434:
	pushq	%rbp
LCFI1552:
	movq	%rsp, %rbp
LCFI1553:
	movq	176+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1554:
	ret
LFE434:
.no_dead_strip _Mod__Mod_var_i64_u16
_Mod__Mod_var_i64_u16:
LFB435:
	pushq	%rbp
LCFI1555:
	movq	%rsp, %rbp
LCFI1556:
	pushq	%rbx
LCFI1557:
	movq	176+_MM_Mod(%rip), %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L829
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L829
	addq	%rcx, %rdx
L829:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1558:
	ret
LFE435:
.no_dead_strip _Mod__uMod_param_i64_u16
_Mod__uMod_param_i64_u16:
LFB436:
	pushq	%rbp
LCFI1559:
	movq	%rsp, %rbp
LCFI1560:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1561:
	ret
LFE436:
.no_dead_strip _Mod__Mod_param_i64_u16
_Mod__Mod_param_i64_u16:
LFB437:
	pushq	%rbp
LCFI1562:
	movq	%rsp, %rbp
LCFI1563:
	pushq	%rbx
LCFI1564:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L832
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L832
	addq	%rcx, %rdx
L832:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1565:
	ret
LFE437:
.no_dead_strip _Mod__uMod_var_i64_u32
_Mod__uMod_var_i64_u32:
LFB438:
	pushq	%rbp
LCFI1566:
	movq	%rsp, %rbp
LCFI1567:
	movq	176+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1568:
	ret
LFE438:
.no_dead_strip _Mod__Mod_var_i64_u32
_Mod__Mod_var_i64_u32:
LFB439:
	pushq	%rbp
LCFI1569:
	movq	%rsp, %rbp
LCFI1570:
	pushq	%rbx
LCFI1571:
	movq	176+_MM_Mod(%rip), %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L835
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L835
	addq	%rcx, %rdx
L835:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1572:
	ret
LFE439:
.no_dead_strip _Mod__uMod_param_i64_u32
_Mod__uMod_param_i64_u32:
LFB440:
	pushq	%rbp
LCFI1573:
	movq	%rsp, %rbp
LCFI1574:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1575:
	ret
LFE440:
.no_dead_strip _Mod__Mod_param_i64_u32
_Mod__Mod_param_i64_u32:
LFB441:
	pushq	%rbp
LCFI1576:
	movq	%rsp, %rbp
LCFI1577:
	pushq	%rbx
LCFI1578:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L838
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L838
	addq	%rcx, %rdx
L838:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1579:
	ret
LFE441:
.no_dead_strip _Mod__uMod_var_i64_LC
_Mod__uMod_var_i64_LC:
LFB442:
	pushq	%rbp
LCFI1580:
	movq	%rsp, %rbp
LCFI1581:
	movq	176+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1582:
	ret
LFE442:
.no_dead_strip _Mod__Mod_var_i64_LC
_Mod__Mod_var_i64_LC:
LFB443:
	pushq	%rbp
LCFI1583:
	movq	%rsp, %rbp
LCFI1584:
	pushq	%rbx
LCFI1585:
	movq	176+_MM_Mod(%rip), %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L841
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L841
	addq	%rcx, %rdx
L841:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1586:
	ret
LFE443:
.no_dead_strip _Mod__uMod_param_i64_LC
_Mod__uMod_param_i64_LC:
LFB444:
	pushq	%rbp
LCFI1587:
	movq	%rsp, %rbp
LCFI1588:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1589:
	ret
LFE444:
.no_dead_strip _Mod__Mod_param_i64_LC
_Mod__Mod_param_i64_LC:
LFB445:
	pushq	%rbp
LCFI1590:
	movq	%rsp, %rbp
LCFI1591:
	pushq	%rbx
LCFI1592:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L844
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L844
	addq	%rcx, %rdx
L844:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1593:
	ret
LFE445:
.no_dead_strip _Mod__uMod_var_i64_I
_Mod__uMod_var_i64_I:
LFB446:
	pushq	%rbp
LCFI1594:
	movq	%rsp, %rbp
LCFI1595:
	movq	176+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1596:
	ret
LFE446:
.no_dead_strip _Mod__Mod_var_i64_I
_Mod__Mod_var_i64_I:
LFB447:
	pushq	%rbp
LCFI1597:
	movq	%rsp, %rbp
LCFI1598:
	pushq	%rbx
LCFI1599:
	movq	176+_MM_Mod(%rip), %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L847
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L847
	addq	%rcx, %rdx
L847:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1600:
	ret
LFE447:
.no_dead_strip _Mod__uMod_param_i64_I
_Mod__uMod_param_i64_I:
LFB448:
	pushq	%rbp
LCFI1601:
	movq	%rsp, %rbp
LCFI1602:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1603:
	ret
LFE448:
.no_dead_strip _Mod__Mod_param_i64_I
_Mod__Mod_param_i64_I:
LFB449:
	pushq	%rbp
LCFI1604:
	movq	%rsp, %rbp
LCFI1605:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L850
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L850
	addq	-16(%rbp), %rdx
L850:
	movq	%rdx, %rax
	leave
LCFI1606:
	ret
LFE449:
.no_dead_strip _Mod__uMod_var_i64_i16
_Mod__uMod_var_i64_i16:
LFB450:
	pushq	%rbp
LCFI1607:
	movq	%rsp, %rbp
LCFI1608:
	movq	176+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1609:
	ret
LFE450:
.no_dead_strip _Mod__Mod_var_i64_i16
_Mod__Mod_var_i64_i16:
LFB451:
	pushq	%rbp
LCFI1610:
	movq	%rsp, %rbp
LCFI1611:
	pushq	%rbx
LCFI1612:
	movq	176+_MM_Mod(%rip), %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L853
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L853
	addq	%rcx, %rdx
L853:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1613:
	ret
LFE451:
.no_dead_strip _Mod__uMod_param_i64_i16
_Mod__uMod_param_i64_i16:
LFB452:
	pushq	%rbp
LCFI1614:
	movq	%rsp, %rbp
LCFI1615:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1616:
	ret
LFE452:
.no_dead_strip _Mod__Mod_param_i64_i16
_Mod__Mod_param_i64_i16:
LFB453:
	pushq	%rbp
LCFI1617:
	movq	%rsp, %rbp
LCFI1618:
	pushq	%rbx
LCFI1619:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L856
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L856
	addq	%rcx, %rdx
L856:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1620:
	ret
LFE453:
.no_dead_strip _Mod__uMod_var_i64_u64
_Mod__uMod_var_i64_u64:
LFB454:
	pushq	%rbp
LCFI1621:
	movq	%rsp, %rbp
LCFI1622:
	movq	176+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1623:
	ret
LFE454:
.no_dead_strip _Mod__Mod_var_i64_u64
_Mod__Mod_var_i64_u64:
LFB455:
	pushq	%rbp
LCFI1624:
	movq	%rsp, %rbp
LCFI1625:
	pushq	%rbx
LCFI1626:
	movq	176+_MM_Mod(%rip), %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L859
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L859
	addq	%rcx, %rdx
L859:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1627:
	ret
LFE455:
.no_dead_strip _Mod__uMod_param_i64_u64
_Mod__uMod_param_i64_u64:
LFB456:
	pushq	%rbp
LCFI1628:
	movq	%rsp, %rbp
LCFI1629:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1630:
	ret
LFE456:
.no_dead_strip _Mod__Mod_param_i64_u64
_Mod__Mod_param_i64_u64:
LFB457:
	pushq	%rbp
LCFI1631:
	movq	%rsp, %rbp
LCFI1632:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L862
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L862
	addq	-16(%rbp), %rdx
L862:
	movq	%rdx, %rax
	leave
LCFI1633:
	ret
LFE457:
.no_dead_strip _Mod__uMod_var_i64_i32
_Mod__uMod_var_i64_i32:
LFB458:
	pushq	%rbp
LCFI1634:
	movq	%rsp, %rbp
LCFI1635:
	movq	176+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1636:
	ret
LFE458:
.no_dead_strip _Mod__Mod_var_i64_i32
_Mod__Mod_var_i64_i32:
LFB459:
	pushq	%rbp
LCFI1637:
	movq	%rsp, %rbp
LCFI1638:
	pushq	%rbx
LCFI1639:
	movq	176+_MM_Mod(%rip), %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L865
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L865
	addq	%rcx, %rdx
L865:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1640:
	ret
LFE459:
.no_dead_strip _Mod__uMod_param_i64_i32
_Mod__uMod_param_i64_i32:
LFB460:
	pushq	%rbp
LCFI1641:
	movq	%rsp, %rbp
LCFI1642:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1643:
	ret
LFE460:
.no_dead_strip _Mod__Mod_param_i64_i32
_Mod__Mod_param_i64_i32:
LFB461:
	pushq	%rbp
LCFI1644:
	movq	%rsp, %rbp
LCFI1645:
	pushq	%rbx
LCFI1646:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L868
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L868
	addq	%rcx, %rdx
L868:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1647:
	ret
LFE461:
.no_dead_strip _Mod__uMod_var_i64_C
_Mod__uMod_var_i64_C:
LFB462:
	pushq	%rbp
LCFI1648:
	movq	%rsp, %rbp
LCFI1649:
	movq	176+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1650:
	ret
LFE462:
.no_dead_strip _Mod__Mod_var_i64_C
_Mod__Mod_var_i64_C:
LFB463:
	pushq	%rbp
LCFI1651:
	movq	%rsp, %rbp
LCFI1652:
	pushq	%rbx
LCFI1653:
	movq	176+_MM_Mod(%rip), %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L871
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L871
	addq	%rcx, %rdx
L871:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1654:
	ret
LFE463:
.no_dead_strip _Mod__uMod_param_i64_C
_Mod__uMod_param_i64_C:
LFB464:
	pushq	%rbp
LCFI1655:
	movq	%rsp, %rbp
LCFI1656:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1657:
	ret
LFE464:
.no_dead_strip _Mod__Mod_param_i64_C
_Mod__Mod_param_i64_C:
LFB465:
	pushq	%rbp
LCFI1658:
	movq	%rsp, %rbp
LCFI1659:
	pushq	%rbx
LCFI1660:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L874
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L874
	addq	%rcx, %rdx
L874:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1661:
	ret
LFE465:
.no_dead_strip _Mod__uMod_var_i64_u8
_Mod__uMod_var_i64_u8:
LFB466:
	pushq	%rbp
LCFI1662:
	movq	%rsp, %rbp
LCFI1663:
	movq	176+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1664:
	ret
LFE466:
.no_dead_strip _Mod__Mod_var_i64_u8
_Mod__Mod_var_i64_u8:
LFB467:
	pushq	%rbp
LCFI1665:
	movq	%rsp, %rbp
LCFI1666:
	pushq	%rbx
LCFI1667:
	movq	176+_MM_Mod(%rip), %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L877
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L877
	addq	%rcx, %rdx
L877:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1668:
	ret
LFE467:
.no_dead_strip _Mod__uMod_param_i64_u8
_Mod__uMod_param_i64_u8:
LFB468:
	pushq	%rbp
LCFI1669:
	movq	%rsp, %rbp
LCFI1670:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1671:
	ret
LFE468:
.no_dead_strip _Mod__Mod_param_i64_u8
_Mod__Mod_param_i64_u8:
LFB469:
	pushq	%rbp
LCFI1672:
	movq	%rsp, %rbp
LCFI1673:
	pushq	%rbx
LCFI1674:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L880
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L880
	addq	%rcx, %rdx
L880:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1675:
	ret
LFE469:
.no_dead_strip _Mod__uMod_var_i64_i64
_Mod__uMod_var_i64_i64:
LFB470:
	pushq	%rbp
LCFI1676:
	movq	%rsp, %rbp
LCFI1677:
	movq	176+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1678:
	ret
LFE470:
.no_dead_strip _Mod__Mod_var_i64_i64
_Mod__Mod_var_i64_i64:
LFB471:
	pushq	%rbp
LCFI1679:
	movq	%rsp, %rbp
LCFI1680:
	pushq	%rbx
LCFI1681:
	movq	176+_MM_Mod(%rip), %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L883
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L883
	addq	%rcx, %rdx
L883:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1682:
	ret
LFE471:
.no_dead_strip _Mod__uMod_param_i64_i64
_Mod__uMod_param_i64_i64:
LFB472:
	pushq	%rbp
LCFI1683:
	movq	%rsp, %rbp
LCFI1684:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1685:
	ret
LFE472:
.no_dead_strip _Mod__Mod_param_i64_i64
_Mod__Mod_param_i64_i64:
LFB473:
	pushq	%rbp
LCFI1686:
	movq	%rsp, %rbp
LCFI1687:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L886
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L886
	addq	-16(%rbp), %rdx
L886:
	movq	%rdx, %rax
	leave
LCFI1688:
	ret
LFE473:
.no_dead_strip _Mod__uMod_var_i64_L
_Mod__uMod_var_i64_L:
LFB474:
	pushq	%rbp
LCFI1689:
	movq	%rsp, %rbp
LCFI1690:
	movq	176+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1691:
	ret
LFE474:
.no_dead_strip _Mod__Mod_var_i64_L
_Mod__Mod_var_i64_L:
LFB475:
	pushq	%rbp
LCFI1692:
	movq	%rsp, %rbp
LCFI1693:
	pushq	%rbx
LCFI1694:
	movq	176+_MM_Mod(%rip), %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L889
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L889
	addq	%rcx, %rdx
L889:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1695:
	ret
LFE475:
.no_dead_strip _Mod__uMod_param_i64_L
_Mod__uMod_param_i64_L:
LFB476:
	pushq	%rbp
LCFI1696:
	movq	%rsp, %rbp
LCFI1697:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1698:
	ret
LFE476:
.no_dead_strip _Mod__Mod_param_i64_L
_Mod__Mod_param_i64_L:
LFB477:
	pushq	%rbp
LCFI1699:
	movq	%rsp, %rbp
LCFI1700:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L892
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L892
	addq	-16(%rbp), %rdx
L892:
	movq	%rdx, %rax
	leave
LCFI1701:
	ret
LFE477:
.no_dead_strip _Mod__uMod_var_i64_i8
_Mod__uMod_var_i64_i8:
LFB478:
	pushq	%rbp
LCFI1702:
	movq	%rsp, %rbp
LCFI1703:
	movq	176+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1704:
	ret
LFE478:
.no_dead_strip _Mod__Mod_var_i64_i8
_Mod__Mod_var_i64_i8:
LFB479:
	pushq	%rbp
LCFI1705:
	movq	%rsp, %rbp
LCFI1706:
	pushq	%rbx
LCFI1707:
	movq	176+_MM_Mod(%rip), %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L895
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L895
	addq	%rcx, %rdx
L895:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1708:
	ret
LFE479:
.no_dead_strip _Mod__uMod_param_i64_i8
_Mod__uMod_param_i64_i8:
LFB480:
	pushq	%rbp
LCFI1709:
	movq	%rsp, %rbp
LCFI1710:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1711:
	ret
LFE480:
.no_dead_strip _Mod__Mod_param_i64_i8
_Mod__Mod_param_i64_i8:
LFB481:
	pushq	%rbp
LCFI1712:
	movq	%rsp, %rbp
LCFI1713:
	pushq	%rbx
LCFI1714:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L898
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L898
	addq	%rcx, %rdx
L898:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1715:
	ret
LFE481:
.no_dead_strip _Mod__Mod_var_f32_f32
_Mod__Mod_var_f32_f32:
LFB482:
	pushq	%rbp
LCFI1716:
	movq	%rsp, %rbp
LCFI1717:
	leaq	_MM_Mod(%rip), %rax
	addq	$184, %rax
	movl	(%rax), %eax
	movl	%eax, -24(%rbp)
	leaq	_MM_Mod(%rip), %rax
	addq	$184, %rax
	movl	(%rax), %eax
	movl	%eax, -20(%rbp)
	movss	-24(%rbp), %xmm0
	movss	-20(%rbp), %xmm1
	divss	%xmm1, %xmm0
	movss	%xmm0, -4(%rbp)
	movss	-4(%rbp), %xmm0
	cvttss2siq	%xmm0, %rax
	movq	%rax, -16(%rbp)
	movss	-24(%rbp), %xmm1
	cvtsi2ssq	-16(%rbp), %xmm0
	movss	-4(%rbp), %xmm2
	ucomiss	%xmm0, %xmm2
	setae	%al
	testb	%al, %al
	je	L900
	cvtsi2ssq	-16(%rbp), %xmm0
	jmp	L901
L900:
	movq	-16(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
L901:
	movss	-20(%rbp), %xmm2
	mulss	%xmm2, %xmm0
	movaps	%xmm1, %xmm2
	subss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
LCFI1718:
	ret
LFE482:
.no_dead_strip _Mod__Mod_param_f32_f32
_Mod__Mod_param_f32_f32:
LFB483:
	pushq	%rbp
LCFI1719:
	movq	%rsp, %rbp
LCFI1720:
	movss	%xmm0, -36(%rbp)
	movss	%xmm1, -40(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, -24(%rbp)
	movl	-40(%rbp), %eax
	movl	%eax, -20(%rbp)
	movss	-24(%rbp), %xmm0
	movss	-20(%rbp), %xmm1
	divss	%xmm1, %xmm0
	movss	%xmm0, -4(%rbp)
	movss	-4(%rbp), %xmm0
	cvttss2siq	%xmm0, %rax
	movq	%rax, -16(%rbp)
	movss	-24(%rbp), %xmm1
	cvtsi2ssq	-16(%rbp), %xmm0
	movss	-4(%rbp), %xmm2
	ucomiss	%xmm0, %xmm2
	setae	%al
	testb	%al, %al
	je	L903
	cvtsi2ssq	-16(%rbp), %xmm0
	jmp	L904
L903:
	movq	-16(%rbp), %rax
	subq	$1, %rax
	cvtsi2ssq	%rax, %xmm0
L904:
	movss	-20(%rbp), %xmm2
	mulss	%xmm2, %xmm0
	movaps	%xmm1, %xmm2
	subss	%xmm0, %xmm2
	movaps	%xmm2, %xmm0
	leave
LCFI1721:
	ret
LFE483:
.no_dead_strip _Mod__uMod_var_L_u16
_Mod__uMod_var_L_u16:
LFB484:
	pushq	%rbp
LCFI1722:
	movq	%rsp, %rbp
LCFI1723:
	movq	192+_MM_Mod(%rip), %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1724:
	ret
LFE484:
.no_dead_strip _Mod__Mod_var_L_u16
_Mod__Mod_var_L_u16:
LFB485:
	pushq	%rbp
LCFI1725:
	movq	%rsp, %rbp
LCFI1726:
	pushq	%rbx
LCFI1727:
	movq	192+_MM_Mod(%rip), %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L907
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L907
	addq	%rcx, %rdx
L907:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1728:
	ret
LFE485:
.no_dead_strip _Mod__uMod_param_L_u16
_Mod__uMod_param_L_u16:
LFB486:
	pushq	%rbp
LCFI1729:
	movq	%rsp, %rbp
LCFI1730:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movzwl	-10(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1731:
	ret
LFE486:
.no_dead_strip _Mod__Mod_param_L_u16
_Mod__Mod_param_L_u16:
LFB487:
	pushq	%rbp
LCFI1732:
	movq	%rsp, %rbp
LCFI1733:
	pushq	%rbx
LCFI1734:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movzwl	-26(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L910
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L910
	addq	%rcx, %rdx
L910:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1735:
	ret
LFE487:
.no_dead_strip _Mod__uMod_var_L_u32
_Mod__uMod_var_L_u32:
LFB488:
	pushq	%rbp
LCFI1736:
	movq	%rsp, %rbp
LCFI1737:
	movq	192+_MM_Mod(%rip), %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1738:
	ret
LFE488:
.no_dead_strip _Mod__Mod_var_L_u32
_Mod__Mod_var_L_u32:
LFB489:
	pushq	%rbp
LCFI1739:
	movq	%rsp, %rbp
LCFI1740:
	pushq	%rbx
LCFI1741:
	movq	192+_MM_Mod(%rip), %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L913
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L913
	addq	%rcx, %rdx
L913:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1742:
	ret
LFE489:
.no_dead_strip _Mod__uMod_param_L_u32
_Mod__uMod_param_L_u32:
LFB490:
	pushq	%rbp
LCFI1743:
	movq	%rsp, %rbp
LCFI1744:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	mov	-12(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1745:
	ret
LFE490:
.no_dead_strip _Mod__Mod_param_L_u32
_Mod__Mod_param_L_u32:
LFB491:
	pushq	%rbp
LCFI1746:
	movq	%rsp, %rbp
LCFI1747:
	pushq	%rbx
LCFI1748:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	mov	-28(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L916
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L916
	addq	%rcx, %rdx
L916:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1749:
	ret
LFE491:
.no_dead_strip _Mod__uMod_var_L_LC
_Mod__uMod_var_L_LC:
LFB492:
	pushq	%rbp
LCFI1750:
	movq	%rsp, %rbp
LCFI1751:
	movq	192+_MM_Mod(%rip), %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1752:
	ret
LFE492:
.no_dead_strip _Mod__Mod_var_L_LC
_Mod__Mod_var_L_LC:
LFB493:
	pushq	%rbp
LCFI1753:
	movq	%rsp, %rbp
LCFI1754:
	pushq	%rbx
LCFI1755:
	movq	192+_MM_Mod(%rip), %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L919
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L919
	addq	%rcx, %rdx
L919:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1756:
	ret
LFE493:
.no_dead_strip _Mod__uMod_param_L_LC
_Mod__uMod_param_L_LC:
LFB494:
	pushq	%rbp
LCFI1757:
	movq	%rsp, %rbp
LCFI1758:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1759:
	ret
LFE494:
.no_dead_strip _Mod__Mod_param_L_LC
_Mod__Mod_param_L_LC:
LFB495:
	pushq	%rbp
LCFI1760:
	movq	%rsp, %rbp
LCFI1761:
	pushq	%rbx
LCFI1762:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L922
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L922
	addq	%rcx, %rdx
L922:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1763:
	ret
LFE495:
.no_dead_strip _Mod__uMod_var_L_I
_Mod__uMod_var_L_I:
LFB496:
	pushq	%rbp
LCFI1764:
	movq	%rsp, %rbp
LCFI1765:
	movq	192+_MM_Mod(%rip), %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1766:
	ret
LFE496:
.no_dead_strip _Mod__Mod_var_L_I
_Mod__Mod_var_L_I:
LFB497:
	pushq	%rbp
LCFI1767:
	movq	%rsp, %rbp
LCFI1768:
	pushq	%rbx
LCFI1769:
	movq	192+_MM_Mod(%rip), %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L925
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L925
	addq	%rcx, %rdx
L925:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1770:
	ret
LFE497:
.no_dead_strip _Mod__uMod_param_L_I
_Mod__uMod_param_L_I:
LFB498:
	pushq	%rbp
LCFI1771:
	movq	%rsp, %rbp
LCFI1772:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1773:
	ret
LFE498:
.no_dead_strip _Mod__Mod_param_L_I
_Mod__Mod_param_L_I:
LFB499:
	pushq	%rbp
LCFI1774:
	movq	%rsp, %rbp
LCFI1775:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L928
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L928
	addq	-16(%rbp), %rdx
L928:
	movq	%rdx, %rax
	leave
LCFI1776:
	ret
LFE499:
.no_dead_strip _Mod__uMod_var_L_i16
_Mod__uMod_var_L_i16:
LFB500:
	pushq	%rbp
LCFI1777:
	movq	%rsp, %rbp
LCFI1778:
	movq	192+_MM_Mod(%rip), %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1779:
	ret
LFE500:
.no_dead_strip _Mod__Mod_var_L_i16
_Mod__Mod_var_L_i16:
LFB501:
	pushq	%rbp
LCFI1780:
	movq	%rsp, %rbp
LCFI1781:
	pushq	%rbx
LCFI1782:
	movq	192+_MM_Mod(%rip), %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L931
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L931
	addq	%rcx, %rdx
L931:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1783:
	ret
LFE501:
.no_dead_strip _Mod__uMod_param_L_i16
_Mod__uMod_param_L_i16:
LFB502:
	pushq	%rbp
LCFI1784:
	movq	%rsp, %rbp
LCFI1785:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rax
	movswq	-10(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1786:
	ret
LFE502:
.no_dead_strip _Mod__Mod_param_L_i16
_Mod__Mod_param_L_i16:
LFB503:
	pushq	%rbp
LCFI1787:
	movq	%rsp, %rbp
LCFI1788:
	pushq	%rbx
LCFI1789:
	movq	%rdi, -24(%rbp)
	movw	%si, -26(%rbp)
	movswq	-26(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L934
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L934
	addq	%rcx, %rdx
L934:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1790:
	ret
LFE503:
.no_dead_strip _Mod__uMod_var_L_u64
_Mod__uMod_var_L_u64:
LFB504:
	pushq	%rbp
LCFI1791:
	movq	%rsp, %rbp
LCFI1792:
	movq	192+_MM_Mod(%rip), %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1793:
	ret
LFE504:
.no_dead_strip _Mod__Mod_var_L_u64
_Mod__Mod_var_L_u64:
LFB505:
	pushq	%rbp
LCFI1794:
	movq	%rsp, %rbp
LCFI1795:
	pushq	%rbx
LCFI1796:
	movq	192+_MM_Mod(%rip), %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L937
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L937
	addq	%rcx, %rdx
L937:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1797:
	ret
LFE505:
.no_dead_strip _Mod__uMod_param_L_u64
_Mod__uMod_param_L_u64:
LFB506:
	pushq	%rbp
LCFI1798:
	movq	%rsp, %rbp
LCFI1799:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1800:
	ret
LFE506:
.no_dead_strip _Mod__Mod_param_L_u64
_Mod__Mod_param_L_u64:
LFB507:
	pushq	%rbp
LCFI1801:
	movq	%rsp, %rbp
LCFI1802:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L940
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L940
	addq	-16(%rbp), %rdx
L940:
	movq	%rdx, %rax
	leave
LCFI1803:
	ret
LFE507:
.no_dead_strip _Mod__uMod_var_L_i32
_Mod__uMod_var_L_i32:
LFB508:
	pushq	%rbp
LCFI1804:
	movq	%rsp, %rbp
LCFI1805:
	movq	192+_MM_Mod(%rip), %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1806:
	ret
LFE508:
.no_dead_strip _Mod__Mod_var_L_i32
_Mod__Mod_var_L_i32:
LFB509:
	pushq	%rbp
LCFI1807:
	movq	%rsp, %rbp
LCFI1808:
	pushq	%rbx
LCFI1809:
	movq	192+_MM_Mod(%rip), %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L943
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L943
	addq	%rcx, %rdx
L943:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1810:
	ret
LFE509:
.no_dead_strip _Mod__uMod_param_L_i32
_Mod__uMod_param_L_i32:
LFB510:
	pushq	%rbp
LCFI1811:
	movq	%rsp, %rbp
LCFI1812:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1813:
	ret
LFE510:
.no_dead_strip _Mod__Mod_param_L_i32
_Mod__Mod_param_L_i32:
LFB511:
	pushq	%rbp
LCFI1814:
	movq	%rsp, %rbp
LCFI1815:
	pushq	%rbx
LCFI1816:
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L946
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L946
	addq	%rcx, %rdx
L946:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1817:
	ret
LFE511:
.no_dead_strip _Mod__uMod_var_L_C
_Mod__uMod_var_L_C:
LFB512:
	pushq	%rbp
LCFI1818:
	movq	%rsp, %rbp
LCFI1819:
	movq	192+_MM_Mod(%rip), %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1820:
	ret
LFE512:
.no_dead_strip _Mod__Mod_var_L_C
_Mod__Mod_var_L_C:
LFB513:
	pushq	%rbp
LCFI1821:
	movq	%rsp, %rbp
LCFI1822:
	pushq	%rbx
LCFI1823:
	movq	192+_MM_Mod(%rip), %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L949
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L949
	addq	%rcx, %rdx
L949:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1824:
	ret
LFE513:
.no_dead_strip _Mod__uMod_param_L_C
_Mod__uMod_param_L_C:
LFB514:
	pushq	%rbp
LCFI1825:
	movq	%rsp, %rbp
LCFI1826:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1827:
	ret
LFE514:
.no_dead_strip _Mod__Mod_param_L_C
_Mod__Mod_param_L_C:
LFB515:
	pushq	%rbp
LCFI1828:
	movq	%rsp, %rbp
LCFI1829:
	pushq	%rbx
LCFI1830:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L952
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L952
	addq	%rcx, %rdx
L952:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1831:
	ret
LFE515:
.no_dead_strip _Mod__uMod_var_L_u8
_Mod__uMod_var_L_u8:
LFB516:
	pushq	%rbp
LCFI1832:
	movq	%rsp, %rbp
LCFI1833:
	movq	192+_MM_Mod(%rip), %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1834:
	ret
LFE516:
.no_dead_strip _Mod__Mod_var_L_u8
_Mod__Mod_var_L_u8:
LFB517:
	pushq	%rbp
LCFI1835:
	movq	%rsp, %rbp
LCFI1836:
	pushq	%rbx
LCFI1837:
	movq	192+_MM_Mod(%rip), %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L955
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L955
	addq	%rcx, %rdx
L955:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1838:
	ret
LFE517:
.no_dead_strip _Mod__uMod_param_L_u8
_Mod__uMod_param_L_u8:
LFB518:
	pushq	%rbp
LCFI1839:
	movq	%rsp, %rbp
LCFI1840:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movzbl	-9(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1841:
	ret
LFE518:
.no_dead_strip _Mod__Mod_param_L_u8
_Mod__Mod_param_L_u8:
LFB519:
	pushq	%rbp
LCFI1842:
	movq	%rsp, %rbp
LCFI1843:
	pushq	%rbx
LCFI1844:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L958
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L958
	addq	%rcx, %rdx
L958:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1845:
	ret
LFE519:
.no_dead_strip _Mod__uMod_var_L_i64
_Mod__uMod_var_L_i64:
LFB520:
	pushq	%rbp
LCFI1846:
	movq	%rsp, %rbp
LCFI1847:
	movq	192+_MM_Mod(%rip), %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1848:
	ret
LFE520:
.no_dead_strip _Mod__Mod_var_L_i64
_Mod__Mod_var_L_i64:
LFB521:
	pushq	%rbp
LCFI1849:
	movq	%rsp, %rbp
LCFI1850:
	pushq	%rbx
LCFI1851:
	movq	192+_MM_Mod(%rip), %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L961
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L961
	addq	%rcx, %rdx
L961:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1852:
	ret
LFE521:
.no_dead_strip _Mod__uMod_param_L_i64
_Mod__uMod_param_L_i64:
LFB522:
	pushq	%rbp
LCFI1853:
	movq	%rsp, %rbp
LCFI1854:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1855:
	ret
LFE522:
.no_dead_strip _Mod__Mod_param_L_i64
_Mod__Mod_param_L_i64:
LFB523:
	pushq	%rbp
LCFI1856:
	movq	%rsp, %rbp
LCFI1857:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L964
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L964
	addq	-16(%rbp), %rdx
L964:
	movq	%rdx, %rax
	leave
LCFI1858:
	ret
LFE523:
.no_dead_strip _Mod__uMod_var_L_L
_Mod__uMod_var_L_L:
LFB524:
	pushq	%rbp
LCFI1859:
	movq	%rsp, %rbp
LCFI1860:
	movq	192+_MM_Mod(%rip), %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1861:
	ret
LFE524:
.no_dead_strip _Mod__Mod_var_L_L
_Mod__Mod_var_L_L:
LFB525:
	pushq	%rbp
LCFI1862:
	movq	%rsp, %rbp
LCFI1863:
	pushq	%rbx
LCFI1864:
	movq	192+_MM_Mod(%rip), %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L967
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L967
	addq	%rcx, %rdx
L967:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1865:
	ret
LFE525:
.no_dead_strip _Mod__uMod_param_L_L
_Mod__uMod_param_L_L:
LFB526:
	pushq	%rbp
LCFI1866:
	movq	%rsp, %rbp
LCFI1867:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1868:
	ret
LFE526:
.no_dead_strip _Mod__Mod_param_L_L
_Mod__Mod_param_L_L:
LFB527:
	pushq	%rbp
LCFI1869:
	movq	%rsp, %rbp
LCFI1870:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L970
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L970
	addq	-16(%rbp), %rdx
L970:
	movq	%rdx, %rax
	leave
LCFI1871:
	ret
LFE527:
.no_dead_strip _Mod__uMod_var_L_i8
_Mod__uMod_var_L_i8:
LFB528:
	pushq	%rbp
LCFI1872:
	movq	%rsp, %rbp
LCFI1873:
	movq	192+_MM_Mod(%rip), %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1874:
	ret
LFE528:
.no_dead_strip _Mod__Mod_var_L_i8
_Mod__Mod_var_L_i8:
LFB529:
	pushq	%rbp
LCFI1875:
	movq	%rsp, %rbp
LCFI1876:
	pushq	%rbx
LCFI1877:
	movq	192+_MM_Mod(%rip), %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L973
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L973
	addq	%rcx, %rdx
L973:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1878:
	ret
LFE529:
.no_dead_strip _Mod__uMod_param_L_i8
_Mod__uMod_param_L_i8:
LFB530:
	pushq	%rbp
LCFI1879:
	movq	%rsp, %rbp
LCFI1880:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rax
	movsbq	-9(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1881:
	ret
LFE530:
.no_dead_strip _Mod__Mod_param_L_i8
_Mod__Mod_param_L_i8:
LFB531:
	pushq	%rbp
LCFI1882:
	movq	%rsp, %rbp
LCFI1883:
	pushq	%rbx
LCFI1884:
	movq	%rdi, -24(%rbp)
	movb	%sil, -25(%rbp)
	movsbq	-25(%rbp), %rcx
	movq	-24(%rbp), %rbx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L976
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L976
	addq	%rcx, %rdx
L976:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1885:
	ret
LFE531:
.no_dead_strip _Mod__uMod_var_i8_u16
_Mod__uMod_var_i8_u16:
LFB532:
	pushq	%rbp
LCFI1886:
	movq	%rsp, %rbp
LCFI1887:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movzwl	104+_MM_Mod(%rip), %edx
	movzwl	%dx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1888:
	ret
LFE532:
.no_dead_strip _Mod__Mod_var_i8_u16
_Mod__Mod_var_i8_u16:
LFB533:
	pushq	%rbp
LCFI1889:
	movq	%rsp, %rbp
LCFI1890:
	pushq	%rbx
LCFI1891:
	subq	$8, %rsp
LCFI1892:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L979
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L979
	addq	%rcx, %rdx
L979:
	cmpq	$127, %rdx
	jle	L980
	movl	$18785, %edi
	call	__m3_fault
L980:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzwl	104+_MM_Mod(%rip), %eax
	movzwl	%ax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L981
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L981
	addq	%rcx, %rdx
L981:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1893:
	ret
LFE533:
.no_dead_strip _Mod__uMod_param_i8_u16
_Mod__uMod_param_i8_u16:
LFB534:
	pushq	%rbp
LCFI1894:
	movq	%rsp, %rbp
LCFI1895:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rax
	movzwl	-4(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1896:
	ret
LFE534:
.no_dead_strip _Mod__Mod_param_i8_u16
_Mod__Mod_param_i8_u16:
LFB535:
	pushq	%rbp
LCFI1897:
	movq	%rsp, %rbp
LCFI1898:
	pushq	%rbx
LCFI1899:
	subq	$24, %rsp
LCFI1900:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L984
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L984
	addq	%rcx, %rdx
L984:
	cmpq	$127, %rdx
	jle	L985
	movl	$18849, %edi
	call	__m3_fault
L985:
	movsbq	-17(%rbp), %rbx
	movzwl	-20(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L986
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L986
	addq	%rcx, %rdx
L986:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1901:
	ret
LFE535:
.no_dead_strip _Mod__uMod_var_i8_u32
_Mod__uMod_var_i8_u32:
LFB536:
	pushq	%rbp
LCFI1902:
	movq	%rsp, %rbp
LCFI1903:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movl	108+_MM_Mod(%rip), %edx
	mov	%edx, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1904:
	ret
LFE536:
.no_dead_strip _Mod__Mod_var_i8_u32
_Mod__Mod_var_i8_u32:
LFB537:
	pushq	%rbp
LCFI1905:
	movq	%rsp, %rbp
LCFI1906:
	pushq	%rbx
LCFI1907:
	subq	$8, %rsp
LCFI1908:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L989
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L989
	addq	%rcx, %rdx
L989:
	cmpq	$127, %rdx
	jle	L990
	movl	$18913, %edi
	call	__m3_fault
L990:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movl	108+_MM_Mod(%rip), %eax
	mov	%eax, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L991
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L991
	addq	%rcx, %rdx
L991:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1909:
	ret
LFE537:
.no_dead_strip _Mod__uMod_param_i8_u32
_Mod__uMod_param_i8_u32:
LFB538:
	pushq	%rbp
LCFI1910:
	movq	%rsp, %rbp
LCFI1911:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rax
	mov	-8(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1912:
	ret
LFE538:
.no_dead_strip _Mod__Mod_param_i8_u32
_Mod__Mod_param_i8_u32:
LFB539:
	pushq	%rbp
LCFI1913:
	movq	%rsp, %rbp
LCFI1914:
	pushq	%rbx
LCFI1915:
	subq	$24, %rsp
LCFI1916:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L994
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L994
	addq	%rcx, %rdx
L994:
	cmpq	$127, %rdx
	jle	L995
	movl	$18977, %edi
	call	__m3_fault
L995:
	movsbq	-17(%rbp), %rbx
	mov	-24(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L996
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L996
	addq	%rcx, %rdx
L996:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1917:
	ret
LFE539:
.no_dead_strip _Mod__uMod_var_i8_LC
_Mod__uMod_var_i8_LC:
LFB540:
	pushq	%rbp
LCFI1918:
	movq	%rsp, %rbp
LCFI1919:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	112+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1920:
	ret
LFE540:
.no_dead_strip _Mod__Mod_var_i8_LC
_Mod__Mod_var_i8_LC:
LFB541:
	pushq	%rbp
LCFI1921:
	movq	%rsp, %rbp
LCFI1922:
	pushq	%rbx
LCFI1923:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	112+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L999
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L999
	addq	%rcx, %rdx
L999:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1924:
	ret
LFE541:
.no_dead_strip _Mod__uMod_param_i8_LC
_Mod__uMod_param_i8_LC:
LFB542:
	pushq	%rbp
LCFI1925:
	movq	%rsp, %rbp
LCFI1926:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI1927:
	ret
LFE542:
.no_dead_strip _Mod__Mod_param_i8_LC
_Mod__Mod_param_i8_LC:
LFB543:
	pushq	%rbp
LCFI1928:
	movq	%rsp, %rbp
LCFI1929:
	pushq	%rbx
LCFI1930:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1002
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1002
	addq	%rcx, %rdx
L1002:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1931:
	ret
LFE543:
.no_dead_strip _Mod__uMod_var_i8_I
_Mod__uMod_var_i8_I:
LFB544:
	pushq	%rbp
LCFI1932:
	movq	%rsp, %rbp
LCFI1933:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	120+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1934:
	ret
LFE544:
.no_dead_strip _Mod__Mod_var_i8_I
_Mod__Mod_var_i8_I:
LFB545:
	pushq	%rbp
LCFI1935:
	movq	%rsp, %rbp
LCFI1936:
	pushq	%rbx
LCFI1937:
	subq	$8, %rsp
LCFI1938:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1005
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1005
	addq	%rcx, %rdx
L1005:
	cmpq	$-128, %rdx
	jl	L1006
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1007
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1007
	addq	%rcx, %rdx
L1007:
	cmpq	$127, %rdx
	jle	L1008
L1006:
	movl	$19169, %edi
	call	__m3_fault
L1008:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	120+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1009
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1009
	addq	%rcx, %rdx
L1009:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1939:
	ret
LFE545:
.no_dead_strip _Mod__uMod_param_i8_I
_Mod__uMod_param_i8_I:
LFB546:
	pushq	%rbp
LCFI1940:
	movq	%rsp, %rbp
LCFI1941:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1942:
	ret
LFE546:
.no_dead_strip _Mod__Mod_param_i8_I
_Mod__Mod_param_i8_I:
LFB547:
	pushq	%rbp
LCFI1943:
	movq	%rsp, %rbp
LCFI1944:
	subq	$16, %rsp
LCFI1945:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1012
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1012
	addq	-16(%rbp), %rdx
L1012:
	cmpq	$-128, %rdx
	jl	L1013
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1014
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1014
	addq	-16(%rbp), %rdx
L1014:
	cmpq	$127, %rdx
	jle	L1015
L1013:
	movl	$19233, %edi
	call	__m3_fault
L1015:
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1016
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1016
	addq	-16(%rbp), %rdx
L1016:
	movl	%edx, %eax
	leave
LCFI1946:
	ret
LFE547:
.no_dead_strip _Mod__uMod_var_i8_i16
_Mod__uMod_var_i8_i16:
LFB548:
	pushq	%rbp
LCFI1947:
	movq	%rsp, %rbp
LCFI1948:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movzwl	128+_MM_Mod(%rip), %edx
	movswq	%dx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1949:
	ret
LFE548:
.no_dead_strip _Mod__Mod_var_i8_i16
_Mod__Mod_var_i8_i16:
LFB549:
	pushq	%rbp
LCFI1950:
	movq	%rsp, %rbp
LCFI1951:
	pushq	%rbx
LCFI1952:
	subq	$8, %rsp
LCFI1953:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1019
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1019
	addq	%rcx, %rdx
L1019:
	cmpq	$-128, %rdx
	jl	L1020
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1021
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1021
	addq	%rcx, %rdx
L1021:
	cmpq	$127, %rdx
	jle	L1022
L1020:
	movl	$19297, %edi
	call	__m3_fault
L1022:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzwl	128+_MM_Mod(%rip), %eax
	movswq	%ax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1023
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1023
	addq	%rcx, %rdx
L1023:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1954:
	ret
LFE549:
.no_dead_strip _Mod__uMod_param_i8_i16
_Mod__uMod_param_i8_i16:
LFB550:
	pushq	%rbp
LCFI1955:
	movq	%rsp, %rbp
LCFI1956:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rax
	movswq	-4(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1957:
	ret
LFE550:
.no_dead_strip _Mod__Mod_param_i8_i16
_Mod__Mod_param_i8_i16:
LFB551:
	pushq	%rbp
LCFI1958:
	movq	%rsp, %rbp
LCFI1959:
	pushq	%rbx
LCFI1960:
	subq	$24, %rsp
LCFI1961:
	movb	%dil, -17(%rbp)
	movw	%si, -20(%rbp)
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1026
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1026
	addq	%rcx, %rdx
L1026:
	cmpq	$-128, %rdx
	jl	L1027
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1028
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1028
	addq	%rcx, %rdx
L1028:
	cmpq	$127, %rdx
	jle	L1029
L1027:
	movl	$19361, %edi
	call	__m3_fault
L1029:
	movsbq	-17(%rbp), %rbx
	movswq	-20(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1030
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1030
	addq	%rcx, %rdx
L1030:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1962:
	ret
LFE551:
.no_dead_strip _Mod__uMod_var_i8_u64
_Mod__uMod_var_i8_u64:
LFB552:
	pushq	%rbp
LCFI1963:
	movq	%rsp, %rbp
LCFI1964:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	136+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1965:
	ret
LFE552:
.no_dead_strip _Mod__Mod_var_i8_u64
_Mod__Mod_var_i8_u64:
LFB553:
	pushq	%rbp
LCFI1966:
	movq	%rsp, %rbp
LCFI1967:
	pushq	%rbx
LCFI1968:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	136+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1033
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1033
	addq	%rcx, %rdx
L1033:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI1969:
	ret
LFE553:
.no_dead_strip _Mod__uMod_param_i8_u64
_Mod__uMod_param_i8_u64:
LFB554:
	pushq	%rbp
LCFI1970:
	movq	%rsp, %rbp
LCFI1971:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1972:
	ret
LFE554:
.no_dead_strip _Mod__Mod_param_i8_u64
_Mod__Mod_param_i8_u64:
LFB555:
	pushq	%rbp
LCFI1973:
	movq	%rsp, %rbp
LCFI1974:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1036
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1036
	addq	-16(%rbp), %rdx
L1036:
	movq	%rdx, %rax
	leave
LCFI1975:
	ret
LFE555:
.no_dead_strip _Mod__uMod_var_i8_i32
_Mod__uMod_var_i8_i32:
LFB556:
	pushq	%rbp
LCFI1976:
	movq	%rsp, %rbp
LCFI1977:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movl	152+_MM_Mod(%rip), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1978:
	ret
LFE556:
.no_dead_strip _Mod__Mod_var_i8_i32
_Mod__Mod_var_i8_i32:
LFB557:
	pushq	%rbp
LCFI1979:
	movq	%rsp, %rbp
LCFI1980:
	pushq	%rbx
LCFI1981:
	subq	$8, %rsp
LCFI1982:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1039
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1039
	addq	%rcx, %rdx
L1039:
	cmpq	$-128, %rdx
	jl	L1040
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1041
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1041
	addq	%rcx, %rdx
L1041:
	cmpq	$127, %rdx
	jle	L1042
L1040:
	movl	$19553, %edi
	call	__m3_fault
L1042:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movl	152+_MM_Mod(%rip), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1043
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1043
	addq	%rcx, %rdx
L1043:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1983:
	ret
LFE557:
.no_dead_strip _Mod__uMod_param_i8_i32
_Mod__uMod_param_i8_i32:
LFB558:
	pushq	%rbp
LCFI1984:
	movq	%rsp, %rbp
LCFI1985:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rax
	movl	-8(%rbp), %edx
	movslq	%edx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1986:
	ret
LFE558:
.no_dead_strip _Mod__Mod_param_i8_i32
_Mod__Mod_param_i8_i32:
LFB559:
	pushq	%rbp
LCFI1987:
	movq	%rsp, %rbp
LCFI1988:
	pushq	%rbx
LCFI1989:
	subq	$24, %rsp
LCFI1990:
	movb	%dil, -17(%rbp)
	movl	%esi, -24(%rbp)
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1046
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1046
	addq	%rcx, %rdx
L1046:
	cmpq	$-128, %rdx
	jl	L1047
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1048
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1048
	addq	%rcx, %rdx
L1048:
	cmpq	$127, %rdx
	jle	L1049
L1047:
	movl	$19617, %edi
	call	__m3_fault
L1049:
	movsbq	-17(%rbp), %rbx
	movl	-24(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1050
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1050
	addq	%rcx, %rdx
L1050:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI1991:
	ret
LFE559:
.no_dead_strip _Mod__uMod_var_i8_C
_Mod__uMod_var_i8_C:
LFB560:
	pushq	%rbp
LCFI1992:
	movq	%rsp, %rbp
LCFI1993:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	160+_MM_Mod(%rip), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI1994:
	ret
LFE560:
.no_dead_strip _Mod__Mod_var_i8_C
_Mod__Mod_var_i8_C:
LFB561:
	pushq	%rbp
LCFI1995:
	movq	%rsp, %rbp
LCFI1996:
	pushq	%rbx
LCFI1997:
	subq	$8, %rsp
LCFI1998:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1053
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1053
	addq	%rcx, %rdx
L1053:
	cmpq	$127, %rdx
	jle	L1054
	movl	$19681, %edi
	call	__m3_fault
L1054:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	160+_MM_Mod(%rip), %rax
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1055
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1055
	addq	%rcx, %rdx
L1055:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI1999:
	ret
LFE561:
.no_dead_strip _Mod__uMod_param_i8_C
_Mod__uMod_param_i8_C:
LFB562:
	pushq	%rbp
LCFI2000:
	movq	%rsp, %rbp
LCFI2001:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movl	$0, %edx
	divq	-16(%rbp)
	movq	%rdx, %rax
	leave
LCFI2002:
	ret
LFE562:
.no_dead_strip _Mod__Mod_param_i8_C
_Mod__Mod_param_i8_C:
LFB563:
	pushq	%rbp
LCFI2003:
	movq	%rsp, %rbp
LCFI2004:
	pushq	%rbx
LCFI2005:
	subq	$24, %rsp
LCFI2006:
	movb	%dil, -17(%rbp)
	movq	%rsi, -32(%rbp)
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1058
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1058
	addq	%rcx, %rdx
L1058:
	cmpq	$127, %rdx
	jle	L1059
	movl	$19745, %edi
	call	__m3_fault
L1059:
	movsbq	-17(%rbp), %rbx
	movq	-32(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1060
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1060
	addq	%rcx, %rdx
L1060:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2007:
	ret
LFE563:
.no_dead_strip _Mod__uMod_var_i8_u8
_Mod__uMod_var_i8_u8:
LFB564:
	pushq	%rbp
LCFI2008:
	movq	%rsp, %rbp
LCFI2009:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movzbl	168+_MM_Mod(%rip), %edx
	movzbl	%dl, %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2010:
	ret
LFE564:
.no_dead_strip _Mod__Mod_var_i8_u8
_Mod__Mod_var_i8_u8:
LFB565:
	pushq	%rbp
LCFI2011:
	movq	%rsp, %rbp
LCFI2012:
	pushq	%rbx
LCFI2013:
	subq	$8, %rsp
LCFI2014:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1063
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1063
	addq	%rcx, %rdx
L1063:
	cmpq	$127, %rdx
	jle	L1064
	movl	$19809, %edi
	call	__m3_fault
L1064:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzbl	168+_MM_Mod(%rip), %eax
	movzbl	%al, %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1065
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1065
	addq	%rcx, %rdx
L1065:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2015:
	ret
LFE565:
.no_dead_strip _Mod__uMod_param_i8_u8
_Mod__uMod_param_i8_u8:
LFB566:
	pushq	%rbp
LCFI2016:
	movq	%rsp, %rbp
LCFI2017:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rax
	movzbl	-2(%rbp), %ecx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2018:
	ret
LFE566:
.no_dead_strip _Mod__Mod_param_i8_u8
_Mod__Mod_param_i8_u8:
LFB567:
	pushq	%rbp
LCFI2019:
	movq	%rsp, %rbp
LCFI2020:
	pushq	%rbx
LCFI2021:
	subq	$24, %rsp
LCFI2022:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp), %rbx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1068
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1068
	addq	%rcx, %rdx
L1068:
	cmpq	$127, %rdx
	jle	L1069
	movl	$19873, %edi
	call	__m3_fault
L1069:
	movsbq	-17(%rbp), %rbx
	movzbl	-18(%rbp), %ecx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1070
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1070
	addq	%rcx, %rdx
L1070:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2023:
	ret
LFE567:
.no_dead_strip _Mod__uMod_var_i8_i64
_Mod__uMod_var_i8_i64:
LFB568:
	pushq	%rbp
LCFI2024:
	movq	%rsp, %rbp
LCFI2025:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	176+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2026:
	ret
LFE568:
.no_dead_strip _Mod__Mod_var_i8_i64
_Mod__Mod_var_i8_i64:
LFB569:
	pushq	%rbp
LCFI2027:
	movq	%rsp, %rbp
LCFI2028:
	pushq	%rbx
LCFI2029:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	176+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1073
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1073
	addq	%rcx, %rdx
L1073:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI2030:
	ret
LFE569:
.no_dead_strip _Mod__uMod_param_i8_i64
_Mod__uMod_param_i8_i64:
LFB570:
	pushq	%rbp
LCFI2031:
	movq	%rsp, %rbp
LCFI2032:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2033:
	ret
LFE570:
.no_dead_strip _Mod__Mod_param_i8_i64
_Mod__Mod_param_i8_i64:
LFB571:
	pushq	%rbp
LCFI2034:
	movq	%rsp, %rbp
LCFI2035:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1076
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1076
	addq	-16(%rbp), %rdx
L1076:
	movq	%rdx, %rax
	leave
LCFI2036:
	ret
LFE571:
.no_dead_strip _Mod__uMod_var_i8_L
_Mod__uMod_var_i8_L:
LFB572:
	pushq	%rbp
LCFI2037:
	movq	%rsp, %rbp
LCFI2038:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movq	192+_MM_Mod(%rip), %rdx
	movq	%rdx, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2039:
	ret
LFE572:
.no_dead_strip _Mod__Mod_var_i8_L
_Mod__Mod_var_i8_L:
LFB573:
	pushq	%rbp
LCFI2040:
	movq	%rsp, %rbp
LCFI2041:
	pushq	%rbx
LCFI2042:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movq	192+_MM_Mod(%rip), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1079
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1079
	addq	%rcx, %rdx
L1079:
	movq	%rdx, %rax
	popq	%rbx
	leave
LCFI2043:
	ret
LFE573:
.no_dead_strip _Mod__uMod_param_i8_L
_Mod__uMod_param_i8_L:
LFB574:
	pushq	%rbp
LCFI2044:
	movq	%rsp, %rbp
LCFI2045:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	movq	-16(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2046:
	ret
LFE574:
.no_dead_strip _Mod__Mod_param_i8_L
_Mod__Mod_param_i8_L:
LFB575:
	pushq	%rbp
LCFI2047:
	movq	%rsp, %rbp
LCFI2048:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rcx
	movq	%rcx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	-16(%rbp)
	testq	%rdx, %rdx
	je	L1082
	movq	%rcx, %rax
	xorq	-16(%rbp), %rax
	testq	%rax, %rax
	jns	L1082
	addq	-16(%rbp), %rdx
L1082:
	movq	%rdx, %rax
	leave
LCFI2049:
	ret
LFE575:
.no_dead_strip _Mod__uMod_var_i8_i8
_Mod__uMod_var_i8_i8:
LFB576:
	pushq	%rbp
LCFI2050:
	movq	%rsp, %rbp
LCFI2051:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rax
	movzbl	200+_MM_Mod(%rip), %edx
	movsbq	%dl, %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2052:
	ret
LFE576:
.no_dead_strip _Mod__Mod_var_i8_i8
_Mod__Mod_var_i8_i8:
LFB577:
	pushq	%rbp
LCFI2053:
	movq	%rsp, %rbp
LCFI2054:
	pushq	%rbx
LCFI2055:
	subq	$8, %rsp
LCFI2056:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1085
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1085
	addq	%rcx, %rdx
L1085:
	cmpq	$-128, %rdx
	jl	L1086
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1087
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1087
	addq	%rcx, %rdx
L1087:
	cmpq	$127, %rdx
	jle	L1088
L1086:
	movl	$20193, %edi
	call	__m3_fault
L1088:
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rbx
	movzbl	200+_MM_Mod(%rip), %eax
	movsbq	%al, %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1089
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1089
	addq	%rcx, %rdx
L1089:
	movl	%edx, %eax
	addq	$8, %rsp
	popq	%rbx
	leave
LCFI2057:
	ret
LFE577:
.no_dead_strip _Mod__uMod_param_i8_i8
_Mod__uMod_param_i8_i8:
LFB578:
	pushq	%rbp
LCFI2058:
	movq	%rsp, %rbp
LCFI2059:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rax
	movsbq	-2(%rbp), %rcx
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	leave
LCFI2060:
	ret
LFE578:
.no_dead_strip _Mod__Mod_param_i8_i8
_Mod__Mod_param_i8_i8:
LFB579:
	pushq	%rbp
LCFI2061:
	movq	%rsp, %rbp
LCFI2062:
	pushq	%rbx
LCFI2063:
	subq	$24, %rsp
LCFI2064:
	movb	%dil, -17(%rbp)
	movb	%sil, -18(%rbp)
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1092
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1092
	addq	%rcx, %rdx
L1092:
	cmpq	$-128, %rdx
	jl	L1093
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1094
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1094
	addq	%rcx, %rdx
L1094:
	cmpq	$127, %rdx
	jle	L1095
L1093:
	movl	$20257, %edi
	call	__m3_fault
L1095:
	movsbq	-17(%rbp), %rbx
	movsbq	-18(%rbp), %rcx
	movq	%rbx, %rax
	movq	%rax, %rdx
	sarq	$63, %rdx
	idivq	%rcx
	testq	%rdx, %rdx
	je	L1096
	movq	%rbx, %rax
	xorq	%rcx, %rax
	testq	%rax, %rax
	jns	L1096
	addq	%rcx, %rdx
L1096:
	movl	%edx, %eax
	addq	$24, %rsp
	popq	%rbx
	leave
LCFI2065:
	ret
LFE579:
.globl _Mod_M3
.no_dead_strip _Mod_M3
_Mod_M3:
LFB580:
	pushq	%rbp
LCFI2066:
	movq	%rsp, %rbp
LCFI2067:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Mod(%rip), %rax
	leave
LCFI2068:
	ret
LFE580:
__m3_fault:
LFB581:
	pushq	%rbp
LCFI2069:
	movq	%rsp, %rbp
LCFI2070:
	subq	$16, %rsp
LCFI2071:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_Mod(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI2072:
	ret
LFE581:
	.const_data
	.align 5
_L_1:
	.ascii "Mod_M3"
	.space 1
	.ascii "Mod_param_i8_i8"
	.space 1
	.ascii "uMod_param_i8_i8"
	.space 1
	.ascii "Mod_var_i8_i8"
	.space 1
	.ascii "uMod_var_i8_i8"
	.space 1
	.ascii "Mod_param_i8_L"
	.space 1
	.ascii "uMod_param_i8_L"
	.space 1
	.ascii "Mod_var_i8_L"
	.space 1
	.ascii "uMod_var_i8_L"
	.space 1
	.ascii "Mod_param_i8_i64"
	.space 1
	.ascii "uMod_param_i8_i64"
	.space 1
	.ascii "Mod_var_i8_i64"
	.space 1
	.ascii "uMod_var_i8_i64"
	.space 1
	.ascii "Mod_param_i8_u8"
	.space 1
	.ascii "uMod_param_i8_u8"
	.space 1
	.ascii "Mod_var_i8_u8"
	.space 1
	.ascii "uMod_var_i8_u8"
	.space 1
	.ascii "Mod_param_i8_C"
	.space 1
	.ascii "uMod_param_i8_C"
	.space 1
	.ascii "Mod_var_i8_C"
	.space 1
	.ascii "uMod_var_i8_C"
	.space 1
	.ascii "Mod_param_i8_i32"
	.space 1
	.ascii "uMod_param_i8_i32"
	.space 1
	.ascii "Mod_var_i8_i32"
	.space 1
	.ascii "uMod_var_i8_i32"
	.space 1
	.ascii "Mod_param_i8_u64"
	.space 1
	.ascii "uMod_param_i8_u64"
	.space 1
	.ascii "Mod_var_i8_u64"
	.space 1
	.ascii "uMod_var_i8_u64"
	.space 1
	.ascii "Mod_param_i8_i16"
	.space 1
	.ascii "uMod_param_i8_i16"
	.space 1
	.ascii "Mod_var_i8_i16"
	.space 1
	.ascii "uMod_var_i8_i16"
	.space 1
	.ascii "Mod_param_i8_I"
	.space 1
	.ascii "uMod_param_i8_I"
	.space 1
	.ascii "Mod_var_i8_I"
	.space 1
	.ascii "uMod_var_i8_I"
	.space 1
	.ascii "Mod_param_i8_LC"
	.space 1
	.ascii "uMod_param_i8_LC"
	.space 1
	.ascii "Mod_var_i8_LC"
	.space 1
	.ascii "uMod_var_i8_LC"
	.space 1
	.ascii "Mod_param_i8_u32"
	.space 1
	.ascii "uMod_param_i8_u32"
	.space 1
	.ascii "Mod_var_i8_u32"
	.space 1
	.ascii "uMod_var_i8_u32"
	.space 1
	.ascii "Mod_param_i8_u16"
	.space 1
	.ascii "uMod_param_i8_u16"
	.space 1
	.ascii "Mod_var_i8_u16"
	.space 1
	.ascii "uMod_var_i8_u16"
	.space 1
	.ascii "Mod_param_L_i8"
	.space 1
	.ascii "uMod_param_L_i8"
	.space 1
	.ascii "Mod_var_L_i8"
	.space 1
	.ascii "uMod_var_L_i8"
	.space 1
	.ascii "Mod_param_L_L"
	.space 1
	.ascii "uMod_param_L_L"
	.space 1
	.ascii "Mod_var_L_L"
	.space 1
	.ascii "uMod_var_L_L"
	.space 1
	.ascii "Mod_param_L_i64"
	.space 1
	.ascii "uMod_param_L_i64"
	.space 1
	.ascii "Mod_var_L_i64"
	.space 1
	.ascii "uMod_var_L_i64"
	.space 1
	.ascii "Mod_param_L_u8"
	.space 1
	.ascii "uMod_param_L_u8"
	.space 1
	.ascii "Mod_var_L_u8"
	.space 1
	.ascii "uMod_var_L_u8"
	.space 1
	.ascii "Mod_param_L_C"
	.space 1
	.ascii "uMod_param_L_C"
	.space 1
	.ascii "Mod_var_L_C"
	.space 1
	.ascii "uMod_var_L_C"
	.space 1
	.ascii "Mod_param_L_i32"
	.space 1
	.ascii "uMod_param_L_i32"
	.space 1
	.ascii "Mod_var_L_i32"
	.space 1
	.ascii "uMod_var_L_i32"
	.space 1
	.ascii "Mod_param_L_u64"
	.space 1
	.ascii "uMod_param_L_u64"
	.space 1
	.ascii "Mod_var_L_u64"
	.space 1
	.ascii "uMod_var_L_u64"
	.space 1
	.ascii "Mod_param_L_i16"
	.space 1
	.ascii "uMod_param_L_i16"
	.space 1
	.ascii "Mod_var_L_i16"
	.space 1
	.ascii "uMod_var_L_i16"
	.space 1
	.ascii "Mod_param_L_I"
	.space 1
	.ascii "uMod_param_L_I"
	.space 1
	.ascii "Mod_var_L_I"
	.space 1
	.ascii "uMod_var_L_I"
	.space 1
	.ascii "Mod_param_L_LC"
	.space 1
	.ascii "uMod_param_L_LC"
	.space 1
	.ascii "Mod_var_L_LC"
	.space 1
	.ascii "uMod_var_L_LC"
	.space 1
	.ascii "Mod_param_L_u32"
	.space 1
	.ascii "uMod_param_L_u32"
	.space 1
	.ascii "Mod_var_L_u32"
	.space 1
	.ascii "uMod_var_L_u32"
	.space 1
	.ascii "Mod_param_L_u16"
	.space 1
	.ascii "uMod_param_L_u16"
	.space 1
	.ascii "Mod_var_L_u16"
	.space 1
	.ascii "uMod_var_L_u16"
	.space 1
	.ascii "Mod_param_f32_f32"
	.space 1
	.ascii "Mod_var_f32_f32"
	.space 1
	.ascii "Mod_param_i64_i8"
	.space 1
	.ascii "uMod_param_i64_i8"
	.space 1
	.ascii "Mod_var_i64_i8"
	.space 1
	.ascii "uMod_var_i64_i8"
	.space 1
	.ascii "Mod_param_i64_L"
	.space 1
	.ascii "uMod_param_i64_L"
	.space 1
	.ascii "Mod_var_i64_L"
	.space 1
	.ascii "uMod_var_i64_L"
	.space 1
	.ascii "Mod_param_i64_i64"
	.space 1
	.ascii "uMod_param_i64_i64"
	.space 1
	.ascii "Mod_var_i64_i64"
	.space 1
	.ascii "uMod_var_i64_i64"
	.space 1
	.ascii "Mod_param_i64_u8"
	.space 1
	.ascii "uMod_param_i64_u8"
	.space 1
	.ascii "Mod_var_i64_u8"
	.space 1
	.ascii "uMod_var_i64_u8"
	.space 1
	.ascii "Mod_param_i64_C"
	.space 1
	.ascii "uMod_param_i64_C"
	.space 1
	.ascii "Mod_var_i64_C"
	.space 1
	.ascii "uMod_var_i64_C"
	.space 1
	.ascii "Mod_param_i64_i32"
	.space 1
	.ascii "uMod_param_i64_i32"
	.space 1
	.ascii "Mod_var_i64_i32"
	.space 1
	.ascii "uMod_var_i64_i32"
	.space 1
	.ascii "Mod_param_i64_u64"
	.space 1
	.ascii "uMod_param_i64_u64"
	.space 1
	.ascii "Mod_var_i64_u64"
	.space 1
	.ascii "uMod_var_i64_u64"
	.space 1
	.ascii "Mod_param_i64_i16"
	.space 1
	.ascii "uMod_param_i64_i16"
	.space 1
	.ascii "Mod_var_i64_i16"
	.space 1
	.ascii "uMod_var_i64_i16"
	.space 1
	.ascii "Mod_param_i64_I"
	.space 1
	.ascii "uMod_param_i64_I"
	.space 1
	.ascii "Mod_var_i64_I"
	.space 1
	.ascii "uMod_var_i64_I"
	.space 1
	.ascii "Mod_param_i64_LC"
	.space 1
	.ascii "uMod_param_i64_LC"
	.space 1
	.ascii "Mod_var_i64_LC"
	.space 1
	.ascii "uMod_var_i64_LC"
	.space 1
	.ascii "Mod_param_i64_u32"
	.space 1
	.ascii "uMod_param_i64_u32"
	.space 1
	.ascii "Mod_var_i64_u32"
	.space 1
	.ascii "uMod_var_i64_u32"
	.space 1
	.ascii "Mod_param_i64_u16"
	.space 1
	.ascii "uMod_param_i64_u16"
	.space 1
	.ascii "Mod_var_i64_u16"
	.space 1
	.ascii "uMod_var_i64_u16"
	.space 1
	.ascii "Mod_param_u8_i8"
	.space 1
	.ascii "uMod_param_u8_i8"
	.space 1
	.ascii "Mod_var_u8_i8"
	.space 1
	.ascii "uMod_var_u8_i8"
	.space 1
	.ascii "Mod_param_u8_L"
	.space 1
	.ascii "uMod_param_u8_L"
	.space 1
	.ascii "Mod_var_u8_L"
	.space 1
	.ascii "uMod_var_u8_L"
	.space 1
	.ascii "Mod_param_u8_i64"
	.space 1
	.ascii "uMod_param_u8_i64"
	.space 1
	.ascii "Mod_var_u8_i64"
	.space 1
	.ascii "uMod_var_u8_i64"
	.space 1
	.ascii "Mod_param_u8_u8"
	.space 1
	.ascii "uMod_param_u8_u8"
	.space 1
	.ascii "Mod_var_u8_u8"
	.space 1
	.ascii "uMod_var_u8_u8"
	.space 1
	.ascii "Mod_param_u8_C"
	.space 1
	.ascii "uMod_param_u8_C"
	.space 1
	.ascii "Mod_var_u8_C"
	.space 1
	.ascii "uMod_var_u8_C"
	.space 1
	.ascii "Mod_param_u8_i32"
	.space 1
	.ascii "uMod_param_u8_i32"
	.space 1
	.ascii "Mod_var_u8_i32"
	.space 1
	.ascii "uMod_var_u8_i32"
	.space 1
	.ascii "Mod_param_u8_u64"
	.space 1
	.ascii "uMod_param_u8_u64"
	.space 1
	.ascii "Mod_var_u8_u64"
	.space 1
	.ascii "uMod_var_u8_u64"
	.space 1
	.ascii "Mod_param_u8_i16"
	.space 1
	.ascii "uMod_param_u8_i16"
	.space 1
	.ascii "Mod_var_u8_i16"
	.space 1
	.ascii "uMod_var_u8_i16"
	.space 1
	.ascii "Mod_param_u8_I"
	.space 1
	.ascii "uMod_param_u8_I"
	.space 1
	.ascii "Mod_var_u8_I"
	.space 1
	.ascii "uMod_var_u8_I"
	.space 1
	.ascii "Mod_param_u8_LC"
	.space 1
	.ascii "uMod_param_u8_LC"
	.space 1
	.ascii "Mod_var_u8_LC"
	.space 1
	.ascii "uMod_var_u8_LC"
	.space 1
	.ascii "Mod_param_u8_u32"
	.space 1
	.ascii "uMod_param_u8_u32"
	.space 1
	.ascii "Mod_var_u8_u32"
	.space 1
	.ascii "uMod_var_u8_u32"
	.space 1
	.ascii "Mod_param_u8_u16"
	.space 1
	.ascii "uMod_param_u8_u16"
	.space 1
	.ascii "Mod_var_u8_u16"
	.space 1
	.ascii "uMod_var_u8_u16"
	.space 1
	.ascii "Mod_param_C_i8"
	.space 1
	.ascii "uMod_param_C_i8"
	.space 1
	.ascii "Mod_var_C_i8"
	.space 1
	.ascii "uMod_var_C_i8"
	.space 1
	.ascii "Mod_param_C_L"
	.space 1
	.ascii "uMod_param_C_L"
	.space 1
	.ascii "Mod_var_C_L"
	.space 1
	.ascii "uMod_var_C_L"
	.space 1
	.ascii "Mod_param_C_i64"
	.space 1
	.ascii "uMod_param_C_i64"
	.space 1
	.ascii "Mod_var_C_i64"
	.space 1
	.ascii "uMod_var_C_i64"
	.space 1
	.ascii "Mod_param_C_u8"
	.space 1
	.ascii "uMod_param_C_u8"
	.space 1
	.ascii "Mod_var_C_u8"
	.space 1
	.ascii "uMod_var_C_u8"
	.space 1
	.ascii "Mod_param_C_C"
	.space 1
	.ascii "uMod_param_C_C"
	.space 1
	.ascii "Mod_var_C_C"
	.space 1
	.ascii "uMod_var_C_C"
	.space 1
	.ascii "Mod_param_C_i32"
	.space 1
	.ascii "uMod_param_C_i32"
	.space 1
	.ascii "Mod_var_C_i32"
	.space 1
	.ascii "uMod_var_C_i32"
	.space 1
	.ascii "Mod_param_C_u64"
	.space 1
	.ascii "uMod_param_C_u64"
	.space 1
	.ascii "Mod_var_C_u64"
	.space 1
	.ascii "uMod_var_C_u64"
	.space 1
	.ascii "Mod_param_C_i16"
	.space 1
	.ascii "uMod_param_C_i16"
	.space 1
	.ascii "Mod_var_C_i16"
	.space 1
	.ascii "uMod_var_C_i16"
	.space 1
	.ascii "Mod_param_C_I"
	.space 1
	.ascii "uMod_param_C_I"
	.space 1
	.ascii "Mod_var_C_I"
	.space 1
	.ascii "uMod_var_C_I"
	.space 1
	.ascii "Mod_param_C_LC"
	.space 1
	.ascii "uMod_param_C_LC"
	.space 1
	.ascii "Mod_var_C_LC"
	.space 1
	.ascii "uMod_var_C_LC"
	.space 1
	.ascii "Mod_param_C_u32"
	.space 1
	.ascii "uMod_param_C_u32"
	.space 1
	.ascii "Mod_var_C_u32"
	.space 1
	.ascii "uMod_var_C_u32"
	.space 1
	.ascii "Mod_param_C_u16"
	.space 1
	.ascii "uMod_param_C_u16"
	.space 1
	.ascii "Mod_var_C_u16"
	.space 1
	.ascii "uMod_var_C_u16"
	.space 1
	.ascii "Mod_param_i32_i8"
	.space 1
	.ascii "uMod_param_i32_i8"
	.space 1
	.ascii "Mod_var_i32_i8"
	.space 1
	.ascii "uMod_var_i32_i8"
	.space 1
	.ascii "Mod_param_i32_L"
	.space 1
	.ascii "uMod_param_i32_L"
	.space 1
	.ascii "Mod_var_i32_L"
	.space 1
	.ascii "uMod_var_i32_L"
	.space 1
	.ascii "Mod_param_i32_i64"
	.space 1
	.ascii "uMod_param_i32_i64"
	.space 1
	.ascii "Mod_var_i32_i64"
	.space 1
	.ascii "uMod_var_i32_i64"
	.space 1
	.ascii "Mod_param_i32_u8"
	.space 1
	.ascii "uMod_param_i32_u8"
	.space 1
	.ascii "Mod_var_i32_u8"
	.space 1
	.ascii "uMod_var_i32_u8"
	.space 1
	.ascii "Mod_param_i32_C"
	.space 1
	.ascii "uMod_param_i32_C"
	.space 1
	.ascii "Mod_var_i32_C"
	.space 1
	.ascii "uMod_var_i32_C"
	.space 1
	.ascii "Mod_param_i32_i32"
	.space 1
	.ascii "uMod_param_i32_i32"
	.space 1
	.ascii "Mod_var_i32_i32"
	.space 1
	.ascii "uMod_var_i32_i32"
	.space 1
	.ascii "Mod_param_i32_u64"
	.space 1
	.ascii "uMod_param_i32_u64"
	.space 1
	.ascii "Mod_var_i32_u64"
	.space 1
	.ascii "uMod_var_i32_u64"
	.space 1
	.ascii "Mod_param_i32_i16"
	.space 1
	.ascii "uMod_param_i32_i16"
	.space 1
	.ascii "Mod_var_i32_i16"
	.space 1
	.ascii "uMod_var_i32_i16"
	.space 1
	.ascii "Mod_param_i32_I"
	.space 1
	.ascii "uMod_param_i32_I"
	.space 1
	.ascii "Mod_var_i32_I"
	.space 1
	.ascii "uMod_var_i32_I"
	.space 1
	.ascii "Mod_param_i32_LC"
	.space 1
	.ascii "uMod_param_i32_LC"
	.space 1
	.ascii "Mod_var_i32_LC"
	.space 1
	.ascii "uMod_var_i32_LC"
	.space 1
	.ascii "Mod_param_i32_u32"
	.space 1
	.ascii "uMod_param_i32_u32"
	.space 1
	.ascii "Mod_var_i32_u32"
	.space 1
	.ascii "uMod_var_i32_u32"
	.space 1
	.ascii "Mod_param_i32_u16"
	.space 1
	.ascii "uMod_param_i32_u16"
	.space 1
	.ascii "Mod_var_i32_u16"
	.space 1
	.ascii "uMod_var_i32_u16"
	.space 1
	.ascii "Mod_param_f64_f64"
	.space 1
	.ascii "Mod_var_f64_f64"
	.space 1
	.ascii "Mod_param_u64_i8"
	.space 1
	.ascii "uMod_param_u64_i8"
	.space 1
	.ascii "Mod_var_u64_i8"
	.space 1
	.ascii "uMod_var_u64_i8"
	.space 1
	.ascii "Mod_param_u64_L"
	.space 1
	.ascii "uMod_param_u64_L"
	.space 1
	.ascii "Mod_var_u64_L"
	.space 1
	.ascii "uMod_var_u64_L"
	.space 1
	.ascii "Mod_param_u64_i64"
	.space 1
	.ascii "uMod_param_u64_i64"
	.space 1
	.ascii "Mod_var_u64_i64"
	.space 1
	.ascii "uMod_var_u64_i64"
	.space 1
	.ascii "Mod_param_u64_u8"
	.space 1
	.ascii "uMod_param_u64_u8"
	.space 1
	.ascii "Mod_var_u64_u8"
	.space 1
	.ascii "uMod_var_u64_u8"
	.space 1
	.ascii "Mod_param_u64_C"
	.space 1
	.ascii "uMod_param_u64_C"
	.space 1
	.ascii "Mod_var_u64_C"
	.space 1
	.ascii "uMod_var_u64_C"
	.space 1
	.ascii "Mod_param_u64_i32"
	.space 1
	.ascii "uMod_param_u64_i32"
	.space 1
	.ascii "Mod_var_u64_i32"
	.space 1
	.ascii "uMod_var_u64_i32"
	.space 1
	.ascii "Mod_param_u64_u64"
	.space 1
	.ascii "uMod_param_u64_u64"
	.space 1
	.ascii "Mod_var_u64_u64"
	.space 1
	.ascii "uMod_var_u64_u64"
	.space 1
	.ascii "Mod_param_u64_i16"
	.space 1
	.ascii "uMod_param_u64_i16"
	.space 1
	.ascii "Mod_var_u64_i16"
	.space 1
	.ascii "uMod_var_u64_i16"
	.space 1
	.ascii "Mod_param_u64_I"
	.space 1
	.ascii "uMod_param_u64_I"
	.space 1
	.ascii "Mod_var_u64_I"
	.space 1
	.ascii "uMod_var_u64_I"
	.space 1
	.ascii "Mod_param_u64_LC"
	.space 1
	.ascii "uMod_param_u64_LC"
	.space 1
	.ascii "Mod_var_u64_LC"
	.space 1
	.ascii "uMod_var_u64_LC"
	.space 1
	.ascii "Mod_param_u64_u32"
	.space 1
	.ascii "uMod_param_u64_u32"
	.space 1
	.ascii "Mod_var_u64_u32"
	.space 1
	.ascii "uMod_var_u64_u32"
	.space 1
	.ascii "Mod_param_u64_u16"
	.space 1
	.ascii "uMod_param_u64_u16"
	.space 1
	.ascii "Mod_var_u64_u16"
	.space 1
	.ascii "uMod_var_u64_u16"
	.space 1
	.ascii "Mod_param_i16_i8"
	.space 1
	.ascii "uMod_param_i16_i8"
	.space 1
	.ascii "Mod_var_i16_i8"
	.space 1
	.ascii "uMod_var_i16_i8"
	.space 1
	.ascii "Mod_param_i16_L"
	.space 1
	.ascii "uMod_param_i16_L"
	.space 1
	.ascii "Mod_var_i16_L"
	.space 1
	.ascii "uMod_var_i16_L"
	.space 1
	.ascii "Mod_param_i16_i64"
	.space 1
	.ascii "uMod_param_i16_i64"
	.space 1
	.ascii "Mod_var_i16_i64"
	.space 1
	.ascii "uMod_var_i16_i64"
	.space 1
	.ascii "Mod_param_i16_u8"
	.space 1
	.ascii "uMod_param_i16_u8"
	.space 1
	.ascii "Mod_var_i16_u8"
	.space 1
	.ascii "uMod_var_i16_u8"
	.space 1
	.ascii "Mod_param_i16_C"
	.space 1
	.ascii "uMod_param_i16_C"
	.space 1
	.ascii "Mod_var_i16_C"
	.space 1
	.ascii "uMod_var_i16_C"
	.space 1
	.ascii "Mod_param_i16_i32"
	.space 1
	.ascii "uMod_param_i16_i32"
	.space 1
	.ascii "Mod_var_i16_i32"
	.space 1
	.ascii "uMod_var_i16_i32"
	.space 1
	.ascii "Mod_param_i16_u64"
	.space 1
	.ascii "uMod_param_i16_u64"
	.space 1
	.ascii "Mod_var_i16_u64"
	.space 1
	.ascii "uMod_var_i16_u64"
	.space 1
	.ascii "Mod_param_i16_i16"
	.space 1
	.ascii "uMod_param_i16_i16"
	.space 1
	.ascii "Mod_var_i16_i16"
	.space 1
	.ascii "uMod_var_i16_i16"
	.space 1
	.ascii "Mod_param_i16_I"
	.space 1
	.ascii "uMod_param_i16_I"
	.space 1
	.ascii "Mod_var_i16_I"
	.space 1
	.ascii "uMod_var_i16_I"
	.space 1
	.ascii "Mod_param_i16_LC"
	.space 1
	.ascii "uMod_param_i16_LC"
	.space 1
	.ascii "Mod_var_i16_LC"
	.space 1
	.ascii "uMod_var_i16_LC"
	.space 1
	.ascii "Mod_param_i16_u32"
	.space 1
	.ascii "uMod_param_i16_u32"
	.space 1
	.ascii "Mod_var_i16_u32"
	.space 1
	.ascii "uMod_var_i16_u32"
	.space 1
	.ascii "Mod_param_i16_u16"
	.space 1
	.ascii "uMod_param_i16_u16"
	.space 1
	.ascii "Mod_var_i16_u16"
	.space 1
	.ascii "uMod_var_i16_u16"
	.space 1
	.ascii "Mod_param_I_i8"
	.space 1
	.ascii "uMod_param_I_i8"
	.space 1
	.ascii "Mod_var_I_i8"
	.space 1
	.ascii "uMod_var_I_i8"
	.space 1
	.ascii "Mod_param_I_L"
	.space 1
	.ascii "uMod_param_I_L"
	.space 1
	.ascii "Mod_var_I_L"
	.space 1
	.ascii "uMod_var_I_L"
	.space 1
	.ascii "Mod_param_I_i64"
	.space 1
	.ascii "uMod_param_I_i64"
	.space 1
	.ascii "Mod_var_I_i64"
	.space 1
	.ascii "uMod_var_I_i64"
	.space 1
	.ascii "Mod_param_I_u8"
	.space 1
	.ascii "uMod_param_I_u8"
	.space 1
	.ascii "Mod_var_I_u8"
	.space 1
	.ascii "uMod_var_I_u8"
	.space 1
	.ascii "Mod_param_I_C"
	.space 1
	.ascii "uMod_param_I_C"
	.space 1
	.ascii "Mod_var_I_C"
	.space 1
	.ascii "uMod_var_I_C"
	.space 1
	.ascii "Mod_param_I_i32"
	.space 1
	.ascii "uMod_param_I_i32"
	.space 1
	.ascii "Mod_var_I_i32"
	.space 1
	.ascii "uMod_var_I_i32"
	.space 1
	.ascii "Mod_param_I_u64"
	.space 1
	.ascii "uMod_param_I_u64"
	.space 1
	.ascii "Mod_var_I_u64"
	.space 1
	.ascii "uMod_var_I_u64"
	.space 1
	.ascii "Mod_param_I_i16"
	.space 1
	.ascii "uMod_param_I_i16"
	.space 1
	.ascii "Mod_var_I_i16"
	.space 1
	.ascii "uMod_var_I_i16"
	.space 1
	.ascii "Mod_param_I_I"
	.space 1
	.ascii "uMod_param_I_I"
	.space 1
	.ascii "Mod_var_I_I"
	.space 1
	.ascii "uMod_var_I_I"
	.space 1
	.ascii "Mod_param_I_LC"
	.space 1
	.ascii "uMod_param_I_LC"
	.space 1
	.ascii "Mod_var_I_LC"
	.space 1
	.ascii "uMod_var_I_LC"
	.space 1
	.ascii "Mod_param_I_u32"
	.space 1
	.ascii "uMod_param_I_u32"
	.space 1
	.ascii "Mod_var_I_u32"
	.space 1
	.ascii "uMod_var_I_u32"
	.space 1
	.ascii "Mod_param_I_u16"
	.space 1
	.ascii "uMod_param_I_u16"
	.space 1
	.ascii "Mod_var_I_u16"
	.space 1
	.ascii "uMod_var_I_u16"
	.space 1
	.ascii "Mod_param_LC_i8"
	.space 1
	.ascii "uMod_param_LC_i8"
	.space 1
	.ascii "Mod_var_LC_i8"
	.space 1
	.ascii "uMod_var_LC_i8"
	.space 1
	.ascii "Mod_param_LC_L"
	.space 1
	.ascii "uMod_param_LC_L"
	.space 1
	.ascii "Mod_var_LC_L"
	.space 1
	.ascii "uMod_var_LC_L"
	.space 1
	.ascii "Mod_param_LC_i64"
	.space 1
	.ascii "uMod_param_LC_i64"
	.space 1
	.ascii "Mod_var_LC_i64"
	.space 1
	.ascii "uMod_var_LC_i64"
	.space 1
	.ascii "Mod_param_LC_u8"
	.space 1
	.ascii "uMod_param_LC_u8"
	.space 1
	.ascii "Mod_var_LC_u8"
	.space 1
	.ascii "uMod_var_LC_u8"
	.space 1
	.ascii "Mod_param_LC_C"
	.space 1
	.ascii "uMod_param_LC_C"
	.space 1
	.ascii "Mod_var_LC_C"
	.space 1
	.ascii "uMod_var_LC_C"
	.space 1
	.ascii "Mod_param_LC_i32"
	.space 1
	.ascii "uMod_param_LC_i32"
	.space 1
	.ascii "Mod_var_LC_i32"
	.space 1
	.ascii "uMod_var_LC_i32"
	.space 1
	.ascii "Mod_param_LC_u64"
	.space 1
	.ascii "uMod_param_LC_u64"
	.space 1
	.ascii "Mod_var_LC_u64"
	.space 1
	.ascii "uMod_var_LC_u64"
	.space 1
	.ascii "Mod_param_LC_i16"
	.space 1
	.ascii "uMod_param_LC_i16"
	.space 1
	.ascii "Mod_var_LC_i16"
	.space 1
	.ascii "uMod_var_LC_i16"
	.space 1
	.ascii "Mod_param_LC_I"
	.space 1
	.ascii "uMod_param_LC_I"
	.space 1
	.ascii "Mod_var_LC_I"
	.space 1
	.ascii "uMod_var_LC_I"
	.space 1
	.ascii "Mod_param_LC_LC"
	.space 1
	.ascii "uMod_param_LC_LC"
	.space 1
	.ascii "Mod_var_LC_LC"
	.space 1
	.ascii "uMod_var_LC_LC"
	.space 1
	.ascii "Mod_param_LC_u32"
	.space 1
	.ascii "uMod_param_LC_u32"
	.space 1
	.ascii "Mod_var_LC_u32"
	.space 1
	.ascii "uMod_var_LC_u32"
	.space 1
	.ascii "Mod_param_LC_u16"
	.space 1
	.ascii "uMod_param_LC_u16"
	.space 1
	.ascii "Mod_var_LC_u16"
	.space 1
	.ascii "uMod_var_LC_u16"
	.space 1
	.ascii "Mod_param_u32_i8"
	.space 1
	.ascii "uMod_param_u32_i8"
	.space 1
	.ascii "Mod_var_u32_i8"
	.space 1
	.ascii "uMod_var_u32_i8"
	.space 1
	.ascii "Mod_param_u32_L"
	.space 1
	.ascii "uMod_param_u32_L"
	.space 1
	.ascii "Mod_var_u32_L"
	.space 1
	.ascii "uMod_var_u32_L"
	.space 1
	.ascii "Mod_param_u32_i64"
	.space 1
	.ascii "uMod_param_u32_i64"
	.space 1
	.ascii "Mod_var_u32_i64"
	.space 1
	.ascii "uMod_var_u32_i64"
	.space 1
	.ascii "Mod_param_u32_u8"
	.space 1
	.ascii "uMod_param_u32_u8"
	.space 1
	.ascii "Mod_var_u32_u8"
	.space 1
	.ascii "uMod_var_u32_u8"
	.space 1
	.ascii "Mod_param_u32_C"
	.space 1
	.ascii "uMod_param_u32_C"
	.space 1
	.ascii "Mod_var_u32_C"
	.space 1
	.ascii "uMod_var_u32_C"
	.space 1
	.ascii "Mod_param_u32_i32"
	.space 1
	.ascii "uMod_param_u32_i32"
	.space 1
	.ascii "Mod_var_u32_i32"
	.space 1
	.ascii "uMod_var_u32_i32"
	.space 1
	.ascii "Mod_param_u32_u64"
	.space 1
	.ascii "uMod_param_u32_u64"
	.space 1
	.ascii "Mod_var_u32_u64"
	.space 1
	.ascii "uMod_var_u32_u64"
	.space 1
	.ascii "Mod_param_u32_i16"
	.space 1
	.ascii "uMod_param_u32_i16"
	.space 1
	.ascii "Mod_var_u32_i16"
	.space 1
	.ascii "uMod_var_u32_i16"
	.space 1
	.ascii "Mod_param_u32_I"
	.space 1
	.ascii "uMod_param_u32_I"
	.space 1
	.ascii "Mod_var_u32_I"
	.space 1
	.ascii "uMod_var_u32_I"
	.space 1
	.ascii "Mod_param_u32_LC"
	.space 1
	.ascii "uMod_param_u32_LC"
	.space 1
	.ascii "Mod_var_u32_LC"
	.space 1
	.ascii "uMod_var_u32_LC"
	.space 1
	.ascii "Mod_param_u32_u32"
	.space 1
	.ascii "uMod_param_u32_u32"
	.space 1
	.ascii "Mod_var_u32_u32"
	.space 1
	.ascii "uMod_var_u32_u32"
	.space 1
	.ascii "Mod_param_u32_u16"
	.space 1
	.ascii "uMod_param_u32_u16"
	.space 1
	.ascii "Mod_var_u32_u16"
	.space 1
	.ascii "uMod_var_u32_u16"
	.space 1
	.ascii "Mod_param_u16_i8"
	.space 1
	.ascii "uMod_param_u16_i8"
	.space 1
	.ascii "Mod_var_u16_i8"
	.space 1
	.ascii "uMod_var_u16_i8"
	.space 1
	.ascii "Mod_param_u16_L"
	.space 1
	.ascii "uMod_param_u16_L"
	.space 1
	.ascii "Mod_var_u16_L"
	.space 1
	.ascii "uMod_var_u16_L"
	.space 1
	.ascii "Mod_param_u16_i64"
	.space 1
	.ascii "uMod_param_u16_i64"
	.space 1
	.ascii "Mod_var_u16_i64"
	.space 1
	.ascii "uMod_var_u16_i64"
	.space 1
	.ascii "Mod_param_u16_u8"
	.space 1
	.ascii "uMod_param_u16_u8"
	.space 1
	.ascii "Mod_var_u16_u8"
	.space 1
	.ascii "uMod_var_u16_u8"
	.space 1
	.ascii "Mod_param_u16_C"
	.space 1
	.ascii "uMod_param_u16_C"
	.space 1
	.ascii "Mod_var_u16_C"
	.space 1
	.ascii "uMod_var_u16_C"
	.space 1
	.ascii "Mod_param_u16_i32"
	.space 1
	.ascii "uMod_param_u16_i32"
	.space 1
	.ascii "Mod_var_u16_i32"
	.space 1
	.ascii "uMod_var_u16_i32"
	.space 1
	.ascii "Mod_param_u16_u64"
	.space 1
	.ascii "uMod_param_u16_u64"
	.space 1
	.ascii "Mod_var_u16_u64"
	.space 1
	.ascii "uMod_var_u16_u64"
	.space 1
	.ascii "Mod_param_u16_i16"
	.space 1
	.ascii "uMod_param_u16_i16"
	.space 1
	.ascii "Mod_var_u16_i16"
	.space 1
	.ascii "uMod_var_u16_i16"
	.space 1
	.ascii "Mod_param_u16_I"
	.space 1
	.ascii "uMod_param_u16_I"
	.space 1
	.ascii "Mod_var_u16_I"
	.space 1
	.ascii "uMod_var_u16_I"
	.space 1
	.ascii "Mod_param_u16_LC"
	.space 1
	.ascii "uMod_param_u16_LC"
	.space 1
	.ascii "Mod_var_u16_LC"
	.space 1
	.ascii "uMod_var_u16_LC"
	.space 1
	.ascii "Mod_param_u16_u32"
	.space 1
	.ascii "uMod_param_u16_u32"
	.space 1
	.ascii "Mod_var_u16_u32"
	.space 1
	.ascii "uMod_var_u16_u32"
	.space 1
	.ascii "Mod_param_u16_u16"
	.space 1
	.ascii "uMod_param_u16_u16"
	.space 1
	.ascii "Mod_var_u16_u16"
	.space 1
	.ascii "uMod_var_u16_u16"
	.space 6
	.quad	_Mod_M3
	.quad	_L_1
	.quad	_Mod__Mod_param_i8_i8
	.quad	_L_1+7
	.quad	_Mod__uMod_param_i8_i8
	.quad	_L_1+23
	.quad	_Mod__Mod_var_i8_i8
	.quad	_L_1+40
	.quad	_Mod__uMod_var_i8_i8
	.quad	_L_1+54
	.quad	_Mod__Mod_param_i8_L
	.quad	_L_1+69
	.quad	_Mod__uMod_param_i8_L
	.quad	_L_1+84
	.quad	_Mod__Mod_var_i8_L
	.quad	_L_1+100
	.quad	_Mod__uMod_var_i8_L
	.quad	_L_1+113
	.quad	_Mod__Mod_param_i8_i64
	.quad	_L_1+127
	.quad	_Mod__uMod_param_i8_i64
	.quad	_L_1+144
	.quad	_Mod__Mod_var_i8_i64
	.quad	_L_1+162
	.quad	_Mod__uMod_var_i8_i64
	.quad	_L_1+177
	.quad	_Mod__Mod_param_i8_u8
	.quad	_L_1+193
	.quad	_Mod__uMod_param_i8_u8
	.quad	_L_1+209
	.quad	_Mod__Mod_var_i8_u8
	.quad	_L_1+226
	.quad	_Mod__uMod_var_i8_u8
	.quad	_L_1+240
	.quad	_Mod__Mod_param_i8_C
	.quad	_L_1+255
	.quad	_Mod__uMod_param_i8_C
	.quad	_L_1+270
	.quad	_Mod__Mod_var_i8_C
	.quad	_L_1+286
	.quad	_Mod__uMod_var_i8_C
	.quad	_L_1+299
	.quad	_Mod__Mod_param_i8_i32
	.quad	_L_1+313
	.quad	_Mod__uMod_param_i8_i32
	.quad	_L_1+330
	.quad	_Mod__Mod_var_i8_i32
	.quad	_L_1+348
	.quad	_Mod__uMod_var_i8_i32
	.quad	_L_1+363
	.quad	_Mod__Mod_param_i8_u64
	.quad	_L_1+379
	.quad	_Mod__uMod_param_i8_u64
	.quad	_L_1+396
	.quad	_Mod__Mod_var_i8_u64
	.quad	_L_1+414
	.quad	_Mod__uMod_var_i8_u64
	.quad	_L_1+429
	.quad	_Mod__Mod_param_i8_i16
	.quad	_L_1+445
	.quad	_Mod__uMod_param_i8_i16
	.quad	_L_1+462
	.quad	_Mod__Mod_var_i8_i16
	.quad	_L_1+480
	.quad	_Mod__uMod_var_i8_i16
	.quad	_L_1+495
	.quad	_Mod__Mod_param_i8_I
	.quad	_L_1+511
	.quad	_Mod__uMod_param_i8_I
	.quad	_L_1+526
	.quad	_Mod__Mod_var_i8_I
	.quad	_L_1+542
	.quad	_Mod__uMod_var_i8_I
	.quad	_L_1+555
	.quad	_Mod__Mod_param_i8_LC
	.quad	_L_1+569
	.quad	_Mod__uMod_param_i8_LC
	.quad	_L_1+585
	.quad	_Mod__Mod_var_i8_LC
	.quad	_L_1+602
	.quad	_Mod__uMod_var_i8_LC
	.quad	_L_1+616
	.quad	_Mod__Mod_param_i8_u32
	.quad	_L_1+631
	.quad	_Mod__uMod_param_i8_u32
	.quad	_L_1+648
	.quad	_Mod__Mod_var_i8_u32
	.quad	_L_1+666
	.quad	_Mod__uMod_var_i8_u32
	.quad	_L_1+681
	.quad	_Mod__Mod_param_i8_u16
	.quad	_L_1+697
	.quad	_Mod__uMod_param_i8_u16
	.quad	_L_1+714
	.quad	_Mod__Mod_var_i8_u16
	.quad	_L_1+732
	.quad	_Mod__uMod_var_i8_u16
	.quad	_L_1+747
	.quad	_Mod__Mod_param_L_i8
	.quad	_L_1+763
	.quad	_Mod__uMod_param_L_i8
	.quad	_L_1+778
	.quad	_Mod__Mod_var_L_i8
	.quad	_L_1+794
	.quad	_Mod__uMod_var_L_i8
	.quad	_L_1+807
	.quad	_Mod__Mod_param_L_L
	.quad	_L_1+821
	.quad	_Mod__uMod_param_L_L
	.quad	_L_1+835
	.quad	_Mod__Mod_var_L_L
	.quad	_L_1+850
	.quad	_Mod__uMod_var_L_L
	.quad	_L_1+862
	.quad	_Mod__Mod_param_L_i64
	.quad	_L_1+875
	.quad	_Mod__uMod_param_L_i64
	.quad	_L_1+891
	.quad	_Mod__Mod_var_L_i64
	.quad	_L_1+908
	.quad	_Mod__uMod_var_L_i64
	.quad	_L_1+922
	.quad	_Mod__Mod_param_L_u8
	.quad	_L_1+937
	.quad	_Mod__uMod_param_L_u8
	.quad	_L_1+952
	.quad	_Mod__Mod_var_L_u8
	.quad	_L_1+968
	.quad	_Mod__uMod_var_L_u8
	.quad	_L_1+981
	.quad	_Mod__Mod_param_L_C
	.quad	_L_1+995
	.quad	_Mod__uMod_param_L_C
	.quad	_L_1+1009
	.quad	_Mod__Mod_var_L_C
	.quad	_L_1+1024
	.quad	_Mod__uMod_var_L_C
	.quad	_L_1+1036
	.quad	_Mod__Mod_param_L_i32
	.quad	_L_1+1049
	.quad	_Mod__uMod_param_L_i32
	.quad	_L_1+1065
	.quad	_Mod__Mod_var_L_i32
	.quad	_L_1+1082
	.quad	_Mod__uMod_var_L_i32
	.quad	_L_1+1096
	.quad	_Mod__Mod_param_L_u64
	.quad	_L_1+1111
	.quad	_Mod__uMod_param_L_u64
	.quad	_L_1+1127
	.quad	_Mod__Mod_var_L_u64
	.quad	_L_1+1144
	.quad	_Mod__uMod_var_L_u64
	.quad	_L_1+1158
	.quad	_Mod__Mod_param_L_i16
	.quad	_L_1+1173
	.quad	_Mod__uMod_param_L_i16
	.quad	_L_1+1189
	.quad	_Mod__Mod_var_L_i16
	.quad	_L_1+1206
	.quad	_Mod__uMod_var_L_i16
	.quad	_L_1+1220
	.quad	_Mod__Mod_param_L_I
	.quad	_L_1+1235
	.quad	_Mod__uMod_param_L_I
	.quad	_L_1+1249
	.quad	_Mod__Mod_var_L_I
	.quad	_L_1+1264
	.quad	_Mod__uMod_var_L_I
	.quad	_L_1+1276
	.quad	_Mod__Mod_param_L_LC
	.quad	_L_1+1289
	.quad	_Mod__uMod_param_L_LC
	.quad	_L_1+1304
	.quad	_Mod__Mod_var_L_LC
	.quad	_L_1+1320
	.quad	_Mod__uMod_var_L_LC
	.quad	_L_1+1333
	.quad	_Mod__Mod_param_L_u32
	.quad	_L_1+1347
	.quad	_Mod__uMod_param_L_u32
	.quad	_L_1+1363
	.quad	_Mod__Mod_var_L_u32
	.quad	_L_1+1380
	.quad	_Mod__uMod_var_L_u32
	.quad	_L_1+1394
	.quad	_Mod__Mod_param_L_u16
	.quad	_L_1+1409
	.quad	_Mod__uMod_param_L_u16
	.quad	_L_1+1425
	.quad	_Mod__Mod_var_L_u16
	.quad	_L_1+1442
	.quad	_Mod__uMod_var_L_u16
	.quad	_L_1+1456
	.quad	_Mod__Mod_param_f32_f32
	.quad	_L_1+1471
	.quad	_Mod__Mod_var_f32_f32
	.quad	_L_1+1489
	.quad	_Mod__Mod_param_i64_i8
	.quad	_L_1+1505
	.quad	_Mod__uMod_param_i64_i8
	.quad	_L_1+1522
	.quad	_Mod__Mod_var_i64_i8
	.quad	_L_1+1540
	.quad	_Mod__uMod_var_i64_i8
	.quad	_L_1+1555
	.quad	_Mod__Mod_param_i64_L
	.quad	_L_1+1571
	.quad	_Mod__uMod_param_i64_L
	.quad	_L_1+1587
	.quad	_Mod__Mod_var_i64_L
	.quad	_L_1+1604
	.quad	_Mod__uMod_var_i64_L
	.quad	_L_1+1618
	.quad	_Mod__Mod_param_i64_i64
	.quad	_L_1+1633
	.quad	_Mod__uMod_param_i64_i64
	.quad	_L_1+1651
	.quad	_Mod__Mod_var_i64_i64
	.quad	_L_1+1670
	.quad	_Mod__uMod_var_i64_i64
	.quad	_L_1+1686
	.quad	_Mod__Mod_param_i64_u8
	.quad	_L_1+1703
	.quad	_Mod__uMod_param_i64_u8
	.quad	_L_1+1720
	.quad	_Mod__Mod_var_i64_u8
	.quad	_L_1+1738
	.quad	_Mod__uMod_var_i64_u8
	.quad	_L_1+1753
	.quad	_Mod__Mod_param_i64_C
	.quad	_L_1+1769
	.quad	_Mod__uMod_param_i64_C
	.quad	_L_1+1785
	.quad	_Mod__Mod_var_i64_C
	.quad	_L_1+1802
	.quad	_Mod__uMod_var_i64_C
	.quad	_L_1+1816
	.quad	_Mod__Mod_param_i64_i32
	.quad	_L_1+1831
	.quad	_Mod__uMod_param_i64_i32
	.quad	_L_1+1849
	.quad	_Mod__Mod_var_i64_i32
	.quad	_L_1+1868
	.quad	_Mod__uMod_var_i64_i32
	.quad	_L_1+1884
	.quad	_Mod__Mod_param_i64_u64
	.quad	_L_1+1901
	.quad	_Mod__uMod_param_i64_u64
	.quad	_L_1+1919
	.quad	_Mod__Mod_var_i64_u64
	.quad	_L_1+1938
	.quad	_Mod__uMod_var_i64_u64
	.quad	_L_1+1954
	.quad	_Mod__Mod_param_i64_i16
	.quad	_L_1+1971
	.quad	_Mod__uMod_param_i64_i16
	.quad	_L_1+1989
	.quad	_Mod__Mod_var_i64_i16
	.quad	_L_1+2008
	.quad	_Mod__uMod_var_i64_i16
	.quad	_L_1+2024
	.quad	_Mod__Mod_param_i64_I
	.quad	_L_1+2041
	.quad	_Mod__uMod_param_i64_I
	.quad	_L_1+2057
	.quad	_Mod__Mod_var_i64_I
	.quad	_L_1+2074
	.quad	_Mod__uMod_var_i64_I
	.quad	_L_1+2088
	.quad	_Mod__Mod_param_i64_LC
	.quad	_L_1+2103
	.quad	_Mod__uMod_param_i64_LC
	.quad	_L_1+2120
	.quad	_Mod__Mod_var_i64_LC
	.quad	_L_1+2138
	.quad	_Mod__uMod_var_i64_LC
	.quad	_L_1+2153
	.quad	_Mod__Mod_param_i64_u32
	.quad	_L_1+2169
	.quad	_Mod__uMod_param_i64_u32
	.quad	_L_1+2187
	.quad	_Mod__Mod_var_i64_u32
	.quad	_L_1+2206
	.quad	_Mod__uMod_var_i64_u32
	.quad	_L_1+2222
	.quad	_Mod__Mod_param_i64_u16
	.quad	_L_1+2239
	.quad	_Mod__uMod_param_i64_u16
	.quad	_L_1+2257
	.quad	_Mod__Mod_var_i64_u16
	.quad	_L_1+2276
	.quad	_Mod__uMod_var_i64_u16
	.quad	_L_1+2292
	.quad	_Mod__Mod_param_u8_i8
	.quad	_L_1+2309
	.quad	_Mod__uMod_param_u8_i8
	.quad	_L_1+2325
	.quad	_Mod__Mod_var_u8_i8
	.quad	_L_1+2342
	.quad	_Mod__uMod_var_u8_i8
	.quad	_L_1+2356
	.quad	_Mod__Mod_param_u8_L
	.quad	_L_1+2371
	.quad	_Mod__uMod_param_u8_L
	.quad	_L_1+2386
	.quad	_Mod__Mod_var_u8_L
	.quad	_L_1+2402
	.quad	_Mod__uMod_var_u8_L
	.quad	_L_1+2415
	.quad	_Mod__Mod_param_u8_i64
	.quad	_L_1+2429
	.quad	_Mod__uMod_param_u8_i64
	.quad	_L_1+2446
	.quad	_Mod__Mod_var_u8_i64
	.quad	_L_1+2464
	.quad	_Mod__uMod_var_u8_i64
	.quad	_L_1+2479
	.quad	_Mod__Mod_param_u8_u8
	.quad	_L_1+2495
	.quad	_Mod__uMod_param_u8_u8
	.quad	_L_1+2511
	.quad	_Mod__Mod_var_u8_u8
	.quad	_L_1+2528
	.quad	_Mod__uMod_var_u8_u8
	.quad	_L_1+2542
	.quad	_Mod__Mod_param_u8_C
	.quad	_L_1+2557
	.quad	_Mod__uMod_param_u8_C
	.quad	_L_1+2572
	.quad	_Mod__Mod_var_u8_C
	.quad	_L_1+2588
	.quad	_Mod__uMod_var_u8_C
	.quad	_L_1+2601
	.quad	_Mod__Mod_param_u8_i32
	.quad	_L_1+2615
	.quad	_Mod__uMod_param_u8_i32
	.quad	_L_1+2632
	.quad	_Mod__Mod_var_u8_i32
	.quad	_L_1+2650
	.quad	_Mod__uMod_var_u8_i32
	.quad	_L_1+2665
	.quad	_Mod__Mod_param_u8_u64
	.quad	_L_1+2681
	.quad	_Mod__uMod_param_u8_u64
	.quad	_L_1+2698
	.quad	_Mod__Mod_var_u8_u64
	.quad	_L_1+2716
	.quad	_Mod__uMod_var_u8_u64
	.quad	_L_1+2731
	.quad	_Mod__Mod_param_u8_i16
	.quad	_L_1+2747
	.quad	_Mod__uMod_param_u8_i16
	.quad	_L_1+2764
	.quad	_Mod__Mod_var_u8_i16
	.quad	_L_1+2782
	.quad	_Mod__uMod_var_u8_i16
	.quad	_L_1+2797
	.quad	_Mod__Mod_param_u8_I
	.quad	_L_1+2813
	.quad	_Mod__uMod_param_u8_I
	.quad	_L_1+2828
	.quad	_Mod__Mod_var_u8_I
	.quad	_L_1+2844
	.quad	_Mod__uMod_var_u8_I
	.quad	_L_1+2857
	.quad	_Mod__Mod_param_u8_LC
	.quad	_L_1+2871
	.quad	_Mod__uMod_param_u8_LC
	.quad	_L_1+2887
	.quad	_Mod__Mod_var_u8_LC
	.quad	_L_1+2904
	.quad	_Mod__uMod_var_u8_LC
	.quad	_L_1+2918
	.quad	_Mod__Mod_param_u8_u32
	.quad	_L_1+2933
	.quad	_Mod__uMod_param_u8_u32
	.quad	_L_1+2950
	.quad	_Mod__Mod_var_u8_u32
	.quad	_L_1+2968
	.quad	_Mod__uMod_var_u8_u32
	.quad	_L_1+2983
	.quad	_Mod__Mod_param_u8_u16
	.quad	_L_1+2999
	.quad	_Mod__uMod_param_u8_u16
	.quad	_L_1+3016
	.quad	_Mod__Mod_var_u8_u16
	.quad	_L_1+3034
	.quad	_Mod__uMod_var_u8_u16
	.quad	_L_1+3049
	.quad	_Mod__Mod_param_C_i8
	.quad	_L_1+3065
	.quad	_Mod__uMod_param_C_i8
	.quad	_L_1+3080
	.quad	_Mod__Mod_var_C_i8
	.quad	_L_1+3096
	.quad	_Mod__uMod_var_C_i8
	.quad	_L_1+3109
	.quad	_Mod__Mod_param_C_L
	.quad	_L_1+3123
	.quad	_Mod__uMod_param_C_L
	.quad	_L_1+3137
	.quad	_Mod__Mod_var_C_L
	.quad	_L_1+3152
	.quad	_Mod__uMod_var_C_L
	.quad	_L_1+3164
	.quad	_Mod__Mod_param_C_i64
	.quad	_L_1+3177
	.quad	_Mod__uMod_param_C_i64
	.quad	_L_1+3193
	.quad	_Mod__Mod_var_C_i64
	.quad	_L_1+3210
	.quad	_Mod__uMod_var_C_i64
	.quad	_L_1+3224
	.quad	_Mod__Mod_param_C_u8
	.quad	_L_1+3239
	.quad	_Mod__uMod_param_C_u8
	.quad	_L_1+3254
	.quad	_Mod__Mod_var_C_u8
	.quad	_L_1+3270
	.quad	_Mod__uMod_var_C_u8
	.quad	_L_1+3283
	.quad	_Mod__Mod_param_C_C
	.quad	_L_1+3297
	.quad	_Mod__uMod_param_C_C
	.quad	_L_1+3311
	.quad	_Mod__Mod_var_C_C
	.quad	_L_1+3326
	.quad	_Mod__uMod_var_C_C
	.quad	_L_1+3338
	.quad	_Mod__Mod_param_C_i32
	.quad	_L_1+3351
	.quad	_Mod__uMod_param_C_i32
	.quad	_L_1+3367
	.quad	_Mod__Mod_var_C_i32
	.quad	_L_1+3384
	.quad	_Mod__uMod_var_C_i32
	.quad	_L_1+3398
	.quad	_Mod__Mod_param_C_u64
	.quad	_L_1+3413
	.quad	_Mod__uMod_param_C_u64
	.quad	_L_1+3429
	.quad	_Mod__Mod_var_C_u64
	.quad	_L_1+3446
	.quad	_Mod__uMod_var_C_u64
	.quad	_L_1+3460
	.quad	_Mod__Mod_param_C_i16
	.quad	_L_1+3475
	.quad	_Mod__uMod_param_C_i16
	.quad	_L_1+3491
	.quad	_Mod__Mod_var_C_i16
	.quad	_L_1+3508
	.quad	_Mod__uMod_var_C_i16
	.quad	_L_1+3522
	.quad	_Mod__Mod_param_C_I
	.quad	_L_1+3537
	.quad	_Mod__uMod_param_C_I
	.quad	_L_1+3551
	.quad	_Mod__Mod_var_C_I
	.quad	_L_1+3566
	.quad	_Mod__uMod_var_C_I
	.quad	_L_1+3578
	.quad	_Mod__Mod_param_C_LC
	.quad	_L_1+3591
	.quad	_Mod__uMod_param_C_LC
	.quad	_L_1+3606
	.quad	_Mod__Mod_var_C_LC
	.quad	_L_1+3622
	.quad	_Mod__uMod_var_C_LC
	.quad	_L_1+3635
	.quad	_Mod__Mod_param_C_u32
	.quad	_L_1+3649
	.quad	_Mod__uMod_param_C_u32
	.quad	_L_1+3665
	.quad	_Mod__Mod_var_C_u32
	.quad	_L_1+3682
	.quad	_Mod__uMod_var_C_u32
	.quad	_L_1+3696
	.quad	_Mod__Mod_param_C_u16
	.quad	_L_1+3711
	.quad	_Mod__uMod_param_C_u16
	.quad	_L_1+3727
	.quad	_Mod__Mod_var_C_u16
	.quad	_L_1+3744
	.quad	_Mod__uMod_var_C_u16
	.quad	_L_1+3758
	.quad	_Mod__Mod_param_i32_i8
	.quad	_L_1+3773
	.quad	_Mod__uMod_param_i32_i8
	.quad	_L_1+3790
	.quad	_Mod__Mod_var_i32_i8
	.quad	_L_1+3808
	.quad	_Mod__uMod_var_i32_i8
	.quad	_L_1+3823
	.quad	_Mod__Mod_param_i32_L
	.quad	_L_1+3839
	.quad	_Mod__uMod_param_i32_L
	.quad	_L_1+3855
	.quad	_Mod__Mod_var_i32_L
	.quad	_L_1+3872
	.quad	_Mod__uMod_var_i32_L
	.quad	_L_1+3886
	.quad	_Mod__Mod_param_i32_i64
	.quad	_L_1+3901
	.quad	_Mod__uMod_param_i32_i64
	.quad	_L_1+3919
	.quad	_Mod__Mod_var_i32_i64
	.quad	_L_1+3938
	.quad	_Mod__uMod_var_i32_i64
	.quad	_L_1+3954
	.quad	_Mod__Mod_param_i32_u8
	.quad	_L_1+3971
	.quad	_Mod__uMod_param_i32_u8
	.quad	_L_1+3988
	.quad	_Mod__Mod_var_i32_u8
	.quad	_L_1+4006
	.quad	_Mod__uMod_var_i32_u8
	.quad	_L_1+4021
	.quad	_Mod__Mod_param_i32_C
	.quad	_L_1+4037
	.quad	_Mod__uMod_param_i32_C
	.quad	_L_1+4053
	.quad	_Mod__Mod_var_i32_C
	.quad	_L_1+4070
	.quad	_Mod__uMod_var_i32_C
	.quad	_L_1+4084
	.quad	_Mod__Mod_param_i32_i32
	.quad	_L_1+4099
	.quad	_Mod__uMod_param_i32_i32
	.quad	_L_1+4117
	.quad	_Mod__Mod_var_i32_i32
	.quad	_L_1+4136
	.quad	_Mod__uMod_var_i32_i32
	.quad	_L_1+4152
	.quad	_Mod__Mod_param_i32_u64
	.quad	_L_1+4169
	.quad	_Mod__uMod_param_i32_u64
	.quad	_L_1+4187
	.quad	_Mod__Mod_var_i32_u64
	.quad	_L_1+4206
	.quad	_Mod__uMod_var_i32_u64
	.quad	_L_1+4222
	.quad	_Mod__Mod_param_i32_i16
	.quad	_L_1+4239
	.quad	_Mod__uMod_param_i32_i16
	.quad	_L_1+4257
	.quad	_Mod__Mod_var_i32_i16
	.quad	_L_1+4276
	.quad	_Mod__uMod_var_i32_i16
	.quad	_L_1+4292
	.quad	_Mod__Mod_param_i32_I
	.quad	_L_1+4309
	.quad	_Mod__uMod_param_i32_I
	.quad	_L_1+4325
	.quad	_Mod__Mod_var_i32_I
	.quad	_L_1+4342
	.quad	_Mod__uMod_var_i32_I
	.quad	_L_1+4356
	.quad	_Mod__Mod_param_i32_LC
	.quad	_L_1+4371
	.quad	_Mod__uMod_param_i32_LC
	.quad	_L_1+4388
	.quad	_Mod__Mod_var_i32_LC
	.quad	_L_1+4406
	.quad	_Mod__uMod_var_i32_LC
	.quad	_L_1+4421
	.quad	_Mod__Mod_param_i32_u32
	.quad	_L_1+4437
	.quad	_Mod__uMod_param_i32_u32
	.quad	_L_1+4455
	.quad	_Mod__Mod_var_i32_u32
	.quad	_L_1+4474
	.quad	_Mod__uMod_var_i32_u32
	.quad	_L_1+4490
	.quad	_Mod__Mod_param_i32_u16
	.quad	_L_1+4507
	.quad	_Mod__uMod_param_i32_u16
	.quad	_L_1+4525
	.quad	_Mod__Mod_var_i32_u16
	.quad	_L_1+4544
	.quad	_Mod__uMod_var_i32_u16
	.quad	_L_1+4560
	.quad	_Mod__Mod_param_f64_f64
	.quad	_L_1+4577
	.quad	_Mod__Mod_var_f64_f64
	.quad	_L_1+4595
	.quad	_Mod__Mod_param_u64_i8
	.quad	_L_1+4611
	.quad	_Mod__uMod_param_u64_i8
	.quad	_L_1+4628
	.quad	_Mod__Mod_var_u64_i8
	.quad	_L_1+4646
	.quad	_Mod__uMod_var_u64_i8
	.quad	_L_1+4661
	.quad	_Mod__Mod_param_u64_L
	.quad	_L_1+4677
	.quad	_Mod__uMod_param_u64_L
	.quad	_L_1+4693
	.quad	_Mod__Mod_var_u64_L
	.quad	_L_1+4710
	.quad	_Mod__uMod_var_u64_L
	.quad	_L_1+4724
	.quad	_Mod__Mod_param_u64_i64
	.quad	_L_1+4739
	.quad	_Mod__uMod_param_u64_i64
	.quad	_L_1+4757
	.quad	_Mod__Mod_var_u64_i64
	.quad	_L_1+4776
	.quad	_Mod__uMod_var_u64_i64
	.quad	_L_1+4792
	.quad	_Mod__Mod_param_u64_u8
	.quad	_L_1+4809
	.quad	_Mod__uMod_param_u64_u8
	.quad	_L_1+4826
	.quad	_Mod__Mod_var_u64_u8
	.quad	_L_1+4844
	.quad	_Mod__uMod_var_u64_u8
	.quad	_L_1+4859
	.quad	_Mod__Mod_param_u64_C
	.quad	_L_1+4875
	.quad	_Mod__uMod_param_u64_C
	.quad	_L_1+4891
	.quad	_Mod__Mod_var_u64_C
	.quad	_L_1+4908
	.quad	_Mod__uMod_var_u64_C
	.quad	_L_1+4922
	.quad	_Mod__Mod_param_u64_i32
	.quad	_L_1+4937
	.quad	_Mod__uMod_param_u64_i32
	.quad	_L_1+4955
	.quad	_Mod__Mod_var_u64_i32
	.quad	_L_1+4974
	.quad	_Mod__uMod_var_u64_i32
	.quad	_L_1+4990
	.quad	_Mod__Mod_param_u64_u64
	.quad	_L_1+5007
	.quad	_Mod__uMod_param_u64_u64
	.quad	_L_1+5025
	.quad	_Mod__Mod_var_u64_u64
	.quad	_L_1+5044
	.quad	_Mod__uMod_var_u64_u64
	.quad	_L_1+5060
	.quad	_Mod__Mod_param_u64_i16
	.quad	_L_1+5077
	.quad	_Mod__uMod_param_u64_i16
	.quad	_L_1+5095
	.quad	_Mod__Mod_var_u64_i16
	.quad	_L_1+5114
	.quad	_Mod__uMod_var_u64_i16
	.quad	_L_1+5130
	.quad	_Mod__Mod_param_u64_I
	.quad	_L_1+5147
	.quad	_Mod__uMod_param_u64_I
	.quad	_L_1+5163
	.quad	_Mod__Mod_var_u64_I
	.quad	_L_1+5180
	.quad	_Mod__uMod_var_u64_I
	.quad	_L_1+5194
	.quad	_Mod__Mod_param_u64_LC
	.quad	_L_1+5209
	.quad	_Mod__uMod_param_u64_LC
	.quad	_L_1+5226
	.quad	_Mod__Mod_var_u64_LC
	.quad	_L_1+5244
	.quad	_Mod__uMod_var_u64_LC
	.quad	_L_1+5259
	.quad	_Mod__Mod_param_u64_u32
	.quad	_L_1+5275
	.quad	_Mod__uMod_param_u64_u32
	.quad	_L_1+5293
	.quad	_Mod__Mod_var_u64_u32
	.quad	_L_1+5312
	.quad	_Mod__uMod_var_u64_u32
	.quad	_L_1+5328
	.quad	_Mod__Mod_param_u64_u16
	.quad	_L_1+5345
	.quad	_Mod__uMod_param_u64_u16
	.quad	_L_1+5363
	.quad	_Mod__Mod_var_u64_u16
	.quad	_L_1+5382
	.quad	_Mod__uMod_var_u64_u16
	.quad	_L_1+5398
	.quad	_Mod__Mod_param_i16_i8
	.quad	_L_1+5415
	.quad	_Mod__uMod_param_i16_i8
	.quad	_L_1+5432
	.quad	_Mod__Mod_var_i16_i8
	.quad	_L_1+5450
	.quad	_Mod__uMod_var_i16_i8
	.quad	_L_1+5465
	.quad	_Mod__Mod_param_i16_L
	.quad	_L_1+5481
	.quad	_Mod__uMod_param_i16_L
	.quad	_L_1+5497
	.quad	_Mod__Mod_var_i16_L
	.quad	_L_1+5514
	.quad	_Mod__uMod_var_i16_L
	.quad	_L_1+5528
	.quad	_Mod__Mod_param_i16_i64
	.quad	_L_1+5543
	.quad	_Mod__uMod_param_i16_i64
	.quad	_L_1+5561
	.quad	_Mod__Mod_var_i16_i64
	.quad	_L_1+5580
	.quad	_Mod__uMod_var_i16_i64
	.quad	_L_1+5596
	.quad	_Mod__Mod_param_i16_u8
	.quad	_L_1+5613
	.quad	_Mod__uMod_param_i16_u8
	.quad	_L_1+5630
	.quad	_Mod__Mod_var_i16_u8
	.quad	_L_1+5648
	.quad	_Mod__uMod_var_i16_u8
	.quad	_L_1+5663
	.quad	_Mod__Mod_param_i16_C
	.quad	_L_1+5679
	.quad	_Mod__uMod_param_i16_C
	.quad	_L_1+5695
	.quad	_Mod__Mod_var_i16_C
	.quad	_L_1+5712
	.quad	_Mod__uMod_var_i16_C
	.quad	_L_1+5726
	.quad	_Mod__Mod_param_i16_i32
	.quad	_L_1+5741
	.quad	_Mod__uMod_param_i16_i32
	.quad	_L_1+5759
	.quad	_Mod__Mod_var_i16_i32
	.quad	_L_1+5778
	.quad	_Mod__uMod_var_i16_i32
	.quad	_L_1+5794
	.quad	_Mod__Mod_param_i16_u64
	.quad	_L_1+5811
	.quad	_Mod__uMod_param_i16_u64
	.quad	_L_1+5829
	.quad	_Mod__Mod_var_i16_u64
	.quad	_L_1+5848
	.quad	_Mod__uMod_var_i16_u64
	.quad	_L_1+5864
	.quad	_Mod__Mod_param_i16_i16
	.quad	_L_1+5881
	.quad	_Mod__uMod_param_i16_i16
	.quad	_L_1+5899
	.quad	_Mod__Mod_var_i16_i16
	.quad	_L_1+5918
	.quad	_Mod__uMod_var_i16_i16
	.quad	_L_1+5934
	.quad	_Mod__Mod_param_i16_I
	.quad	_L_1+5951
	.quad	_Mod__uMod_param_i16_I
	.quad	_L_1+5967
	.quad	_Mod__Mod_var_i16_I
	.quad	_L_1+5984
	.quad	_Mod__uMod_var_i16_I
	.quad	_L_1+5998
	.quad	_Mod__Mod_param_i16_LC
	.quad	_L_1+6013
	.quad	_Mod__uMod_param_i16_LC
	.quad	_L_1+6030
	.quad	_Mod__Mod_var_i16_LC
	.quad	_L_1+6048
	.quad	_Mod__uMod_var_i16_LC
	.quad	_L_1+6063
	.quad	_Mod__Mod_param_i16_u32
	.quad	_L_1+6079
	.quad	_Mod__uMod_param_i16_u32
	.quad	_L_1+6097
	.quad	_Mod__Mod_var_i16_u32
	.quad	_L_1+6116
	.quad	_Mod__uMod_var_i16_u32
	.quad	_L_1+6132
	.quad	_Mod__Mod_param_i16_u16
	.quad	_L_1+6149
	.quad	_Mod__uMod_param_i16_u16
	.quad	_L_1+6167
	.quad	_Mod__Mod_var_i16_u16
	.quad	_L_1+6186
	.quad	_Mod__uMod_var_i16_u16
	.quad	_L_1+6202
	.quad	_Mod__Mod_param_I_i8
	.quad	_L_1+6219
	.quad	_Mod__uMod_param_I_i8
	.quad	_L_1+6234
	.quad	_Mod__Mod_var_I_i8
	.quad	_L_1+6250
	.quad	_Mod__uMod_var_I_i8
	.quad	_L_1+6263
	.quad	_Mod__Mod_param_I_L
	.quad	_L_1+6277
	.quad	_Mod__uMod_param_I_L
	.quad	_L_1+6291
	.quad	_Mod__Mod_var_I_L
	.quad	_L_1+6306
	.quad	_Mod__uMod_var_I_L
	.quad	_L_1+6318
	.quad	_Mod__Mod_param_I_i64
	.quad	_L_1+6331
	.quad	_Mod__uMod_param_I_i64
	.quad	_L_1+6347
	.quad	_Mod__Mod_var_I_i64
	.quad	_L_1+6364
	.quad	_Mod__uMod_var_I_i64
	.quad	_L_1+6378
	.quad	_Mod__Mod_param_I_u8
	.quad	_L_1+6393
	.quad	_Mod__uMod_param_I_u8
	.quad	_L_1+6408
	.quad	_Mod__Mod_var_I_u8
	.quad	_L_1+6424
	.quad	_Mod__uMod_var_I_u8
	.quad	_L_1+6437
	.quad	_Mod__Mod_param_I_C
	.quad	_L_1+6451
	.quad	_Mod__uMod_param_I_C
	.quad	_L_1+6465
	.quad	_Mod__Mod_var_I_C
	.quad	_L_1+6480
	.quad	_Mod__uMod_var_I_C
	.quad	_L_1+6492
	.quad	_Mod__Mod_param_I_i32
	.quad	_L_1+6505
	.quad	_Mod__uMod_param_I_i32
	.quad	_L_1+6521
	.quad	_Mod__Mod_var_I_i32
	.quad	_L_1+6538
	.quad	_Mod__uMod_var_I_i32
	.quad	_L_1+6552
	.quad	_Mod__Mod_param_I_u64
	.quad	_L_1+6567
	.quad	_Mod__uMod_param_I_u64
	.quad	_L_1+6583
	.quad	_Mod__Mod_var_I_u64
	.quad	_L_1+6600
	.quad	_Mod__uMod_var_I_u64
	.quad	_L_1+6614
	.quad	_Mod__Mod_param_I_i16
	.quad	_L_1+6629
	.quad	_Mod__uMod_param_I_i16
	.quad	_L_1+6645
	.quad	_Mod__Mod_var_I_i16
	.quad	_L_1+6662
	.quad	_Mod__uMod_var_I_i16
	.quad	_L_1+6676
	.quad	_Mod__Mod_param_I_I
	.quad	_L_1+6691
	.quad	_Mod__uMod_param_I_I
	.quad	_L_1+6705
	.quad	_Mod__Mod_var_I_I
	.quad	_L_1+6720
	.quad	_Mod__uMod_var_I_I
	.quad	_L_1+6732
	.quad	_Mod__Mod_param_I_LC
	.quad	_L_1+6745
	.quad	_Mod__uMod_param_I_LC
	.quad	_L_1+6760
	.quad	_Mod__Mod_var_I_LC
	.quad	_L_1+6776
	.quad	_Mod__uMod_var_I_LC
	.quad	_L_1+6789
	.quad	_Mod__Mod_param_I_u32
	.quad	_L_1+6803
	.quad	_Mod__uMod_param_I_u32
	.quad	_L_1+6819
	.quad	_Mod__Mod_var_I_u32
	.quad	_L_1+6836
	.quad	_Mod__uMod_var_I_u32
	.quad	_L_1+6850
	.quad	_Mod__Mod_param_I_u16
	.quad	_L_1+6865
	.quad	_Mod__uMod_param_I_u16
	.quad	_L_1+6881
	.quad	_Mod__Mod_var_I_u16
	.quad	_L_1+6898
	.quad	_Mod__uMod_var_I_u16
	.quad	_L_1+6912
	.quad	_Mod__Mod_param_LC_i8
	.quad	_L_1+6927
	.quad	_Mod__uMod_param_LC_i8
	.quad	_L_1+6943
	.quad	_Mod__Mod_var_LC_i8
	.quad	_L_1+6960
	.quad	_Mod__uMod_var_LC_i8
	.quad	_L_1+6974
	.quad	_Mod__Mod_param_LC_L
	.quad	_L_1+6989
	.quad	_Mod__uMod_param_LC_L
	.quad	_L_1+7004
	.quad	_Mod__Mod_var_LC_L
	.quad	_L_1+7020
	.quad	_Mod__uMod_var_LC_L
	.quad	_L_1+7033
	.quad	_Mod__Mod_param_LC_i64
	.quad	_L_1+7047
	.quad	_Mod__uMod_param_LC_i64
	.quad	_L_1+7064
	.quad	_Mod__Mod_var_LC_i64
	.quad	_L_1+7082
	.quad	_Mod__uMod_var_LC_i64
	.quad	_L_1+7097
	.quad	_Mod__Mod_param_LC_u8
	.quad	_L_1+7113
	.quad	_Mod__uMod_param_LC_u8
	.quad	_L_1+7129
	.quad	_Mod__Mod_var_LC_u8
	.quad	_L_1+7146
	.quad	_Mod__uMod_var_LC_u8
	.quad	_L_1+7160
	.quad	_Mod__Mod_param_LC_C
	.quad	_L_1+7175
	.quad	_Mod__uMod_param_LC_C
	.quad	_L_1+7190
	.quad	_Mod__Mod_var_LC_C
	.quad	_L_1+7206
	.quad	_Mod__uMod_var_LC_C
	.quad	_L_1+7219
	.quad	_Mod__Mod_param_LC_i32
	.quad	_L_1+7233
	.quad	_Mod__uMod_param_LC_i32
	.quad	_L_1+7250
	.quad	_Mod__Mod_var_LC_i32
	.quad	_L_1+7268
	.quad	_Mod__uMod_var_LC_i32
	.quad	_L_1+7283
	.quad	_Mod__Mod_param_LC_u64
	.quad	_L_1+7299
	.quad	_Mod__uMod_param_LC_u64
	.quad	_L_1+7316
	.quad	_Mod__Mod_var_LC_u64
	.quad	_L_1+7334
	.quad	_Mod__uMod_var_LC_u64
	.quad	_L_1+7349
	.quad	_Mod__Mod_param_LC_i16
	.quad	_L_1+7365
	.quad	_Mod__uMod_param_LC_i16
	.quad	_L_1+7382
	.quad	_Mod__Mod_var_LC_i16
	.quad	_L_1+7400
	.quad	_Mod__uMod_var_LC_i16
	.quad	_L_1+7415
	.quad	_Mod__Mod_param_LC_I
	.quad	_L_1+7431
	.quad	_Mod__uMod_param_LC_I
	.quad	_L_1+7446
	.quad	_Mod__Mod_var_LC_I
	.quad	_L_1+7462
	.quad	_Mod__uMod_var_LC_I
	.quad	_L_1+7475
	.quad	_Mod__Mod_param_LC_LC
	.quad	_L_1+7489
	.quad	_Mod__uMod_param_LC_LC
	.quad	_L_1+7505
	.quad	_Mod__Mod_var_LC_LC
	.quad	_L_1+7522
	.quad	_Mod__uMod_var_LC_LC
	.quad	_L_1+7536
	.quad	_Mod__Mod_param_LC_u32
	.quad	_L_1+7551
	.quad	_Mod__uMod_param_LC_u32
	.quad	_L_1+7568
	.quad	_Mod__Mod_var_LC_u32
	.quad	_L_1+7586
	.quad	_Mod__uMod_var_LC_u32
	.quad	_L_1+7601
	.quad	_Mod__Mod_param_LC_u16
	.quad	_L_1+7617
	.quad	_Mod__uMod_param_LC_u16
	.quad	_L_1+7634
	.quad	_Mod__Mod_var_LC_u16
	.quad	_L_1+7652
	.quad	_Mod__uMod_var_LC_u16
	.quad	_L_1+7667
	.quad	_Mod__Mod_param_u32_i8
	.quad	_L_1+7683
	.quad	_Mod__uMod_param_u32_i8
	.quad	_L_1+7700
	.quad	_Mod__Mod_var_u32_i8
	.quad	_L_1+7718
	.quad	_Mod__uMod_var_u32_i8
	.quad	_L_1+7733
	.quad	_Mod__Mod_param_u32_L
	.quad	_L_1+7749
	.quad	_Mod__uMod_param_u32_L
	.quad	_L_1+7765
	.quad	_Mod__Mod_var_u32_L
	.quad	_L_1+7782
	.quad	_Mod__uMod_var_u32_L
	.quad	_L_1+7796
	.quad	_Mod__Mod_param_u32_i64
	.quad	_L_1+7811
	.quad	_Mod__uMod_param_u32_i64
	.quad	_L_1+7829
	.quad	_Mod__Mod_var_u32_i64
	.quad	_L_1+7848
	.quad	_Mod__uMod_var_u32_i64
	.quad	_L_1+7864
	.quad	_Mod__Mod_param_u32_u8
	.quad	_L_1+7881
	.quad	_Mod__uMod_param_u32_u8
	.quad	_L_1+7898
	.quad	_Mod__Mod_var_u32_u8
	.quad	_L_1+7916
	.quad	_Mod__uMod_var_u32_u8
	.quad	_L_1+7931
	.quad	_Mod__Mod_param_u32_C
	.quad	_L_1+7947
	.quad	_Mod__uMod_param_u32_C
	.quad	_L_1+7963
	.quad	_Mod__Mod_var_u32_C
	.quad	_L_1+7980
	.quad	_Mod__uMod_var_u32_C
	.quad	_L_1+7994
	.quad	_Mod__Mod_param_u32_i32
	.quad	_L_1+8009
	.quad	_Mod__uMod_param_u32_i32
	.quad	_L_1+8027
	.quad	_Mod__Mod_var_u32_i32
	.quad	_L_1+8046
	.quad	_Mod__uMod_var_u32_i32
	.quad	_L_1+8062
	.quad	_Mod__Mod_param_u32_u64
	.quad	_L_1+8079
	.quad	_Mod__uMod_param_u32_u64
	.quad	_L_1+8097
	.quad	_Mod__Mod_var_u32_u64
	.quad	_L_1+8116
	.quad	_Mod__uMod_var_u32_u64
	.quad	_L_1+8132
	.quad	_Mod__Mod_param_u32_i16
	.quad	_L_1+8149
	.quad	_Mod__uMod_param_u32_i16
	.quad	_L_1+8167
	.quad	_Mod__Mod_var_u32_i16
	.quad	_L_1+8186
	.quad	_Mod__uMod_var_u32_i16
	.quad	_L_1+8202
	.quad	_Mod__Mod_param_u32_I
	.quad	_L_1+8219
	.quad	_Mod__uMod_param_u32_I
	.quad	_L_1+8235
	.quad	_Mod__Mod_var_u32_I
	.quad	_L_1+8252
	.quad	_Mod__uMod_var_u32_I
	.quad	_L_1+8266
	.quad	_Mod__Mod_param_u32_LC
	.quad	_L_1+8281
	.quad	_Mod__uMod_param_u32_LC
	.quad	_L_1+8298
	.quad	_Mod__Mod_var_u32_LC
	.quad	_L_1+8316
	.quad	_Mod__uMod_var_u32_LC
	.quad	_L_1+8331
	.quad	_Mod__Mod_param_u32_u32
	.quad	_L_1+8347
	.quad	_Mod__uMod_param_u32_u32
	.quad	_L_1+8365
	.quad	_Mod__Mod_var_u32_u32
	.quad	_L_1+8384
	.quad	_Mod__uMod_var_u32_u32
	.quad	_L_1+8400
	.quad	_Mod__Mod_param_u32_u16
	.quad	_L_1+8417
	.quad	_Mod__uMod_param_u32_u16
	.quad	_L_1+8435
	.quad	_Mod__Mod_var_u32_u16
	.quad	_L_1+8454
	.quad	_Mod__uMod_var_u32_u16
	.quad	_L_1+8470
	.quad	_Mod__Mod_param_u16_i8
	.quad	_L_1+8487
	.quad	_Mod__uMod_param_u16_i8
	.quad	_L_1+8504
	.quad	_Mod__Mod_var_u16_i8
	.quad	_L_1+8522
	.quad	_Mod__uMod_var_u16_i8
	.quad	_L_1+8537
	.quad	_Mod__Mod_param_u16_L
	.quad	_L_1+8553
	.quad	_Mod__uMod_param_u16_L
	.quad	_L_1+8569
	.quad	_Mod__Mod_var_u16_L
	.quad	_L_1+8586
	.quad	_Mod__uMod_var_u16_L
	.quad	_L_1+8600
	.quad	_Mod__Mod_param_u16_i64
	.quad	_L_1+8615
	.quad	_Mod__uMod_param_u16_i64
	.quad	_L_1+8633
	.quad	_Mod__Mod_var_u16_i64
	.quad	_L_1+8652
	.quad	_Mod__uMod_var_u16_i64
	.quad	_L_1+8668
	.quad	_Mod__Mod_param_u16_u8
	.quad	_L_1+8685
	.quad	_Mod__uMod_param_u16_u8
	.quad	_L_1+8702
	.quad	_Mod__Mod_var_u16_u8
	.quad	_L_1+8720
	.quad	_Mod__uMod_var_u16_u8
	.quad	_L_1+8735
	.quad	_Mod__Mod_param_u16_C
	.quad	_L_1+8751
	.quad	_Mod__uMod_param_u16_C
	.quad	_L_1+8767
	.quad	_Mod__Mod_var_u16_C
	.quad	_L_1+8784
	.quad	_Mod__uMod_var_u16_C
	.quad	_L_1+8798
	.quad	_Mod__Mod_param_u16_i32
	.quad	_L_1+8813
	.quad	_Mod__uMod_param_u16_i32
	.quad	_L_1+8831
	.quad	_Mod__Mod_var_u16_i32
	.quad	_L_1+8850
	.quad	_Mod__uMod_var_u16_i32
	.quad	_L_1+8866
	.quad	_Mod__Mod_param_u16_u64
	.quad	_L_1+8883
	.quad	_Mod__uMod_param_u16_u64
	.quad	_L_1+8901
	.quad	_Mod__Mod_var_u16_u64
	.quad	_L_1+8920
	.quad	_Mod__uMod_var_u16_u64
	.quad	_L_1+8936
	.quad	_Mod__Mod_param_u16_i16
	.quad	_L_1+8953
	.quad	_Mod__uMod_param_u16_i16
	.quad	_L_1+8971
	.quad	_Mod__Mod_var_u16_i16
	.quad	_L_1+8990
	.quad	_Mod__uMod_var_u16_i16
	.quad	_L_1+9006
	.quad	_Mod__Mod_param_u16_I
	.quad	_L_1+9023
	.quad	_Mod__uMod_param_u16_I
	.quad	_L_1+9039
	.quad	_Mod__Mod_var_u16_I
	.quad	_L_1+9056
	.quad	_Mod__uMod_var_u16_I
	.quad	_L_1+9070
	.quad	_Mod__Mod_param_u16_LC
	.quad	_L_1+9085
	.quad	_Mod__uMod_param_u16_LC
	.quad	_L_1+9102
	.quad	_Mod__Mod_var_u16_LC
	.quad	_L_1+9120
	.quad	_Mod__uMod_var_u16_LC
	.quad	_L_1+9135
	.quad	_Mod__Mod_param_u16_u32
	.quad	_L_1+9151
	.quad	_Mod__uMod_param_u16_u32
	.quad	_L_1+9169
	.quad	_Mod__Mod_var_u16_u32
	.quad	_L_1+9188
	.quad	_Mod__uMod_var_u16_u32
	.quad	_L_1+9204
	.quad	_Mod__Mod_param_u16_u16
	.quad	_L_1+9221
	.quad	_Mod__uMod_param_u16_u16
	.quad	_L_1+9239
	.quad	_Mod__Mod_var_u16_u16
	.quad	_L_1+9258
	.quad	_Mod__uMod_var_u16_u16
	.quad	_L_1+9274
	.space 8
	.ascii "../AMD64_DARWIN/Mod.m3"
	.space 2
	.data
	.align 5
_MM_Mod:
	.quad	_L_1+18600
	.space 32
	.quad	_L_1+9296
	.space 24
	.quad	_MM_Mod+224
	.space 8
	.quad	_Mod_M3
	.quad	3
	.word	273
	.space 2
	.long	274
	.quad	275
	.quad	276
	.word	277
	.space 6
	.quad	278
	.long	-515396076
	.long	1081177210
	.long	281
	.space 4
	.quad	282
	.byte	27
	.space 7
	.quad	284
	.long	1133421724
	.space 4
	.quad	287
	.byte	32
	.space 31
	.quad	_Mod_I3
	.quad	_MM_Mod+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Mod+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Mod+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Mod+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Mod__uMod_var_u16_u16.eh = 0
.no_dead_strip _Mod__uMod_var_u16_u16.eh
_Mod__Mod_var_u16_u16.eh = 0
.no_dead_strip _Mod__Mod_var_u16_u16.eh
_Mod__uMod_param_u16_u16.eh = 0
.no_dead_strip _Mod__uMod_param_u16_u16.eh
_Mod__Mod_param_u16_u16.eh = 0
.no_dead_strip _Mod__Mod_param_u16_u16.eh
_Mod__uMod_var_u16_u32.eh = 0
.no_dead_strip _Mod__uMod_var_u16_u32.eh
_Mod__uMod_param_u16_u32.eh = 0
.no_dead_strip _Mod__uMod_param_u16_u32.eh
_Mod__uMod_var_u16_LC.eh = 0
.no_dead_strip _Mod__uMod_var_u16_LC.eh
_Mod__Mod_var_u16_LC.eh = 0
.no_dead_strip _Mod__Mod_var_u16_LC.eh
_Mod__uMod_param_u16_LC.eh = 0
.no_dead_strip _Mod__uMod_param_u16_LC.eh
_Mod__Mod_param_u16_LC.eh = 0
.no_dead_strip _Mod__Mod_param_u16_LC.eh
_Mod__uMod_var_u16_I.eh = 0
.no_dead_strip _Mod__uMod_var_u16_I.eh
_Mod__uMod_param_u16_I.eh = 0
.no_dead_strip _Mod__uMod_param_u16_I.eh
_Mod__uMod_var_u16_i16.eh = 0
.no_dead_strip _Mod__uMod_var_u16_i16.eh
_Mod__uMod_param_u16_i16.eh = 0
.no_dead_strip _Mod__uMod_param_u16_i16.eh
_Mod__uMod_var_u16_u64.eh = 0
.no_dead_strip _Mod__uMod_var_u16_u64.eh
_Mod__Mod_var_u16_u64.eh = 0
.no_dead_strip _Mod__Mod_var_u16_u64.eh
_Mod__uMod_param_u16_u64.eh = 0
.no_dead_strip _Mod__uMod_param_u16_u64.eh
_Mod__Mod_param_u16_u64.eh = 0
.no_dead_strip _Mod__Mod_param_u16_u64.eh
_Mod__uMod_var_u16_i32.eh = 0
.no_dead_strip _Mod__uMod_var_u16_i32.eh
_Mod__uMod_param_u16_i32.eh = 0
.no_dead_strip _Mod__uMod_param_u16_i32.eh
_Mod__uMod_var_u16_C.eh = 0
.no_dead_strip _Mod__uMod_var_u16_C.eh
_Mod__uMod_param_u16_C.eh = 0
.no_dead_strip _Mod__uMod_param_u16_C.eh
_Mod__uMod_var_u16_u8.eh = 0
.no_dead_strip _Mod__uMod_var_u16_u8.eh
_Mod__Mod_var_u16_u8.eh = 0
.no_dead_strip _Mod__Mod_var_u16_u8.eh
_Mod__uMod_param_u16_u8.eh = 0
.no_dead_strip _Mod__uMod_param_u16_u8.eh
_Mod__Mod_param_u16_u8.eh = 0
.no_dead_strip _Mod__Mod_param_u16_u8.eh
_Mod__uMod_var_u16_i64.eh = 0
.no_dead_strip _Mod__uMod_var_u16_i64.eh
_Mod__Mod_var_u16_i64.eh = 0
.no_dead_strip _Mod__Mod_var_u16_i64.eh
_Mod__uMod_param_u16_i64.eh = 0
.no_dead_strip _Mod__uMod_param_u16_i64.eh
_Mod__Mod_param_u16_i64.eh = 0
.no_dead_strip _Mod__Mod_param_u16_i64.eh
_Mod__uMod_var_u16_L.eh = 0
.no_dead_strip _Mod__uMod_var_u16_L.eh
_Mod__Mod_var_u16_L.eh = 0
.no_dead_strip _Mod__Mod_var_u16_L.eh
_Mod__uMod_param_u16_L.eh = 0
.no_dead_strip _Mod__uMod_param_u16_L.eh
_Mod__Mod_param_u16_L.eh = 0
.no_dead_strip _Mod__Mod_param_u16_L.eh
_Mod__uMod_var_u16_i8.eh = 0
.no_dead_strip _Mod__uMod_var_u16_i8.eh
_Mod__uMod_param_u16_i8.eh = 0
.no_dead_strip _Mod__uMod_param_u16_i8.eh
_Mod__uMod_var_u32_u16.eh = 0
.no_dead_strip _Mod__uMod_var_u32_u16.eh
_Mod__Mod_var_u32_u16.eh = 0
.no_dead_strip _Mod__Mod_var_u32_u16.eh
_Mod__uMod_param_u32_u16.eh = 0
.no_dead_strip _Mod__uMod_param_u32_u16.eh
_Mod__Mod_param_u32_u16.eh = 0
.no_dead_strip _Mod__Mod_param_u32_u16.eh
_Mod__uMod_var_u32_u32.eh = 0
.no_dead_strip _Mod__uMod_var_u32_u32.eh
_Mod__Mod_var_u32_u32.eh = 0
.no_dead_strip _Mod__Mod_var_u32_u32.eh
_Mod__uMod_param_u32_u32.eh = 0
.no_dead_strip _Mod__uMod_param_u32_u32.eh
_Mod__Mod_param_u32_u32.eh = 0
.no_dead_strip _Mod__Mod_param_u32_u32.eh
_Mod__uMod_var_u32_LC.eh = 0
.no_dead_strip _Mod__uMod_var_u32_LC.eh
_Mod__Mod_var_u32_LC.eh = 0
.no_dead_strip _Mod__Mod_var_u32_LC.eh
_Mod__uMod_param_u32_LC.eh = 0
.no_dead_strip _Mod__uMod_param_u32_LC.eh
_Mod__Mod_param_u32_LC.eh = 0
.no_dead_strip _Mod__Mod_param_u32_LC.eh
_Mod__uMod_var_u32_I.eh = 0
.no_dead_strip _Mod__uMod_var_u32_I.eh
_Mod__uMod_param_u32_I.eh = 0
.no_dead_strip _Mod__uMod_param_u32_I.eh
_Mod__uMod_var_u32_i16.eh = 0
.no_dead_strip _Mod__uMod_var_u32_i16.eh
_Mod__uMod_param_u32_i16.eh = 0
.no_dead_strip _Mod__uMod_param_u32_i16.eh
_Mod__uMod_var_u32_u64.eh = 0
.no_dead_strip _Mod__uMod_var_u32_u64.eh
_Mod__Mod_var_u32_u64.eh = 0
.no_dead_strip _Mod__Mod_var_u32_u64.eh
_Mod__uMod_param_u32_u64.eh = 0
.no_dead_strip _Mod__uMod_param_u32_u64.eh
_Mod__Mod_param_u32_u64.eh = 0
.no_dead_strip _Mod__Mod_param_u32_u64.eh
_Mod__uMod_var_u32_i32.eh = 0
.no_dead_strip _Mod__uMod_var_u32_i32.eh
_Mod__uMod_param_u32_i32.eh = 0
.no_dead_strip _Mod__uMod_param_u32_i32.eh
_Mod__uMod_var_u32_C.eh = 0
.no_dead_strip _Mod__uMod_var_u32_C.eh
_Mod__uMod_param_u32_C.eh = 0
.no_dead_strip _Mod__uMod_param_u32_C.eh
_Mod__uMod_var_u32_u8.eh = 0
.no_dead_strip _Mod__uMod_var_u32_u8.eh
_Mod__Mod_var_u32_u8.eh = 0
.no_dead_strip _Mod__Mod_var_u32_u8.eh
_Mod__uMod_param_u32_u8.eh = 0
.no_dead_strip _Mod__uMod_param_u32_u8.eh
_Mod__Mod_param_u32_u8.eh = 0
.no_dead_strip _Mod__Mod_param_u32_u8.eh
_Mod__uMod_var_u32_i64.eh = 0
.no_dead_strip _Mod__uMod_var_u32_i64.eh
_Mod__Mod_var_u32_i64.eh = 0
.no_dead_strip _Mod__Mod_var_u32_i64.eh
_Mod__uMod_param_u32_i64.eh = 0
.no_dead_strip _Mod__uMod_param_u32_i64.eh
_Mod__Mod_param_u32_i64.eh = 0
.no_dead_strip _Mod__Mod_param_u32_i64.eh
_Mod__uMod_var_u32_L.eh = 0
.no_dead_strip _Mod__uMod_var_u32_L.eh
_Mod__Mod_var_u32_L.eh = 0
.no_dead_strip _Mod__Mod_var_u32_L.eh
_Mod__uMod_param_u32_L.eh = 0
.no_dead_strip _Mod__uMod_param_u32_L.eh
_Mod__Mod_param_u32_L.eh = 0
.no_dead_strip _Mod__Mod_param_u32_L.eh
_Mod__uMod_var_u32_i8.eh = 0
.no_dead_strip _Mod__uMod_var_u32_i8.eh
_Mod__uMod_param_u32_i8.eh = 0
.no_dead_strip _Mod__uMod_param_u32_i8.eh
_Mod__uMod_var_LC_u16.eh = 0
.no_dead_strip _Mod__uMod_var_LC_u16.eh
_Mod__Mod_var_LC_u16.eh = 0
.no_dead_strip _Mod__Mod_var_LC_u16.eh
_Mod__uMod_param_LC_u16.eh = 0
.no_dead_strip _Mod__uMod_param_LC_u16.eh
_Mod__Mod_param_LC_u16.eh = 0
.no_dead_strip _Mod__Mod_param_LC_u16.eh
_Mod__uMod_var_LC_u32.eh = 0
.no_dead_strip _Mod__uMod_var_LC_u32.eh
_Mod__Mod_var_LC_u32.eh = 0
.no_dead_strip _Mod__Mod_var_LC_u32.eh
_Mod__uMod_param_LC_u32.eh = 0
.no_dead_strip _Mod__uMod_param_LC_u32.eh
_Mod__Mod_param_LC_u32.eh = 0
.no_dead_strip _Mod__Mod_param_LC_u32.eh
_Mod__uMod_var_LC_LC.eh = 0
.no_dead_strip _Mod__uMod_var_LC_LC.eh
_Mod__Mod_var_LC_LC.eh = 0
.no_dead_strip _Mod__Mod_var_LC_LC.eh
_Mod__uMod_param_LC_LC.eh = 0
.no_dead_strip _Mod__uMod_param_LC_LC.eh
_Mod__Mod_param_LC_LC.eh = 0
.no_dead_strip _Mod__Mod_param_LC_LC.eh
_Mod__uMod_var_LC_I.eh = 0
.no_dead_strip _Mod__uMod_var_LC_I.eh
_Mod__Mod_var_LC_I.eh = 0
.no_dead_strip _Mod__Mod_var_LC_I.eh
_Mod__uMod_param_LC_I.eh = 0
.no_dead_strip _Mod__uMod_param_LC_I.eh
_Mod__Mod_param_LC_I.eh = 0
.no_dead_strip _Mod__Mod_param_LC_I.eh
_Mod__uMod_var_LC_i16.eh = 0
.no_dead_strip _Mod__uMod_var_LC_i16.eh
_Mod__Mod_var_LC_i16.eh = 0
.no_dead_strip _Mod__Mod_var_LC_i16.eh
_Mod__uMod_param_LC_i16.eh = 0
.no_dead_strip _Mod__uMod_param_LC_i16.eh
_Mod__Mod_param_LC_i16.eh = 0
.no_dead_strip _Mod__Mod_param_LC_i16.eh
_Mod__uMod_var_LC_u64.eh = 0
.no_dead_strip _Mod__uMod_var_LC_u64.eh
_Mod__Mod_var_LC_u64.eh = 0
.no_dead_strip _Mod__Mod_var_LC_u64.eh
_Mod__uMod_param_LC_u64.eh = 0
.no_dead_strip _Mod__uMod_param_LC_u64.eh
_Mod__Mod_param_LC_u64.eh = 0
.no_dead_strip _Mod__Mod_param_LC_u64.eh
_Mod__uMod_var_LC_i32.eh = 0
.no_dead_strip _Mod__uMod_var_LC_i32.eh
_Mod__Mod_var_LC_i32.eh = 0
.no_dead_strip _Mod__Mod_var_LC_i32.eh
_Mod__uMod_param_LC_i32.eh = 0
.no_dead_strip _Mod__uMod_param_LC_i32.eh
_Mod__Mod_param_LC_i32.eh = 0
.no_dead_strip _Mod__Mod_param_LC_i32.eh
_Mod__uMod_var_LC_C.eh = 0
.no_dead_strip _Mod__uMod_var_LC_C.eh
_Mod__Mod_var_LC_C.eh = 0
.no_dead_strip _Mod__Mod_var_LC_C.eh
_Mod__uMod_param_LC_C.eh = 0
.no_dead_strip _Mod__uMod_param_LC_C.eh
_Mod__Mod_param_LC_C.eh = 0
.no_dead_strip _Mod__Mod_param_LC_C.eh
_Mod__uMod_var_LC_u8.eh = 0
.no_dead_strip _Mod__uMod_var_LC_u8.eh
_Mod__Mod_var_LC_u8.eh = 0
.no_dead_strip _Mod__Mod_var_LC_u8.eh
_Mod__uMod_param_LC_u8.eh = 0
.no_dead_strip _Mod__uMod_param_LC_u8.eh
_Mod__Mod_param_LC_u8.eh = 0
.no_dead_strip _Mod__Mod_param_LC_u8.eh
_Mod__uMod_var_LC_i64.eh = 0
.no_dead_strip _Mod__uMod_var_LC_i64.eh
_Mod__Mod_var_LC_i64.eh = 0
.no_dead_strip _Mod__Mod_var_LC_i64.eh
_Mod__uMod_param_LC_i64.eh = 0
.no_dead_strip _Mod__uMod_param_LC_i64.eh
_Mod__Mod_param_LC_i64.eh = 0
.no_dead_strip _Mod__Mod_param_LC_i64.eh
_Mod__uMod_var_LC_L.eh = 0
.no_dead_strip _Mod__uMod_var_LC_L.eh
_Mod__Mod_var_LC_L.eh = 0
.no_dead_strip _Mod__Mod_var_LC_L.eh
_Mod__uMod_param_LC_L.eh = 0
.no_dead_strip _Mod__uMod_param_LC_L.eh
_Mod__Mod_param_LC_L.eh = 0
.no_dead_strip _Mod__Mod_param_LC_L.eh
_Mod__uMod_var_LC_i8.eh = 0
.no_dead_strip _Mod__uMod_var_LC_i8.eh
_Mod__Mod_var_LC_i8.eh = 0
.no_dead_strip _Mod__Mod_var_LC_i8.eh
_Mod__uMod_param_LC_i8.eh = 0
.no_dead_strip _Mod__uMod_param_LC_i8.eh
_Mod__Mod_param_LC_i8.eh = 0
.no_dead_strip _Mod__Mod_param_LC_i8.eh
_Mod__uMod_var_I_u16.eh = 0
.no_dead_strip _Mod__uMod_var_I_u16.eh
_Mod__Mod_var_I_u16.eh = 0
.no_dead_strip _Mod__Mod_var_I_u16.eh
_Mod__uMod_param_I_u16.eh = 0
.no_dead_strip _Mod__uMod_param_I_u16.eh
_Mod__Mod_param_I_u16.eh = 0
.no_dead_strip _Mod__Mod_param_I_u16.eh
_Mod__uMod_var_I_u32.eh = 0
.no_dead_strip _Mod__uMod_var_I_u32.eh
_Mod__Mod_var_I_u32.eh = 0
.no_dead_strip _Mod__Mod_var_I_u32.eh
_Mod__uMod_param_I_u32.eh = 0
.no_dead_strip _Mod__uMod_param_I_u32.eh
_Mod__Mod_param_I_u32.eh = 0
.no_dead_strip _Mod__Mod_param_I_u32.eh
_Mod__uMod_var_I_LC.eh = 0
.no_dead_strip _Mod__uMod_var_I_LC.eh
_Mod__Mod_var_I_LC.eh = 0
.no_dead_strip _Mod__Mod_var_I_LC.eh
_Mod__uMod_param_I_LC.eh = 0
.no_dead_strip _Mod__uMod_param_I_LC.eh
_Mod__Mod_param_I_LC.eh = 0
.no_dead_strip _Mod__Mod_param_I_LC.eh
_Mod__uMod_var_I_I.eh = 0
.no_dead_strip _Mod__uMod_var_I_I.eh
_Mod__Mod_var_I_I.eh = 0
.no_dead_strip _Mod__Mod_var_I_I.eh
_Mod__uMod_param_I_I.eh = 0
.no_dead_strip _Mod__uMod_param_I_I.eh
_Mod__Mod_param_I_I.eh = 0
.no_dead_strip _Mod__Mod_param_I_I.eh
_Mod__uMod_var_I_i16.eh = 0
.no_dead_strip _Mod__uMod_var_I_i16.eh
_Mod__Mod_var_I_i16.eh = 0
.no_dead_strip _Mod__Mod_var_I_i16.eh
_Mod__uMod_param_I_i16.eh = 0
.no_dead_strip _Mod__uMod_param_I_i16.eh
_Mod__Mod_param_I_i16.eh = 0
.no_dead_strip _Mod__Mod_param_I_i16.eh
_Mod__uMod_var_I_u64.eh = 0
.no_dead_strip _Mod__uMod_var_I_u64.eh
_Mod__Mod_var_I_u64.eh = 0
.no_dead_strip _Mod__Mod_var_I_u64.eh
_Mod__uMod_param_I_u64.eh = 0
.no_dead_strip _Mod__uMod_param_I_u64.eh
_Mod__Mod_param_I_u64.eh = 0
.no_dead_strip _Mod__Mod_param_I_u64.eh
_Mod__uMod_var_I_i32.eh = 0
.no_dead_strip _Mod__uMod_var_I_i32.eh
_Mod__Mod_var_I_i32.eh = 0
.no_dead_strip _Mod__Mod_var_I_i32.eh
_Mod__uMod_param_I_i32.eh = 0
.no_dead_strip _Mod__uMod_param_I_i32.eh
_Mod__Mod_param_I_i32.eh = 0
.no_dead_strip _Mod__Mod_param_I_i32.eh
_Mod__uMod_var_I_C.eh = 0
.no_dead_strip _Mod__uMod_var_I_C.eh
_Mod__Mod_var_I_C.eh = 0
.no_dead_strip _Mod__Mod_var_I_C.eh
_Mod__uMod_param_I_C.eh = 0
.no_dead_strip _Mod__uMod_param_I_C.eh
_Mod__Mod_param_I_C.eh = 0
.no_dead_strip _Mod__Mod_param_I_C.eh
_Mod__uMod_var_I_u8.eh = 0
.no_dead_strip _Mod__uMod_var_I_u8.eh
_Mod__Mod_var_I_u8.eh = 0
.no_dead_strip _Mod__Mod_var_I_u8.eh
_Mod__uMod_param_I_u8.eh = 0
.no_dead_strip _Mod__uMod_param_I_u8.eh
_Mod__Mod_param_I_u8.eh = 0
.no_dead_strip _Mod__Mod_param_I_u8.eh
_Mod__uMod_var_I_i64.eh = 0
.no_dead_strip _Mod__uMod_var_I_i64.eh
_Mod__Mod_var_I_i64.eh = 0
.no_dead_strip _Mod__Mod_var_I_i64.eh
_Mod__uMod_param_I_i64.eh = 0
.no_dead_strip _Mod__uMod_param_I_i64.eh
_Mod__Mod_param_I_i64.eh = 0
.no_dead_strip _Mod__Mod_param_I_i64.eh
_Mod__uMod_var_I_L.eh = 0
.no_dead_strip _Mod__uMod_var_I_L.eh
_Mod__Mod_var_I_L.eh = 0
.no_dead_strip _Mod__Mod_var_I_L.eh
_Mod__uMod_param_I_L.eh = 0
.no_dead_strip _Mod__uMod_param_I_L.eh
_Mod__Mod_param_I_L.eh = 0
.no_dead_strip _Mod__Mod_param_I_L.eh
_Mod__uMod_var_I_i8.eh = 0
.no_dead_strip _Mod__uMod_var_I_i8.eh
_Mod__Mod_var_I_i8.eh = 0
.no_dead_strip _Mod__Mod_var_I_i8.eh
_Mod__uMod_param_I_i8.eh = 0
.no_dead_strip _Mod__uMod_param_I_i8.eh
_Mod__Mod_param_I_i8.eh = 0
.no_dead_strip _Mod__Mod_param_I_i8.eh
_Mod__uMod_var_i16_u16.eh = 0
.no_dead_strip _Mod__uMod_var_i16_u16.eh
_Mod__uMod_param_i16_u16.eh = 0
.no_dead_strip _Mod__uMod_param_i16_u16.eh
_Mod__uMod_var_i16_u32.eh = 0
.no_dead_strip _Mod__uMod_var_i16_u32.eh
_Mod__uMod_param_i16_u32.eh = 0
.no_dead_strip _Mod__uMod_param_i16_u32.eh
_Mod__uMod_var_i16_LC.eh = 0
.no_dead_strip _Mod__uMod_var_i16_LC.eh
_Mod__Mod_var_i16_LC.eh = 0
.no_dead_strip _Mod__Mod_var_i16_LC.eh
_Mod__uMod_param_i16_LC.eh = 0
.no_dead_strip _Mod__uMod_param_i16_LC.eh
_Mod__Mod_param_i16_LC.eh = 0
.no_dead_strip _Mod__Mod_param_i16_LC.eh
_Mod__uMod_var_i16_I.eh = 0
.no_dead_strip _Mod__uMod_var_i16_I.eh
_Mod__uMod_param_i16_I.eh = 0
.no_dead_strip _Mod__uMod_param_i16_I.eh
_Mod__uMod_var_i16_i16.eh = 0
.no_dead_strip _Mod__uMod_var_i16_i16.eh
_Mod__uMod_param_i16_i16.eh = 0
.no_dead_strip _Mod__uMod_param_i16_i16.eh
_Mod__uMod_var_i16_u64.eh = 0
.no_dead_strip _Mod__uMod_var_i16_u64.eh
_Mod__Mod_var_i16_u64.eh = 0
.no_dead_strip _Mod__Mod_var_i16_u64.eh
_Mod__uMod_param_i16_u64.eh = 0
.no_dead_strip _Mod__uMod_param_i16_u64.eh
_Mod__Mod_param_i16_u64.eh = 0
.no_dead_strip _Mod__Mod_param_i16_u64.eh
_Mod__uMod_var_i16_i32.eh = 0
.no_dead_strip _Mod__uMod_var_i16_i32.eh
_Mod__uMod_param_i16_i32.eh = 0
.no_dead_strip _Mod__uMod_param_i16_i32.eh
_Mod__uMod_var_i16_C.eh = 0
.no_dead_strip _Mod__uMod_var_i16_C.eh
_Mod__uMod_param_i16_C.eh = 0
.no_dead_strip _Mod__uMod_param_i16_C.eh
_Mod__uMod_var_i16_u8.eh = 0
.no_dead_strip _Mod__uMod_var_i16_u8.eh
_Mod__Mod_var_i16_u8.eh = 0
.no_dead_strip _Mod__Mod_var_i16_u8.eh
_Mod__uMod_param_i16_u8.eh = 0
.no_dead_strip _Mod__uMod_param_i16_u8.eh
_Mod__Mod_param_i16_u8.eh = 0
.no_dead_strip _Mod__Mod_param_i16_u8.eh
_Mod__uMod_var_i16_i64.eh = 0
.no_dead_strip _Mod__uMod_var_i16_i64.eh
_Mod__Mod_var_i16_i64.eh = 0
.no_dead_strip _Mod__Mod_var_i16_i64.eh
_Mod__uMod_param_i16_i64.eh = 0
.no_dead_strip _Mod__uMod_param_i16_i64.eh
_Mod__Mod_param_i16_i64.eh = 0
.no_dead_strip _Mod__Mod_param_i16_i64.eh
_Mod__uMod_var_i16_L.eh = 0
.no_dead_strip _Mod__uMod_var_i16_L.eh
_Mod__Mod_var_i16_L.eh = 0
.no_dead_strip _Mod__Mod_var_i16_L.eh
_Mod__uMod_param_i16_L.eh = 0
.no_dead_strip _Mod__uMod_param_i16_L.eh
_Mod__Mod_param_i16_L.eh = 0
.no_dead_strip _Mod__Mod_param_i16_L.eh
_Mod__uMod_var_i16_i8.eh = 0
.no_dead_strip _Mod__uMod_var_i16_i8.eh
_Mod__uMod_param_i16_i8.eh = 0
.no_dead_strip _Mod__uMod_param_i16_i8.eh
_Mod__uMod_var_u64_u16.eh = 0
.no_dead_strip _Mod__uMod_var_u64_u16.eh
_Mod__Mod_var_u64_u16.eh = 0
.no_dead_strip _Mod__Mod_var_u64_u16.eh
_Mod__uMod_param_u64_u16.eh = 0
.no_dead_strip _Mod__uMod_param_u64_u16.eh
_Mod__Mod_param_u64_u16.eh = 0
.no_dead_strip _Mod__Mod_param_u64_u16.eh
_Mod__uMod_var_u64_u32.eh = 0
.no_dead_strip _Mod__uMod_var_u64_u32.eh
_Mod__Mod_var_u64_u32.eh = 0
.no_dead_strip _Mod__Mod_var_u64_u32.eh
_Mod__uMod_param_u64_u32.eh = 0
.no_dead_strip _Mod__uMod_param_u64_u32.eh
_Mod__Mod_param_u64_u32.eh = 0
.no_dead_strip _Mod__Mod_param_u64_u32.eh
_Mod__uMod_var_u64_LC.eh = 0
.no_dead_strip _Mod__uMod_var_u64_LC.eh
_Mod__Mod_var_u64_LC.eh = 0
.no_dead_strip _Mod__Mod_var_u64_LC.eh
_Mod__uMod_param_u64_LC.eh = 0
.no_dead_strip _Mod__uMod_param_u64_LC.eh
_Mod__Mod_param_u64_LC.eh = 0
.no_dead_strip _Mod__Mod_param_u64_LC.eh
_Mod__uMod_var_u64_I.eh = 0
.no_dead_strip _Mod__uMod_var_u64_I.eh
_Mod__Mod_var_u64_I.eh = 0
.no_dead_strip _Mod__Mod_var_u64_I.eh
_Mod__uMod_param_u64_I.eh = 0
.no_dead_strip _Mod__uMod_param_u64_I.eh
_Mod__Mod_param_u64_I.eh = 0
.no_dead_strip _Mod__Mod_param_u64_I.eh
_Mod__uMod_var_u64_i16.eh = 0
.no_dead_strip _Mod__uMod_var_u64_i16.eh
_Mod__Mod_var_u64_i16.eh = 0
.no_dead_strip _Mod__Mod_var_u64_i16.eh
_Mod__uMod_param_u64_i16.eh = 0
.no_dead_strip _Mod__uMod_param_u64_i16.eh
_Mod__Mod_param_u64_i16.eh = 0
.no_dead_strip _Mod__Mod_param_u64_i16.eh
_Mod__uMod_var_u64_u64.eh = 0
.no_dead_strip _Mod__uMod_var_u64_u64.eh
_Mod__Mod_var_u64_u64.eh = 0
.no_dead_strip _Mod__Mod_var_u64_u64.eh
_Mod__uMod_param_u64_u64.eh = 0
.no_dead_strip _Mod__uMod_param_u64_u64.eh
_Mod__Mod_param_u64_u64.eh = 0
.no_dead_strip _Mod__Mod_param_u64_u64.eh
_Mod__uMod_var_u64_i32.eh = 0
.no_dead_strip _Mod__uMod_var_u64_i32.eh
_Mod__Mod_var_u64_i32.eh = 0
.no_dead_strip _Mod__Mod_var_u64_i32.eh
_Mod__uMod_param_u64_i32.eh = 0
.no_dead_strip _Mod__uMod_param_u64_i32.eh
_Mod__Mod_param_u64_i32.eh = 0
.no_dead_strip _Mod__Mod_param_u64_i32.eh
_Mod__uMod_var_u64_C.eh = 0
.no_dead_strip _Mod__uMod_var_u64_C.eh
_Mod__Mod_var_u64_C.eh = 0
.no_dead_strip _Mod__Mod_var_u64_C.eh
_Mod__uMod_param_u64_C.eh = 0
.no_dead_strip _Mod__uMod_param_u64_C.eh
_Mod__Mod_param_u64_C.eh = 0
.no_dead_strip _Mod__Mod_param_u64_C.eh
_Mod__uMod_var_u64_u8.eh = 0
.no_dead_strip _Mod__uMod_var_u64_u8.eh
_Mod__Mod_var_u64_u8.eh = 0
.no_dead_strip _Mod__Mod_var_u64_u8.eh
_Mod__uMod_param_u64_u8.eh = 0
.no_dead_strip _Mod__uMod_param_u64_u8.eh
_Mod__Mod_param_u64_u8.eh = 0
.no_dead_strip _Mod__Mod_param_u64_u8.eh
_Mod__uMod_var_u64_i64.eh = 0
.no_dead_strip _Mod__uMod_var_u64_i64.eh
_Mod__Mod_var_u64_i64.eh = 0
.no_dead_strip _Mod__Mod_var_u64_i64.eh
_Mod__uMod_param_u64_i64.eh = 0
.no_dead_strip _Mod__uMod_param_u64_i64.eh
_Mod__Mod_param_u64_i64.eh = 0
.no_dead_strip _Mod__Mod_param_u64_i64.eh
_Mod__uMod_var_u64_L.eh = 0
.no_dead_strip _Mod__uMod_var_u64_L.eh
_Mod__Mod_var_u64_L.eh = 0
.no_dead_strip _Mod__Mod_var_u64_L.eh
_Mod__uMod_param_u64_L.eh = 0
.no_dead_strip _Mod__uMod_param_u64_L.eh
_Mod__Mod_param_u64_L.eh = 0
.no_dead_strip _Mod__Mod_param_u64_L.eh
_Mod__uMod_var_u64_i8.eh = 0
.no_dead_strip _Mod__uMod_var_u64_i8.eh
_Mod__Mod_var_u64_i8.eh = 0
.no_dead_strip _Mod__Mod_var_u64_i8.eh
_Mod__uMod_param_u64_i8.eh = 0
.no_dead_strip _Mod__uMod_param_u64_i8.eh
_Mod__Mod_param_u64_i8.eh = 0
.no_dead_strip _Mod__Mod_param_u64_i8.eh
_Mod__Mod_var_f64_f64.eh = 0
.no_dead_strip _Mod__Mod_var_f64_f64.eh
_Mod__Mod_param_f64_f64.eh = 0
.no_dead_strip _Mod__Mod_param_f64_f64.eh
_Mod__uMod_var_i32_u16.eh = 0
.no_dead_strip _Mod__uMod_var_i32_u16.eh
_Mod__Mod_var_i32_u16.eh = 0
.no_dead_strip _Mod__Mod_var_i32_u16.eh
_Mod__uMod_param_i32_u16.eh = 0
.no_dead_strip _Mod__uMod_param_i32_u16.eh
_Mod__Mod_param_i32_u16.eh = 0
.no_dead_strip _Mod__Mod_param_i32_u16.eh
_Mod__uMod_var_i32_u32.eh = 0
.no_dead_strip _Mod__uMod_var_i32_u32.eh
_Mod__uMod_param_i32_u32.eh = 0
.no_dead_strip _Mod__uMod_param_i32_u32.eh
_Mod__uMod_var_i32_LC.eh = 0
.no_dead_strip _Mod__uMod_var_i32_LC.eh
_Mod__Mod_var_i32_LC.eh = 0
.no_dead_strip _Mod__Mod_var_i32_LC.eh
_Mod__uMod_param_i32_LC.eh = 0
.no_dead_strip _Mod__uMod_param_i32_LC.eh
_Mod__Mod_param_i32_LC.eh = 0
.no_dead_strip _Mod__Mod_param_i32_LC.eh
_Mod__uMod_var_i32_I.eh = 0
.no_dead_strip _Mod__uMod_var_i32_I.eh
_Mod__uMod_param_i32_I.eh = 0
.no_dead_strip _Mod__uMod_param_i32_I.eh
_Mod__uMod_var_i32_i16.eh = 0
.no_dead_strip _Mod__uMod_var_i32_i16.eh
_Mod__uMod_param_i32_i16.eh = 0
.no_dead_strip _Mod__uMod_param_i32_i16.eh
_Mod__uMod_var_i32_u64.eh = 0
.no_dead_strip _Mod__uMod_var_i32_u64.eh
_Mod__Mod_var_i32_u64.eh = 0
.no_dead_strip _Mod__Mod_var_i32_u64.eh
_Mod__uMod_param_i32_u64.eh = 0
.no_dead_strip _Mod__uMod_param_i32_u64.eh
_Mod__Mod_param_i32_u64.eh = 0
.no_dead_strip _Mod__Mod_param_i32_u64.eh
_Mod__uMod_var_i32_i32.eh = 0
.no_dead_strip _Mod__uMod_var_i32_i32.eh
_Mod__uMod_param_i32_i32.eh = 0
.no_dead_strip _Mod__uMod_param_i32_i32.eh
_Mod__uMod_var_i32_C.eh = 0
.no_dead_strip _Mod__uMod_var_i32_C.eh
_Mod__uMod_param_i32_C.eh = 0
.no_dead_strip _Mod__uMod_param_i32_C.eh
_Mod__uMod_var_i32_u8.eh = 0
.no_dead_strip _Mod__uMod_var_i32_u8.eh
_Mod__Mod_var_i32_u8.eh = 0
.no_dead_strip _Mod__Mod_var_i32_u8.eh
_Mod__uMod_param_i32_u8.eh = 0
.no_dead_strip _Mod__uMod_param_i32_u8.eh
_Mod__Mod_param_i32_u8.eh = 0
.no_dead_strip _Mod__Mod_param_i32_u8.eh
_Mod__uMod_var_i32_i64.eh = 0
.no_dead_strip _Mod__uMod_var_i32_i64.eh
_Mod__Mod_var_i32_i64.eh = 0
.no_dead_strip _Mod__Mod_var_i32_i64.eh
_Mod__uMod_param_i32_i64.eh = 0
.no_dead_strip _Mod__uMod_param_i32_i64.eh
_Mod__Mod_param_i32_i64.eh = 0
.no_dead_strip _Mod__Mod_param_i32_i64.eh
_Mod__uMod_var_i32_L.eh = 0
.no_dead_strip _Mod__uMod_var_i32_L.eh
_Mod__Mod_var_i32_L.eh = 0
.no_dead_strip _Mod__Mod_var_i32_L.eh
_Mod__uMod_param_i32_L.eh = 0
.no_dead_strip _Mod__uMod_param_i32_L.eh
_Mod__Mod_param_i32_L.eh = 0
.no_dead_strip _Mod__Mod_param_i32_L.eh
_Mod__uMod_var_i32_i8.eh = 0
.no_dead_strip _Mod__uMod_var_i32_i8.eh
_Mod__uMod_param_i32_i8.eh = 0
.no_dead_strip _Mod__uMod_param_i32_i8.eh
_Mod__uMod_var_C_u16.eh = 0
.no_dead_strip _Mod__uMod_var_C_u16.eh
_Mod__Mod_var_C_u16.eh = 0
.no_dead_strip _Mod__Mod_var_C_u16.eh
_Mod__uMod_param_C_u16.eh = 0
.no_dead_strip _Mod__uMod_param_C_u16.eh
_Mod__Mod_param_C_u16.eh = 0
.no_dead_strip _Mod__Mod_param_C_u16.eh
_Mod__uMod_var_C_u32.eh = 0
.no_dead_strip _Mod__uMod_var_C_u32.eh
_Mod__Mod_var_C_u32.eh = 0
.no_dead_strip _Mod__Mod_var_C_u32.eh
_Mod__uMod_param_C_u32.eh = 0
.no_dead_strip _Mod__uMod_param_C_u32.eh
_Mod__Mod_param_C_u32.eh = 0
.no_dead_strip _Mod__Mod_param_C_u32.eh
_Mod__uMod_var_C_LC.eh = 0
.no_dead_strip _Mod__uMod_var_C_LC.eh
_Mod__Mod_var_C_LC.eh = 0
.no_dead_strip _Mod__Mod_var_C_LC.eh
_Mod__uMod_param_C_LC.eh = 0
.no_dead_strip _Mod__uMod_param_C_LC.eh
_Mod__Mod_param_C_LC.eh = 0
.no_dead_strip _Mod__Mod_param_C_LC.eh
_Mod__uMod_var_C_I.eh = 0
.no_dead_strip _Mod__uMod_var_C_I.eh
_Mod__uMod_param_C_I.eh = 0
.no_dead_strip _Mod__uMod_param_C_I.eh
_Mod__uMod_var_C_i16.eh = 0
.no_dead_strip _Mod__uMod_var_C_i16.eh
_Mod__uMod_param_C_i16.eh = 0
.no_dead_strip _Mod__uMod_param_C_i16.eh
_Mod__uMod_var_C_u64.eh = 0
.no_dead_strip _Mod__uMod_var_C_u64.eh
_Mod__Mod_var_C_u64.eh = 0
.no_dead_strip _Mod__Mod_var_C_u64.eh
_Mod__uMod_param_C_u64.eh = 0
.no_dead_strip _Mod__uMod_param_C_u64.eh
_Mod__Mod_param_C_u64.eh = 0
.no_dead_strip _Mod__Mod_param_C_u64.eh
_Mod__uMod_var_C_i32.eh = 0
.no_dead_strip _Mod__uMod_var_C_i32.eh
_Mod__uMod_param_C_i32.eh = 0
.no_dead_strip _Mod__uMod_param_C_i32.eh
_Mod__uMod_var_C_C.eh = 0
.no_dead_strip _Mod__uMod_var_C_C.eh
_Mod__Mod_var_C_C.eh = 0
.no_dead_strip _Mod__Mod_var_C_C.eh
_Mod__uMod_param_C_C.eh = 0
.no_dead_strip _Mod__uMod_param_C_C.eh
_Mod__Mod_param_C_C.eh = 0
.no_dead_strip _Mod__Mod_param_C_C.eh
_Mod__uMod_var_C_u8.eh = 0
.no_dead_strip _Mod__uMod_var_C_u8.eh
_Mod__Mod_var_C_u8.eh = 0
.no_dead_strip _Mod__Mod_var_C_u8.eh
_Mod__uMod_param_C_u8.eh = 0
.no_dead_strip _Mod__uMod_param_C_u8.eh
_Mod__Mod_param_C_u8.eh = 0
.no_dead_strip _Mod__Mod_param_C_u8.eh
_Mod__uMod_var_C_i64.eh = 0
.no_dead_strip _Mod__uMod_var_C_i64.eh
_Mod__Mod_var_C_i64.eh = 0
.no_dead_strip _Mod__Mod_var_C_i64.eh
_Mod__uMod_param_C_i64.eh = 0
.no_dead_strip _Mod__uMod_param_C_i64.eh
_Mod__Mod_param_C_i64.eh = 0
.no_dead_strip _Mod__Mod_param_C_i64.eh
_Mod__uMod_var_C_L.eh = 0
.no_dead_strip _Mod__uMod_var_C_L.eh
_Mod__Mod_var_C_L.eh = 0
.no_dead_strip _Mod__Mod_var_C_L.eh
_Mod__uMod_param_C_L.eh = 0
.no_dead_strip _Mod__uMod_param_C_L.eh
_Mod__Mod_param_C_L.eh = 0
.no_dead_strip _Mod__Mod_param_C_L.eh
_Mod__uMod_var_C_i8.eh = 0
.no_dead_strip _Mod__uMod_var_C_i8.eh
_Mod__uMod_param_C_i8.eh = 0
.no_dead_strip _Mod__uMod_param_C_i8.eh
_Mod__uMod_var_u8_u16.eh = 0
.no_dead_strip _Mod__uMod_var_u8_u16.eh
_Mod__uMod_param_u8_u16.eh = 0
.no_dead_strip _Mod__uMod_param_u8_u16.eh
_Mod__uMod_var_u8_u32.eh = 0
.no_dead_strip _Mod__uMod_var_u8_u32.eh
_Mod__uMod_param_u8_u32.eh = 0
.no_dead_strip _Mod__uMod_param_u8_u32.eh
_Mod__uMod_var_u8_LC.eh = 0
.no_dead_strip _Mod__uMod_var_u8_LC.eh
_Mod__Mod_var_u8_LC.eh = 0
.no_dead_strip _Mod__Mod_var_u8_LC.eh
_Mod__uMod_param_u8_LC.eh = 0
.no_dead_strip _Mod__uMod_param_u8_LC.eh
_Mod__Mod_param_u8_LC.eh = 0
.no_dead_strip _Mod__Mod_param_u8_LC.eh
_Mod__uMod_var_u8_I.eh = 0
.no_dead_strip _Mod__uMod_var_u8_I.eh
_Mod__uMod_param_u8_I.eh = 0
.no_dead_strip _Mod__uMod_param_u8_I.eh
_Mod__uMod_var_u8_i16.eh = 0
.no_dead_strip _Mod__uMod_var_u8_i16.eh
_Mod__uMod_param_u8_i16.eh = 0
.no_dead_strip _Mod__uMod_param_u8_i16.eh
_Mod__uMod_var_u8_u64.eh = 0
.no_dead_strip _Mod__uMod_var_u8_u64.eh
_Mod__Mod_var_u8_u64.eh = 0
.no_dead_strip _Mod__Mod_var_u8_u64.eh
_Mod__uMod_param_u8_u64.eh = 0
.no_dead_strip _Mod__uMod_param_u8_u64.eh
_Mod__Mod_param_u8_u64.eh = 0
.no_dead_strip _Mod__Mod_param_u8_u64.eh
_Mod__uMod_var_u8_i32.eh = 0
.no_dead_strip _Mod__uMod_var_u8_i32.eh
_Mod__uMod_param_u8_i32.eh = 0
.no_dead_strip _Mod__uMod_param_u8_i32.eh
_Mod__uMod_var_u8_C.eh = 0
.no_dead_strip _Mod__uMod_var_u8_C.eh
_Mod__uMod_param_u8_C.eh = 0
.no_dead_strip _Mod__uMod_param_u8_C.eh
_Mod__uMod_var_u8_u8.eh = 0
.no_dead_strip _Mod__uMod_var_u8_u8.eh
_Mod__Mod_var_u8_u8.eh = 0
.no_dead_strip _Mod__Mod_var_u8_u8.eh
_Mod__uMod_param_u8_u8.eh = 0
.no_dead_strip _Mod__uMod_param_u8_u8.eh
_Mod__Mod_param_u8_u8.eh = 0
.no_dead_strip _Mod__Mod_param_u8_u8.eh
_Mod__uMod_var_u8_i64.eh = 0
.no_dead_strip _Mod__uMod_var_u8_i64.eh
_Mod__Mod_var_u8_i64.eh = 0
.no_dead_strip _Mod__Mod_var_u8_i64.eh
_Mod__uMod_param_u8_i64.eh = 0
.no_dead_strip _Mod__uMod_param_u8_i64.eh
_Mod__Mod_param_u8_i64.eh = 0
.no_dead_strip _Mod__Mod_param_u8_i64.eh
_Mod__uMod_var_u8_L.eh = 0
.no_dead_strip _Mod__uMod_var_u8_L.eh
_Mod__Mod_var_u8_L.eh = 0
.no_dead_strip _Mod__Mod_var_u8_L.eh
_Mod__uMod_param_u8_L.eh = 0
.no_dead_strip _Mod__uMod_param_u8_L.eh
_Mod__Mod_param_u8_L.eh = 0
.no_dead_strip _Mod__Mod_param_u8_L.eh
_Mod__uMod_var_u8_i8.eh = 0
.no_dead_strip _Mod__uMod_var_u8_i8.eh
_Mod__uMod_param_u8_i8.eh = 0
.no_dead_strip _Mod__uMod_param_u8_i8.eh
_Mod__uMod_var_i64_u16.eh = 0
.no_dead_strip _Mod__uMod_var_i64_u16.eh
_Mod__Mod_var_i64_u16.eh = 0
.no_dead_strip _Mod__Mod_var_i64_u16.eh
_Mod__uMod_param_i64_u16.eh = 0
.no_dead_strip _Mod__uMod_param_i64_u16.eh
_Mod__Mod_param_i64_u16.eh = 0
.no_dead_strip _Mod__Mod_param_i64_u16.eh
_Mod__uMod_var_i64_u32.eh = 0
.no_dead_strip _Mod__uMod_var_i64_u32.eh
_Mod__Mod_var_i64_u32.eh = 0
.no_dead_strip _Mod__Mod_var_i64_u32.eh
_Mod__uMod_param_i64_u32.eh = 0
.no_dead_strip _Mod__uMod_param_i64_u32.eh
_Mod__Mod_param_i64_u32.eh = 0
.no_dead_strip _Mod__Mod_param_i64_u32.eh
_Mod__uMod_var_i64_LC.eh = 0
.no_dead_strip _Mod__uMod_var_i64_LC.eh
_Mod__Mod_var_i64_LC.eh = 0
.no_dead_strip _Mod__Mod_var_i64_LC.eh
_Mod__uMod_param_i64_LC.eh = 0
.no_dead_strip _Mod__uMod_param_i64_LC.eh
_Mod__Mod_param_i64_LC.eh = 0
.no_dead_strip _Mod__Mod_param_i64_LC.eh
_Mod__uMod_var_i64_I.eh = 0
.no_dead_strip _Mod__uMod_var_i64_I.eh
_Mod__Mod_var_i64_I.eh = 0
.no_dead_strip _Mod__Mod_var_i64_I.eh
_Mod__uMod_param_i64_I.eh = 0
.no_dead_strip _Mod__uMod_param_i64_I.eh
_Mod__Mod_param_i64_I.eh = 0
.no_dead_strip _Mod__Mod_param_i64_I.eh
_Mod__uMod_var_i64_i16.eh = 0
.no_dead_strip _Mod__uMod_var_i64_i16.eh
_Mod__Mod_var_i64_i16.eh = 0
.no_dead_strip _Mod__Mod_var_i64_i16.eh
_Mod__uMod_param_i64_i16.eh = 0
.no_dead_strip _Mod__uMod_param_i64_i16.eh
_Mod__Mod_param_i64_i16.eh = 0
.no_dead_strip _Mod__Mod_param_i64_i16.eh
_Mod__uMod_var_i64_u64.eh = 0
.no_dead_strip _Mod__uMod_var_i64_u64.eh
_Mod__Mod_var_i64_u64.eh = 0
.no_dead_strip _Mod__Mod_var_i64_u64.eh
_Mod__uMod_param_i64_u64.eh = 0
.no_dead_strip _Mod__uMod_param_i64_u64.eh
_Mod__Mod_param_i64_u64.eh = 0
.no_dead_strip _Mod__Mod_param_i64_u64.eh
_Mod__uMod_var_i64_i32.eh = 0
.no_dead_strip _Mod__uMod_var_i64_i32.eh
_Mod__Mod_var_i64_i32.eh = 0
.no_dead_strip _Mod__Mod_var_i64_i32.eh
_Mod__uMod_param_i64_i32.eh = 0
.no_dead_strip _Mod__uMod_param_i64_i32.eh
_Mod__Mod_param_i64_i32.eh = 0
.no_dead_strip _Mod__Mod_param_i64_i32.eh
_Mod__uMod_var_i64_C.eh = 0
.no_dead_strip _Mod__uMod_var_i64_C.eh
_Mod__Mod_var_i64_C.eh = 0
.no_dead_strip _Mod__Mod_var_i64_C.eh
_Mod__uMod_param_i64_C.eh = 0
.no_dead_strip _Mod__uMod_param_i64_C.eh
_Mod__Mod_param_i64_C.eh = 0
.no_dead_strip _Mod__Mod_param_i64_C.eh
_Mod__uMod_var_i64_u8.eh = 0
.no_dead_strip _Mod__uMod_var_i64_u8.eh
_Mod__Mod_var_i64_u8.eh = 0
.no_dead_strip _Mod__Mod_var_i64_u8.eh
_Mod__uMod_param_i64_u8.eh = 0
.no_dead_strip _Mod__uMod_param_i64_u8.eh
_Mod__Mod_param_i64_u8.eh = 0
.no_dead_strip _Mod__Mod_param_i64_u8.eh
_Mod__uMod_var_i64_i64.eh = 0
.no_dead_strip _Mod__uMod_var_i64_i64.eh
_Mod__Mod_var_i64_i64.eh = 0
.no_dead_strip _Mod__Mod_var_i64_i64.eh
_Mod__uMod_param_i64_i64.eh = 0
.no_dead_strip _Mod__uMod_param_i64_i64.eh
_Mod__Mod_param_i64_i64.eh = 0
.no_dead_strip _Mod__Mod_param_i64_i64.eh
_Mod__uMod_var_i64_L.eh = 0
.no_dead_strip _Mod__uMod_var_i64_L.eh
_Mod__Mod_var_i64_L.eh = 0
.no_dead_strip _Mod__Mod_var_i64_L.eh
_Mod__uMod_param_i64_L.eh = 0
.no_dead_strip _Mod__uMod_param_i64_L.eh
_Mod__Mod_param_i64_L.eh = 0
.no_dead_strip _Mod__Mod_param_i64_L.eh
_Mod__uMod_var_i64_i8.eh = 0
.no_dead_strip _Mod__uMod_var_i64_i8.eh
_Mod__Mod_var_i64_i8.eh = 0
.no_dead_strip _Mod__Mod_var_i64_i8.eh
_Mod__uMod_param_i64_i8.eh = 0
.no_dead_strip _Mod__uMod_param_i64_i8.eh
_Mod__Mod_param_i64_i8.eh = 0
.no_dead_strip _Mod__Mod_param_i64_i8.eh
_Mod__Mod_var_f32_f32.eh = 0
.no_dead_strip _Mod__Mod_var_f32_f32.eh
_Mod__Mod_param_f32_f32.eh = 0
.no_dead_strip _Mod__Mod_param_f32_f32.eh
_Mod__uMod_var_L_u16.eh = 0
.no_dead_strip _Mod__uMod_var_L_u16.eh
_Mod__Mod_var_L_u16.eh = 0
.no_dead_strip _Mod__Mod_var_L_u16.eh
_Mod__uMod_param_L_u16.eh = 0
.no_dead_strip _Mod__uMod_param_L_u16.eh
_Mod__Mod_param_L_u16.eh = 0
.no_dead_strip _Mod__Mod_param_L_u16.eh
_Mod__uMod_var_L_u32.eh = 0
.no_dead_strip _Mod__uMod_var_L_u32.eh
_Mod__Mod_var_L_u32.eh = 0
.no_dead_strip _Mod__Mod_var_L_u32.eh
_Mod__uMod_param_L_u32.eh = 0
.no_dead_strip _Mod__uMod_param_L_u32.eh
_Mod__Mod_param_L_u32.eh = 0
.no_dead_strip _Mod__Mod_param_L_u32.eh
_Mod__uMod_var_L_LC.eh = 0
.no_dead_strip _Mod__uMod_var_L_LC.eh
_Mod__Mod_var_L_LC.eh = 0
.no_dead_strip _Mod__Mod_var_L_LC.eh
_Mod__uMod_param_L_LC.eh = 0
.no_dead_strip _Mod__uMod_param_L_LC.eh
_Mod__Mod_param_L_LC.eh = 0
.no_dead_strip _Mod__Mod_param_L_LC.eh
_Mod__uMod_var_L_I.eh = 0
.no_dead_strip _Mod__uMod_var_L_I.eh
_Mod__Mod_var_L_I.eh = 0
.no_dead_strip _Mod__Mod_var_L_I.eh
_Mod__uMod_param_L_I.eh = 0
.no_dead_strip _Mod__uMod_param_L_I.eh
_Mod__Mod_param_L_I.eh = 0
.no_dead_strip _Mod__Mod_param_L_I.eh
_Mod__uMod_var_L_i16.eh = 0
.no_dead_strip _Mod__uMod_var_L_i16.eh
_Mod__Mod_var_L_i16.eh = 0
.no_dead_strip _Mod__Mod_var_L_i16.eh
_Mod__uMod_param_L_i16.eh = 0
.no_dead_strip _Mod__uMod_param_L_i16.eh
_Mod__Mod_param_L_i16.eh = 0
.no_dead_strip _Mod__Mod_param_L_i16.eh
_Mod__uMod_var_L_u64.eh = 0
.no_dead_strip _Mod__uMod_var_L_u64.eh
_Mod__Mod_var_L_u64.eh = 0
.no_dead_strip _Mod__Mod_var_L_u64.eh
_Mod__uMod_param_L_u64.eh = 0
.no_dead_strip _Mod__uMod_param_L_u64.eh
_Mod__Mod_param_L_u64.eh = 0
.no_dead_strip _Mod__Mod_param_L_u64.eh
_Mod__uMod_var_L_i32.eh = 0
.no_dead_strip _Mod__uMod_var_L_i32.eh
_Mod__Mod_var_L_i32.eh = 0
.no_dead_strip _Mod__Mod_var_L_i32.eh
_Mod__uMod_param_L_i32.eh = 0
.no_dead_strip _Mod__uMod_param_L_i32.eh
_Mod__Mod_param_L_i32.eh = 0
.no_dead_strip _Mod__Mod_param_L_i32.eh
_Mod__uMod_var_L_C.eh = 0
.no_dead_strip _Mod__uMod_var_L_C.eh
_Mod__Mod_var_L_C.eh = 0
.no_dead_strip _Mod__Mod_var_L_C.eh
_Mod__uMod_param_L_C.eh = 0
.no_dead_strip _Mod__uMod_param_L_C.eh
_Mod__Mod_param_L_C.eh = 0
.no_dead_strip _Mod__Mod_param_L_C.eh
_Mod__uMod_var_L_u8.eh = 0
.no_dead_strip _Mod__uMod_var_L_u8.eh
_Mod__Mod_var_L_u8.eh = 0
.no_dead_strip _Mod__Mod_var_L_u8.eh
_Mod__uMod_param_L_u8.eh = 0
.no_dead_strip _Mod__uMod_param_L_u8.eh
_Mod__Mod_param_L_u8.eh = 0
.no_dead_strip _Mod__Mod_param_L_u8.eh
_Mod__uMod_var_L_i64.eh = 0
.no_dead_strip _Mod__uMod_var_L_i64.eh
_Mod__Mod_var_L_i64.eh = 0
.no_dead_strip _Mod__Mod_var_L_i64.eh
_Mod__uMod_param_L_i64.eh = 0
.no_dead_strip _Mod__uMod_param_L_i64.eh
_Mod__Mod_param_L_i64.eh = 0
.no_dead_strip _Mod__Mod_param_L_i64.eh
_Mod__uMod_var_L_L.eh = 0
.no_dead_strip _Mod__uMod_var_L_L.eh
_Mod__Mod_var_L_L.eh = 0
.no_dead_strip _Mod__Mod_var_L_L.eh
_Mod__uMod_param_L_L.eh = 0
.no_dead_strip _Mod__uMod_param_L_L.eh
_Mod__Mod_param_L_L.eh = 0
.no_dead_strip _Mod__Mod_param_L_L.eh
_Mod__uMod_var_L_i8.eh = 0
.no_dead_strip _Mod__uMod_var_L_i8.eh
_Mod__Mod_var_L_i8.eh = 0
.no_dead_strip _Mod__Mod_var_L_i8.eh
_Mod__uMod_param_L_i8.eh = 0
.no_dead_strip _Mod__uMod_param_L_i8.eh
_Mod__Mod_param_L_i8.eh = 0
.no_dead_strip _Mod__Mod_param_L_i8.eh
_Mod__uMod_var_i8_u16.eh = 0
.no_dead_strip _Mod__uMod_var_i8_u16.eh
_Mod__uMod_param_i8_u16.eh = 0
.no_dead_strip _Mod__uMod_param_i8_u16.eh
_Mod__uMod_var_i8_u32.eh = 0
.no_dead_strip _Mod__uMod_var_i8_u32.eh
_Mod__uMod_param_i8_u32.eh = 0
.no_dead_strip _Mod__uMod_param_i8_u32.eh
_Mod__uMod_var_i8_LC.eh = 0
.no_dead_strip _Mod__uMod_var_i8_LC.eh
_Mod__Mod_var_i8_LC.eh = 0
.no_dead_strip _Mod__Mod_var_i8_LC.eh
_Mod__uMod_param_i8_LC.eh = 0
.no_dead_strip _Mod__uMod_param_i8_LC.eh
_Mod__Mod_param_i8_LC.eh = 0
.no_dead_strip _Mod__Mod_param_i8_LC.eh
_Mod__uMod_var_i8_I.eh = 0
.no_dead_strip _Mod__uMod_var_i8_I.eh
_Mod__uMod_param_i8_I.eh = 0
.no_dead_strip _Mod__uMod_param_i8_I.eh
_Mod__uMod_var_i8_i16.eh = 0
.no_dead_strip _Mod__uMod_var_i8_i16.eh
_Mod__uMod_param_i8_i16.eh = 0
.no_dead_strip _Mod__uMod_param_i8_i16.eh
_Mod__uMod_var_i8_u64.eh = 0
.no_dead_strip _Mod__uMod_var_i8_u64.eh
_Mod__Mod_var_i8_u64.eh = 0
.no_dead_strip _Mod__Mod_var_i8_u64.eh
_Mod__uMod_param_i8_u64.eh = 0
.no_dead_strip _Mod__uMod_param_i8_u64.eh
_Mod__Mod_param_i8_u64.eh = 0
.no_dead_strip _Mod__Mod_param_i8_u64.eh
_Mod__uMod_var_i8_i32.eh = 0
.no_dead_strip _Mod__uMod_var_i8_i32.eh
_Mod__uMod_param_i8_i32.eh = 0
.no_dead_strip _Mod__uMod_param_i8_i32.eh
_Mod__uMod_var_i8_C.eh = 0
.no_dead_strip _Mod__uMod_var_i8_C.eh
_Mod__uMod_param_i8_C.eh = 0
.no_dead_strip _Mod__uMod_param_i8_C.eh
_Mod__uMod_var_i8_u8.eh = 0
.no_dead_strip _Mod__uMod_var_i8_u8.eh
_Mod__uMod_param_i8_u8.eh = 0
.no_dead_strip _Mod__uMod_param_i8_u8.eh
_Mod__uMod_var_i8_i64.eh = 0
.no_dead_strip _Mod__uMod_var_i8_i64.eh
_Mod__Mod_var_i8_i64.eh = 0
.no_dead_strip _Mod__Mod_var_i8_i64.eh
_Mod__uMod_param_i8_i64.eh = 0
.no_dead_strip _Mod__uMod_param_i8_i64.eh
_Mod__Mod_param_i8_i64.eh = 0
.no_dead_strip _Mod__Mod_param_i8_i64.eh
_Mod__uMod_var_i8_L.eh = 0
.no_dead_strip _Mod__uMod_var_i8_L.eh
_Mod__Mod_var_i8_L.eh = 0
.no_dead_strip _Mod__Mod_var_i8_L.eh
_Mod__uMod_param_i8_L.eh = 0
.no_dead_strip _Mod__uMod_param_i8_L.eh
_Mod__Mod_param_i8_L.eh = 0
.no_dead_strip _Mod__Mod_param_i8_L.eh
_Mod__uMod_var_i8_i8.eh = 0
.no_dead_strip _Mod__uMod_var_i8_i8.eh
_Mod__uMod_param_i8_i8.eh = 0
.no_dead_strip _Mod__uMod_param_i8_i8.eh
.globl _Mod_M3.eh
_Mod_M3.eh = 0
.no_dead_strip _Mod_M3.eh
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
_Mod__Mod_var_u16_u32.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB5-.
	.set L$set$2,LFE5-LFB5
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI17-LFB5
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI18-LCFI17
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$5,LCFI20-LCFI18
	.long L$set$5
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$6,LCFI21-LCFI20
	.long L$set$6
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE1:
_Mod__Mod_param_u16_u32.eh:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB7-.
	.set L$set$8,LFE7-LFB7
	.quad L$set$8
	.byte	0x0
	.byte	0x4
	.set L$set$9,LCFI25-LFB7
	.long L$set$9
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$10,LCFI26-LCFI25
	.long L$set$10
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$11,LCFI28-LCFI26
	.long L$set$11
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$12,LCFI29-LCFI28
	.long L$set$12
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE3:
_Mod__Mod_var_u16_I.eh:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB13-.
	.set L$set$14,LFE13-LFB13
	.quad L$set$14
	.byte	0x0
	.byte	0x4
	.set L$set$15,LCFI47-LFB13
	.long L$set$15
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$16,LCFI48-LCFI47
	.long L$set$16
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$17,LCFI50-LCFI48
	.long L$set$17
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$18,LCFI51-LCFI50
	.long L$set$18
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE5:
_Mod__Mod_param_u16_I.eh:
LSFDE7:
	.set L$set$19,LEFDE7-LASFDE7
	.long L$set$19
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB15-.
	.set L$set$20,LFE15-LFB15
	.quad L$set$20
	.byte	0x0
	.byte	0x4
	.set L$set$21,LCFI55-LFB15
	.long L$set$21
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$22,LCFI56-LCFI55
	.long L$set$22
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$23,LCFI58-LCFI56
	.long L$set$23
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE7:
_Mod__Mod_var_u16_i16.eh:
LSFDE9:
	.set L$set$24,LEFDE9-LASFDE9
	.long L$set$24
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB17-.
	.set L$set$25,LFE17-LFB17
	.quad L$set$25
	.byte	0x0
	.byte	0x4
	.set L$set$26,LCFI62-LFB17
	.long L$set$26
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$27,LCFI63-LCFI62
	.long L$set$27
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$28,LCFI65-LCFI63
	.long L$set$28
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$29,LCFI66-LCFI65
	.long L$set$29
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE9:
_Mod__Mod_param_u16_i16.eh:
LSFDE11:
	.set L$set$30,LEFDE11-LASFDE11
	.long L$set$30
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB19-.
	.set L$set$31,LFE19-LFB19
	.quad L$set$31
	.byte	0x0
	.byte	0x4
	.set L$set$32,LCFI70-LFB19
	.long L$set$32
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$33,LCFI71-LCFI70
	.long L$set$33
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$34,LCFI73-LCFI71
	.long L$set$34
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$35,LCFI74-LCFI73
	.long L$set$35
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE11:
_Mod__Mod_var_u16_i32.eh:
LSFDE13:
	.set L$set$36,LEFDE13-LASFDE13
	.long L$set$36
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB25-.
	.set L$set$37,LFE25-LFB25
	.quad L$set$37
	.byte	0x0
	.byte	0x4
	.set L$set$38,LCFI91-LFB25
	.long L$set$38
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$39,LCFI92-LCFI91
	.long L$set$39
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$40,LCFI94-LCFI92
	.long L$set$40
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$41,LCFI95-LCFI94
	.long L$set$41
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE13:
_Mod__Mod_param_u16_i32.eh:
LSFDE15:
	.set L$set$42,LEFDE15-LASFDE15
	.long L$set$42
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB27-.
	.set L$set$43,LFE27-LFB27
	.quad L$set$43
	.byte	0x0
	.byte	0x4
	.set L$set$44,LCFI99-LFB27
	.long L$set$44
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$45,LCFI100-LCFI99
	.long L$set$45
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$46,LCFI102-LCFI100
	.long L$set$46
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$47,LCFI103-LCFI102
	.long L$set$47
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE15:
_Mod__Mod_var_u16_C.eh:
LSFDE17:
	.set L$set$48,LEFDE17-LASFDE17
	.long L$set$48
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB29-.
	.set L$set$49,LFE29-LFB29
	.quad L$set$49
	.byte	0x0
	.byte	0x4
	.set L$set$50,LCFI107-LFB29
	.long L$set$50
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$51,LCFI108-LCFI107
	.long L$set$51
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$52,LCFI110-LCFI108
	.long L$set$52
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$53,LCFI111-LCFI110
	.long L$set$53
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE17:
_Mod__Mod_param_u16_C.eh:
LSFDE19:
	.set L$set$54,LEFDE19-LASFDE19
	.long L$set$54
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB31-.
	.set L$set$55,LFE31-LFB31
	.quad L$set$55
	.byte	0x0
	.byte	0x4
	.set L$set$56,LCFI115-LFB31
	.long L$set$56
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$57,LCFI116-LCFI115
	.long L$set$57
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$58,LCFI118-LCFI116
	.long L$set$58
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$59,LCFI119-LCFI118
	.long L$set$59
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE19:
_Mod__Mod_var_u16_i8.eh:
LSFDE21:
	.set L$set$60,LEFDE21-LASFDE21
	.long L$set$60
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB45-.
	.set L$set$61,LFE45-LFB45
	.quad L$set$61
	.byte	0x0
	.byte	0x4
	.set L$set$62,LCFI163-LFB45
	.long L$set$62
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$63,LCFI164-LCFI163
	.long L$set$63
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$64,LCFI166-LCFI164
	.long L$set$64
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$65,LCFI167-LCFI166
	.long L$set$65
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE21:
_Mod__Mod_param_u16_i8.eh:
LSFDE23:
	.set L$set$66,LEFDE23-LASFDE23
	.long L$set$66
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB47-.
	.set L$set$67,LFE47-LFB47
	.quad L$set$67
	.byte	0x0
	.byte	0x4
	.set L$set$68,LCFI171-LFB47
	.long L$set$68
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$69,LCFI172-LCFI171
	.long L$set$69
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$70,LCFI174-LCFI172
	.long L$set$70
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$71,LCFI175-LCFI174
	.long L$set$71
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE23:
_Mod__Mod_var_u32_I.eh:
LSFDE25:
	.set L$set$72,LEFDE25-LASFDE25
	.long L$set$72
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB61-.
	.set L$set$73,LFE61-LFB61
	.quad L$set$73
	.byte	0x0
	.byte	0x4
	.set L$set$74,LCFI221-LFB61
	.long L$set$74
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$75,LCFI222-LCFI221
	.long L$set$75
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$76,LCFI224-LCFI222
	.long L$set$76
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$77,LCFI225-LCFI224
	.long L$set$77
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE25:
_Mod__Mod_param_u32_I.eh:
LSFDE27:
	.set L$set$78,LEFDE27-LASFDE27
	.long L$set$78
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB63-.
	.set L$set$79,LFE63-LFB63
	.quad L$set$79
	.byte	0x0
	.byte	0x4
	.set L$set$80,LCFI229-LFB63
	.long L$set$80
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$81,LCFI230-LCFI229
	.long L$set$81
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$82,LCFI232-LCFI230
	.long L$set$82
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE27:
_Mod__Mod_var_u32_i16.eh:
LSFDE29:
	.set L$set$83,LEFDE29-LASFDE29
	.long L$set$83
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB65-.
	.set L$set$84,LFE65-LFB65
	.quad L$set$84
	.byte	0x0
	.byte	0x4
	.set L$set$85,LCFI236-LFB65
	.long L$set$85
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$86,LCFI237-LCFI236
	.long L$set$86
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$87,LCFI239-LCFI237
	.long L$set$87
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$88,LCFI240-LCFI239
	.long L$set$88
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE29:
_Mod__Mod_param_u32_i16.eh:
LSFDE31:
	.set L$set$89,LEFDE31-LASFDE31
	.long L$set$89
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB67-.
	.set L$set$90,LFE67-LFB67
	.quad L$set$90
	.byte	0x0
	.byte	0x4
	.set L$set$91,LCFI244-LFB67
	.long L$set$91
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$92,LCFI245-LCFI244
	.long L$set$92
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$93,LCFI247-LCFI245
	.long L$set$93
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$94,LCFI248-LCFI247
	.long L$set$94
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE31:
_Mod__Mod_var_u32_i32.eh:
LSFDE33:
	.set L$set$95,LEFDE33-LASFDE33
	.long L$set$95
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB73-.
	.set L$set$96,LFE73-LFB73
	.quad L$set$96
	.byte	0x0
	.byte	0x4
	.set L$set$97,LCFI265-LFB73
	.long L$set$97
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$98,LCFI266-LCFI265
	.long L$set$98
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$99,LCFI268-LCFI266
	.long L$set$99
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$100,LCFI269-LCFI268
	.long L$set$100
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE33:
_Mod__Mod_param_u32_i32.eh:
LSFDE35:
	.set L$set$101,LEFDE35-LASFDE35
	.long L$set$101
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB75-.
	.set L$set$102,LFE75-LFB75
	.quad L$set$102
	.byte	0x0
	.byte	0x4
	.set L$set$103,LCFI273-LFB75
	.long L$set$103
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$104,LCFI274-LCFI273
	.long L$set$104
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$105,LCFI276-LCFI274
	.long L$set$105
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$106,LCFI277-LCFI276
	.long L$set$106
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE35:
_Mod__Mod_var_u32_C.eh:
LSFDE37:
	.set L$set$107,LEFDE37-LASFDE37
	.long L$set$107
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB77-.
	.set L$set$108,LFE77-LFB77
	.quad L$set$108
	.byte	0x0
	.byte	0x4
	.set L$set$109,LCFI281-LFB77
	.long L$set$109
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$110,LCFI282-LCFI281
	.long L$set$110
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$111,LCFI284-LCFI282
	.long L$set$111
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$112,LCFI285-LCFI284
	.long L$set$112
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE37:
_Mod__Mod_param_u32_C.eh:
LSFDE39:
	.set L$set$113,LEFDE39-LASFDE39
	.long L$set$113
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB79-.
	.set L$set$114,LFE79-LFB79
	.quad L$set$114
	.byte	0x0
	.byte	0x4
	.set L$set$115,LCFI289-LFB79
	.long L$set$115
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$116,LCFI290-LCFI289
	.long L$set$116
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$117,LCFI292-LCFI290
	.long L$set$117
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$118,LCFI293-LCFI292
	.long L$set$118
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE39:
_Mod__Mod_var_u32_i8.eh:
LSFDE41:
	.set L$set$119,LEFDE41-LASFDE41
	.long L$set$119
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB93-.
	.set L$set$120,LFE93-LFB93
	.quad L$set$120
	.byte	0x0
	.byte	0x4
	.set L$set$121,LCFI337-LFB93
	.long L$set$121
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$122,LCFI338-LCFI337
	.long L$set$122
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$123,LCFI340-LCFI338
	.long L$set$123
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$124,LCFI341-LCFI340
	.long L$set$124
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE41:
_Mod__Mod_param_u32_i8.eh:
LSFDE43:
	.set L$set$125,LEFDE43-LASFDE43
	.long L$set$125
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB95-.
	.set L$set$126,LFE95-LFB95
	.quad L$set$126
	.byte	0x0
	.byte	0x4
	.set L$set$127,LCFI345-LFB95
	.long L$set$127
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$128,LCFI346-LCFI345
	.long L$set$128
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$129,LCFI348-LCFI346
	.long L$set$129
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$130,LCFI349-LCFI348
	.long L$set$130
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE43:
_Mod__Mod_var_i16_u16.eh:
LSFDE45:
	.set L$set$131,LEFDE45-LASFDE45
	.long L$set$131
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB193-.
	.set L$set$132,LFE193-LFB193
	.quad L$set$132
	.byte	0x0
	.byte	0x4
	.set L$set$133,LCFI681-LFB193
	.long L$set$133
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$134,LCFI682-LCFI681
	.long L$set$134
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$135,LCFI684-LCFI682
	.long L$set$135
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$136,LCFI685-LCFI684
	.long L$set$136
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE45:
_Mod__Mod_param_i16_u16.eh:
LSFDE47:
	.set L$set$137,LEFDE47-LASFDE47
	.long L$set$137
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB195-.
	.set L$set$138,LFE195-LFB195
	.quad L$set$138
	.byte	0x0
	.byte	0x4
	.set L$set$139,LCFI689-LFB195
	.long L$set$139
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$140,LCFI690-LCFI689
	.long L$set$140
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$141,LCFI692-LCFI690
	.long L$set$141
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$142,LCFI693-LCFI692
	.long L$set$142
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE47:
_Mod__Mod_var_i16_u32.eh:
LSFDE49:
	.set L$set$143,LEFDE49-LASFDE49
	.long L$set$143
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB197-.
	.set L$set$144,LFE197-LFB197
	.quad L$set$144
	.byte	0x0
	.byte	0x4
	.set L$set$145,LCFI697-LFB197
	.long L$set$145
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$146,LCFI698-LCFI697
	.long L$set$146
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$147,LCFI700-LCFI698
	.long L$set$147
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$148,LCFI701-LCFI700
	.long L$set$148
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
_Mod__Mod_param_i16_u32.eh:
LSFDE51:
	.set L$set$149,LEFDE51-LASFDE51
	.long L$set$149
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB199-.
	.set L$set$150,LFE199-LFB199
	.quad L$set$150
	.byte	0x0
	.byte	0x4
	.set L$set$151,LCFI705-LFB199
	.long L$set$151
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$152,LCFI706-LCFI705
	.long L$set$152
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$153,LCFI708-LCFI706
	.long L$set$153
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$154,LCFI709-LCFI708
	.long L$set$154
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE51:
_Mod__Mod_var_i16_I.eh:
LSFDE53:
	.set L$set$155,LEFDE53-LASFDE53
	.long L$set$155
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB205-.
	.set L$set$156,LFE205-LFB205
	.quad L$set$156
	.byte	0x0
	.byte	0x4
	.set L$set$157,LCFI727-LFB205
	.long L$set$157
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$158,LCFI728-LCFI727
	.long L$set$158
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$159,LCFI730-LCFI728
	.long L$set$159
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$160,LCFI731-LCFI730
	.long L$set$160
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE53:
_Mod__Mod_param_i16_I.eh:
LSFDE55:
	.set L$set$161,LEFDE55-LASFDE55
	.long L$set$161
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB207-.
	.set L$set$162,LFE207-LFB207
	.quad L$set$162
	.byte	0x0
	.byte	0x4
	.set L$set$163,LCFI735-LFB207
	.long L$set$163
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$164,LCFI736-LCFI735
	.long L$set$164
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$165,LCFI738-LCFI736
	.long L$set$165
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE55:
_Mod__Mod_var_i16_i16.eh:
LSFDE57:
	.set L$set$166,LEFDE57-LASFDE57
	.long L$set$166
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB209-.
	.set L$set$167,LFE209-LFB209
	.quad L$set$167
	.byte	0x0
	.byte	0x4
	.set L$set$168,LCFI742-LFB209
	.long L$set$168
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$169,LCFI743-LCFI742
	.long L$set$169
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$170,LCFI745-LCFI743
	.long L$set$170
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$171,LCFI746-LCFI745
	.long L$set$171
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE57:
_Mod__Mod_param_i16_i16.eh:
LSFDE59:
	.set L$set$172,LEFDE59-LASFDE59
	.long L$set$172
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB211-.
	.set L$set$173,LFE211-LFB211
	.quad L$set$173
	.byte	0x0
	.byte	0x4
	.set L$set$174,LCFI750-LFB211
	.long L$set$174
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$175,LCFI751-LCFI750
	.long L$set$175
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$176,LCFI753-LCFI751
	.long L$set$176
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$177,LCFI754-LCFI753
	.long L$set$177
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE59:
_Mod__Mod_var_i16_i32.eh:
LSFDE61:
	.set L$set$178,LEFDE61-LASFDE61
	.long L$set$178
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB217-.
	.set L$set$179,LFE217-LFB217
	.quad L$set$179
	.byte	0x0
	.byte	0x4
	.set L$set$180,LCFI771-LFB217
	.long L$set$180
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$181,LCFI772-LCFI771
	.long L$set$181
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$182,LCFI774-LCFI772
	.long L$set$182
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$183,LCFI775-LCFI774
	.long L$set$183
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE61:
_Mod__Mod_param_i16_i32.eh:
LSFDE63:
	.set L$set$184,LEFDE63-LASFDE63
	.long L$set$184
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB219-.
	.set L$set$185,LFE219-LFB219
	.quad L$set$185
	.byte	0x0
	.byte	0x4
	.set L$set$186,LCFI779-LFB219
	.long L$set$186
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$187,LCFI780-LCFI779
	.long L$set$187
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$188,LCFI782-LCFI780
	.long L$set$188
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$189,LCFI783-LCFI782
	.long L$set$189
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE63:
_Mod__Mod_var_i16_C.eh:
LSFDE65:
	.set L$set$190,LEFDE65-LASFDE65
	.long L$set$190
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB221-.
	.set L$set$191,LFE221-LFB221
	.quad L$set$191
	.byte	0x0
	.byte	0x4
	.set L$set$192,LCFI787-LFB221
	.long L$set$192
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$193,LCFI788-LCFI787
	.long L$set$193
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$194,LCFI790-LCFI788
	.long L$set$194
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$195,LCFI791-LCFI790
	.long L$set$195
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE65:
_Mod__Mod_param_i16_C.eh:
LSFDE67:
	.set L$set$196,LEFDE67-LASFDE67
	.long L$set$196
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB223-.
	.set L$set$197,LFE223-LFB223
	.quad L$set$197
	.byte	0x0
	.byte	0x4
	.set L$set$198,LCFI795-LFB223
	.long L$set$198
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$199,LCFI796-LCFI795
	.long L$set$199
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$200,LCFI798-LCFI796
	.long L$set$200
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$201,LCFI799-LCFI798
	.long L$set$201
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE67:
_Mod__Mod_var_i16_i8.eh:
LSFDE69:
	.set L$set$202,LEFDE69-LASFDE69
	.long L$set$202
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB237-.
	.set L$set$203,LFE237-LFB237
	.quad L$set$203
	.byte	0x0
	.byte	0x4
	.set L$set$204,LCFI843-LFB237
	.long L$set$204
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$205,LCFI844-LCFI843
	.long L$set$205
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$206,LCFI846-LCFI844
	.long L$set$206
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$207,LCFI847-LCFI846
	.long L$set$207
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE69:
_Mod__Mod_param_i16_i8.eh:
LSFDE71:
	.set L$set$208,LEFDE71-LASFDE71
	.long L$set$208
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB239-.
	.set L$set$209,LFE239-LFB239
	.quad L$set$209
	.byte	0x0
	.byte	0x4
	.set L$set$210,LCFI851-LFB239
	.long L$set$210
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$211,LCFI852-LCFI851
	.long L$set$211
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$212,LCFI854-LCFI852
	.long L$set$212
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$213,LCFI855-LCFI854
	.long L$set$213
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE71:
_Mod__Mod_var_i32_u32.eh:
LSFDE73:
	.set L$set$214,LEFDE73-LASFDE73
	.long L$set$214
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB295-.
	.set L$set$215,LFE295-LFB295
	.quad L$set$215
	.byte	0x0
	.byte	0x4
	.set L$set$216,LCFI1043-LFB295
	.long L$set$216
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$217,LCFI1044-LCFI1043
	.long L$set$217
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$218,LCFI1046-LCFI1044
	.long L$set$218
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$219,LCFI1047-LCFI1046
	.long L$set$219
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE73:
_Mod__Mod_param_i32_u32.eh:
LSFDE75:
	.set L$set$220,LEFDE75-LASFDE75
	.long L$set$220
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB297-.
	.set L$set$221,LFE297-LFB297
	.quad L$set$221
	.byte	0x0
	.byte	0x4
	.set L$set$222,LCFI1051-LFB297
	.long L$set$222
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$223,LCFI1052-LCFI1051
	.long L$set$223
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$224,LCFI1054-LCFI1052
	.long L$set$224
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$225,LCFI1055-LCFI1054
	.long L$set$225
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE75:
_Mod__Mod_var_i32_I.eh:
LSFDE77:
	.set L$set$226,LEFDE77-LASFDE77
	.long L$set$226
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB303-.
	.set L$set$227,LFE303-LFB303
	.quad L$set$227
	.byte	0x0
	.byte	0x4
	.set L$set$228,LCFI1073-LFB303
	.long L$set$228
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$229,LCFI1074-LCFI1073
	.long L$set$229
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$230,LCFI1076-LCFI1074
	.long L$set$230
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$231,LCFI1077-LCFI1076
	.long L$set$231
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE77:
_Mod__Mod_param_i32_I.eh:
LSFDE79:
	.set L$set$232,LEFDE79-LASFDE79
	.long L$set$232
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB305-.
	.set L$set$233,LFE305-LFB305
	.quad L$set$233
	.byte	0x0
	.byte	0x4
	.set L$set$234,LCFI1081-LFB305
	.long L$set$234
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$235,LCFI1082-LCFI1081
	.long L$set$235
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$236,LCFI1084-LCFI1082
	.long L$set$236
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE79:
_Mod__Mod_var_i32_i16.eh:
LSFDE81:
	.set L$set$237,LEFDE81-LASFDE81
	.long L$set$237
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB307-.
	.set L$set$238,LFE307-LFB307
	.quad L$set$238
	.byte	0x0
	.byte	0x4
	.set L$set$239,LCFI1088-LFB307
	.long L$set$239
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$240,LCFI1089-LCFI1088
	.long L$set$240
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$241,LCFI1091-LCFI1089
	.long L$set$241
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$242,LCFI1092-LCFI1091
	.long L$set$242
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE81:
_Mod__Mod_param_i32_i16.eh:
LSFDE83:
	.set L$set$243,LEFDE83-LASFDE83
	.long L$set$243
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB309-.
	.set L$set$244,LFE309-LFB309
	.quad L$set$244
	.byte	0x0
	.byte	0x4
	.set L$set$245,LCFI1096-LFB309
	.long L$set$245
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$246,LCFI1097-LCFI1096
	.long L$set$246
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$247,LCFI1099-LCFI1097
	.long L$set$247
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$248,LCFI1100-LCFI1099
	.long L$set$248
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE83:
_Mod__Mod_var_i32_i32.eh:
LSFDE85:
	.set L$set$249,LEFDE85-LASFDE85
	.long L$set$249
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB315-.
	.set L$set$250,LFE315-LFB315
	.quad L$set$250
	.byte	0x0
	.byte	0x4
	.set L$set$251,LCFI1117-LFB315
	.long L$set$251
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$252,LCFI1118-LCFI1117
	.long L$set$252
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$253,LCFI1120-LCFI1118
	.long L$set$253
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$254,LCFI1121-LCFI1120
	.long L$set$254
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE85:
_Mod__Mod_param_i32_i32.eh:
LSFDE87:
	.set L$set$255,LEFDE87-LASFDE87
	.long L$set$255
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB317-.
	.set L$set$256,LFE317-LFB317
	.quad L$set$256
	.byte	0x0
	.byte	0x4
	.set L$set$257,LCFI1125-LFB317
	.long L$set$257
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$258,LCFI1126-LCFI1125
	.long L$set$258
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$259,LCFI1128-LCFI1126
	.long L$set$259
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$260,LCFI1129-LCFI1128
	.long L$set$260
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE87:
_Mod__Mod_var_i32_C.eh:
LSFDE89:
	.set L$set$261,LEFDE89-LASFDE89
	.long L$set$261
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB319-.
	.set L$set$262,LFE319-LFB319
	.quad L$set$262
	.byte	0x0
	.byte	0x4
	.set L$set$263,LCFI1133-LFB319
	.long L$set$263
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$264,LCFI1134-LCFI1133
	.long L$set$264
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$265,LCFI1136-LCFI1134
	.long L$set$265
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$266,LCFI1137-LCFI1136
	.long L$set$266
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE89:
_Mod__Mod_param_i32_C.eh:
LSFDE91:
	.set L$set$267,LEFDE91-LASFDE91
	.long L$set$267
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB321-.
	.set L$set$268,LFE321-LFB321
	.quad L$set$268
	.byte	0x0
	.byte	0x4
	.set L$set$269,LCFI1141-LFB321
	.long L$set$269
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$270,LCFI1142-LCFI1141
	.long L$set$270
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$271,LCFI1144-LCFI1142
	.long L$set$271
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$272,LCFI1145-LCFI1144
	.long L$set$272
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE91:
_Mod__Mod_var_i32_i8.eh:
LSFDE93:
	.set L$set$273,LEFDE93-LASFDE93
	.long L$set$273
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB335-.
	.set L$set$274,LFE335-LFB335
	.quad L$set$274
	.byte	0x0
	.byte	0x4
	.set L$set$275,LCFI1189-LFB335
	.long L$set$275
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$276,LCFI1190-LCFI1189
	.long L$set$276
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$277,LCFI1192-LCFI1190
	.long L$set$277
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$278,LCFI1193-LCFI1192
	.long L$set$278
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE93:
_Mod__Mod_param_i32_i8.eh:
LSFDE95:
	.set L$set$279,LEFDE95-LASFDE95
	.long L$set$279
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB337-.
	.set L$set$280,LFE337-LFB337
	.quad L$set$280
	.byte	0x0
	.byte	0x4
	.set L$set$281,LCFI1197-LFB337
	.long L$set$281
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$282,LCFI1198-LCFI1197
	.long L$set$282
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$283,LCFI1200-LCFI1198
	.long L$set$283
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$284,LCFI1201-LCFI1200
	.long L$set$284
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE95:
_Mod__Mod_var_C_I.eh:
LSFDE97:
	.set L$set$285,LEFDE97-LASFDE97
	.long L$set$285
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB351-.
	.set L$set$286,LFE351-LFB351
	.quad L$set$286
	.byte	0x0
	.byte	0x4
	.set L$set$287,LCFI1247-LFB351
	.long L$set$287
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$288,LCFI1248-LCFI1247
	.long L$set$288
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$289,LCFI1250-LCFI1248
	.long L$set$289
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$290,LCFI1251-LCFI1250
	.long L$set$290
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE97:
_Mod__Mod_param_C_I.eh:
LSFDE99:
	.set L$set$291,LEFDE99-LASFDE99
	.long L$set$291
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB353-.
	.set L$set$292,LFE353-LFB353
	.quad L$set$292
	.byte	0x0
	.byte	0x4
	.set L$set$293,LCFI1255-LFB353
	.long L$set$293
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$294,LCFI1256-LCFI1255
	.long L$set$294
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$295,LCFI1258-LCFI1256
	.long L$set$295
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE99:
_Mod__Mod_var_C_i16.eh:
LSFDE101:
	.set L$set$296,LEFDE101-LASFDE101
	.long L$set$296
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB355-.
	.set L$set$297,LFE355-LFB355
	.quad L$set$297
	.byte	0x0
	.byte	0x4
	.set L$set$298,LCFI1262-LFB355
	.long L$set$298
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$299,LCFI1263-LCFI1262
	.long L$set$299
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$300,LCFI1265-LCFI1263
	.long L$set$300
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$301,LCFI1266-LCFI1265
	.long L$set$301
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE101:
_Mod__Mod_param_C_i16.eh:
LSFDE103:
	.set L$set$302,LEFDE103-LASFDE103
	.long L$set$302
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB357-.
	.set L$set$303,LFE357-LFB357
	.quad L$set$303
	.byte	0x0
	.byte	0x4
	.set L$set$304,LCFI1270-LFB357
	.long L$set$304
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$305,LCFI1271-LCFI1270
	.long L$set$305
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$306,LCFI1273-LCFI1271
	.long L$set$306
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$307,LCFI1274-LCFI1273
	.long L$set$307
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE103:
_Mod__Mod_var_C_i32.eh:
LSFDE105:
	.set L$set$308,LEFDE105-LASFDE105
	.long L$set$308
LASFDE105:
	.long	LASFDE105-EH_frame1
	.quad	LFB363-.
	.set L$set$309,LFE363-LFB363
	.quad L$set$309
	.byte	0x0
	.byte	0x4
	.set L$set$310,LCFI1291-LFB363
	.long L$set$310
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$311,LCFI1292-LCFI1291
	.long L$set$311
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$312,LCFI1294-LCFI1292
	.long L$set$312
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$313,LCFI1295-LCFI1294
	.long L$set$313
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE105:
_Mod__Mod_param_C_i32.eh:
LSFDE107:
	.set L$set$314,LEFDE107-LASFDE107
	.long L$set$314
LASFDE107:
	.long	LASFDE107-EH_frame1
	.quad	LFB365-.
	.set L$set$315,LFE365-LFB365
	.quad L$set$315
	.byte	0x0
	.byte	0x4
	.set L$set$316,LCFI1299-LFB365
	.long L$set$316
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$317,LCFI1300-LCFI1299
	.long L$set$317
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$318,LCFI1302-LCFI1300
	.long L$set$318
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$319,LCFI1303-LCFI1302
	.long L$set$319
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE107:
_Mod__Mod_var_C_i8.eh:
LSFDE109:
	.set L$set$320,LEFDE109-LASFDE109
	.long L$set$320
LASFDE109:
	.long	LASFDE109-EH_frame1
	.quad	LFB383-.
	.set L$set$321,LFE383-LFB383
	.quad L$set$321
	.byte	0x0
	.byte	0x4
	.set L$set$322,LCFI1361-LFB383
	.long L$set$322
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$323,LCFI1362-LCFI1361
	.long L$set$323
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$324,LCFI1364-LCFI1362
	.long L$set$324
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$325,LCFI1365-LCFI1364
	.long L$set$325
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE109:
_Mod__Mod_param_C_i8.eh:
LSFDE111:
	.set L$set$326,LEFDE111-LASFDE111
	.long L$set$326
LASFDE111:
	.long	LASFDE111-EH_frame1
	.quad	LFB385-.
	.set L$set$327,LFE385-LFB385
	.quad L$set$327
	.byte	0x0
	.byte	0x4
	.set L$set$328,LCFI1369-LFB385
	.long L$set$328
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$329,LCFI1370-LCFI1369
	.long L$set$329
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$330,LCFI1372-LCFI1370
	.long L$set$330
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$331,LCFI1373-LCFI1372
	.long L$set$331
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE111:
_Mod__Mod_var_u8_u16.eh:
LSFDE113:
	.set L$set$332,LEFDE113-LASFDE113
	.long L$set$332
LASFDE113:
	.long	LASFDE113-EH_frame1
	.quad	LFB387-.
	.set L$set$333,LFE387-LFB387
	.quad L$set$333
	.byte	0x0
	.byte	0x4
	.set L$set$334,LCFI1377-LFB387
	.long L$set$334
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$335,LCFI1378-LCFI1377
	.long L$set$335
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$336,LCFI1380-LCFI1378
	.long L$set$336
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$337,LCFI1381-LCFI1380
	.long L$set$337
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE113:
_Mod__Mod_param_u8_u16.eh:
LSFDE115:
	.set L$set$338,LEFDE115-LASFDE115
	.long L$set$338
LASFDE115:
	.long	LASFDE115-EH_frame1
	.quad	LFB389-.
	.set L$set$339,LFE389-LFB389
	.quad L$set$339
	.byte	0x0
	.byte	0x4
	.set L$set$340,LCFI1385-LFB389
	.long L$set$340
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$341,LCFI1386-LCFI1385
	.long L$set$341
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$342,LCFI1388-LCFI1386
	.long L$set$342
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$343,LCFI1389-LCFI1388
	.long L$set$343
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE115:
_Mod__Mod_var_u8_u32.eh:
LSFDE117:
	.set L$set$344,LEFDE117-LASFDE117
	.long L$set$344
LASFDE117:
	.long	LASFDE117-EH_frame1
	.quad	LFB391-.
	.set L$set$345,LFE391-LFB391
	.quad L$set$345
	.byte	0x0
	.byte	0x4
	.set L$set$346,LCFI1393-LFB391
	.long L$set$346
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$347,LCFI1394-LCFI1393
	.long L$set$347
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$348,LCFI1396-LCFI1394
	.long L$set$348
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$349,LCFI1397-LCFI1396
	.long L$set$349
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE117:
_Mod__Mod_param_u8_u32.eh:
LSFDE119:
	.set L$set$350,LEFDE119-LASFDE119
	.long L$set$350
LASFDE119:
	.long	LASFDE119-EH_frame1
	.quad	LFB393-.
	.set L$set$351,LFE393-LFB393
	.quad L$set$351
	.byte	0x0
	.byte	0x4
	.set L$set$352,LCFI1401-LFB393
	.long L$set$352
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$353,LCFI1402-LCFI1401
	.long L$set$353
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$354,LCFI1404-LCFI1402
	.long L$set$354
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$355,LCFI1405-LCFI1404
	.long L$set$355
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE119:
_Mod__Mod_var_u8_I.eh:
LSFDE121:
	.set L$set$356,LEFDE121-LASFDE121
	.long L$set$356
LASFDE121:
	.long	LASFDE121-EH_frame1
	.quad	LFB399-.
	.set L$set$357,LFE399-LFB399
	.quad L$set$357
	.byte	0x0
	.byte	0x4
	.set L$set$358,LCFI1423-LFB399
	.long L$set$358
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$359,LCFI1424-LCFI1423
	.long L$set$359
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$360,LCFI1426-LCFI1424
	.long L$set$360
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$361,LCFI1427-LCFI1426
	.long L$set$361
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE121:
_Mod__Mod_param_u8_I.eh:
LSFDE123:
	.set L$set$362,LEFDE123-LASFDE123
	.long L$set$362
LASFDE123:
	.long	LASFDE123-EH_frame1
	.quad	LFB401-.
	.set L$set$363,LFE401-LFB401
	.quad L$set$363
	.byte	0x0
	.byte	0x4
	.set L$set$364,LCFI1431-LFB401
	.long L$set$364
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$365,LCFI1432-LCFI1431
	.long L$set$365
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$366,LCFI1434-LCFI1432
	.long L$set$366
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE123:
_Mod__Mod_var_u8_i16.eh:
LSFDE125:
	.set L$set$367,LEFDE125-LASFDE125
	.long L$set$367
LASFDE125:
	.long	LASFDE125-EH_frame1
	.quad	LFB403-.
	.set L$set$368,LFE403-LFB403
	.quad L$set$368
	.byte	0x0
	.byte	0x4
	.set L$set$369,LCFI1438-LFB403
	.long L$set$369
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$370,LCFI1439-LCFI1438
	.long L$set$370
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$371,LCFI1441-LCFI1439
	.long L$set$371
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$372,LCFI1442-LCFI1441
	.long L$set$372
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE125:
_Mod__Mod_param_u8_i16.eh:
LSFDE127:
	.set L$set$373,LEFDE127-LASFDE127
	.long L$set$373
LASFDE127:
	.long	LASFDE127-EH_frame1
	.quad	LFB405-.
	.set L$set$374,LFE405-LFB405
	.quad L$set$374
	.byte	0x0
	.byte	0x4
	.set L$set$375,LCFI1446-LFB405
	.long L$set$375
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$376,LCFI1447-LCFI1446
	.long L$set$376
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$377,LCFI1449-LCFI1447
	.long L$set$377
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$378,LCFI1450-LCFI1449
	.long L$set$378
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE127:
_Mod__Mod_var_u8_i32.eh:
LSFDE129:
	.set L$set$379,LEFDE129-LASFDE129
	.long L$set$379
LASFDE129:
	.long	LASFDE129-EH_frame1
	.quad	LFB411-.
	.set L$set$380,LFE411-LFB411
	.quad L$set$380
	.byte	0x0
	.byte	0x4
	.set L$set$381,LCFI1467-LFB411
	.long L$set$381
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$382,LCFI1468-LCFI1467
	.long L$set$382
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$383,LCFI1470-LCFI1468
	.long L$set$383
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$384,LCFI1471-LCFI1470
	.long L$set$384
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE129:
_Mod__Mod_param_u8_i32.eh:
LSFDE131:
	.set L$set$385,LEFDE131-LASFDE131
	.long L$set$385
LASFDE131:
	.long	LASFDE131-EH_frame1
	.quad	LFB413-.
	.set L$set$386,LFE413-LFB413
	.quad L$set$386
	.byte	0x0
	.byte	0x4
	.set L$set$387,LCFI1475-LFB413
	.long L$set$387
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$388,LCFI1476-LCFI1475
	.long L$set$388
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$389,LCFI1478-LCFI1476
	.long L$set$389
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$390,LCFI1479-LCFI1478
	.long L$set$390
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE131:
_Mod__Mod_var_u8_C.eh:
LSFDE133:
	.set L$set$391,LEFDE133-LASFDE133
	.long L$set$391
LASFDE133:
	.long	LASFDE133-EH_frame1
	.quad	LFB415-.
	.set L$set$392,LFE415-LFB415
	.quad L$set$392
	.byte	0x0
	.byte	0x4
	.set L$set$393,LCFI1483-LFB415
	.long L$set$393
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$394,LCFI1484-LCFI1483
	.long L$set$394
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$395,LCFI1486-LCFI1484
	.long L$set$395
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$396,LCFI1487-LCFI1486
	.long L$set$396
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE133:
_Mod__Mod_param_u8_C.eh:
LSFDE135:
	.set L$set$397,LEFDE135-LASFDE135
	.long L$set$397
LASFDE135:
	.long	LASFDE135-EH_frame1
	.quad	LFB417-.
	.set L$set$398,LFE417-LFB417
	.quad L$set$398
	.byte	0x0
	.byte	0x4
	.set L$set$399,LCFI1491-LFB417
	.long L$set$399
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$400,LCFI1492-LCFI1491
	.long L$set$400
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$401,LCFI1494-LCFI1492
	.long L$set$401
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$402,LCFI1495-LCFI1494
	.long L$set$402
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE135:
_Mod__Mod_var_u8_i8.eh:
LSFDE137:
	.set L$set$403,LEFDE137-LASFDE137
	.long L$set$403
LASFDE137:
	.long	LASFDE137-EH_frame1
	.quad	LFB431-.
	.set L$set$404,LFE431-LFB431
	.quad L$set$404
	.byte	0x0
	.byte	0x4
	.set L$set$405,LCFI1539-LFB431
	.long L$set$405
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$406,LCFI1540-LCFI1539
	.long L$set$406
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$407,LCFI1542-LCFI1540
	.long L$set$407
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$408,LCFI1543-LCFI1542
	.long L$set$408
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE137:
_Mod__Mod_param_u8_i8.eh:
LSFDE139:
	.set L$set$409,LEFDE139-LASFDE139
	.long L$set$409
LASFDE139:
	.long	LASFDE139-EH_frame1
	.quad	LFB433-.
	.set L$set$410,LFE433-LFB433
	.quad L$set$410
	.byte	0x0
	.byte	0x4
	.set L$set$411,LCFI1547-LFB433
	.long L$set$411
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$412,LCFI1548-LCFI1547
	.long L$set$412
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$413,LCFI1550-LCFI1548
	.long L$set$413
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$414,LCFI1551-LCFI1550
	.long L$set$414
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE139:
_Mod__Mod_var_i8_u16.eh:
LSFDE141:
	.set L$set$415,LEFDE141-LASFDE141
	.long L$set$415
LASFDE141:
	.long	LASFDE141-EH_frame1
	.quad	LFB533-.
	.set L$set$416,LFE533-LFB533
	.quad L$set$416
	.byte	0x0
	.byte	0x4
	.set L$set$417,LCFI1889-LFB533
	.long L$set$417
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$418,LCFI1890-LCFI1889
	.long L$set$418
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$419,LCFI1892-LCFI1890
	.long L$set$419
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$420,LCFI1893-LCFI1892
	.long L$set$420
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE141:
_Mod__Mod_param_i8_u16.eh:
LSFDE143:
	.set L$set$421,LEFDE143-LASFDE143
	.long L$set$421
LASFDE143:
	.long	LASFDE143-EH_frame1
	.quad	LFB535-.
	.set L$set$422,LFE535-LFB535
	.quad L$set$422
	.byte	0x0
	.byte	0x4
	.set L$set$423,LCFI1897-LFB535
	.long L$set$423
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$424,LCFI1898-LCFI1897
	.long L$set$424
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$425,LCFI1900-LCFI1898
	.long L$set$425
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$426,LCFI1901-LCFI1900
	.long L$set$426
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE143:
_Mod__Mod_var_i8_u32.eh:
LSFDE145:
	.set L$set$427,LEFDE145-LASFDE145
	.long L$set$427
LASFDE145:
	.long	LASFDE145-EH_frame1
	.quad	LFB537-.
	.set L$set$428,LFE537-LFB537
	.quad L$set$428
	.byte	0x0
	.byte	0x4
	.set L$set$429,LCFI1905-LFB537
	.long L$set$429
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$430,LCFI1906-LCFI1905
	.long L$set$430
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$431,LCFI1908-LCFI1906
	.long L$set$431
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$432,LCFI1909-LCFI1908
	.long L$set$432
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE145:
_Mod__Mod_param_i8_u32.eh:
LSFDE147:
	.set L$set$433,LEFDE147-LASFDE147
	.long L$set$433
LASFDE147:
	.long	LASFDE147-EH_frame1
	.quad	LFB539-.
	.set L$set$434,LFE539-LFB539
	.quad L$set$434
	.byte	0x0
	.byte	0x4
	.set L$set$435,LCFI1913-LFB539
	.long L$set$435
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$436,LCFI1914-LCFI1913
	.long L$set$436
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$437,LCFI1916-LCFI1914
	.long L$set$437
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$438,LCFI1917-LCFI1916
	.long L$set$438
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE147:
_Mod__Mod_var_i8_I.eh:
LSFDE149:
	.set L$set$439,LEFDE149-LASFDE149
	.long L$set$439
LASFDE149:
	.long	LASFDE149-EH_frame1
	.quad	LFB545-.
	.set L$set$440,LFE545-LFB545
	.quad L$set$440
	.byte	0x0
	.byte	0x4
	.set L$set$441,LCFI1935-LFB545
	.long L$set$441
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$442,LCFI1936-LCFI1935
	.long L$set$442
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$443,LCFI1938-LCFI1936
	.long L$set$443
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$444,LCFI1939-LCFI1938
	.long L$set$444
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE149:
_Mod__Mod_param_i8_I.eh:
LSFDE151:
	.set L$set$445,LEFDE151-LASFDE151
	.long L$set$445
LASFDE151:
	.long	LASFDE151-EH_frame1
	.quad	LFB547-.
	.set L$set$446,LFE547-LFB547
	.quad L$set$446
	.byte	0x0
	.byte	0x4
	.set L$set$447,LCFI1943-LFB547
	.long L$set$447
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$448,LCFI1944-LCFI1943
	.long L$set$448
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$449,LCFI1946-LCFI1944
	.long L$set$449
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE151:
_Mod__Mod_var_i8_i16.eh:
LSFDE153:
	.set L$set$450,LEFDE153-LASFDE153
	.long L$set$450
LASFDE153:
	.long	LASFDE153-EH_frame1
	.quad	LFB549-.
	.set L$set$451,LFE549-LFB549
	.quad L$set$451
	.byte	0x0
	.byte	0x4
	.set L$set$452,LCFI1950-LFB549
	.long L$set$452
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$453,LCFI1951-LCFI1950
	.long L$set$453
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$454,LCFI1953-LCFI1951
	.long L$set$454
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$455,LCFI1954-LCFI1953
	.long L$set$455
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE153:
_Mod__Mod_param_i8_i16.eh:
LSFDE155:
	.set L$set$456,LEFDE155-LASFDE155
	.long L$set$456
LASFDE155:
	.long	LASFDE155-EH_frame1
	.quad	LFB551-.
	.set L$set$457,LFE551-LFB551
	.quad L$set$457
	.byte	0x0
	.byte	0x4
	.set L$set$458,LCFI1958-LFB551
	.long L$set$458
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$459,LCFI1959-LCFI1958
	.long L$set$459
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$460,LCFI1961-LCFI1959
	.long L$set$460
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$461,LCFI1962-LCFI1961
	.long L$set$461
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE155:
_Mod__Mod_var_i8_i32.eh:
LSFDE157:
	.set L$set$462,LEFDE157-LASFDE157
	.long L$set$462
LASFDE157:
	.long	LASFDE157-EH_frame1
	.quad	LFB557-.
	.set L$set$463,LFE557-LFB557
	.quad L$set$463
	.byte	0x0
	.byte	0x4
	.set L$set$464,LCFI1979-LFB557
	.long L$set$464
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$465,LCFI1980-LCFI1979
	.long L$set$465
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$466,LCFI1982-LCFI1980
	.long L$set$466
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$467,LCFI1983-LCFI1982
	.long L$set$467
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE157:
_Mod__Mod_param_i8_i32.eh:
LSFDE159:
	.set L$set$468,LEFDE159-LASFDE159
	.long L$set$468
LASFDE159:
	.long	LASFDE159-EH_frame1
	.quad	LFB559-.
	.set L$set$469,LFE559-LFB559
	.quad L$set$469
	.byte	0x0
	.byte	0x4
	.set L$set$470,LCFI1987-LFB559
	.long L$set$470
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$471,LCFI1988-LCFI1987
	.long L$set$471
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$472,LCFI1990-LCFI1988
	.long L$set$472
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$473,LCFI1991-LCFI1990
	.long L$set$473
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE159:
_Mod__Mod_var_i8_C.eh:
LSFDE161:
	.set L$set$474,LEFDE161-LASFDE161
	.long L$set$474
LASFDE161:
	.long	LASFDE161-EH_frame1
	.quad	LFB561-.
	.set L$set$475,LFE561-LFB561
	.quad L$set$475
	.byte	0x0
	.byte	0x4
	.set L$set$476,LCFI1995-LFB561
	.long L$set$476
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$477,LCFI1996-LCFI1995
	.long L$set$477
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$478,LCFI1998-LCFI1996
	.long L$set$478
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$479,LCFI1999-LCFI1998
	.long L$set$479
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE161:
_Mod__Mod_param_i8_C.eh:
LSFDE163:
	.set L$set$480,LEFDE163-LASFDE163
	.long L$set$480
LASFDE163:
	.long	LASFDE163-EH_frame1
	.quad	LFB563-.
	.set L$set$481,LFE563-LFB563
	.quad L$set$481
	.byte	0x0
	.byte	0x4
	.set L$set$482,LCFI2003-LFB563
	.long L$set$482
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$483,LCFI2004-LCFI2003
	.long L$set$483
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$484,LCFI2006-LCFI2004
	.long L$set$484
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$485,LCFI2007-LCFI2006
	.long L$set$485
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE163:
_Mod__Mod_var_i8_u8.eh:
LSFDE165:
	.set L$set$486,LEFDE165-LASFDE165
	.long L$set$486
LASFDE165:
	.long	LASFDE165-EH_frame1
	.quad	LFB565-.
	.set L$set$487,LFE565-LFB565
	.quad L$set$487
	.byte	0x0
	.byte	0x4
	.set L$set$488,LCFI2011-LFB565
	.long L$set$488
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$489,LCFI2012-LCFI2011
	.long L$set$489
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$490,LCFI2014-LCFI2012
	.long L$set$490
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$491,LCFI2015-LCFI2014
	.long L$set$491
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE165:
_Mod__Mod_param_i8_u8.eh:
LSFDE167:
	.set L$set$492,LEFDE167-LASFDE167
	.long L$set$492
LASFDE167:
	.long	LASFDE167-EH_frame1
	.quad	LFB567-.
	.set L$set$493,LFE567-LFB567
	.quad L$set$493
	.byte	0x0
	.byte	0x4
	.set L$set$494,LCFI2019-LFB567
	.long L$set$494
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$495,LCFI2020-LCFI2019
	.long L$set$495
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$496,LCFI2022-LCFI2020
	.long L$set$496
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$497,LCFI2023-LCFI2022
	.long L$set$497
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE167:
_Mod__Mod_var_i8_i8.eh:
LSFDE169:
	.set L$set$498,LEFDE169-LASFDE169
	.long L$set$498
LASFDE169:
	.long	LASFDE169-EH_frame1
	.quad	LFB577-.
	.set L$set$499,LFE577-LFB577
	.quad L$set$499
	.byte	0x0
	.byte	0x4
	.set L$set$500,LCFI2053-LFB577
	.long L$set$500
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$501,LCFI2054-LCFI2053
	.long L$set$501
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$502,LCFI2056-LCFI2054
	.long L$set$502
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$503,LCFI2057-LCFI2056
	.long L$set$503
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE169:
_Mod__Mod_param_i8_i8.eh:
LSFDE171:
	.set L$set$504,LEFDE171-LASFDE171
	.long L$set$504
LASFDE171:
	.long	LASFDE171-EH_frame1
	.quad	LFB579-.
	.set L$set$505,LFE579-LFB579
	.quad L$set$505
	.byte	0x0
	.byte	0x4
	.set L$set$506,LCFI2061-LFB579
	.long L$set$506
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$507,LCFI2062-LCFI2061
	.long L$set$507
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$508,LCFI2064-LCFI2062
	.long L$set$508
	.byte	0x83
	.byte	0x3
	.byte	0x4
	.set L$set$509,LCFI2065-LCFI2064
	.long L$set$509
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE171:
__m3_fault.eh:
LSFDE173:
	.set L$set$510,LEFDE173-LASFDE173
	.long L$set$510
LASFDE173:
	.long	LASFDE173-EH_frame1
	.quad	LFB581-.
	.set L$set$511,LFE581-LFB581
	.quad L$set$511
	.byte	0x0
	.byte	0x4
	.set L$set$512,LCFI2069-LFB581
	.long L$set$512
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$513,LCFI2070-LCFI2069
	.long L$set$513
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$514,LCFI2072-LCFI2070
	.long L$set$514
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE173:
	.subsections_via_symbols
