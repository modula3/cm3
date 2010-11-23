	.text
.no_dead_strip _div_pow2__div_pow2_i32_0
_div_pow2__div_pow2_i32_0:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _div_pow2__div_pow2_i32_1
_div_pow2__div_pow2_i32_1:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	subq	$16, %rsp
LCFI5:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	%rax
	cmpq	$-2147483648, %rax
	jl	L3
	movl	-4(%rbp), %eax
	cltq
	sarq	%rax
	cmpq	$2147483647, %rax
	jle	L4
L3:
	movl	$1825, %edi
	call	__m3_fault
L4:
	movl	-4(%rbp), %eax
	cltq
	sarq	%rax
	leave
LCFI6:
	ret
LFE1:
.no_dead_strip _div_pow2__div_pow2_i32_2
_div_pow2__div_pow2_i32_2:
LFB2:
	pushq	%rbp
LCFI7:
	movq	%rsp, %rbp
LCFI8:
	subq	$16, %rsp
LCFI9:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$2, %rax
	cmpq	$-2147483648, %rax
	jl	L6
	movl	-4(%rbp), %eax
	cltq
	sarq	$2, %rax
	cmpq	$2147483647, %rax
	jle	L7
L6:
	movl	$1889, %edi
	call	__m3_fault
L7:
	movl	-4(%rbp), %eax
	cltq
	sarq	$2, %rax
	leave
LCFI10:
	ret
LFE2:
.no_dead_strip _div_pow2__div_pow2_i32_3
_div_pow2__div_pow2_i32_3:
LFB3:
	pushq	%rbp
LCFI11:
	movq	%rsp, %rbp
LCFI12:
	subq	$16, %rsp
LCFI13:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$3, %rax
	cmpq	$-2147483648, %rax
	jl	L9
	movl	-4(%rbp), %eax
	cltq
	sarq	$3, %rax
	cmpq	$2147483647, %rax
	jle	L10
L9:
	movl	$1953, %edi
	call	__m3_fault
L10:
	movl	-4(%rbp), %eax
	cltq
	sarq	$3, %rax
	leave
LCFI14:
	ret
LFE3:
.no_dead_strip _div_pow2__div_pow2_i32_4
_div_pow2__div_pow2_i32_4:
LFB4:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	subq	$16, %rsp
LCFI17:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$4, %rax
	cmpq	$-2147483648, %rax
	jl	L12
	movl	-4(%rbp), %eax
	cltq
	sarq	$4, %rax
	cmpq	$2147483647, %rax
	jle	L13
L12:
	movl	$2017, %edi
	call	__m3_fault
L13:
	movl	-4(%rbp), %eax
	cltq
	sarq	$4, %rax
	leave
LCFI18:
	ret
LFE4:
.no_dead_strip _div_pow2__div_pow2_i32_5
_div_pow2__div_pow2_i32_5:
LFB5:
	pushq	%rbp
LCFI19:
	movq	%rsp, %rbp
LCFI20:
	subq	$16, %rsp
LCFI21:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$5, %rax
	cmpq	$-2147483648, %rax
	jl	L15
	movl	-4(%rbp), %eax
	cltq
	sarq	$5, %rax
	cmpq	$2147483647, %rax
	jle	L16
L15:
	movl	$2081, %edi
	call	__m3_fault
L16:
	movl	-4(%rbp), %eax
	cltq
	sarq	$5, %rax
	leave
LCFI22:
	ret
LFE5:
.no_dead_strip _div_pow2__div_pow2_i32_6
_div_pow2__div_pow2_i32_6:
LFB6:
	pushq	%rbp
LCFI23:
	movq	%rsp, %rbp
LCFI24:
	subq	$16, %rsp
LCFI25:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$6, %rax
	cmpq	$-2147483648, %rax
	jl	L18
	movl	-4(%rbp), %eax
	cltq
	sarq	$6, %rax
	cmpq	$2147483647, %rax
	jle	L19
L18:
	movl	$2145, %edi
	call	__m3_fault
L19:
	movl	-4(%rbp), %eax
	cltq
	sarq	$6, %rax
	leave
LCFI26:
	ret
LFE6:
.no_dead_strip _div_pow2__div_pow2_i32_7
_div_pow2__div_pow2_i32_7:
LFB7:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	subq	$16, %rsp
LCFI29:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$7, %rax
	cmpq	$-2147483648, %rax
	jl	L21
	movl	-4(%rbp), %eax
	cltq
	sarq	$7, %rax
	cmpq	$2147483647, %rax
	jle	L22
L21:
	movl	$2209, %edi
	call	__m3_fault
L22:
	movl	-4(%rbp), %eax
	cltq
	sarq	$7, %rax
	leave
LCFI30:
	ret
LFE7:
.no_dead_strip _div_pow2__div_pow2_i32_8
_div_pow2__div_pow2_i32_8:
LFB8:
	pushq	%rbp
LCFI31:
	movq	%rsp, %rbp
LCFI32:
	subq	$16, %rsp
LCFI33:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$8, %rax
	cmpq	$-2147483648, %rax
	jl	L24
	movl	-4(%rbp), %eax
	cltq
	sarq	$8, %rax
	cmpq	$2147483647, %rax
	jle	L25
L24:
	movl	$2273, %edi
	call	__m3_fault
L25:
	movl	-4(%rbp), %eax
	cltq
	sarq	$8, %rax
	leave
LCFI34:
	ret
LFE8:
.no_dead_strip _div_pow2__div_pow2_i32_9
_div_pow2__div_pow2_i32_9:
LFB9:
	pushq	%rbp
LCFI35:
	movq	%rsp, %rbp
LCFI36:
	subq	$16, %rsp
LCFI37:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$9, %rax
	cmpq	$-2147483648, %rax
	jl	L27
	movl	-4(%rbp), %eax
	cltq
	sarq	$9, %rax
	cmpq	$2147483647, %rax
	jle	L28
L27:
	movl	$2337, %edi
	call	__m3_fault
L28:
	movl	-4(%rbp), %eax
	cltq
	sarq	$9, %rax
	leave
LCFI38:
	ret
LFE9:
.no_dead_strip _div_pow2__div_pow2_i32_10
_div_pow2__div_pow2_i32_10:
LFB10:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	subq	$16, %rsp
LCFI41:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$10, %rax
	cmpq	$-2147483648, %rax
	jl	L30
	movl	-4(%rbp), %eax
	cltq
	sarq	$10, %rax
	cmpq	$2147483647, %rax
	jle	L31
L30:
	movl	$2401, %edi
	call	__m3_fault
L31:
	movl	-4(%rbp), %eax
	cltq
	sarq	$10, %rax
	leave
LCFI42:
	ret
LFE10:
.no_dead_strip _div_pow2__div_pow2_i32_11
_div_pow2__div_pow2_i32_11:
LFB11:
	pushq	%rbp
LCFI43:
	movq	%rsp, %rbp
LCFI44:
	subq	$16, %rsp
LCFI45:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$11, %rax
	cmpq	$-2147483648, %rax
	jl	L33
	movl	-4(%rbp), %eax
	cltq
	sarq	$11, %rax
	cmpq	$2147483647, %rax
	jle	L34
L33:
	movl	$2465, %edi
	call	__m3_fault
L34:
	movl	-4(%rbp), %eax
	cltq
	sarq	$11, %rax
	leave
LCFI46:
	ret
LFE11:
.no_dead_strip _div_pow2__div_pow2_i32_12
_div_pow2__div_pow2_i32_12:
LFB12:
	pushq	%rbp
LCFI47:
	movq	%rsp, %rbp
LCFI48:
	subq	$16, %rsp
LCFI49:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$12, %rax
	cmpq	$-2147483648, %rax
	jl	L36
	movl	-4(%rbp), %eax
	cltq
	sarq	$12, %rax
	cmpq	$2147483647, %rax
	jle	L37
L36:
	movl	$2529, %edi
	call	__m3_fault
L37:
	movl	-4(%rbp), %eax
	cltq
	sarq	$12, %rax
	leave
LCFI50:
	ret
LFE12:
.no_dead_strip _div_pow2__div_pow2_i32_13
_div_pow2__div_pow2_i32_13:
LFB13:
	pushq	%rbp
LCFI51:
	movq	%rsp, %rbp
LCFI52:
	subq	$16, %rsp
LCFI53:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$13, %rax
	cmpq	$-2147483648, %rax
	jl	L39
	movl	-4(%rbp), %eax
	cltq
	sarq	$13, %rax
	cmpq	$2147483647, %rax
	jle	L40
L39:
	movl	$2593, %edi
	call	__m3_fault
L40:
	movl	-4(%rbp), %eax
	cltq
	sarq	$13, %rax
	leave
LCFI54:
	ret
LFE13:
.no_dead_strip _div_pow2__div_pow2_i32_14
_div_pow2__div_pow2_i32_14:
LFB14:
	pushq	%rbp
LCFI55:
	movq	%rsp, %rbp
LCFI56:
	subq	$16, %rsp
LCFI57:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$14, %rax
	cmpq	$-2147483648, %rax
	jl	L42
	movl	-4(%rbp), %eax
	cltq
	sarq	$14, %rax
	cmpq	$2147483647, %rax
	jle	L43
L42:
	movl	$2657, %edi
	call	__m3_fault
L43:
	movl	-4(%rbp), %eax
	cltq
	sarq	$14, %rax
	leave
LCFI58:
	ret
LFE14:
.no_dead_strip _div_pow2__div_pow2_i32_15
_div_pow2__div_pow2_i32_15:
LFB15:
	pushq	%rbp
LCFI59:
	movq	%rsp, %rbp
LCFI60:
	subq	$16, %rsp
LCFI61:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$15, %rax
	cmpq	$-2147483648, %rax
	jl	L45
	movl	-4(%rbp), %eax
	cltq
	sarq	$15, %rax
	cmpq	$2147483647, %rax
	jle	L46
L45:
	movl	$2721, %edi
	call	__m3_fault
L46:
	movl	-4(%rbp), %eax
	cltq
	sarq	$15, %rax
	leave
LCFI62:
	ret
LFE15:
.no_dead_strip _div_pow2__div_pow2_i32_16
_div_pow2__div_pow2_i32_16:
LFB16:
	pushq	%rbp
LCFI63:
	movq	%rsp, %rbp
LCFI64:
	subq	$16, %rsp
LCFI65:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$16, %rax
	cmpq	$-2147483648, %rax
	jl	L48
	movl	-4(%rbp), %eax
	cltq
	sarq	$16, %rax
	cmpq	$2147483647, %rax
	jle	L49
L48:
	movl	$2785, %edi
	call	__m3_fault
L49:
	movl	-4(%rbp), %eax
	cltq
	sarq	$16, %rax
	leave
LCFI66:
	ret
LFE16:
.no_dead_strip _div_pow2__div_pow2_i32_17
_div_pow2__div_pow2_i32_17:
LFB17:
	pushq	%rbp
LCFI67:
	movq	%rsp, %rbp
LCFI68:
	subq	$16, %rsp
LCFI69:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$17, %rax
	cmpq	$-2147483648, %rax
	jl	L51
	movl	-4(%rbp), %eax
	cltq
	sarq	$17, %rax
	cmpq	$2147483647, %rax
	jle	L52
L51:
	movl	$2849, %edi
	call	__m3_fault
L52:
	movl	-4(%rbp), %eax
	cltq
	sarq	$17, %rax
	leave
LCFI70:
	ret
LFE17:
.no_dead_strip _div_pow2__div_pow2_i32_18
_div_pow2__div_pow2_i32_18:
LFB18:
	pushq	%rbp
LCFI71:
	movq	%rsp, %rbp
LCFI72:
	subq	$16, %rsp
LCFI73:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$18, %rax
	cmpq	$-2147483648, %rax
	jl	L54
	movl	-4(%rbp), %eax
	cltq
	sarq	$18, %rax
	cmpq	$2147483647, %rax
	jle	L55
L54:
	movl	$2913, %edi
	call	__m3_fault
L55:
	movl	-4(%rbp), %eax
	cltq
	sarq	$18, %rax
	leave
LCFI74:
	ret
LFE18:
.no_dead_strip _div_pow2__div_pow2_i32_19
_div_pow2__div_pow2_i32_19:
LFB19:
	pushq	%rbp
LCFI75:
	movq	%rsp, %rbp
LCFI76:
	subq	$16, %rsp
LCFI77:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$19, %rax
	cmpq	$-2147483648, %rax
	jl	L57
	movl	-4(%rbp), %eax
	cltq
	sarq	$19, %rax
	cmpq	$2147483647, %rax
	jle	L58
L57:
	movl	$2977, %edi
	call	__m3_fault
L58:
	movl	-4(%rbp), %eax
	cltq
	sarq	$19, %rax
	leave
LCFI78:
	ret
LFE19:
.no_dead_strip _div_pow2__div_pow2_i32_20
_div_pow2__div_pow2_i32_20:
LFB20:
	pushq	%rbp
LCFI79:
	movq	%rsp, %rbp
LCFI80:
	subq	$16, %rsp
LCFI81:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$20, %rax
	cmpq	$-2147483648, %rax
	jl	L60
	movl	-4(%rbp), %eax
	cltq
	sarq	$20, %rax
	cmpq	$2147483647, %rax
	jle	L61
L60:
	movl	$3041, %edi
	call	__m3_fault
L61:
	movl	-4(%rbp), %eax
	cltq
	sarq	$20, %rax
	leave
LCFI82:
	ret
LFE20:
.no_dead_strip _div_pow2__div_pow2_i32_21
_div_pow2__div_pow2_i32_21:
LFB21:
	pushq	%rbp
LCFI83:
	movq	%rsp, %rbp
LCFI84:
	subq	$16, %rsp
LCFI85:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$21, %rax
	cmpq	$-2147483648, %rax
	jl	L63
	movl	-4(%rbp), %eax
	cltq
	sarq	$21, %rax
	cmpq	$2147483647, %rax
	jle	L64
L63:
	movl	$3105, %edi
	call	__m3_fault
L64:
	movl	-4(%rbp), %eax
	cltq
	sarq	$21, %rax
	leave
LCFI86:
	ret
LFE21:
.no_dead_strip _div_pow2__div_pow2_i32_22
_div_pow2__div_pow2_i32_22:
LFB22:
	pushq	%rbp
LCFI87:
	movq	%rsp, %rbp
LCFI88:
	subq	$16, %rsp
LCFI89:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$22, %rax
	cmpq	$-2147483648, %rax
	jl	L66
	movl	-4(%rbp), %eax
	cltq
	sarq	$22, %rax
	cmpq	$2147483647, %rax
	jle	L67
L66:
	movl	$3169, %edi
	call	__m3_fault
L67:
	movl	-4(%rbp), %eax
	cltq
	sarq	$22, %rax
	leave
LCFI90:
	ret
LFE22:
.no_dead_strip _div_pow2__div_pow2_i32_23
_div_pow2__div_pow2_i32_23:
LFB23:
	pushq	%rbp
LCFI91:
	movq	%rsp, %rbp
LCFI92:
	subq	$16, %rsp
LCFI93:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$23, %rax
	cmpq	$-2147483648, %rax
	jl	L69
	movl	-4(%rbp), %eax
	cltq
	sarq	$23, %rax
	cmpq	$2147483647, %rax
	jle	L70
L69:
	movl	$3233, %edi
	call	__m3_fault
L70:
	movl	-4(%rbp), %eax
	cltq
	sarq	$23, %rax
	leave
LCFI94:
	ret
LFE23:
.no_dead_strip _div_pow2__div_pow2_i32_24
_div_pow2__div_pow2_i32_24:
LFB24:
	pushq	%rbp
LCFI95:
	movq	%rsp, %rbp
LCFI96:
	subq	$16, %rsp
LCFI97:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$24, %rax
	cmpq	$-2147483648, %rax
	jl	L72
	movl	-4(%rbp), %eax
	cltq
	sarq	$24, %rax
	cmpq	$2147483647, %rax
	jle	L73
L72:
	movl	$3297, %edi
	call	__m3_fault
L73:
	movl	-4(%rbp), %eax
	cltq
	sarq	$24, %rax
	leave
LCFI98:
	ret
LFE24:
.no_dead_strip _div_pow2__div_pow2_i32_25
_div_pow2__div_pow2_i32_25:
LFB25:
	pushq	%rbp
LCFI99:
	movq	%rsp, %rbp
LCFI100:
	subq	$16, %rsp
LCFI101:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$25, %rax
	cmpq	$-2147483648, %rax
	jl	L75
	movl	-4(%rbp), %eax
	cltq
	sarq	$25, %rax
	cmpq	$2147483647, %rax
	jle	L76
L75:
	movl	$3361, %edi
	call	__m3_fault
L76:
	movl	-4(%rbp), %eax
	cltq
	sarq	$25, %rax
	leave
LCFI102:
	ret
LFE25:
.no_dead_strip _div_pow2__div_pow2_i32_26
_div_pow2__div_pow2_i32_26:
LFB26:
	pushq	%rbp
LCFI103:
	movq	%rsp, %rbp
LCFI104:
	subq	$16, %rsp
LCFI105:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$26, %rax
	cmpq	$-2147483648, %rax
	jl	L78
	movl	-4(%rbp), %eax
	cltq
	sarq	$26, %rax
	cmpq	$2147483647, %rax
	jle	L79
L78:
	movl	$3425, %edi
	call	__m3_fault
L79:
	movl	-4(%rbp), %eax
	cltq
	sarq	$26, %rax
	leave
LCFI106:
	ret
LFE26:
.no_dead_strip _div_pow2__div_pow2_i32_27
_div_pow2__div_pow2_i32_27:
LFB27:
	pushq	%rbp
LCFI107:
	movq	%rsp, %rbp
LCFI108:
	subq	$16, %rsp
LCFI109:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$27, %rax
	cmpq	$-2147483648, %rax
	jl	L81
	movl	-4(%rbp), %eax
	cltq
	sarq	$27, %rax
	cmpq	$2147483647, %rax
	jle	L82
L81:
	movl	$3489, %edi
	call	__m3_fault
L82:
	movl	-4(%rbp), %eax
	cltq
	sarq	$27, %rax
	leave
LCFI110:
	ret
LFE27:
.no_dead_strip _div_pow2__div_pow2_i32_28
_div_pow2__div_pow2_i32_28:
LFB28:
	pushq	%rbp
LCFI111:
	movq	%rsp, %rbp
LCFI112:
	subq	$16, %rsp
LCFI113:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$28, %rax
	cmpq	$-2147483648, %rax
	jl	L84
	movl	-4(%rbp), %eax
	cltq
	sarq	$28, %rax
	cmpq	$2147483647, %rax
	jle	L85
L84:
	movl	$3553, %edi
	call	__m3_fault
L85:
	movl	-4(%rbp), %eax
	cltq
	sarq	$28, %rax
	leave
LCFI114:
	ret
LFE28:
.no_dead_strip _div_pow2__div_pow2_i32_29
_div_pow2__div_pow2_i32_29:
LFB29:
	pushq	%rbp
LCFI115:
	movq	%rsp, %rbp
LCFI116:
	subq	$16, %rsp
LCFI117:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$29, %rax
	cmpq	$-2147483648, %rax
	jl	L87
	movl	-4(%rbp), %eax
	cltq
	sarq	$29, %rax
	cmpq	$2147483647, %rax
	jle	L88
L87:
	movl	$3617, %edi
	call	__m3_fault
L88:
	movl	-4(%rbp), %eax
	cltq
	sarq	$29, %rax
	leave
LCFI118:
	ret
LFE29:
.no_dead_strip _div_pow2__div_pow2_i32_30
_div_pow2__div_pow2_i32_30:
LFB30:
	pushq	%rbp
LCFI119:
	movq	%rsp, %rbp
LCFI120:
	subq	$16, %rsp
LCFI121:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$30, %rax
	cmpq	$-2147483648, %rax
	jl	L90
	movl	-4(%rbp), %eax
	cltq
	sarq	$30, %rax
	cmpq	$2147483647, %rax
	jle	L91
L90:
	movl	$3681, %edi
	call	__m3_fault
L91:
	movl	-4(%rbp), %eax
	cltq
	sarq	$30, %rax
	leave
LCFI122:
	ret
LFE30:
.no_dead_strip _div_pow2__div_pow2_i32_31
_div_pow2__div_pow2_i32_31:
LFB31:
	pushq	%rbp
LCFI123:
	movq	%rsp, %rbp
LCFI124:
	subq	$16, %rsp
LCFI125:
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	cltq
	sarq	$31, %rax
	cmpq	$-2147483648, %rax
	jl	L93
	movl	-4(%rbp), %eax
	cltq
	sarq	$31, %rax
	cmpq	$2147483647, %rax
	jle	L94
L93:
	movl	$3745, %edi
	call	__m3_fault
L94:
	movl	-4(%rbp), %eax
	cltq
	sarq	$31, %rax
	leave
LCFI126:
	ret
LFE31:
.no_dead_strip _div_pow2__div_pow2_u32_0
_div_pow2__div_pow2_u32_0:
LFB32:
	pushq	%rbp
LCFI127:
	movq	%rsp, %rbp
LCFI128:
	subq	$16, %rsp
LCFI129:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %edx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L96
	movl	$3809, %edi
	call	__m3_fault
L96:
	movl	-4(%rbp), %eax
	leave
LCFI130:
	ret
LFE32:
.no_dead_strip _div_pow2__div_pow2_u32_1
_div_pow2__div_pow2_u32_1:
LFB33:
	pushq	%rbp
LCFI131:
	movq	%rsp, %rbp
LCFI132:
	subq	$16, %rsp
LCFI133:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	%rax
	testq	%rax, %rax
	js	L98
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	%rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L99
L98:
	movl	$3873, %edi
	call	__m3_fault
L99:
	mov	-4(%rbp), %eax
	sarq	%rax
	leave
LCFI134:
	ret
LFE33:
.no_dead_strip _div_pow2__div_pow2_u32_2
_div_pow2__div_pow2_u32_2:
LFB34:
	pushq	%rbp
LCFI135:
	movq	%rsp, %rbp
LCFI136:
	subq	$16, %rsp
LCFI137:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$2, %rax
	testq	%rax, %rax
	js	L101
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$2, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L102
L101:
	movl	$3937, %edi
	call	__m3_fault
L102:
	mov	-4(%rbp), %eax
	sarq	$2, %rax
	leave
LCFI138:
	ret
LFE34:
.no_dead_strip _div_pow2__div_pow2_u32_3
_div_pow2__div_pow2_u32_3:
LFB35:
	pushq	%rbp
LCFI139:
	movq	%rsp, %rbp
LCFI140:
	subq	$16, %rsp
LCFI141:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$3, %rax
	testq	%rax, %rax
	js	L104
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$3, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L105
L104:
	movl	$4001, %edi
	call	__m3_fault
L105:
	mov	-4(%rbp), %eax
	sarq	$3, %rax
	leave
LCFI142:
	ret
LFE35:
.no_dead_strip _div_pow2__div_pow2_u32_4
_div_pow2__div_pow2_u32_4:
LFB36:
	pushq	%rbp
LCFI143:
	movq	%rsp, %rbp
LCFI144:
	subq	$16, %rsp
LCFI145:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$4, %rax
	testq	%rax, %rax
	js	L107
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$4, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L108
L107:
	movl	$4065, %edi
	call	__m3_fault
L108:
	mov	-4(%rbp), %eax
	sarq	$4, %rax
	leave
LCFI146:
	ret
LFE36:
.no_dead_strip _div_pow2__div_pow2_u32_5
_div_pow2__div_pow2_u32_5:
LFB37:
	pushq	%rbp
LCFI147:
	movq	%rsp, %rbp
LCFI148:
	subq	$16, %rsp
LCFI149:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$5, %rax
	testq	%rax, %rax
	js	L110
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$5, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L111
L110:
	movl	$4129, %edi
	call	__m3_fault
L111:
	mov	-4(%rbp), %eax
	sarq	$5, %rax
	leave
LCFI150:
	ret
LFE37:
.no_dead_strip _div_pow2__div_pow2_u32_6
_div_pow2__div_pow2_u32_6:
LFB38:
	pushq	%rbp
LCFI151:
	movq	%rsp, %rbp
LCFI152:
	subq	$16, %rsp
LCFI153:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$6, %rax
	testq	%rax, %rax
	js	L113
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$6, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L114
L113:
	movl	$4193, %edi
	call	__m3_fault
L114:
	mov	-4(%rbp), %eax
	sarq	$6, %rax
	leave
LCFI154:
	ret
LFE38:
.no_dead_strip _div_pow2__div_pow2_u32_7
_div_pow2__div_pow2_u32_7:
LFB39:
	pushq	%rbp
LCFI155:
	movq	%rsp, %rbp
LCFI156:
	subq	$16, %rsp
LCFI157:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$7, %rax
	testq	%rax, %rax
	js	L116
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$7, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L117
L116:
	movl	$4257, %edi
	call	__m3_fault
L117:
	mov	-4(%rbp), %eax
	sarq	$7, %rax
	leave
LCFI158:
	ret
LFE39:
.no_dead_strip _div_pow2__div_pow2_u32_8
_div_pow2__div_pow2_u32_8:
LFB40:
	pushq	%rbp
LCFI159:
	movq	%rsp, %rbp
LCFI160:
	subq	$16, %rsp
LCFI161:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$8, %rax
	testq	%rax, %rax
	js	L119
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$8, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L120
L119:
	movl	$4321, %edi
	call	__m3_fault
L120:
	mov	-4(%rbp), %eax
	sarq	$8, %rax
	leave
LCFI162:
	ret
LFE40:
.no_dead_strip _div_pow2__div_pow2_u32_9
_div_pow2__div_pow2_u32_9:
LFB41:
	pushq	%rbp
LCFI163:
	movq	%rsp, %rbp
LCFI164:
	subq	$16, %rsp
LCFI165:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$9, %rax
	testq	%rax, %rax
	js	L122
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$9, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L123
L122:
	movl	$4385, %edi
	call	__m3_fault
L123:
	mov	-4(%rbp), %eax
	sarq	$9, %rax
	leave
LCFI166:
	ret
LFE41:
.no_dead_strip _div_pow2__div_pow2_u32_10
_div_pow2__div_pow2_u32_10:
LFB42:
	pushq	%rbp
LCFI167:
	movq	%rsp, %rbp
LCFI168:
	subq	$16, %rsp
LCFI169:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$10, %rax
	testq	%rax, %rax
	js	L125
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$10, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L126
L125:
	movl	$4449, %edi
	call	__m3_fault
L126:
	mov	-4(%rbp), %eax
	sarq	$10, %rax
	leave
LCFI170:
	ret
LFE42:
.no_dead_strip _div_pow2__div_pow2_u32_11
_div_pow2__div_pow2_u32_11:
LFB43:
	pushq	%rbp
LCFI171:
	movq	%rsp, %rbp
LCFI172:
	subq	$16, %rsp
LCFI173:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$11, %rax
	testq	%rax, %rax
	js	L128
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$11, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L129
L128:
	movl	$4513, %edi
	call	__m3_fault
L129:
	mov	-4(%rbp), %eax
	sarq	$11, %rax
	leave
LCFI174:
	ret
LFE43:
.no_dead_strip _div_pow2__div_pow2_u32_12
_div_pow2__div_pow2_u32_12:
LFB44:
	pushq	%rbp
LCFI175:
	movq	%rsp, %rbp
LCFI176:
	subq	$16, %rsp
LCFI177:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$12, %rax
	testq	%rax, %rax
	js	L131
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$12, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L132
L131:
	movl	$4577, %edi
	call	__m3_fault
L132:
	mov	-4(%rbp), %eax
	sarq	$12, %rax
	leave
LCFI178:
	ret
LFE44:
.no_dead_strip _div_pow2__div_pow2_u32_13
_div_pow2__div_pow2_u32_13:
LFB45:
	pushq	%rbp
LCFI179:
	movq	%rsp, %rbp
LCFI180:
	subq	$16, %rsp
LCFI181:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$13, %rax
	testq	%rax, %rax
	js	L134
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$13, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L135
L134:
	movl	$4641, %edi
	call	__m3_fault
L135:
	mov	-4(%rbp), %eax
	sarq	$13, %rax
	leave
LCFI182:
	ret
LFE45:
.no_dead_strip _div_pow2__div_pow2_u32_14
_div_pow2__div_pow2_u32_14:
LFB46:
	pushq	%rbp
LCFI183:
	movq	%rsp, %rbp
LCFI184:
	subq	$16, %rsp
LCFI185:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$14, %rax
	testq	%rax, %rax
	js	L137
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$14, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L138
L137:
	movl	$4705, %edi
	call	__m3_fault
L138:
	mov	-4(%rbp), %eax
	sarq	$14, %rax
	leave
LCFI186:
	ret
LFE46:
.no_dead_strip _div_pow2__div_pow2_u32_15
_div_pow2__div_pow2_u32_15:
LFB47:
	pushq	%rbp
LCFI187:
	movq	%rsp, %rbp
LCFI188:
	subq	$16, %rsp
LCFI189:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$15, %rax
	testq	%rax, %rax
	js	L140
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$15, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L141
L140:
	movl	$4769, %edi
	call	__m3_fault
L141:
	mov	-4(%rbp), %eax
	sarq	$15, %rax
	leave
LCFI190:
	ret
LFE47:
.no_dead_strip _div_pow2__div_pow2_u32_16
_div_pow2__div_pow2_u32_16:
LFB48:
	pushq	%rbp
LCFI191:
	movq	%rsp, %rbp
LCFI192:
	subq	$16, %rsp
LCFI193:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$16, %rax
	testq	%rax, %rax
	js	L143
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$16, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L144
L143:
	movl	$4833, %edi
	call	__m3_fault
L144:
	mov	-4(%rbp), %eax
	sarq	$16, %rax
	leave
LCFI194:
	ret
LFE48:
.no_dead_strip _div_pow2__div_pow2_u32_17
_div_pow2__div_pow2_u32_17:
LFB49:
	pushq	%rbp
LCFI195:
	movq	%rsp, %rbp
LCFI196:
	subq	$16, %rsp
LCFI197:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$17, %rax
	testq	%rax, %rax
	js	L146
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$17, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L147
L146:
	movl	$4897, %edi
	call	__m3_fault
L147:
	mov	-4(%rbp), %eax
	sarq	$17, %rax
	leave
LCFI198:
	ret
LFE49:
.no_dead_strip _div_pow2__div_pow2_u32_18
_div_pow2__div_pow2_u32_18:
LFB50:
	pushq	%rbp
LCFI199:
	movq	%rsp, %rbp
LCFI200:
	subq	$16, %rsp
LCFI201:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$18, %rax
	testq	%rax, %rax
	js	L149
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$18, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L150
L149:
	movl	$4961, %edi
	call	__m3_fault
L150:
	mov	-4(%rbp), %eax
	sarq	$18, %rax
	leave
LCFI202:
	ret
LFE50:
.no_dead_strip _div_pow2__div_pow2_u32_19
_div_pow2__div_pow2_u32_19:
LFB51:
	pushq	%rbp
LCFI203:
	movq	%rsp, %rbp
LCFI204:
	subq	$16, %rsp
LCFI205:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$19, %rax
	testq	%rax, %rax
	js	L152
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$19, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L153
L152:
	movl	$5025, %edi
	call	__m3_fault
L153:
	mov	-4(%rbp), %eax
	sarq	$19, %rax
	leave
LCFI206:
	ret
LFE51:
.no_dead_strip _div_pow2__div_pow2_u32_20
_div_pow2__div_pow2_u32_20:
LFB52:
	pushq	%rbp
LCFI207:
	movq	%rsp, %rbp
LCFI208:
	subq	$16, %rsp
LCFI209:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$20, %rax
	testq	%rax, %rax
	js	L155
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$20, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L156
L155:
	movl	$5089, %edi
	call	__m3_fault
L156:
	mov	-4(%rbp), %eax
	sarq	$20, %rax
	leave
LCFI210:
	ret
LFE52:
.no_dead_strip _div_pow2__div_pow2_u32_21
_div_pow2__div_pow2_u32_21:
LFB53:
	pushq	%rbp
LCFI211:
	movq	%rsp, %rbp
LCFI212:
	subq	$16, %rsp
LCFI213:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$21, %rax
	testq	%rax, %rax
	js	L158
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$21, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L159
L158:
	movl	$5153, %edi
	call	__m3_fault
L159:
	mov	-4(%rbp), %eax
	sarq	$21, %rax
	leave
LCFI214:
	ret
LFE53:
.no_dead_strip _div_pow2__div_pow2_u32_22
_div_pow2__div_pow2_u32_22:
LFB54:
	pushq	%rbp
LCFI215:
	movq	%rsp, %rbp
LCFI216:
	subq	$16, %rsp
LCFI217:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$22, %rax
	testq	%rax, %rax
	js	L161
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$22, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L162
L161:
	movl	$5217, %edi
	call	__m3_fault
L162:
	mov	-4(%rbp), %eax
	sarq	$22, %rax
	leave
LCFI218:
	ret
LFE54:
.no_dead_strip _div_pow2__div_pow2_u32_23
_div_pow2__div_pow2_u32_23:
LFB55:
	pushq	%rbp
LCFI219:
	movq	%rsp, %rbp
LCFI220:
	subq	$16, %rsp
LCFI221:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$23, %rax
	testq	%rax, %rax
	js	L164
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$23, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L165
L164:
	movl	$5281, %edi
	call	__m3_fault
L165:
	mov	-4(%rbp), %eax
	sarq	$23, %rax
	leave
LCFI222:
	ret
LFE55:
.no_dead_strip _div_pow2__div_pow2_u32_24
_div_pow2__div_pow2_u32_24:
LFB56:
	pushq	%rbp
LCFI223:
	movq	%rsp, %rbp
LCFI224:
	subq	$16, %rsp
LCFI225:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$24, %rax
	testq	%rax, %rax
	js	L167
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$24, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L168
L167:
	movl	$5345, %edi
	call	__m3_fault
L168:
	mov	-4(%rbp), %eax
	sarq	$24, %rax
	leave
LCFI226:
	ret
LFE56:
.no_dead_strip _div_pow2__div_pow2_u32_25
_div_pow2__div_pow2_u32_25:
LFB57:
	pushq	%rbp
LCFI227:
	movq	%rsp, %rbp
LCFI228:
	subq	$16, %rsp
LCFI229:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$25, %rax
	testq	%rax, %rax
	js	L170
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$25, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L171
L170:
	movl	$5409, %edi
	call	__m3_fault
L171:
	mov	-4(%rbp), %eax
	sarq	$25, %rax
	leave
LCFI230:
	ret
LFE57:
.no_dead_strip _div_pow2__div_pow2_u32_26
_div_pow2__div_pow2_u32_26:
LFB58:
	pushq	%rbp
LCFI231:
	movq	%rsp, %rbp
LCFI232:
	subq	$16, %rsp
LCFI233:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$26, %rax
	testq	%rax, %rax
	js	L173
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$26, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L174
L173:
	movl	$5473, %edi
	call	__m3_fault
L174:
	mov	-4(%rbp), %eax
	sarq	$26, %rax
	leave
LCFI234:
	ret
LFE58:
.no_dead_strip _div_pow2__div_pow2_u32_27
_div_pow2__div_pow2_u32_27:
LFB59:
	pushq	%rbp
LCFI235:
	movq	%rsp, %rbp
LCFI236:
	subq	$16, %rsp
LCFI237:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$27, %rax
	testq	%rax, %rax
	js	L176
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$27, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L177
L176:
	movl	$5537, %edi
	call	__m3_fault
L177:
	mov	-4(%rbp), %eax
	sarq	$27, %rax
	leave
LCFI238:
	ret
LFE59:
.no_dead_strip _div_pow2__div_pow2_u32_28
_div_pow2__div_pow2_u32_28:
LFB60:
	pushq	%rbp
LCFI239:
	movq	%rsp, %rbp
LCFI240:
	subq	$16, %rsp
LCFI241:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$28, %rax
	testq	%rax, %rax
	js	L179
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$28, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L180
L179:
	movl	$5601, %edi
	call	__m3_fault
L180:
	mov	-4(%rbp), %eax
	sarq	$28, %rax
	leave
LCFI242:
	ret
LFE60:
.no_dead_strip _div_pow2__div_pow2_u32_29
_div_pow2__div_pow2_u32_29:
LFB61:
	pushq	%rbp
LCFI243:
	movq	%rsp, %rbp
LCFI244:
	subq	$16, %rsp
LCFI245:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$29, %rax
	testq	%rax, %rax
	js	L182
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$29, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L183
L182:
	movl	$5665, %edi
	call	__m3_fault
L183:
	mov	-4(%rbp), %eax
	sarq	$29, %rax
	leave
LCFI246:
	ret
LFE61:
.no_dead_strip _div_pow2__div_pow2_u32_30
_div_pow2__div_pow2_u32_30:
LFB62:
	pushq	%rbp
LCFI247:
	movq	%rsp, %rbp
LCFI248:
	subq	$16, %rsp
LCFI249:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$30, %rax
	testq	%rax, %rax
	js	L185
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$30, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L186
L185:
	movl	$5729, %edi
	call	__m3_fault
L186:
	mov	-4(%rbp), %eax
	sarq	$30, %rax
	leave
LCFI250:
	ret
LFE62:
.no_dead_strip _div_pow2__div_pow2_u32_31
_div_pow2__div_pow2_u32_31:
LFB63:
	pushq	%rbp
LCFI251:
	movq	%rsp, %rbp
LCFI252:
	subq	$16, %rsp
LCFI253:
	movl	%edi, -4(%rbp)
	mov	-4(%rbp), %eax
	sarq	$31, %rax
	testq	%rax, %rax
	js	L188
	mov	-4(%rbp), %eax
	movq	%rax, %rdx
	sarq	$31, %rdx
	movl	$4294967295, %eax
	cmpq	%rax, %rdx
	jle	L189
L188:
	movl	$5793, %edi
	call	__m3_fault
L189:
	mov	-4(%rbp), %eax
	sarq	$31, %rax
	leave
LCFI254:
	ret
LFE63:
.no_dead_strip _div_pow2__div_pow2_i64_0
_div_pow2__div_pow2_i64_0:
LFB64:
	pushq	%rbp
LCFI255:
	movq	%rsp, %rbp
LCFI256:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI257:
	ret
LFE64:
.no_dead_strip _div_pow2__div_pow2_i64_1
_div_pow2__div_pow2_i64_1:
LFB65:
	pushq	%rbp
LCFI258:
	movq	%rsp, %rbp
LCFI259:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI260:
	ret
LFE65:
.no_dead_strip _div_pow2__div_pow2_i64_2
_div_pow2__div_pow2_i64_2:
LFB66:
	pushq	%rbp
LCFI261:
	movq	%rsp, %rbp
LCFI262:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI263:
	ret
LFE66:
.no_dead_strip _div_pow2__div_pow2_i64_3
_div_pow2__div_pow2_i64_3:
LFB67:
	pushq	%rbp
LCFI264:
	movq	%rsp, %rbp
LCFI265:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI266:
	ret
LFE67:
.no_dead_strip _div_pow2__div_pow2_i64_4
_div_pow2__div_pow2_i64_4:
LFB68:
	pushq	%rbp
LCFI267:
	movq	%rsp, %rbp
LCFI268:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI269:
	ret
LFE68:
.no_dead_strip _div_pow2__div_pow2_i64_5
_div_pow2__div_pow2_i64_5:
LFB69:
	pushq	%rbp
LCFI270:
	movq	%rsp, %rbp
LCFI271:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI272:
	ret
LFE69:
.no_dead_strip _div_pow2__div_pow2_i64_6
_div_pow2__div_pow2_i64_6:
LFB70:
	pushq	%rbp
LCFI273:
	movq	%rsp, %rbp
LCFI274:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI275:
	ret
LFE70:
.no_dead_strip _div_pow2__div_pow2_i64_7
_div_pow2__div_pow2_i64_7:
LFB71:
	pushq	%rbp
LCFI276:
	movq	%rsp, %rbp
LCFI277:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI278:
	ret
LFE71:
.no_dead_strip _div_pow2__div_pow2_i64_8
_div_pow2__div_pow2_i64_8:
LFB72:
	pushq	%rbp
LCFI279:
	movq	%rsp, %rbp
LCFI280:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI281:
	ret
LFE72:
.no_dead_strip _div_pow2__div_pow2_i64_9
_div_pow2__div_pow2_i64_9:
LFB73:
	pushq	%rbp
LCFI282:
	movq	%rsp, %rbp
LCFI283:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI284:
	ret
LFE73:
.no_dead_strip _div_pow2__div_pow2_i64_10
_div_pow2__div_pow2_i64_10:
LFB74:
	pushq	%rbp
LCFI285:
	movq	%rsp, %rbp
LCFI286:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI287:
	ret
LFE74:
.no_dead_strip _div_pow2__div_pow2_i64_11
_div_pow2__div_pow2_i64_11:
LFB75:
	pushq	%rbp
LCFI288:
	movq	%rsp, %rbp
LCFI289:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI290:
	ret
LFE75:
.no_dead_strip _div_pow2__div_pow2_i64_12
_div_pow2__div_pow2_i64_12:
LFB76:
	pushq	%rbp
LCFI291:
	movq	%rsp, %rbp
LCFI292:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI293:
	ret
LFE76:
.no_dead_strip _div_pow2__div_pow2_i64_13
_div_pow2__div_pow2_i64_13:
LFB77:
	pushq	%rbp
LCFI294:
	movq	%rsp, %rbp
LCFI295:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI296:
	ret
LFE77:
.no_dead_strip _div_pow2__div_pow2_i64_14
_div_pow2__div_pow2_i64_14:
LFB78:
	pushq	%rbp
LCFI297:
	movq	%rsp, %rbp
LCFI298:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI299:
	ret
LFE78:
.no_dead_strip _div_pow2__div_pow2_i64_15
_div_pow2__div_pow2_i64_15:
LFB79:
	pushq	%rbp
LCFI300:
	movq	%rsp, %rbp
LCFI301:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI302:
	ret
LFE79:
.no_dead_strip _div_pow2__div_pow2_i64_16
_div_pow2__div_pow2_i64_16:
LFB80:
	pushq	%rbp
LCFI303:
	movq	%rsp, %rbp
LCFI304:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI305:
	ret
LFE80:
.no_dead_strip _div_pow2__div_pow2_i64_17
_div_pow2__div_pow2_i64_17:
LFB81:
	pushq	%rbp
LCFI306:
	movq	%rsp, %rbp
LCFI307:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI308:
	ret
LFE81:
.no_dead_strip _div_pow2__div_pow2_i64_18
_div_pow2__div_pow2_i64_18:
LFB82:
	pushq	%rbp
LCFI309:
	movq	%rsp, %rbp
LCFI310:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI311:
	ret
LFE82:
.no_dead_strip _div_pow2__div_pow2_i64_19
_div_pow2__div_pow2_i64_19:
LFB83:
	pushq	%rbp
LCFI312:
	movq	%rsp, %rbp
LCFI313:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI314:
	ret
LFE83:
.no_dead_strip _div_pow2__div_pow2_i64_20
_div_pow2__div_pow2_i64_20:
LFB84:
	pushq	%rbp
LCFI315:
	movq	%rsp, %rbp
LCFI316:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI317:
	ret
LFE84:
.no_dead_strip _div_pow2__div_pow2_i64_21
_div_pow2__div_pow2_i64_21:
LFB85:
	pushq	%rbp
LCFI318:
	movq	%rsp, %rbp
LCFI319:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI320:
	ret
LFE85:
.no_dead_strip _div_pow2__div_pow2_i64_22
_div_pow2__div_pow2_i64_22:
LFB86:
	pushq	%rbp
LCFI321:
	movq	%rsp, %rbp
LCFI322:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI323:
	ret
LFE86:
.no_dead_strip _div_pow2__div_pow2_i64_23
_div_pow2__div_pow2_i64_23:
LFB87:
	pushq	%rbp
LCFI324:
	movq	%rsp, %rbp
LCFI325:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI326:
	ret
LFE87:
.no_dead_strip _div_pow2__div_pow2_i64_24
_div_pow2__div_pow2_i64_24:
LFB88:
	pushq	%rbp
LCFI327:
	movq	%rsp, %rbp
LCFI328:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI329:
	ret
LFE88:
.no_dead_strip _div_pow2__div_pow2_i64_25
_div_pow2__div_pow2_i64_25:
LFB89:
	pushq	%rbp
LCFI330:
	movq	%rsp, %rbp
LCFI331:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI332:
	ret
LFE89:
.no_dead_strip _div_pow2__div_pow2_i64_26
_div_pow2__div_pow2_i64_26:
LFB90:
	pushq	%rbp
LCFI333:
	movq	%rsp, %rbp
LCFI334:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI335:
	ret
LFE90:
.no_dead_strip _div_pow2__div_pow2_i64_27
_div_pow2__div_pow2_i64_27:
LFB91:
	pushq	%rbp
LCFI336:
	movq	%rsp, %rbp
LCFI337:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI338:
	ret
LFE91:
.no_dead_strip _div_pow2__div_pow2_i64_28
_div_pow2__div_pow2_i64_28:
LFB92:
	pushq	%rbp
LCFI339:
	movq	%rsp, %rbp
LCFI340:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI341:
	ret
LFE92:
.no_dead_strip _div_pow2__div_pow2_i64_29
_div_pow2__div_pow2_i64_29:
LFB93:
	pushq	%rbp
LCFI342:
	movq	%rsp, %rbp
LCFI343:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI344:
	ret
LFE93:
.no_dead_strip _div_pow2__div_pow2_i64_30
_div_pow2__div_pow2_i64_30:
LFB94:
	pushq	%rbp
LCFI345:
	movq	%rsp, %rbp
LCFI346:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI347:
	ret
LFE94:
.no_dead_strip _div_pow2__div_pow2_i64_31
_div_pow2__div_pow2_i64_31:
LFB95:
	pushq	%rbp
LCFI348:
	movq	%rsp, %rbp
LCFI349:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI350:
	ret
LFE95:
.no_dead_strip _div_pow2__div_pow2_i64_32
_div_pow2__div_pow2_i64_32:
LFB96:
	pushq	%rbp
LCFI351:
	movq	%rsp, %rbp
LCFI352:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI353:
	ret
LFE96:
.no_dead_strip _div_pow2__div_pow2_i64_33
_div_pow2__div_pow2_i64_33:
LFB97:
	pushq	%rbp
LCFI354:
	movq	%rsp, %rbp
LCFI355:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI356:
	ret
LFE97:
.no_dead_strip _div_pow2__div_pow2_i64_34
_div_pow2__div_pow2_i64_34:
LFB98:
	pushq	%rbp
LCFI357:
	movq	%rsp, %rbp
LCFI358:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI359:
	ret
LFE98:
.no_dead_strip _div_pow2__div_pow2_i64_35
_div_pow2__div_pow2_i64_35:
LFB99:
	pushq	%rbp
LCFI360:
	movq	%rsp, %rbp
LCFI361:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI362:
	ret
LFE99:
.no_dead_strip _div_pow2__div_pow2_i64_36
_div_pow2__div_pow2_i64_36:
LFB100:
	pushq	%rbp
LCFI363:
	movq	%rsp, %rbp
LCFI364:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI365:
	ret
LFE100:
.no_dead_strip _div_pow2__div_pow2_i64_37
_div_pow2__div_pow2_i64_37:
LFB101:
	pushq	%rbp
LCFI366:
	movq	%rsp, %rbp
LCFI367:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI368:
	ret
LFE101:
.no_dead_strip _div_pow2__div_pow2_i64_38
_div_pow2__div_pow2_i64_38:
LFB102:
	pushq	%rbp
LCFI369:
	movq	%rsp, %rbp
LCFI370:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI371:
	ret
LFE102:
.no_dead_strip _div_pow2__div_pow2_i64_39
_div_pow2__div_pow2_i64_39:
LFB103:
	pushq	%rbp
LCFI372:
	movq	%rsp, %rbp
LCFI373:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI374:
	ret
LFE103:
.no_dead_strip _div_pow2__div_pow2_i64_40
_div_pow2__div_pow2_i64_40:
LFB104:
	pushq	%rbp
LCFI375:
	movq	%rsp, %rbp
LCFI376:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI377:
	ret
LFE104:
.no_dead_strip _div_pow2__div_pow2_i64_41
_div_pow2__div_pow2_i64_41:
LFB105:
	pushq	%rbp
LCFI378:
	movq	%rsp, %rbp
LCFI379:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI380:
	ret
LFE105:
.no_dead_strip _div_pow2__div_pow2_i64_42
_div_pow2__div_pow2_i64_42:
LFB106:
	pushq	%rbp
LCFI381:
	movq	%rsp, %rbp
LCFI382:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI383:
	ret
LFE106:
.no_dead_strip _div_pow2__div_pow2_i64_43
_div_pow2__div_pow2_i64_43:
LFB107:
	pushq	%rbp
LCFI384:
	movq	%rsp, %rbp
LCFI385:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI386:
	ret
LFE107:
.no_dead_strip _div_pow2__div_pow2_i64_44
_div_pow2__div_pow2_i64_44:
LFB108:
	pushq	%rbp
LCFI387:
	movq	%rsp, %rbp
LCFI388:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI389:
	ret
LFE108:
.no_dead_strip _div_pow2__div_pow2_i64_45
_div_pow2__div_pow2_i64_45:
LFB109:
	pushq	%rbp
LCFI390:
	movq	%rsp, %rbp
LCFI391:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI392:
	ret
LFE109:
.no_dead_strip _div_pow2__div_pow2_i64_46
_div_pow2__div_pow2_i64_46:
LFB110:
	pushq	%rbp
LCFI393:
	movq	%rsp, %rbp
LCFI394:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI395:
	ret
LFE110:
.no_dead_strip _div_pow2__div_pow2_i64_47
_div_pow2__div_pow2_i64_47:
LFB111:
	pushq	%rbp
LCFI396:
	movq	%rsp, %rbp
LCFI397:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI398:
	ret
LFE111:
.no_dead_strip _div_pow2__div_pow2_i64_48
_div_pow2__div_pow2_i64_48:
LFB112:
	pushq	%rbp
LCFI399:
	movq	%rsp, %rbp
LCFI400:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI401:
	ret
LFE112:
.no_dead_strip _div_pow2__div_pow2_i64_49
_div_pow2__div_pow2_i64_49:
LFB113:
	pushq	%rbp
LCFI402:
	movq	%rsp, %rbp
LCFI403:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI404:
	ret
LFE113:
.no_dead_strip _div_pow2__div_pow2_i64_50
_div_pow2__div_pow2_i64_50:
LFB114:
	pushq	%rbp
LCFI405:
	movq	%rsp, %rbp
LCFI406:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI407:
	ret
LFE114:
.no_dead_strip _div_pow2__div_pow2_i64_51
_div_pow2__div_pow2_i64_51:
LFB115:
	pushq	%rbp
LCFI408:
	movq	%rsp, %rbp
LCFI409:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI410:
	ret
LFE115:
.no_dead_strip _div_pow2__div_pow2_i64_52
_div_pow2__div_pow2_i64_52:
LFB116:
	pushq	%rbp
LCFI411:
	movq	%rsp, %rbp
LCFI412:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI413:
	ret
LFE116:
.no_dead_strip _div_pow2__div_pow2_i64_53
_div_pow2__div_pow2_i64_53:
LFB117:
	pushq	%rbp
LCFI414:
	movq	%rsp, %rbp
LCFI415:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI416:
	ret
LFE117:
.no_dead_strip _div_pow2__div_pow2_i64_54
_div_pow2__div_pow2_i64_54:
LFB118:
	pushq	%rbp
LCFI417:
	movq	%rsp, %rbp
LCFI418:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI419:
	ret
LFE118:
.no_dead_strip _div_pow2__div_pow2_i64_55
_div_pow2__div_pow2_i64_55:
LFB119:
	pushq	%rbp
LCFI420:
	movq	%rsp, %rbp
LCFI421:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI422:
	ret
LFE119:
.no_dead_strip _div_pow2__div_pow2_i64_56
_div_pow2__div_pow2_i64_56:
LFB120:
	pushq	%rbp
LCFI423:
	movq	%rsp, %rbp
LCFI424:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI425:
	ret
LFE120:
.no_dead_strip _div_pow2__div_pow2_i64_57
_div_pow2__div_pow2_i64_57:
LFB121:
	pushq	%rbp
LCFI426:
	movq	%rsp, %rbp
LCFI427:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI428:
	ret
LFE121:
.no_dead_strip _div_pow2__div_pow2_i64_58
_div_pow2__div_pow2_i64_58:
LFB122:
	pushq	%rbp
LCFI429:
	movq	%rsp, %rbp
LCFI430:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI431:
	ret
LFE122:
.no_dead_strip _div_pow2__div_pow2_i64_59
_div_pow2__div_pow2_i64_59:
LFB123:
	pushq	%rbp
LCFI432:
	movq	%rsp, %rbp
LCFI433:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI434:
	ret
LFE123:
.no_dead_strip _div_pow2__div_pow2_i64_60
_div_pow2__div_pow2_i64_60:
LFB124:
	pushq	%rbp
LCFI435:
	movq	%rsp, %rbp
LCFI436:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI437:
	ret
LFE124:
.no_dead_strip _div_pow2__div_pow2_i64_61
_div_pow2__div_pow2_i64_61:
LFB125:
	pushq	%rbp
LCFI438:
	movq	%rsp, %rbp
LCFI439:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI440:
	ret
LFE125:
.no_dead_strip _div_pow2__div_pow2_i64_62
_div_pow2__div_pow2_i64_62:
LFB126:
	pushq	%rbp
LCFI441:
	movq	%rsp, %rbp
LCFI442:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI443:
	ret
LFE126:
.no_dead_strip _div_pow2__div_pow2_i64_63
_div_pow2__div_pow2_i64_63:
LFB127:
	pushq	%rbp
LCFI444:
	movq	%rsp, %rbp
LCFI445:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI446:
	ret
LFE127:
.no_dead_strip _div_pow2__div_pow2_u64_0
_div_pow2__div_pow2_u64_0:
LFB128:
	pushq	%rbp
LCFI447:
	movq	%rsp, %rbp
LCFI448:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI449:
	ret
LFE128:
.no_dead_strip _div_pow2__div_pow2_u64_1
_div_pow2__div_pow2_u64_1:
LFB129:
	pushq	%rbp
LCFI450:
	movq	%rsp, %rbp
LCFI451:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI452:
	ret
LFE129:
.no_dead_strip _div_pow2__div_pow2_u64_2
_div_pow2__div_pow2_u64_2:
LFB130:
	pushq	%rbp
LCFI453:
	movq	%rsp, %rbp
LCFI454:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI455:
	ret
LFE130:
.no_dead_strip _div_pow2__div_pow2_u64_3
_div_pow2__div_pow2_u64_3:
LFB131:
	pushq	%rbp
LCFI456:
	movq	%rsp, %rbp
LCFI457:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI458:
	ret
LFE131:
.no_dead_strip _div_pow2__div_pow2_u64_4
_div_pow2__div_pow2_u64_4:
LFB132:
	pushq	%rbp
LCFI459:
	movq	%rsp, %rbp
LCFI460:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI461:
	ret
LFE132:
.no_dead_strip _div_pow2__div_pow2_u64_5
_div_pow2__div_pow2_u64_5:
LFB133:
	pushq	%rbp
LCFI462:
	movq	%rsp, %rbp
LCFI463:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI464:
	ret
LFE133:
.no_dead_strip _div_pow2__div_pow2_u64_6
_div_pow2__div_pow2_u64_6:
LFB134:
	pushq	%rbp
LCFI465:
	movq	%rsp, %rbp
LCFI466:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI467:
	ret
LFE134:
.no_dead_strip _div_pow2__div_pow2_u64_7
_div_pow2__div_pow2_u64_7:
LFB135:
	pushq	%rbp
LCFI468:
	movq	%rsp, %rbp
LCFI469:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI470:
	ret
LFE135:
.no_dead_strip _div_pow2__div_pow2_u64_8
_div_pow2__div_pow2_u64_8:
LFB136:
	pushq	%rbp
LCFI471:
	movq	%rsp, %rbp
LCFI472:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI473:
	ret
LFE136:
.no_dead_strip _div_pow2__div_pow2_u64_9
_div_pow2__div_pow2_u64_9:
LFB137:
	pushq	%rbp
LCFI474:
	movq	%rsp, %rbp
LCFI475:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI476:
	ret
LFE137:
.no_dead_strip _div_pow2__div_pow2_u64_10
_div_pow2__div_pow2_u64_10:
LFB138:
	pushq	%rbp
LCFI477:
	movq	%rsp, %rbp
LCFI478:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI479:
	ret
LFE138:
.no_dead_strip _div_pow2__div_pow2_u64_11
_div_pow2__div_pow2_u64_11:
LFB139:
	pushq	%rbp
LCFI480:
	movq	%rsp, %rbp
LCFI481:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI482:
	ret
LFE139:
.no_dead_strip _div_pow2__div_pow2_u64_12
_div_pow2__div_pow2_u64_12:
LFB140:
	pushq	%rbp
LCFI483:
	movq	%rsp, %rbp
LCFI484:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI485:
	ret
LFE140:
.no_dead_strip _div_pow2__div_pow2_u64_13
_div_pow2__div_pow2_u64_13:
LFB141:
	pushq	%rbp
LCFI486:
	movq	%rsp, %rbp
LCFI487:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI488:
	ret
LFE141:
.no_dead_strip _div_pow2__div_pow2_u64_14
_div_pow2__div_pow2_u64_14:
LFB142:
	pushq	%rbp
LCFI489:
	movq	%rsp, %rbp
LCFI490:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI491:
	ret
LFE142:
.no_dead_strip _div_pow2__div_pow2_u64_15
_div_pow2__div_pow2_u64_15:
LFB143:
	pushq	%rbp
LCFI492:
	movq	%rsp, %rbp
LCFI493:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI494:
	ret
LFE143:
.no_dead_strip _div_pow2__div_pow2_u64_16
_div_pow2__div_pow2_u64_16:
LFB144:
	pushq	%rbp
LCFI495:
	movq	%rsp, %rbp
LCFI496:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI497:
	ret
LFE144:
.no_dead_strip _div_pow2__div_pow2_u64_17
_div_pow2__div_pow2_u64_17:
LFB145:
	pushq	%rbp
LCFI498:
	movq	%rsp, %rbp
LCFI499:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI500:
	ret
LFE145:
.no_dead_strip _div_pow2__div_pow2_u64_18
_div_pow2__div_pow2_u64_18:
LFB146:
	pushq	%rbp
LCFI501:
	movq	%rsp, %rbp
LCFI502:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI503:
	ret
LFE146:
.no_dead_strip _div_pow2__div_pow2_u64_19
_div_pow2__div_pow2_u64_19:
LFB147:
	pushq	%rbp
LCFI504:
	movq	%rsp, %rbp
LCFI505:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI506:
	ret
LFE147:
.no_dead_strip _div_pow2__div_pow2_u64_20
_div_pow2__div_pow2_u64_20:
LFB148:
	pushq	%rbp
LCFI507:
	movq	%rsp, %rbp
LCFI508:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI509:
	ret
LFE148:
.no_dead_strip _div_pow2__div_pow2_u64_21
_div_pow2__div_pow2_u64_21:
LFB149:
	pushq	%rbp
LCFI510:
	movq	%rsp, %rbp
LCFI511:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI512:
	ret
LFE149:
.no_dead_strip _div_pow2__div_pow2_u64_22
_div_pow2__div_pow2_u64_22:
LFB150:
	pushq	%rbp
LCFI513:
	movq	%rsp, %rbp
LCFI514:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI515:
	ret
LFE150:
.no_dead_strip _div_pow2__div_pow2_u64_23
_div_pow2__div_pow2_u64_23:
LFB151:
	pushq	%rbp
LCFI516:
	movq	%rsp, %rbp
LCFI517:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI518:
	ret
LFE151:
.no_dead_strip _div_pow2__div_pow2_u64_24
_div_pow2__div_pow2_u64_24:
LFB152:
	pushq	%rbp
LCFI519:
	movq	%rsp, %rbp
LCFI520:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI521:
	ret
LFE152:
.no_dead_strip _div_pow2__div_pow2_u64_25
_div_pow2__div_pow2_u64_25:
LFB153:
	pushq	%rbp
LCFI522:
	movq	%rsp, %rbp
LCFI523:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI524:
	ret
LFE153:
.no_dead_strip _div_pow2__div_pow2_u64_26
_div_pow2__div_pow2_u64_26:
LFB154:
	pushq	%rbp
LCFI525:
	movq	%rsp, %rbp
LCFI526:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI527:
	ret
LFE154:
.no_dead_strip _div_pow2__div_pow2_u64_27
_div_pow2__div_pow2_u64_27:
LFB155:
	pushq	%rbp
LCFI528:
	movq	%rsp, %rbp
LCFI529:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI530:
	ret
LFE155:
.no_dead_strip _div_pow2__div_pow2_u64_28
_div_pow2__div_pow2_u64_28:
LFB156:
	pushq	%rbp
LCFI531:
	movq	%rsp, %rbp
LCFI532:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI533:
	ret
LFE156:
.no_dead_strip _div_pow2__div_pow2_u64_29
_div_pow2__div_pow2_u64_29:
LFB157:
	pushq	%rbp
LCFI534:
	movq	%rsp, %rbp
LCFI535:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI536:
	ret
LFE157:
.no_dead_strip _div_pow2__div_pow2_u64_30
_div_pow2__div_pow2_u64_30:
LFB158:
	pushq	%rbp
LCFI537:
	movq	%rsp, %rbp
LCFI538:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI539:
	ret
LFE158:
.no_dead_strip _div_pow2__div_pow2_u64_31
_div_pow2__div_pow2_u64_31:
LFB159:
	pushq	%rbp
LCFI540:
	movq	%rsp, %rbp
LCFI541:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI542:
	ret
LFE159:
.no_dead_strip _div_pow2__div_pow2_u64_32
_div_pow2__div_pow2_u64_32:
LFB160:
	pushq	%rbp
LCFI543:
	movq	%rsp, %rbp
LCFI544:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI545:
	ret
LFE160:
.no_dead_strip _div_pow2__div_pow2_u64_33
_div_pow2__div_pow2_u64_33:
LFB161:
	pushq	%rbp
LCFI546:
	movq	%rsp, %rbp
LCFI547:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI548:
	ret
LFE161:
.no_dead_strip _div_pow2__div_pow2_u64_34
_div_pow2__div_pow2_u64_34:
LFB162:
	pushq	%rbp
LCFI549:
	movq	%rsp, %rbp
LCFI550:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI551:
	ret
LFE162:
.no_dead_strip _div_pow2__div_pow2_u64_35
_div_pow2__div_pow2_u64_35:
LFB163:
	pushq	%rbp
LCFI552:
	movq	%rsp, %rbp
LCFI553:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI554:
	ret
LFE163:
.no_dead_strip _div_pow2__div_pow2_u64_36
_div_pow2__div_pow2_u64_36:
LFB164:
	pushq	%rbp
LCFI555:
	movq	%rsp, %rbp
LCFI556:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI557:
	ret
LFE164:
.no_dead_strip _div_pow2__div_pow2_u64_37
_div_pow2__div_pow2_u64_37:
LFB165:
	pushq	%rbp
LCFI558:
	movq	%rsp, %rbp
LCFI559:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI560:
	ret
LFE165:
.no_dead_strip _div_pow2__div_pow2_u64_38
_div_pow2__div_pow2_u64_38:
LFB166:
	pushq	%rbp
LCFI561:
	movq	%rsp, %rbp
LCFI562:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI563:
	ret
LFE166:
.no_dead_strip _div_pow2__div_pow2_u64_39
_div_pow2__div_pow2_u64_39:
LFB167:
	pushq	%rbp
LCFI564:
	movq	%rsp, %rbp
LCFI565:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI566:
	ret
LFE167:
.no_dead_strip _div_pow2__div_pow2_u64_40
_div_pow2__div_pow2_u64_40:
LFB168:
	pushq	%rbp
LCFI567:
	movq	%rsp, %rbp
LCFI568:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI569:
	ret
LFE168:
.no_dead_strip _div_pow2__div_pow2_u64_41
_div_pow2__div_pow2_u64_41:
LFB169:
	pushq	%rbp
LCFI570:
	movq	%rsp, %rbp
LCFI571:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI572:
	ret
LFE169:
.no_dead_strip _div_pow2__div_pow2_u64_42
_div_pow2__div_pow2_u64_42:
LFB170:
	pushq	%rbp
LCFI573:
	movq	%rsp, %rbp
LCFI574:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI575:
	ret
LFE170:
.no_dead_strip _div_pow2__div_pow2_u64_43
_div_pow2__div_pow2_u64_43:
LFB171:
	pushq	%rbp
LCFI576:
	movq	%rsp, %rbp
LCFI577:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI578:
	ret
LFE171:
.no_dead_strip _div_pow2__div_pow2_u64_44
_div_pow2__div_pow2_u64_44:
LFB172:
	pushq	%rbp
LCFI579:
	movq	%rsp, %rbp
LCFI580:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI581:
	ret
LFE172:
.no_dead_strip _div_pow2__div_pow2_u64_45
_div_pow2__div_pow2_u64_45:
LFB173:
	pushq	%rbp
LCFI582:
	movq	%rsp, %rbp
LCFI583:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI584:
	ret
LFE173:
.no_dead_strip _div_pow2__div_pow2_u64_46
_div_pow2__div_pow2_u64_46:
LFB174:
	pushq	%rbp
LCFI585:
	movq	%rsp, %rbp
LCFI586:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI587:
	ret
LFE174:
.no_dead_strip _div_pow2__div_pow2_u64_47
_div_pow2__div_pow2_u64_47:
LFB175:
	pushq	%rbp
LCFI588:
	movq	%rsp, %rbp
LCFI589:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI590:
	ret
LFE175:
.no_dead_strip _div_pow2__div_pow2_u64_48
_div_pow2__div_pow2_u64_48:
LFB176:
	pushq	%rbp
LCFI591:
	movq	%rsp, %rbp
LCFI592:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI593:
	ret
LFE176:
.no_dead_strip _div_pow2__div_pow2_u64_49
_div_pow2__div_pow2_u64_49:
LFB177:
	pushq	%rbp
LCFI594:
	movq	%rsp, %rbp
LCFI595:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI596:
	ret
LFE177:
.no_dead_strip _div_pow2__div_pow2_u64_50
_div_pow2__div_pow2_u64_50:
LFB178:
	pushq	%rbp
LCFI597:
	movq	%rsp, %rbp
LCFI598:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI599:
	ret
LFE178:
.no_dead_strip _div_pow2__div_pow2_u64_51
_div_pow2__div_pow2_u64_51:
LFB179:
	pushq	%rbp
LCFI600:
	movq	%rsp, %rbp
LCFI601:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI602:
	ret
LFE179:
.no_dead_strip _div_pow2__div_pow2_u64_52
_div_pow2__div_pow2_u64_52:
LFB180:
	pushq	%rbp
LCFI603:
	movq	%rsp, %rbp
LCFI604:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI605:
	ret
LFE180:
.no_dead_strip _div_pow2__div_pow2_u64_53
_div_pow2__div_pow2_u64_53:
LFB181:
	pushq	%rbp
LCFI606:
	movq	%rsp, %rbp
LCFI607:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI608:
	ret
LFE181:
.no_dead_strip _div_pow2__div_pow2_u64_54
_div_pow2__div_pow2_u64_54:
LFB182:
	pushq	%rbp
LCFI609:
	movq	%rsp, %rbp
LCFI610:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI611:
	ret
LFE182:
.no_dead_strip _div_pow2__div_pow2_u64_55
_div_pow2__div_pow2_u64_55:
LFB183:
	pushq	%rbp
LCFI612:
	movq	%rsp, %rbp
LCFI613:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI614:
	ret
LFE183:
.no_dead_strip _div_pow2__div_pow2_u64_56
_div_pow2__div_pow2_u64_56:
LFB184:
	pushq	%rbp
LCFI615:
	movq	%rsp, %rbp
LCFI616:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI617:
	ret
LFE184:
.no_dead_strip _div_pow2__div_pow2_u64_57
_div_pow2__div_pow2_u64_57:
LFB185:
	pushq	%rbp
LCFI618:
	movq	%rsp, %rbp
LCFI619:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI620:
	ret
LFE185:
.no_dead_strip _div_pow2__div_pow2_u64_58
_div_pow2__div_pow2_u64_58:
LFB186:
	pushq	%rbp
LCFI621:
	movq	%rsp, %rbp
LCFI622:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI623:
	ret
LFE186:
.no_dead_strip _div_pow2__div_pow2_u64_59
_div_pow2__div_pow2_u64_59:
LFB187:
	pushq	%rbp
LCFI624:
	movq	%rsp, %rbp
LCFI625:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI626:
	ret
LFE187:
.no_dead_strip _div_pow2__div_pow2_u64_60
_div_pow2__div_pow2_u64_60:
LFB188:
	pushq	%rbp
LCFI627:
	movq	%rsp, %rbp
LCFI628:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI629:
	ret
LFE188:
.no_dead_strip _div_pow2__div_pow2_u64_61
_div_pow2__div_pow2_u64_61:
LFB189:
	pushq	%rbp
LCFI630:
	movq	%rsp, %rbp
LCFI631:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI632:
	ret
LFE189:
.no_dead_strip _div_pow2__div_pow2_u64_62
_div_pow2__div_pow2_u64_62:
LFB190:
	pushq	%rbp
LCFI633:
	movq	%rsp, %rbp
LCFI634:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI635:
	ret
LFE190:
.no_dead_strip _div_pow2__div_pow2_u64_63
_div_pow2__div_pow2_u64_63:
LFB191:
	pushq	%rbp
LCFI636:
	movq	%rsp, %rbp
LCFI637:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI638:
	ret
LFE191:
.no_dead_strip _div_pow2__div_pow2_I_0
_div_pow2__div_pow2_I_0:
LFB192:
	pushq	%rbp
LCFI639:
	movq	%rsp, %rbp
LCFI640:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI641:
	ret
LFE192:
.no_dead_strip _div_pow2__div_pow2_I_1
_div_pow2__div_pow2_I_1:
LFB193:
	pushq	%rbp
LCFI642:
	movq	%rsp, %rbp
LCFI643:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI644:
	ret
LFE193:
.no_dead_strip _div_pow2__div_pow2_I_2
_div_pow2__div_pow2_I_2:
LFB194:
	pushq	%rbp
LCFI645:
	movq	%rsp, %rbp
LCFI646:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI647:
	ret
LFE194:
.no_dead_strip _div_pow2__div_pow2_I_3
_div_pow2__div_pow2_I_3:
LFB195:
	pushq	%rbp
LCFI648:
	movq	%rsp, %rbp
LCFI649:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI650:
	ret
LFE195:
.no_dead_strip _div_pow2__div_pow2_I_4
_div_pow2__div_pow2_I_4:
LFB196:
	pushq	%rbp
LCFI651:
	movq	%rsp, %rbp
LCFI652:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI653:
	ret
LFE196:
.no_dead_strip _div_pow2__div_pow2_I_5
_div_pow2__div_pow2_I_5:
LFB197:
	pushq	%rbp
LCFI654:
	movq	%rsp, %rbp
LCFI655:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI656:
	ret
LFE197:
.no_dead_strip _div_pow2__div_pow2_I_6
_div_pow2__div_pow2_I_6:
LFB198:
	pushq	%rbp
LCFI657:
	movq	%rsp, %rbp
LCFI658:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI659:
	ret
LFE198:
.no_dead_strip _div_pow2__div_pow2_I_7
_div_pow2__div_pow2_I_7:
LFB199:
	pushq	%rbp
LCFI660:
	movq	%rsp, %rbp
LCFI661:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI662:
	ret
LFE199:
.no_dead_strip _div_pow2__div_pow2_I_8
_div_pow2__div_pow2_I_8:
LFB200:
	pushq	%rbp
LCFI663:
	movq	%rsp, %rbp
LCFI664:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI665:
	ret
LFE200:
.no_dead_strip _div_pow2__div_pow2_I_9
_div_pow2__div_pow2_I_9:
LFB201:
	pushq	%rbp
LCFI666:
	movq	%rsp, %rbp
LCFI667:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI668:
	ret
LFE201:
.no_dead_strip _div_pow2__div_pow2_I_10
_div_pow2__div_pow2_I_10:
LFB202:
	pushq	%rbp
LCFI669:
	movq	%rsp, %rbp
LCFI670:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI671:
	ret
LFE202:
.no_dead_strip _div_pow2__div_pow2_I_11
_div_pow2__div_pow2_I_11:
LFB203:
	pushq	%rbp
LCFI672:
	movq	%rsp, %rbp
LCFI673:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI674:
	ret
LFE203:
.no_dead_strip _div_pow2__div_pow2_I_12
_div_pow2__div_pow2_I_12:
LFB204:
	pushq	%rbp
LCFI675:
	movq	%rsp, %rbp
LCFI676:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI677:
	ret
LFE204:
.no_dead_strip _div_pow2__div_pow2_I_13
_div_pow2__div_pow2_I_13:
LFB205:
	pushq	%rbp
LCFI678:
	movq	%rsp, %rbp
LCFI679:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI680:
	ret
LFE205:
.no_dead_strip _div_pow2__div_pow2_I_14
_div_pow2__div_pow2_I_14:
LFB206:
	pushq	%rbp
LCFI681:
	movq	%rsp, %rbp
LCFI682:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI683:
	ret
LFE206:
.no_dead_strip _div_pow2__div_pow2_I_15
_div_pow2__div_pow2_I_15:
LFB207:
	pushq	%rbp
LCFI684:
	movq	%rsp, %rbp
LCFI685:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI686:
	ret
LFE207:
.no_dead_strip _div_pow2__div_pow2_I_16
_div_pow2__div_pow2_I_16:
LFB208:
	pushq	%rbp
LCFI687:
	movq	%rsp, %rbp
LCFI688:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI689:
	ret
LFE208:
.no_dead_strip _div_pow2__div_pow2_I_17
_div_pow2__div_pow2_I_17:
LFB209:
	pushq	%rbp
LCFI690:
	movq	%rsp, %rbp
LCFI691:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI692:
	ret
LFE209:
.no_dead_strip _div_pow2__div_pow2_I_18
_div_pow2__div_pow2_I_18:
LFB210:
	pushq	%rbp
LCFI693:
	movq	%rsp, %rbp
LCFI694:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI695:
	ret
LFE210:
.no_dead_strip _div_pow2__div_pow2_I_19
_div_pow2__div_pow2_I_19:
LFB211:
	pushq	%rbp
LCFI696:
	movq	%rsp, %rbp
LCFI697:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI698:
	ret
LFE211:
.no_dead_strip _div_pow2__div_pow2_I_20
_div_pow2__div_pow2_I_20:
LFB212:
	pushq	%rbp
LCFI699:
	movq	%rsp, %rbp
LCFI700:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI701:
	ret
LFE212:
.no_dead_strip _div_pow2__div_pow2_I_21
_div_pow2__div_pow2_I_21:
LFB213:
	pushq	%rbp
LCFI702:
	movq	%rsp, %rbp
LCFI703:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI704:
	ret
LFE213:
.no_dead_strip _div_pow2__div_pow2_I_22
_div_pow2__div_pow2_I_22:
LFB214:
	pushq	%rbp
LCFI705:
	movq	%rsp, %rbp
LCFI706:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI707:
	ret
LFE214:
.no_dead_strip _div_pow2__div_pow2_I_23
_div_pow2__div_pow2_I_23:
LFB215:
	pushq	%rbp
LCFI708:
	movq	%rsp, %rbp
LCFI709:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI710:
	ret
LFE215:
.no_dead_strip _div_pow2__div_pow2_I_24
_div_pow2__div_pow2_I_24:
LFB216:
	pushq	%rbp
LCFI711:
	movq	%rsp, %rbp
LCFI712:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI713:
	ret
LFE216:
.no_dead_strip _div_pow2__div_pow2_I_25
_div_pow2__div_pow2_I_25:
LFB217:
	pushq	%rbp
LCFI714:
	movq	%rsp, %rbp
LCFI715:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI716:
	ret
LFE217:
.no_dead_strip _div_pow2__div_pow2_I_26
_div_pow2__div_pow2_I_26:
LFB218:
	pushq	%rbp
LCFI717:
	movq	%rsp, %rbp
LCFI718:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI719:
	ret
LFE218:
.no_dead_strip _div_pow2__div_pow2_I_27
_div_pow2__div_pow2_I_27:
LFB219:
	pushq	%rbp
LCFI720:
	movq	%rsp, %rbp
LCFI721:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI722:
	ret
LFE219:
.no_dead_strip _div_pow2__div_pow2_I_28
_div_pow2__div_pow2_I_28:
LFB220:
	pushq	%rbp
LCFI723:
	movq	%rsp, %rbp
LCFI724:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI725:
	ret
LFE220:
.no_dead_strip _div_pow2__div_pow2_I_29
_div_pow2__div_pow2_I_29:
LFB221:
	pushq	%rbp
LCFI726:
	movq	%rsp, %rbp
LCFI727:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI728:
	ret
LFE221:
.no_dead_strip _div_pow2__div_pow2_I_30
_div_pow2__div_pow2_I_30:
LFB222:
	pushq	%rbp
LCFI729:
	movq	%rsp, %rbp
LCFI730:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI731:
	ret
LFE222:
.no_dead_strip _div_pow2__div_pow2_I_31
_div_pow2__div_pow2_I_31:
LFB223:
	pushq	%rbp
LCFI732:
	movq	%rsp, %rbp
LCFI733:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI734:
	ret
LFE223:
.no_dead_strip _div_pow2__div_pow2_I_32
_div_pow2__div_pow2_I_32:
LFB224:
	pushq	%rbp
LCFI735:
	movq	%rsp, %rbp
LCFI736:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI737:
	ret
LFE224:
.no_dead_strip _div_pow2__div_pow2_I_33
_div_pow2__div_pow2_I_33:
LFB225:
	pushq	%rbp
LCFI738:
	movq	%rsp, %rbp
LCFI739:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI740:
	ret
LFE225:
.no_dead_strip _div_pow2__div_pow2_I_34
_div_pow2__div_pow2_I_34:
LFB226:
	pushq	%rbp
LCFI741:
	movq	%rsp, %rbp
LCFI742:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI743:
	ret
LFE226:
.no_dead_strip _div_pow2__div_pow2_I_35
_div_pow2__div_pow2_I_35:
LFB227:
	pushq	%rbp
LCFI744:
	movq	%rsp, %rbp
LCFI745:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI746:
	ret
LFE227:
.no_dead_strip _div_pow2__div_pow2_I_36
_div_pow2__div_pow2_I_36:
LFB228:
	pushq	%rbp
LCFI747:
	movq	%rsp, %rbp
LCFI748:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI749:
	ret
LFE228:
.no_dead_strip _div_pow2__div_pow2_I_37
_div_pow2__div_pow2_I_37:
LFB229:
	pushq	%rbp
LCFI750:
	movq	%rsp, %rbp
LCFI751:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI752:
	ret
LFE229:
.no_dead_strip _div_pow2__div_pow2_I_38
_div_pow2__div_pow2_I_38:
LFB230:
	pushq	%rbp
LCFI753:
	movq	%rsp, %rbp
LCFI754:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI755:
	ret
LFE230:
.no_dead_strip _div_pow2__div_pow2_I_39
_div_pow2__div_pow2_I_39:
LFB231:
	pushq	%rbp
LCFI756:
	movq	%rsp, %rbp
LCFI757:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI758:
	ret
LFE231:
.no_dead_strip _div_pow2__div_pow2_I_40
_div_pow2__div_pow2_I_40:
LFB232:
	pushq	%rbp
LCFI759:
	movq	%rsp, %rbp
LCFI760:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI761:
	ret
LFE232:
.no_dead_strip _div_pow2__div_pow2_I_41
_div_pow2__div_pow2_I_41:
LFB233:
	pushq	%rbp
LCFI762:
	movq	%rsp, %rbp
LCFI763:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI764:
	ret
LFE233:
.no_dead_strip _div_pow2__div_pow2_I_42
_div_pow2__div_pow2_I_42:
LFB234:
	pushq	%rbp
LCFI765:
	movq	%rsp, %rbp
LCFI766:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI767:
	ret
LFE234:
.no_dead_strip _div_pow2__div_pow2_I_43
_div_pow2__div_pow2_I_43:
LFB235:
	pushq	%rbp
LCFI768:
	movq	%rsp, %rbp
LCFI769:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI770:
	ret
LFE235:
.no_dead_strip _div_pow2__div_pow2_I_44
_div_pow2__div_pow2_I_44:
LFB236:
	pushq	%rbp
LCFI771:
	movq	%rsp, %rbp
LCFI772:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI773:
	ret
LFE236:
.no_dead_strip _div_pow2__div_pow2_I_45
_div_pow2__div_pow2_I_45:
LFB237:
	pushq	%rbp
LCFI774:
	movq	%rsp, %rbp
LCFI775:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI776:
	ret
LFE237:
.no_dead_strip _div_pow2__div_pow2_I_46
_div_pow2__div_pow2_I_46:
LFB238:
	pushq	%rbp
LCFI777:
	movq	%rsp, %rbp
LCFI778:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI779:
	ret
LFE238:
.no_dead_strip _div_pow2__div_pow2_I_47
_div_pow2__div_pow2_I_47:
LFB239:
	pushq	%rbp
LCFI780:
	movq	%rsp, %rbp
LCFI781:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI782:
	ret
LFE239:
.no_dead_strip _div_pow2__div_pow2_I_48
_div_pow2__div_pow2_I_48:
LFB240:
	pushq	%rbp
LCFI783:
	movq	%rsp, %rbp
LCFI784:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI785:
	ret
LFE240:
.no_dead_strip _div_pow2__div_pow2_I_49
_div_pow2__div_pow2_I_49:
LFB241:
	pushq	%rbp
LCFI786:
	movq	%rsp, %rbp
LCFI787:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI788:
	ret
LFE241:
.no_dead_strip _div_pow2__div_pow2_I_50
_div_pow2__div_pow2_I_50:
LFB242:
	pushq	%rbp
LCFI789:
	movq	%rsp, %rbp
LCFI790:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI791:
	ret
LFE242:
.no_dead_strip _div_pow2__div_pow2_I_51
_div_pow2__div_pow2_I_51:
LFB243:
	pushq	%rbp
LCFI792:
	movq	%rsp, %rbp
LCFI793:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI794:
	ret
LFE243:
.no_dead_strip _div_pow2__div_pow2_I_52
_div_pow2__div_pow2_I_52:
LFB244:
	pushq	%rbp
LCFI795:
	movq	%rsp, %rbp
LCFI796:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI797:
	ret
LFE244:
.no_dead_strip _div_pow2__div_pow2_I_53
_div_pow2__div_pow2_I_53:
LFB245:
	pushq	%rbp
LCFI798:
	movq	%rsp, %rbp
LCFI799:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI800:
	ret
LFE245:
.no_dead_strip _div_pow2__div_pow2_I_54
_div_pow2__div_pow2_I_54:
LFB246:
	pushq	%rbp
LCFI801:
	movq	%rsp, %rbp
LCFI802:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI803:
	ret
LFE246:
.no_dead_strip _div_pow2__div_pow2_I_55
_div_pow2__div_pow2_I_55:
LFB247:
	pushq	%rbp
LCFI804:
	movq	%rsp, %rbp
LCFI805:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI806:
	ret
LFE247:
.no_dead_strip _div_pow2__div_pow2_I_56
_div_pow2__div_pow2_I_56:
LFB248:
	pushq	%rbp
LCFI807:
	movq	%rsp, %rbp
LCFI808:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI809:
	ret
LFE248:
.no_dead_strip _div_pow2__div_pow2_I_57
_div_pow2__div_pow2_I_57:
LFB249:
	pushq	%rbp
LCFI810:
	movq	%rsp, %rbp
LCFI811:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI812:
	ret
LFE249:
.no_dead_strip _div_pow2__div_pow2_I_58
_div_pow2__div_pow2_I_58:
LFB250:
	pushq	%rbp
LCFI813:
	movq	%rsp, %rbp
LCFI814:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI815:
	ret
LFE250:
.no_dead_strip _div_pow2__div_pow2_I_59
_div_pow2__div_pow2_I_59:
LFB251:
	pushq	%rbp
LCFI816:
	movq	%rsp, %rbp
LCFI817:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI818:
	ret
LFE251:
.no_dead_strip _div_pow2__div_pow2_I_60
_div_pow2__div_pow2_I_60:
LFB252:
	pushq	%rbp
LCFI819:
	movq	%rsp, %rbp
LCFI820:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI821:
	ret
LFE252:
.no_dead_strip _div_pow2__div_pow2_I_61
_div_pow2__div_pow2_I_61:
LFB253:
	pushq	%rbp
LCFI822:
	movq	%rsp, %rbp
LCFI823:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI824:
	ret
LFE253:
.no_dead_strip _div_pow2__div_pow2_I_62
_div_pow2__div_pow2_I_62:
LFB254:
	pushq	%rbp
LCFI825:
	movq	%rsp, %rbp
LCFI826:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI827:
	ret
LFE254:
.no_dead_strip _div_pow2__div_pow2_I_63
_div_pow2__div_pow2_I_63:
LFB255:
	pushq	%rbp
LCFI828:
	movq	%rsp, %rbp
LCFI829:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI830:
	ret
LFE255:
.no_dead_strip _div_pow2__div_pow2_C_0
_div_pow2__div_pow2_C_0:
LFB256:
	pushq	%rbp
LCFI831:
	movq	%rsp, %rbp
LCFI832:
	subq	$16, %rsp
LCFI833:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	testq	%rax, %rax
	jns	L383
	movl	$18145, %edi
	call	__m3_fault
L383:
	movq	-8(%rbp), %rax
	leave
LCFI834:
	ret
LFE256:
.no_dead_strip _div_pow2__div_pow2_C_1
_div_pow2__div_pow2_C_1:
LFB257:
	pushq	%rbp
LCFI835:
	movq	%rsp, %rbp
LCFI836:
	subq	$16, %rsp
LCFI837:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	testq	%rax, %rax
	jns	L385
	movl	$18209, %edi
	call	__m3_fault
L385:
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI838:
	ret
LFE257:
.no_dead_strip _div_pow2__div_pow2_C_2
_div_pow2__div_pow2_C_2:
LFB258:
	pushq	%rbp
LCFI839:
	movq	%rsp, %rbp
LCFI840:
	subq	$16, %rsp
LCFI841:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	testq	%rax, %rax
	jns	L387
	movl	$18273, %edi
	call	__m3_fault
L387:
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI842:
	ret
LFE258:
.no_dead_strip _div_pow2__div_pow2_C_3
_div_pow2__div_pow2_C_3:
LFB259:
	pushq	%rbp
LCFI843:
	movq	%rsp, %rbp
LCFI844:
	subq	$16, %rsp
LCFI845:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	testq	%rax, %rax
	jns	L389
	movl	$18337, %edi
	call	__m3_fault
L389:
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI846:
	ret
LFE259:
.no_dead_strip _div_pow2__div_pow2_C_4
_div_pow2__div_pow2_C_4:
LFB260:
	pushq	%rbp
LCFI847:
	movq	%rsp, %rbp
LCFI848:
	subq	$16, %rsp
LCFI849:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	testq	%rax, %rax
	jns	L391
	movl	$18401, %edi
	call	__m3_fault
L391:
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI850:
	ret
LFE260:
.no_dead_strip _div_pow2__div_pow2_C_5
_div_pow2__div_pow2_C_5:
LFB261:
	pushq	%rbp
LCFI851:
	movq	%rsp, %rbp
LCFI852:
	subq	$16, %rsp
LCFI853:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	testq	%rax, %rax
	jns	L393
	movl	$18465, %edi
	call	__m3_fault
L393:
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI854:
	ret
LFE261:
.no_dead_strip _div_pow2__div_pow2_C_6
_div_pow2__div_pow2_C_6:
LFB262:
	pushq	%rbp
LCFI855:
	movq	%rsp, %rbp
LCFI856:
	subq	$16, %rsp
LCFI857:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	testq	%rax, %rax
	jns	L395
	movl	$18529, %edi
	call	__m3_fault
L395:
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI858:
	ret
LFE262:
.no_dead_strip _div_pow2__div_pow2_C_7
_div_pow2__div_pow2_C_7:
LFB263:
	pushq	%rbp
LCFI859:
	movq	%rsp, %rbp
LCFI860:
	subq	$16, %rsp
LCFI861:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	testq	%rax, %rax
	jns	L397
	movl	$18593, %edi
	call	__m3_fault
L397:
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI862:
	ret
LFE263:
.no_dead_strip _div_pow2__div_pow2_C_8
_div_pow2__div_pow2_C_8:
LFB264:
	pushq	%rbp
LCFI863:
	movq	%rsp, %rbp
LCFI864:
	subq	$16, %rsp
LCFI865:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	testq	%rax, %rax
	jns	L399
	movl	$18657, %edi
	call	__m3_fault
L399:
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI866:
	ret
LFE264:
.no_dead_strip _div_pow2__div_pow2_C_9
_div_pow2__div_pow2_C_9:
LFB265:
	pushq	%rbp
LCFI867:
	movq	%rsp, %rbp
LCFI868:
	subq	$16, %rsp
LCFI869:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	testq	%rax, %rax
	jns	L401
	movl	$18721, %edi
	call	__m3_fault
L401:
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI870:
	ret
LFE265:
.no_dead_strip _div_pow2__div_pow2_C_10
_div_pow2__div_pow2_C_10:
LFB266:
	pushq	%rbp
LCFI871:
	movq	%rsp, %rbp
LCFI872:
	subq	$16, %rsp
LCFI873:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	testq	%rax, %rax
	jns	L403
	movl	$18785, %edi
	call	__m3_fault
L403:
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI874:
	ret
LFE266:
.no_dead_strip _div_pow2__div_pow2_C_11
_div_pow2__div_pow2_C_11:
LFB267:
	pushq	%rbp
LCFI875:
	movq	%rsp, %rbp
LCFI876:
	subq	$16, %rsp
LCFI877:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	testq	%rax, %rax
	jns	L405
	movl	$18849, %edi
	call	__m3_fault
L405:
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI878:
	ret
LFE267:
.no_dead_strip _div_pow2__div_pow2_C_12
_div_pow2__div_pow2_C_12:
LFB268:
	pushq	%rbp
LCFI879:
	movq	%rsp, %rbp
LCFI880:
	subq	$16, %rsp
LCFI881:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	testq	%rax, %rax
	jns	L407
	movl	$18913, %edi
	call	__m3_fault
L407:
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI882:
	ret
LFE268:
.no_dead_strip _div_pow2__div_pow2_C_13
_div_pow2__div_pow2_C_13:
LFB269:
	pushq	%rbp
LCFI883:
	movq	%rsp, %rbp
LCFI884:
	subq	$16, %rsp
LCFI885:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	testq	%rax, %rax
	jns	L409
	movl	$18977, %edi
	call	__m3_fault
L409:
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI886:
	ret
LFE269:
.no_dead_strip _div_pow2__div_pow2_C_14
_div_pow2__div_pow2_C_14:
LFB270:
	pushq	%rbp
LCFI887:
	movq	%rsp, %rbp
LCFI888:
	subq	$16, %rsp
LCFI889:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	testq	%rax, %rax
	jns	L411
	movl	$19041, %edi
	call	__m3_fault
L411:
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI890:
	ret
LFE270:
.no_dead_strip _div_pow2__div_pow2_C_15
_div_pow2__div_pow2_C_15:
LFB271:
	pushq	%rbp
LCFI891:
	movq	%rsp, %rbp
LCFI892:
	subq	$16, %rsp
LCFI893:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	testq	%rax, %rax
	jns	L413
	movl	$19105, %edi
	call	__m3_fault
L413:
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI894:
	ret
LFE271:
.no_dead_strip _div_pow2__div_pow2_C_16
_div_pow2__div_pow2_C_16:
LFB272:
	pushq	%rbp
LCFI895:
	movq	%rsp, %rbp
LCFI896:
	subq	$16, %rsp
LCFI897:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	testq	%rax, %rax
	jns	L415
	movl	$19169, %edi
	call	__m3_fault
L415:
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI898:
	ret
LFE272:
.no_dead_strip _div_pow2__div_pow2_C_17
_div_pow2__div_pow2_C_17:
LFB273:
	pushq	%rbp
LCFI899:
	movq	%rsp, %rbp
LCFI900:
	subq	$16, %rsp
LCFI901:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	testq	%rax, %rax
	jns	L417
	movl	$19233, %edi
	call	__m3_fault
L417:
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI902:
	ret
LFE273:
.no_dead_strip _div_pow2__div_pow2_C_18
_div_pow2__div_pow2_C_18:
LFB274:
	pushq	%rbp
LCFI903:
	movq	%rsp, %rbp
LCFI904:
	subq	$16, %rsp
LCFI905:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	testq	%rax, %rax
	jns	L419
	movl	$19297, %edi
	call	__m3_fault
L419:
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI906:
	ret
LFE274:
.no_dead_strip _div_pow2__div_pow2_C_19
_div_pow2__div_pow2_C_19:
LFB275:
	pushq	%rbp
LCFI907:
	movq	%rsp, %rbp
LCFI908:
	subq	$16, %rsp
LCFI909:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	testq	%rax, %rax
	jns	L421
	movl	$19361, %edi
	call	__m3_fault
L421:
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI910:
	ret
LFE275:
.no_dead_strip _div_pow2__div_pow2_C_20
_div_pow2__div_pow2_C_20:
LFB276:
	pushq	%rbp
LCFI911:
	movq	%rsp, %rbp
LCFI912:
	subq	$16, %rsp
LCFI913:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	testq	%rax, %rax
	jns	L423
	movl	$19425, %edi
	call	__m3_fault
L423:
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI914:
	ret
LFE276:
.no_dead_strip _div_pow2__div_pow2_C_21
_div_pow2__div_pow2_C_21:
LFB277:
	pushq	%rbp
LCFI915:
	movq	%rsp, %rbp
LCFI916:
	subq	$16, %rsp
LCFI917:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	testq	%rax, %rax
	jns	L425
	movl	$19489, %edi
	call	__m3_fault
L425:
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI918:
	ret
LFE277:
.no_dead_strip _div_pow2__div_pow2_C_22
_div_pow2__div_pow2_C_22:
LFB278:
	pushq	%rbp
LCFI919:
	movq	%rsp, %rbp
LCFI920:
	subq	$16, %rsp
LCFI921:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	testq	%rax, %rax
	jns	L427
	movl	$19553, %edi
	call	__m3_fault
L427:
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI922:
	ret
LFE278:
.no_dead_strip _div_pow2__div_pow2_C_23
_div_pow2__div_pow2_C_23:
LFB279:
	pushq	%rbp
LCFI923:
	movq	%rsp, %rbp
LCFI924:
	subq	$16, %rsp
LCFI925:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	testq	%rax, %rax
	jns	L429
	movl	$19617, %edi
	call	__m3_fault
L429:
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI926:
	ret
LFE279:
.no_dead_strip _div_pow2__div_pow2_C_24
_div_pow2__div_pow2_C_24:
LFB280:
	pushq	%rbp
LCFI927:
	movq	%rsp, %rbp
LCFI928:
	subq	$16, %rsp
LCFI929:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	testq	%rax, %rax
	jns	L431
	movl	$19681, %edi
	call	__m3_fault
L431:
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI930:
	ret
LFE280:
.no_dead_strip _div_pow2__div_pow2_C_25
_div_pow2__div_pow2_C_25:
LFB281:
	pushq	%rbp
LCFI931:
	movq	%rsp, %rbp
LCFI932:
	subq	$16, %rsp
LCFI933:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	testq	%rax, %rax
	jns	L433
	movl	$19745, %edi
	call	__m3_fault
L433:
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI934:
	ret
LFE281:
.no_dead_strip _div_pow2__div_pow2_C_26
_div_pow2__div_pow2_C_26:
LFB282:
	pushq	%rbp
LCFI935:
	movq	%rsp, %rbp
LCFI936:
	subq	$16, %rsp
LCFI937:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	testq	%rax, %rax
	jns	L435
	movl	$19809, %edi
	call	__m3_fault
L435:
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI938:
	ret
LFE282:
.no_dead_strip _div_pow2__div_pow2_C_27
_div_pow2__div_pow2_C_27:
LFB283:
	pushq	%rbp
LCFI939:
	movq	%rsp, %rbp
LCFI940:
	subq	$16, %rsp
LCFI941:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	testq	%rax, %rax
	jns	L437
	movl	$19873, %edi
	call	__m3_fault
L437:
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI942:
	ret
LFE283:
.no_dead_strip _div_pow2__div_pow2_C_28
_div_pow2__div_pow2_C_28:
LFB284:
	pushq	%rbp
LCFI943:
	movq	%rsp, %rbp
LCFI944:
	subq	$16, %rsp
LCFI945:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	testq	%rax, %rax
	jns	L439
	movl	$19937, %edi
	call	__m3_fault
L439:
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI946:
	ret
LFE284:
.no_dead_strip _div_pow2__div_pow2_C_29
_div_pow2__div_pow2_C_29:
LFB285:
	pushq	%rbp
LCFI947:
	movq	%rsp, %rbp
LCFI948:
	subq	$16, %rsp
LCFI949:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	testq	%rax, %rax
	jns	L441
	movl	$20001, %edi
	call	__m3_fault
L441:
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI950:
	ret
LFE285:
.no_dead_strip _div_pow2__div_pow2_C_30
_div_pow2__div_pow2_C_30:
LFB286:
	pushq	%rbp
LCFI951:
	movq	%rsp, %rbp
LCFI952:
	subq	$16, %rsp
LCFI953:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	testq	%rax, %rax
	jns	L443
	movl	$20065, %edi
	call	__m3_fault
L443:
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI954:
	ret
LFE286:
.no_dead_strip _div_pow2__div_pow2_C_31
_div_pow2__div_pow2_C_31:
LFB287:
	pushq	%rbp
LCFI955:
	movq	%rsp, %rbp
LCFI956:
	subq	$16, %rsp
LCFI957:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	testq	%rax, %rax
	jns	L445
	movl	$20129, %edi
	call	__m3_fault
L445:
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI958:
	ret
LFE287:
.no_dead_strip _div_pow2__div_pow2_C_32
_div_pow2__div_pow2_C_32:
LFB288:
	pushq	%rbp
LCFI959:
	movq	%rsp, %rbp
LCFI960:
	subq	$16, %rsp
LCFI961:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	testq	%rax, %rax
	jns	L447
	movl	$20193, %edi
	call	__m3_fault
L447:
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI962:
	ret
LFE288:
.no_dead_strip _div_pow2__div_pow2_C_33
_div_pow2__div_pow2_C_33:
LFB289:
	pushq	%rbp
LCFI963:
	movq	%rsp, %rbp
LCFI964:
	subq	$16, %rsp
LCFI965:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	testq	%rax, %rax
	jns	L449
	movl	$20257, %edi
	call	__m3_fault
L449:
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI966:
	ret
LFE289:
.no_dead_strip _div_pow2__div_pow2_C_34
_div_pow2__div_pow2_C_34:
LFB290:
	pushq	%rbp
LCFI967:
	movq	%rsp, %rbp
LCFI968:
	subq	$16, %rsp
LCFI969:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	testq	%rax, %rax
	jns	L451
	movl	$20321, %edi
	call	__m3_fault
L451:
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI970:
	ret
LFE290:
.no_dead_strip _div_pow2__div_pow2_C_35
_div_pow2__div_pow2_C_35:
LFB291:
	pushq	%rbp
LCFI971:
	movq	%rsp, %rbp
LCFI972:
	subq	$16, %rsp
LCFI973:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	testq	%rax, %rax
	jns	L453
	movl	$20385, %edi
	call	__m3_fault
L453:
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI974:
	ret
LFE291:
.no_dead_strip _div_pow2__div_pow2_C_36
_div_pow2__div_pow2_C_36:
LFB292:
	pushq	%rbp
LCFI975:
	movq	%rsp, %rbp
LCFI976:
	subq	$16, %rsp
LCFI977:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	testq	%rax, %rax
	jns	L455
	movl	$20449, %edi
	call	__m3_fault
L455:
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI978:
	ret
LFE292:
.no_dead_strip _div_pow2__div_pow2_C_37
_div_pow2__div_pow2_C_37:
LFB293:
	pushq	%rbp
LCFI979:
	movq	%rsp, %rbp
LCFI980:
	subq	$16, %rsp
LCFI981:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	testq	%rax, %rax
	jns	L457
	movl	$20513, %edi
	call	__m3_fault
L457:
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI982:
	ret
LFE293:
.no_dead_strip _div_pow2__div_pow2_C_38
_div_pow2__div_pow2_C_38:
LFB294:
	pushq	%rbp
LCFI983:
	movq	%rsp, %rbp
LCFI984:
	subq	$16, %rsp
LCFI985:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	testq	%rax, %rax
	jns	L459
	movl	$20577, %edi
	call	__m3_fault
L459:
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI986:
	ret
LFE294:
.no_dead_strip _div_pow2__div_pow2_C_39
_div_pow2__div_pow2_C_39:
LFB295:
	pushq	%rbp
LCFI987:
	movq	%rsp, %rbp
LCFI988:
	subq	$16, %rsp
LCFI989:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	testq	%rax, %rax
	jns	L461
	movl	$20641, %edi
	call	__m3_fault
L461:
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI990:
	ret
LFE295:
.no_dead_strip _div_pow2__div_pow2_C_40
_div_pow2__div_pow2_C_40:
LFB296:
	pushq	%rbp
LCFI991:
	movq	%rsp, %rbp
LCFI992:
	subq	$16, %rsp
LCFI993:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	testq	%rax, %rax
	jns	L463
	movl	$20705, %edi
	call	__m3_fault
L463:
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI994:
	ret
LFE296:
.no_dead_strip _div_pow2__div_pow2_C_41
_div_pow2__div_pow2_C_41:
LFB297:
	pushq	%rbp
LCFI995:
	movq	%rsp, %rbp
LCFI996:
	subq	$16, %rsp
LCFI997:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	testq	%rax, %rax
	jns	L465
	movl	$20769, %edi
	call	__m3_fault
L465:
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI998:
	ret
LFE297:
.no_dead_strip _div_pow2__div_pow2_C_42
_div_pow2__div_pow2_C_42:
LFB298:
	pushq	%rbp
LCFI999:
	movq	%rsp, %rbp
LCFI1000:
	subq	$16, %rsp
LCFI1001:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	testq	%rax, %rax
	jns	L467
	movl	$20833, %edi
	call	__m3_fault
L467:
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI1002:
	ret
LFE298:
.no_dead_strip _div_pow2__div_pow2_C_43
_div_pow2__div_pow2_C_43:
LFB299:
	pushq	%rbp
LCFI1003:
	movq	%rsp, %rbp
LCFI1004:
	subq	$16, %rsp
LCFI1005:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	testq	%rax, %rax
	jns	L469
	movl	$20897, %edi
	call	__m3_fault
L469:
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI1006:
	ret
LFE299:
.no_dead_strip _div_pow2__div_pow2_C_44
_div_pow2__div_pow2_C_44:
LFB300:
	pushq	%rbp
LCFI1007:
	movq	%rsp, %rbp
LCFI1008:
	subq	$16, %rsp
LCFI1009:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	testq	%rax, %rax
	jns	L471
	movl	$20961, %edi
	call	__m3_fault
L471:
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI1010:
	ret
LFE300:
.no_dead_strip _div_pow2__div_pow2_C_45
_div_pow2__div_pow2_C_45:
LFB301:
	pushq	%rbp
LCFI1011:
	movq	%rsp, %rbp
LCFI1012:
	subq	$16, %rsp
LCFI1013:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	testq	%rax, %rax
	jns	L473
	movl	$21025, %edi
	call	__m3_fault
L473:
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI1014:
	ret
LFE301:
.no_dead_strip _div_pow2__div_pow2_C_46
_div_pow2__div_pow2_C_46:
LFB302:
	pushq	%rbp
LCFI1015:
	movq	%rsp, %rbp
LCFI1016:
	subq	$16, %rsp
LCFI1017:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	testq	%rax, %rax
	jns	L475
	movl	$21089, %edi
	call	__m3_fault
L475:
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI1018:
	ret
LFE302:
.no_dead_strip _div_pow2__div_pow2_C_47
_div_pow2__div_pow2_C_47:
LFB303:
	pushq	%rbp
LCFI1019:
	movq	%rsp, %rbp
LCFI1020:
	subq	$16, %rsp
LCFI1021:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	testq	%rax, %rax
	jns	L477
	movl	$21153, %edi
	call	__m3_fault
L477:
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI1022:
	ret
LFE303:
.no_dead_strip _div_pow2__div_pow2_C_48
_div_pow2__div_pow2_C_48:
LFB304:
	pushq	%rbp
LCFI1023:
	movq	%rsp, %rbp
LCFI1024:
	subq	$16, %rsp
LCFI1025:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	testq	%rax, %rax
	jns	L479
	movl	$21217, %edi
	call	__m3_fault
L479:
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI1026:
	ret
LFE304:
.no_dead_strip _div_pow2__div_pow2_C_49
_div_pow2__div_pow2_C_49:
LFB305:
	pushq	%rbp
LCFI1027:
	movq	%rsp, %rbp
LCFI1028:
	subq	$16, %rsp
LCFI1029:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	testq	%rax, %rax
	jns	L481
	movl	$21281, %edi
	call	__m3_fault
L481:
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI1030:
	ret
LFE305:
.no_dead_strip _div_pow2__div_pow2_C_50
_div_pow2__div_pow2_C_50:
LFB306:
	pushq	%rbp
LCFI1031:
	movq	%rsp, %rbp
LCFI1032:
	subq	$16, %rsp
LCFI1033:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	testq	%rax, %rax
	jns	L483
	movl	$21345, %edi
	call	__m3_fault
L483:
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI1034:
	ret
LFE306:
.no_dead_strip _div_pow2__div_pow2_C_51
_div_pow2__div_pow2_C_51:
LFB307:
	pushq	%rbp
LCFI1035:
	movq	%rsp, %rbp
LCFI1036:
	subq	$16, %rsp
LCFI1037:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	testq	%rax, %rax
	jns	L485
	movl	$21409, %edi
	call	__m3_fault
L485:
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI1038:
	ret
LFE307:
.no_dead_strip _div_pow2__div_pow2_C_52
_div_pow2__div_pow2_C_52:
LFB308:
	pushq	%rbp
LCFI1039:
	movq	%rsp, %rbp
LCFI1040:
	subq	$16, %rsp
LCFI1041:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	testq	%rax, %rax
	jns	L487
	movl	$21473, %edi
	call	__m3_fault
L487:
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI1042:
	ret
LFE308:
.no_dead_strip _div_pow2__div_pow2_C_53
_div_pow2__div_pow2_C_53:
LFB309:
	pushq	%rbp
LCFI1043:
	movq	%rsp, %rbp
LCFI1044:
	subq	$16, %rsp
LCFI1045:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	testq	%rax, %rax
	jns	L489
	movl	$21537, %edi
	call	__m3_fault
L489:
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI1046:
	ret
LFE309:
.no_dead_strip _div_pow2__div_pow2_C_54
_div_pow2__div_pow2_C_54:
LFB310:
	pushq	%rbp
LCFI1047:
	movq	%rsp, %rbp
LCFI1048:
	subq	$16, %rsp
LCFI1049:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	testq	%rax, %rax
	jns	L491
	movl	$21601, %edi
	call	__m3_fault
L491:
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI1050:
	ret
LFE310:
.no_dead_strip _div_pow2__div_pow2_C_55
_div_pow2__div_pow2_C_55:
LFB311:
	pushq	%rbp
LCFI1051:
	movq	%rsp, %rbp
LCFI1052:
	subq	$16, %rsp
LCFI1053:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	testq	%rax, %rax
	jns	L493
	movl	$21665, %edi
	call	__m3_fault
L493:
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI1054:
	ret
LFE311:
.no_dead_strip _div_pow2__div_pow2_C_56
_div_pow2__div_pow2_C_56:
LFB312:
	pushq	%rbp
LCFI1055:
	movq	%rsp, %rbp
LCFI1056:
	subq	$16, %rsp
LCFI1057:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	testq	%rax, %rax
	jns	L495
	movl	$21729, %edi
	call	__m3_fault
L495:
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI1058:
	ret
LFE312:
.no_dead_strip _div_pow2__div_pow2_C_57
_div_pow2__div_pow2_C_57:
LFB313:
	pushq	%rbp
LCFI1059:
	movq	%rsp, %rbp
LCFI1060:
	subq	$16, %rsp
LCFI1061:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	testq	%rax, %rax
	jns	L497
	movl	$21793, %edi
	call	__m3_fault
L497:
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI1062:
	ret
LFE313:
.no_dead_strip _div_pow2__div_pow2_C_58
_div_pow2__div_pow2_C_58:
LFB314:
	pushq	%rbp
LCFI1063:
	movq	%rsp, %rbp
LCFI1064:
	subq	$16, %rsp
LCFI1065:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	testq	%rax, %rax
	jns	L499
	movl	$21857, %edi
	call	__m3_fault
L499:
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI1066:
	ret
LFE314:
.no_dead_strip _div_pow2__div_pow2_C_59
_div_pow2__div_pow2_C_59:
LFB315:
	pushq	%rbp
LCFI1067:
	movq	%rsp, %rbp
LCFI1068:
	subq	$16, %rsp
LCFI1069:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	testq	%rax, %rax
	jns	L501
	movl	$21921, %edi
	call	__m3_fault
L501:
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI1070:
	ret
LFE315:
.no_dead_strip _div_pow2__div_pow2_C_60
_div_pow2__div_pow2_C_60:
LFB316:
	pushq	%rbp
LCFI1071:
	movq	%rsp, %rbp
LCFI1072:
	subq	$16, %rsp
LCFI1073:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	testq	%rax, %rax
	jns	L503
	movl	$21985, %edi
	call	__m3_fault
L503:
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI1074:
	ret
LFE316:
.no_dead_strip _div_pow2__div_pow2_C_61
_div_pow2__div_pow2_C_61:
LFB317:
	pushq	%rbp
LCFI1075:
	movq	%rsp, %rbp
LCFI1076:
	subq	$16, %rsp
LCFI1077:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	testq	%rax, %rax
	jns	L505
	movl	$22049, %edi
	call	__m3_fault
L505:
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI1078:
	ret
LFE317:
.no_dead_strip _div_pow2__div_pow2_C_62
_div_pow2__div_pow2_C_62:
LFB318:
	pushq	%rbp
LCFI1079:
	movq	%rsp, %rbp
LCFI1080:
	subq	$16, %rsp
LCFI1081:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	testq	%rax, %rax
	jns	L507
	movl	$22113, %edi
	call	__m3_fault
L507:
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI1082:
	ret
LFE318:
.no_dead_strip _div_pow2__div_pow2_C_63
_div_pow2__div_pow2_C_63:
LFB319:
	pushq	%rbp
LCFI1083:
	movq	%rsp, %rbp
LCFI1084:
	subq	$16, %rsp
LCFI1085:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	testq	%rax, %rax
	jns	L509
	movl	$22177, %edi
	call	__m3_fault
L509:
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI1086:
	ret
LFE319:
.no_dead_strip _div_pow2__div_pow2_L_0
_div_pow2__div_pow2_L_0:
LFB320:
	pushq	%rbp
LCFI1087:
	movq	%rsp, %rbp
LCFI1088:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leave
LCFI1089:
	ret
LFE320:
.no_dead_strip _div_pow2__div_pow2_L_1
_div_pow2__div_pow2_L_1:
LFB321:
	pushq	%rbp
LCFI1090:
	movq	%rsp, %rbp
LCFI1091:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI1092:
	ret
LFE321:
.no_dead_strip _div_pow2__div_pow2_L_2
_div_pow2__div_pow2_L_2:
LFB322:
	pushq	%rbp
LCFI1093:
	movq	%rsp, %rbp
LCFI1094:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI1095:
	ret
LFE322:
.no_dead_strip _div_pow2__div_pow2_L_3
_div_pow2__div_pow2_L_3:
LFB323:
	pushq	%rbp
LCFI1096:
	movq	%rsp, %rbp
LCFI1097:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI1098:
	ret
LFE323:
.no_dead_strip _div_pow2__div_pow2_L_4
_div_pow2__div_pow2_L_4:
LFB324:
	pushq	%rbp
LCFI1099:
	movq	%rsp, %rbp
LCFI1100:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI1101:
	ret
LFE324:
.no_dead_strip _div_pow2__div_pow2_L_5
_div_pow2__div_pow2_L_5:
LFB325:
	pushq	%rbp
LCFI1102:
	movq	%rsp, %rbp
LCFI1103:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI1104:
	ret
LFE325:
.no_dead_strip _div_pow2__div_pow2_L_6
_div_pow2__div_pow2_L_6:
LFB326:
	pushq	%rbp
LCFI1105:
	movq	%rsp, %rbp
LCFI1106:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI1107:
	ret
LFE326:
.no_dead_strip _div_pow2__div_pow2_L_7
_div_pow2__div_pow2_L_7:
LFB327:
	pushq	%rbp
LCFI1108:
	movq	%rsp, %rbp
LCFI1109:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI1110:
	ret
LFE327:
.no_dead_strip _div_pow2__div_pow2_L_8
_div_pow2__div_pow2_L_8:
LFB328:
	pushq	%rbp
LCFI1111:
	movq	%rsp, %rbp
LCFI1112:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI1113:
	ret
LFE328:
.no_dead_strip _div_pow2__div_pow2_L_9
_div_pow2__div_pow2_L_9:
LFB329:
	pushq	%rbp
LCFI1114:
	movq	%rsp, %rbp
LCFI1115:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI1116:
	ret
LFE329:
.no_dead_strip _div_pow2__div_pow2_L_10
_div_pow2__div_pow2_L_10:
LFB330:
	pushq	%rbp
LCFI1117:
	movq	%rsp, %rbp
LCFI1118:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI1119:
	ret
LFE330:
.no_dead_strip _div_pow2__div_pow2_L_11
_div_pow2__div_pow2_L_11:
LFB331:
	pushq	%rbp
LCFI1120:
	movq	%rsp, %rbp
LCFI1121:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI1122:
	ret
LFE331:
.no_dead_strip _div_pow2__div_pow2_L_12
_div_pow2__div_pow2_L_12:
LFB332:
	pushq	%rbp
LCFI1123:
	movq	%rsp, %rbp
LCFI1124:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI1125:
	ret
LFE332:
.no_dead_strip _div_pow2__div_pow2_L_13
_div_pow2__div_pow2_L_13:
LFB333:
	pushq	%rbp
LCFI1126:
	movq	%rsp, %rbp
LCFI1127:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI1128:
	ret
LFE333:
.no_dead_strip _div_pow2__div_pow2_L_14
_div_pow2__div_pow2_L_14:
LFB334:
	pushq	%rbp
LCFI1129:
	movq	%rsp, %rbp
LCFI1130:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI1131:
	ret
LFE334:
.no_dead_strip _div_pow2__div_pow2_L_15
_div_pow2__div_pow2_L_15:
LFB335:
	pushq	%rbp
LCFI1132:
	movq	%rsp, %rbp
LCFI1133:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI1134:
	ret
LFE335:
.no_dead_strip _div_pow2__div_pow2_L_16
_div_pow2__div_pow2_L_16:
LFB336:
	pushq	%rbp
LCFI1135:
	movq	%rsp, %rbp
LCFI1136:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI1137:
	ret
LFE336:
.no_dead_strip _div_pow2__div_pow2_L_17
_div_pow2__div_pow2_L_17:
LFB337:
	pushq	%rbp
LCFI1138:
	movq	%rsp, %rbp
LCFI1139:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI1140:
	ret
LFE337:
.no_dead_strip _div_pow2__div_pow2_L_18
_div_pow2__div_pow2_L_18:
LFB338:
	pushq	%rbp
LCFI1141:
	movq	%rsp, %rbp
LCFI1142:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI1143:
	ret
LFE338:
.no_dead_strip _div_pow2__div_pow2_L_19
_div_pow2__div_pow2_L_19:
LFB339:
	pushq	%rbp
LCFI1144:
	movq	%rsp, %rbp
LCFI1145:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI1146:
	ret
LFE339:
.no_dead_strip _div_pow2__div_pow2_L_20
_div_pow2__div_pow2_L_20:
LFB340:
	pushq	%rbp
LCFI1147:
	movq	%rsp, %rbp
LCFI1148:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI1149:
	ret
LFE340:
.no_dead_strip _div_pow2__div_pow2_L_21
_div_pow2__div_pow2_L_21:
LFB341:
	pushq	%rbp
LCFI1150:
	movq	%rsp, %rbp
LCFI1151:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI1152:
	ret
LFE341:
.no_dead_strip _div_pow2__div_pow2_L_22
_div_pow2__div_pow2_L_22:
LFB342:
	pushq	%rbp
LCFI1153:
	movq	%rsp, %rbp
LCFI1154:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI1155:
	ret
LFE342:
.no_dead_strip _div_pow2__div_pow2_L_23
_div_pow2__div_pow2_L_23:
LFB343:
	pushq	%rbp
LCFI1156:
	movq	%rsp, %rbp
LCFI1157:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI1158:
	ret
LFE343:
.no_dead_strip _div_pow2__div_pow2_L_24
_div_pow2__div_pow2_L_24:
LFB344:
	pushq	%rbp
LCFI1159:
	movq	%rsp, %rbp
LCFI1160:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI1161:
	ret
LFE344:
.no_dead_strip _div_pow2__div_pow2_L_25
_div_pow2__div_pow2_L_25:
LFB345:
	pushq	%rbp
LCFI1162:
	movq	%rsp, %rbp
LCFI1163:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI1164:
	ret
LFE345:
.no_dead_strip _div_pow2__div_pow2_L_26
_div_pow2__div_pow2_L_26:
LFB346:
	pushq	%rbp
LCFI1165:
	movq	%rsp, %rbp
LCFI1166:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI1167:
	ret
LFE346:
.no_dead_strip _div_pow2__div_pow2_L_27
_div_pow2__div_pow2_L_27:
LFB347:
	pushq	%rbp
LCFI1168:
	movq	%rsp, %rbp
LCFI1169:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI1170:
	ret
LFE347:
.no_dead_strip _div_pow2__div_pow2_L_28
_div_pow2__div_pow2_L_28:
LFB348:
	pushq	%rbp
LCFI1171:
	movq	%rsp, %rbp
LCFI1172:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI1173:
	ret
LFE348:
.no_dead_strip _div_pow2__div_pow2_L_29
_div_pow2__div_pow2_L_29:
LFB349:
	pushq	%rbp
LCFI1174:
	movq	%rsp, %rbp
LCFI1175:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI1176:
	ret
LFE349:
.no_dead_strip _div_pow2__div_pow2_L_30
_div_pow2__div_pow2_L_30:
LFB350:
	pushq	%rbp
LCFI1177:
	movq	%rsp, %rbp
LCFI1178:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI1179:
	ret
LFE350:
.no_dead_strip _div_pow2__div_pow2_L_31
_div_pow2__div_pow2_L_31:
LFB351:
	pushq	%rbp
LCFI1180:
	movq	%rsp, %rbp
LCFI1181:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI1182:
	ret
LFE351:
.no_dead_strip _div_pow2__div_pow2_L_32
_div_pow2__div_pow2_L_32:
LFB352:
	pushq	%rbp
LCFI1183:
	movq	%rsp, %rbp
LCFI1184:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI1185:
	ret
LFE352:
.no_dead_strip _div_pow2__div_pow2_L_33
_div_pow2__div_pow2_L_33:
LFB353:
	pushq	%rbp
LCFI1186:
	movq	%rsp, %rbp
LCFI1187:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI1188:
	ret
LFE353:
.no_dead_strip _div_pow2__div_pow2_L_34
_div_pow2__div_pow2_L_34:
LFB354:
	pushq	%rbp
LCFI1189:
	movq	%rsp, %rbp
LCFI1190:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI1191:
	ret
LFE354:
.no_dead_strip _div_pow2__div_pow2_L_35
_div_pow2__div_pow2_L_35:
LFB355:
	pushq	%rbp
LCFI1192:
	movq	%rsp, %rbp
LCFI1193:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI1194:
	ret
LFE355:
.no_dead_strip _div_pow2__div_pow2_L_36
_div_pow2__div_pow2_L_36:
LFB356:
	pushq	%rbp
LCFI1195:
	movq	%rsp, %rbp
LCFI1196:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI1197:
	ret
LFE356:
.no_dead_strip _div_pow2__div_pow2_L_37
_div_pow2__div_pow2_L_37:
LFB357:
	pushq	%rbp
LCFI1198:
	movq	%rsp, %rbp
LCFI1199:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI1200:
	ret
LFE357:
.no_dead_strip _div_pow2__div_pow2_L_38
_div_pow2__div_pow2_L_38:
LFB358:
	pushq	%rbp
LCFI1201:
	movq	%rsp, %rbp
LCFI1202:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI1203:
	ret
LFE358:
.no_dead_strip _div_pow2__div_pow2_L_39
_div_pow2__div_pow2_L_39:
LFB359:
	pushq	%rbp
LCFI1204:
	movq	%rsp, %rbp
LCFI1205:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI1206:
	ret
LFE359:
.no_dead_strip _div_pow2__div_pow2_L_40
_div_pow2__div_pow2_L_40:
LFB360:
	pushq	%rbp
LCFI1207:
	movq	%rsp, %rbp
LCFI1208:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI1209:
	ret
LFE360:
.no_dead_strip _div_pow2__div_pow2_L_41
_div_pow2__div_pow2_L_41:
LFB361:
	pushq	%rbp
LCFI1210:
	movq	%rsp, %rbp
LCFI1211:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI1212:
	ret
LFE361:
.no_dead_strip _div_pow2__div_pow2_L_42
_div_pow2__div_pow2_L_42:
LFB362:
	pushq	%rbp
LCFI1213:
	movq	%rsp, %rbp
LCFI1214:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI1215:
	ret
LFE362:
.no_dead_strip _div_pow2__div_pow2_L_43
_div_pow2__div_pow2_L_43:
LFB363:
	pushq	%rbp
LCFI1216:
	movq	%rsp, %rbp
LCFI1217:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI1218:
	ret
LFE363:
.no_dead_strip _div_pow2__div_pow2_L_44
_div_pow2__div_pow2_L_44:
LFB364:
	pushq	%rbp
LCFI1219:
	movq	%rsp, %rbp
LCFI1220:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI1221:
	ret
LFE364:
.no_dead_strip _div_pow2__div_pow2_L_45
_div_pow2__div_pow2_L_45:
LFB365:
	pushq	%rbp
LCFI1222:
	movq	%rsp, %rbp
LCFI1223:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI1224:
	ret
LFE365:
.no_dead_strip _div_pow2__div_pow2_L_46
_div_pow2__div_pow2_L_46:
LFB366:
	pushq	%rbp
LCFI1225:
	movq	%rsp, %rbp
LCFI1226:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI1227:
	ret
LFE366:
.no_dead_strip _div_pow2__div_pow2_L_47
_div_pow2__div_pow2_L_47:
LFB367:
	pushq	%rbp
LCFI1228:
	movq	%rsp, %rbp
LCFI1229:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI1230:
	ret
LFE367:
.no_dead_strip _div_pow2__div_pow2_L_48
_div_pow2__div_pow2_L_48:
LFB368:
	pushq	%rbp
LCFI1231:
	movq	%rsp, %rbp
LCFI1232:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI1233:
	ret
LFE368:
.no_dead_strip _div_pow2__div_pow2_L_49
_div_pow2__div_pow2_L_49:
LFB369:
	pushq	%rbp
LCFI1234:
	movq	%rsp, %rbp
LCFI1235:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI1236:
	ret
LFE369:
.no_dead_strip _div_pow2__div_pow2_L_50
_div_pow2__div_pow2_L_50:
LFB370:
	pushq	%rbp
LCFI1237:
	movq	%rsp, %rbp
LCFI1238:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI1239:
	ret
LFE370:
.no_dead_strip _div_pow2__div_pow2_L_51
_div_pow2__div_pow2_L_51:
LFB371:
	pushq	%rbp
LCFI1240:
	movq	%rsp, %rbp
LCFI1241:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI1242:
	ret
LFE371:
.no_dead_strip _div_pow2__div_pow2_L_52
_div_pow2__div_pow2_L_52:
LFB372:
	pushq	%rbp
LCFI1243:
	movq	%rsp, %rbp
LCFI1244:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI1245:
	ret
LFE372:
.no_dead_strip _div_pow2__div_pow2_L_53
_div_pow2__div_pow2_L_53:
LFB373:
	pushq	%rbp
LCFI1246:
	movq	%rsp, %rbp
LCFI1247:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI1248:
	ret
LFE373:
.no_dead_strip _div_pow2__div_pow2_L_54
_div_pow2__div_pow2_L_54:
LFB374:
	pushq	%rbp
LCFI1249:
	movq	%rsp, %rbp
LCFI1250:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI1251:
	ret
LFE374:
.no_dead_strip _div_pow2__div_pow2_L_55
_div_pow2__div_pow2_L_55:
LFB375:
	pushq	%rbp
LCFI1252:
	movq	%rsp, %rbp
LCFI1253:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI1254:
	ret
LFE375:
.no_dead_strip _div_pow2__div_pow2_L_56
_div_pow2__div_pow2_L_56:
LFB376:
	pushq	%rbp
LCFI1255:
	movq	%rsp, %rbp
LCFI1256:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI1257:
	ret
LFE376:
.no_dead_strip _div_pow2__div_pow2_L_57
_div_pow2__div_pow2_L_57:
LFB377:
	pushq	%rbp
LCFI1258:
	movq	%rsp, %rbp
LCFI1259:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI1260:
	ret
LFE377:
.no_dead_strip _div_pow2__div_pow2_L_58
_div_pow2__div_pow2_L_58:
LFB378:
	pushq	%rbp
LCFI1261:
	movq	%rsp, %rbp
LCFI1262:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI1263:
	ret
LFE378:
.no_dead_strip _div_pow2__div_pow2_L_59
_div_pow2__div_pow2_L_59:
LFB379:
	pushq	%rbp
LCFI1264:
	movq	%rsp, %rbp
LCFI1265:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI1266:
	ret
LFE379:
.no_dead_strip _div_pow2__div_pow2_L_60
_div_pow2__div_pow2_L_60:
LFB380:
	pushq	%rbp
LCFI1267:
	movq	%rsp, %rbp
LCFI1268:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI1269:
	ret
LFE380:
.no_dead_strip _div_pow2__div_pow2_L_61
_div_pow2__div_pow2_L_61:
LFB381:
	pushq	%rbp
LCFI1270:
	movq	%rsp, %rbp
LCFI1271:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI1272:
	ret
LFE381:
.no_dead_strip _div_pow2__div_pow2_L_62
_div_pow2__div_pow2_L_62:
LFB382:
	pushq	%rbp
LCFI1273:
	movq	%rsp, %rbp
LCFI1274:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI1275:
	ret
LFE382:
.no_dead_strip _div_pow2__div_pow2_L_63
_div_pow2__div_pow2_L_63:
LFB383:
	pushq	%rbp
LCFI1276:
	movq	%rsp, %rbp
LCFI1277:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI1278:
	ret
LFE383:
.no_dead_strip _div_pow2__div_pow2_LC_0
_div_pow2__div_pow2_LC_0:
LFB384:
	pushq	%rbp
LCFI1279:
	movq	%rsp, %rbp
LCFI1280:
	subq	$16, %rsp
LCFI1281:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	testq	%rax, %rax
	jns	L575
	movl	$26337, %edi
	call	__m3_fault
L575:
	movq	-8(%rbp), %rax
	leave
LCFI1282:
	ret
LFE384:
.no_dead_strip _div_pow2__div_pow2_LC_1
_div_pow2__div_pow2_LC_1:
LFB385:
	pushq	%rbp
LCFI1283:
	movq	%rsp, %rbp
LCFI1284:
	subq	$16, %rsp
LCFI1285:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	%rax
	testq	%rax, %rax
	jns	L577
	movl	$26401, %edi
	call	__m3_fault
L577:
	movq	-8(%rbp), %rax
	sarq	%rax
	leave
LCFI1286:
	ret
LFE385:
.no_dead_strip _div_pow2__div_pow2_LC_2
_div_pow2__div_pow2_LC_2:
LFB386:
	pushq	%rbp
LCFI1287:
	movq	%rsp, %rbp
LCFI1288:
	subq	$16, %rsp
LCFI1289:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	testq	%rax, %rax
	jns	L579
	movl	$26465, %edi
	call	__m3_fault
L579:
	movq	-8(%rbp), %rax
	sarq	$2, %rax
	leave
LCFI1290:
	ret
LFE386:
.no_dead_strip _div_pow2__div_pow2_LC_3
_div_pow2__div_pow2_LC_3:
LFB387:
	pushq	%rbp
LCFI1291:
	movq	%rsp, %rbp
LCFI1292:
	subq	$16, %rsp
LCFI1293:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	testq	%rax, %rax
	jns	L581
	movl	$26529, %edi
	call	__m3_fault
L581:
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	leave
LCFI1294:
	ret
LFE387:
.no_dead_strip _div_pow2__div_pow2_LC_4
_div_pow2__div_pow2_LC_4:
LFB388:
	pushq	%rbp
LCFI1295:
	movq	%rsp, %rbp
LCFI1296:
	subq	$16, %rsp
LCFI1297:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	testq	%rax, %rax
	jns	L583
	movl	$26593, %edi
	call	__m3_fault
L583:
	movq	-8(%rbp), %rax
	sarq	$4, %rax
	leave
LCFI1298:
	ret
LFE388:
.no_dead_strip _div_pow2__div_pow2_LC_5
_div_pow2__div_pow2_LC_5:
LFB389:
	pushq	%rbp
LCFI1299:
	movq	%rsp, %rbp
LCFI1300:
	subq	$16, %rsp
LCFI1301:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	testq	%rax, %rax
	jns	L585
	movl	$26657, %edi
	call	__m3_fault
L585:
	movq	-8(%rbp), %rax
	sarq	$5, %rax
	leave
LCFI1302:
	ret
LFE389:
.no_dead_strip _div_pow2__div_pow2_LC_6
_div_pow2__div_pow2_LC_6:
LFB390:
	pushq	%rbp
LCFI1303:
	movq	%rsp, %rbp
LCFI1304:
	subq	$16, %rsp
LCFI1305:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	testq	%rax, %rax
	jns	L587
	movl	$26721, %edi
	call	__m3_fault
L587:
	movq	-8(%rbp), %rax
	sarq	$6, %rax
	leave
LCFI1306:
	ret
LFE390:
.no_dead_strip _div_pow2__div_pow2_LC_7
_div_pow2__div_pow2_LC_7:
LFB391:
	pushq	%rbp
LCFI1307:
	movq	%rsp, %rbp
LCFI1308:
	subq	$16, %rsp
LCFI1309:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	testq	%rax, %rax
	jns	L589
	movl	$26785, %edi
	call	__m3_fault
L589:
	movq	-8(%rbp), %rax
	sarq	$7, %rax
	leave
LCFI1310:
	ret
LFE391:
.no_dead_strip _div_pow2__div_pow2_LC_8
_div_pow2__div_pow2_LC_8:
LFB392:
	pushq	%rbp
LCFI1311:
	movq	%rsp, %rbp
LCFI1312:
	subq	$16, %rsp
LCFI1313:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	testq	%rax, %rax
	jns	L591
	movl	$26849, %edi
	call	__m3_fault
L591:
	movq	-8(%rbp), %rax
	sarq	$8, %rax
	leave
LCFI1314:
	ret
LFE392:
.no_dead_strip _div_pow2__div_pow2_LC_9
_div_pow2__div_pow2_LC_9:
LFB393:
	pushq	%rbp
LCFI1315:
	movq	%rsp, %rbp
LCFI1316:
	subq	$16, %rsp
LCFI1317:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	testq	%rax, %rax
	jns	L593
	movl	$26913, %edi
	call	__m3_fault
L593:
	movq	-8(%rbp), %rax
	sarq	$9, %rax
	leave
LCFI1318:
	ret
LFE393:
.no_dead_strip _div_pow2__div_pow2_LC_10
_div_pow2__div_pow2_LC_10:
LFB394:
	pushq	%rbp
LCFI1319:
	movq	%rsp, %rbp
LCFI1320:
	subq	$16, %rsp
LCFI1321:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	testq	%rax, %rax
	jns	L595
	movl	$26977, %edi
	call	__m3_fault
L595:
	movq	-8(%rbp), %rax
	sarq	$10, %rax
	leave
LCFI1322:
	ret
LFE394:
.no_dead_strip _div_pow2__div_pow2_LC_11
_div_pow2__div_pow2_LC_11:
LFB395:
	pushq	%rbp
LCFI1323:
	movq	%rsp, %rbp
LCFI1324:
	subq	$16, %rsp
LCFI1325:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	testq	%rax, %rax
	jns	L597
	movl	$27041, %edi
	call	__m3_fault
L597:
	movq	-8(%rbp), %rax
	sarq	$11, %rax
	leave
LCFI1326:
	ret
LFE395:
.no_dead_strip _div_pow2__div_pow2_LC_12
_div_pow2__div_pow2_LC_12:
LFB396:
	pushq	%rbp
LCFI1327:
	movq	%rsp, %rbp
LCFI1328:
	subq	$16, %rsp
LCFI1329:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	testq	%rax, %rax
	jns	L599
	movl	$27105, %edi
	call	__m3_fault
L599:
	movq	-8(%rbp), %rax
	sarq	$12, %rax
	leave
LCFI1330:
	ret
LFE396:
.no_dead_strip _div_pow2__div_pow2_LC_13
_div_pow2__div_pow2_LC_13:
LFB397:
	pushq	%rbp
LCFI1331:
	movq	%rsp, %rbp
LCFI1332:
	subq	$16, %rsp
LCFI1333:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	testq	%rax, %rax
	jns	L601
	movl	$27169, %edi
	call	__m3_fault
L601:
	movq	-8(%rbp), %rax
	sarq	$13, %rax
	leave
LCFI1334:
	ret
LFE397:
.no_dead_strip _div_pow2__div_pow2_LC_14
_div_pow2__div_pow2_LC_14:
LFB398:
	pushq	%rbp
LCFI1335:
	movq	%rsp, %rbp
LCFI1336:
	subq	$16, %rsp
LCFI1337:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	testq	%rax, %rax
	jns	L603
	movl	$27233, %edi
	call	__m3_fault
L603:
	movq	-8(%rbp), %rax
	sarq	$14, %rax
	leave
LCFI1338:
	ret
LFE398:
.no_dead_strip _div_pow2__div_pow2_LC_15
_div_pow2__div_pow2_LC_15:
LFB399:
	pushq	%rbp
LCFI1339:
	movq	%rsp, %rbp
LCFI1340:
	subq	$16, %rsp
LCFI1341:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	testq	%rax, %rax
	jns	L605
	movl	$27297, %edi
	call	__m3_fault
L605:
	movq	-8(%rbp), %rax
	sarq	$15, %rax
	leave
LCFI1342:
	ret
LFE399:
.no_dead_strip _div_pow2__div_pow2_LC_16
_div_pow2__div_pow2_LC_16:
LFB400:
	pushq	%rbp
LCFI1343:
	movq	%rsp, %rbp
LCFI1344:
	subq	$16, %rsp
LCFI1345:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	testq	%rax, %rax
	jns	L607
	movl	$27361, %edi
	call	__m3_fault
L607:
	movq	-8(%rbp), %rax
	sarq	$16, %rax
	leave
LCFI1346:
	ret
LFE400:
.no_dead_strip _div_pow2__div_pow2_LC_17
_div_pow2__div_pow2_LC_17:
LFB401:
	pushq	%rbp
LCFI1347:
	movq	%rsp, %rbp
LCFI1348:
	subq	$16, %rsp
LCFI1349:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	testq	%rax, %rax
	jns	L609
	movl	$27425, %edi
	call	__m3_fault
L609:
	movq	-8(%rbp), %rax
	sarq	$17, %rax
	leave
LCFI1350:
	ret
LFE401:
.no_dead_strip _div_pow2__div_pow2_LC_18
_div_pow2__div_pow2_LC_18:
LFB402:
	pushq	%rbp
LCFI1351:
	movq	%rsp, %rbp
LCFI1352:
	subq	$16, %rsp
LCFI1353:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	testq	%rax, %rax
	jns	L611
	movl	$27489, %edi
	call	__m3_fault
L611:
	movq	-8(%rbp), %rax
	sarq	$18, %rax
	leave
LCFI1354:
	ret
LFE402:
.no_dead_strip _div_pow2__div_pow2_LC_19
_div_pow2__div_pow2_LC_19:
LFB403:
	pushq	%rbp
LCFI1355:
	movq	%rsp, %rbp
LCFI1356:
	subq	$16, %rsp
LCFI1357:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	testq	%rax, %rax
	jns	L613
	movl	$27553, %edi
	call	__m3_fault
L613:
	movq	-8(%rbp), %rax
	sarq	$19, %rax
	leave
LCFI1358:
	ret
LFE403:
.no_dead_strip _div_pow2__div_pow2_LC_20
_div_pow2__div_pow2_LC_20:
LFB404:
	pushq	%rbp
LCFI1359:
	movq	%rsp, %rbp
LCFI1360:
	subq	$16, %rsp
LCFI1361:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	testq	%rax, %rax
	jns	L615
	movl	$27617, %edi
	call	__m3_fault
L615:
	movq	-8(%rbp), %rax
	sarq	$20, %rax
	leave
LCFI1362:
	ret
LFE404:
.no_dead_strip _div_pow2__div_pow2_LC_21
_div_pow2__div_pow2_LC_21:
LFB405:
	pushq	%rbp
LCFI1363:
	movq	%rsp, %rbp
LCFI1364:
	subq	$16, %rsp
LCFI1365:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	testq	%rax, %rax
	jns	L617
	movl	$27681, %edi
	call	__m3_fault
L617:
	movq	-8(%rbp), %rax
	sarq	$21, %rax
	leave
LCFI1366:
	ret
LFE405:
.no_dead_strip _div_pow2__div_pow2_LC_22
_div_pow2__div_pow2_LC_22:
LFB406:
	pushq	%rbp
LCFI1367:
	movq	%rsp, %rbp
LCFI1368:
	subq	$16, %rsp
LCFI1369:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	testq	%rax, %rax
	jns	L619
	movl	$27745, %edi
	call	__m3_fault
L619:
	movq	-8(%rbp), %rax
	sarq	$22, %rax
	leave
LCFI1370:
	ret
LFE406:
.no_dead_strip _div_pow2__div_pow2_LC_23
_div_pow2__div_pow2_LC_23:
LFB407:
	pushq	%rbp
LCFI1371:
	movq	%rsp, %rbp
LCFI1372:
	subq	$16, %rsp
LCFI1373:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	testq	%rax, %rax
	jns	L621
	movl	$27809, %edi
	call	__m3_fault
L621:
	movq	-8(%rbp), %rax
	sarq	$23, %rax
	leave
LCFI1374:
	ret
LFE407:
.no_dead_strip _div_pow2__div_pow2_LC_24
_div_pow2__div_pow2_LC_24:
LFB408:
	pushq	%rbp
LCFI1375:
	movq	%rsp, %rbp
LCFI1376:
	subq	$16, %rsp
LCFI1377:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	testq	%rax, %rax
	jns	L623
	movl	$27873, %edi
	call	__m3_fault
L623:
	movq	-8(%rbp), %rax
	sarq	$24, %rax
	leave
LCFI1378:
	ret
LFE408:
.no_dead_strip _div_pow2__div_pow2_LC_25
_div_pow2__div_pow2_LC_25:
LFB409:
	pushq	%rbp
LCFI1379:
	movq	%rsp, %rbp
LCFI1380:
	subq	$16, %rsp
LCFI1381:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	testq	%rax, %rax
	jns	L625
	movl	$27937, %edi
	call	__m3_fault
L625:
	movq	-8(%rbp), %rax
	sarq	$25, %rax
	leave
LCFI1382:
	ret
LFE409:
.no_dead_strip _div_pow2__div_pow2_LC_26
_div_pow2__div_pow2_LC_26:
LFB410:
	pushq	%rbp
LCFI1383:
	movq	%rsp, %rbp
LCFI1384:
	subq	$16, %rsp
LCFI1385:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	testq	%rax, %rax
	jns	L627
	movl	$28001, %edi
	call	__m3_fault
L627:
	movq	-8(%rbp), %rax
	sarq	$26, %rax
	leave
LCFI1386:
	ret
LFE410:
.no_dead_strip _div_pow2__div_pow2_LC_27
_div_pow2__div_pow2_LC_27:
LFB411:
	pushq	%rbp
LCFI1387:
	movq	%rsp, %rbp
LCFI1388:
	subq	$16, %rsp
LCFI1389:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	testq	%rax, %rax
	jns	L629
	movl	$28065, %edi
	call	__m3_fault
L629:
	movq	-8(%rbp), %rax
	sarq	$27, %rax
	leave
LCFI1390:
	ret
LFE411:
.no_dead_strip _div_pow2__div_pow2_LC_28
_div_pow2__div_pow2_LC_28:
LFB412:
	pushq	%rbp
LCFI1391:
	movq	%rsp, %rbp
LCFI1392:
	subq	$16, %rsp
LCFI1393:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	testq	%rax, %rax
	jns	L631
	movl	$28129, %edi
	call	__m3_fault
L631:
	movq	-8(%rbp), %rax
	sarq	$28, %rax
	leave
LCFI1394:
	ret
LFE412:
.no_dead_strip _div_pow2__div_pow2_LC_29
_div_pow2__div_pow2_LC_29:
LFB413:
	pushq	%rbp
LCFI1395:
	movq	%rsp, %rbp
LCFI1396:
	subq	$16, %rsp
LCFI1397:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	testq	%rax, %rax
	jns	L633
	movl	$28193, %edi
	call	__m3_fault
L633:
	movq	-8(%rbp), %rax
	sarq	$29, %rax
	leave
LCFI1398:
	ret
LFE413:
.no_dead_strip _div_pow2__div_pow2_LC_30
_div_pow2__div_pow2_LC_30:
LFB414:
	pushq	%rbp
LCFI1399:
	movq	%rsp, %rbp
LCFI1400:
	subq	$16, %rsp
LCFI1401:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	testq	%rax, %rax
	jns	L635
	movl	$28257, %edi
	call	__m3_fault
L635:
	movq	-8(%rbp), %rax
	sarq	$30, %rax
	leave
LCFI1402:
	ret
LFE414:
.no_dead_strip _div_pow2__div_pow2_LC_31
_div_pow2__div_pow2_LC_31:
LFB415:
	pushq	%rbp
LCFI1403:
	movq	%rsp, %rbp
LCFI1404:
	subq	$16, %rsp
LCFI1405:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	testq	%rax, %rax
	jns	L637
	movl	$28321, %edi
	call	__m3_fault
L637:
	movq	-8(%rbp), %rax
	sarq	$31, %rax
	leave
LCFI1406:
	ret
LFE415:
.no_dead_strip _div_pow2__div_pow2_LC_32
_div_pow2__div_pow2_LC_32:
LFB416:
	pushq	%rbp
LCFI1407:
	movq	%rsp, %rbp
LCFI1408:
	subq	$16, %rsp
LCFI1409:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	testq	%rax, %rax
	jns	L639
	movl	$28385, %edi
	call	__m3_fault
L639:
	movq	-8(%rbp), %rax
	sarq	$32, %rax
	leave
LCFI1410:
	ret
LFE416:
.no_dead_strip _div_pow2__div_pow2_LC_33
_div_pow2__div_pow2_LC_33:
LFB417:
	pushq	%rbp
LCFI1411:
	movq	%rsp, %rbp
LCFI1412:
	subq	$16, %rsp
LCFI1413:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	testq	%rax, %rax
	jns	L641
	movl	$28449, %edi
	call	__m3_fault
L641:
	movq	-8(%rbp), %rax
	sarq	$33, %rax
	leave
LCFI1414:
	ret
LFE417:
.no_dead_strip _div_pow2__div_pow2_LC_34
_div_pow2__div_pow2_LC_34:
LFB418:
	pushq	%rbp
LCFI1415:
	movq	%rsp, %rbp
LCFI1416:
	subq	$16, %rsp
LCFI1417:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	testq	%rax, %rax
	jns	L643
	movl	$28513, %edi
	call	__m3_fault
L643:
	movq	-8(%rbp), %rax
	sarq	$34, %rax
	leave
LCFI1418:
	ret
LFE418:
.no_dead_strip _div_pow2__div_pow2_LC_35
_div_pow2__div_pow2_LC_35:
LFB419:
	pushq	%rbp
LCFI1419:
	movq	%rsp, %rbp
LCFI1420:
	subq	$16, %rsp
LCFI1421:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	testq	%rax, %rax
	jns	L645
	movl	$28577, %edi
	call	__m3_fault
L645:
	movq	-8(%rbp), %rax
	sarq	$35, %rax
	leave
LCFI1422:
	ret
LFE419:
.no_dead_strip _div_pow2__div_pow2_LC_36
_div_pow2__div_pow2_LC_36:
LFB420:
	pushq	%rbp
LCFI1423:
	movq	%rsp, %rbp
LCFI1424:
	subq	$16, %rsp
LCFI1425:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	testq	%rax, %rax
	jns	L647
	movl	$28641, %edi
	call	__m3_fault
L647:
	movq	-8(%rbp), %rax
	sarq	$36, %rax
	leave
LCFI1426:
	ret
LFE420:
.no_dead_strip _div_pow2__div_pow2_LC_37
_div_pow2__div_pow2_LC_37:
LFB421:
	pushq	%rbp
LCFI1427:
	movq	%rsp, %rbp
LCFI1428:
	subq	$16, %rsp
LCFI1429:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	testq	%rax, %rax
	jns	L649
	movl	$28705, %edi
	call	__m3_fault
L649:
	movq	-8(%rbp), %rax
	sarq	$37, %rax
	leave
LCFI1430:
	ret
LFE421:
.no_dead_strip _div_pow2__div_pow2_LC_38
_div_pow2__div_pow2_LC_38:
LFB422:
	pushq	%rbp
LCFI1431:
	movq	%rsp, %rbp
LCFI1432:
	subq	$16, %rsp
LCFI1433:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	testq	%rax, %rax
	jns	L651
	movl	$28769, %edi
	call	__m3_fault
L651:
	movq	-8(%rbp), %rax
	sarq	$38, %rax
	leave
LCFI1434:
	ret
LFE422:
.no_dead_strip _div_pow2__div_pow2_LC_39
_div_pow2__div_pow2_LC_39:
LFB423:
	pushq	%rbp
LCFI1435:
	movq	%rsp, %rbp
LCFI1436:
	subq	$16, %rsp
LCFI1437:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	testq	%rax, %rax
	jns	L653
	movl	$28833, %edi
	call	__m3_fault
L653:
	movq	-8(%rbp), %rax
	sarq	$39, %rax
	leave
LCFI1438:
	ret
LFE423:
.no_dead_strip _div_pow2__div_pow2_LC_40
_div_pow2__div_pow2_LC_40:
LFB424:
	pushq	%rbp
LCFI1439:
	movq	%rsp, %rbp
LCFI1440:
	subq	$16, %rsp
LCFI1441:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	testq	%rax, %rax
	jns	L655
	movl	$28897, %edi
	call	__m3_fault
L655:
	movq	-8(%rbp), %rax
	sarq	$40, %rax
	leave
LCFI1442:
	ret
LFE424:
.no_dead_strip _div_pow2__div_pow2_LC_41
_div_pow2__div_pow2_LC_41:
LFB425:
	pushq	%rbp
LCFI1443:
	movq	%rsp, %rbp
LCFI1444:
	subq	$16, %rsp
LCFI1445:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	testq	%rax, %rax
	jns	L657
	movl	$28961, %edi
	call	__m3_fault
L657:
	movq	-8(%rbp), %rax
	sarq	$41, %rax
	leave
LCFI1446:
	ret
LFE425:
.no_dead_strip _div_pow2__div_pow2_LC_42
_div_pow2__div_pow2_LC_42:
LFB426:
	pushq	%rbp
LCFI1447:
	movq	%rsp, %rbp
LCFI1448:
	subq	$16, %rsp
LCFI1449:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	testq	%rax, %rax
	jns	L659
	movl	$29025, %edi
	call	__m3_fault
L659:
	movq	-8(%rbp), %rax
	sarq	$42, %rax
	leave
LCFI1450:
	ret
LFE426:
.no_dead_strip _div_pow2__div_pow2_LC_43
_div_pow2__div_pow2_LC_43:
LFB427:
	pushq	%rbp
LCFI1451:
	movq	%rsp, %rbp
LCFI1452:
	subq	$16, %rsp
LCFI1453:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	testq	%rax, %rax
	jns	L661
	movl	$29089, %edi
	call	__m3_fault
L661:
	movq	-8(%rbp), %rax
	sarq	$43, %rax
	leave
LCFI1454:
	ret
LFE427:
.no_dead_strip _div_pow2__div_pow2_LC_44
_div_pow2__div_pow2_LC_44:
LFB428:
	pushq	%rbp
LCFI1455:
	movq	%rsp, %rbp
LCFI1456:
	subq	$16, %rsp
LCFI1457:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	testq	%rax, %rax
	jns	L663
	movl	$29153, %edi
	call	__m3_fault
L663:
	movq	-8(%rbp), %rax
	sarq	$44, %rax
	leave
LCFI1458:
	ret
LFE428:
.no_dead_strip _div_pow2__div_pow2_LC_45
_div_pow2__div_pow2_LC_45:
LFB429:
	pushq	%rbp
LCFI1459:
	movq	%rsp, %rbp
LCFI1460:
	subq	$16, %rsp
LCFI1461:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	testq	%rax, %rax
	jns	L665
	movl	$29217, %edi
	call	__m3_fault
L665:
	movq	-8(%rbp), %rax
	sarq	$45, %rax
	leave
LCFI1462:
	ret
LFE429:
.no_dead_strip _div_pow2__div_pow2_LC_46
_div_pow2__div_pow2_LC_46:
LFB430:
	pushq	%rbp
LCFI1463:
	movq	%rsp, %rbp
LCFI1464:
	subq	$16, %rsp
LCFI1465:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	testq	%rax, %rax
	jns	L667
	movl	$29281, %edi
	call	__m3_fault
L667:
	movq	-8(%rbp), %rax
	sarq	$46, %rax
	leave
LCFI1466:
	ret
LFE430:
.no_dead_strip _div_pow2__div_pow2_LC_47
_div_pow2__div_pow2_LC_47:
LFB431:
	pushq	%rbp
LCFI1467:
	movq	%rsp, %rbp
LCFI1468:
	subq	$16, %rsp
LCFI1469:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	testq	%rax, %rax
	jns	L669
	movl	$29345, %edi
	call	__m3_fault
L669:
	movq	-8(%rbp), %rax
	sarq	$47, %rax
	leave
LCFI1470:
	ret
LFE431:
.no_dead_strip _div_pow2__div_pow2_LC_48
_div_pow2__div_pow2_LC_48:
LFB432:
	pushq	%rbp
LCFI1471:
	movq	%rsp, %rbp
LCFI1472:
	subq	$16, %rsp
LCFI1473:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	testq	%rax, %rax
	jns	L671
	movl	$29409, %edi
	call	__m3_fault
L671:
	movq	-8(%rbp), %rax
	sarq	$48, %rax
	leave
LCFI1474:
	ret
LFE432:
.no_dead_strip _div_pow2__div_pow2_LC_49
_div_pow2__div_pow2_LC_49:
LFB433:
	pushq	%rbp
LCFI1475:
	movq	%rsp, %rbp
LCFI1476:
	subq	$16, %rsp
LCFI1477:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	testq	%rax, %rax
	jns	L673
	movl	$29473, %edi
	call	__m3_fault
L673:
	movq	-8(%rbp), %rax
	sarq	$49, %rax
	leave
LCFI1478:
	ret
LFE433:
.no_dead_strip _div_pow2__div_pow2_LC_50
_div_pow2__div_pow2_LC_50:
LFB434:
	pushq	%rbp
LCFI1479:
	movq	%rsp, %rbp
LCFI1480:
	subq	$16, %rsp
LCFI1481:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	testq	%rax, %rax
	jns	L675
	movl	$29537, %edi
	call	__m3_fault
L675:
	movq	-8(%rbp), %rax
	sarq	$50, %rax
	leave
LCFI1482:
	ret
LFE434:
.no_dead_strip _div_pow2__div_pow2_LC_51
_div_pow2__div_pow2_LC_51:
LFB435:
	pushq	%rbp
LCFI1483:
	movq	%rsp, %rbp
LCFI1484:
	subq	$16, %rsp
LCFI1485:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	testq	%rax, %rax
	jns	L677
	movl	$29601, %edi
	call	__m3_fault
L677:
	movq	-8(%rbp), %rax
	sarq	$51, %rax
	leave
LCFI1486:
	ret
LFE435:
.no_dead_strip _div_pow2__div_pow2_LC_52
_div_pow2__div_pow2_LC_52:
LFB436:
	pushq	%rbp
LCFI1487:
	movq	%rsp, %rbp
LCFI1488:
	subq	$16, %rsp
LCFI1489:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	testq	%rax, %rax
	jns	L679
	movl	$29665, %edi
	call	__m3_fault
L679:
	movq	-8(%rbp), %rax
	sarq	$52, %rax
	leave
LCFI1490:
	ret
LFE436:
.no_dead_strip _div_pow2__div_pow2_LC_53
_div_pow2__div_pow2_LC_53:
LFB437:
	pushq	%rbp
LCFI1491:
	movq	%rsp, %rbp
LCFI1492:
	subq	$16, %rsp
LCFI1493:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	testq	%rax, %rax
	jns	L681
	movl	$29729, %edi
	call	__m3_fault
L681:
	movq	-8(%rbp), %rax
	sarq	$53, %rax
	leave
LCFI1494:
	ret
LFE437:
.no_dead_strip _div_pow2__div_pow2_LC_54
_div_pow2__div_pow2_LC_54:
LFB438:
	pushq	%rbp
LCFI1495:
	movq	%rsp, %rbp
LCFI1496:
	subq	$16, %rsp
LCFI1497:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	testq	%rax, %rax
	jns	L683
	movl	$29793, %edi
	call	__m3_fault
L683:
	movq	-8(%rbp), %rax
	sarq	$54, %rax
	leave
LCFI1498:
	ret
LFE438:
.no_dead_strip _div_pow2__div_pow2_LC_55
_div_pow2__div_pow2_LC_55:
LFB439:
	pushq	%rbp
LCFI1499:
	movq	%rsp, %rbp
LCFI1500:
	subq	$16, %rsp
LCFI1501:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	testq	%rax, %rax
	jns	L685
	movl	$29857, %edi
	call	__m3_fault
L685:
	movq	-8(%rbp), %rax
	sarq	$55, %rax
	leave
LCFI1502:
	ret
LFE439:
.no_dead_strip _div_pow2__div_pow2_LC_56
_div_pow2__div_pow2_LC_56:
LFB440:
	pushq	%rbp
LCFI1503:
	movq	%rsp, %rbp
LCFI1504:
	subq	$16, %rsp
LCFI1505:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	testq	%rax, %rax
	jns	L687
	movl	$29921, %edi
	call	__m3_fault
L687:
	movq	-8(%rbp), %rax
	sarq	$56, %rax
	leave
LCFI1506:
	ret
LFE440:
.no_dead_strip _div_pow2__div_pow2_LC_57
_div_pow2__div_pow2_LC_57:
LFB441:
	pushq	%rbp
LCFI1507:
	movq	%rsp, %rbp
LCFI1508:
	subq	$16, %rsp
LCFI1509:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	testq	%rax, %rax
	jns	L689
	movl	$29985, %edi
	call	__m3_fault
L689:
	movq	-8(%rbp), %rax
	sarq	$57, %rax
	leave
LCFI1510:
	ret
LFE441:
.no_dead_strip _div_pow2__div_pow2_LC_58
_div_pow2__div_pow2_LC_58:
LFB442:
	pushq	%rbp
LCFI1511:
	movq	%rsp, %rbp
LCFI1512:
	subq	$16, %rsp
LCFI1513:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	testq	%rax, %rax
	jns	L691
	movl	$30049, %edi
	call	__m3_fault
L691:
	movq	-8(%rbp), %rax
	sarq	$58, %rax
	leave
LCFI1514:
	ret
LFE442:
.no_dead_strip _div_pow2__div_pow2_LC_59
_div_pow2__div_pow2_LC_59:
LFB443:
	pushq	%rbp
LCFI1515:
	movq	%rsp, %rbp
LCFI1516:
	subq	$16, %rsp
LCFI1517:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	testq	%rax, %rax
	jns	L693
	movl	$30113, %edi
	call	__m3_fault
L693:
	movq	-8(%rbp), %rax
	sarq	$59, %rax
	leave
LCFI1518:
	ret
LFE443:
.no_dead_strip _div_pow2__div_pow2_LC_60
_div_pow2__div_pow2_LC_60:
LFB444:
	pushq	%rbp
LCFI1519:
	movq	%rsp, %rbp
LCFI1520:
	subq	$16, %rsp
LCFI1521:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	testq	%rax, %rax
	jns	L695
	movl	$30177, %edi
	call	__m3_fault
L695:
	movq	-8(%rbp), %rax
	sarq	$60, %rax
	leave
LCFI1522:
	ret
LFE444:
.no_dead_strip _div_pow2__div_pow2_LC_61
_div_pow2__div_pow2_LC_61:
LFB445:
	pushq	%rbp
LCFI1523:
	movq	%rsp, %rbp
LCFI1524:
	subq	$16, %rsp
LCFI1525:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	testq	%rax, %rax
	jns	L697
	movl	$30241, %edi
	call	__m3_fault
L697:
	movq	-8(%rbp), %rax
	sarq	$61, %rax
	leave
LCFI1526:
	ret
LFE445:
.no_dead_strip _div_pow2__div_pow2_LC_62
_div_pow2__div_pow2_LC_62:
LFB446:
	pushq	%rbp
LCFI1527:
	movq	%rsp, %rbp
LCFI1528:
	subq	$16, %rsp
LCFI1529:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	testq	%rax, %rax
	jns	L699
	movl	$30305, %edi
	call	__m3_fault
L699:
	movq	-8(%rbp), %rax
	sarq	$62, %rax
	leave
LCFI1530:
	ret
LFE446:
.no_dead_strip _div_pow2__div_pow2_LC_63
_div_pow2__div_pow2_LC_63:
LFB447:
	pushq	%rbp
LCFI1531:
	movq	%rsp, %rbp
LCFI1532:
	subq	$16, %rsp
LCFI1533:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	testq	%rax, %rax
	jns	L701
	movl	$30369, %edi
	call	__m3_fault
L701:
	movq	-8(%rbp), %rax
	leaq	-1(%rax), %rdx
	orq	%rdx, %rax
	sarq	$63, %rax
	movq	%rdx, %rcx
	subq	%rax, %rcx
	movabsq	$-9223372036854775808, %rdx
	cmpq	%rdx, %rcx
	sete	%dl
	movzbl	%dl, %edx
	notq	%rax
	leaq	(%rdx,%rax), %rax
	leave
LCFI1534:
	ret
LFE447:
.globl _div_pow2_M3
.no_dead_strip _div_pow2_M3
_div_pow2_M3:
LFB448:
	pushq	%rbp
LCFI1535:
	movq	%rsp, %rbp
LCFI1536:
	movq	%rdi, -8(%rbp)
	leaq	_MM_div_pow2(%rip), %rax
	leave
LCFI1537:
	ret
LFE448:
__m3_fault:
LFB449:
	pushq	%rbp
LCFI1538:
	movq	%rsp, %rbp
LCFI1539:
	subq	$16, %rsp
LCFI1540:
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rsi
	leaq	_MM_div_pow2(%rip), %rdi
	call	_RTHooks__ReportFault
	leave
LCFI1541:
	ret
LFE449:
	.const_data
	.align 5
_L_1:
	.ascii "div_pow2_M3"
	.space 1
	.ascii "div_pow2_LC_63"
	.space 1
	.ascii "div_pow2_LC_62"
	.space 1
	.ascii "div_pow2_LC_61"
	.space 1
	.ascii "div_pow2_LC_60"
	.space 1
	.ascii "div_pow2_LC_59"
	.space 1
	.ascii "div_pow2_LC_58"
	.space 1
	.ascii "div_pow2_LC_57"
	.space 1
	.ascii "div_pow2_LC_56"
	.space 1
	.ascii "div_pow2_LC_55"
	.space 1
	.ascii "div_pow2_LC_54"
	.space 1
	.ascii "div_pow2_LC_53"
	.space 1
	.ascii "div_pow2_LC_52"
	.space 1
	.ascii "div_pow2_LC_51"
	.space 1
	.ascii "div_pow2_LC_50"
	.space 1
	.ascii "div_pow2_LC_49"
	.space 1
	.ascii "div_pow2_LC_48"
	.space 1
	.ascii "div_pow2_LC_47"
	.space 1
	.ascii "div_pow2_LC_46"
	.space 1
	.ascii "div_pow2_LC_45"
	.space 1
	.ascii "div_pow2_LC_44"
	.space 1
	.ascii "div_pow2_LC_43"
	.space 1
	.ascii "div_pow2_LC_42"
	.space 1
	.ascii "div_pow2_LC_41"
	.space 1
	.ascii "div_pow2_LC_40"
	.space 1
	.ascii "div_pow2_LC_39"
	.space 1
	.ascii "div_pow2_LC_38"
	.space 1
	.ascii "div_pow2_LC_37"
	.space 1
	.ascii "div_pow2_LC_36"
	.space 1
	.ascii "div_pow2_LC_35"
	.space 1
	.ascii "div_pow2_LC_34"
	.space 1
	.ascii "div_pow2_LC_33"
	.space 1
	.ascii "div_pow2_LC_32"
	.space 1
	.ascii "div_pow2_LC_31"
	.space 1
	.ascii "div_pow2_LC_30"
	.space 1
	.ascii "div_pow2_LC_29"
	.space 1
	.ascii "div_pow2_LC_28"
	.space 1
	.ascii "div_pow2_LC_27"
	.space 1
	.ascii "div_pow2_LC_26"
	.space 1
	.ascii "div_pow2_LC_25"
	.space 1
	.ascii "div_pow2_LC_24"
	.space 1
	.ascii "div_pow2_LC_23"
	.space 1
	.ascii "div_pow2_LC_22"
	.space 1
	.ascii "div_pow2_LC_21"
	.space 1
	.ascii "div_pow2_LC_20"
	.space 1
	.ascii "div_pow2_LC_19"
	.space 1
	.ascii "div_pow2_LC_18"
	.space 1
	.ascii "div_pow2_LC_17"
	.space 1
	.ascii "div_pow2_LC_16"
	.space 1
	.ascii "div_pow2_LC_15"
	.space 1
	.ascii "div_pow2_LC_14"
	.space 1
	.ascii "div_pow2_LC_13"
	.space 1
	.ascii "div_pow2_LC_12"
	.space 1
	.ascii "div_pow2_LC_11"
	.space 1
	.ascii "div_pow2_LC_10"
	.space 1
	.ascii "div_pow2_LC_9"
	.space 1
	.ascii "div_pow2_LC_8"
	.space 1
	.ascii "div_pow2_LC_7"
	.space 1
	.ascii "div_pow2_LC_6"
	.space 1
	.ascii "div_pow2_LC_5"
	.space 1
	.ascii "div_pow2_LC_4"
	.space 1
	.ascii "div_pow2_LC_3"
	.space 1
	.ascii "div_pow2_LC_2"
	.space 1
	.ascii "div_pow2_LC_1"
	.space 1
	.ascii "div_pow2_LC_0"
	.space 1
	.ascii "div_pow2_L_63"
	.space 1
	.ascii "div_pow2_L_62"
	.space 1
	.ascii "div_pow2_L_61"
	.space 1
	.ascii "div_pow2_L_60"
	.space 1
	.ascii "div_pow2_L_59"
	.space 1
	.ascii "div_pow2_L_58"
	.space 1
	.ascii "div_pow2_L_57"
	.space 1
	.ascii "div_pow2_L_56"
	.space 1
	.ascii "div_pow2_L_55"
	.space 1
	.ascii "div_pow2_L_54"
	.space 1
	.ascii "div_pow2_L_53"
	.space 1
	.ascii "div_pow2_L_52"
	.space 1
	.ascii "div_pow2_L_51"
	.space 1
	.ascii "div_pow2_L_50"
	.space 1
	.ascii "div_pow2_L_49"
	.space 1
	.ascii "div_pow2_L_48"
	.space 1
	.ascii "div_pow2_L_47"
	.space 1
	.ascii "div_pow2_L_46"
	.space 1
	.ascii "div_pow2_L_45"
	.space 1
	.ascii "div_pow2_L_44"
	.space 1
	.ascii "div_pow2_L_43"
	.space 1
	.ascii "div_pow2_L_42"
	.space 1
	.ascii "div_pow2_L_41"
	.space 1
	.ascii "div_pow2_L_40"
	.space 1
	.ascii "div_pow2_L_39"
	.space 1
	.ascii "div_pow2_L_38"
	.space 1
	.ascii "div_pow2_L_37"
	.space 1
	.ascii "div_pow2_L_36"
	.space 1
	.ascii "div_pow2_L_35"
	.space 1
	.ascii "div_pow2_L_34"
	.space 1
	.ascii "div_pow2_L_33"
	.space 1
	.ascii "div_pow2_L_32"
	.space 1
	.ascii "div_pow2_L_31"
	.space 1
	.ascii "div_pow2_L_30"
	.space 1
	.ascii "div_pow2_L_29"
	.space 1
	.ascii "div_pow2_L_28"
	.space 1
	.ascii "div_pow2_L_27"
	.space 1
	.ascii "div_pow2_L_26"
	.space 1
	.ascii "div_pow2_L_25"
	.space 1
	.ascii "div_pow2_L_24"
	.space 1
	.ascii "div_pow2_L_23"
	.space 1
	.ascii "div_pow2_L_22"
	.space 1
	.ascii "div_pow2_L_21"
	.space 1
	.ascii "div_pow2_L_20"
	.space 1
	.ascii "div_pow2_L_19"
	.space 1
	.ascii "div_pow2_L_18"
	.space 1
	.ascii "div_pow2_L_17"
	.space 1
	.ascii "div_pow2_L_16"
	.space 1
	.ascii "div_pow2_L_15"
	.space 1
	.ascii "div_pow2_L_14"
	.space 1
	.ascii "div_pow2_L_13"
	.space 1
	.ascii "div_pow2_L_12"
	.space 1
	.ascii "div_pow2_L_11"
	.space 1
	.ascii "div_pow2_L_10"
	.space 1
	.ascii "div_pow2_L_9"
	.space 1
	.ascii "div_pow2_L_8"
	.space 1
	.ascii "div_pow2_L_7"
	.space 1
	.ascii "div_pow2_L_6"
	.space 1
	.ascii "div_pow2_L_5"
	.space 1
	.ascii "div_pow2_L_4"
	.space 1
	.ascii "div_pow2_L_3"
	.space 1
	.ascii "div_pow2_L_2"
	.space 1
	.ascii "div_pow2_L_1"
	.space 1
	.ascii "div_pow2_L_0"
	.space 1
	.ascii "div_pow2_C_63"
	.space 1
	.ascii "div_pow2_C_62"
	.space 1
	.ascii "div_pow2_C_61"
	.space 1
	.ascii "div_pow2_C_60"
	.space 1
	.ascii "div_pow2_C_59"
	.space 1
	.ascii "div_pow2_C_58"
	.space 1
	.ascii "div_pow2_C_57"
	.space 1
	.ascii "div_pow2_C_56"
	.space 1
	.ascii "div_pow2_C_55"
	.space 1
	.ascii "div_pow2_C_54"
	.space 1
	.ascii "div_pow2_C_53"
	.space 1
	.ascii "div_pow2_C_52"
	.space 1
	.ascii "div_pow2_C_51"
	.space 1
	.ascii "div_pow2_C_50"
	.space 1
	.ascii "div_pow2_C_49"
	.space 1
	.ascii "div_pow2_C_48"
	.space 1
	.ascii "div_pow2_C_47"
	.space 1
	.ascii "div_pow2_C_46"
	.space 1
	.ascii "div_pow2_C_45"
	.space 1
	.ascii "div_pow2_C_44"
	.space 1
	.ascii "div_pow2_C_43"
	.space 1
	.ascii "div_pow2_C_42"
	.space 1
	.ascii "div_pow2_C_41"
	.space 1
	.ascii "div_pow2_C_40"
	.space 1
	.ascii "div_pow2_C_39"
	.space 1
	.ascii "div_pow2_C_38"
	.space 1
	.ascii "div_pow2_C_37"
	.space 1
	.ascii "div_pow2_C_36"
	.space 1
	.ascii "div_pow2_C_35"
	.space 1
	.ascii "div_pow2_C_34"
	.space 1
	.ascii "div_pow2_C_33"
	.space 1
	.ascii "div_pow2_C_32"
	.space 1
	.ascii "div_pow2_C_31"
	.space 1
	.ascii "div_pow2_C_30"
	.space 1
	.ascii "div_pow2_C_29"
	.space 1
	.ascii "div_pow2_C_28"
	.space 1
	.ascii "div_pow2_C_27"
	.space 1
	.ascii "div_pow2_C_26"
	.space 1
	.ascii "div_pow2_C_25"
	.space 1
	.ascii "div_pow2_C_24"
	.space 1
	.ascii "div_pow2_C_23"
	.space 1
	.ascii "div_pow2_C_22"
	.space 1
	.ascii "div_pow2_C_21"
	.space 1
	.ascii "div_pow2_C_20"
	.space 1
	.ascii "div_pow2_C_19"
	.space 1
	.ascii "div_pow2_C_18"
	.space 1
	.ascii "div_pow2_C_17"
	.space 1
	.ascii "div_pow2_C_16"
	.space 1
	.ascii "div_pow2_C_15"
	.space 1
	.ascii "div_pow2_C_14"
	.space 1
	.ascii "div_pow2_C_13"
	.space 1
	.ascii "div_pow2_C_12"
	.space 1
	.ascii "div_pow2_C_11"
	.space 1
	.ascii "div_pow2_C_10"
	.space 1
	.ascii "div_pow2_C_9"
	.space 1
	.ascii "div_pow2_C_8"
	.space 1
	.ascii "div_pow2_C_7"
	.space 1
	.ascii "div_pow2_C_6"
	.space 1
	.ascii "div_pow2_C_5"
	.space 1
	.ascii "div_pow2_C_4"
	.space 1
	.ascii "div_pow2_C_3"
	.space 1
	.ascii "div_pow2_C_2"
	.space 1
	.ascii "div_pow2_C_1"
	.space 1
	.ascii "div_pow2_C_0"
	.space 1
	.ascii "div_pow2_I_63"
	.space 1
	.ascii "div_pow2_I_62"
	.space 1
	.ascii "div_pow2_I_61"
	.space 1
	.ascii "div_pow2_I_60"
	.space 1
	.ascii "div_pow2_I_59"
	.space 1
	.ascii "div_pow2_I_58"
	.space 1
	.ascii "div_pow2_I_57"
	.space 1
	.ascii "div_pow2_I_56"
	.space 1
	.ascii "div_pow2_I_55"
	.space 1
	.ascii "div_pow2_I_54"
	.space 1
	.ascii "div_pow2_I_53"
	.space 1
	.ascii "div_pow2_I_52"
	.space 1
	.ascii "div_pow2_I_51"
	.space 1
	.ascii "div_pow2_I_50"
	.space 1
	.ascii "div_pow2_I_49"
	.space 1
	.ascii "div_pow2_I_48"
	.space 1
	.ascii "div_pow2_I_47"
	.space 1
	.ascii "div_pow2_I_46"
	.space 1
	.ascii "div_pow2_I_45"
	.space 1
	.ascii "div_pow2_I_44"
	.space 1
	.ascii "div_pow2_I_43"
	.space 1
	.ascii "div_pow2_I_42"
	.space 1
	.ascii "div_pow2_I_41"
	.space 1
	.ascii "div_pow2_I_40"
	.space 1
	.ascii "div_pow2_I_39"
	.space 1
	.ascii "div_pow2_I_38"
	.space 1
	.ascii "div_pow2_I_37"
	.space 1
	.ascii "div_pow2_I_36"
	.space 1
	.ascii "div_pow2_I_35"
	.space 1
	.ascii "div_pow2_I_34"
	.space 1
	.ascii "div_pow2_I_33"
	.space 1
	.ascii "div_pow2_I_32"
	.space 1
	.ascii "div_pow2_I_31"
	.space 1
	.ascii "div_pow2_I_30"
	.space 1
	.ascii "div_pow2_I_29"
	.space 1
	.ascii "div_pow2_I_28"
	.space 1
	.ascii "div_pow2_I_27"
	.space 1
	.ascii "div_pow2_I_26"
	.space 1
	.ascii "div_pow2_I_25"
	.space 1
	.ascii "div_pow2_I_24"
	.space 1
	.ascii "div_pow2_I_23"
	.space 1
	.ascii "div_pow2_I_22"
	.space 1
	.ascii "div_pow2_I_21"
	.space 1
	.ascii "div_pow2_I_20"
	.space 1
	.ascii "div_pow2_I_19"
	.space 1
	.ascii "div_pow2_I_18"
	.space 1
	.ascii "div_pow2_I_17"
	.space 1
	.ascii "div_pow2_I_16"
	.space 1
	.ascii "div_pow2_I_15"
	.space 1
	.ascii "div_pow2_I_14"
	.space 1
	.ascii "div_pow2_I_13"
	.space 1
	.ascii "div_pow2_I_12"
	.space 1
	.ascii "div_pow2_I_11"
	.space 1
	.ascii "div_pow2_I_10"
	.space 1
	.ascii "div_pow2_I_9"
	.space 1
	.ascii "div_pow2_I_8"
	.space 1
	.ascii "div_pow2_I_7"
	.space 1
	.ascii "div_pow2_I_6"
	.space 1
	.ascii "div_pow2_I_5"
	.space 1
	.ascii "div_pow2_I_4"
	.space 1
	.ascii "div_pow2_I_3"
	.space 1
	.ascii "div_pow2_I_2"
	.space 1
	.ascii "div_pow2_I_1"
	.space 1
	.ascii "div_pow2_I_0"
	.space 1
	.ascii "div_pow2_u64_63"
	.space 1
	.ascii "div_pow2_u64_62"
	.space 1
	.ascii "div_pow2_u64_61"
	.space 1
	.ascii "div_pow2_u64_60"
	.space 1
	.ascii "div_pow2_u64_59"
	.space 1
	.ascii "div_pow2_u64_58"
	.space 1
	.ascii "div_pow2_u64_57"
	.space 1
	.ascii "div_pow2_u64_56"
	.space 1
	.ascii "div_pow2_u64_55"
	.space 1
	.ascii "div_pow2_u64_54"
	.space 1
	.ascii "div_pow2_u64_53"
	.space 1
	.ascii "div_pow2_u64_52"
	.space 1
	.ascii "div_pow2_u64_51"
	.space 1
	.ascii "div_pow2_u64_50"
	.space 1
	.ascii "div_pow2_u64_49"
	.space 1
	.ascii "div_pow2_u64_48"
	.space 1
	.ascii "div_pow2_u64_47"
	.space 1
	.ascii "div_pow2_u64_46"
	.space 1
	.ascii "div_pow2_u64_45"
	.space 1
	.ascii "div_pow2_u64_44"
	.space 1
	.ascii "div_pow2_u64_43"
	.space 1
	.ascii "div_pow2_u64_42"
	.space 1
	.ascii "div_pow2_u64_41"
	.space 1
	.ascii "div_pow2_u64_40"
	.space 1
	.ascii "div_pow2_u64_39"
	.space 1
	.ascii "div_pow2_u64_38"
	.space 1
	.ascii "div_pow2_u64_37"
	.space 1
	.ascii "div_pow2_u64_36"
	.space 1
	.ascii "div_pow2_u64_35"
	.space 1
	.ascii "div_pow2_u64_34"
	.space 1
	.ascii "div_pow2_u64_33"
	.space 1
	.ascii "div_pow2_u64_32"
	.space 1
	.ascii "div_pow2_u64_31"
	.space 1
	.ascii "div_pow2_u64_30"
	.space 1
	.ascii "div_pow2_u64_29"
	.space 1
	.ascii "div_pow2_u64_28"
	.space 1
	.ascii "div_pow2_u64_27"
	.space 1
	.ascii "div_pow2_u64_26"
	.space 1
	.ascii "div_pow2_u64_25"
	.space 1
	.ascii "div_pow2_u64_24"
	.space 1
	.ascii "div_pow2_u64_23"
	.space 1
	.ascii "div_pow2_u64_22"
	.space 1
	.ascii "div_pow2_u64_21"
	.space 1
	.ascii "div_pow2_u64_20"
	.space 1
	.ascii "div_pow2_u64_19"
	.space 1
	.ascii "div_pow2_u64_18"
	.space 1
	.ascii "div_pow2_u64_17"
	.space 1
	.ascii "div_pow2_u64_16"
	.space 1
	.ascii "div_pow2_u64_15"
	.space 1
	.ascii "div_pow2_u64_14"
	.space 1
	.ascii "div_pow2_u64_13"
	.space 1
	.ascii "div_pow2_u64_12"
	.space 1
	.ascii "div_pow2_u64_11"
	.space 1
	.ascii "div_pow2_u64_10"
	.space 1
	.ascii "div_pow2_u64_9"
	.space 1
	.ascii "div_pow2_u64_8"
	.space 1
	.ascii "div_pow2_u64_7"
	.space 1
	.ascii "div_pow2_u64_6"
	.space 1
	.ascii "div_pow2_u64_5"
	.space 1
	.ascii "div_pow2_u64_4"
	.space 1
	.ascii "div_pow2_u64_3"
	.space 1
	.ascii "div_pow2_u64_2"
	.space 1
	.ascii "div_pow2_u64_1"
	.space 1
	.ascii "div_pow2_u64_0"
	.space 1
	.ascii "div_pow2_i64_63"
	.space 1
	.ascii "div_pow2_i64_62"
	.space 1
	.ascii "div_pow2_i64_61"
	.space 1
	.ascii "div_pow2_i64_60"
	.space 1
	.ascii "div_pow2_i64_59"
	.space 1
	.ascii "div_pow2_i64_58"
	.space 1
	.ascii "div_pow2_i64_57"
	.space 1
	.ascii "div_pow2_i64_56"
	.space 1
	.ascii "div_pow2_i64_55"
	.space 1
	.ascii "div_pow2_i64_54"
	.space 1
	.ascii "div_pow2_i64_53"
	.space 1
	.ascii "div_pow2_i64_52"
	.space 1
	.ascii "div_pow2_i64_51"
	.space 1
	.ascii "div_pow2_i64_50"
	.space 1
	.ascii "div_pow2_i64_49"
	.space 1
	.ascii "div_pow2_i64_48"
	.space 1
	.ascii "div_pow2_i64_47"
	.space 1
	.ascii "div_pow2_i64_46"
	.space 1
	.ascii "div_pow2_i64_45"
	.space 1
	.ascii "div_pow2_i64_44"
	.space 1
	.ascii "div_pow2_i64_43"
	.space 1
	.ascii "div_pow2_i64_42"
	.space 1
	.ascii "div_pow2_i64_41"
	.space 1
	.ascii "div_pow2_i64_40"
	.space 1
	.ascii "div_pow2_i64_39"
	.space 1
	.ascii "div_pow2_i64_38"
	.space 1
	.ascii "div_pow2_i64_37"
	.space 1
	.ascii "div_pow2_i64_36"
	.space 1
	.ascii "div_pow2_i64_35"
	.space 1
	.ascii "div_pow2_i64_34"
	.space 1
	.ascii "div_pow2_i64_33"
	.space 1
	.ascii "div_pow2_i64_32"
	.space 1
	.ascii "div_pow2_i64_31"
	.space 1
	.ascii "div_pow2_i64_30"
	.space 1
	.ascii "div_pow2_i64_29"
	.space 1
	.ascii "div_pow2_i64_28"
	.space 1
	.ascii "div_pow2_i64_27"
	.space 1
	.ascii "div_pow2_i64_26"
	.space 1
	.ascii "div_pow2_i64_25"
	.space 1
	.ascii "div_pow2_i64_24"
	.space 1
	.ascii "div_pow2_i64_23"
	.space 1
	.ascii "div_pow2_i64_22"
	.space 1
	.ascii "div_pow2_i64_21"
	.space 1
	.ascii "div_pow2_i64_20"
	.space 1
	.ascii "div_pow2_i64_19"
	.space 1
	.ascii "div_pow2_i64_18"
	.space 1
	.ascii "div_pow2_i64_17"
	.space 1
	.ascii "div_pow2_i64_16"
	.space 1
	.ascii "div_pow2_i64_15"
	.space 1
	.ascii "div_pow2_i64_14"
	.space 1
	.ascii "div_pow2_i64_13"
	.space 1
	.ascii "div_pow2_i64_12"
	.space 1
	.ascii "div_pow2_i64_11"
	.space 1
	.ascii "div_pow2_i64_10"
	.space 1
	.ascii "div_pow2_i64_9"
	.space 1
	.ascii "div_pow2_i64_8"
	.space 1
	.ascii "div_pow2_i64_7"
	.space 1
	.ascii "div_pow2_i64_6"
	.space 1
	.ascii "div_pow2_i64_5"
	.space 1
	.ascii "div_pow2_i64_4"
	.space 1
	.ascii "div_pow2_i64_3"
	.space 1
	.ascii "div_pow2_i64_2"
	.space 1
	.ascii "div_pow2_i64_1"
	.space 1
	.ascii "div_pow2_i64_0"
	.space 1
	.ascii "div_pow2_u32_31"
	.space 1
	.ascii "div_pow2_u32_30"
	.space 1
	.ascii "div_pow2_u32_29"
	.space 1
	.ascii "div_pow2_u32_28"
	.space 1
	.ascii "div_pow2_u32_27"
	.space 1
	.ascii "div_pow2_u32_26"
	.space 1
	.ascii "div_pow2_u32_25"
	.space 1
	.ascii "div_pow2_u32_24"
	.space 1
	.ascii "div_pow2_u32_23"
	.space 1
	.ascii "div_pow2_u32_22"
	.space 1
	.ascii "div_pow2_u32_21"
	.space 1
	.ascii "div_pow2_u32_20"
	.space 1
	.ascii "div_pow2_u32_19"
	.space 1
	.ascii "div_pow2_u32_18"
	.space 1
	.ascii "div_pow2_u32_17"
	.space 1
	.ascii "div_pow2_u32_16"
	.space 1
	.ascii "div_pow2_u32_15"
	.space 1
	.ascii "div_pow2_u32_14"
	.space 1
	.ascii "div_pow2_u32_13"
	.space 1
	.ascii "div_pow2_u32_12"
	.space 1
	.ascii "div_pow2_u32_11"
	.space 1
	.ascii "div_pow2_u32_10"
	.space 1
	.ascii "div_pow2_u32_9"
	.space 1
	.ascii "div_pow2_u32_8"
	.space 1
	.ascii "div_pow2_u32_7"
	.space 1
	.ascii "div_pow2_u32_6"
	.space 1
	.ascii "div_pow2_u32_5"
	.space 1
	.ascii "div_pow2_u32_4"
	.space 1
	.ascii "div_pow2_u32_3"
	.space 1
	.ascii "div_pow2_u32_2"
	.space 1
	.ascii "div_pow2_u32_1"
	.space 1
	.ascii "div_pow2_u32_0"
	.space 1
	.ascii "div_pow2_i32_31"
	.space 1
	.ascii "div_pow2_i32_30"
	.space 1
	.ascii "div_pow2_i32_29"
	.space 1
	.ascii "div_pow2_i32_28"
	.space 1
	.ascii "div_pow2_i32_27"
	.space 1
	.ascii "div_pow2_i32_26"
	.space 1
	.ascii "div_pow2_i32_25"
	.space 1
	.ascii "div_pow2_i32_24"
	.space 1
	.ascii "div_pow2_i32_23"
	.space 1
	.ascii "div_pow2_i32_22"
	.space 1
	.ascii "div_pow2_i32_21"
	.space 1
	.ascii "div_pow2_i32_20"
	.space 1
	.ascii "div_pow2_i32_19"
	.space 1
	.ascii "div_pow2_i32_18"
	.space 1
	.ascii "div_pow2_i32_17"
	.space 1
	.ascii "div_pow2_i32_16"
	.space 1
	.ascii "div_pow2_i32_15"
	.space 1
	.ascii "div_pow2_i32_14"
	.space 1
	.ascii "div_pow2_i32_13"
	.space 1
	.ascii "div_pow2_i32_12"
	.space 1
	.ascii "div_pow2_i32_11"
	.space 1
	.ascii "div_pow2_i32_10"
	.space 1
	.ascii "div_pow2_i32_9"
	.space 1
	.ascii "div_pow2_i32_8"
	.space 1
	.ascii "div_pow2_i32_7"
	.space 1
	.ascii "div_pow2_i32_6"
	.space 1
	.ascii "div_pow2_i32_5"
	.space 1
	.ascii "div_pow2_i32_4"
	.space 1
	.ascii "div_pow2_i32_3"
	.space 1
	.ascii "div_pow2_i32_2"
	.space 1
	.ascii "div_pow2_i32_1"
	.space 1
	.ascii "div_pow2_i32_0"
	.space 5
	.quad	_div_pow2_M3
	.quad	_L_1
	.quad	_div_pow2__div_pow2_LC_63
	.quad	_L_1+12
	.quad	_div_pow2__div_pow2_LC_62
	.quad	_L_1+27
	.quad	_div_pow2__div_pow2_LC_61
	.quad	_L_1+42
	.quad	_div_pow2__div_pow2_LC_60
	.quad	_L_1+57
	.quad	_div_pow2__div_pow2_LC_59
	.quad	_L_1+72
	.quad	_div_pow2__div_pow2_LC_58
	.quad	_L_1+87
	.quad	_div_pow2__div_pow2_LC_57
	.quad	_L_1+102
	.quad	_div_pow2__div_pow2_LC_56
	.quad	_L_1+117
	.quad	_div_pow2__div_pow2_LC_55
	.quad	_L_1+132
	.quad	_div_pow2__div_pow2_LC_54
	.quad	_L_1+147
	.quad	_div_pow2__div_pow2_LC_53
	.quad	_L_1+162
	.quad	_div_pow2__div_pow2_LC_52
	.quad	_L_1+177
	.quad	_div_pow2__div_pow2_LC_51
	.quad	_L_1+192
	.quad	_div_pow2__div_pow2_LC_50
	.quad	_L_1+207
	.quad	_div_pow2__div_pow2_LC_49
	.quad	_L_1+222
	.quad	_div_pow2__div_pow2_LC_48
	.quad	_L_1+237
	.quad	_div_pow2__div_pow2_LC_47
	.quad	_L_1+252
	.quad	_div_pow2__div_pow2_LC_46
	.quad	_L_1+267
	.quad	_div_pow2__div_pow2_LC_45
	.quad	_L_1+282
	.quad	_div_pow2__div_pow2_LC_44
	.quad	_L_1+297
	.quad	_div_pow2__div_pow2_LC_43
	.quad	_L_1+312
	.quad	_div_pow2__div_pow2_LC_42
	.quad	_L_1+327
	.quad	_div_pow2__div_pow2_LC_41
	.quad	_L_1+342
	.quad	_div_pow2__div_pow2_LC_40
	.quad	_L_1+357
	.quad	_div_pow2__div_pow2_LC_39
	.quad	_L_1+372
	.quad	_div_pow2__div_pow2_LC_38
	.quad	_L_1+387
	.quad	_div_pow2__div_pow2_LC_37
	.quad	_L_1+402
	.quad	_div_pow2__div_pow2_LC_36
	.quad	_L_1+417
	.quad	_div_pow2__div_pow2_LC_35
	.quad	_L_1+432
	.quad	_div_pow2__div_pow2_LC_34
	.quad	_L_1+447
	.quad	_div_pow2__div_pow2_LC_33
	.quad	_L_1+462
	.quad	_div_pow2__div_pow2_LC_32
	.quad	_L_1+477
	.quad	_div_pow2__div_pow2_LC_31
	.quad	_L_1+492
	.quad	_div_pow2__div_pow2_LC_30
	.quad	_L_1+507
	.quad	_div_pow2__div_pow2_LC_29
	.quad	_L_1+522
	.quad	_div_pow2__div_pow2_LC_28
	.quad	_L_1+537
	.quad	_div_pow2__div_pow2_LC_27
	.quad	_L_1+552
	.quad	_div_pow2__div_pow2_LC_26
	.quad	_L_1+567
	.quad	_div_pow2__div_pow2_LC_25
	.quad	_L_1+582
	.quad	_div_pow2__div_pow2_LC_24
	.quad	_L_1+597
	.quad	_div_pow2__div_pow2_LC_23
	.quad	_L_1+612
	.quad	_div_pow2__div_pow2_LC_22
	.quad	_L_1+627
	.quad	_div_pow2__div_pow2_LC_21
	.quad	_L_1+642
	.quad	_div_pow2__div_pow2_LC_20
	.quad	_L_1+657
	.quad	_div_pow2__div_pow2_LC_19
	.quad	_L_1+672
	.quad	_div_pow2__div_pow2_LC_18
	.quad	_L_1+687
	.quad	_div_pow2__div_pow2_LC_17
	.quad	_L_1+702
	.quad	_div_pow2__div_pow2_LC_16
	.quad	_L_1+717
	.quad	_div_pow2__div_pow2_LC_15
	.quad	_L_1+732
	.quad	_div_pow2__div_pow2_LC_14
	.quad	_L_1+747
	.quad	_div_pow2__div_pow2_LC_13
	.quad	_L_1+762
	.quad	_div_pow2__div_pow2_LC_12
	.quad	_L_1+777
	.quad	_div_pow2__div_pow2_LC_11
	.quad	_L_1+792
	.quad	_div_pow2__div_pow2_LC_10
	.quad	_L_1+807
	.quad	_div_pow2__div_pow2_LC_9
	.quad	_L_1+822
	.quad	_div_pow2__div_pow2_LC_8
	.quad	_L_1+836
	.quad	_div_pow2__div_pow2_LC_7
	.quad	_L_1+850
	.quad	_div_pow2__div_pow2_LC_6
	.quad	_L_1+864
	.quad	_div_pow2__div_pow2_LC_5
	.quad	_L_1+878
	.quad	_div_pow2__div_pow2_LC_4
	.quad	_L_1+892
	.quad	_div_pow2__div_pow2_LC_3
	.quad	_L_1+906
	.quad	_div_pow2__div_pow2_LC_2
	.quad	_L_1+920
	.quad	_div_pow2__div_pow2_LC_1
	.quad	_L_1+934
	.quad	_div_pow2__div_pow2_LC_0
	.quad	_L_1+948
	.quad	_div_pow2__div_pow2_L_63
	.quad	_L_1+962
	.quad	_div_pow2__div_pow2_L_62
	.quad	_L_1+976
	.quad	_div_pow2__div_pow2_L_61
	.quad	_L_1+990
	.quad	_div_pow2__div_pow2_L_60
	.quad	_L_1+1004
	.quad	_div_pow2__div_pow2_L_59
	.quad	_L_1+1018
	.quad	_div_pow2__div_pow2_L_58
	.quad	_L_1+1032
	.quad	_div_pow2__div_pow2_L_57
	.quad	_L_1+1046
	.quad	_div_pow2__div_pow2_L_56
	.quad	_L_1+1060
	.quad	_div_pow2__div_pow2_L_55
	.quad	_L_1+1074
	.quad	_div_pow2__div_pow2_L_54
	.quad	_L_1+1088
	.quad	_div_pow2__div_pow2_L_53
	.quad	_L_1+1102
	.quad	_div_pow2__div_pow2_L_52
	.quad	_L_1+1116
	.quad	_div_pow2__div_pow2_L_51
	.quad	_L_1+1130
	.quad	_div_pow2__div_pow2_L_50
	.quad	_L_1+1144
	.quad	_div_pow2__div_pow2_L_49
	.quad	_L_1+1158
	.quad	_div_pow2__div_pow2_L_48
	.quad	_L_1+1172
	.quad	_div_pow2__div_pow2_L_47
	.quad	_L_1+1186
	.quad	_div_pow2__div_pow2_L_46
	.quad	_L_1+1200
	.quad	_div_pow2__div_pow2_L_45
	.quad	_L_1+1214
	.quad	_div_pow2__div_pow2_L_44
	.quad	_L_1+1228
	.quad	_div_pow2__div_pow2_L_43
	.quad	_L_1+1242
	.quad	_div_pow2__div_pow2_L_42
	.quad	_L_1+1256
	.quad	_div_pow2__div_pow2_L_41
	.quad	_L_1+1270
	.quad	_div_pow2__div_pow2_L_40
	.quad	_L_1+1284
	.quad	_div_pow2__div_pow2_L_39
	.quad	_L_1+1298
	.quad	_div_pow2__div_pow2_L_38
	.quad	_L_1+1312
	.quad	_div_pow2__div_pow2_L_37
	.quad	_L_1+1326
	.quad	_div_pow2__div_pow2_L_36
	.quad	_L_1+1340
	.quad	_div_pow2__div_pow2_L_35
	.quad	_L_1+1354
	.quad	_div_pow2__div_pow2_L_34
	.quad	_L_1+1368
	.quad	_div_pow2__div_pow2_L_33
	.quad	_L_1+1382
	.quad	_div_pow2__div_pow2_L_32
	.quad	_L_1+1396
	.quad	_div_pow2__div_pow2_L_31
	.quad	_L_1+1410
	.quad	_div_pow2__div_pow2_L_30
	.quad	_L_1+1424
	.quad	_div_pow2__div_pow2_L_29
	.quad	_L_1+1438
	.quad	_div_pow2__div_pow2_L_28
	.quad	_L_1+1452
	.quad	_div_pow2__div_pow2_L_27
	.quad	_L_1+1466
	.quad	_div_pow2__div_pow2_L_26
	.quad	_L_1+1480
	.quad	_div_pow2__div_pow2_L_25
	.quad	_L_1+1494
	.quad	_div_pow2__div_pow2_L_24
	.quad	_L_1+1508
	.quad	_div_pow2__div_pow2_L_23
	.quad	_L_1+1522
	.quad	_div_pow2__div_pow2_L_22
	.quad	_L_1+1536
	.quad	_div_pow2__div_pow2_L_21
	.quad	_L_1+1550
	.quad	_div_pow2__div_pow2_L_20
	.quad	_L_1+1564
	.quad	_div_pow2__div_pow2_L_19
	.quad	_L_1+1578
	.quad	_div_pow2__div_pow2_L_18
	.quad	_L_1+1592
	.quad	_div_pow2__div_pow2_L_17
	.quad	_L_1+1606
	.quad	_div_pow2__div_pow2_L_16
	.quad	_L_1+1620
	.quad	_div_pow2__div_pow2_L_15
	.quad	_L_1+1634
	.quad	_div_pow2__div_pow2_L_14
	.quad	_L_1+1648
	.quad	_div_pow2__div_pow2_L_13
	.quad	_L_1+1662
	.quad	_div_pow2__div_pow2_L_12
	.quad	_L_1+1676
	.quad	_div_pow2__div_pow2_L_11
	.quad	_L_1+1690
	.quad	_div_pow2__div_pow2_L_10
	.quad	_L_1+1704
	.quad	_div_pow2__div_pow2_L_9
	.quad	_L_1+1718
	.quad	_div_pow2__div_pow2_L_8
	.quad	_L_1+1731
	.quad	_div_pow2__div_pow2_L_7
	.quad	_L_1+1744
	.quad	_div_pow2__div_pow2_L_6
	.quad	_L_1+1757
	.quad	_div_pow2__div_pow2_L_5
	.quad	_L_1+1770
	.quad	_div_pow2__div_pow2_L_4
	.quad	_L_1+1783
	.quad	_div_pow2__div_pow2_L_3
	.quad	_L_1+1796
	.quad	_div_pow2__div_pow2_L_2
	.quad	_L_1+1809
	.quad	_div_pow2__div_pow2_L_1
	.quad	_L_1+1822
	.quad	_div_pow2__div_pow2_L_0
	.quad	_L_1+1835
	.quad	_div_pow2__div_pow2_C_63
	.quad	_L_1+1848
	.quad	_div_pow2__div_pow2_C_62
	.quad	_L_1+1862
	.quad	_div_pow2__div_pow2_C_61
	.quad	_L_1+1876
	.quad	_div_pow2__div_pow2_C_60
	.quad	_L_1+1890
	.quad	_div_pow2__div_pow2_C_59
	.quad	_L_1+1904
	.quad	_div_pow2__div_pow2_C_58
	.quad	_L_1+1918
	.quad	_div_pow2__div_pow2_C_57
	.quad	_L_1+1932
	.quad	_div_pow2__div_pow2_C_56
	.quad	_L_1+1946
	.quad	_div_pow2__div_pow2_C_55
	.quad	_L_1+1960
	.quad	_div_pow2__div_pow2_C_54
	.quad	_L_1+1974
	.quad	_div_pow2__div_pow2_C_53
	.quad	_L_1+1988
	.quad	_div_pow2__div_pow2_C_52
	.quad	_L_1+2002
	.quad	_div_pow2__div_pow2_C_51
	.quad	_L_1+2016
	.quad	_div_pow2__div_pow2_C_50
	.quad	_L_1+2030
	.quad	_div_pow2__div_pow2_C_49
	.quad	_L_1+2044
	.quad	_div_pow2__div_pow2_C_48
	.quad	_L_1+2058
	.quad	_div_pow2__div_pow2_C_47
	.quad	_L_1+2072
	.quad	_div_pow2__div_pow2_C_46
	.quad	_L_1+2086
	.quad	_div_pow2__div_pow2_C_45
	.quad	_L_1+2100
	.quad	_div_pow2__div_pow2_C_44
	.quad	_L_1+2114
	.quad	_div_pow2__div_pow2_C_43
	.quad	_L_1+2128
	.quad	_div_pow2__div_pow2_C_42
	.quad	_L_1+2142
	.quad	_div_pow2__div_pow2_C_41
	.quad	_L_1+2156
	.quad	_div_pow2__div_pow2_C_40
	.quad	_L_1+2170
	.quad	_div_pow2__div_pow2_C_39
	.quad	_L_1+2184
	.quad	_div_pow2__div_pow2_C_38
	.quad	_L_1+2198
	.quad	_div_pow2__div_pow2_C_37
	.quad	_L_1+2212
	.quad	_div_pow2__div_pow2_C_36
	.quad	_L_1+2226
	.quad	_div_pow2__div_pow2_C_35
	.quad	_L_1+2240
	.quad	_div_pow2__div_pow2_C_34
	.quad	_L_1+2254
	.quad	_div_pow2__div_pow2_C_33
	.quad	_L_1+2268
	.quad	_div_pow2__div_pow2_C_32
	.quad	_L_1+2282
	.quad	_div_pow2__div_pow2_C_31
	.quad	_L_1+2296
	.quad	_div_pow2__div_pow2_C_30
	.quad	_L_1+2310
	.quad	_div_pow2__div_pow2_C_29
	.quad	_L_1+2324
	.quad	_div_pow2__div_pow2_C_28
	.quad	_L_1+2338
	.quad	_div_pow2__div_pow2_C_27
	.quad	_L_1+2352
	.quad	_div_pow2__div_pow2_C_26
	.quad	_L_1+2366
	.quad	_div_pow2__div_pow2_C_25
	.quad	_L_1+2380
	.quad	_div_pow2__div_pow2_C_24
	.quad	_L_1+2394
	.quad	_div_pow2__div_pow2_C_23
	.quad	_L_1+2408
	.quad	_div_pow2__div_pow2_C_22
	.quad	_L_1+2422
	.quad	_div_pow2__div_pow2_C_21
	.quad	_L_1+2436
	.quad	_div_pow2__div_pow2_C_20
	.quad	_L_1+2450
	.quad	_div_pow2__div_pow2_C_19
	.quad	_L_1+2464
	.quad	_div_pow2__div_pow2_C_18
	.quad	_L_1+2478
	.quad	_div_pow2__div_pow2_C_17
	.quad	_L_1+2492
	.quad	_div_pow2__div_pow2_C_16
	.quad	_L_1+2506
	.quad	_div_pow2__div_pow2_C_15
	.quad	_L_1+2520
	.quad	_div_pow2__div_pow2_C_14
	.quad	_L_1+2534
	.quad	_div_pow2__div_pow2_C_13
	.quad	_L_1+2548
	.quad	_div_pow2__div_pow2_C_12
	.quad	_L_1+2562
	.quad	_div_pow2__div_pow2_C_11
	.quad	_L_1+2576
	.quad	_div_pow2__div_pow2_C_10
	.quad	_L_1+2590
	.quad	_div_pow2__div_pow2_C_9
	.quad	_L_1+2604
	.quad	_div_pow2__div_pow2_C_8
	.quad	_L_1+2617
	.quad	_div_pow2__div_pow2_C_7
	.quad	_L_1+2630
	.quad	_div_pow2__div_pow2_C_6
	.quad	_L_1+2643
	.quad	_div_pow2__div_pow2_C_5
	.quad	_L_1+2656
	.quad	_div_pow2__div_pow2_C_4
	.quad	_L_1+2669
	.quad	_div_pow2__div_pow2_C_3
	.quad	_L_1+2682
	.quad	_div_pow2__div_pow2_C_2
	.quad	_L_1+2695
	.quad	_div_pow2__div_pow2_C_1
	.quad	_L_1+2708
	.quad	_div_pow2__div_pow2_C_0
	.quad	_L_1+2721
	.quad	_div_pow2__div_pow2_I_63
	.quad	_L_1+2734
	.quad	_div_pow2__div_pow2_I_62
	.quad	_L_1+2748
	.quad	_div_pow2__div_pow2_I_61
	.quad	_L_1+2762
	.quad	_div_pow2__div_pow2_I_60
	.quad	_L_1+2776
	.quad	_div_pow2__div_pow2_I_59
	.quad	_L_1+2790
	.quad	_div_pow2__div_pow2_I_58
	.quad	_L_1+2804
	.quad	_div_pow2__div_pow2_I_57
	.quad	_L_1+2818
	.quad	_div_pow2__div_pow2_I_56
	.quad	_L_1+2832
	.quad	_div_pow2__div_pow2_I_55
	.quad	_L_1+2846
	.quad	_div_pow2__div_pow2_I_54
	.quad	_L_1+2860
	.quad	_div_pow2__div_pow2_I_53
	.quad	_L_1+2874
	.quad	_div_pow2__div_pow2_I_52
	.quad	_L_1+2888
	.quad	_div_pow2__div_pow2_I_51
	.quad	_L_1+2902
	.quad	_div_pow2__div_pow2_I_50
	.quad	_L_1+2916
	.quad	_div_pow2__div_pow2_I_49
	.quad	_L_1+2930
	.quad	_div_pow2__div_pow2_I_48
	.quad	_L_1+2944
	.quad	_div_pow2__div_pow2_I_47
	.quad	_L_1+2958
	.quad	_div_pow2__div_pow2_I_46
	.quad	_L_1+2972
	.quad	_div_pow2__div_pow2_I_45
	.quad	_L_1+2986
	.quad	_div_pow2__div_pow2_I_44
	.quad	_L_1+3000
	.quad	_div_pow2__div_pow2_I_43
	.quad	_L_1+3014
	.quad	_div_pow2__div_pow2_I_42
	.quad	_L_1+3028
	.quad	_div_pow2__div_pow2_I_41
	.quad	_L_1+3042
	.quad	_div_pow2__div_pow2_I_40
	.quad	_L_1+3056
	.quad	_div_pow2__div_pow2_I_39
	.quad	_L_1+3070
	.quad	_div_pow2__div_pow2_I_38
	.quad	_L_1+3084
	.quad	_div_pow2__div_pow2_I_37
	.quad	_L_1+3098
	.quad	_div_pow2__div_pow2_I_36
	.quad	_L_1+3112
	.quad	_div_pow2__div_pow2_I_35
	.quad	_L_1+3126
	.quad	_div_pow2__div_pow2_I_34
	.quad	_L_1+3140
	.quad	_div_pow2__div_pow2_I_33
	.quad	_L_1+3154
	.quad	_div_pow2__div_pow2_I_32
	.quad	_L_1+3168
	.quad	_div_pow2__div_pow2_I_31
	.quad	_L_1+3182
	.quad	_div_pow2__div_pow2_I_30
	.quad	_L_1+3196
	.quad	_div_pow2__div_pow2_I_29
	.quad	_L_1+3210
	.quad	_div_pow2__div_pow2_I_28
	.quad	_L_1+3224
	.quad	_div_pow2__div_pow2_I_27
	.quad	_L_1+3238
	.quad	_div_pow2__div_pow2_I_26
	.quad	_L_1+3252
	.quad	_div_pow2__div_pow2_I_25
	.quad	_L_1+3266
	.quad	_div_pow2__div_pow2_I_24
	.quad	_L_1+3280
	.quad	_div_pow2__div_pow2_I_23
	.quad	_L_1+3294
	.quad	_div_pow2__div_pow2_I_22
	.quad	_L_1+3308
	.quad	_div_pow2__div_pow2_I_21
	.quad	_L_1+3322
	.quad	_div_pow2__div_pow2_I_20
	.quad	_L_1+3336
	.quad	_div_pow2__div_pow2_I_19
	.quad	_L_1+3350
	.quad	_div_pow2__div_pow2_I_18
	.quad	_L_1+3364
	.quad	_div_pow2__div_pow2_I_17
	.quad	_L_1+3378
	.quad	_div_pow2__div_pow2_I_16
	.quad	_L_1+3392
	.quad	_div_pow2__div_pow2_I_15
	.quad	_L_1+3406
	.quad	_div_pow2__div_pow2_I_14
	.quad	_L_1+3420
	.quad	_div_pow2__div_pow2_I_13
	.quad	_L_1+3434
	.quad	_div_pow2__div_pow2_I_12
	.quad	_L_1+3448
	.quad	_div_pow2__div_pow2_I_11
	.quad	_L_1+3462
	.quad	_div_pow2__div_pow2_I_10
	.quad	_L_1+3476
	.quad	_div_pow2__div_pow2_I_9
	.quad	_L_1+3490
	.quad	_div_pow2__div_pow2_I_8
	.quad	_L_1+3503
	.quad	_div_pow2__div_pow2_I_7
	.quad	_L_1+3516
	.quad	_div_pow2__div_pow2_I_6
	.quad	_L_1+3529
	.quad	_div_pow2__div_pow2_I_5
	.quad	_L_1+3542
	.quad	_div_pow2__div_pow2_I_4
	.quad	_L_1+3555
	.quad	_div_pow2__div_pow2_I_3
	.quad	_L_1+3568
	.quad	_div_pow2__div_pow2_I_2
	.quad	_L_1+3581
	.quad	_div_pow2__div_pow2_I_1
	.quad	_L_1+3594
	.quad	_div_pow2__div_pow2_I_0
	.quad	_L_1+3607
	.quad	_div_pow2__div_pow2_u64_63
	.quad	_L_1+3620
	.quad	_div_pow2__div_pow2_u64_62
	.quad	_L_1+3636
	.quad	_div_pow2__div_pow2_u64_61
	.quad	_L_1+3652
	.quad	_div_pow2__div_pow2_u64_60
	.quad	_L_1+3668
	.quad	_div_pow2__div_pow2_u64_59
	.quad	_L_1+3684
	.quad	_div_pow2__div_pow2_u64_58
	.quad	_L_1+3700
	.quad	_div_pow2__div_pow2_u64_57
	.quad	_L_1+3716
	.quad	_div_pow2__div_pow2_u64_56
	.quad	_L_1+3732
	.quad	_div_pow2__div_pow2_u64_55
	.quad	_L_1+3748
	.quad	_div_pow2__div_pow2_u64_54
	.quad	_L_1+3764
	.quad	_div_pow2__div_pow2_u64_53
	.quad	_L_1+3780
	.quad	_div_pow2__div_pow2_u64_52
	.quad	_L_1+3796
	.quad	_div_pow2__div_pow2_u64_51
	.quad	_L_1+3812
	.quad	_div_pow2__div_pow2_u64_50
	.quad	_L_1+3828
	.quad	_div_pow2__div_pow2_u64_49
	.quad	_L_1+3844
	.quad	_div_pow2__div_pow2_u64_48
	.quad	_L_1+3860
	.quad	_div_pow2__div_pow2_u64_47
	.quad	_L_1+3876
	.quad	_div_pow2__div_pow2_u64_46
	.quad	_L_1+3892
	.quad	_div_pow2__div_pow2_u64_45
	.quad	_L_1+3908
	.quad	_div_pow2__div_pow2_u64_44
	.quad	_L_1+3924
	.quad	_div_pow2__div_pow2_u64_43
	.quad	_L_1+3940
	.quad	_div_pow2__div_pow2_u64_42
	.quad	_L_1+3956
	.quad	_div_pow2__div_pow2_u64_41
	.quad	_L_1+3972
	.quad	_div_pow2__div_pow2_u64_40
	.quad	_L_1+3988
	.quad	_div_pow2__div_pow2_u64_39
	.quad	_L_1+4004
	.quad	_div_pow2__div_pow2_u64_38
	.quad	_L_1+4020
	.quad	_div_pow2__div_pow2_u64_37
	.quad	_L_1+4036
	.quad	_div_pow2__div_pow2_u64_36
	.quad	_L_1+4052
	.quad	_div_pow2__div_pow2_u64_35
	.quad	_L_1+4068
	.quad	_div_pow2__div_pow2_u64_34
	.quad	_L_1+4084
	.quad	_div_pow2__div_pow2_u64_33
	.quad	_L_1+4100
	.quad	_div_pow2__div_pow2_u64_32
	.quad	_L_1+4116
	.quad	_div_pow2__div_pow2_u64_31
	.quad	_L_1+4132
	.quad	_div_pow2__div_pow2_u64_30
	.quad	_L_1+4148
	.quad	_div_pow2__div_pow2_u64_29
	.quad	_L_1+4164
	.quad	_div_pow2__div_pow2_u64_28
	.quad	_L_1+4180
	.quad	_div_pow2__div_pow2_u64_27
	.quad	_L_1+4196
	.quad	_div_pow2__div_pow2_u64_26
	.quad	_L_1+4212
	.quad	_div_pow2__div_pow2_u64_25
	.quad	_L_1+4228
	.quad	_div_pow2__div_pow2_u64_24
	.quad	_L_1+4244
	.quad	_div_pow2__div_pow2_u64_23
	.quad	_L_1+4260
	.quad	_div_pow2__div_pow2_u64_22
	.quad	_L_1+4276
	.quad	_div_pow2__div_pow2_u64_21
	.quad	_L_1+4292
	.quad	_div_pow2__div_pow2_u64_20
	.quad	_L_1+4308
	.quad	_div_pow2__div_pow2_u64_19
	.quad	_L_1+4324
	.quad	_div_pow2__div_pow2_u64_18
	.quad	_L_1+4340
	.quad	_div_pow2__div_pow2_u64_17
	.quad	_L_1+4356
	.quad	_div_pow2__div_pow2_u64_16
	.quad	_L_1+4372
	.quad	_div_pow2__div_pow2_u64_15
	.quad	_L_1+4388
	.quad	_div_pow2__div_pow2_u64_14
	.quad	_L_1+4404
	.quad	_div_pow2__div_pow2_u64_13
	.quad	_L_1+4420
	.quad	_div_pow2__div_pow2_u64_12
	.quad	_L_1+4436
	.quad	_div_pow2__div_pow2_u64_11
	.quad	_L_1+4452
	.quad	_div_pow2__div_pow2_u64_10
	.quad	_L_1+4468
	.quad	_div_pow2__div_pow2_u64_9
	.quad	_L_1+4484
	.quad	_div_pow2__div_pow2_u64_8
	.quad	_L_1+4499
	.quad	_div_pow2__div_pow2_u64_7
	.quad	_L_1+4514
	.quad	_div_pow2__div_pow2_u64_6
	.quad	_L_1+4529
	.quad	_div_pow2__div_pow2_u64_5
	.quad	_L_1+4544
	.quad	_div_pow2__div_pow2_u64_4
	.quad	_L_1+4559
	.quad	_div_pow2__div_pow2_u64_3
	.quad	_L_1+4574
	.quad	_div_pow2__div_pow2_u64_2
	.quad	_L_1+4589
	.quad	_div_pow2__div_pow2_u64_1
	.quad	_L_1+4604
	.quad	_div_pow2__div_pow2_u64_0
	.quad	_L_1+4619
	.quad	_div_pow2__div_pow2_i64_63
	.quad	_L_1+4634
	.quad	_div_pow2__div_pow2_i64_62
	.quad	_L_1+4650
	.quad	_div_pow2__div_pow2_i64_61
	.quad	_L_1+4666
	.quad	_div_pow2__div_pow2_i64_60
	.quad	_L_1+4682
	.quad	_div_pow2__div_pow2_i64_59
	.quad	_L_1+4698
	.quad	_div_pow2__div_pow2_i64_58
	.quad	_L_1+4714
	.quad	_div_pow2__div_pow2_i64_57
	.quad	_L_1+4730
	.quad	_div_pow2__div_pow2_i64_56
	.quad	_L_1+4746
	.quad	_div_pow2__div_pow2_i64_55
	.quad	_L_1+4762
	.quad	_div_pow2__div_pow2_i64_54
	.quad	_L_1+4778
	.quad	_div_pow2__div_pow2_i64_53
	.quad	_L_1+4794
	.quad	_div_pow2__div_pow2_i64_52
	.quad	_L_1+4810
	.quad	_div_pow2__div_pow2_i64_51
	.quad	_L_1+4826
	.quad	_div_pow2__div_pow2_i64_50
	.quad	_L_1+4842
	.quad	_div_pow2__div_pow2_i64_49
	.quad	_L_1+4858
	.quad	_div_pow2__div_pow2_i64_48
	.quad	_L_1+4874
	.quad	_div_pow2__div_pow2_i64_47
	.quad	_L_1+4890
	.quad	_div_pow2__div_pow2_i64_46
	.quad	_L_1+4906
	.quad	_div_pow2__div_pow2_i64_45
	.quad	_L_1+4922
	.quad	_div_pow2__div_pow2_i64_44
	.quad	_L_1+4938
	.quad	_div_pow2__div_pow2_i64_43
	.quad	_L_1+4954
	.quad	_div_pow2__div_pow2_i64_42
	.quad	_L_1+4970
	.quad	_div_pow2__div_pow2_i64_41
	.quad	_L_1+4986
	.quad	_div_pow2__div_pow2_i64_40
	.quad	_L_1+5002
	.quad	_div_pow2__div_pow2_i64_39
	.quad	_L_1+5018
	.quad	_div_pow2__div_pow2_i64_38
	.quad	_L_1+5034
	.quad	_div_pow2__div_pow2_i64_37
	.quad	_L_1+5050
	.quad	_div_pow2__div_pow2_i64_36
	.quad	_L_1+5066
	.quad	_div_pow2__div_pow2_i64_35
	.quad	_L_1+5082
	.quad	_div_pow2__div_pow2_i64_34
	.quad	_L_1+5098
	.quad	_div_pow2__div_pow2_i64_33
	.quad	_L_1+5114
	.quad	_div_pow2__div_pow2_i64_32
	.quad	_L_1+5130
	.quad	_div_pow2__div_pow2_i64_31
	.quad	_L_1+5146
	.quad	_div_pow2__div_pow2_i64_30
	.quad	_L_1+5162
	.quad	_div_pow2__div_pow2_i64_29
	.quad	_L_1+5178
	.quad	_div_pow2__div_pow2_i64_28
	.quad	_L_1+5194
	.quad	_div_pow2__div_pow2_i64_27
	.quad	_L_1+5210
	.quad	_div_pow2__div_pow2_i64_26
	.quad	_L_1+5226
	.quad	_div_pow2__div_pow2_i64_25
	.quad	_L_1+5242
	.quad	_div_pow2__div_pow2_i64_24
	.quad	_L_1+5258
	.quad	_div_pow2__div_pow2_i64_23
	.quad	_L_1+5274
	.quad	_div_pow2__div_pow2_i64_22
	.quad	_L_1+5290
	.quad	_div_pow2__div_pow2_i64_21
	.quad	_L_1+5306
	.quad	_div_pow2__div_pow2_i64_20
	.quad	_L_1+5322
	.quad	_div_pow2__div_pow2_i64_19
	.quad	_L_1+5338
	.quad	_div_pow2__div_pow2_i64_18
	.quad	_L_1+5354
	.quad	_div_pow2__div_pow2_i64_17
	.quad	_L_1+5370
	.quad	_div_pow2__div_pow2_i64_16
	.quad	_L_1+5386
	.quad	_div_pow2__div_pow2_i64_15
	.quad	_L_1+5402
	.quad	_div_pow2__div_pow2_i64_14
	.quad	_L_1+5418
	.quad	_div_pow2__div_pow2_i64_13
	.quad	_L_1+5434
	.quad	_div_pow2__div_pow2_i64_12
	.quad	_L_1+5450
	.quad	_div_pow2__div_pow2_i64_11
	.quad	_L_1+5466
	.quad	_div_pow2__div_pow2_i64_10
	.quad	_L_1+5482
	.quad	_div_pow2__div_pow2_i64_9
	.quad	_L_1+5498
	.quad	_div_pow2__div_pow2_i64_8
	.quad	_L_1+5513
	.quad	_div_pow2__div_pow2_i64_7
	.quad	_L_1+5528
	.quad	_div_pow2__div_pow2_i64_6
	.quad	_L_1+5543
	.quad	_div_pow2__div_pow2_i64_5
	.quad	_L_1+5558
	.quad	_div_pow2__div_pow2_i64_4
	.quad	_L_1+5573
	.quad	_div_pow2__div_pow2_i64_3
	.quad	_L_1+5588
	.quad	_div_pow2__div_pow2_i64_2
	.quad	_L_1+5603
	.quad	_div_pow2__div_pow2_i64_1
	.quad	_L_1+5618
	.quad	_div_pow2__div_pow2_i64_0
	.quad	_L_1+5633
	.quad	_div_pow2__div_pow2_u32_31
	.quad	_L_1+5648
	.quad	_div_pow2__div_pow2_u32_30
	.quad	_L_1+5664
	.quad	_div_pow2__div_pow2_u32_29
	.quad	_L_1+5680
	.quad	_div_pow2__div_pow2_u32_28
	.quad	_L_1+5696
	.quad	_div_pow2__div_pow2_u32_27
	.quad	_L_1+5712
	.quad	_div_pow2__div_pow2_u32_26
	.quad	_L_1+5728
	.quad	_div_pow2__div_pow2_u32_25
	.quad	_L_1+5744
	.quad	_div_pow2__div_pow2_u32_24
	.quad	_L_1+5760
	.quad	_div_pow2__div_pow2_u32_23
	.quad	_L_1+5776
	.quad	_div_pow2__div_pow2_u32_22
	.quad	_L_1+5792
	.quad	_div_pow2__div_pow2_u32_21
	.quad	_L_1+5808
	.quad	_div_pow2__div_pow2_u32_20
	.quad	_L_1+5824
	.quad	_div_pow2__div_pow2_u32_19
	.quad	_L_1+5840
	.quad	_div_pow2__div_pow2_u32_18
	.quad	_L_1+5856
	.quad	_div_pow2__div_pow2_u32_17
	.quad	_L_1+5872
	.quad	_div_pow2__div_pow2_u32_16
	.quad	_L_1+5888
	.quad	_div_pow2__div_pow2_u32_15
	.quad	_L_1+5904
	.quad	_div_pow2__div_pow2_u32_14
	.quad	_L_1+5920
	.quad	_div_pow2__div_pow2_u32_13
	.quad	_L_1+5936
	.quad	_div_pow2__div_pow2_u32_12
	.quad	_L_1+5952
	.quad	_div_pow2__div_pow2_u32_11
	.quad	_L_1+5968
	.quad	_div_pow2__div_pow2_u32_10
	.quad	_L_1+5984
	.quad	_div_pow2__div_pow2_u32_9
	.quad	_L_1+6000
	.quad	_div_pow2__div_pow2_u32_8
	.quad	_L_1+6015
	.quad	_div_pow2__div_pow2_u32_7
	.quad	_L_1+6030
	.quad	_div_pow2__div_pow2_u32_6
	.quad	_L_1+6045
	.quad	_div_pow2__div_pow2_u32_5
	.quad	_L_1+6060
	.quad	_div_pow2__div_pow2_u32_4
	.quad	_L_1+6075
	.quad	_div_pow2__div_pow2_u32_3
	.quad	_L_1+6090
	.quad	_div_pow2__div_pow2_u32_2
	.quad	_L_1+6105
	.quad	_div_pow2__div_pow2_u32_1
	.quad	_L_1+6120
	.quad	_div_pow2__div_pow2_u32_0
	.quad	_L_1+6135
	.quad	_div_pow2__div_pow2_i32_31
	.quad	_L_1+6150
	.quad	_div_pow2__div_pow2_i32_30
	.quad	_L_1+6166
	.quad	_div_pow2__div_pow2_i32_29
	.quad	_L_1+6182
	.quad	_div_pow2__div_pow2_i32_28
	.quad	_L_1+6198
	.quad	_div_pow2__div_pow2_i32_27
	.quad	_L_1+6214
	.quad	_div_pow2__div_pow2_i32_26
	.quad	_L_1+6230
	.quad	_div_pow2__div_pow2_i32_25
	.quad	_L_1+6246
	.quad	_div_pow2__div_pow2_i32_24
	.quad	_L_1+6262
	.quad	_div_pow2__div_pow2_i32_23
	.quad	_L_1+6278
	.quad	_div_pow2__div_pow2_i32_22
	.quad	_L_1+6294
	.quad	_div_pow2__div_pow2_i32_21
	.quad	_L_1+6310
	.quad	_div_pow2__div_pow2_i32_20
	.quad	_L_1+6326
	.quad	_div_pow2__div_pow2_i32_19
	.quad	_L_1+6342
	.quad	_div_pow2__div_pow2_i32_18
	.quad	_L_1+6358
	.quad	_div_pow2__div_pow2_i32_17
	.quad	_L_1+6374
	.quad	_div_pow2__div_pow2_i32_16
	.quad	_L_1+6390
	.quad	_div_pow2__div_pow2_i32_15
	.quad	_L_1+6406
	.quad	_div_pow2__div_pow2_i32_14
	.quad	_L_1+6422
	.quad	_div_pow2__div_pow2_i32_13
	.quad	_L_1+6438
	.quad	_div_pow2__div_pow2_i32_12
	.quad	_L_1+6454
	.quad	_div_pow2__div_pow2_i32_11
	.quad	_L_1+6470
	.quad	_div_pow2__div_pow2_i32_10
	.quad	_L_1+6486
	.quad	_div_pow2__div_pow2_i32_9
	.quad	_L_1+6502
	.quad	_div_pow2__div_pow2_i32_8
	.quad	_L_1+6517
	.quad	_div_pow2__div_pow2_i32_7
	.quad	_L_1+6532
	.quad	_div_pow2__div_pow2_i32_6
	.quad	_L_1+6547
	.quad	_div_pow2__div_pow2_i32_5
	.quad	_L_1+6562
	.quad	_div_pow2__div_pow2_i32_4
	.quad	_L_1+6577
	.quad	_div_pow2__div_pow2_i32_3
	.quad	_L_1+6592
	.quad	_div_pow2__div_pow2_i32_2
	.quad	_L_1+6607
	.quad	_div_pow2__div_pow2_i32_1
	.quad	_L_1+6622
	.quad	_div_pow2__div_pow2_i32_0
	.quad	_L_1+6637
	.space 8
	.ascii "../AMD64_DARWIN/div_pow2.m3"
	.space 5
	.data
	.align 5
_MM_div_pow2:
	.quad	_L_1+13848
	.space 32
	.quad	_L_1+6656
	.space 24
	.quad	_MM_div_pow2+224
	.space 8
	.quad	_div_pow2_M3
	.quad	3
	.word	913
	.space 2
	.long	914
	.quad	915
	.quad	916
	.word	917
	.space 6
	.quad	918
	.long	687194767
	.long	1082965852
	.long	921
	.space 4
	.quad	922
	.byte	-101
	.space 7
	.quad	924
	.long	1147632452
	.space 4
	.quad	927
	.byte	-96
	.space 31
	.quad	_div_pow2_I3
	.quad	_MM_div_pow2+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_div_pow2+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_div_pow2+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_div_pow2+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_div_pow2__div_pow2_i32_0.eh = 0
.no_dead_strip _div_pow2__div_pow2_i32_0.eh
_div_pow2__div_pow2_i64_0.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_0.eh
_div_pow2__div_pow2_i64_1.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_1.eh
_div_pow2__div_pow2_i64_2.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_2.eh
_div_pow2__div_pow2_i64_3.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_3.eh
_div_pow2__div_pow2_i64_4.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_4.eh
_div_pow2__div_pow2_i64_5.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_5.eh
_div_pow2__div_pow2_i64_6.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_6.eh
_div_pow2__div_pow2_i64_7.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_7.eh
_div_pow2__div_pow2_i64_8.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_8.eh
_div_pow2__div_pow2_i64_9.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_9.eh
_div_pow2__div_pow2_i64_10.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_10.eh
_div_pow2__div_pow2_i64_11.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_11.eh
_div_pow2__div_pow2_i64_12.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_12.eh
_div_pow2__div_pow2_i64_13.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_13.eh
_div_pow2__div_pow2_i64_14.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_14.eh
_div_pow2__div_pow2_i64_15.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_15.eh
_div_pow2__div_pow2_i64_16.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_16.eh
_div_pow2__div_pow2_i64_17.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_17.eh
_div_pow2__div_pow2_i64_18.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_18.eh
_div_pow2__div_pow2_i64_19.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_19.eh
_div_pow2__div_pow2_i64_20.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_20.eh
_div_pow2__div_pow2_i64_21.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_21.eh
_div_pow2__div_pow2_i64_22.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_22.eh
_div_pow2__div_pow2_i64_23.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_23.eh
_div_pow2__div_pow2_i64_24.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_24.eh
_div_pow2__div_pow2_i64_25.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_25.eh
_div_pow2__div_pow2_i64_26.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_26.eh
_div_pow2__div_pow2_i64_27.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_27.eh
_div_pow2__div_pow2_i64_28.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_28.eh
_div_pow2__div_pow2_i64_29.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_29.eh
_div_pow2__div_pow2_i64_30.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_30.eh
_div_pow2__div_pow2_i64_31.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_31.eh
_div_pow2__div_pow2_i64_32.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_32.eh
_div_pow2__div_pow2_i64_33.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_33.eh
_div_pow2__div_pow2_i64_34.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_34.eh
_div_pow2__div_pow2_i64_35.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_35.eh
_div_pow2__div_pow2_i64_36.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_36.eh
_div_pow2__div_pow2_i64_37.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_37.eh
_div_pow2__div_pow2_i64_38.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_38.eh
_div_pow2__div_pow2_i64_39.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_39.eh
_div_pow2__div_pow2_i64_40.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_40.eh
_div_pow2__div_pow2_i64_41.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_41.eh
_div_pow2__div_pow2_i64_42.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_42.eh
_div_pow2__div_pow2_i64_43.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_43.eh
_div_pow2__div_pow2_i64_44.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_44.eh
_div_pow2__div_pow2_i64_45.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_45.eh
_div_pow2__div_pow2_i64_46.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_46.eh
_div_pow2__div_pow2_i64_47.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_47.eh
_div_pow2__div_pow2_i64_48.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_48.eh
_div_pow2__div_pow2_i64_49.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_49.eh
_div_pow2__div_pow2_i64_50.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_50.eh
_div_pow2__div_pow2_i64_51.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_51.eh
_div_pow2__div_pow2_i64_52.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_52.eh
_div_pow2__div_pow2_i64_53.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_53.eh
_div_pow2__div_pow2_i64_54.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_54.eh
_div_pow2__div_pow2_i64_55.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_55.eh
_div_pow2__div_pow2_i64_56.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_56.eh
_div_pow2__div_pow2_i64_57.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_57.eh
_div_pow2__div_pow2_i64_58.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_58.eh
_div_pow2__div_pow2_i64_59.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_59.eh
_div_pow2__div_pow2_i64_60.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_60.eh
_div_pow2__div_pow2_i64_61.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_61.eh
_div_pow2__div_pow2_i64_62.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_62.eh
_div_pow2__div_pow2_i64_63.eh = 0
.no_dead_strip _div_pow2__div_pow2_i64_63.eh
_div_pow2__div_pow2_u64_0.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_0.eh
_div_pow2__div_pow2_u64_1.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_1.eh
_div_pow2__div_pow2_u64_2.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_2.eh
_div_pow2__div_pow2_u64_3.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_3.eh
_div_pow2__div_pow2_u64_4.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_4.eh
_div_pow2__div_pow2_u64_5.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_5.eh
_div_pow2__div_pow2_u64_6.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_6.eh
_div_pow2__div_pow2_u64_7.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_7.eh
_div_pow2__div_pow2_u64_8.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_8.eh
_div_pow2__div_pow2_u64_9.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_9.eh
_div_pow2__div_pow2_u64_10.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_10.eh
_div_pow2__div_pow2_u64_11.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_11.eh
_div_pow2__div_pow2_u64_12.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_12.eh
_div_pow2__div_pow2_u64_13.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_13.eh
_div_pow2__div_pow2_u64_14.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_14.eh
_div_pow2__div_pow2_u64_15.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_15.eh
_div_pow2__div_pow2_u64_16.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_16.eh
_div_pow2__div_pow2_u64_17.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_17.eh
_div_pow2__div_pow2_u64_18.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_18.eh
_div_pow2__div_pow2_u64_19.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_19.eh
_div_pow2__div_pow2_u64_20.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_20.eh
_div_pow2__div_pow2_u64_21.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_21.eh
_div_pow2__div_pow2_u64_22.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_22.eh
_div_pow2__div_pow2_u64_23.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_23.eh
_div_pow2__div_pow2_u64_24.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_24.eh
_div_pow2__div_pow2_u64_25.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_25.eh
_div_pow2__div_pow2_u64_26.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_26.eh
_div_pow2__div_pow2_u64_27.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_27.eh
_div_pow2__div_pow2_u64_28.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_28.eh
_div_pow2__div_pow2_u64_29.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_29.eh
_div_pow2__div_pow2_u64_30.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_30.eh
_div_pow2__div_pow2_u64_31.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_31.eh
_div_pow2__div_pow2_u64_32.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_32.eh
_div_pow2__div_pow2_u64_33.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_33.eh
_div_pow2__div_pow2_u64_34.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_34.eh
_div_pow2__div_pow2_u64_35.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_35.eh
_div_pow2__div_pow2_u64_36.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_36.eh
_div_pow2__div_pow2_u64_37.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_37.eh
_div_pow2__div_pow2_u64_38.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_38.eh
_div_pow2__div_pow2_u64_39.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_39.eh
_div_pow2__div_pow2_u64_40.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_40.eh
_div_pow2__div_pow2_u64_41.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_41.eh
_div_pow2__div_pow2_u64_42.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_42.eh
_div_pow2__div_pow2_u64_43.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_43.eh
_div_pow2__div_pow2_u64_44.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_44.eh
_div_pow2__div_pow2_u64_45.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_45.eh
_div_pow2__div_pow2_u64_46.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_46.eh
_div_pow2__div_pow2_u64_47.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_47.eh
_div_pow2__div_pow2_u64_48.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_48.eh
_div_pow2__div_pow2_u64_49.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_49.eh
_div_pow2__div_pow2_u64_50.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_50.eh
_div_pow2__div_pow2_u64_51.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_51.eh
_div_pow2__div_pow2_u64_52.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_52.eh
_div_pow2__div_pow2_u64_53.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_53.eh
_div_pow2__div_pow2_u64_54.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_54.eh
_div_pow2__div_pow2_u64_55.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_55.eh
_div_pow2__div_pow2_u64_56.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_56.eh
_div_pow2__div_pow2_u64_57.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_57.eh
_div_pow2__div_pow2_u64_58.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_58.eh
_div_pow2__div_pow2_u64_59.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_59.eh
_div_pow2__div_pow2_u64_60.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_60.eh
_div_pow2__div_pow2_u64_61.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_61.eh
_div_pow2__div_pow2_u64_62.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_62.eh
_div_pow2__div_pow2_u64_63.eh = 0
.no_dead_strip _div_pow2__div_pow2_u64_63.eh
_div_pow2__div_pow2_I_0.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_0.eh
_div_pow2__div_pow2_I_1.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_1.eh
_div_pow2__div_pow2_I_2.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_2.eh
_div_pow2__div_pow2_I_3.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_3.eh
_div_pow2__div_pow2_I_4.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_4.eh
_div_pow2__div_pow2_I_5.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_5.eh
_div_pow2__div_pow2_I_6.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_6.eh
_div_pow2__div_pow2_I_7.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_7.eh
_div_pow2__div_pow2_I_8.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_8.eh
_div_pow2__div_pow2_I_9.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_9.eh
_div_pow2__div_pow2_I_10.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_10.eh
_div_pow2__div_pow2_I_11.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_11.eh
_div_pow2__div_pow2_I_12.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_12.eh
_div_pow2__div_pow2_I_13.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_13.eh
_div_pow2__div_pow2_I_14.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_14.eh
_div_pow2__div_pow2_I_15.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_15.eh
_div_pow2__div_pow2_I_16.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_16.eh
_div_pow2__div_pow2_I_17.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_17.eh
_div_pow2__div_pow2_I_18.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_18.eh
_div_pow2__div_pow2_I_19.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_19.eh
_div_pow2__div_pow2_I_20.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_20.eh
_div_pow2__div_pow2_I_21.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_21.eh
_div_pow2__div_pow2_I_22.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_22.eh
_div_pow2__div_pow2_I_23.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_23.eh
_div_pow2__div_pow2_I_24.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_24.eh
_div_pow2__div_pow2_I_25.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_25.eh
_div_pow2__div_pow2_I_26.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_26.eh
_div_pow2__div_pow2_I_27.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_27.eh
_div_pow2__div_pow2_I_28.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_28.eh
_div_pow2__div_pow2_I_29.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_29.eh
_div_pow2__div_pow2_I_30.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_30.eh
_div_pow2__div_pow2_I_31.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_31.eh
_div_pow2__div_pow2_I_32.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_32.eh
_div_pow2__div_pow2_I_33.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_33.eh
_div_pow2__div_pow2_I_34.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_34.eh
_div_pow2__div_pow2_I_35.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_35.eh
_div_pow2__div_pow2_I_36.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_36.eh
_div_pow2__div_pow2_I_37.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_37.eh
_div_pow2__div_pow2_I_38.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_38.eh
_div_pow2__div_pow2_I_39.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_39.eh
_div_pow2__div_pow2_I_40.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_40.eh
_div_pow2__div_pow2_I_41.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_41.eh
_div_pow2__div_pow2_I_42.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_42.eh
_div_pow2__div_pow2_I_43.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_43.eh
_div_pow2__div_pow2_I_44.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_44.eh
_div_pow2__div_pow2_I_45.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_45.eh
_div_pow2__div_pow2_I_46.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_46.eh
_div_pow2__div_pow2_I_47.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_47.eh
_div_pow2__div_pow2_I_48.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_48.eh
_div_pow2__div_pow2_I_49.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_49.eh
_div_pow2__div_pow2_I_50.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_50.eh
_div_pow2__div_pow2_I_51.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_51.eh
_div_pow2__div_pow2_I_52.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_52.eh
_div_pow2__div_pow2_I_53.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_53.eh
_div_pow2__div_pow2_I_54.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_54.eh
_div_pow2__div_pow2_I_55.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_55.eh
_div_pow2__div_pow2_I_56.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_56.eh
_div_pow2__div_pow2_I_57.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_57.eh
_div_pow2__div_pow2_I_58.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_58.eh
_div_pow2__div_pow2_I_59.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_59.eh
_div_pow2__div_pow2_I_60.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_60.eh
_div_pow2__div_pow2_I_61.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_61.eh
_div_pow2__div_pow2_I_62.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_62.eh
_div_pow2__div_pow2_I_63.eh = 0
.no_dead_strip _div_pow2__div_pow2_I_63.eh
_div_pow2__div_pow2_L_0.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_0.eh
_div_pow2__div_pow2_L_1.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_1.eh
_div_pow2__div_pow2_L_2.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_2.eh
_div_pow2__div_pow2_L_3.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_3.eh
_div_pow2__div_pow2_L_4.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_4.eh
_div_pow2__div_pow2_L_5.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_5.eh
_div_pow2__div_pow2_L_6.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_6.eh
_div_pow2__div_pow2_L_7.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_7.eh
_div_pow2__div_pow2_L_8.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_8.eh
_div_pow2__div_pow2_L_9.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_9.eh
_div_pow2__div_pow2_L_10.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_10.eh
_div_pow2__div_pow2_L_11.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_11.eh
_div_pow2__div_pow2_L_12.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_12.eh
_div_pow2__div_pow2_L_13.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_13.eh
_div_pow2__div_pow2_L_14.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_14.eh
_div_pow2__div_pow2_L_15.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_15.eh
_div_pow2__div_pow2_L_16.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_16.eh
_div_pow2__div_pow2_L_17.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_17.eh
_div_pow2__div_pow2_L_18.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_18.eh
_div_pow2__div_pow2_L_19.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_19.eh
_div_pow2__div_pow2_L_20.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_20.eh
_div_pow2__div_pow2_L_21.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_21.eh
_div_pow2__div_pow2_L_22.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_22.eh
_div_pow2__div_pow2_L_23.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_23.eh
_div_pow2__div_pow2_L_24.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_24.eh
_div_pow2__div_pow2_L_25.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_25.eh
_div_pow2__div_pow2_L_26.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_26.eh
_div_pow2__div_pow2_L_27.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_27.eh
_div_pow2__div_pow2_L_28.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_28.eh
_div_pow2__div_pow2_L_29.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_29.eh
_div_pow2__div_pow2_L_30.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_30.eh
_div_pow2__div_pow2_L_31.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_31.eh
_div_pow2__div_pow2_L_32.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_32.eh
_div_pow2__div_pow2_L_33.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_33.eh
_div_pow2__div_pow2_L_34.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_34.eh
_div_pow2__div_pow2_L_35.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_35.eh
_div_pow2__div_pow2_L_36.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_36.eh
_div_pow2__div_pow2_L_37.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_37.eh
_div_pow2__div_pow2_L_38.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_38.eh
_div_pow2__div_pow2_L_39.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_39.eh
_div_pow2__div_pow2_L_40.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_40.eh
_div_pow2__div_pow2_L_41.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_41.eh
_div_pow2__div_pow2_L_42.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_42.eh
_div_pow2__div_pow2_L_43.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_43.eh
_div_pow2__div_pow2_L_44.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_44.eh
_div_pow2__div_pow2_L_45.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_45.eh
_div_pow2__div_pow2_L_46.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_46.eh
_div_pow2__div_pow2_L_47.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_47.eh
_div_pow2__div_pow2_L_48.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_48.eh
_div_pow2__div_pow2_L_49.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_49.eh
_div_pow2__div_pow2_L_50.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_50.eh
_div_pow2__div_pow2_L_51.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_51.eh
_div_pow2__div_pow2_L_52.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_52.eh
_div_pow2__div_pow2_L_53.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_53.eh
_div_pow2__div_pow2_L_54.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_54.eh
_div_pow2__div_pow2_L_55.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_55.eh
_div_pow2__div_pow2_L_56.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_56.eh
_div_pow2__div_pow2_L_57.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_57.eh
_div_pow2__div_pow2_L_58.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_58.eh
_div_pow2__div_pow2_L_59.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_59.eh
_div_pow2__div_pow2_L_60.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_60.eh
_div_pow2__div_pow2_L_61.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_61.eh
_div_pow2__div_pow2_L_62.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_62.eh
_div_pow2__div_pow2_L_63.eh = 0
.no_dead_strip _div_pow2__div_pow2_L_63.eh
.globl _div_pow2_M3.eh
_div_pow2_M3.eh = 0
.no_dead_strip _div_pow2_M3.eh
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
_div_pow2__div_pow2_i32_1.eh:
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
	.set L$set$5,LCFI6-LCFI4
	.long L$set$5
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE1:
_div_pow2__div_pow2_i32_2.eh:
LSFDE3:
	.set L$set$6,LEFDE3-LASFDE3
	.long L$set$6
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$7,LFE2-LFB2
	.quad L$set$7
	.byte	0x0
	.byte	0x4
	.set L$set$8,LCFI7-LFB2
	.long L$set$8
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$9,LCFI8-LCFI7
	.long L$set$9
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$10,LCFI10-LCFI8
	.long L$set$10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE3:
_div_pow2__div_pow2_i32_3.eh:
LSFDE5:
	.set L$set$11,LEFDE5-LASFDE5
	.long L$set$11
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$12,LFE3-LFB3
	.quad L$set$12
	.byte	0x0
	.byte	0x4
	.set L$set$13,LCFI11-LFB3
	.long L$set$13
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$14,LCFI12-LCFI11
	.long L$set$14
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$15,LCFI14-LCFI12
	.long L$set$15
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE5:
_div_pow2__div_pow2_i32_4.eh:
LSFDE7:
	.set L$set$16,LEFDE7-LASFDE7
	.long L$set$16
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$17,LFE4-LFB4
	.quad L$set$17
	.byte	0x0
	.byte	0x4
	.set L$set$18,LCFI15-LFB4
	.long L$set$18
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$19,LCFI16-LCFI15
	.long L$set$19
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$20,LCFI18-LCFI16
	.long L$set$20
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE7:
_div_pow2__div_pow2_i32_5.eh:
LSFDE9:
	.set L$set$21,LEFDE9-LASFDE9
	.long L$set$21
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$22,LFE5-LFB5
	.quad L$set$22
	.byte	0x0
	.byte	0x4
	.set L$set$23,LCFI19-LFB5
	.long L$set$23
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$24,LCFI20-LCFI19
	.long L$set$24
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$25,LCFI22-LCFI20
	.long L$set$25
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE9:
_div_pow2__div_pow2_i32_6.eh:
LSFDE11:
	.set L$set$26,LEFDE11-LASFDE11
	.long L$set$26
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$27,LFE6-LFB6
	.quad L$set$27
	.byte	0x0
	.byte	0x4
	.set L$set$28,LCFI23-LFB6
	.long L$set$28
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$29,LCFI24-LCFI23
	.long L$set$29
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$30,LCFI26-LCFI24
	.long L$set$30
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE11:
_div_pow2__div_pow2_i32_7.eh:
LSFDE13:
	.set L$set$31,LEFDE13-LASFDE13
	.long L$set$31
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB7-.
	.set L$set$32,LFE7-LFB7
	.quad L$set$32
	.byte	0x0
	.byte	0x4
	.set L$set$33,LCFI27-LFB7
	.long L$set$33
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$34,LCFI28-LCFI27
	.long L$set$34
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$35,LCFI30-LCFI28
	.long L$set$35
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE13:
_div_pow2__div_pow2_i32_8.eh:
LSFDE15:
	.set L$set$36,LEFDE15-LASFDE15
	.long L$set$36
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$37,LFE8-LFB8
	.quad L$set$37
	.byte	0x0
	.byte	0x4
	.set L$set$38,LCFI31-LFB8
	.long L$set$38
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$39,LCFI32-LCFI31
	.long L$set$39
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$40,LCFI34-LCFI32
	.long L$set$40
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE15:
_div_pow2__div_pow2_i32_9.eh:
LSFDE17:
	.set L$set$41,LEFDE17-LASFDE17
	.long L$set$41
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB9-.
	.set L$set$42,LFE9-LFB9
	.quad L$set$42
	.byte	0x0
	.byte	0x4
	.set L$set$43,LCFI35-LFB9
	.long L$set$43
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$44,LCFI36-LCFI35
	.long L$set$44
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$45,LCFI38-LCFI36
	.long L$set$45
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE17:
_div_pow2__div_pow2_i32_10.eh:
LSFDE19:
	.set L$set$46,LEFDE19-LASFDE19
	.long L$set$46
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB10-.
	.set L$set$47,LFE10-LFB10
	.quad L$set$47
	.byte	0x0
	.byte	0x4
	.set L$set$48,LCFI39-LFB10
	.long L$set$48
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$49,LCFI40-LCFI39
	.long L$set$49
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$50,LCFI42-LCFI40
	.long L$set$50
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE19:
_div_pow2__div_pow2_i32_11.eh:
LSFDE21:
	.set L$set$51,LEFDE21-LASFDE21
	.long L$set$51
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB11-.
	.set L$set$52,LFE11-LFB11
	.quad L$set$52
	.byte	0x0
	.byte	0x4
	.set L$set$53,LCFI43-LFB11
	.long L$set$53
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$54,LCFI44-LCFI43
	.long L$set$54
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$55,LCFI46-LCFI44
	.long L$set$55
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE21:
_div_pow2__div_pow2_i32_12.eh:
LSFDE23:
	.set L$set$56,LEFDE23-LASFDE23
	.long L$set$56
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB12-.
	.set L$set$57,LFE12-LFB12
	.quad L$set$57
	.byte	0x0
	.byte	0x4
	.set L$set$58,LCFI47-LFB12
	.long L$set$58
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$59,LCFI48-LCFI47
	.long L$set$59
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$60,LCFI50-LCFI48
	.long L$set$60
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE23:
_div_pow2__div_pow2_i32_13.eh:
LSFDE25:
	.set L$set$61,LEFDE25-LASFDE25
	.long L$set$61
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB13-.
	.set L$set$62,LFE13-LFB13
	.quad L$set$62
	.byte	0x0
	.byte	0x4
	.set L$set$63,LCFI51-LFB13
	.long L$set$63
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$64,LCFI52-LCFI51
	.long L$set$64
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$65,LCFI54-LCFI52
	.long L$set$65
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE25:
_div_pow2__div_pow2_i32_14.eh:
LSFDE27:
	.set L$set$66,LEFDE27-LASFDE27
	.long L$set$66
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB14-.
	.set L$set$67,LFE14-LFB14
	.quad L$set$67
	.byte	0x0
	.byte	0x4
	.set L$set$68,LCFI55-LFB14
	.long L$set$68
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$69,LCFI56-LCFI55
	.long L$set$69
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$70,LCFI58-LCFI56
	.long L$set$70
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE27:
_div_pow2__div_pow2_i32_15.eh:
LSFDE29:
	.set L$set$71,LEFDE29-LASFDE29
	.long L$set$71
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB15-.
	.set L$set$72,LFE15-LFB15
	.quad L$set$72
	.byte	0x0
	.byte	0x4
	.set L$set$73,LCFI59-LFB15
	.long L$set$73
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$74,LCFI60-LCFI59
	.long L$set$74
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$75,LCFI62-LCFI60
	.long L$set$75
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE29:
_div_pow2__div_pow2_i32_16.eh:
LSFDE31:
	.set L$set$76,LEFDE31-LASFDE31
	.long L$set$76
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB16-.
	.set L$set$77,LFE16-LFB16
	.quad L$set$77
	.byte	0x0
	.byte	0x4
	.set L$set$78,LCFI63-LFB16
	.long L$set$78
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$79,LCFI64-LCFI63
	.long L$set$79
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$80,LCFI66-LCFI64
	.long L$set$80
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE31:
_div_pow2__div_pow2_i32_17.eh:
LSFDE33:
	.set L$set$81,LEFDE33-LASFDE33
	.long L$set$81
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB17-.
	.set L$set$82,LFE17-LFB17
	.quad L$set$82
	.byte	0x0
	.byte	0x4
	.set L$set$83,LCFI67-LFB17
	.long L$set$83
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$84,LCFI68-LCFI67
	.long L$set$84
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$85,LCFI70-LCFI68
	.long L$set$85
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE33:
_div_pow2__div_pow2_i32_18.eh:
LSFDE35:
	.set L$set$86,LEFDE35-LASFDE35
	.long L$set$86
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB18-.
	.set L$set$87,LFE18-LFB18
	.quad L$set$87
	.byte	0x0
	.byte	0x4
	.set L$set$88,LCFI71-LFB18
	.long L$set$88
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$89,LCFI72-LCFI71
	.long L$set$89
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$90,LCFI74-LCFI72
	.long L$set$90
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE35:
_div_pow2__div_pow2_i32_19.eh:
LSFDE37:
	.set L$set$91,LEFDE37-LASFDE37
	.long L$set$91
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB19-.
	.set L$set$92,LFE19-LFB19
	.quad L$set$92
	.byte	0x0
	.byte	0x4
	.set L$set$93,LCFI75-LFB19
	.long L$set$93
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$94,LCFI76-LCFI75
	.long L$set$94
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$95,LCFI78-LCFI76
	.long L$set$95
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE37:
_div_pow2__div_pow2_i32_20.eh:
LSFDE39:
	.set L$set$96,LEFDE39-LASFDE39
	.long L$set$96
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB20-.
	.set L$set$97,LFE20-LFB20
	.quad L$set$97
	.byte	0x0
	.byte	0x4
	.set L$set$98,LCFI79-LFB20
	.long L$set$98
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$99,LCFI80-LCFI79
	.long L$set$99
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$100,LCFI82-LCFI80
	.long L$set$100
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE39:
_div_pow2__div_pow2_i32_21.eh:
LSFDE41:
	.set L$set$101,LEFDE41-LASFDE41
	.long L$set$101
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB21-.
	.set L$set$102,LFE21-LFB21
	.quad L$set$102
	.byte	0x0
	.byte	0x4
	.set L$set$103,LCFI83-LFB21
	.long L$set$103
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$104,LCFI84-LCFI83
	.long L$set$104
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$105,LCFI86-LCFI84
	.long L$set$105
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE41:
_div_pow2__div_pow2_i32_22.eh:
LSFDE43:
	.set L$set$106,LEFDE43-LASFDE43
	.long L$set$106
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB22-.
	.set L$set$107,LFE22-LFB22
	.quad L$set$107
	.byte	0x0
	.byte	0x4
	.set L$set$108,LCFI87-LFB22
	.long L$set$108
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$109,LCFI88-LCFI87
	.long L$set$109
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$110,LCFI90-LCFI88
	.long L$set$110
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE43:
_div_pow2__div_pow2_i32_23.eh:
LSFDE45:
	.set L$set$111,LEFDE45-LASFDE45
	.long L$set$111
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB23-.
	.set L$set$112,LFE23-LFB23
	.quad L$set$112
	.byte	0x0
	.byte	0x4
	.set L$set$113,LCFI91-LFB23
	.long L$set$113
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$114,LCFI92-LCFI91
	.long L$set$114
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$115,LCFI94-LCFI92
	.long L$set$115
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE45:
_div_pow2__div_pow2_i32_24.eh:
LSFDE47:
	.set L$set$116,LEFDE47-LASFDE47
	.long L$set$116
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB24-.
	.set L$set$117,LFE24-LFB24
	.quad L$set$117
	.byte	0x0
	.byte	0x4
	.set L$set$118,LCFI95-LFB24
	.long L$set$118
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$119,LCFI96-LCFI95
	.long L$set$119
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$120,LCFI98-LCFI96
	.long L$set$120
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE47:
_div_pow2__div_pow2_i32_25.eh:
LSFDE49:
	.set L$set$121,LEFDE49-LASFDE49
	.long L$set$121
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB25-.
	.set L$set$122,LFE25-LFB25
	.quad L$set$122
	.byte	0x0
	.byte	0x4
	.set L$set$123,LCFI99-LFB25
	.long L$set$123
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$124,LCFI100-LCFI99
	.long L$set$124
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$125,LCFI102-LCFI100
	.long L$set$125
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE49:
_div_pow2__div_pow2_i32_26.eh:
LSFDE51:
	.set L$set$126,LEFDE51-LASFDE51
	.long L$set$126
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB26-.
	.set L$set$127,LFE26-LFB26
	.quad L$set$127
	.byte	0x0
	.byte	0x4
	.set L$set$128,LCFI103-LFB26
	.long L$set$128
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$129,LCFI104-LCFI103
	.long L$set$129
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$130,LCFI106-LCFI104
	.long L$set$130
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE51:
_div_pow2__div_pow2_i32_27.eh:
LSFDE53:
	.set L$set$131,LEFDE53-LASFDE53
	.long L$set$131
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB27-.
	.set L$set$132,LFE27-LFB27
	.quad L$set$132
	.byte	0x0
	.byte	0x4
	.set L$set$133,LCFI107-LFB27
	.long L$set$133
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$134,LCFI108-LCFI107
	.long L$set$134
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$135,LCFI110-LCFI108
	.long L$set$135
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE53:
_div_pow2__div_pow2_i32_28.eh:
LSFDE55:
	.set L$set$136,LEFDE55-LASFDE55
	.long L$set$136
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB28-.
	.set L$set$137,LFE28-LFB28
	.quad L$set$137
	.byte	0x0
	.byte	0x4
	.set L$set$138,LCFI111-LFB28
	.long L$set$138
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$139,LCFI112-LCFI111
	.long L$set$139
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$140,LCFI114-LCFI112
	.long L$set$140
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE55:
_div_pow2__div_pow2_i32_29.eh:
LSFDE57:
	.set L$set$141,LEFDE57-LASFDE57
	.long L$set$141
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB29-.
	.set L$set$142,LFE29-LFB29
	.quad L$set$142
	.byte	0x0
	.byte	0x4
	.set L$set$143,LCFI115-LFB29
	.long L$set$143
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$144,LCFI116-LCFI115
	.long L$set$144
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$145,LCFI118-LCFI116
	.long L$set$145
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE57:
_div_pow2__div_pow2_i32_30.eh:
LSFDE59:
	.set L$set$146,LEFDE59-LASFDE59
	.long L$set$146
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB30-.
	.set L$set$147,LFE30-LFB30
	.quad L$set$147
	.byte	0x0
	.byte	0x4
	.set L$set$148,LCFI119-LFB30
	.long L$set$148
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$149,LCFI120-LCFI119
	.long L$set$149
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$150,LCFI122-LCFI120
	.long L$set$150
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE59:
_div_pow2__div_pow2_i32_31.eh:
LSFDE61:
	.set L$set$151,LEFDE61-LASFDE61
	.long L$set$151
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB31-.
	.set L$set$152,LFE31-LFB31
	.quad L$set$152
	.byte	0x0
	.byte	0x4
	.set L$set$153,LCFI123-LFB31
	.long L$set$153
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$154,LCFI124-LCFI123
	.long L$set$154
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$155,LCFI126-LCFI124
	.long L$set$155
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE61:
_div_pow2__div_pow2_u32_0.eh:
LSFDE63:
	.set L$set$156,LEFDE63-LASFDE63
	.long L$set$156
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB32-.
	.set L$set$157,LFE32-LFB32
	.quad L$set$157
	.byte	0x0
	.byte	0x4
	.set L$set$158,LCFI127-LFB32
	.long L$set$158
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$159,LCFI128-LCFI127
	.long L$set$159
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$160,LCFI130-LCFI128
	.long L$set$160
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE63:
_div_pow2__div_pow2_u32_1.eh:
LSFDE65:
	.set L$set$161,LEFDE65-LASFDE65
	.long L$set$161
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB33-.
	.set L$set$162,LFE33-LFB33
	.quad L$set$162
	.byte	0x0
	.byte	0x4
	.set L$set$163,LCFI131-LFB33
	.long L$set$163
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$164,LCFI132-LCFI131
	.long L$set$164
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$165,LCFI134-LCFI132
	.long L$set$165
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE65:
_div_pow2__div_pow2_u32_2.eh:
LSFDE67:
	.set L$set$166,LEFDE67-LASFDE67
	.long L$set$166
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB34-.
	.set L$set$167,LFE34-LFB34
	.quad L$set$167
	.byte	0x0
	.byte	0x4
	.set L$set$168,LCFI135-LFB34
	.long L$set$168
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$169,LCFI136-LCFI135
	.long L$set$169
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$170,LCFI138-LCFI136
	.long L$set$170
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE67:
_div_pow2__div_pow2_u32_3.eh:
LSFDE69:
	.set L$set$171,LEFDE69-LASFDE69
	.long L$set$171
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB35-.
	.set L$set$172,LFE35-LFB35
	.quad L$set$172
	.byte	0x0
	.byte	0x4
	.set L$set$173,LCFI139-LFB35
	.long L$set$173
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$174,LCFI140-LCFI139
	.long L$set$174
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$175,LCFI142-LCFI140
	.long L$set$175
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE69:
_div_pow2__div_pow2_u32_4.eh:
LSFDE71:
	.set L$set$176,LEFDE71-LASFDE71
	.long L$set$176
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB36-.
	.set L$set$177,LFE36-LFB36
	.quad L$set$177
	.byte	0x0
	.byte	0x4
	.set L$set$178,LCFI143-LFB36
	.long L$set$178
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$179,LCFI144-LCFI143
	.long L$set$179
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$180,LCFI146-LCFI144
	.long L$set$180
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE71:
_div_pow2__div_pow2_u32_5.eh:
LSFDE73:
	.set L$set$181,LEFDE73-LASFDE73
	.long L$set$181
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB37-.
	.set L$set$182,LFE37-LFB37
	.quad L$set$182
	.byte	0x0
	.byte	0x4
	.set L$set$183,LCFI147-LFB37
	.long L$set$183
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$184,LCFI148-LCFI147
	.long L$set$184
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$185,LCFI150-LCFI148
	.long L$set$185
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE73:
_div_pow2__div_pow2_u32_6.eh:
LSFDE75:
	.set L$set$186,LEFDE75-LASFDE75
	.long L$set$186
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB38-.
	.set L$set$187,LFE38-LFB38
	.quad L$set$187
	.byte	0x0
	.byte	0x4
	.set L$set$188,LCFI151-LFB38
	.long L$set$188
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$189,LCFI152-LCFI151
	.long L$set$189
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$190,LCFI154-LCFI152
	.long L$set$190
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE75:
_div_pow2__div_pow2_u32_7.eh:
LSFDE77:
	.set L$set$191,LEFDE77-LASFDE77
	.long L$set$191
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB39-.
	.set L$set$192,LFE39-LFB39
	.quad L$set$192
	.byte	0x0
	.byte	0x4
	.set L$set$193,LCFI155-LFB39
	.long L$set$193
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$194,LCFI156-LCFI155
	.long L$set$194
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$195,LCFI158-LCFI156
	.long L$set$195
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE77:
_div_pow2__div_pow2_u32_8.eh:
LSFDE79:
	.set L$set$196,LEFDE79-LASFDE79
	.long L$set$196
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB40-.
	.set L$set$197,LFE40-LFB40
	.quad L$set$197
	.byte	0x0
	.byte	0x4
	.set L$set$198,LCFI159-LFB40
	.long L$set$198
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$199,LCFI160-LCFI159
	.long L$set$199
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$200,LCFI162-LCFI160
	.long L$set$200
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE79:
_div_pow2__div_pow2_u32_9.eh:
LSFDE81:
	.set L$set$201,LEFDE81-LASFDE81
	.long L$set$201
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB41-.
	.set L$set$202,LFE41-LFB41
	.quad L$set$202
	.byte	0x0
	.byte	0x4
	.set L$set$203,LCFI163-LFB41
	.long L$set$203
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$204,LCFI164-LCFI163
	.long L$set$204
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$205,LCFI166-LCFI164
	.long L$set$205
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE81:
_div_pow2__div_pow2_u32_10.eh:
LSFDE83:
	.set L$set$206,LEFDE83-LASFDE83
	.long L$set$206
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB42-.
	.set L$set$207,LFE42-LFB42
	.quad L$set$207
	.byte	0x0
	.byte	0x4
	.set L$set$208,LCFI167-LFB42
	.long L$set$208
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$209,LCFI168-LCFI167
	.long L$set$209
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$210,LCFI170-LCFI168
	.long L$set$210
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE83:
_div_pow2__div_pow2_u32_11.eh:
LSFDE85:
	.set L$set$211,LEFDE85-LASFDE85
	.long L$set$211
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB43-.
	.set L$set$212,LFE43-LFB43
	.quad L$set$212
	.byte	0x0
	.byte	0x4
	.set L$set$213,LCFI171-LFB43
	.long L$set$213
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$214,LCFI172-LCFI171
	.long L$set$214
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$215,LCFI174-LCFI172
	.long L$set$215
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE85:
_div_pow2__div_pow2_u32_12.eh:
LSFDE87:
	.set L$set$216,LEFDE87-LASFDE87
	.long L$set$216
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB44-.
	.set L$set$217,LFE44-LFB44
	.quad L$set$217
	.byte	0x0
	.byte	0x4
	.set L$set$218,LCFI175-LFB44
	.long L$set$218
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$219,LCFI176-LCFI175
	.long L$set$219
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$220,LCFI178-LCFI176
	.long L$set$220
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE87:
_div_pow2__div_pow2_u32_13.eh:
LSFDE89:
	.set L$set$221,LEFDE89-LASFDE89
	.long L$set$221
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB45-.
	.set L$set$222,LFE45-LFB45
	.quad L$set$222
	.byte	0x0
	.byte	0x4
	.set L$set$223,LCFI179-LFB45
	.long L$set$223
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$224,LCFI180-LCFI179
	.long L$set$224
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$225,LCFI182-LCFI180
	.long L$set$225
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE89:
_div_pow2__div_pow2_u32_14.eh:
LSFDE91:
	.set L$set$226,LEFDE91-LASFDE91
	.long L$set$226
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB46-.
	.set L$set$227,LFE46-LFB46
	.quad L$set$227
	.byte	0x0
	.byte	0x4
	.set L$set$228,LCFI183-LFB46
	.long L$set$228
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$229,LCFI184-LCFI183
	.long L$set$229
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$230,LCFI186-LCFI184
	.long L$set$230
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE91:
_div_pow2__div_pow2_u32_15.eh:
LSFDE93:
	.set L$set$231,LEFDE93-LASFDE93
	.long L$set$231
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB47-.
	.set L$set$232,LFE47-LFB47
	.quad L$set$232
	.byte	0x0
	.byte	0x4
	.set L$set$233,LCFI187-LFB47
	.long L$set$233
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$234,LCFI188-LCFI187
	.long L$set$234
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$235,LCFI190-LCFI188
	.long L$set$235
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE93:
_div_pow2__div_pow2_u32_16.eh:
LSFDE95:
	.set L$set$236,LEFDE95-LASFDE95
	.long L$set$236
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB48-.
	.set L$set$237,LFE48-LFB48
	.quad L$set$237
	.byte	0x0
	.byte	0x4
	.set L$set$238,LCFI191-LFB48
	.long L$set$238
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$239,LCFI192-LCFI191
	.long L$set$239
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$240,LCFI194-LCFI192
	.long L$set$240
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE95:
_div_pow2__div_pow2_u32_17.eh:
LSFDE97:
	.set L$set$241,LEFDE97-LASFDE97
	.long L$set$241
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB49-.
	.set L$set$242,LFE49-LFB49
	.quad L$set$242
	.byte	0x0
	.byte	0x4
	.set L$set$243,LCFI195-LFB49
	.long L$set$243
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$244,LCFI196-LCFI195
	.long L$set$244
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$245,LCFI198-LCFI196
	.long L$set$245
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE97:
_div_pow2__div_pow2_u32_18.eh:
LSFDE99:
	.set L$set$246,LEFDE99-LASFDE99
	.long L$set$246
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB50-.
	.set L$set$247,LFE50-LFB50
	.quad L$set$247
	.byte	0x0
	.byte	0x4
	.set L$set$248,LCFI199-LFB50
	.long L$set$248
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$249,LCFI200-LCFI199
	.long L$set$249
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$250,LCFI202-LCFI200
	.long L$set$250
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE99:
_div_pow2__div_pow2_u32_19.eh:
LSFDE101:
	.set L$set$251,LEFDE101-LASFDE101
	.long L$set$251
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB51-.
	.set L$set$252,LFE51-LFB51
	.quad L$set$252
	.byte	0x0
	.byte	0x4
	.set L$set$253,LCFI203-LFB51
	.long L$set$253
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$254,LCFI204-LCFI203
	.long L$set$254
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$255,LCFI206-LCFI204
	.long L$set$255
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE101:
_div_pow2__div_pow2_u32_20.eh:
LSFDE103:
	.set L$set$256,LEFDE103-LASFDE103
	.long L$set$256
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB52-.
	.set L$set$257,LFE52-LFB52
	.quad L$set$257
	.byte	0x0
	.byte	0x4
	.set L$set$258,LCFI207-LFB52
	.long L$set$258
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$259,LCFI208-LCFI207
	.long L$set$259
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$260,LCFI210-LCFI208
	.long L$set$260
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE103:
_div_pow2__div_pow2_u32_21.eh:
LSFDE105:
	.set L$set$261,LEFDE105-LASFDE105
	.long L$set$261
LASFDE105:
	.long	LASFDE105-EH_frame1
	.quad	LFB53-.
	.set L$set$262,LFE53-LFB53
	.quad L$set$262
	.byte	0x0
	.byte	0x4
	.set L$set$263,LCFI211-LFB53
	.long L$set$263
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$264,LCFI212-LCFI211
	.long L$set$264
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$265,LCFI214-LCFI212
	.long L$set$265
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE105:
_div_pow2__div_pow2_u32_22.eh:
LSFDE107:
	.set L$set$266,LEFDE107-LASFDE107
	.long L$set$266
LASFDE107:
	.long	LASFDE107-EH_frame1
	.quad	LFB54-.
	.set L$set$267,LFE54-LFB54
	.quad L$set$267
	.byte	0x0
	.byte	0x4
	.set L$set$268,LCFI215-LFB54
	.long L$set$268
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$269,LCFI216-LCFI215
	.long L$set$269
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$270,LCFI218-LCFI216
	.long L$set$270
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE107:
_div_pow2__div_pow2_u32_23.eh:
LSFDE109:
	.set L$set$271,LEFDE109-LASFDE109
	.long L$set$271
LASFDE109:
	.long	LASFDE109-EH_frame1
	.quad	LFB55-.
	.set L$set$272,LFE55-LFB55
	.quad L$set$272
	.byte	0x0
	.byte	0x4
	.set L$set$273,LCFI219-LFB55
	.long L$set$273
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$274,LCFI220-LCFI219
	.long L$set$274
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$275,LCFI222-LCFI220
	.long L$set$275
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE109:
_div_pow2__div_pow2_u32_24.eh:
LSFDE111:
	.set L$set$276,LEFDE111-LASFDE111
	.long L$set$276
LASFDE111:
	.long	LASFDE111-EH_frame1
	.quad	LFB56-.
	.set L$set$277,LFE56-LFB56
	.quad L$set$277
	.byte	0x0
	.byte	0x4
	.set L$set$278,LCFI223-LFB56
	.long L$set$278
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$279,LCFI224-LCFI223
	.long L$set$279
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$280,LCFI226-LCFI224
	.long L$set$280
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE111:
_div_pow2__div_pow2_u32_25.eh:
LSFDE113:
	.set L$set$281,LEFDE113-LASFDE113
	.long L$set$281
LASFDE113:
	.long	LASFDE113-EH_frame1
	.quad	LFB57-.
	.set L$set$282,LFE57-LFB57
	.quad L$set$282
	.byte	0x0
	.byte	0x4
	.set L$set$283,LCFI227-LFB57
	.long L$set$283
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$284,LCFI228-LCFI227
	.long L$set$284
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$285,LCFI230-LCFI228
	.long L$set$285
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE113:
_div_pow2__div_pow2_u32_26.eh:
LSFDE115:
	.set L$set$286,LEFDE115-LASFDE115
	.long L$set$286
LASFDE115:
	.long	LASFDE115-EH_frame1
	.quad	LFB58-.
	.set L$set$287,LFE58-LFB58
	.quad L$set$287
	.byte	0x0
	.byte	0x4
	.set L$set$288,LCFI231-LFB58
	.long L$set$288
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$289,LCFI232-LCFI231
	.long L$set$289
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$290,LCFI234-LCFI232
	.long L$set$290
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE115:
_div_pow2__div_pow2_u32_27.eh:
LSFDE117:
	.set L$set$291,LEFDE117-LASFDE117
	.long L$set$291
LASFDE117:
	.long	LASFDE117-EH_frame1
	.quad	LFB59-.
	.set L$set$292,LFE59-LFB59
	.quad L$set$292
	.byte	0x0
	.byte	0x4
	.set L$set$293,LCFI235-LFB59
	.long L$set$293
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$294,LCFI236-LCFI235
	.long L$set$294
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$295,LCFI238-LCFI236
	.long L$set$295
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE117:
_div_pow2__div_pow2_u32_28.eh:
LSFDE119:
	.set L$set$296,LEFDE119-LASFDE119
	.long L$set$296
LASFDE119:
	.long	LASFDE119-EH_frame1
	.quad	LFB60-.
	.set L$set$297,LFE60-LFB60
	.quad L$set$297
	.byte	0x0
	.byte	0x4
	.set L$set$298,LCFI239-LFB60
	.long L$set$298
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$299,LCFI240-LCFI239
	.long L$set$299
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$300,LCFI242-LCFI240
	.long L$set$300
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE119:
_div_pow2__div_pow2_u32_29.eh:
LSFDE121:
	.set L$set$301,LEFDE121-LASFDE121
	.long L$set$301
LASFDE121:
	.long	LASFDE121-EH_frame1
	.quad	LFB61-.
	.set L$set$302,LFE61-LFB61
	.quad L$set$302
	.byte	0x0
	.byte	0x4
	.set L$set$303,LCFI243-LFB61
	.long L$set$303
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$304,LCFI244-LCFI243
	.long L$set$304
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$305,LCFI246-LCFI244
	.long L$set$305
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE121:
_div_pow2__div_pow2_u32_30.eh:
LSFDE123:
	.set L$set$306,LEFDE123-LASFDE123
	.long L$set$306
LASFDE123:
	.long	LASFDE123-EH_frame1
	.quad	LFB62-.
	.set L$set$307,LFE62-LFB62
	.quad L$set$307
	.byte	0x0
	.byte	0x4
	.set L$set$308,LCFI247-LFB62
	.long L$set$308
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$309,LCFI248-LCFI247
	.long L$set$309
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$310,LCFI250-LCFI248
	.long L$set$310
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE123:
_div_pow2__div_pow2_u32_31.eh:
LSFDE125:
	.set L$set$311,LEFDE125-LASFDE125
	.long L$set$311
LASFDE125:
	.long	LASFDE125-EH_frame1
	.quad	LFB63-.
	.set L$set$312,LFE63-LFB63
	.quad L$set$312
	.byte	0x0
	.byte	0x4
	.set L$set$313,LCFI251-LFB63
	.long L$set$313
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$314,LCFI252-LCFI251
	.long L$set$314
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$315,LCFI254-LCFI252
	.long L$set$315
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE125:
_div_pow2__div_pow2_C_0.eh:
LSFDE127:
	.set L$set$316,LEFDE127-LASFDE127
	.long L$set$316
LASFDE127:
	.long	LASFDE127-EH_frame1
	.quad	LFB256-.
	.set L$set$317,LFE256-LFB256
	.quad L$set$317
	.byte	0x0
	.byte	0x4
	.set L$set$318,LCFI831-LFB256
	.long L$set$318
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$319,LCFI832-LCFI831
	.long L$set$319
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$320,LCFI834-LCFI832
	.long L$set$320
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE127:
_div_pow2__div_pow2_C_1.eh:
LSFDE129:
	.set L$set$321,LEFDE129-LASFDE129
	.long L$set$321
LASFDE129:
	.long	LASFDE129-EH_frame1
	.quad	LFB257-.
	.set L$set$322,LFE257-LFB257
	.quad L$set$322
	.byte	0x0
	.byte	0x4
	.set L$set$323,LCFI835-LFB257
	.long L$set$323
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$324,LCFI836-LCFI835
	.long L$set$324
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$325,LCFI838-LCFI836
	.long L$set$325
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE129:
_div_pow2__div_pow2_C_2.eh:
LSFDE131:
	.set L$set$326,LEFDE131-LASFDE131
	.long L$set$326
LASFDE131:
	.long	LASFDE131-EH_frame1
	.quad	LFB258-.
	.set L$set$327,LFE258-LFB258
	.quad L$set$327
	.byte	0x0
	.byte	0x4
	.set L$set$328,LCFI839-LFB258
	.long L$set$328
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$329,LCFI840-LCFI839
	.long L$set$329
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$330,LCFI842-LCFI840
	.long L$set$330
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE131:
_div_pow2__div_pow2_C_3.eh:
LSFDE133:
	.set L$set$331,LEFDE133-LASFDE133
	.long L$set$331
LASFDE133:
	.long	LASFDE133-EH_frame1
	.quad	LFB259-.
	.set L$set$332,LFE259-LFB259
	.quad L$set$332
	.byte	0x0
	.byte	0x4
	.set L$set$333,LCFI843-LFB259
	.long L$set$333
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$334,LCFI844-LCFI843
	.long L$set$334
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$335,LCFI846-LCFI844
	.long L$set$335
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE133:
_div_pow2__div_pow2_C_4.eh:
LSFDE135:
	.set L$set$336,LEFDE135-LASFDE135
	.long L$set$336
LASFDE135:
	.long	LASFDE135-EH_frame1
	.quad	LFB260-.
	.set L$set$337,LFE260-LFB260
	.quad L$set$337
	.byte	0x0
	.byte	0x4
	.set L$set$338,LCFI847-LFB260
	.long L$set$338
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$339,LCFI848-LCFI847
	.long L$set$339
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$340,LCFI850-LCFI848
	.long L$set$340
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE135:
_div_pow2__div_pow2_C_5.eh:
LSFDE137:
	.set L$set$341,LEFDE137-LASFDE137
	.long L$set$341
LASFDE137:
	.long	LASFDE137-EH_frame1
	.quad	LFB261-.
	.set L$set$342,LFE261-LFB261
	.quad L$set$342
	.byte	0x0
	.byte	0x4
	.set L$set$343,LCFI851-LFB261
	.long L$set$343
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$344,LCFI852-LCFI851
	.long L$set$344
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$345,LCFI854-LCFI852
	.long L$set$345
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE137:
_div_pow2__div_pow2_C_6.eh:
LSFDE139:
	.set L$set$346,LEFDE139-LASFDE139
	.long L$set$346
LASFDE139:
	.long	LASFDE139-EH_frame1
	.quad	LFB262-.
	.set L$set$347,LFE262-LFB262
	.quad L$set$347
	.byte	0x0
	.byte	0x4
	.set L$set$348,LCFI855-LFB262
	.long L$set$348
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$349,LCFI856-LCFI855
	.long L$set$349
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$350,LCFI858-LCFI856
	.long L$set$350
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE139:
_div_pow2__div_pow2_C_7.eh:
LSFDE141:
	.set L$set$351,LEFDE141-LASFDE141
	.long L$set$351
LASFDE141:
	.long	LASFDE141-EH_frame1
	.quad	LFB263-.
	.set L$set$352,LFE263-LFB263
	.quad L$set$352
	.byte	0x0
	.byte	0x4
	.set L$set$353,LCFI859-LFB263
	.long L$set$353
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$354,LCFI860-LCFI859
	.long L$set$354
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$355,LCFI862-LCFI860
	.long L$set$355
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE141:
_div_pow2__div_pow2_C_8.eh:
LSFDE143:
	.set L$set$356,LEFDE143-LASFDE143
	.long L$set$356
LASFDE143:
	.long	LASFDE143-EH_frame1
	.quad	LFB264-.
	.set L$set$357,LFE264-LFB264
	.quad L$set$357
	.byte	0x0
	.byte	0x4
	.set L$set$358,LCFI863-LFB264
	.long L$set$358
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$359,LCFI864-LCFI863
	.long L$set$359
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$360,LCFI866-LCFI864
	.long L$set$360
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE143:
_div_pow2__div_pow2_C_9.eh:
LSFDE145:
	.set L$set$361,LEFDE145-LASFDE145
	.long L$set$361
LASFDE145:
	.long	LASFDE145-EH_frame1
	.quad	LFB265-.
	.set L$set$362,LFE265-LFB265
	.quad L$set$362
	.byte	0x0
	.byte	0x4
	.set L$set$363,LCFI867-LFB265
	.long L$set$363
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$364,LCFI868-LCFI867
	.long L$set$364
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$365,LCFI870-LCFI868
	.long L$set$365
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE145:
_div_pow2__div_pow2_C_10.eh:
LSFDE147:
	.set L$set$366,LEFDE147-LASFDE147
	.long L$set$366
LASFDE147:
	.long	LASFDE147-EH_frame1
	.quad	LFB266-.
	.set L$set$367,LFE266-LFB266
	.quad L$set$367
	.byte	0x0
	.byte	0x4
	.set L$set$368,LCFI871-LFB266
	.long L$set$368
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$369,LCFI872-LCFI871
	.long L$set$369
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$370,LCFI874-LCFI872
	.long L$set$370
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE147:
_div_pow2__div_pow2_C_11.eh:
LSFDE149:
	.set L$set$371,LEFDE149-LASFDE149
	.long L$set$371
LASFDE149:
	.long	LASFDE149-EH_frame1
	.quad	LFB267-.
	.set L$set$372,LFE267-LFB267
	.quad L$set$372
	.byte	0x0
	.byte	0x4
	.set L$set$373,LCFI875-LFB267
	.long L$set$373
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$374,LCFI876-LCFI875
	.long L$set$374
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$375,LCFI878-LCFI876
	.long L$set$375
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE149:
_div_pow2__div_pow2_C_12.eh:
LSFDE151:
	.set L$set$376,LEFDE151-LASFDE151
	.long L$set$376
LASFDE151:
	.long	LASFDE151-EH_frame1
	.quad	LFB268-.
	.set L$set$377,LFE268-LFB268
	.quad L$set$377
	.byte	0x0
	.byte	0x4
	.set L$set$378,LCFI879-LFB268
	.long L$set$378
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$379,LCFI880-LCFI879
	.long L$set$379
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$380,LCFI882-LCFI880
	.long L$set$380
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE151:
_div_pow2__div_pow2_C_13.eh:
LSFDE153:
	.set L$set$381,LEFDE153-LASFDE153
	.long L$set$381
LASFDE153:
	.long	LASFDE153-EH_frame1
	.quad	LFB269-.
	.set L$set$382,LFE269-LFB269
	.quad L$set$382
	.byte	0x0
	.byte	0x4
	.set L$set$383,LCFI883-LFB269
	.long L$set$383
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$384,LCFI884-LCFI883
	.long L$set$384
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$385,LCFI886-LCFI884
	.long L$set$385
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE153:
_div_pow2__div_pow2_C_14.eh:
LSFDE155:
	.set L$set$386,LEFDE155-LASFDE155
	.long L$set$386
LASFDE155:
	.long	LASFDE155-EH_frame1
	.quad	LFB270-.
	.set L$set$387,LFE270-LFB270
	.quad L$set$387
	.byte	0x0
	.byte	0x4
	.set L$set$388,LCFI887-LFB270
	.long L$set$388
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$389,LCFI888-LCFI887
	.long L$set$389
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$390,LCFI890-LCFI888
	.long L$set$390
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE155:
_div_pow2__div_pow2_C_15.eh:
LSFDE157:
	.set L$set$391,LEFDE157-LASFDE157
	.long L$set$391
LASFDE157:
	.long	LASFDE157-EH_frame1
	.quad	LFB271-.
	.set L$set$392,LFE271-LFB271
	.quad L$set$392
	.byte	0x0
	.byte	0x4
	.set L$set$393,LCFI891-LFB271
	.long L$set$393
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$394,LCFI892-LCFI891
	.long L$set$394
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$395,LCFI894-LCFI892
	.long L$set$395
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE157:
_div_pow2__div_pow2_C_16.eh:
LSFDE159:
	.set L$set$396,LEFDE159-LASFDE159
	.long L$set$396
LASFDE159:
	.long	LASFDE159-EH_frame1
	.quad	LFB272-.
	.set L$set$397,LFE272-LFB272
	.quad L$set$397
	.byte	0x0
	.byte	0x4
	.set L$set$398,LCFI895-LFB272
	.long L$set$398
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$399,LCFI896-LCFI895
	.long L$set$399
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$400,LCFI898-LCFI896
	.long L$set$400
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE159:
_div_pow2__div_pow2_C_17.eh:
LSFDE161:
	.set L$set$401,LEFDE161-LASFDE161
	.long L$set$401
LASFDE161:
	.long	LASFDE161-EH_frame1
	.quad	LFB273-.
	.set L$set$402,LFE273-LFB273
	.quad L$set$402
	.byte	0x0
	.byte	0x4
	.set L$set$403,LCFI899-LFB273
	.long L$set$403
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$404,LCFI900-LCFI899
	.long L$set$404
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$405,LCFI902-LCFI900
	.long L$set$405
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE161:
_div_pow2__div_pow2_C_18.eh:
LSFDE163:
	.set L$set$406,LEFDE163-LASFDE163
	.long L$set$406
LASFDE163:
	.long	LASFDE163-EH_frame1
	.quad	LFB274-.
	.set L$set$407,LFE274-LFB274
	.quad L$set$407
	.byte	0x0
	.byte	0x4
	.set L$set$408,LCFI903-LFB274
	.long L$set$408
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$409,LCFI904-LCFI903
	.long L$set$409
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$410,LCFI906-LCFI904
	.long L$set$410
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE163:
_div_pow2__div_pow2_C_19.eh:
LSFDE165:
	.set L$set$411,LEFDE165-LASFDE165
	.long L$set$411
LASFDE165:
	.long	LASFDE165-EH_frame1
	.quad	LFB275-.
	.set L$set$412,LFE275-LFB275
	.quad L$set$412
	.byte	0x0
	.byte	0x4
	.set L$set$413,LCFI907-LFB275
	.long L$set$413
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$414,LCFI908-LCFI907
	.long L$set$414
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$415,LCFI910-LCFI908
	.long L$set$415
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE165:
_div_pow2__div_pow2_C_20.eh:
LSFDE167:
	.set L$set$416,LEFDE167-LASFDE167
	.long L$set$416
LASFDE167:
	.long	LASFDE167-EH_frame1
	.quad	LFB276-.
	.set L$set$417,LFE276-LFB276
	.quad L$set$417
	.byte	0x0
	.byte	0x4
	.set L$set$418,LCFI911-LFB276
	.long L$set$418
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$419,LCFI912-LCFI911
	.long L$set$419
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$420,LCFI914-LCFI912
	.long L$set$420
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE167:
_div_pow2__div_pow2_C_21.eh:
LSFDE169:
	.set L$set$421,LEFDE169-LASFDE169
	.long L$set$421
LASFDE169:
	.long	LASFDE169-EH_frame1
	.quad	LFB277-.
	.set L$set$422,LFE277-LFB277
	.quad L$set$422
	.byte	0x0
	.byte	0x4
	.set L$set$423,LCFI915-LFB277
	.long L$set$423
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$424,LCFI916-LCFI915
	.long L$set$424
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$425,LCFI918-LCFI916
	.long L$set$425
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE169:
_div_pow2__div_pow2_C_22.eh:
LSFDE171:
	.set L$set$426,LEFDE171-LASFDE171
	.long L$set$426
LASFDE171:
	.long	LASFDE171-EH_frame1
	.quad	LFB278-.
	.set L$set$427,LFE278-LFB278
	.quad L$set$427
	.byte	0x0
	.byte	0x4
	.set L$set$428,LCFI919-LFB278
	.long L$set$428
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$429,LCFI920-LCFI919
	.long L$set$429
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$430,LCFI922-LCFI920
	.long L$set$430
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE171:
_div_pow2__div_pow2_C_23.eh:
LSFDE173:
	.set L$set$431,LEFDE173-LASFDE173
	.long L$set$431
LASFDE173:
	.long	LASFDE173-EH_frame1
	.quad	LFB279-.
	.set L$set$432,LFE279-LFB279
	.quad L$set$432
	.byte	0x0
	.byte	0x4
	.set L$set$433,LCFI923-LFB279
	.long L$set$433
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$434,LCFI924-LCFI923
	.long L$set$434
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$435,LCFI926-LCFI924
	.long L$set$435
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE173:
_div_pow2__div_pow2_C_24.eh:
LSFDE175:
	.set L$set$436,LEFDE175-LASFDE175
	.long L$set$436
LASFDE175:
	.long	LASFDE175-EH_frame1
	.quad	LFB280-.
	.set L$set$437,LFE280-LFB280
	.quad L$set$437
	.byte	0x0
	.byte	0x4
	.set L$set$438,LCFI927-LFB280
	.long L$set$438
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$439,LCFI928-LCFI927
	.long L$set$439
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$440,LCFI930-LCFI928
	.long L$set$440
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE175:
_div_pow2__div_pow2_C_25.eh:
LSFDE177:
	.set L$set$441,LEFDE177-LASFDE177
	.long L$set$441
LASFDE177:
	.long	LASFDE177-EH_frame1
	.quad	LFB281-.
	.set L$set$442,LFE281-LFB281
	.quad L$set$442
	.byte	0x0
	.byte	0x4
	.set L$set$443,LCFI931-LFB281
	.long L$set$443
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$444,LCFI932-LCFI931
	.long L$set$444
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$445,LCFI934-LCFI932
	.long L$set$445
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE177:
_div_pow2__div_pow2_C_26.eh:
LSFDE179:
	.set L$set$446,LEFDE179-LASFDE179
	.long L$set$446
LASFDE179:
	.long	LASFDE179-EH_frame1
	.quad	LFB282-.
	.set L$set$447,LFE282-LFB282
	.quad L$set$447
	.byte	0x0
	.byte	0x4
	.set L$set$448,LCFI935-LFB282
	.long L$set$448
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$449,LCFI936-LCFI935
	.long L$set$449
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$450,LCFI938-LCFI936
	.long L$set$450
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE179:
_div_pow2__div_pow2_C_27.eh:
LSFDE181:
	.set L$set$451,LEFDE181-LASFDE181
	.long L$set$451
LASFDE181:
	.long	LASFDE181-EH_frame1
	.quad	LFB283-.
	.set L$set$452,LFE283-LFB283
	.quad L$set$452
	.byte	0x0
	.byte	0x4
	.set L$set$453,LCFI939-LFB283
	.long L$set$453
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$454,LCFI940-LCFI939
	.long L$set$454
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$455,LCFI942-LCFI940
	.long L$set$455
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE181:
_div_pow2__div_pow2_C_28.eh:
LSFDE183:
	.set L$set$456,LEFDE183-LASFDE183
	.long L$set$456
LASFDE183:
	.long	LASFDE183-EH_frame1
	.quad	LFB284-.
	.set L$set$457,LFE284-LFB284
	.quad L$set$457
	.byte	0x0
	.byte	0x4
	.set L$set$458,LCFI943-LFB284
	.long L$set$458
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$459,LCFI944-LCFI943
	.long L$set$459
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$460,LCFI946-LCFI944
	.long L$set$460
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE183:
_div_pow2__div_pow2_C_29.eh:
LSFDE185:
	.set L$set$461,LEFDE185-LASFDE185
	.long L$set$461
LASFDE185:
	.long	LASFDE185-EH_frame1
	.quad	LFB285-.
	.set L$set$462,LFE285-LFB285
	.quad L$set$462
	.byte	0x0
	.byte	0x4
	.set L$set$463,LCFI947-LFB285
	.long L$set$463
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$464,LCFI948-LCFI947
	.long L$set$464
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$465,LCFI950-LCFI948
	.long L$set$465
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE185:
_div_pow2__div_pow2_C_30.eh:
LSFDE187:
	.set L$set$466,LEFDE187-LASFDE187
	.long L$set$466
LASFDE187:
	.long	LASFDE187-EH_frame1
	.quad	LFB286-.
	.set L$set$467,LFE286-LFB286
	.quad L$set$467
	.byte	0x0
	.byte	0x4
	.set L$set$468,LCFI951-LFB286
	.long L$set$468
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$469,LCFI952-LCFI951
	.long L$set$469
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$470,LCFI954-LCFI952
	.long L$set$470
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE187:
_div_pow2__div_pow2_C_31.eh:
LSFDE189:
	.set L$set$471,LEFDE189-LASFDE189
	.long L$set$471
LASFDE189:
	.long	LASFDE189-EH_frame1
	.quad	LFB287-.
	.set L$set$472,LFE287-LFB287
	.quad L$set$472
	.byte	0x0
	.byte	0x4
	.set L$set$473,LCFI955-LFB287
	.long L$set$473
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$474,LCFI956-LCFI955
	.long L$set$474
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$475,LCFI958-LCFI956
	.long L$set$475
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE189:
_div_pow2__div_pow2_C_32.eh:
LSFDE191:
	.set L$set$476,LEFDE191-LASFDE191
	.long L$set$476
LASFDE191:
	.long	LASFDE191-EH_frame1
	.quad	LFB288-.
	.set L$set$477,LFE288-LFB288
	.quad L$set$477
	.byte	0x0
	.byte	0x4
	.set L$set$478,LCFI959-LFB288
	.long L$set$478
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$479,LCFI960-LCFI959
	.long L$set$479
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$480,LCFI962-LCFI960
	.long L$set$480
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE191:
_div_pow2__div_pow2_C_33.eh:
LSFDE193:
	.set L$set$481,LEFDE193-LASFDE193
	.long L$set$481
LASFDE193:
	.long	LASFDE193-EH_frame1
	.quad	LFB289-.
	.set L$set$482,LFE289-LFB289
	.quad L$set$482
	.byte	0x0
	.byte	0x4
	.set L$set$483,LCFI963-LFB289
	.long L$set$483
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$484,LCFI964-LCFI963
	.long L$set$484
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$485,LCFI966-LCFI964
	.long L$set$485
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE193:
_div_pow2__div_pow2_C_34.eh:
LSFDE195:
	.set L$set$486,LEFDE195-LASFDE195
	.long L$set$486
LASFDE195:
	.long	LASFDE195-EH_frame1
	.quad	LFB290-.
	.set L$set$487,LFE290-LFB290
	.quad L$set$487
	.byte	0x0
	.byte	0x4
	.set L$set$488,LCFI967-LFB290
	.long L$set$488
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$489,LCFI968-LCFI967
	.long L$set$489
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$490,LCFI970-LCFI968
	.long L$set$490
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE195:
_div_pow2__div_pow2_C_35.eh:
LSFDE197:
	.set L$set$491,LEFDE197-LASFDE197
	.long L$set$491
LASFDE197:
	.long	LASFDE197-EH_frame1
	.quad	LFB291-.
	.set L$set$492,LFE291-LFB291
	.quad L$set$492
	.byte	0x0
	.byte	0x4
	.set L$set$493,LCFI971-LFB291
	.long L$set$493
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$494,LCFI972-LCFI971
	.long L$set$494
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$495,LCFI974-LCFI972
	.long L$set$495
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE197:
_div_pow2__div_pow2_C_36.eh:
LSFDE199:
	.set L$set$496,LEFDE199-LASFDE199
	.long L$set$496
LASFDE199:
	.long	LASFDE199-EH_frame1
	.quad	LFB292-.
	.set L$set$497,LFE292-LFB292
	.quad L$set$497
	.byte	0x0
	.byte	0x4
	.set L$set$498,LCFI975-LFB292
	.long L$set$498
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$499,LCFI976-LCFI975
	.long L$set$499
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$500,LCFI978-LCFI976
	.long L$set$500
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE199:
_div_pow2__div_pow2_C_37.eh:
LSFDE201:
	.set L$set$501,LEFDE201-LASFDE201
	.long L$set$501
LASFDE201:
	.long	LASFDE201-EH_frame1
	.quad	LFB293-.
	.set L$set$502,LFE293-LFB293
	.quad L$set$502
	.byte	0x0
	.byte	0x4
	.set L$set$503,LCFI979-LFB293
	.long L$set$503
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$504,LCFI980-LCFI979
	.long L$set$504
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$505,LCFI982-LCFI980
	.long L$set$505
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE201:
_div_pow2__div_pow2_C_38.eh:
LSFDE203:
	.set L$set$506,LEFDE203-LASFDE203
	.long L$set$506
LASFDE203:
	.long	LASFDE203-EH_frame1
	.quad	LFB294-.
	.set L$set$507,LFE294-LFB294
	.quad L$set$507
	.byte	0x0
	.byte	0x4
	.set L$set$508,LCFI983-LFB294
	.long L$set$508
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$509,LCFI984-LCFI983
	.long L$set$509
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$510,LCFI986-LCFI984
	.long L$set$510
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE203:
_div_pow2__div_pow2_C_39.eh:
LSFDE205:
	.set L$set$511,LEFDE205-LASFDE205
	.long L$set$511
LASFDE205:
	.long	LASFDE205-EH_frame1
	.quad	LFB295-.
	.set L$set$512,LFE295-LFB295
	.quad L$set$512
	.byte	0x0
	.byte	0x4
	.set L$set$513,LCFI987-LFB295
	.long L$set$513
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$514,LCFI988-LCFI987
	.long L$set$514
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$515,LCFI990-LCFI988
	.long L$set$515
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE205:
_div_pow2__div_pow2_C_40.eh:
LSFDE207:
	.set L$set$516,LEFDE207-LASFDE207
	.long L$set$516
LASFDE207:
	.long	LASFDE207-EH_frame1
	.quad	LFB296-.
	.set L$set$517,LFE296-LFB296
	.quad L$set$517
	.byte	0x0
	.byte	0x4
	.set L$set$518,LCFI991-LFB296
	.long L$set$518
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$519,LCFI992-LCFI991
	.long L$set$519
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$520,LCFI994-LCFI992
	.long L$set$520
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE207:
_div_pow2__div_pow2_C_41.eh:
LSFDE209:
	.set L$set$521,LEFDE209-LASFDE209
	.long L$set$521
LASFDE209:
	.long	LASFDE209-EH_frame1
	.quad	LFB297-.
	.set L$set$522,LFE297-LFB297
	.quad L$set$522
	.byte	0x0
	.byte	0x4
	.set L$set$523,LCFI995-LFB297
	.long L$set$523
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$524,LCFI996-LCFI995
	.long L$set$524
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$525,LCFI998-LCFI996
	.long L$set$525
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE209:
_div_pow2__div_pow2_C_42.eh:
LSFDE211:
	.set L$set$526,LEFDE211-LASFDE211
	.long L$set$526
LASFDE211:
	.long	LASFDE211-EH_frame1
	.quad	LFB298-.
	.set L$set$527,LFE298-LFB298
	.quad L$set$527
	.byte	0x0
	.byte	0x4
	.set L$set$528,LCFI999-LFB298
	.long L$set$528
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$529,LCFI1000-LCFI999
	.long L$set$529
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$530,LCFI1002-LCFI1000
	.long L$set$530
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE211:
_div_pow2__div_pow2_C_43.eh:
LSFDE213:
	.set L$set$531,LEFDE213-LASFDE213
	.long L$set$531
LASFDE213:
	.long	LASFDE213-EH_frame1
	.quad	LFB299-.
	.set L$set$532,LFE299-LFB299
	.quad L$set$532
	.byte	0x0
	.byte	0x4
	.set L$set$533,LCFI1003-LFB299
	.long L$set$533
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$534,LCFI1004-LCFI1003
	.long L$set$534
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$535,LCFI1006-LCFI1004
	.long L$set$535
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE213:
_div_pow2__div_pow2_C_44.eh:
LSFDE215:
	.set L$set$536,LEFDE215-LASFDE215
	.long L$set$536
LASFDE215:
	.long	LASFDE215-EH_frame1
	.quad	LFB300-.
	.set L$set$537,LFE300-LFB300
	.quad L$set$537
	.byte	0x0
	.byte	0x4
	.set L$set$538,LCFI1007-LFB300
	.long L$set$538
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$539,LCFI1008-LCFI1007
	.long L$set$539
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$540,LCFI1010-LCFI1008
	.long L$set$540
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE215:
_div_pow2__div_pow2_C_45.eh:
LSFDE217:
	.set L$set$541,LEFDE217-LASFDE217
	.long L$set$541
LASFDE217:
	.long	LASFDE217-EH_frame1
	.quad	LFB301-.
	.set L$set$542,LFE301-LFB301
	.quad L$set$542
	.byte	0x0
	.byte	0x4
	.set L$set$543,LCFI1011-LFB301
	.long L$set$543
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$544,LCFI1012-LCFI1011
	.long L$set$544
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$545,LCFI1014-LCFI1012
	.long L$set$545
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE217:
_div_pow2__div_pow2_C_46.eh:
LSFDE219:
	.set L$set$546,LEFDE219-LASFDE219
	.long L$set$546
LASFDE219:
	.long	LASFDE219-EH_frame1
	.quad	LFB302-.
	.set L$set$547,LFE302-LFB302
	.quad L$set$547
	.byte	0x0
	.byte	0x4
	.set L$set$548,LCFI1015-LFB302
	.long L$set$548
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$549,LCFI1016-LCFI1015
	.long L$set$549
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$550,LCFI1018-LCFI1016
	.long L$set$550
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE219:
_div_pow2__div_pow2_C_47.eh:
LSFDE221:
	.set L$set$551,LEFDE221-LASFDE221
	.long L$set$551
LASFDE221:
	.long	LASFDE221-EH_frame1
	.quad	LFB303-.
	.set L$set$552,LFE303-LFB303
	.quad L$set$552
	.byte	0x0
	.byte	0x4
	.set L$set$553,LCFI1019-LFB303
	.long L$set$553
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$554,LCFI1020-LCFI1019
	.long L$set$554
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$555,LCFI1022-LCFI1020
	.long L$set$555
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE221:
_div_pow2__div_pow2_C_48.eh:
LSFDE223:
	.set L$set$556,LEFDE223-LASFDE223
	.long L$set$556
LASFDE223:
	.long	LASFDE223-EH_frame1
	.quad	LFB304-.
	.set L$set$557,LFE304-LFB304
	.quad L$set$557
	.byte	0x0
	.byte	0x4
	.set L$set$558,LCFI1023-LFB304
	.long L$set$558
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$559,LCFI1024-LCFI1023
	.long L$set$559
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$560,LCFI1026-LCFI1024
	.long L$set$560
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE223:
_div_pow2__div_pow2_C_49.eh:
LSFDE225:
	.set L$set$561,LEFDE225-LASFDE225
	.long L$set$561
LASFDE225:
	.long	LASFDE225-EH_frame1
	.quad	LFB305-.
	.set L$set$562,LFE305-LFB305
	.quad L$set$562
	.byte	0x0
	.byte	0x4
	.set L$set$563,LCFI1027-LFB305
	.long L$set$563
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$564,LCFI1028-LCFI1027
	.long L$set$564
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$565,LCFI1030-LCFI1028
	.long L$set$565
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE225:
_div_pow2__div_pow2_C_50.eh:
LSFDE227:
	.set L$set$566,LEFDE227-LASFDE227
	.long L$set$566
LASFDE227:
	.long	LASFDE227-EH_frame1
	.quad	LFB306-.
	.set L$set$567,LFE306-LFB306
	.quad L$set$567
	.byte	0x0
	.byte	0x4
	.set L$set$568,LCFI1031-LFB306
	.long L$set$568
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$569,LCFI1032-LCFI1031
	.long L$set$569
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$570,LCFI1034-LCFI1032
	.long L$set$570
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE227:
_div_pow2__div_pow2_C_51.eh:
LSFDE229:
	.set L$set$571,LEFDE229-LASFDE229
	.long L$set$571
LASFDE229:
	.long	LASFDE229-EH_frame1
	.quad	LFB307-.
	.set L$set$572,LFE307-LFB307
	.quad L$set$572
	.byte	0x0
	.byte	0x4
	.set L$set$573,LCFI1035-LFB307
	.long L$set$573
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$574,LCFI1036-LCFI1035
	.long L$set$574
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$575,LCFI1038-LCFI1036
	.long L$set$575
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE229:
_div_pow2__div_pow2_C_52.eh:
LSFDE231:
	.set L$set$576,LEFDE231-LASFDE231
	.long L$set$576
LASFDE231:
	.long	LASFDE231-EH_frame1
	.quad	LFB308-.
	.set L$set$577,LFE308-LFB308
	.quad L$set$577
	.byte	0x0
	.byte	0x4
	.set L$set$578,LCFI1039-LFB308
	.long L$set$578
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$579,LCFI1040-LCFI1039
	.long L$set$579
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$580,LCFI1042-LCFI1040
	.long L$set$580
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE231:
_div_pow2__div_pow2_C_53.eh:
LSFDE233:
	.set L$set$581,LEFDE233-LASFDE233
	.long L$set$581
LASFDE233:
	.long	LASFDE233-EH_frame1
	.quad	LFB309-.
	.set L$set$582,LFE309-LFB309
	.quad L$set$582
	.byte	0x0
	.byte	0x4
	.set L$set$583,LCFI1043-LFB309
	.long L$set$583
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$584,LCFI1044-LCFI1043
	.long L$set$584
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$585,LCFI1046-LCFI1044
	.long L$set$585
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE233:
_div_pow2__div_pow2_C_54.eh:
LSFDE235:
	.set L$set$586,LEFDE235-LASFDE235
	.long L$set$586
LASFDE235:
	.long	LASFDE235-EH_frame1
	.quad	LFB310-.
	.set L$set$587,LFE310-LFB310
	.quad L$set$587
	.byte	0x0
	.byte	0x4
	.set L$set$588,LCFI1047-LFB310
	.long L$set$588
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$589,LCFI1048-LCFI1047
	.long L$set$589
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$590,LCFI1050-LCFI1048
	.long L$set$590
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE235:
_div_pow2__div_pow2_C_55.eh:
LSFDE237:
	.set L$set$591,LEFDE237-LASFDE237
	.long L$set$591
LASFDE237:
	.long	LASFDE237-EH_frame1
	.quad	LFB311-.
	.set L$set$592,LFE311-LFB311
	.quad L$set$592
	.byte	0x0
	.byte	0x4
	.set L$set$593,LCFI1051-LFB311
	.long L$set$593
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$594,LCFI1052-LCFI1051
	.long L$set$594
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$595,LCFI1054-LCFI1052
	.long L$set$595
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE237:
_div_pow2__div_pow2_C_56.eh:
LSFDE239:
	.set L$set$596,LEFDE239-LASFDE239
	.long L$set$596
LASFDE239:
	.long	LASFDE239-EH_frame1
	.quad	LFB312-.
	.set L$set$597,LFE312-LFB312
	.quad L$set$597
	.byte	0x0
	.byte	0x4
	.set L$set$598,LCFI1055-LFB312
	.long L$set$598
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$599,LCFI1056-LCFI1055
	.long L$set$599
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$600,LCFI1058-LCFI1056
	.long L$set$600
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE239:
_div_pow2__div_pow2_C_57.eh:
LSFDE241:
	.set L$set$601,LEFDE241-LASFDE241
	.long L$set$601
LASFDE241:
	.long	LASFDE241-EH_frame1
	.quad	LFB313-.
	.set L$set$602,LFE313-LFB313
	.quad L$set$602
	.byte	0x0
	.byte	0x4
	.set L$set$603,LCFI1059-LFB313
	.long L$set$603
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$604,LCFI1060-LCFI1059
	.long L$set$604
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$605,LCFI1062-LCFI1060
	.long L$set$605
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE241:
_div_pow2__div_pow2_C_58.eh:
LSFDE243:
	.set L$set$606,LEFDE243-LASFDE243
	.long L$set$606
LASFDE243:
	.long	LASFDE243-EH_frame1
	.quad	LFB314-.
	.set L$set$607,LFE314-LFB314
	.quad L$set$607
	.byte	0x0
	.byte	0x4
	.set L$set$608,LCFI1063-LFB314
	.long L$set$608
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$609,LCFI1064-LCFI1063
	.long L$set$609
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$610,LCFI1066-LCFI1064
	.long L$set$610
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE243:
_div_pow2__div_pow2_C_59.eh:
LSFDE245:
	.set L$set$611,LEFDE245-LASFDE245
	.long L$set$611
LASFDE245:
	.long	LASFDE245-EH_frame1
	.quad	LFB315-.
	.set L$set$612,LFE315-LFB315
	.quad L$set$612
	.byte	0x0
	.byte	0x4
	.set L$set$613,LCFI1067-LFB315
	.long L$set$613
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$614,LCFI1068-LCFI1067
	.long L$set$614
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$615,LCFI1070-LCFI1068
	.long L$set$615
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE245:
_div_pow2__div_pow2_C_60.eh:
LSFDE247:
	.set L$set$616,LEFDE247-LASFDE247
	.long L$set$616
LASFDE247:
	.long	LASFDE247-EH_frame1
	.quad	LFB316-.
	.set L$set$617,LFE316-LFB316
	.quad L$set$617
	.byte	0x0
	.byte	0x4
	.set L$set$618,LCFI1071-LFB316
	.long L$set$618
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$619,LCFI1072-LCFI1071
	.long L$set$619
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$620,LCFI1074-LCFI1072
	.long L$set$620
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE247:
_div_pow2__div_pow2_C_61.eh:
LSFDE249:
	.set L$set$621,LEFDE249-LASFDE249
	.long L$set$621
LASFDE249:
	.long	LASFDE249-EH_frame1
	.quad	LFB317-.
	.set L$set$622,LFE317-LFB317
	.quad L$set$622
	.byte	0x0
	.byte	0x4
	.set L$set$623,LCFI1075-LFB317
	.long L$set$623
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$624,LCFI1076-LCFI1075
	.long L$set$624
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$625,LCFI1078-LCFI1076
	.long L$set$625
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE249:
_div_pow2__div_pow2_C_62.eh:
LSFDE251:
	.set L$set$626,LEFDE251-LASFDE251
	.long L$set$626
LASFDE251:
	.long	LASFDE251-EH_frame1
	.quad	LFB318-.
	.set L$set$627,LFE318-LFB318
	.quad L$set$627
	.byte	0x0
	.byte	0x4
	.set L$set$628,LCFI1079-LFB318
	.long L$set$628
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$629,LCFI1080-LCFI1079
	.long L$set$629
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$630,LCFI1082-LCFI1080
	.long L$set$630
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE251:
_div_pow2__div_pow2_C_63.eh:
LSFDE253:
	.set L$set$631,LEFDE253-LASFDE253
	.long L$set$631
LASFDE253:
	.long	LASFDE253-EH_frame1
	.quad	LFB319-.
	.set L$set$632,LFE319-LFB319
	.quad L$set$632
	.byte	0x0
	.byte	0x4
	.set L$set$633,LCFI1083-LFB319
	.long L$set$633
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$634,LCFI1084-LCFI1083
	.long L$set$634
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$635,LCFI1086-LCFI1084
	.long L$set$635
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE253:
_div_pow2__div_pow2_LC_0.eh:
LSFDE255:
	.set L$set$636,LEFDE255-LASFDE255
	.long L$set$636
LASFDE255:
	.long	LASFDE255-EH_frame1
	.quad	LFB384-.
	.set L$set$637,LFE384-LFB384
	.quad L$set$637
	.byte	0x0
	.byte	0x4
	.set L$set$638,LCFI1279-LFB384
	.long L$set$638
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$639,LCFI1280-LCFI1279
	.long L$set$639
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$640,LCFI1282-LCFI1280
	.long L$set$640
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE255:
_div_pow2__div_pow2_LC_1.eh:
LSFDE257:
	.set L$set$641,LEFDE257-LASFDE257
	.long L$set$641
LASFDE257:
	.long	LASFDE257-EH_frame1
	.quad	LFB385-.
	.set L$set$642,LFE385-LFB385
	.quad L$set$642
	.byte	0x0
	.byte	0x4
	.set L$set$643,LCFI1283-LFB385
	.long L$set$643
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$644,LCFI1284-LCFI1283
	.long L$set$644
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$645,LCFI1286-LCFI1284
	.long L$set$645
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE257:
_div_pow2__div_pow2_LC_2.eh:
LSFDE259:
	.set L$set$646,LEFDE259-LASFDE259
	.long L$set$646
LASFDE259:
	.long	LASFDE259-EH_frame1
	.quad	LFB386-.
	.set L$set$647,LFE386-LFB386
	.quad L$set$647
	.byte	0x0
	.byte	0x4
	.set L$set$648,LCFI1287-LFB386
	.long L$set$648
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$649,LCFI1288-LCFI1287
	.long L$set$649
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$650,LCFI1290-LCFI1288
	.long L$set$650
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE259:
_div_pow2__div_pow2_LC_3.eh:
LSFDE261:
	.set L$set$651,LEFDE261-LASFDE261
	.long L$set$651
LASFDE261:
	.long	LASFDE261-EH_frame1
	.quad	LFB387-.
	.set L$set$652,LFE387-LFB387
	.quad L$set$652
	.byte	0x0
	.byte	0x4
	.set L$set$653,LCFI1291-LFB387
	.long L$set$653
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$654,LCFI1292-LCFI1291
	.long L$set$654
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$655,LCFI1294-LCFI1292
	.long L$set$655
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE261:
_div_pow2__div_pow2_LC_4.eh:
LSFDE263:
	.set L$set$656,LEFDE263-LASFDE263
	.long L$set$656
LASFDE263:
	.long	LASFDE263-EH_frame1
	.quad	LFB388-.
	.set L$set$657,LFE388-LFB388
	.quad L$set$657
	.byte	0x0
	.byte	0x4
	.set L$set$658,LCFI1295-LFB388
	.long L$set$658
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$659,LCFI1296-LCFI1295
	.long L$set$659
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$660,LCFI1298-LCFI1296
	.long L$set$660
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE263:
_div_pow2__div_pow2_LC_5.eh:
LSFDE265:
	.set L$set$661,LEFDE265-LASFDE265
	.long L$set$661
LASFDE265:
	.long	LASFDE265-EH_frame1
	.quad	LFB389-.
	.set L$set$662,LFE389-LFB389
	.quad L$set$662
	.byte	0x0
	.byte	0x4
	.set L$set$663,LCFI1299-LFB389
	.long L$set$663
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$664,LCFI1300-LCFI1299
	.long L$set$664
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$665,LCFI1302-LCFI1300
	.long L$set$665
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE265:
_div_pow2__div_pow2_LC_6.eh:
LSFDE267:
	.set L$set$666,LEFDE267-LASFDE267
	.long L$set$666
LASFDE267:
	.long	LASFDE267-EH_frame1
	.quad	LFB390-.
	.set L$set$667,LFE390-LFB390
	.quad L$set$667
	.byte	0x0
	.byte	0x4
	.set L$set$668,LCFI1303-LFB390
	.long L$set$668
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$669,LCFI1304-LCFI1303
	.long L$set$669
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$670,LCFI1306-LCFI1304
	.long L$set$670
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE267:
_div_pow2__div_pow2_LC_7.eh:
LSFDE269:
	.set L$set$671,LEFDE269-LASFDE269
	.long L$set$671
LASFDE269:
	.long	LASFDE269-EH_frame1
	.quad	LFB391-.
	.set L$set$672,LFE391-LFB391
	.quad L$set$672
	.byte	0x0
	.byte	0x4
	.set L$set$673,LCFI1307-LFB391
	.long L$set$673
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$674,LCFI1308-LCFI1307
	.long L$set$674
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$675,LCFI1310-LCFI1308
	.long L$set$675
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE269:
_div_pow2__div_pow2_LC_8.eh:
LSFDE271:
	.set L$set$676,LEFDE271-LASFDE271
	.long L$set$676
LASFDE271:
	.long	LASFDE271-EH_frame1
	.quad	LFB392-.
	.set L$set$677,LFE392-LFB392
	.quad L$set$677
	.byte	0x0
	.byte	0x4
	.set L$set$678,LCFI1311-LFB392
	.long L$set$678
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$679,LCFI1312-LCFI1311
	.long L$set$679
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$680,LCFI1314-LCFI1312
	.long L$set$680
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE271:
_div_pow2__div_pow2_LC_9.eh:
LSFDE273:
	.set L$set$681,LEFDE273-LASFDE273
	.long L$set$681
LASFDE273:
	.long	LASFDE273-EH_frame1
	.quad	LFB393-.
	.set L$set$682,LFE393-LFB393
	.quad L$set$682
	.byte	0x0
	.byte	0x4
	.set L$set$683,LCFI1315-LFB393
	.long L$set$683
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$684,LCFI1316-LCFI1315
	.long L$set$684
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$685,LCFI1318-LCFI1316
	.long L$set$685
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE273:
_div_pow2__div_pow2_LC_10.eh:
LSFDE275:
	.set L$set$686,LEFDE275-LASFDE275
	.long L$set$686
LASFDE275:
	.long	LASFDE275-EH_frame1
	.quad	LFB394-.
	.set L$set$687,LFE394-LFB394
	.quad L$set$687
	.byte	0x0
	.byte	0x4
	.set L$set$688,LCFI1319-LFB394
	.long L$set$688
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$689,LCFI1320-LCFI1319
	.long L$set$689
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$690,LCFI1322-LCFI1320
	.long L$set$690
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE275:
_div_pow2__div_pow2_LC_11.eh:
LSFDE277:
	.set L$set$691,LEFDE277-LASFDE277
	.long L$set$691
LASFDE277:
	.long	LASFDE277-EH_frame1
	.quad	LFB395-.
	.set L$set$692,LFE395-LFB395
	.quad L$set$692
	.byte	0x0
	.byte	0x4
	.set L$set$693,LCFI1323-LFB395
	.long L$set$693
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$694,LCFI1324-LCFI1323
	.long L$set$694
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$695,LCFI1326-LCFI1324
	.long L$set$695
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE277:
_div_pow2__div_pow2_LC_12.eh:
LSFDE279:
	.set L$set$696,LEFDE279-LASFDE279
	.long L$set$696
LASFDE279:
	.long	LASFDE279-EH_frame1
	.quad	LFB396-.
	.set L$set$697,LFE396-LFB396
	.quad L$set$697
	.byte	0x0
	.byte	0x4
	.set L$set$698,LCFI1327-LFB396
	.long L$set$698
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$699,LCFI1328-LCFI1327
	.long L$set$699
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$700,LCFI1330-LCFI1328
	.long L$set$700
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE279:
_div_pow2__div_pow2_LC_13.eh:
LSFDE281:
	.set L$set$701,LEFDE281-LASFDE281
	.long L$set$701
LASFDE281:
	.long	LASFDE281-EH_frame1
	.quad	LFB397-.
	.set L$set$702,LFE397-LFB397
	.quad L$set$702
	.byte	0x0
	.byte	0x4
	.set L$set$703,LCFI1331-LFB397
	.long L$set$703
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$704,LCFI1332-LCFI1331
	.long L$set$704
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$705,LCFI1334-LCFI1332
	.long L$set$705
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE281:
_div_pow2__div_pow2_LC_14.eh:
LSFDE283:
	.set L$set$706,LEFDE283-LASFDE283
	.long L$set$706
LASFDE283:
	.long	LASFDE283-EH_frame1
	.quad	LFB398-.
	.set L$set$707,LFE398-LFB398
	.quad L$set$707
	.byte	0x0
	.byte	0x4
	.set L$set$708,LCFI1335-LFB398
	.long L$set$708
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$709,LCFI1336-LCFI1335
	.long L$set$709
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$710,LCFI1338-LCFI1336
	.long L$set$710
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE283:
_div_pow2__div_pow2_LC_15.eh:
LSFDE285:
	.set L$set$711,LEFDE285-LASFDE285
	.long L$set$711
LASFDE285:
	.long	LASFDE285-EH_frame1
	.quad	LFB399-.
	.set L$set$712,LFE399-LFB399
	.quad L$set$712
	.byte	0x0
	.byte	0x4
	.set L$set$713,LCFI1339-LFB399
	.long L$set$713
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$714,LCFI1340-LCFI1339
	.long L$set$714
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$715,LCFI1342-LCFI1340
	.long L$set$715
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE285:
_div_pow2__div_pow2_LC_16.eh:
LSFDE287:
	.set L$set$716,LEFDE287-LASFDE287
	.long L$set$716
LASFDE287:
	.long	LASFDE287-EH_frame1
	.quad	LFB400-.
	.set L$set$717,LFE400-LFB400
	.quad L$set$717
	.byte	0x0
	.byte	0x4
	.set L$set$718,LCFI1343-LFB400
	.long L$set$718
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$719,LCFI1344-LCFI1343
	.long L$set$719
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$720,LCFI1346-LCFI1344
	.long L$set$720
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE287:
_div_pow2__div_pow2_LC_17.eh:
LSFDE289:
	.set L$set$721,LEFDE289-LASFDE289
	.long L$set$721
LASFDE289:
	.long	LASFDE289-EH_frame1
	.quad	LFB401-.
	.set L$set$722,LFE401-LFB401
	.quad L$set$722
	.byte	0x0
	.byte	0x4
	.set L$set$723,LCFI1347-LFB401
	.long L$set$723
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$724,LCFI1348-LCFI1347
	.long L$set$724
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$725,LCFI1350-LCFI1348
	.long L$set$725
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE289:
_div_pow2__div_pow2_LC_18.eh:
LSFDE291:
	.set L$set$726,LEFDE291-LASFDE291
	.long L$set$726
LASFDE291:
	.long	LASFDE291-EH_frame1
	.quad	LFB402-.
	.set L$set$727,LFE402-LFB402
	.quad L$set$727
	.byte	0x0
	.byte	0x4
	.set L$set$728,LCFI1351-LFB402
	.long L$set$728
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$729,LCFI1352-LCFI1351
	.long L$set$729
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$730,LCFI1354-LCFI1352
	.long L$set$730
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE291:
_div_pow2__div_pow2_LC_19.eh:
LSFDE293:
	.set L$set$731,LEFDE293-LASFDE293
	.long L$set$731
LASFDE293:
	.long	LASFDE293-EH_frame1
	.quad	LFB403-.
	.set L$set$732,LFE403-LFB403
	.quad L$set$732
	.byte	0x0
	.byte	0x4
	.set L$set$733,LCFI1355-LFB403
	.long L$set$733
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$734,LCFI1356-LCFI1355
	.long L$set$734
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$735,LCFI1358-LCFI1356
	.long L$set$735
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE293:
_div_pow2__div_pow2_LC_20.eh:
LSFDE295:
	.set L$set$736,LEFDE295-LASFDE295
	.long L$set$736
LASFDE295:
	.long	LASFDE295-EH_frame1
	.quad	LFB404-.
	.set L$set$737,LFE404-LFB404
	.quad L$set$737
	.byte	0x0
	.byte	0x4
	.set L$set$738,LCFI1359-LFB404
	.long L$set$738
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$739,LCFI1360-LCFI1359
	.long L$set$739
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$740,LCFI1362-LCFI1360
	.long L$set$740
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE295:
_div_pow2__div_pow2_LC_21.eh:
LSFDE297:
	.set L$set$741,LEFDE297-LASFDE297
	.long L$set$741
LASFDE297:
	.long	LASFDE297-EH_frame1
	.quad	LFB405-.
	.set L$set$742,LFE405-LFB405
	.quad L$set$742
	.byte	0x0
	.byte	0x4
	.set L$set$743,LCFI1363-LFB405
	.long L$set$743
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$744,LCFI1364-LCFI1363
	.long L$set$744
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$745,LCFI1366-LCFI1364
	.long L$set$745
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE297:
_div_pow2__div_pow2_LC_22.eh:
LSFDE299:
	.set L$set$746,LEFDE299-LASFDE299
	.long L$set$746
LASFDE299:
	.long	LASFDE299-EH_frame1
	.quad	LFB406-.
	.set L$set$747,LFE406-LFB406
	.quad L$set$747
	.byte	0x0
	.byte	0x4
	.set L$set$748,LCFI1367-LFB406
	.long L$set$748
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$749,LCFI1368-LCFI1367
	.long L$set$749
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$750,LCFI1370-LCFI1368
	.long L$set$750
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE299:
_div_pow2__div_pow2_LC_23.eh:
LSFDE301:
	.set L$set$751,LEFDE301-LASFDE301
	.long L$set$751
LASFDE301:
	.long	LASFDE301-EH_frame1
	.quad	LFB407-.
	.set L$set$752,LFE407-LFB407
	.quad L$set$752
	.byte	0x0
	.byte	0x4
	.set L$set$753,LCFI1371-LFB407
	.long L$set$753
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$754,LCFI1372-LCFI1371
	.long L$set$754
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$755,LCFI1374-LCFI1372
	.long L$set$755
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE301:
_div_pow2__div_pow2_LC_24.eh:
LSFDE303:
	.set L$set$756,LEFDE303-LASFDE303
	.long L$set$756
LASFDE303:
	.long	LASFDE303-EH_frame1
	.quad	LFB408-.
	.set L$set$757,LFE408-LFB408
	.quad L$set$757
	.byte	0x0
	.byte	0x4
	.set L$set$758,LCFI1375-LFB408
	.long L$set$758
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$759,LCFI1376-LCFI1375
	.long L$set$759
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$760,LCFI1378-LCFI1376
	.long L$set$760
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE303:
_div_pow2__div_pow2_LC_25.eh:
LSFDE305:
	.set L$set$761,LEFDE305-LASFDE305
	.long L$set$761
LASFDE305:
	.long	LASFDE305-EH_frame1
	.quad	LFB409-.
	.set L$set$762,LFE409-LFB409
	.quad L$set$762
	.byte	0x0
	.byte	0x4
	.set L$set$763,LCFI1379-LFB409
	.long L$set$763
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$764,LCFI1380-LCFI1379
	.long L$set$764
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$765,LCFI1382-LCFI1380
	.long L$set$765
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE305:
_div_pow2__div_pow2_LC_26.eh:
LSFDE307:
	.set L$set$766,LEFDE307-LASFDE307
	.long L$set$766
LASFDE307:
	.long	LASFDE307-EH_frame1
	.quad	LFB410-.
	.set L$set$767,LFE410-LFB410
	.quad L$set$767
	.byte	0x0
	.byte	0x4
	.set L$set$768,LCFI1383-LFB410
	.long L$set$768
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$769,LCFI1384-LCFI1383
	.long L$set$769
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$770,LCFI1386-LCFI1384
	.long L$set$770
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE307:
_div_pow2__div_pow2_LC_27.eh:
LSFDE309:
	.set L$set$771,LEFDE309-LASFDE309
	.long L$set$771
LASFDE309:
	.long	LASFDE309-EH_frame1
	.quad	LFB411-.
	.set L$set$772,LFE411-LFB411
	.quad L$set$772
	.byte	0x0
	.byte	0x4
	.set L$set$773,LCFI1387-LFB411
	.long L$set$773
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$774,LCFI1388-LCFI1387
	.long L$set$774
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$775,LCFI1390-LCFI1388
	.long L$set$775
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE309:
_div_pow2__div_pow2_LC_28.eh:
LSFDE311:
	.set L$set$776,LEFDE311-LASFDE311
	.long L$set$776
LASFDE311:
	.long	LASFDE311-EH_frame1
	.quad	LFB412-.
	.set L$set$777,LFE412-LFB412
	.quad L$set$777
	.byte	0x0
	.byte	0x4
	.set L$set$778,LCFI1391-LFB412
	.long L$set$778
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$779,LCFI1392-LCFI1391
	.long L$set$779
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$780,LCFI1394-LCFI1392
	.long L$set$780
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE311:
_div_pow2__div_pow2_LC_29.eh:
LSFDE313:
	.set L$set$781,LEFDE313-LASFDE313
	.long L$set$781
LASFDE313:
	.long	LASFDE313-EH_frame1
	.quad	LFB413-.
	.set L$set$782,LFE413-LFB413
	.quad L$set$782
	.byte	0x0
	.byte	0x4
	.set L$set$783,LCFI1395-LFB413
	.long L$set$783
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$784,LCFI1396-LCFI1395
	.long L$set$784
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$785,LCFI1398-LCFI1396
	.long L$set$785
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE313:
_div_pow2__div_pow2_LC_30.eh:
LSFDE315:
	.set L$set$786,LEFDE315-LASFDE315
	.long L$set$786
LASFDE315:
	.long	LASFDE315-EH_frame1
	.quad	LFB414-.
	.set L$set$787,LFE414-LFB414
	.quad L$set$787
	.byte	0x0
	.byte	0x4
	.set L$set$788,LCFI1399-LFB414
	.long L$set$788
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$789,LCFI1400-LCFI1399
	.long L$set$789
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$790,LCFI1402-LCFI1400
	.long L$set$790
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE315:
_div_pow2__div_pow2_LC_31.eh:
LSFDE317:
	.set L$set$791,LEFDE317-LASFDE317
	.long L$set$791
LASFDE317:
	.long	LASFDE317-EH_frame1
	.quad	LFB415-.
	.set L$set$792,LFE415-LFB415
	.quad L$set$792
	.byte	0x0
	.byte	0x4
	.set L$set$793,LCFI1403-LFB415
	.long L$set$793
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$794,LCFI1404-LCFI1403
	.long L$set$794
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$795,LCFI1406-LCFI1404
	.long L$set$795
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE317:
_div_pow2__div_pow2_LC_32.eh:
LSFDE319:
	.set L$set$796,LEFDE319-LASFDE319
	.long L$set$796
LASFDE319:
	.long	LASFDE319-EH_frame1
	.quad	LFB416-.
	.set L$set$797,LFE416-LFB416
	.quad L$set$797
	.byte	0x0
	.byte	0x4
	.set L$set$798,LCFI1407-LFB416
	.long L$set$798
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$799,LCFI1408-LCFI1407
	.long L$set$799
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$800,LCFI1410-LCFI1408
	.long L$set$800
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE319:
_div_pow2__div_pow2_LC_33.eh:
LSFDE321:
	.set L$set$801,LEFDE321-LASFDE321
	.long L$set$801
LASFDE321:
	.long	LASFDE321-EH_frame1
	.quad	LFB417-.
	.set L$set$802,LFE417-LFB417
	.quad L$set$802
	.byte	0x0
	.byte	0x4
	.set L$set$803,LCFI1411-LFB417
	.long L$set$803
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$804,LCFI1412-LCFI1411
	.long L$set$804
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$805,LCFI1414-LCFI1412
	.long L$set$805
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE321:
_div_pow2__div_pow2_LC_34.eh:
LSFDE323:
	.set L$set$806,LEFDE323-LASFDE323
	.long L$set$806
LASFDE323:
	.long	LASFDE323-EH_frame1
	.quad	LFB418-.
	.set L$set$807,LFE418-LFB418
	.quad L$set$807
	.byte	0x0
	.byte	0x4
	.set L$set$808,LCFI1415-LFB418
	.long L$set$808
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$809,LCFI1416-LCFI1415
	.long L$set$809
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$810,LCFI1418-LCFI1416
	.long L$set$810
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE323:
_div_pow2__div_pow2_LC_35.eh:
LSFDE325:
	.set L$set$811,LEFDE325-LASFDE325
	.long L$set$811
LASFDE325:
	.long	LASFDE325-EH_frame1
	.quad	LFB419-.
	.set L$set$812,LFE419-LFB419
	.quad L$set$812
	.byte	0x0
	.byte	0x4
	.set L$set$813,LCFI1419-LFB419
	.long L$set$813
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$814,LCFI1420-LCFI1419
	.long L$set$814
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$815,LCFI1422-LCFI1420
	.long L$set$815
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE325:
_div_pow2__div_pow2_LC_36.eh:
LSFDE327:
	.set L$set$816,LEFDE327-LASFDE327
	.long L$set$816
LASFDE327:
	.long	LASFDE327-EH_frame1
	.quad	LFB420-.
	.set L$set$817,LFE420-LFB420
	.quad L$set$817
	.byte	0x0
	.byte	0x4
	.set L$set$818,LCFI1423-LFB420
	.long L$set$818
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$819,LCFI1424-LCFI1423
	.long L$set$819
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$820,LCFI1426-LCFI1424
	.long L$set$820
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE327:
_div_pow2__div_pow2_LC_37.eh:
LSFDE329:
	.set L$set$821,LEFDE329-LASFDE329
	.long L$set$821
LASFDE329:
	.long	LASFDE329-EH_frame1
	.quad	LFB421-.
	.set L$set$822,LFE421-LFB421
	.quad L$set$822
	.byte	0x0
	.byte	0x4
	.set L$set$823,LCFI1427-LFB421
	.long L$set$823
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$824,LCFI1428-LCFI1427
	.long L$set$824
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$825,LCFI1430-LCFI1428
	.long L$set$825
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE329:
_div_pow2__div_pow2_LC_38.eh:
LSFDE331:
	.set L$set$826,LEFDE331-LASFDE331
	.long L$set$826
LASFDE331:
	.long	LASFDE331-EH_frame1
	.quad	LFB422-.
	.set L$set$827,LFE422-LFB422
	.quad L$set$827
	.byte	0x0
	.byte	0x4
	.set L$set$828,LCFI1431-LFB422
	.long L$set$828
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$829,LCFI1432-LCFI1431
	.long L$set$829
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$830,LCFI1434-LCFI1432
	.long L$set$830
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE331:
_div_pow2__div_pow2_LC_39.eh:
LSFDE333:
	.set L$set$831,LEFDE333-LASFDE333
	.long L$set$831
LASFDE333:
	.long	LASFDE333-EH_frame1
	.quad	LFB423-.
	.set L$set$832,LFE423-LFB423
	.quad L$set$832
	.byte	0x0
	.byte	0x4
	.set L$set$833,LCFI1435-LFB423
	.long L$set$833
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$834,LCFI1436-LCFI1435
	.long L$set$834
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$835,LCFI1438-LCFI1436
	.long L$set$835
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE333:
_div_pow2__div_pow2_LC_40.eh:
LSFDE335:
	.set L$set$836,LEFDE335-LASFDE335
	.long L$set$836
LASFDE335:
	.long	LASFDE335-EH_frame1
	.quad	LFB424-.
	.set L$set$837,LFE424-LFB424
	.quad L$set$837
	.byte	0x0
	.byte	0x4
	.set L$set$838,LCFI1439-LFB424
	.long L$set$838
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$839,LCFI1440-LCFI1439
	.long L$set$839
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$840,LCFI1442-LCFI1440
	.long L$set$840
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE335:
_div_pow2__div_pow2_LC_41.eh:
LSFDE337:
	.set L$set$841,LEFDE337-LASFDE337
	.long L$set$841
LASFDE337:
	.long	LASFDE337-EH_frame1
	.quad	LFB425-.
	.set L$set$842,LFE425-LFB425
	.quad L$set$842
	.byte	0x0
	.byte	0x4
	.set L$set$843,LCFI1443-LFB425
	.long L$set$843
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$844,LCFI1444-LCFI1443
	.long L$set$844
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$845,LCFI1446-LCFI1444
	.long L$set$845
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE337:
_div_pow2__div_pow2_LC_42.eh:
LSFDE339:
	.set L$set$846,LEFDE339-LASFDE339
	.long L$set$846
LASFDE339:
	.long	LASFDE339-EH_frame1
	.quad	LFB426-.
	.set L$set$847,LFE426-LFB426
	.quad L$set$847
	.byte	0x0
	.byte	0x4
	.set L$set$848,LCFI1447-LFB426
	.long L$set$848
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$849,LCFI1448-LCFI1447
	.long L$set$849
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$850,LCFI1450-LCFI1448
	.long L$set$850
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE339:
_div_pow2__div_pow2_LC_43.eh:
LSFDE341:
	.set L$set$851,LEFDE341-LASFDE341
	.long L$set$851
LASFDE341:
	.long	LASFDE341-EH_frame1
	.quad	LFB427-.
	.set L$set$852,LFE427-LFB427
	.quad L$set$852
	.byte	0x0
	.byte	0x4
	.set L$set$853,LCFI1451-LFB427
	.long L$set$853
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$854,LCFI1452-LCFI1451
	.long L$set$854
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$855,LCFI1454-LCFI1452
	.long L$set$855
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE341:
_div_pow2__div_pow2_LC_44.eh:
LSFDE343:
	.set L$set$856,LEFDE343-LASFDE343
	.long L$set$856
LASFDE343:
	.long	LASFDE343-EH_frame1
	.quad	LFB428-.
	.set L$set$857,LFE428-LFB428
	.quad L$set$857
	.byte	0x0
	.byte	0x4
	.set L$set$858,LCFI1455-LFB428
	.long L$set$858
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$859,LCFI1456-LCFI1455
	.long L$set$859
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$860,LCFI1458-LCFI1456
	.long L$set$860
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE343:
_div_pow2__div_pow2_LC_45.eh:
LSFDE345:
	.set L$set$861,LEFDE345-LASFDE345
	.long L$set$861
LASFDE345:
	.long	LASFDE345-EH_frame1
	.quad	LFB429-.
	.set L$set$862,LFE429-LFB429
	.quad L$set$862
	.byte	0x0
	.byte	0x4
	.set L$set$863,LCFI1459-LFB429
	.long L$set$863
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$864,LCFI1460-LCFI1459
	.long L$set$864
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$865,LCFI1462-LCFI1460
	.long L$set$865
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE345:
_div_pow2__div_pow2_LC_46.eh:
LSFDE347:
	.set L$set$866,LEFDE347-LASFDE347
	.long L$set$866
LASFDE347:
	.long	LASFDE347-EH_frame1
	.quad	LFB430-.
	.set L$set$867,LFE430-LFB430
	.quad L$set$867
	.byte	0x0
	.byte	0x4
	.set L$set$868,LCFI1463-LFB430
	.long L$set$868
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$869,LCFI1464-LCFI1463
	.long L$set$869
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$870,LCFI1466-LCFI1464
	.long L$set$870
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE347:
_div_pow2__div_pow2_LC_47.eh:
LSFDE349:
	.set L$set$871,LEFDE349-LASFDE349
	.long L$set$871
LASFDE349:
	.long	LASFDE349-EH_frame1
	.quad	LFB431-.
	.set L$set$872,LFE431-LFB431
	.quad L$set$872
	.byte	0x0
	.byte	0x4
	.set L$set$873,LCFI1467-LFB431
	.long L$set$873
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$874,LCFI1468-LCFI1467
	.long L$set$874
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$875,LCFI1470-LCFI1468
	.long L$set$875
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE349:
_div_pow2__div_pow2_LC_48.eh:
LSFDE351:
	.set L$set$876,LEFDE351-LASFDE351
	.long L$set$876
LASFDE351:
	.long	LASFDE351-EH_frame1
	.quad	LFB432-.
	.set L$set$877,LFE432-LFB432
	.quad L$set$877
	.byte	0x0
	.byte	0x4
	.set L$set$878,LCFI1471-LFB432
	.long L$set$878
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$879,LCFI1472-LCFI1471
	.long L$set$879
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$880,LCFI1474-LCFI1472
	.long L$set$880
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE351:
_div_pow2__div_pow2_LC_49.eh:
LSFDE353:
	.set L$set$881,LEFDE353-LASFDE353
	.long L$set$881
LASFDE353:
	.long	LASFDE353-EH_frame1
	.quad	LFB433-.
	.set L$set$882,LFE433-LFB433
	.quad L$set$882
	.byte	0x0
	.byte	0x4
	.set L$set$883,LCFI1475-LFB433
	.long L$set$883
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$884,LCFI1476-LCFI1475
	.long L$set$884
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$885,LCFI1478-LCFI1476
	.long L$set$885
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE353:
_div_pow2__div_pow2_LC_50.eh:
LSFDE355:
	.set L$set$886,LEFDE355-LASFDE355
	.long L$set$886
LASFDE355:
	.long	LASFDE355-EH_frame1
	.quad	LFB434-.
	.set L$set$887,LFE434-LFB434
	.quad L$set$887
	.byte	0x0
	.byte	0x4
	.set L$set$888,LCFI1479-LFB434
	.long L$set$888
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$889,LCFI1480-LCFI1479
	.long L$set$889
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$890,LCFI1482-LCFI1480
	.long L$set$890
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE355:
_div_pow2__div_pow2_LC_51.eh:
LSFDE357:
	.set L$set$891,LEFDE357-LASFDE357
	.long L$set$891
LASFDE357:
	.long	LASFDE357-EH_frame1
	.quad	LFB435-.
	.set L$set$892,LFE435-LFB435
	.quad L$set$892
	.byte	0x0
	.byte	0x4
	.set L$set$893,LCFI1483-LFB435
	.long L$set$893
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$894,LCFI1484-LCFI1483
	.long L$set$894
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$895,LCFI1486-LCFI1484
	.long L$set$895
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE357:
_div_pow2__div_pow2_LC_52.eh:
LSFDE359:
	.set L$set$896,LEFDE359-LASFDE359
	.long L$set$896
LASFDE359:
	.long	LASFDE359-EH_frame1
	.quad	LFB436-.
	.set L$set$897,LFE436-LFB436
	.quad L$set$897
	.byte	0x0
	.byte	0x4
	.set L$set$898,LCFI1487-LFB436
	.long L$set$898
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$899,LCFI1488-LCFI1487
	.long L$set$899
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$900,LCFI1490-LCFI1488
	.long L$set$900
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE359:
_div_pow2__div_pow2_LC_53.eh:
LSFDE361:
	.set L$set$901,LEFDE361-LASFDE361
	.long L$set$901
LASFDE361:
	.long	LASFDE361-EH_frame1
	.quad	LFB437-.
	.set L$set$902,LFE437-LFB437
	.quad L$set$902
	.byte	0x0
	.byte	0x4
	.set L$set$903,LCFI1491-LFB437
	.long L$set$903
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$904,LCFI1492-LCFI1491
	.long L$set$904
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$905,LCFI1494-LCFI1492
	.long L$set$905
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE361:
_div_pow2__div_pow2_LC_54.eh:
LSFDE363:
	.set L$set$906,LEFDE363-LASFDE363
	.long L$set$906
LASFDE363:
	.long	LASFDE363-EH_frame1
	.quad	LFB438-.
	.set L$set$907,LFE438-LFB438
	.quad L$set$907
	.byte	0x0
	.byte	0x4
	.set L$set$908,LCFI1495-LFB438
	.long L$set$908
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$909,LCFI1496-LCFI1495
	.long L$set$909
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$910,LCFI1498-LCFI1496
	.long L$set$910
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE363:
_div_pow2__div_pow2_LC_55.eh:
LSFDE365:
	.set L$set$911,LEFDE365-LASFDE365
	.long L$set$911
LASFDE365:
	.long	LASFDE365-EH_frame1
	.quad	LFB439-.
	.set L$set$912,LFE439-LFB439
	.quad L$set$912
	.byte	0x0
	.byte	0x4
	.set L$set$913,LCFI1499-LFB439
	.long L$set$913
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$914,LCFI1500-LCFI1499
	.long L$set$914
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$915,LCFI1502-LCFI1500
	.long L$set$915
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE365:
_div_pow2__div_pow2_LC_56.eh:
LSFDE367:
	.set L$set$916,LEFDE367-LASFDE367
	.long L$set$916
LASFDE367:
	.long	LASFDE367-EH_frame1
	.quad	LFB440-.
	.set L$set$917,LFE440-LFB440
	.quad L$set$917
	.byte	0x0
	.byte	0x4
	.set L$set$918,LCFI1503-LFB440
	.long L$set$918
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$919,LCFI1504-LCFI1503
	.long L$set$919
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$920,LCFI1506-LCFI1504
	.long L$set$920
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE367:
_div_pow2__div_pow2_LC_57.eh:
LSFDE369:
	.set L$set$921,LEFDE369-LASFDE369
	.long L$set$921
LASFDE369:
	.long	LASFDE369-EH_frame1
	.quad	LFB441-.
	.set L$set$922,LFE441-LFB441
	.quad L$set$922
	.byte	0x0
	.byte	0x4
	.set L$set$923,LCFI1507-LFB441
	.long L$set$923
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$924,LCFI1508-LCFI1507
	.long L$set$924
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$925,LCFI1510-LCFI1508
	.long L$set$925
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE369:
_div_pow2__div_pow2_LC_58.eh:
LSFDE371:
	.set L$set$926,LEFDE371-LASFDE371
	.long L$set$926
LASFDE371:
	.long	LASFDE371-EH_frame1
	.quad	LFB442-.
	.set L$set$927,LFE442-LFB442
	.quad L$set$927
	.byte	0x0
	.byte	0x4
	.set L$set$928,LCFI1511-LFB442
	.long L$set$928
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$929,LCFI1512-LCFI1511
	.long L$set$929
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$930,LCFI1514-LCFI1512
	.long L$set$930
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE371:
_div_pow2__div_pow2_LC_59.eh:
LSFDE373:
	.set L$set$931,LEFDE373-LASFDE373
	.long L$set$931
LASFDE373:
	.long	LASFDE373-EH_frame1
	.quad	LFB443-.
	.set L$set$932,LFE443-LFB443
	.quad L$set$932
	.byte	0x0
	.byte	0x4
	.set L$set$933,LCFI1515-LFB443
	.long L$set$933
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$934,LCFI1516-LCFI1515
	.long L$set$934
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$935,LCFI1518-LCFI1516
	.long L$set$935
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE373:
_div_pow2__div_pow2_LC_60.eh:
LSFDE375:
	.set L$set$936,LEFDE375-LASFDE375
	.long L$set$936
LASFDE375:
	.long	LASFDE375-EH_frame1
	.quad	LFB444-.
	.set L$set$937,LFE444-LFB444
	.quad L$set$937
	.byte	0x0
	.byte	0x4
	.set L$set$938,LCFI1519-LFB444
	.long L$set$938
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$939,LCFI1520-LCFI1519
	.long L$set$939
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$940,LCFI1522-LCFI1520
	.long L$set$940
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE375:
_div_pow2__div_pow2_LC_61.eh:
LSFDE377:
	.set L$set$941,LEFDE377-LASFDE377
	.long L$set$941
LASFDE377:
	.long	LASFDE377-EH_frame1
	.quad	LFB445-.
	.set L$set$942,LFE445-LFB445
	.quad L$set$942
	.byte	0x0
	.byte	0x4
	.set L$set$943,LCFI1523-LFB445
	.long L$set$943
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$944,LCFI1524-LCFI1523
	.long L$set$944
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$945,LCFI1526-LCFI1524
	.long L$set$945
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE377:
_div_pow2__div_pow2_LC_62.eh:
LSFDE379:
	.set L$set$946,LEFDE379-LASFDE379
	.long L$set$946
LASFDE379:
	.long	LASFDE379-EH_frame1
	.quad	LFB446-.
	.set L$set$947,LFE446-LFB446
	.quad L$set$947
	.byte	0x0
	.byte	0x4
	.set L$set$948,LCFI1527-LFB446
	.long L$set$948
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$949,LCFI1528-LCFI1527
	.long L$set$949
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$950,LCFI1530-LCFI1528
	.long L$set$950
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE379:
_div_pow2__div_pow2_LC_63.eh:
LSFDE381:
	.set L$set$951,LEFDE381-LASFDE381
	.long L$set$951
LASFDE381:
	.long	LASFDE381-EH_frame1
	.quad	LFB447-.
	.set L$set$952,LFE447-LFB447
	.quad L$set$952
	.byte	0x0
	.byte	0x4
	.set L$set$953,LCFI1531-LFB447
	.long L$set$953
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$954,LCFI1532-LCFI1531
	.long L$set$954
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$955,LCFI1534-LCFI1532
	.long L$set$955
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE381:
__m3_fault.eh:
LSFDE383:
	.set L$set$956,LEFDE383-LASFDE383
	.long L$set$956
LASFDE383:
	.long	LASFDE383-EH_frame1
	.quad	LFB449-.
	.set L$set$957,LFE449-LFB449
	.quad L$set$957
	.byte	0x0
	.byte	0x4
	.set L$set$958,LCFI1538-LFB449
	.long L$set$958
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$959,LCFI1539-LCFI1538
	.long L$set$959
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$960,LCFI1541-LCFI1539
	.long L$set$960
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.align 3
LEFDE383:
	.subsections_via_symbols
