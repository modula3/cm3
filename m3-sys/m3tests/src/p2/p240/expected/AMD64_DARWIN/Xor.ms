	.text
.no_dead_strip _Xor__uXor_var_u16_u16
_Xor__uXor_var_u16_u16:
LFB0:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI2:
	ret
LFE0:
.no_dead_strip _Xor__uXor_param_u16_u16
_Xor__uXor_param_u16_u16:
LFB1:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movzwl	-4(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI5:
	ret
LFE1:
.no_dead_strip _Xor__uXor_var_u16_u32
_Xor__uXor_var_u16_u32:
LFB2:
	pushq	%rbp
LCFI6:
	movq	%rsp, %rbp
LCFI7:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI8:
	ret
LFE2:
.no_dead_strip _Xor__uXor_param_u16_u32
_Xor__uXor_param_u16_u32:
LFB3:
	pushq	%rbp
LCFI9:
	movq	%rsp, %rbp
LCFI10:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI11:
	ret
LFE3:
.no_dead_strip _Xor__uXor_var_u16_LC
_Xor__uXor_var_u16_LC:
LFB4:
	pushq	%rbp
LCFI12:
	movq	%rsp, %rbp
LCFI13:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI14:
	ret
LFE4:
.no_dead_strip _Xor__uXor_param_u16_LC
_Xor__uXor_param_u16_LC:
LFB5:
	pushq	%rbp
LCFI15:
	movq	%rsp, %rbp
LCFI16:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI17:
	ret
LFE5:
.no_dead_strip _Xor__uXor_var_u16_I
_Xor__uXor_var_u16_I:
LFB6:
	pushq	%rbp
LCFI18:
	movq	%rsp, %rbp
LCFI19:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI20:
	ret
LFE6:
.no_dead_strip _Xor__uXor_param_u16_I
_Xor__uXor_param_u16_I:
LFB7:
	pushq	%rbp
LCFI21:
	movq	%rsp, %rbp
LCFI22:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI23:
	ret
LFE7:
.no_dead_strip _Xor__uXor_var_u16_i16
_Xor__uXor_var_u16_i16:
LFB8:
	pushq	%rbp
LCFI24:
	movq	%rsp, %rbp
LCFI25:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI26:
	ret
LFE8:
.no_dead_strip _Xor__uXor_param_u16_i16
_Xor__uXor_param_u16_i16:
LFB9:
	pushq	%rbp
LCFI27:
	movq	%rsp, %rbp
LCFI28:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movzwl	-2(%rbp), %edx
	movswq	-4(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI29:
	ret
LFE9:
.no_dead_strip _Xor__uXor_var_u16_u64
_Xor__uXor_var_u16_u64:
LFB10:
	pushq	%rbp
LCFI30:
	movq	%rsp, %rbp
LCFI31:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI32:
	ret
LFE10:
.no_dead_strip _Xor__uXor_param_u16_u64
_Xor__uXor_param_u16_u64:
LFB11:
	pushq	%rbp
LCFI33:
	movq	%rsp, %rbp
LCFI34:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI35:
	ret
LFE11:
.no_dead_strip _Xor__uXor_var_u16_i32
_Xor__uXor_var_u16_i32:
LFB12:
	pushq	%rbp
LCFI36:
	movq	%rsp, %rbp
LCFI37:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI38:
	ret
LFE12:
.no_dead_strip _Xor__uXor_param_u16_i32
_Xor__uXor_param_u16_i32:
LFB13:
	pushq	%rbp
LCFI39:
	movq	%rsp, %rbp
LCFI40:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movzwl	-2(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI41:
	ret
LFE13:
.no_dead_strip _Xor__uXor_var_u16_C
_Xor__uXor_var_u16_C:
LFB14:
	pushq	%rbp
LCFI42:
	movq	%rsp, %rbp
LCFI43:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI44:
	ret
LFE14:
.no_dead_strip _Xor__uXor_param_u16_C
_Xor__uXor_param_u16_C:
LFB15:
	pushq	%rbp
LCFI45:
	movq	%rsp, %rbp
LCFI46:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI47:
	ret
LFE15:
.no_dead_strip _Xor__uXor_var_u16_u8
_Xor__uXor_var_u16_u8:
LFB16:
	pushq	%rbp
LCFI48:
	movq	%rsp, %rbp
LCFI49:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI50:
	ret
LFE16:
.no_dead_strip _Xor__uXor_param_u16_u8
_Xor__uXor_param_u16_u8:
LFB17:
	pushq	%rbp
LCFI51:
	movq	%rsp, %rbp
LCFI52:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movzbl	-3(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI53:
	ret
LFE17:
.no_dead_strip _Xor__uXor_var_u16_i64
_Xor__uXor_var_u16_i64:
LFB18:
	pushq	%rbp
LCFI54:
	movq	%rsp, %rbp
LCFI55:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI56:
	ret
LFE18:
.no_dead_strip _Xor__uXor_param_u16_i64
_Xor__uXor_param_u16_i64:
LFB19:
	pushq	%rbp
LCFI57:
	movq	%rsp, %rbp
LCFI58:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI59:
	ret
LFE19:
.no_dead_strip _Xor__uXor_var_u16_L
_Xor__uXor_var_u16_L:
LFB20:
	pushq	%rbp
LCFI60:
	movq	%rsp, %rbp
LCFI61:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI62:
	ret
LFE20:
.no_dead_strip _Xor__uXor_param_u16_L
_Xor__uXor_param_u16_L:
LFB21:
	pushq	%rbp
LCFI63:
	movq	%rsp, %rbp
LCFI64:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movzwl	-2(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI65:
	ret
LFE21:
.no_dead_strip _Xor__uXor_var_u16_i8
_Xor__uXor_var_u16_i8:
LFB22:
	pushq	%rbp
LCFI66:
	movq	%rsp, %rbp
LCFI67:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI68:
	ret
LFE22:
.no_dead_strip _Xor__uXor_param_u16_i8
_Xor__uXor_param_u16_i8:
LFB23:
	pushq	%rbp
LCFI69:
	movq	%rsp, %rbp
LCFI70:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movzwl	-2(%rbp), %edx
	movsbq	-3(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI71:
	ret
LFE23:
.no_dead_strip _Xor__uXor_var_u32_u16
_Xor__uXor_var_u32_u16:
LFB24:
	pushq	%rbp
LCFI72:
	movq	%rsp, %rbp
LCFI73:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI74:
	ret
LFE24:
.no_dead_strip _Xor__uXor_param_u32_u16
_Xor__uXor_param_u32_u16:
LFB25:
	pushq	%rbp
LCFI75:
	movq	%rsp, %rbp
LCFI76:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movzwl	-6(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI77:
	ret
LFE25:
.no_dead_strip _Xor__uXor_var_u32_u32
_Xor__uXor_var_u32_u32:
LFB26:
	pushq	%rbp
LCFI78:
	movq	%rsp, %rbp
LCFI79:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI80:
	ret
LFE26:
.no_dead_strip _Xor__uXor_param_u32_u32
_Xor__uXor_param_u32_u32:
LFB27:
	pushq	%rbp
LCFI81:
	movq	%rsp, %rbp
LCFI82:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI83:
	ret
LFE27:
.no_dead_strip _Xor__uXor_var_u32_LC
_Xor__uXor_var_u32_LC:
LFB28:
	pushq	%rbp
LCFI84:
	movq	%rsp, %rbp
LCFI85:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI86:
	ret
LFE28:
.no_dead_strip _Xor__uXor_param_u32_LC
_Xor__uXor_param_u32_LC:
LFB29:
	pushq	%rbp
LCFI87:
	movq	%rsp, %rbp
LCFI88:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI89:
	ret
LFE29:
.no_dead_strip _Xor__uXor_var_u32_I
_Xor__uXor_var_u32_I:
LFB30:
	pushq	%rbp
LCFI90:
	movq	%rsp, %rbp
LCFI91:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI92:
	ret
LFE30:
.no_dead_strip _Xor__uXor_param_u32_I
_Xor__uXor_param_u32_I:
LFB31:
	pushq	%rbp
LCFI93:
	movq	%rsp, %rbp
LCFI94:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI95:
	ret
LFE31:
.no_dead_strip _Xor__uXor_var_u32_i16
_Xor__uXor_var_u32_i16:
LFB32:
	pushq	%rbp
LCFI96:
	movq	%rsp, %rbp
LCFI97:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI98:
	ret
LFE32:
.no_dead_strip _Xor__uXor_param_u32_i16
_Xor__uXor_param_u32_i16:
LFB33:
	pushq	%rbp
LCFI99:
	movq	%rsp, %rbp
LCFI100:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	mov	-4(%rbp), %edx
	movswq	-6(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI101:
	ret
LFE33:
.no_dead_strip _Xor__uXor_var_u32_u64
_Xor__uXor_var_u32_u64:
LFB34:
	pushq	%rbp
LCFI102:
	movq	%rsp, %rbp
LCFI103:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI104:
	ret
LFE34:
.no_dead_strip _Xor__uXor_param_u32_u64
_Xor__uXor_param_u32_u64:
LFB35:
	pushq	%rbp
LCFI105:
	movq	%rsp, %rbp
LCFI106:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI107:
	ret
LFE35:
.no_dead_strip _Xor__uXor_var_u32_i32
_Xor__uXor_var_u32_i32:
LFB36:
	pushq	%rbp
LCFI108:
	movq	%rsp, %rbp
LCFI109:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI110:
	ret
LFE36:
.no_dead_strip _Xor__uXor_param_u32_i32
_Xor__uXor_param_u32_i32:
LFB37:
	pushq	%rbp
LCFI111:
	movq	%rsp, %rbp
LCFI112:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	mov	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI113:
	ret
LFE37:
.no_dead_strip _Xor__uXor_var_u32_C
_Xor__uXor_var_u32_C:
LFB38:
	pushq	%rbp
LCFI114:
	movq	%rsp, %rbp
LCFI115:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI116:
	ret
LFE38:
.no_dead_strip _Xor__uXor_param_u32_C
_Xor__uXor_param_u32_C:
LFB39:
	pushq	%rbp
LCFI117:
	movq	%rsp, %rbp
LCFI118:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI119:
	ret
LFE39:
.no_dead_strip _Xor__uXor_var_u32_u8
_Xor__uXor_var_u32_u8:
LFB40:
	pushq	%rbp
LCFI120:
	movq	%rsp, %rbp
LCFI121:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI122:
	ret
LFE40:
.no_dead_strip _Xor__uXor_param_u32_u8
_Xor__uXor_param_u32_u8:
LFB41:
	pushq	%rbp
LCFI123:
	movq	%rsp, %rbp
LCFI124:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movzbl	-5(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI125:
	ret
LFE41:
.no_dead_strip _Xor__uXor_var_u32_i64
_Xor__uXor_var_u32_i64:
LFB42:
	pushq	%rbp
LCFI126:
	movq	%rsp, %rbp
LCFI127:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI128:
	ret
LFE42:
.no_dead_strip _Xor__uXor_param_u32_i64
_Xor__uXor_param_u32_i64:
LFB43:
	pushq	%rbp
LCFI129:
	movq	%rsp, %rbp
LCFI130:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI131:
	ret
LFE43:
.no_dead_strip _Xor__uXor_var_u32_L
_Xor__uXor_var_u32_L:
LFB44:
	pushq	%rbp
LCFI132:
	movq	%rsp, %rbp
LCFI133:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI134:
	ret
LFE44:
.no_dead_strip _Xor__uXor_param_u32_L
_Xor__uXor_param_u32_L:
LFB45:
	pushq	%rbp
LCFI135:
	movq	%rsp, %rbp
LCFI136:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	mov	-4(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI137:
	ret
LFE45:
.no_dead_strip _Xor__uXor_var_u32_i8
_Xor__uXor_var_u32_i8:
LFB46:
	pushq	%rbp
LCFI138:
	movq	%rsp, %rbp
LCFI139:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI140:
	ret
LFE46:
.no_dead_strip _Xor__uXor_param_u32_i8
_Xor__uXor_param_u32_i8:
LFB47:
	pushq	%rbp
LCFI141:
	movq	%rsp, %rbp
LCFI142:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	mov	-4(%rbp), %edx
	movsbq	-5(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI143:
	ret
LFE47:
.no_dead_strip _Xor__uXor_var_LC_u16
_Xor__uXor_var_LC_u16:
LFB48:
	pushq	%rbp
LCFI144:
	movq	%rsp, %rbp
LCFI145:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI146:
	ret
LFE48:
.no_dead_strip _Xor__uXor_param_LC_u16
_Xor__uXor_param_LC_u16:
LFB49:
	pushq	%rbp
LCFI147:
	movq	%rsp, %rbp
LCFI148:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI149:
	ret
LFE49:
.no_dead_strip _Xor__uXor_var_LC_u32
_Xor__uXor_var_LC_u32:
LFB50:
	pushq	%rbp
LCFI150:
	movq	%rsp, %rbp
LCFI151:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI152:
	ret
LFE50:
.no_dead_strip _Xor__uXor_param_LC_u32
_Xor__uXor_param_LC_u32:
LFB51:
	pushq	%rbp
LCFI153:
	movq	%rsp, %rbp
LCFI154:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI155:
	ret
LFE51:
.no_dead_strip _Xor__uXor_var_LC_LC
_Xor__uXor_var_LC_LC:
LFB52:
	pushq	%rbp
LCFI156:
	movq	%rsp, %rbp
LCFI157:
	movq	112+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI158:
	ret
LFE52:
.no_dead_strip _Xor__uXor_param_LC_LC
_Xor__uXor_param_LC_LC:
LFB53:
	pushq	%rbp
LCFI159:
	movq	%rsp, %rbp
LCFI160:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	xorq	%rdx, %rax
	leave
LCFI161:
	ret
LFE53:
.no_dead_strip _Xor__uXor_var_LC_I
_Xor__uXor_var_LC_I:
LFB54:
	pushq	%rbp
LCFI162:
	movq	%rsp, %rbp
LCFI163:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI164:
	ret
LFE54:
.no_dead_strip _Xor__uXor_param_LC_I
_Xor__uXor_param_LC_I:
LFB55:
	pushq	%rbp
LCFI165:
	movq	%rsp, %rbp
LCFI166:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI167:
	ret
LFE55:
.no_dead_strip _Xor__uXor_var_LC_i16
_Xor__uXor_var_LC_i16:
LFB56:
	pushq	%rbp
LCFI168:
	movq	%rsp, %rbp
LCFI169:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI170:
	ret
LFE56:
.no_dead_strip _Xor__uXor_param_LC_i16
_Xor__uXor_param_LC_i16:
LFB57:
	pushq	%rbp
LCFI171:
	movq	%rsp, %rbp
LCFI172:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI173:
	ret
LFE57:
.no_dead_strip _Xor__uXor_var_LC_u64
_Xor__uXor_var_LC_u64:
LFB58:
	pushq	%rbp
LCFI174:
	movq	%rsp, %rbp
LCFI175:
	movq	112+_MM_Xor(%rip), %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI176:
	ret
LFE58:
.no_dead_strip _Xor__uXor_param_LC_u64
_Xor__uXor_param_LC_u64:
LFB59:
	pushq	%rbp
LCFI177:
	movq	%rsp, %rbp
LCFI178:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI179:
	ret
LFE59:
.no_dead_strip _Xor__uXor_var_LC_i32
_Xor__uXor_var_LC_i32:
LFB60:
	pushq	%rbp
LCFI180:
	movq	%rsp, %rbp
LCFI181:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI182:
	ret
LFE60:
.no_dead_strip _Xor__uXor_param_LC_i32
_Xor__uXor_param_LC_i32:
LFB61:
	pushq	%rbp
LCFI183:
	movq	%rsp, %rbp
LCFI184:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	xorq	-8(%rbp), %rax
	leave
LCFI185:
	ret
LFE61:
.no_dead_strip _Xor__uXor_var_LC_C
_Xor__uXor_var_LC_C:
LFB62:
	pushq	%rbp
LCFI186:
	movq	%rsp, %rbp
LCFI187:
	movq	160+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI188:
	ret
LFE62:
.no_dead_strip _Xor__uXor_param_LC_C
_Xor__uXor_param_LC_C:
LFB63:
	pushq	%rbp
LCFI189:
	movq	%rsp, %rbp
LCFI190:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rdx
	xorq	%rdx, %rax
	leave
LCFI191:
	ret
LFE63:
.no_dead_strip _Xor__uXor_var_LC_u8
_Xor__uXor_var_LC_u8:
LFB64:
	pushq	%rbp
LCFI192:
	movq	%rsp, %rbp
LCFI193:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI194:
	ret
LFE64:
.no_dead_strip _Xor__uXor_param_LC_u8
_Xor__uXor_param_LC_u8:
LFB65:
	pushq	%rbp
LCFI195:
	movq	%rsp, %rbp
LCFI196:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI197:
	ret
LFE65:
.no_dead_strip _Xor__uXor_var_LC_i64
_Xor__uXor_var_LC_i64:
LFB66:
	pushq	%rbp
LCFI198:
	movq	%rsp, %rbp
LCFI199:
	movq	112+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI200:
	ret
LFE66:
.no_dead_strip _Xor__uXor_param_LC_i64
_Xor__uXor_param_LC_i64:
LFB67:
	pushq	%rbp
LCFI201:
	movq	%rsp, %rbp
LCFI202:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI203:
	ret
LFE67:
.no_dead_strip _Xor__uXor_var_LC_L
_Xor__uXor_var_LC_L:
LFB68:
	pushq	%rbp
LCFI204:
	movq	%rsp, %rbp
LCFI205:
	movq	112+_MM_Xor(%rip), %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI206:
	ret
LFE68:
.no_dead_strip _Xor__uXor_param_LC_L
_Xor__uXor_param_LC_L:
LFB69:
	pushq	%rbp
LCFI207:
	movq	%rsp, %rbp
LCFI208:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI209:
	ret
LFE69:
.no_dead_strip _Xor__uXor_var_LC_i8
_Xor__uXor_var_LC_i8:
LFB70:
	pushq	%rbp
LCFI210:
	movq	%rsp, %rbp
LCFI211:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI212:
	ret
LFE70:
.no_dead_strip _Xor__uXor_param_LC_i8
_Xor__uXor_param_LC_i8:
LFB71:
	pushq	%rbp
LCFI213:
	movq	%rsp, %rbp
LCFI214:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI215:
	ret
LFE71:
.no_dead_strip _Xor__uXor_var_I_u16
_Xor__uXor_var_I_u16:
LFB72:
	pushq	%rbp
LCFI216:
	movq	%rsp, %rbp
LCFI217:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI218:
	ret
LFE72:
.no_dead_strip _Xor__uXor_param_I_u16
_Xor__uXor_param_I_u16:
LFB73:
	pushq	%rbp
LCFI219:
	movq	%rsp, %rbp
LCFI220:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movzwl	-10(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI221:
	ret
LFE73:
.no_dead_strip _Xor__uXor_var_I_u32
_Xor__uXor_var_I_u32:
LFB74:
	pushq	%rbp
LCFI222:
	movq	%rsp, %rbp
LCFI223:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI224:
	ret
LFE74:
.no_dead_strip _Xor__uXor_param_I_u32
_Xor__uXor_param_I_u32:
LFB75:
	pushq	%rbp
LCFI225:
	movq	%rsp, %rbp
LCFI226:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	mov	-12(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI227:
	ret
LFE75:
.no_dead_strip _Xor__uXor_var_I_LC
_Xor__uXor_var_I_LC:
LFB76:
	pushq	%rbp
LCFI228:
	movq	%rsp, %rbp
LCFI229:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI230:
	ret
LFE76:
.no_dead_strip _Xor__uXor_param_I_LC
_Xor__uXor_param_I_LC:
LFB77:
	pushq	%rbp
LCFI231:
	movq	%rsp, %rbp
LCFI232:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI233:
	ret
LFE77:
.no_dead_strip _Xor__uXor_var_I_I
_Xor__uXor_var_I_I:
LFB78:
	pushq	%rbp
LCFI234:
	movq	%rsp, %rbp
LCFI235:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI236:
	ret
LFE78:
.no_dead_strip _Xor__uXor_param_I_I
_Xor__uXor_param_I_I:
LFB79:
	pushq	%rbp
LCFI237:
	movq	%rsp, %rbp
LCFI238:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI239:
	ret
LFE79:
.no_dead_strip _Xor__uXor_var_I_i16
_Xor__uXor_var_I_i16:
LFB80:
	pushq	%rbp
LCFI240:
	movq	%rsp, %rbp
LCFI241:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI242:
	ret
LFE80:
.no_dead_strip _Xor__uXor_param_I_i16
_Xor__uXor_param_I_i16:
LFB81:
	pushq	%rbp
LCFI243:
	movq	%rsp, %rbp
LCFI244:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movq	-8(%rbp), %rdx
	movswq	-10(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI245:
	ret
LFE81:
.no_dead_strip _Xor__uXor_var_I_u64
_Xor__uXor_var_I_u64:
LFB82:
	pushq	%rbp
LCFI246:
	movq	%rsp, %rbp
LCFI247:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI248:
	ret
LFE82:
.no_dead_strip _Xor__uXor_param_I_u64
_Xor__uXor_param_I_u64:
LFB83:
	pushq	%rbp
LCFI249:
	movq	%rsp, %rbp
LCFI250:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI251:
	ret
LFE83:
.no_dead_strip _Xor__uXor_var_I_i32
_Xor__uXor_var_I_i32:
LFB84:
	pushq	%rbp
LCFI252:
	movq	%rsp, %rbp
LCFI253:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI254:
	ret
LFE84:
.no_dead_strip _Xor__uXor_param_I_i32
_Xor__uXor_param_I_i32:
LFB85:
	pushq	%rbp
LCFI255:
	movq	%rsp, %rbp
LCFI256:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rdx
	movl	-12(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI257:
	ret
LFE85:
.no_dead_strip _Xor__uXor_var_I_C
_Xor__uXor_var_I_C:
LFB86:
	pushq	%rbp
LCFI258:
	movq	%rsp, %rbp
LCFI259:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI260:
	ret
LFE86:
.no_dead_strip _Xor__uXor_param_I_C
_Xor__uXor_param_I_C:
LFB87:
	pushq	%rbp
LCFI261:
	movq	%rsp, %rbp
LCFI262:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI263:
	ret
LFE87:
.no_dead_strip _Xor__uXor_var_I_u8
_Xor__uXor_var_I_u8:
LFB88:
	pushq	%rbp
LCFI264:
	movq	%rsp, %rbp
LCFI265:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI266:
	ret
LFE88:
.no_dead_strip _Xor__uXor_param_I_u8
_Xor__uXor_param_I_u8:
LFB89:
	pushq	%rbp
LCFI267:
	movq	%rsp, %rbp
LCFI268:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movzbl	-9(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI269:
	ret
LFE89:
.no_dead_strip _Xor__uXor_var_I_i64
_Xor__uXor_var_I_i64:
LFB90:
	pushq	%rbp
LCFI270:
	movq	%rsp, %rbp
LCFI271:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI272:
	ret
LFE90:
.no_dead_strip _Xor__uXor_param_I_i64
_Xor__uXor_param_I_i64:
LFB91:
	pushq	%rbp
LCFI273:
	movq	%rsp, %rbp
LCFI274:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI275:
	ret
LFE91:
.no_dead_strip _Xor__uXor_var_I_L
_Xor__uXor_var_I_L:
LFB92:
	pushq	%rbp
LCFI276:
	movq	%rsp, %rbp
LCFI277:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI278:
	ret
LFE92:
.no_dead_strip _Xor__uXor_param_I_L
_Xor__uXor_param_I_L:
LFB93:
	pushq	%rbp
LCFI279:
	movq	%rsp, %rbp
LCFI280:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI281:
	ret
LFE93:
.no_dead_strip _Xor__uXor_var_I_i8
_Xor__uXor_var_I_i8:
LFB94:
	pushq	%rbp
LCFI282:
	movq	%rsp, %rbp
LCFI283:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI284:
	ret
LFE94:
.no_dead_strip _Xor__uXor_param_I_i8
_Xor__uXor_param_I_i8:
LFB95:
	pushq	%rbp
LCFI285:
	movq	%rsp, %rbp
LCFI286:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movq	-8(%rbp), %rdx
	movsbq	-9(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI287:
	ret
LFE95:
.no_dead_strip _Xor__uXor_var_i16_u16
_Xor__uXor_var_i16_u16:
LFB96:
	pushq	%rbp
LCFI288:
	movq	%rsp, %rbp
LCFI289:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI290:
	ret
LFE96:
.no_dead_strip _Xor__uXor_param_i16_u16
_Xor__uXor_param_i16_u16:
LFB97:
	pushq	%rbp
LCFI291:
	movq	%rsp, %rbp
LCFI292:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI293:
	ret
LFE97:
.no_dead_strip _Xor__uXor_var_i16_u32
_Xor__uXor_var_i16_u32:
LFB98:
	pushq	%rbp
LCFI294:
	movq	%rsp, %rbp
LCFI295:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI296:
	ret
LFE98:
.no_dead_strip _Xor__uXor_param_i16_u32
_Xor__uXor_param_i16_u32:
LFB99:
	pushq	%rbp
LCFI297:
	movq	%rsp, %rbp
LCFI298:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI299:
	ret
LFE99:
.no_dead_strip _Xor__uXor_var_i16_LC
_Xor__uXor_var_i16_LC:
LFB100:
	pushq	%rbp
LCFI300:
	movq	%rsp, %rbp
LCFI301:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI302:
	ret
LFE100:
.no_dead_strip _Xor__uXor_param_i16_LC
_Xor__uXor_param_i16_LC:
LFB101:
	pushq	%rbp
LCFI303:
	movq	%rsp, %rbp
LCFI304:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI305:
	ret
LFE101:
.no_dead_strip _Xor__uXor_var_i16_I
_Xor__uXor_var_i16_I:
LFB102:
	pushq	%rbp
LCFI306:
	movq	%rsp, %rbp
LCFI307:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI308:
	ret
LFE102:
.no_dead_strip _Xor__uXor_param_i16_I
_Xor__uXor_param_i16_I:
LFB103:
	pushq	%rbp
LCFI309:
	movq	%rsp, %rbp
LCFI310:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI311:
	ret
LFE103:
.no_dead_strip _Xor__uXor_var_i16_i16
_Xor__uXor_var_i16_i16:
LFB104:
	pushq	%rbp
LCFI312:
	movq	%rsp, %rbp
LCFI313:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI314:
	ret
LFE104:
.no_dead_strip _Xor__uXor_param_i16_i16
_Xor__uXor_param_i16_i16:
LFB105:
	pushq	%rbp
LCFI315:
	movq	%rsp, %rbp
LCFI316:
	movw	%di, -2(%rbp)
	movw	%si, -4(%rbp)
	movswq	-2(%rbp), %rdx
	movswq	-4(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI317:
	ret
LFE105:
.no_dead_strip _Xor__uXor_var_i16_u64
_Xor__uXor_var_i16_u64:
LFB106:
	pushq	%rbp
LCFI318:
	movq	%rsp, %rbp
LCFI319:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI320:
	ret
LFE106:
.no_dead_strip _Xor__uXor_param_i16_u64
_Xor__uXor_param_i16_u64:
LFB107:
	pushq	%rbp
LCFI321:
	movq	%rsp, %rbp
LCFI322:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI323:
	ret
LFE107:
.no_dead_strip _Xor__uXor_var_i16_i32
_Xor__uXor_var_i16_i32:
LFB108:
	pushq	%rbp
LCFI324:
	movq	%rsp, %rbp
LCFI325:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI326:
	ret
LFE108:
.no_dead_strip _Xor__uXor_param_i16_i32
_Xor__uXor_param_i16_i32:
LFB109:
	pushq	%rbp
LCFI327:
	movq	%rsp, %rbp
LCFI328:
	movw	%di, -2(%rbp)
	movl	%esi, -8(%rbp)
	movswq	-2(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI329:
	ret
LFE109:
.no_dead_strip _Xor__uXor_var_i16_C
_Xor__uXor_var_i16_C:
LFB110:
	pushq	%rbp
LCFI330:
	movq	%rsp, %rbp
LCFI331:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI332:
	ret
LFE110:
.no_dead_strip _Xor__uXor_param_i16_C
_Xor__uXor_param_i16_C:
LFB111:
	pushq	%rbp
LCFI333:
	movq	%rsp, %rbp
LCFI334:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI335:
	ret
LFE111:
.no_dead_strip _Xor__uXor_var_i16_u8
_Xor__uXor_var_i16_u8:
LFB112:
	pushq	%rbp
LCFI336:
	movq	%rsp, %rbp
LCFI337:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI338:
	ret
LFE112:
.no_dead_strip _Xor__uXor_param_i16_u8
_Xor__uXor_param_i16_u8:
LFB113:
	pushq	%rbp
LCFI339:
	movq	%rsp, %rbp
LCFI340:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movzbl	-3(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI341:
	ret
LFE113:
.no_dead_strip _Xor__uXor_var_i16_i64
_Xor__uXor_var_i16_i64:
LFB114:
	pushq	%rbp
LCFI342:
	movq	%rsp, %rbp
LCFI343:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI344:
	ret
LFE114:
.no_dead_strip _Xor__uXor_param_i16_i64
_Xor__uXor_param_i16_i64:
LFB115:
	pushq	%rbp
LCFI345:
	movq	%rsp, %rbp
LCFI346:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI347:
	ret
LFE115:
.no_dead_strip _Xor__uXor_var_i16_L
_Xor__uXor_var_i16_L:
LFB116:
	pushq	%rbp
LCFI348:
	movq	%rsp, %rbp
LCFI349:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI350:
	ret
LFE116:
.no_dead_strip _Xor__uXor_param_i16_L
_Xor__uXor_param_i16_L:
LFB117:
	pushq	%rbp
LCFI351:
	movq	%rsp, %rbp
LCFI352:
	movw	%di, -2(%rbp)
	movq	%rsi, -16(%rbp)
	movswq	-2(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI353:
	ret
LFE117:
.no_dead_strip _Xor__uXor_var_i16_i8
_Xor__uXor_var_i16_i8:
LFB118:
	pushq	%rbp
LCFI354:
	movq	%rsp, %rbp
LCFI355:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI356:
	ret
LFE118:
.no_dead_strip _Xor__uXor_param_i16_i8
_Xor__uXor_param_i16_i8:
LFB119:
	pushq	%rbp
LCFI357:
	movq	%rsp, %rbp
LCFI358:
	movw	%di, -2(%rbp)
	movb	%sil, -3(%rbp)
	movswq	-2(%rbp), %rdx
	movsbq	-3(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI359:
	ret
LFE119:
.no_dead_strip _Xor__uXor_var_u64_u16
_Xor__uXor_var_u64_u16:
LFB120:
	pushq	%rbp
LCFI360:
	movq	%rsp, %rbp
LCFI361:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI362:
	ret
LFE120:
.no_dead_strip _Xor__uXor_param_u64_u16
_Xor__uXor_param_u64_u16:
LFB121:
	pushq	%rbp
LCFI363:
	movq	%rsp, %rbp
LCFI364:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI365:
	ret
LFE121:
.no_dead_strip _Xor__uXor_var_u64_u32
_Xor__uXor_var_u64_u32:
LFB122:
	pushq	%rbp
LCFI366:
	movq	%rsp, %rbp
LCFI367:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI368:
	ret
LFE122:
.no_dead_strip _Xor__uXor_param_u64_u32
_Xor__uXor_param_u64_u32:
LFB123:
	pushq	%rbp
LCFI369:
	movq	%rsp, %rbp
LCFI370:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI371:
	ret
LFE123:
.no_dead_strip _Xor__uXor_var_u64_LC
_Xor__uXor_var_u64_LC:
LFB124:
	pushq	%rbp
LCFI372:
	movq	%rsp, %rbp
LCFI373:
	movq	136+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI374:
	ret
LFE124:
.no_dead_strip _Xor__uXor_param_u64_LC
_Xor__uXor_param_u64_LC:
LFB125:
	pushq	%rbp
LCFI375:
	movq	%rsp, %rbp
LCFI376:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI377:
	ret
LFE125:
.no_dead_strip _Xor__uXor_var_u64_I
_Xor__uXor_var_u64_I:
LFB126:
	pushq	%rbp
LCFI378:
	movq	%rsp, %rbp
LCFI379:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI380:
	ret
LFE126:
.no_dead_strip _Xor__uXor_param_u64_I
_Xor__uXor_param_u64_I:
LFB127:
	pushq	%rbp
LCFI381:
	movq	%rsp, %rbp
LCFI382:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI383:
	ret
LFE127:
.no_dead_strip _Xor__uXor_var_u64_i16
_Xor__uXor_var_u64_i16:
LFB128:
	pushq	%rbp
LCFI384:
	movq	%rsp, %rbp
LCFI385:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI386:
	ret
LFE128:
.no_dead_strip _Xor__uXor_param_u64_i16
_Xor__uXor_param_u64_i16:
LFB129:
	pushq	%rbp
LCFI387:
	movq	%rsp, %rbp
LCFI388:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI389:
	ret
LFE129:
.no_dead_strip _Xor__uXor_var_u64_u64
_Xor__uXor_var_u64_u64:
LFB130:
	pushq	%rbp
LCFI390:
	movq	%rsp, %rbp
LCFI391:
	movq	136+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI392:
	ret
LFE130:
.no_dead_strip _Xor__uXor_param_u64_u64
_Xor__uXor_param_u64_u64:
LFB131:
	pushq	%rbp
LCFI393:
	movq	%rsp, %rbp
LCFI394:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI395:
	ret
LFE131:
.no_dead_strip _Xor__uXor_var_u64_i32
_Xor__uXor_var_u64_i32:
LFB132:
	pushq	%rbp
LCFI396:
	movq	%rsp, %rbp
LCFI397:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI398:
	ret
LFE132:
.no_dead_strip _Xor__uXor_param_u64_i32
_Xor__uXor_param_u64_i32:
LFB133:
	pushq	%rbp
LCFI399:
	movq	%rsp, %rbp
LCFI400:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movslq	%eax, %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI401:
	ret
LFE133:
.no_dead_strip _Xor__uXor_var_u64_C
_Xor__uXor_var_u64_C:
LFB134:
	pushq	%rbp
LCFI402:
	movq	%rsp, %rbp
LCFI403:
	movq	160+_MM_Xor(%rip), %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI404:
	ret
LFE134:
.no_dead_strip _Xor__uXor_param_u64_C
_Xor__uXor_param_u64_C:
LFB135:
	pushq	%rbp
LCFI405:
	movq	%rsp, %rbp
LCFI406:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI407:
	ret
LFE135:
.no_dead_strip _Xor__uXor_var_u64_u8
_Xor__uXor_var_u64_u8:
LFB136:
	pushq	%rbp
LCFI408:
	movq	%rsp, %rbp
LCFI409:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI410:
	ret
LFE136:
.no_dead_strip _Xor__uXor_param_u64_u8
_Xor__uXor_param_u64_u8:
LFB137:
	pushq	%rbp
LCFI411:
	movq	%rsp, %rbp
LCFI412:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI413:
	ret
LFE137:
.no_dead_strip _Xor__uXor_var_u64_i64
_Xor__uXor_var_u64_i64:
LFB138:
	pushq	%rbp
LCFI414:
	movq	%rsp, %rbp
LCFI415:
	movq	136+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI416:
	ret
LFE138:
.no_dead_strip _Xor__uXor_param_u64_i64
_Xor__uXor_param_u64_i64:
LFB139:
	pushq	%rbp
LCFI417:
	movq	%rsp, %rbp
LCFI418:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI419:
	ret
LFE139:
.no_dead_strip _Xor__uXor_var_u64_L
_Xor__uXor_var_u64_L:
LFB140:
	pushq	%rbp
LCFI420:
	movq	%rsp, %rbp
LCFI421:
	movq	136+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI422:
	ret
LFE140:
.no_dead_strip _Xor__uXor_param_u64_L
_Xor__uXor_param_u64_L:
LFB141:
	pushq	%rbp
LCFI423:
	movq	%rsp, %rbp
LCFI424:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI425:
	ret
LFE141:
.no_dead_strip _Xor__uXor_var_u64_i8
_Xor__uXor_var_u64_i8:
LFB142:
	pushq	%rbp
LCFI426:
	movq	%rsp, %rbp
LCFI427:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI428:
	ret
LFE142:
.no_dead_strip _Xor__uXor_param_u64_i8
_Xor__uXor_param_u64_i8:
LFB143:
	pushq	%rbp
LCFI429:
	movq	%rsp, %rbp
LCFI430:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI431:
	ret
LFE143:
.no_dead_strip _Xor__uXor_var_i32_u16
_Xor__uXor_var_i32_u16:
LFB144:
	pushq	%rbp
LCFI432:
	movq	%rsp, %rbp
LCFI433:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI434:
	ret
LFE144:
.no_dead_strip _Xor__uXor_param_i32_u16
_Xor__uXor_param_i32_u16:
LFB145:
	pushq	%rbp
LCFI435:
	movq	%rsp, %rbp
LCFI436:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzwl	-6(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI437:
	ret
LFE145:
.no_dead_strip _Xor__uXor_var_i32_u32
_Xor__uXor_var_i32_u32:
LFB146:
	pushq	%rbp
LCFI438:
	movq	%rsp, %rbp
LCFI439:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI440:
	ret
LFE146:
.no_dead_strip _Xor__uXor_param_i32_u32
_Xor__uXor_param_i32_u32:
LFB147:
	pushq	%rbp
LCFI441:
	movq	%rsp, %rbp
LCFI442:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI443:
	ret
LFE147:
.no_dead_strip _Xor__uXor_var_i32_LC
_Xor__uXor_var_i32_LC:
LFB148:
	pushq	%rbp
LCFI444:
	movq	%rsp, %rbp
LCFI445:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI446:
	ret
LFE148:
.no_dead_strip _Xor__uXor_param_i32_LC
_Xor__uXor_param_i32_LC:
LFB149:
	pushq	%rbp
LCFI447:
	movq	%rsp, %rbp
LCFI448:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	xorq	-16(%rbp), %rax
	leave
LCFI449:
	ret
LFE149:
.no_dead_strip _Xor__uXor_var_i32_I
_Xor__uXor_var_i32_I:
LFB150:
	pushq	%rbp
LCFI450:
	movq	%rsp, %rbp
LCFI451:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI452:
	ret
LFE150:
.no_dead_strip _Xor__uXor_param_i32_I
_Xor__uXor_param_i32_I:
LFB151:
	pushq	%rbp
LCFI453:
	movq	%rsp, %rbp
LCFI454:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI455:
	ret
LFE151:
.no_dead_strip _Xor__uXor_var_i32_i16
_Xor__uXor_var_i32_i16:
LFB152:
	pushq	%rbp
LCFI456:
	movq	%rsp, %rbp
LCFI457:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI458:
	ret
LFE152:
.no_dead_strip _Xor__uXor_param_i32_i16
_Xor__uXor_param_i32_i16:
LFB153:
	pushq	%rbp
LCFI459:
	movq	%rsp, %rbp
LCFI460:
	movl	%edi, -4(%rbp)
	movw	%si, -6(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movswq	-6(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI461:
	ret
LFE153:
.no_dead_strip _Xor__uXor_var_i32_u64
_Xor__uXor_var_i32_u64:
LFB154:
	pushq	%rbp
LCFI462:
	movq	%rsp, %rbp
LCFI463:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI464:
	ret
LFE154:
.no_dead_strip _Xor__uXor_param_i32_u64
_Xor__uXor_param_i32_u64:
LFB155:
	pushq	%rbp
LCFI465:
	movq	%rsp, %rbp
LCFI466:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI467:
	ret
LFE155:
.no_dead_strip _Xor__uXor_var_i32_i32
_Xor__uXor_var_i32_i32:
LFB156:
	pushq	%rbp
LCFI468:
	movq	%rsp, %rbp
LCFI469:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI470:
	ret
LFE156:
.no_dead_strip _Xor__uXor_param_i32_i32
_Xor__uXor_param_i32_i32:
LFB157:
	pushq	%rbp
LCFI471:
	movq	%rsp, %rbp
LCFI472:
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI473:
	ret
LFE157:
.no_dead_strip _Xor__uXor_var_i32_C
_Xor__uXor_var_i32_C:
LFB158:
	pushq	%rbp
LCFI474:
	movq	%rsp, %rbp
LCFI475:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI476:
	ret
LFE158:
.no_dead_strip _Xor__uXor_param_i32_C
_Xor__uXor_param_i32_C:
LFB159:
	pushq	%rbp
LCFI477:
	movq	%rsp, %rbp
LCFI478:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	cltq
	xorq	-16(%rbp), %rax
	leave
LCFI479:
	ret
LFE159:
.no_dead_strip _Xor__uXor_var_i32_u8
_Xor__uXor_var_i32_u8:
LFB160:
	pushq	%rbp
LCFI480:
	movq	%rsp, %rbp
LCFI481:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI482:
	ret
LFE160:
.no_dead_strip _Xor__uXor_param_i32_u8
_Xor__uXor_param_i32_u8:
LFB161:
	pushq	%rbp
LCFI483:
	movq	%rsp, %rbp
LCFI484:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movzbl	-5(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI485:
	ret
LFE161:
.no_dead_strip _Xor__uXor_var_i32_i64
_Xor__uXor_var_i32_i64:
LFB162:
	pushq	%rbp
LCFI486:
	movq	%rsp, %rbp
LCFI487:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI488:
	ret
LFE162:
.no_dead_strip _Xor__uXor_param_i32_i64
_Xor__uXor_param_i32_i64:
LFB163:
	pushq	%rbp
LCFI489:
	movq	%rsp, %rbp
LCFI490:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI491:
	ret
LFE163:
.no_dead_strip _Xor__uXor_var_i32_L
_Xor__uXor_var_i32_L:
LFB164:
	pushq	%rbp
LCFI492:
	movq	%rsp, %rbp
LCFI493:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI494:
	ret
LFE164:
.no_dead_strip _Xor__uXor_param_i32_L
_Xor__uXor_param_i32_L:
LFB165:
	pushq	%rbp
LCFI495:
	movq	%rsp, %rbp
LCFI496:
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI497:
	ret
LFE165:
.no_dead_strip _Xor__uXor_var_i32_i8
_Xor__uXor_var_i32_i8:
LFB166:
	pushq	%rbp
LCFI498:
	movq	%rsp, %rbp
LCFI499:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI500:
	ret
LFE166:
.no_dead_strip _Xor__uXor_param_i32_i8
_Xor__uXor_param_i32_i8:
LFB167:
	pushq	%rbp
LCFI501:
	movq	%rsp, %rbp
LCFI502:
	movl	%edi, -4(%rbp)
	movb	%sil, -5(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movsbq	-5(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI503:
	ret
LFE167:
.no_dead_strip _Xor__uXor_var_C_u16
_Xor__uXor_var_C_u16:
LFB168:
	pushq	%rbp
LCFI504:
	movq	%rsp, %rbp
LCFI505:
	movq	160+_MM_Xor(%rip), %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI506:
	ret
LFE168:
.no_dead_strip _Xor__uXor_param_C_u16
_Xor__uXor_param_C_u16:
LFB169:
	pushq	%rbp
LCFI507:
	movq	%rsp, %rbp
LCFI508:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI509:
	ret
LFE169:
.no_dead_strip _Xor__uXor_var_C_u32
_Xor__uXor_var_C_u32:
LFB170:
	pushq	%rbp
LCFI510:
	movq	%rsp, %rbp
LCFI511:
	movq	160+_MM_Xor(%rip), %rdx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI512:
	ret
LFE170:
.no_dead_strip _Xor__uXor_param_C_u32
_Xor__uXor_param_C_u32:
LFB171:
	pushq	%rbp
LCFI513:
	movq	%rsp, %rbp
LCFI514:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI515:
	ret
LFE171:
.no_dead_strip _Xor__uXor_var_C_LC
_Xor__uXor_var_C_LC:
LFB172:
	pushq	%rbp
LCFI516:
	movq	%rsp, %rbp
LCFI517:
	movq	160+_MM_Xor(%rip), %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI518:
	ret
LFE172:
.no_dead_strip _Xor__uXor_param_C_LC
_Xor__uXor_param_C_LC:
LFB173:
	pushq	%rbp
LCFI519:
	movq	%rsp, %rbp
LCFI520:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	xorq	%rdx, %rax
	leave
LCFI521:
	ret
LFE173:
.no_dead_strip _Xor__uXor_var_C_I
_Xor__uXor_var_C_I:
LFB174:
	pushq	%rbp
LCFI522:
	movq	%rsp, %rbp
LCFI523:
	movq	160+_MM_Xor(%rip), %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI524:
	ret
LFE174:
.no_dead_strip _Xor__uXor_param_C_I
_Xor__uXor_param_C_I:
LFB175:
	pushq	%rbp
LCFI525:
	movq	%rsp, %rbp
LCFI526:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI527:
	ret
LFE175:
.no_dead_strip _Xor__uXor_var_C_i16
_Xor__uXor_var_C_i16:
LFB176:
	pushq	%rbp
LCFI528:
	movq	%rsp, %rbp
LCFI529:
	movq	160+_MM_Xor(%rip), %rdx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI530:
	ret
LFE176:
.no_dead_strip _Xor__uXor_param_C_i16
_Xor__uXor_param_C_i16:
LFB177:
	pushq	%rbp
LCFI531:
	movq	%rsp, %rbp
LCFI532:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI533:
	ret
LFE177:
.no_dead_strip _Xor__uXor_var_C_u64
_Xor__uXor_var_C_u64:
LFB178:
	pushq	%rbp
LCFI534:
	movq	%rsp, %rbp
LCFI535:
	movq	160+_MM_Xor(%rip), %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI536:
	ret
LFE178:
.no_dead_strip _Xor__uXor_param_C_u64
_Xor__uXor_param_C_u64:
LFB179:
	pushq	%rbp
LCFI537:
	movq	%rsp, %rbp
LCFI538:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI539:
	ret
LFE179:
.no_dead_strip _Xor__uXor_var_C_i32
_Xor__uXor_var_C_i32:
LFB180:
	pushq	%rbp
LCFI540:
	movq	%rsp, %rbp
LCFI541:
	movq	160+_MM_Xor(%rip), %rdx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI542:
	ret
LFE180:
.no_dead_strip _Xor__uXor_param_C_i32
_Xor__uXor_param_C_i32:
LFB181:
	pushq	%rbp
LCFI543:
	movq	%rsp, %rbp
LCFI544:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	xorq	-8(%rbp), %rax
	leave
LCFI545:
	ret
LFE181:
.no_dead_strip _Xor__uXor_var_C_C
_Xor__uXor_var_C_C:
LFB182:
	pushq	%rbp
LCFI546:
	movq	%rsp, %rbp
LCFI547:
	movq	160+_MM_Xor(%rip), %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI548:
	ret
LFE182:
.no_dead_strip _Xor__uXor_param_C_C
_Xor__uXor_param_C_C:
LFB183:
	pushq	%rbp
LCFI549:
	movq	%rsp, %rbp
LCFI550:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rdx
	xorq	%rdx, %rax
	leave
LCFI551:
	ret
LFE183:
.no_dead_strip _Xor__uXor_var_C_u8
_Xor__uXor_var_C_u8:
LFB184:
	pushq	%rbp
LCFI552:
	movq	%rsp, %rbp
LCFI553:
	movq	160+_MM_Xor(%rip), %rdx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI554:
	ret
LFE184:
.no_dead_strip _Xor__uXor_param_C_u8
_Xor__uXor_param_C_u8:
LFB185:
	pushq	%rbp
LCFI555:
	movq	%rsp, %rbp
LCFI556:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %eax
	xorq	-8(%rbp), %rax
	leave
LCFI557:
	ret
LFE185:
.no_dead_strip _Xor__uXor_var_C_i64
_Xor__uXor_var_C_i64:
LFB186:
	pushq	%rbp
LCFI558:
	movq	%rsp, %rbp
LCFI559:
	movq	160+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI560:
	ret
LFE186:
.no_dead_strip _Xor__uXor_param_C_i64
_Xor__uXor_param_C_i64:
LFB187:
	pushq	%rbp
LCFI561:
	movq	%rsp, %rbp
LCFI562:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI563:
	ret
LFE187:
.no_dead_strip _Xor__uXor_var_C_L
_Xor__uXor_var_C_L:
LFB188:
	pushq	%rbp
LCFI564:
	movq	%rsp, %rbp
LCFI565:
	movq	160+_MM_Xor(%rip), %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI566:
	ret
LFE188:
.no_dead_strip _Xor__uXor_param_C_L
_Xor__uXor_param_C_L:
LFB189:
	pushq	%rbp
LCFI567:
	movq	%rsp, %rbp
LCFI568:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI569:
	ret
LFE189:
.no_dead_strip _Xor__uXor_var_C_i8
_Xor__uXor_var_C_i8:
LFB190:
	pushq	%rbp
LCFI570:
	movq	%rsp, %rbp
LCFI571:
	movq	160+_MM_Xor(%rip), %rdx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI572:
	ret
LFE190:
.no_dead_strip _Xor__uXor_param_C_i8
_Xor__uXor_param_C_i8:
LFB191:
	pushq	%rbp
LCFI573:
	movq	%rsp, %rbp
LCFI574:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rax
	xorq	-8(%rbp), %rax
	leave
LCFI575:
	ret
LFE191:
.no_dead_strip _Xor__uXor_var_u8_u16
_Xor__uXor_var_u8_u16:
LFB192:
	pushq	%rbp
LCFI576:
	movq	%rsp, %rbp
LCFI577:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI578:
	ret
LFE192:
.no_dead_strip _Xor__uXor_param_u8_u16
_Xor__uXor_param_u8_u16:
LFB193:
	pushq	%rbp
LCFI579:
	movq	%rsp, %rbp
LCFI580:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movzwl	-4(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI581:
	ret
LFE193:
.no_dead_strip _Xor__uXor_var_u8_u32
_Xor__uXor_var_u8_u32:
LFB194:
	pushq	%rbp
LCFI582:
	movq	%rsp, %rbp
LCFI583:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI584:
	ret
LFE194:
.no_dead_strip _Xor__uXor_param_u8_u32
_Xor__uXor_param_u8_u32:
LFB195:
	pushq	%rbp
LCFI585:
	movq	%rsp, %rbp
LCFI586:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI587:
	ret
LFE195:
.no_dead_strip _Xor__uXor_var_u8_LC
_Xor__uXor_var_u8_LC:
LFB196:
	pushq	%rbp
LCFI588:
	movq	%rsp, %rbp
LCFI589:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI590:
	ret
LFE196:
.no_dead_strip _Xor__uXor_param_u8_LC
_Xor__uXor_param_u8_LC:
LFB197:
	pushq	%rbp
LCFI591:
	movq	%rsp, %rbp
LCFI592:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI593:
	ret
LFE197:
.no_dead_strip _Xor__uXor_var_u8_I
_Xor__uXor_var_u8_I:
LFB198:
	pushq	%rbp
LCFI594:
	movq	%rsp, %rbp
LCFI595:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI596:
	ret
LFE198:
.no_dead_strip _Xor__uXor_param_u8_I
_Xor__uXor_param_u8_I:
LFB199:
	pushq	%rbp
LCFI597:
	movq	%rsp, %rbp
LCFI598:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI599:
	ret
LFE199:
.no_dead_strip _Xor__uXor_var_u8_i16
_Xor__uXor_var_u8_i16:
LFB200:
	pushq	%rbp
LCFI600:
	movq	%rsp, %rbp
LCFI601:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI602:
	ret
LFE200:
.no_dead_strip _Xor__uXor_param_u8_i16
_Xor__uXor_param_u8_i16:
LFB201:
	pushq	%rbp
LCFI603:
	movq	%rsp, %rbp
LCFI604:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movzbl	-1(%rbp), %edx
	movswq	-4(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI605:
	ret
LFE201:
.no_dead_strip _Xor__uXor_var_u8_u64
_Xor__uXor_var_u8_u64:
LFB202:
	pushq	%rbp
LCFI606:
	movq	%rsp, %rbp
LCFI607:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI608:
	ret
LFE202:
.no_dead_strip _Xor__uXor_param_u8_u64
_Xor__uXor_param_u8_u64:
LFB203:
	pushq	%rbp
LCFI609:
	movq	%rsp, %rbp
LCFI610:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI611:
	ret
LFE203:
.no_dead_strip _Xor__uXor_var_u8_i32
_Xor__uXor_var_u8_i32:
LFB204:
	pushq	%rbp
LCFI612:
	movq	%rsp, %rbp
LCFI613:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI614:
	ret
LFE204:
.no_dead_strip _Xor__uXor_param_u8_i32
_Xor__uXor_param_u8_i32:
LFB205:
	pushq	%rbp
LCFI615:
	movq	%rsp, %rbp
LCFI616:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movzbl	-1(%rbp), %edx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI617:
	ret
LFE205:
.no_dead_strip _Xor__uXor_var_u8_C
_Xor__uXor_var_u8_C:
LFB206:
	pushq	%rbp
LCFI618:
	movq	%rsp, %rbp
LCFI619:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI620:
	ret
LFE206:
.no_dead_strip _Xor__uXor_param_u8_C
_Xor__uXor_param_u8_C:
LFB207:
	pushq	%rbp
LCFI621:
	movq	%rsp, %rbp
LCFI622:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %eax
	xorq	-16(%rbp), %rax
	leave
LCFI623:
	ret
LFE207:
.no_dead_strip _Xor__uXor_var_u8_u8
_Xor__uXor_var_u8_u8:
LFB208:
	pushq	%rbp
LCFI624:
	movq	%rsp, %rbp
LCFI625:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI626:
	ret
LFE208:
.no_dead_strip _Xor__uXor_param_u8_u8
_Xor__uXor_param_u8_u8:
LFB209:
	pushq	%rbp
LCFI627:
	movq	%rsp, %rbp
LCFI628:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movzbl	-2(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI629:
	ret
LFE209:
.no_dead_strip _Xor__uXor_var_u8_i64
_Xor__uXor_var_u8_i64:
LFB210:
	pushq	%rbp
LCFI630:
	movq	%rsp, %rbp
LCFI631:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI632:
	ret
LFE210:
.no_dead_strip _Xor__uXor_param_u8_i64
_Xor__uXor_param_u8_i64:
LFB211:
	pushq	%rbp
LCFI633:
	movq	%rsp, %rbp
LCFI634:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI635:
	ret
LFE211:
.no_dead_strip _Xor__uXor_var_u8_L
_Xor__uXor_var_u8_L:
LFB212:
	pushq	%rbp
LCFI636:
	movq	%rsp, %rbp
LCFI637:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI638:
	ret
LFE212:
.no_dead_strip _Xor__uXor_param_u8_L
_Xor__uXor_param_u8_L:
LFB213:
	pushq	%rbp
LCFI639:
	movq	%rsp, %rbp
LCFI640:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movzbl	-1(%rbp), %edx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI641:
	ret
LFE213:
.no_dead_strip _Xor__uXor_var_u8_i8
_Xor__uXor_var_u8_i8:
LFB214:
	pushq	%rbp
LCFI642:
	movq	%rsp, %rbp
LCFI643:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI644:
	ret
LFE214:
.no_dead_strip _Xor__uXor_param_u8_i8
_Xor__uXor_param_u8_i8:
LFB215:
	pushq	%rbp
LCFI645:
	movq	%rsp, %rbp
LCFI646:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movzbl	-1(%rbp), %edx
	movsbq	-2(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI647:
	ret
LFE215:
.no_dead_strip _Xor__uXor_var_i64_u16
_Xor__uXor_var_i64_u16:
LFB216:
	pushq	%rbp
LCFI648:
	movq	%rsp, %rbp
LCFI649:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI650:
	ret
LFE216:
.no_dead_strip _Xor__uXor_param_i64_u16
_Xor__uXor_param_i64_u16:
LFB217:
	pushq	%rbp
LCFI651:
	movq	%rsp, %rbp
LCFI652:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI653:
	ret
LFE217:
.no_dead_strip _Xor__uXor_var_i64_u32
_Xor__uXor_var_i64_u32:
LFB218:
	pushq	%rbp
LCFI654:
	movq	%rsp, %rbp
LCFI655:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI656:
	ret
LFE218:
.no_dead_strip _Xor__uXor_param_i64_u32
_Xor__uXor_param_i64_u32:
LFB219:
	pushq	%rbp
LCFI657:
	movq	%rsp, %rbp
LCFI658:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI659:
	ret
LFE219:
.no_dead_strip _Xor__uXor_var_i64_LC
_Xor__uXor_var_i64_LC:
LFB220:
	pushq	%rbp
LCFI660:
	movq	%rsp, %rbp
LCFI661:
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI662:
	ret
LFE220:
.no_dead_strip _Xor__uXor_param_i64_LC
_Xor__uXor_param_i64_LC:
LFB221:
	pushq	%rbp
LCFI663:
	movq	%rsp, %rbp
LCFI664:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI665:
	ret
LFE221:
.no_dead_strip _Xor__uXor_var_i64_I
_Xor__uXor_var_i64_I:
LFB222:
	pushq	%rbp
LCFI666:
	movq	%rsp, %rbp
LCFI667:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI668:
	ret
LFE222:
.no_dead_strip _Xor__uXor_param_i64_I
_Xor__uXor_param_i64_I:
LFB223:
	pushq	%rbp
LCFI669:
	movq	%rsp, %rbp
LCFI670:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI671:
	ret
LFE223:
.no_dead_strip _Xor__uXor_var_i64_i16
_Xor__uXor_var_i64_i16:
LFB224:
	pushq	%rbp
LCFI672:
	movq	%rsp, %rbp
LCFI673:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI674:
	ret
LFE224:
.no_dead_strip _Xor__uXor_param_i64_i16
_Xor__uXor_param_i64_i16:
LFB225:
	pushq	%rbp
LCFI675:
	movq	%rsp, %rbp
LCFI676:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI677:
	ret
LFE225:
.no_dead_strip _Xor__uXor_var_i64_u64
_Xor__uXor_var_i64_u64:
LFB226:
	pushq	%rbp
LCFI678:
	movq	%rsp, %rbp
LCFI679:
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI680:
	ret
LFE226:
.no_dead_strip _Xor__uXor_param_i64_u64
_Xor__uXor_param_i64_u64:
LFB227:
	pushq	%rbp
LCFI681:
	movq	%rsp, %rbp
LCFI682:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI683:
	ret
LFE227:
.no_dead_strip _Xor__uXor_var_i64_i32
_Xor__uXor_var_i64_i32:
LFB228:
	pushq	%rbp
LCFI684:
	movq	%rsp, %rbp
LCFI685:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI686:
	ret
LFE228:
.no_dead_strip _Xor__uXor_param_i64_i32
_Xor__uXor_param_i64_i32:
LFB229:
	pushq	%rbp
LCFI687:
	movq	%rsp, %rbp
LCFI688:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movslq	%eax, %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI689:
	ret
LFE229:
.no_dead_strip _Xor__uXor_var_i64_C
_Xor__uXor_var_i64_C:
LFB230:
	pushq	%rbp
LCFI690:
	movq	%rsp, %rbp
LCFI691:
	movq	160+_MM_Xor(%rip), %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI692:
	ret
LFE230:
.no_dead_strip _Xor__uXor_param_i64_C
_Xor__uXor_param_i64_C:
LFB231:
	pushq	%rbp
LCFI693:
	movq	%rsp, %rbp
LCFI694:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI695:
	ret
LFE231:
.no_dead_strip _Xor__uXor_var_i64_u8
_Xor__uXor_var_i64_u8:
LFB232:
	pushq	%rbp
LCFI696:
	movq	%rsp, %rbp
LCFI697:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI698:
	ret
LFE232:
.no_dead_strip _Xor__uXor_param_i64_u8
_Xor__uXor_param_i64_u8:
LFB233:
	pushq	%rbp
LCFI699:
	movq	%rsp, %rbp
LCFI700:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI701:
	ret
LFE233:
.no_dead_strip _Xor__uXor_var_i64_i64
_Xor__uXor_var_i64_i64:
LFB234:
	pushq	%rbp
LCFI702:
	movq	%rsp, %rbp
LCFI703:
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI704:
	ret
LFE234:
.no_dead_strip _Xor__uXor_param_i64_i64
_Xor__uXor_param_i64_i64:
LFB235:
	pushq	%rbp
LCFI705:
	movq	%rsp, %rbp
LCFI706:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI707:
	ret
LFE235:
.no_dead_strip _Xor__uXor_var_i64_L
_Xor__uXor_var_i64_L:
LFB236:
	pushq	%rbp
LCFI708:
	movq	%rsp, %rbp
LCFI709:
	movq	176+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI710:
	ret
LFE236:
.no_dead_strip _Xor__uXor_param_i64_L
_Xor__uXor_param_i64_L:
LFB237:
	pushq	%rbp
LCFI711:
	movq	%rsp, %rbp
LCFI712:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI713:
	ret
LFE237:
.no_dead_strip _Xor__uXor_var_i64_i8
_Xor__uXor_var_i64_i8:
LFB238:
	pushq	%rbp
LCFI714:
	movq	%rsp, %rbp
LCFI715:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI716:
	ret
LFE238:
.no_dead_strip _Xor__uXor_param_i64_i8
_Xor__uXor_param_i64_i8:
LFB239:
	pushq	%rbp
LCFI717:
	movq	%rsp, %rbp
LCFI718:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI719:
	ret
LFE239:
.no_dead_strip _Xor__uXor_var_L_u16
_Xor__uXor_var_L_u16:
LFB240:
	pushq	%rbp
LCFI720:
	movq	%rsp, %rbp
LCFI721:
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI722:
	ret
LFE240:
.no_dead_strip _Xor__uXor_param_L_u16
_Xor__uXor_param_L_u16:
LFB241:
	pushq	%rbp
LCFI723:
	movq	%rsp, %rbp
LCFI724:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movzwl	-10(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI725:
	ret
LFE241:
.no_dead_strip _Xor__uXor_var_L_u32
_Xor__uXor_var_L_u32:
LFB242:
	pushq	%rbp
LCFI726:
	movq	%rsp, %rbp
LCFI727:
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI728:
	ret
LFE242:
.no_dead_strip _Xor__uXor_param_L_u32
_Xor__uXor_param_L_u32:
LFB243:
	pushq	%rbp
LCFI729:
	movq	%rsp, %rbp
LCFI730:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	mov	-12(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI731:
	ret
LFE243:
.no_dead_strip _Xor__uXor_var_L_LC
_Xor__uXor_var_L_LC:
LFB244:
	pushq	%rbp
LCFI732:
	movq	%rsp, %rbp
LCFI733:
	movq	192+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI734:
	ret
LFE244:
.no_dead_strip _Xor__uXor_param_L_LC
_Xor__uXor_param_L_LC:
LFB245:
	pushq	%rbp
LCFI735:
	movq	%rsp, %rbp
LCFI736:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI737:
	ret
LFE245:
.no_dead_strip _Xor__uXor_var_L_I
_Xor__uXor_var_L_I:
LFB246:
	pushq	%rbp
LCFI738:
	movq	%rsp, %rbp
LCFI739:
	movq	120+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI740:
	ret
LFE246:
.no_dead_strip _Xor__uXor_param_L_I
_Xor__uXor_param_L_I:
LFB247:
	pushq	%rbp
LCFI741:
	movq	%rsp, %rbp
LCFI742:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI743:
	ret
LFE247:
.no_dead_strip _Xor__uXor_var_L_i16
_Xor__uXor_var_L_i16:
LFB248:
	pushq	%rbp
LCFI744:
	movq	%rsp, %rbp
LCFI745:
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI746:
	ret
LFE248:
.no_dead_strip _Xor__uXor_param_L_i16
_Xor__uXor_param_L_i16:
LFB249:
	pushq	%rbp
LCFI747:
	movq	%rsp, %rbp
LCFI748:
	movq	%rdi, -8(%rbp)
	movw	%si, -10(%rbp)
	movswq	-10(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI749:
	ret
LFE249:
.no_dead_strip _Xor__uXor_var_L_u64
_Xor__uXor_var_L_u64:
LFB250:
	pushq	%rbp
LCFI750:
	movq	%rsp, %rbp
LCFI751:
	movq	192+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI752:
	ret
LFE250:
.no_dead_strip _Xor__uXor_param_L_u64
_Xor__uXor_param_L_u64:
LFB251:
	pushq	%rbp
LCFI753:
	movq	%rsp, %rbp
LCFI754:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI755:
	ret
LFE251:
.no_dead_strip _Xor__uXor_var_L_i32
_Xor__uXor_var_L_i32:
LFB252:
	pushq	%rbp
LCFI756:
	movq	%rsp, %rbp
LCFI757:
	movl	152+_MM_Xor(%rip), %eax
	movslq	%eax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI758:
	ret
LFE252:
.no_dead_strip _Xor__uXor_param_L_i32
_Xor__uXor_param_L_i32:
LFB253:
	pushq	%rbp
LCFI759:
	movq	%rsp, %rbp
LCFI760:
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movslq	%eax, %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI761:
	ret
LFE253:
.no_dead_strip _Xor__uXor_var_L_C
_Xor__uXor_var_L_C:
LFB254:
	pushq	%rbp
LCFI762:
	movq	%rsp, %rbp
LCFI763:
	movq	160+_MM_Xor(%rip), %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI764:
	ret
LFE254:
.no_dead_strip _Xor__uXor_param_L_C
_Xor__uXor_param_L_C:
LFB255:
	pushq	%rbp
LCFI765:
	movq	%rsp, %rbp
LCFI766:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI767:
	ret
LFE255:
.no_dead_strip _Xor__uXor_var_L_u8
_Xor__uXor_var_L_u8:
LFB256:
	pushq	%rbp
LCFI768:
	movq	%rsp, %rbp
LCFI769:
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %edx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI770:
	ret
LFE256:
.no_dead_strip _Xor__uXor_param_L_u8
_Xor__uXor_param_L_u8:
LFB257:
	pushq	%rbp
LCFI771:
	movq	%rsp, %rbp
LCFI772:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movzbl	-9(%rbp), %edx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI773:
	ret
LFE257:
.no_dead_strip _Xor__uXor_var_L_i64
_Xor__uXor_var_L_i64:
LFB258:
	pushq	%rbp
LCFI774:
	movq	%rsp, %rbp
LCFI775:
	movq	192+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI776:
	ret
LFE258:
.no_dead_strip _Xor__uXor_param_L_i64
_Xor__uXor_param_L_i64:
LFB259:
	pushq	%rbp
LCFI777:
	movq	%rsp, %rbp
LCFI778:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI779:
	ret
LFE259:
.no_dead_strip _Xor__uXor_var_L_L
_Xor__uXor_var_L_L:
LFB260:
	pushq	%rbp
LCFI780:
	movq	%rsp, %rbp
LCFI781:
	movq	192+_MM_Xor(%rip), %rax
	movq	%rax, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI782:
	ret
LFE260:
.no_dead_strip _Xor__uXor_param_L_L
_Xor__uXor_param_L_L:
LFB261:
	pushq	%rbp
LCFI783:
	movq	%rsp, %rbp
LCFI784:
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI785:
	ret
LFE261:
.no_dead_strip _Xor__uXor_var_L_i8
_Xor__uXor_var_L_i8:
LFB262:
	pushq	%rbp
LCFI786:
	movq	%rsp, %rbp
LCFI787:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI788:
	ret
LFE262:
.no_dead_strip _Xor__uXor_param_L_i8
_Xor__uXor_param_L_i8:
LFB263:
	pushq	%rbp
LCFI789:
	movq	%rsp, %rbp
LCFI790:
	movq	%rdi, -8(%rbp)
	movb	%sil, -9(%rbp)
	movsbq	-9(%rbp), %rdx
	movq	-8(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI791:
	ret
LFE263:
.no_dead_strip _Xor__uXor_var_i8_u16
_Xor__uXor_var_i8_u16:
LFB264:
	pushq	%rbp
LCFI792:
	movq	%rsp, %rbp
LCFI793:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movzwl	104+_MM_Xor(%rip), %eax
	movzwl	%ax, %eax
	xorq	%rdx, %rax
	leave
LCFI794:
	ret
LFE264:
.no_dead_strip _Xor__uXor_param_i8_u16
_Xor__uXor_param_i8_u16:
LFB265:
	pushq	%rbp
LCFI795:
	movq	%rsp, %rbp
LCFI796:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movzwl	-4(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI797:
	ret
LFE265:
.no_dead_strip _Xor__uXor_var_i8_u32
_Xor__uXor_var_i8_u32:
LFB266:
	pushq	%rbp
LCFI798:
	movq	%rsp, %rbp
LCFI799:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movl	108+_MM_Xor(%rip), %eax
	mov	%eax, %eax
	xorq	%rdx, %rax
	leave
LCFI800:
	ret
LFE266:
.no_dead_strip _Xor__uXor_param_i8_u32
_Xor__uXor_param_i8_u32:
LFB267:
	pushq	%rbp
LCFI801:
	movq	%rsp, %rbp
LCFI802:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	mov	-8(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI803:
	ret
LFE267:
.no_dead_strip _Xor__uXor_var_i8_LC
_Xor__uXor_var_i8_LC:
LFB268:
	pushq	%rbp
LCFI804:
	movq	%rsp, %rbp
LCFI805:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	112+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI806:
	ret
LFE268:
.no_dead_strip _Xor__uXor_param_i8_LC
_Xor__uXor_param_i8_LC:
LFB269:
	pushq	%rbp
LCFI807:
	movq	%rsp, %rbp
LCFI808:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI809:
	ret
LFE269:
.no_dead_strip _Xor__uXor_var_i8_I
_Xor__uXor_var_i8_I:
LFB270:
	pushq	%rbp
LCFI810:
	movq	%rsp, %rbp
LCFI811:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	120+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI812:
	ret
LFE270:
.no_dead_strip _Xor__uXor_param_i8_I
_Xor__uXor_param_i8_I:
LFB271:
	pushq	%rbp
LCFI813:
	movq	%rsp, %rbp
LCFI814:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI815:
	ret
LFE271:
.no_dead_strip _Xor__uXor_var_i8_i16
_Xor__uXor_var_i8_i16:
LFB272:
	pushq	%rbp
LCFI816:
	movq	%rsp, %rbp
LCFI817:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movzwl	128+_MM_Xor(%rip), %eax
	movswq	%ax, %rax
	xorq	%rdx, %rax
	leave
LCFI818:
	ret
LFE272:
.no_dead_strip _Xor__uXor_param_i8_i16
_Xor__uXor_param_i8_i16:
LFB273:
	pushq	%rbp
LCFI819:
	movq	%rsp, %rbp
LCFI820:
	movb	%dil, -1(%rbp)
	movw	%si, -4(%rbp)
	movsbq	-1(%rbp), %rdx
	movswq	-4(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI821:
	ret
LFE273:
.no_dead_strip _Xor__uXor_var_i8_u64
_Xor__uXor_var_i8_u64:
LFB274:
	pushq	%rbp
LCFI822:
	movq	%rsp, %rbp
LCFI823:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	136+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI824:
	ret
LFE274:
.no_dead_strip _Xor__uXor_param_i8_u64
_Xor__uXor_param_i8_u64:
LFB275:
	pushq	%rbp
LCFI825:
	movq	%rsp, %rbp
LCFI826:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI827:
	ret
LFE275:
.no_dead_strip _Xor__uXor_var_i8_i32
_Xor__uXor_var_i8_i32:
LFB276:
	pushq	%rbp
LCFI828:
	movq	%rsp, %rbp
LCFI829:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movl	152+_MM_Xor(%rip), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI830:
	ret
LFE276:
.no_dead_strip _Xor__uXor_param_i8_i32
_Xor__uXor_param_i8_i32:
LFB277:
	pushq	%rbp
LCFI831:
	movq	%rsp, %rbp
LCFI832:
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	movsbq	-1(%rbp), %rdx
	movl	-8(%rbp), %eax
	cltq
	xorq	%rdx, %rax
	leave
LCFI833:
	ret
LFE277:
.no_dead_strip _Xor__uXor_var_i8_C
_Xor__uXor_var_i8_C:
LFB278:
	pushq	%rbp
LCFI834:
	movq	%rsp, %rbp
LCFI835:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	160+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI836:
	ret
LFE278:
.no_dead_strip _Xor__uXor_param_i8_C
_Xor__uXor_param_i8_C:
LFB279:
	pushq	%rbp
LCFI837:
	movq	%rsp, %rbp
LCFI838:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rax
	xorq	-16(%rbp), %rax
	leave
LCFI839:
	ret
LFE279:
.no_dead_strip _Xor__uXor_var_i8_u8
_Xor__uXor_var_i8_u8:
LFB280:
	pushq	%rbp
LCFI840:
	movq	%rsp, %rbp
LCFI841:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movzbl	168+_MM_Xor(%rip), %eax
	movzbl	%al, %eax
	xorq	%rdx, %rax
	leave
LCFI842:
	ret
LFE280:
.no_dead_strip _Xor__uXor_param_i8_u8
_Xor__uXor_param_i8_u8:
LFB281:
	pushq	%rbp
LCFI843:
	movq	%rsp, %rbp
LCFI844:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movzbl	-2(%rbp), %eax
	xorq	%rdx, %rax
	leave
LCFI845:
	ret
LFE281:
.no_dead_strip _Xor__uXor_var_i8_i64
_Xor__uXor_var_i8_i64:
LFB282:
	pushq	%rbp
LCFI846:
	movq	%rsp, %rbp
LCFI847:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	176+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI848:
	ret
LFE282:
.no_dead_strip _Xor__uXor_param_i8_i64
_Xor__uXor_param_i8_i64:
LFB283:
	pushq	%rbp
LCFI849:
	movq	%rsp, %rbp
LCFI850:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI851:
	ret
LFE283:
.no_dead_strip _Xor__uXor_var_i8_L
_Xor__uXor_var_i8_L:
LFB284:
	pushq	%rbp
LCFI852:
	movq	%rsp, %rbp
LCFI853:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movq	192+_MM_Xor(%rip), %rax
	xorq	%rdx, %rax
	leave
LCFI854:
	ret
LFE284:
.no_dead_strip _Xor__uXor_param_i8_L
_Xor__uXor_param_i8_L:
LFB285:
	pushq	%rbp
LCFI855:
	movq	%rsp, %rbp
LCFI856:
	movb	%dil, -1(%rbp)
	movq	%rsi, -16(%rbp)
	movsbq	-1(%rbp), %rdx
	movq	-16(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI857:
	ret
LFE285:
.no_dead_strip _Xor__uXor_var_i8_i8
_Xor__uXor_var_i8_i8:
LFB286:
	pushq	%rbp
LCFI858:
	movq	%rsp, %rbp
LCFI859:
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rdx
	movzbl	200+_MM_Xor(%rip), %eax
	movsbq	%al, %rax
	xorq	%rdx, %rax
	leave
LCFI860:
	ret
LFE286:
.no_dead_strip _Xor__uXor_param_i8_i8
_Xor__uXor_param_i8_i8:
LFB287:
	pushq	%rbp
LCFI861:
	movq	%rsp, %rbp
LCFI862:
	movb	%dil, -1(%rbp)
	movb	%sil, -2(%rbp)
	movsbq	-1(%rbp), %rdx
	movsbq	-2(%rbp), %rax
	xorq	%rdx, %rax
	leave
LCFI863:
	ret
LFE287:
.globl _Xor_M3
.no_dead_strip _Xor_M3
_Xor_M3:
LFB288:
	pushq	%rbp
LCFI864:
	movq	%rsp, %rbp
LCFI865:
	movq	%rdi, -8(%rbp)
	leaq	_MM_Xor(%rip), %rax
	leave
LCFI866:
	ret
LFE288:
	.const_data
	.align 5
_L_1:
	.ascii "Xor_M3"
	.space 1
	.ascii "uXor_param_i8_i8"
	.space 1
	.ascii "uXor_var_i8_i8"
	.space 1
	.ascii "uXor_param_i8_L"
	.space 1
	.ascii "uXor_var_i8_L"
	.space 1
	.ascii "uXor_param_i8_i64"
	.space 1
	.ascii "uXor_var_i8_i64"
	.space 1
	.ascii "uXor_param_i8_u8"
	.space 1
	.ascii "uXor_var_i8_u8"
	.space 1
	.ascii "uXor_param_i8_C"
	.space 1
	.ascii "uXor_var_i8_C"
	.space 1
	.ascii "uXor_param_i8_i32"
	.space 1
	.ascii "uXor_var_i8_i32"
	.space 1
	.ascii "uXor_param_i8_u64"
	.space 1
	.ascii "uXor_var_i8_u64"
	.space 1
	.ascii "uXor_param_i8_i16"
	.space 1
	.ascii "uXor_var_i8_i16"
	.space 1
	.ascii "uXor_param_i8_I"
	.space 1
	.ascii "uXor_var_i8_I"
	.space 1
	.ascii "uXor_param_i8_LC"
	.space 1
	.ascii "uXor_var_i8_LC"
	.space 1
	.ascii "uXor_param_i8_u32"
	.space 1
	.ascii "uXor_var_i8_u32"
	.space 1
	.ascii "uXor_param_i8_u16"
	.space 1
	.ascii "uXor_var_i8_u16"
	.space 1
	.ascii "uXor_param_L_i8"
	.space 1
	.ascii "uXor_var_L_i8"
	.space 1
	.ascii "uXor_param_L_L"
	.space 1
	.ascii "uXor_var_L_L"
	.space 1
	.ascii "uXor_param_L_i64"
	.space 1
	.ascii "uXor_var_L_i64"
	.space 1
	.ascii "uXor_param_L_u8"
	.space 1
	.ascii "uXor_var_L_u8"
	.space 1
	.ascii "uXor_param_L_C"
	.space 1
	.ascii "uXor_var_L_C"
	.space 1
	.ascii "uXor_param_L_i32"
	.space 1
	.ascii "uXor_var_L_i32"
	.space 1
	.ascii "uXor_param_L_u64"
	.space 1
	.ascii "uXor_var_L_u64"
	.space 1
	.ascii "uXor_param_L_i16"
	.space 1
	.ascii "uXor_var_L_i16"
	.space 1
	.ascii "uXor_param_L_I"
	.space 1
	.ascii "uXor_var_L_I"
	.space 1
	.ascii "uXor_param_L_LC"
	.space 1
	.ascii "uXor_var_L_LC"
	.space 1
	.ascii "uXor_param_L_u32"
	.space 1
	.ascii "uXor_var_L_u32"
	.space 1
	.ascii "uXor_param_L_u16"
	.space 1
	.ascii "uXor_var_L_u16"
	.space 1
	.ascii "uXor_param_i64_i8"
	.space 1
	.ascii "uXor_var_i64_i8"
	.space 1
	.ascii "uXor_param_i64_L"
	.space 1
	.ascii "uXor_var_i64_L"
	.space 1
	.ascii "uXor_param_i64_i64"
	.space 1
	.ascii "uXor_var_i64_i64"
	.space 1
	.ascii "uXor_param_i64_u8"
	.space 1
	.ascii "uXor_var_i64_u8"
	.space 1
	.ascii "uXor_param_i64_C"
	.space 1
	.ascii "uXor_var_i64_C"
	.space 1
	.ascii "uXor_param_i64_i32"
	.space 1
	.ascii "uXor_var_i64_i32"
	.space 1
	.ascii "uXor_param_i64_u64"
	.space 1
	.ascii "uXor_var_i64_u64"
	.space 1
	.ascii "uXor_param_i64_i16"
	.space 1
	.ascii "uXor_var_i64_i16"
	.space 1
	.ascii "uXor_param_i64_I"
	.space 1
	.ascii "uXor_var_i64_I"
	.space 1
	.ascii "uXor_param_i64_LC"
	.space 1
	.ascii "uXor_var_i64_LC"
	.space 1
	.ascii "uXor_param_i64_u32"
	.space 1
	.ascii "uXor_var_i64_u32"
	.space 1
	.ascii "uXor_param_i64_u16"
	.space 1
	.ascii "uXor_var_i64_u16"
	.space 1
	.ascii "uXor_param_u8_i8"
	.space 1
	.ascii "uXor_var_u8_i8"
	.space 1
	.ascii "uXor_param_u8_L"
	.space 1
	.ascii "uXor_var_u8_L"
	.space 1
	.ascii "uXor_param_u8_i64"
	.space 1
	.ascii "uXor_var_u8_i64"
	.space 1
	.ascii "uXor_param_u8_u8"
	.space 1
	.ascii "uXor_var_u8_u8"
	.space 1
	.ascii "uXor_param_u8_C"
	.space 1
	.ascii "uXor_var_u8_C"
	.space 1
	.ascii "uXor_param_u8_i32"
	.space 1
	.ascii "uXor_var_u8_i32"
	.space 1
	.ascii "uXor_param_u8_u64"
	.space 1
	.ascii "uXor_var_u8_u64"
	.space 1
	.ascii "uXor_param_u8_i16"
	.space 1
	.ascii "uXor_var_u8_i16"
	.space 1
	.ascii "uXor_param_u8_I"
	.space 1
	.ascii "uXor_var_u8_I"
	.space 1
	.ascii "uXor_param_u8_LC"
	.space 1
	.ascii "uXor_var_u8_LC"
	.space 1
	.ascii "uXor_param_u8_u32"
	.space 1
	.ascii "uXor_var_u8_u32"
	.space 1
	.ascii "uXor_param_u8_u16"
	.space 1
	.ascii "uXor_var_u8_u16"
	.space 1
	.ascii "uXor_param_C_i8"
	.space 1
	.ascii "uXor_var_C_i8"
	.space 1
	.ascii "uXor_param_C_L"
	.space 1
	.ascii "uXor_var_C_L"
	.space 1
	.ascii "uXor_param_C_i64"
	.space 1
	.ascii "uXor_var_C_i64"
	.space 1
	.ascii "uXor_param_C_u8"
	.space 1
	.ascii "uXor_var_C_u8"
	.space 1
	.ascii "uXor_param_C_C"
	.space 1
	.ascii "uXor_var_C_C"
	.space 1
	.ascii "uXor_param_C_i32"
	.space 1
	.ascii "uXor_var_C_i32"
	.space 1
	.ascii "uXor_param_C_u64"
	.space 1
	.ascii "uXor_var_C_u64"
	.space 1
	.ascii "uXor_param_C_i16"
	.space 1
	.ascii "uXor_var_C_i16"
	.space 1
	.ascii "uXor_param_C_I"
	.space 1
	.ascii "uXor_var_C_I"
	.space 1
	.ascii "uXor_param_C_LC"
	.space 1
	.ascii "uXor_var_C_LC"
	.space 1
	.ascii "uXor_param_C_u32"
	.space 1
	.ascii "uXor_var_C_u32"
	.space 1
	.ascii "uXor_param_C_u16"
	.space 1
	.ascii "uXor_var_C_u16"
	.space 1
	.ascii "uXor_param_i32_i8"
	.space 1
	.ascii "uXor_var_i32_i8"
	.space 1
	.ascii "uXor_param_i32_L"
	.space 1
	.ascii "uXor_var_i32_L"
	.space 1
	.ascii "uXor_param_i32_i64"
	.space 1
	.ascii "uXor_var_i32_i64"
	.space 1
	.ascii "uXor_param_i32_u8"
	.space 1
	.ascii "uXor_var_i32_u8"
	.space 1
	.ascii "uXor_param_i32_C"
	.space 1
	.ascii "uXor_var_i32_C"
	.space 1
	.ascii "uXor_param_i32_i32"
	.space 1
	.ascii "uXor_var_i32_i32"
	.space 1
	.ascii "uXor_param_i32_u64"
	.space 1
	.ascii "uXor_var_i32_u64"
	.space 1
	.ascii "uXor_param_i32_i16"
	.space 1
	.ascii "uXor_var_i32_i16"
	.space 1
	.ascii "uXor_param_i32_I"
	.space 1
	.ascii "uXor_var_i32_I"
	.space 1
	.ascii "uXor_param_i32_LC"
	.space 1
	.ascii "uXor_var_i32_LC"
	.space 1
	.ascii "uXor_param_i32_u32"
	.space 1
	.ascii "uXor_var_i32_u32"
	.space 1
	.ascii "uXor_param_i32_u16"
	.space 1
	.ascii "uXor_var_i32_u16"
	.space 1
	.ascii "uXor_param_u64_i8"
	.space 1
	.ascii "uXor_var_u64_i8"
	.space 1
	.ascii "uXor_param_u64_L"
	.space 1
	.ascii "uXor_var_u64_L"
	.space 1
	.ascii "uXor_param_u64_i64"
	.space 1
	.ascii "uXor_var_u64_i64"
	.space 1
	.ascii "uXor_param_u64_u8"
	.space 1
	.ascii "uXor_var_u64_u8"
	.space 1
	.ascii "uXor_param_u64_C"
	.space 1
	.ascii "uXor_var_u64_C"
	.space 1
	.ascii "uXor_param_u64_i32"
	.space 1
	.ascii "uXor_var_u64_i32"
	.space 1
	.ascii "uXor_param_u64_u64"
	.space 1
	.ascii "uXor_var_u64_u64"
	.space 1
	.ascii "uXor_param_u64_i16"
	.space 1
	.ascii "uXor_var_u64_i16"
	.space 1
	.ascii "uXor_param_u64_I"
	.space 1
	.ascii "uXor_var_u64_I"
	.space 1
	.ascii "uXor_param_u64_LC"
	.space 1
	.ascii "uXor_var_u64_LC"
	.space 1
	.ascii "uXor_param_u64_u32"
	.space 1
	.ascii "uXor_var_u64_u32"
	.space 1
	.ascii "uXor_param_u64_u16"
	.space 1
	.ascii "uXor_var_u64_u16"
	.space 1
	.ascii "uXor_param_i16_i8"
	.space 1
	.ascii "uXor_var_i16_i8"
	.space 1
	.ascii "uXor_param_i16_L"
	.space 1
	.ascii "uXor_var_i16_L"
	.space 1
	.ascii "uXor_param_i16_i64"
	.space 1
	.ascii "uXor_var_i16_i64"
	.space 1
	.ascii "uXor_param_i16_u8"
	.space 1
	.ascii "uXor_var_i16_u8"
	.space 1
	.ascii "uXor_param_i16_C"
	.space 1
	.ascii "uXor_var_i16_C"
	.space 1
	.ascii "uXor_param_i16_i32"
	.space 1
	.ascii "uXor_var_i16_i32"
	.space 1
	.ascii "uXor_param_i16_u64"
	.space 1
	.ascii "uXor_var_i16_u64"
	.space 1
	.ascii "uXor_param_i16_i16"
	.space 1
	.ascii "uXor_var_i16_i16"
	.space 1
	.ascii "uXor_param_i16_I"
	.space 1
	.ascii "uXor_var_i16_I"
	.space 1
	.ascii "uXor_param_i16_LC"
	.space 1
	.ascii "uXor_var_i16_LC"
	.space 1
	.ascii "uXor_param_i16_u32"
	.space 1
	.ascii "uXor_var_i16_u32"
	.space 1
	.ascii "uXor_param_i16_u16"
	.space 1
	.ascii "uXor_var_i16_u16"
	.space 1
	.ascii "uXor_param_I_i8"
	.space 1
	.ascii "uXor_var_I_i8"
	.space 1
	.ascii "uXor_param_I_L"
	.space 1
	.ascii "uXor_var_I_L"
	.space 1
	.ascii "uXor_param_I_i64"
	.space 1
	.ascii "uXor_var_I_i64"
	.space 1
	.ascii "uXor_param_I_u8"
	.space 1
	.ascii "uXor_var_I_u8"
	.space 1
	.ascii "uXor_param_I_C"
	.space 1
	.ascii "uXor_var_I_C"
	.space 1
	.ascii "uXor_param_I_i32"
	.space 1
	.ascii "uXor_var_I_i32"
	.space 1
	.ascii "uXor_param_I_u64"
	.space 1
	.ascii "uXor_var_I_u64"
	.space 1
	.ascii "uXor_param_I_i16"
	.space 1
	.ascii "uXor_var_I_i16"
	.space 1
	.ascii "uXor_param_I_I"
	.space 1
	.ascii "uXor_var_I_I"
	.space 1
	.ascii "uXor_param_I_LC"
	.space 1
	.ascii "uXor_var_I_LC"
	.space 1
	.ascii "uXor_param_I_u32"
	.space 1
	.ascii "uXor_var_I_u32"
	.space 1
	.ascii "uXor_param_I_u16"
	.space 1
	.ascii "uXor_var_I_u16"
	.space 1
	.ascii "uXor_param_LC_i8"
	.space 1
	.ascii "uXor_var_LC_i8"
	.space 1
	.ascii "uXor_param_LC_L"
	.space 1
	.ascii "uXor_var_LC_L"
	.space 1
	.ascii "uXor_param_LC_i64"
	.space 1
	.ascii "uXor_var_LC_i64"
	.space 1
	.ascii "uXor_param_LC_u8"
	.space 1
	.ascii "uXor_var_LC_u8"
	.space 1
	.ascii "uXor_param_LC_C"
	.space 1
	.ascii "uXor_var_LC_C"
	.space 1
	.ascii "uXor_param_LC_i32"
	.space 1
	.ascii "uXor_var_LC_i32"
	.space 1
	.ascii "uXor_param_LC_u64"
	.space 1
	.ascii "uXor_var_LC_u64"
	.space 1
	.ascii "uXor_param_LC_i16"
	.space 1
	.ascii "uXor_var_LC_i16"
	.space 1
	.ascii "uXor_param_LC_I"
	.space 1
	.ascii "uXor_var_LC_I"
	.space 1
	.ascii "uXor_param_LC_LC"
	.space 1
	.ascii "uXor_var_LC_LC"
	.space 1
	.ascii "uXor_param_LC_u32"
	.space 1
	.ascii "uXor_var_LC_u32"
	.space 1
	.ascii "uXor_param_LC_u16"
	.space 1
	.ascii "uXor_var_LC_u16"
	.space 1
	.ascii "uXor_param_u32_i8"
	.space 1
	.ascii "uXor_var_u32_i8"
	.space 1
	.ascii "uXor_param_u32_L"
	.space 1
	.ascii "uXor_var_u32_L"
	.space 1
	.ascii "uXor_param_u32_i64"
	.space 1
	.ascii "uXor_var_u32_i64"
	.space 1
	.ascii "uXor_param_u32_u8"
	.space 1
	.ascii "uXor_var_u32_u8"
	.space 1
	.ascii "uXor_param_u32_C"
	.space 1
	.ascii "uXor_var_u32_C"
	.space 1
	.ascii "uXor_param_u32_i32"
	.space 1
	.ascii "uXor_var_u32_i32"
	.space 1
	.ascii "uXor_param_u32_u64"
	.space 1
	.ascii "uXor_var_u32_u64"
	.space 1
	.ascii "uXor_param_u32_i16"
	.space 1
	.ascii "uXor_var_u32_i16"
	.space 1
	.ascii "uXor_param_u32_I"
	.space 1
	.ascii "uXor_var_u32_I"
	.space 1
	.ascii "uXor_param_u32_LC"
	.space 1
	.ascii "uXor_var_u32_LC"
	.space 1
	.ascii "uXor_param_u32_u32"
	.space 1
	.ascii "uXor_var_u32_u32"
	.space 1
	.ascii "uXor_param_u32_u16"
	.space 1
	.ascii "uXor_var_u32_u16"
	.space 1
	.ascii "uXor_param_u16_i8"
	.space 1
	.ascii "uXor_var_u16_i8"
	.space 1
	.ascii "uXor_param_u16_L"
	.space 1
	.ascii "uXor_var_u16_L"
	.space 1
	.ascii "uXor_param_u16_i64"
	.space 1
	.ascii "uXor_var_u16_i64"
	.space 1
	.ascii "uXor_param_u16_u8"
	.space 1
	.ascii "uXor_var_u16_u8"
	.space 1
	.ascii "uXor_param_u16_C"
	.space 1
	.ascii "uXor_var_u16_C"
	.space 1
	.ascii "uXor_param_u16_i32"
	.space 1
	.ascii "uXor_var_u16_i32"
	.space 1
	.ascii "uXor_param_u16_u64"
	.space 1
	.ascii "uXor_var_u16_u64"
	.space 1
	.ascii "uXor_param_u16_i16"
	.space 1
	.ascii "uXor_var_u16_i16"
	.space 1
	.ascii "uXor_param_u16_I"
	.space 1
	.ascii "uXor_var_u16_I"
	.space 1
	.ascii "uXor_param_u16_LC"
	.space 1
	.ascii "uXor_var_u16_LC"
	.space 1
	.ascii "uXor_param_u16_u32"
	.space 1
	.ascii "uXor_var_u16_u32"
	.space 1
	.ascii "uXor_param_u16_u16"
	.space 1
	.ascii "uXor_var_u16_u16"
	.space 2
	.quad	_Xor_M3
	.quad	_L_1
	.quad	_Xor__uXor_param_i8_i8
	.quad	_L_1+7
	.quad	_Xor__uXor_var_i8_i8
	.quad	_L_1+24
	.quad	_Xor__uXor_param_i8_L
	.quad	_L_1+39
	.quad	_Xor__uXor_var_i8_L
	.quad	_L_1+55
	.quad	_Xor__uXor_param_i8_i64
	.quad	_L_1+69
	.quad	_Xor__uXor_var_i8_i64
	.quad	_L_1+87
	.quad	_Xor__uXor_param_i8_u8
	.quad	_L_1+103
	.quad	_Xor__uXor_var_i8_u8
	.quad	_L_1+120
	.quad	_Xor__uXor_param_i8_C
	.quad	_L_1+135
	.quad	_Xor__uXor_var_i8_C
	.quad	_L_1+151
	.quad	_Xor__uXor_param_i8_i32
	.quad	_L_1+165
	.quad	_Xor__uXor_var_i8_i32
	.quad	_L_1+183
	.quad	_Xor__uXor_param_i8_u64
	.quad	_L_1+199
	.quad	_Xor__uXor_var_i8_u64
	.quad	_L_1+217
	.quad	_Xor__uXor_param_i8_i16
	.quad	_L_1+233
	.quad	_Xor__uXor_var_i8_i16
	.quad	_L_1+251
	.quad	_Xor__uXor_param_i8_I
	.quad	_L_1+267
	.quad	_Xor__uXor_var_i8_I
	.quad	_L_1+283
	.quad	_Xor__uXor_param_i8_LC
	.quad	_L_1+297
	.quad	_Xor__uXor_var_i8_LC
	.quad	_L_1+314
	.quad	_Xor__uXor_param_i8_u32
	.quad	_L_1+329
	.quad	_Xor__uXor_var_i8_u32
	.quad	_L_1+347
	.quad	_Xor__uXor_param_i8_u16
	.quad	_L_1+363
	.quad	_Xor__uXor_var_i8_u16
	.quad	_L_1+381
	.quad	_Xor__uXor_param_L_i8
	.quad	_L_1+397
	.quad	_Xor__uXor_var_L_i8
	.quad	_L_1+413
	.quad	_Xor__uXor_param_L_L
	.quad	_L_1+427
	.quad	_Xor__uXor_var_L_L
	.quad	_L_1+442
	.quad	_Xor__uXor_param_L_i64
	.quad	_L_1+455
	.quad	_Xor__uXor_var_L_i64
	.quad	_L_1+472
	.quad	_Xor__uXor_param_L_u8
	.quad	_L_1+487
	.quad	_Xor__uXor_var_L_u8
	.quad	_L_1+503
	.quad	_Xor__uXor_param_L_C
	.quad	_L_1+517
	.quad	_Xor__uXor_var_L_C
	.quad	_L_1+532
	.quad	_Xor__uXor_param_L_i32
	.quad	_L_1+545
	.quad	_Xor__uXor_var_L_i32
	.quad	_L_1+562
	.quad	_Xor__uXor_param_L_u64
	.quad	_L_1+577
	.quad	_Xor__uXor_var_L_u64
	.quad	_L_1+594
	.quad	_Xor__uXor_param_L_i16
	.quad	_L_1+609
	.quad	_Xor__uXor_var_L_i16
	.quad	_L_1+626
	.quad	_Xor__uXor_param_L_I
	.quad	_L_1+641
	.quad	_Xor__uXor_var_L_I
	.quad	_L_1+656
	.quad	_Xor__uXor_param_L_LC
	.quad	_L_1+669
	.quad	_Xor__uXor_var_L_LC
	.quad	_L_1+685
	.quad	_Xor__uXor_param_L_u32
	.quad	_L_1+699
	.quad	_Xor__uXor_var_L_u32
	.quad	_L_1+716
	.quad	_Xor__uXor_param_L_u16
	.quad	_L_1+731
	.quad	_Xor__uXor_var_L_u16
	.quad	_L_1+748
	.quad	_Xor__uXor_param_i64_i8
	.quad	_L_1+763
	.quad	_Xor__uXor_var_i64_i8
	.quad	_L_1+781
	.quad	_Xor__uXor_param_i64_L
	.quad	_L_1+797
	.quad	_Xor__uXor_var_i64_L
	.quad	_L_1+814
	.quad	_Xor__uXor_param_i64_i64
	.quad	_L_1+829
	.quad	_Xor__uXor_var_i64_i64
	.quad	_L_1+848
	.quad	_Xor__uXor_param_i64_u8
	.quad	_L_1+865
	.quad	_Xor__uXor_var_i64_u8
	.quad	_L_1+883
	.quad	_Xor__uXor_param_i64_C
	.quad	_L_1+899
	.quad	_Xor__uXor_var_i64_C
	.quad	_L_1+916
	.quad	_Xor__uXor_param_i64_i32
	.quad	_L_1+931
	.quad	_Xor__uXor_var_i64_i32
	.quad	_L_1+950
	.quad	_Xor__uXor_param_i64_u64
	.quad	_L_1+967
	.quad	_Xor__uXor_var_i64_u64
	.quad	_L_1+986
	.quad	_Xor__uXor_param_i64_i16
	.quad	_L_1+1003
	.quad	_Xor__uXor_var_i64_i16
	.quad	_L_1+1022
	.quad	_Xor__uXor_param_i64_I
	.quad	_L_1+1039
	.quad	_Xor__uXor_var_i64_I
	.quad	_L_1+1056
	.quad	_Xor__uXor_param_i64_LC
	.quad	_L_1+1071
	.quad	_Xor__uXor_var_i64_LC
	.quad	_L_1+1089
	.quad	_Xor__uXor_param_i64_u32
	.quad	_L_1+1105
	.quad	_Xor__uXor_var_i64_u32
	.quad	_L_1+1124
	.quad	_Xor__uXor_param_i64_u16
	.quad	_L_1+1141
	.quad	_Xor__uXor_var_i64_u16
	.quad	_L_1+1160
	.quad	_Xor__uXor_param_u8_i8
	.quad	_L_1+1177
	.quad	_Xor__uXor_var_u8_i8
	.quad	_L_1+1194
	.quad	_Xor__uXor_param_u8_L
	.quad	_L_1+1209
	.quad	_Xor__uXor_var_u8_L
	.quad	_L_1+1225
	.quad	_Xor__uXor_param_u8_i64
	.quad	_L_1+1239
	.quad	_Xor__uXor_var_u8_i64
	.quad	_L_1+1257
	.quad	_Xor__uXor_param_u8_u8
	.quad	_L_1+1273
	.quad	_Xor__uXor_var_u8_u8
	.quad	_L_1+1290
	.quad	_Xor__uXor_param_u8_C
	.quad	_L_1+1305
	.quad	_Xor__uXor_var_u8_C
	.quad	_L_1+1321
	.quad	_Xor__uXor_param_u8_i32
	.quad	_L_1+1335
	.quad	_Xor__uXor_var_u8_i32
	.quad	_L_1+1353
	.quad	_Xor__uXor_param_u8_u64
	.quad	_L_1+1369
	.quad	_Xor__uXor_var_u8_u64
	.quad	_L_1+1387
	.quad	_Xor__uXor_param_u8_i16
	.quad	_L_1+1403
	.quad	_Xor__uXor_var_u8_i16
	.quad	_L_1+1421
	.quad	_Xor__uXor_param_u8_I
	.quad	_L_1+1437
	.quad	_Xor__uXor_var_u8_I
	.quad	_L_1+1453
	.quad	_Xor__uXor_param_u8_LC
	.quad	_L_1+1467
	.quad	_Xor__uXor_var_u8_LC
	.quad	_L_1+1484
	.quad	_Xor__uXor_param_u8_u32
	.quad	_L_1+1499
	.quad	_Xor__uXor_var_u8_u32
	.quad	_L_1+1517
	.quad	_Xor__uXor_param_u8_u16
	.quad	_L_1+1533
	.quad	_Xor__uXor_var_u8_u16
	.quad	_L_1+1551
	.quad	_Xor__uXor_param_C_i8
	.quad	_L_1+1567
	.quad	_Xor__uXor_var_C_i8
	.quad	_L_1+1583
	.quad	_Xor__uXor_param_C_L
	.quad	_L_1+1597
	.quad	_Xor__uXor_var_C_L
	.quad	_L_1+1612
	.quad	_Xor__uXor_param_C_i64
	.quad	_L_1+1625
	.quad	_Xor__uXor_var_C_i64
	.quad	_L_1+1642
	.quad	_Xor__uXor_param_C_u8
	.quad	_L_1+1657
	.quad	_Xor__uXor_var_C_u8
	.quad	_L_1+1673
	.quad	_Xor__uXor_param_C_C
	.quad	_L_1+1687
	.quad	_Xor__uXor_var_C_C
	.quad	_L_1+1702
	.quad	_Xor__uXor_param_C_i32
	.quad	_L_1+1715
	.quad	_Xor__uXor_var_C_i32
	.quad	_L_1+1732
	.quad	_Xor__uXor_param_C_u64
	.quad	_L_1+1747
	.quad	_Xor__uXor_var_C_u64
	.quad	_L_1+1764
	.quad	_Xor__uXor_param_C_i16
	.quad	_L_1+1779
	.quad	_Xor__uXor_var_C_i16
	.quad	_L_1+1796
	.quad	_Xor__uXor_param_C_I
	.quad	_L_1+1811
	.quad	_Xor__uXor_var_C_I
	.quad	_L_1+1826
	.quad	_Xor__uXor_param_C_LC
	.quad	_L_1+1839
	.quad	_Xor__uXor_var_C_LC
	.quad	_L_1+1855
	.quad	_Xor__uXor_param_C_u32
	.quad	_L_1+1869
	.quad	_Xor__uXor_var_C_u32
	.quad	_L_1+1886
	.quad	_Xor__uXor_param_C_u16
	.quad	_L_1+1901
	.quad	_Xor__uXor_var_C_u16
	.quad	_L_1+1918
	.quad	_Xor__uXor_param_i32_i8
	.quad	_L_1+1933
	.quad	_Xor__uXor_var_i32_i8
	.quad	_L_1+1951
	.quad	_Xor__uXor_param_i32_L
	.quad	_L_1+1967
	.quad	_Xor__uXor_var_i32_L
	.quad	_L_1+1984
	.quad	_Xor__uXor_param_i32_i64
	.quad	_L_1+1999
	.quad	_Xor__uXor_var_i32_i64
	.quad	_L_1+2018
	.quad	_Xor__uXor_param_i32_u8
	.quad	_L_1+2035
	.quad	_Xor__uXor_var_i32_u8
	.quad	_L_1+2053
	.quad	_Xor__uXor_param_i32_C
	.quad	_L_1+2069
	.quad	_Xor__uXor_var_i32_C
	.quad	_L_1+2086
	.quad	_Xor__uXor_param_i32_i32
	.quad	_L_1+2101
	.quad	_Xor__uXor_var_i32_i32
	.quad	_L_1+2120
	.quad	_Xor__uXor_param_i32_u64
	.quad	_L_1+2137
	.quad	_Xor__uXor_var_i32_u64
	.quad	_L_1+2156
	.quad	_Xor__uXor_param_i32_i16
	.quad	_L_1+2173
	.quad	_Xor__uXor_var_i32_i16
	.quad	_L_1+2192
	.quad	_Xor__uXor_param_i32_I
	.quad	_L_1+2209
	.quad	_Xor__uXor_var_i32_I
	.quad	_L_1+2226
	.quad	_Xor__uXor_param_i32_LC
	.quad	_L_1+2241
	.quad	_Xor__uXor_var_i32_LC
	.quad	_L_1+2259
	.quad	_Xor__uXor_param_i32_u32
	.quad	_L_1+2275
	.quad	_Xor__uXor_var_i32_u32
	.quad	_L_1+2294
	.quad	_Xor__uXor_param_i32_u16
	.quad	_L_1+2311
	.quad	_Xor__uXor_var_i32_u16
	.quad	_L_1+2330
	.quad	_Xor__uXor_param_u64_i8
	.quad	_L_1+2347
	.quad	_Xor__uXor_var_u64_i8
	.quad	_L_1+2365
	.quad	_Xor__uXor_param_u64_L
	.quad	_L_1+2381
	.quad	_Xor__uXor_var_u64_L
	.quad	_L_1+2398
	.quad	_Xor__uXor_param_u64_i64
	.quad	_L_1+2413
	.quad	_Xor__uXor_var_u64_i64
	.quad	_L_1+2432
	.quad	_Xor__uXor_param_u64_u8
	.quad	_L_1+2449
	.quad	_Xor__uXor_var_u64_u8
	.quad	_L_1+2467
	.quad	_Xor__uXor_param_u64_C
	.quad	_L_1+2483
	.quad	_Xor__uXor_var_u64_C
	.quad	_L_1+2500
	.quad	_Xor__uXor_param_u64_i32
	.quad	_L_1+2515
	.quad	_Xor__uXor_var_u64_i32
	.quad	_L_1+2534
	.quad	_Xor__uXor_param_u64_u64
	.quad	_L_1+2551
	.quad	_Xor__uXor_var_u64_u64
	.quad	_L_1+2570
	.quad	_Xor__uXor_param_u64_i16
	.quad	_L_1+2587
	.quad	_Xor__uXor_var_u64_i16
	.quad	_L_1+2606
	.quad	_Xor__uXor_param_u64_I
	.quad	_L_1+2623
	.quad	_Xor__uXor_var_u64_I
	.quad	_L_1+2640
	.quad	_Xor__uXor_param_u64_LC
	.quad	_L_1+2655
	.quad	_Xor__uXor_var_u64_LC
	.quad	_L_1+2673
	.quad	_Xor__uXor_param_u64_u32
	.quad	_L_1+2689
	.quad	_Xor__uXor_var_u64_u32
	.quad	_L_1+2708
	.quad	_Xor__uXor_param_u64_u16
	.quad	_L_1+2725
	.quad	_Xor__uXor_var_u64_u16
	.quad	_L_1+2744
	.quad	_Xor__uXor_param_i16_i8
	.quad	_L_1+2761
	.quad	_Xor__uXor_var_i16_i8
	.quad	_L_1+2779
	.quad	_Xor__uXor_param_i16_L
	.quad	_L_1+2795
	.quad	_Xor__uXor_var_i16_L
	.quad	_L_1+2812
	.quad	_Xor__uXor_param_i16_i64
	.quad	_L_1+2827
	.quad	_Xor__uXor_var_i16_i64
	.quad	_L_1+2846
	.quad	_Xor__uXor_param_i16_u8
	.quad	_L_1+2863
	.quad	_Xor__uXor_var_i16_u8
	.quad	_L_1+2881
	.quad	_Xor__uXor_param_i16_C
	.quad	_L_1+2897
	.quad	_Xor__uXor_var_i16_C
	.quad	_L_1+2914
	.quad	_Xor__uXor_param_i16_i32
	.quad	_L_1+2929
	.quad	_Xor__uXor_var_i16_i32
	.quad	_L_1+2948
	.quad	_Xor__uXor_param_i16_u64
	.quad	_L_1+2965
	.quad	_Xor__uXor_var_i16_u64
	.quad	_L_1+2984
	.quad	_Xor__uXor_param_i16_i16
	.quad	_L_1+3001
	.quad	_Xor__uXor_var_i16_i16
	.quad	_L_1+3020
	.quad	_Xor__uXor_param_i16_I
	.quad	_L_1+3037
	.quad	_Xor__uXor_var_i16_I
	.quad	_L_1+3054
	.quad	_Xor__uXor_param_i16_LC
	.quad	_L_1+3069
	.quad	_Xor__uXor_var_i16_LC
	.quad	_L_1+3087
	.quad	_Xor__uXor_param_i16_u32
	.quad	_L_1+3103
	.quad	_Xor__uXor_var_i16_u32
	.quad	_L_1+3122
	.quad	_Xor__uXor_param_i16_u16
	.quad	_L_1+3139
	.quad	_Xor__uXor_var_i16_u16
	.quad	_L_1+3158
	.quad	_Xor__uXor_param_I_i8
	.quad	_L_1+3175
	.quad	_Xor__uXor_var_I_i8
	.quad	_L_1+3191
	.quad	_Xor__uXor_param_I_L
	.quad	_L_1+3205
	.quad	_Xor__uXor_var_I_L
	.quad	_L_1+3220
	.quad	_Xor__uXor_param_I_i64
	.quad	_L_1+3233
	.quad	_Xor__uXor_var_I_i64
	.quad	_L_1+3250
	.quad	_Xor__uXor_param_I_u8
	.quad	_L_1+3265
	.quad	_Xor__uXor_var_I_u8
	.quad	_L_1+3281
	.quad	_Xor__uXor_param_I_C
	.quad	_L_1+3295
	.quad	_Xor__uXor_var_I_C
	.quad	_L_1+3310
	.quad	_Xor__uXor_param_I_i32
	.quad	_L_1+3323
	.quad	_Xor__uXor_var_I_i32
	.quad	_L_1+3340
	.quad	_Xor__uXor_param_I_u64
	.quad	_L_1+3355
	.quad	_Xor__uXor_var_I_u64
	.quad	_L_1+3372
	.quad	_Xor__uXor_param_I_i16
	.quad	_L_1+3387
	.quad	_Xor__uXor_var_I_i16
	.quad	_L_1+3404
	.quad	_Xor__uXor_param_I_I
	.quad	_L_1+3419
	.quad	_Xor__uXor_var_I_I
	.quad	_L_1+3434
	.quad	_Xor__uXor_param_I_LC
	.quad	_L_1+3447
	.quad	_Xor__uXor_var_I_LC
	.quad	_L_1+3463
	.quad	_Xor__uXor_param_I_u32
	.quad	_L_1+3477
	.quad	_Xor__uXor_var_I_u32
	.quad	_L_1+3494
	.quad	_Xor__uXor_param_I_u16
	.quad	_L_1+3509
	.quad	_Xor__uXor_var_I_u16
	.quad	_L_1+3526
	.quad	_Xor__uXor_param_LC_i8
	.quad	_L_1+3541
	.quad	_Xor__uXor_var_LC_i8
	.quad	_L_1+3558
	.quad	_Xor__uXor_param_LC_L
	.quad	_L_1+3573
	.quad	_Xor__uXor_var_LC_L
	.quad	_L_1+3589
	.quad	_Xor__uXor_param_LC_i64
	.quad	_L_1+3603
	.quad	_Xor__uXor_var_LC_i64
	.quad	_L_1+3621
	.quad	_Xor__uXor_param_LC_u8
	.quad	_L_1+3637
	.quad	_Xor__uXor_var_LC_u8
	.quad	_L_1+3654
	.quad	_Xor__uXor_param_LC_C
	.quad	_L_1+3669
	.quad	_Xor__uXor_var_LC_C
	.quad	_L_1+3685
	.quad	_Xor__uXor_param_LC_i32
	.quad	_L_1+3699
	.quad	_Xor__uXor_var_LC_i32
	.quad	_L_1+3717
	.quad	_Xor__uXor_param_LC_u64
	.quad	_L_1+3733
	.quad	_Xor__uXor_var_LC_u64
	.quad	_L_1+3751
	.quad	_Xor__uXor_param_LC_i16
	.quad	_L_1+3767
	.quad	_Xor__uXor_var_LC_i16
	.quad	_L_1+3785
	.quad	_Xor__uXor_param_LC_I
	.quad	_L_1+3801
	.quad	_Xor__uXor_var_LC_I
	.quad	_L_1+3817
	.quad	_Xor__uXor_param_LC_LC
	.quad	_L_1+3831
	.quad	_Xor__uXor_var_LC_LC
	.quad	_L_1+3848
	.quad	_Xor__uXor_param_LC_u32
	.quad	_L_1+3863
	.quad	_Xor__uXor_var_LC_u32
	.quad	_L_1+3881
	.quad	_Xor__uXor_param_LC_u16
	.quad	_L_1+3897
	.quad	_Xor__uXor_var_LC_u16
	.quad	_L_1+3915
	.quad	_Xor__uXor_param_u32_i8
	.quad	_L_1+3931
	.quad	_Xor__uXor_var_u32_i8
	.quad	_L_1+3949
	.quad	_Xor__uXor_param_u32_L
	.quad	_L_1+3965
	.quad	_Xor__uXor_var_u32_L
	.quad	_L_1+3982
	.quad	_Xor__uXor_param_u32_i64
	.quad	_L_1+3997
	.quad	_Xor__uXor_var_u32_i64
	.quad	_L_1+4016
	.quad	_Xor__uXor_param_u32_u8
	.quad	_L_1+4033
	.quad	_Xor__uXor_var_u32_u8
	.quad	_L_1+4051
	.quad	_Xor__uXor_param_u32_C
	.quad	_L_1+4067
	.quad	_Xor__uXor_var_u32_C
	.quad	_L_1+4084
	.quad	_Xor__uXor_param_u32_i32
	.quad	_L_1+4099
	.quad	_Xor__uXor_var_u32_i32
	.quad	_L_1+4118
	.quad	_Xor__uXor_param_u32_u64
	.quad	_L_1+4135
	.quad	_Xor__uXor_var_u32_u64
	.quad	_L_1+4154
	.quad	_Xor__uXor_param_u32_i16
	.quad	_L_1+4171
	.quad	_Xor__uXor_var_u32_i16
	.quad	_L_1+4190
	.quad	_Xor__uXor_param_u32_I
	.quad	_L_1+4207
	.quad	_Xor__uXor_var_u32_I
	.quad	_L_1+4224
	.quad	_Xor__uXor_param_u32_LC
	.quad	_L_1+4239
	.quad	_Xor__uXor_var_u32_LC
	.quad	_L_1+4257
	.quad	_Xor__uXor_param_u32_u32
	.quad	_L_1+4273
	.quad	_Xor__uXor_var_u32_u32
	.quad	_L_1+4292
	.quad	_Xor__uXor_param_u32_u16
	.quad	_L_1+4309
	.quad	_Xor__uXor_var_u32_u16
	.quad	_L_1+4328
	.quad	_Xor__uXor_param_u16_i8
	.quad	_L_1+4345
	.quad	_Xor__uXor_var_u16_i8
	.quad	_L_1+4363
	.quad	_Xor__uXor_param_u16_L
	.quad	_L_1+4379
	.quad	_Xor__uXor_var_u16_L
	.quad	_L_1+4396
	.quad	_Xor__uXor_param_u16_i64
	.quad	_L_1+4411
	.quad	_Xor__uXor_var_u16_i64
	.quad	_L_1+4430
	.quad	_Xor__uXor_param_u16_u8
	.quad	_L_1+4447
	.quad	_Xor__uXor_var_u16_u8
	.quad	_L_1+4465
	.quad	_Xor__uXor_param_u16_C
	.quad	_L_1+4481
	.quad	_Xor__uXor_var_u16_C
	.quad	_L_1+4498
	.quad	_Xor__uXor_param_u16_i32
	.quad	_L_1+4513
	.quad	_Xor__uXor_var_u16_i32
	.quad	_L_1+4532
	.quad	_Xor__uXor_param_u16_u64
	.quad	_L_1+4549
	.quad	_Xor__uXor_var_u16_u64
	.quad	_L_1+4568
	.quad	_Xor__uXor_param_u16_i16
	.quad	_L_1+4585
	.quad	_Xor__uXor_var_u16_i16
	.quad	_L_1+4604
	.quad	_Xor__uXor_param_u16_I
	.quad	_L_1+4621
	.quad	_Xor__uXor_var_u16_I
	.quad	_L_1+4638
	.quad	_Xor__uXor_param_u16_LC
	.quad	_L_1+4653
	.quad	_Xor__uXor_var_u16_LC
	.quad	_L_1+4671
	.quad	_Xor__uXor_param_u16_u32
	.quad	_L_1+4687
	.quad	_Xor__uXor_var_u16_u32
	.quad	_L_1+4706
	.quad	_Xor__uXor_param_u16_u16
	.quad	_L_1+4723
	.quad	_Xor__uXor_var_u16_u16
	.quad	_L_1+4742
	.space 8
	.ascii "../AMD64_DARWIN/Xor.m3"
	.space 2
	.data
	.align 5
_MM_Xor:
	.quad	_L_1+9392
	.space 32
	.quad	_L_1+4760
	.space 24
	.quad	_MM_Xor+224
	.space 8
	.quad	_Xor_M3
	.quad	3
	.word	497
	.space 2
	.long	498
	.quad	499
	.quad	500
	.word	501
	.space 6
	.quad	502
	.long	1649267442
	.long	1082095632
	.long	505
	.space 4
	.quad	506
	.byte	-5
	.space 7
	.quad	508
	.long	1140769096
	.space 4
	.quad	511
	.byte	0
	.space 31
	.quad	_Xor_I3
	.quad	_MM_Xor+248
	.space 8
	.quad	_Long_I3
	.quad	_MM_Xor+272
	.space 8
	.quad	_Word_I3
	.quad	_MM_Xor+296
	.space 8
	.quad	_Cstdint_I3
	.quad	_MM_Xor+320
	.space 8
	.quad	_RTHooks_I3
	.space 8
_Xor__uXor_var_u16_u16.eh = 0
.no_dead_strip _Xor__uXor_var_u16_u16.eh
_Xor__uXor_param_u16_u16.eh = 0
.no_dead_strip _Xor__uXor_param_u16_u16.eh
_Xor__uXor_var_u16_u32.eh = 0
.no_dead_strip _Xor__uXor_var_u16_u32.eh
_Xor__uXor_param_u16_u32.eh = 0
.no_dead_strip _Xor__uXor_param_u16_u32.eh
_Xor__uXor_var_u16_LC.eh = 0
.no_dead_strip _Xor__uXor_var_u16_LC.eh
_Xor__uXor_param_u16_LC.eh = 0
.no_dead_strip _Xor__uXor_param_u16_LC.eh
_Xor__uXor_var_u16_I.eh = 0
.no_dead_strip _Xor__uXor_var_u16_I.eh
_Xor__uXor_param_u16_I.eh = 0
.no_dead_strip _Xor__uXor_param_u16_I.eh
_Xor__uXor_var_u16_i16.eh = 0
.no_dead_strip _Xor__uXor_var_u16_i16.eh
_Xor__uXor_param_u16_i16.eh = 0
.no_dead_strip _Xor__uXor_param_u16_i16.eh
_Xor__uXor_var_u16_u64.eh = 0
.no_dead_strip _Xor__uXor_var_u16_u64.eh
_Xor__uXor_param_u16_u64.eh = 0
.no_dead_strip _Xor__uXor_param_u16_u64.eh
_Xor__uXor_var_u16_i32.eh = 0
.no_dead_strip _Xor__uXor_var_u16_i32.eh
_Xor__uXor_param_u16_i32.eh = 0
.no_dead_strip _Xor__uXor_param_u16_i32.eh
_Xor__uXor_var_u16_C.eh = 0
.no_dead_strip _Xor__uXor_var_u16_C.eh
_Xor__uXor_param_u16_C.eh = 0
.no_dead_strip _Xor__uXor_param_u16_C.eh
_Xor__uXor_var_u16_u8.eh = 0
.no_dead_strip _Xor__uXor_var_u16_u8.eh
_Xor__uXor_param_u16_u8.eh = 0
.no_dead_strip _Xor__uXor_param_u16_u8.eh
_Xor__uXor_var_u16_i64.eh = 0
.no_dead_strip _Xor__uXor_var_u16_i64.eh
_Xor__uXor_param_u16_i64.eh = 0
.no_dead_strip _Xor__uXor_param_u16_i64.eh
_Xor__uXor_var_u16_L.eh = 0
.no_dead_strip _Xor__uXor_var_u16_L.eh
_Xor__uXor_param_u16_L.eh = 0
.no_dead_strip _Xor__uXor_param_u16_L.eh
_Xor__uXor_var_u16_i8.eh = 0
.no_dead_strip _Xor__uXor_var_u16_i8.eh
_Xor__uXor_param_u16_i8.eh = 0
.no_dead_strip _Xor__uXor_param_u16_i8.eh
_Xor__uXor_var_u32_u16.eh = 0
.no_dead_strip _Xor__uXor_var_u32_u16.eh
_Xor__uXor_param_u32_u16.eh = 0
.no_dead_strip _Xor__uXor_param_u32_u16.eh
_Xor__uXor_var_u32_u32.eh = 0
.no_dead_strip _Xor__uXor_var_u32_u32.eh
_Xor__uXor_param_u32_u32.eh = 0
.no_dead_strip _Xor__uXor_param_u32_u32.eh
_Xor__uXor_var_u32_LC.eh = 0
.no_dead_strip _Xor__uXor_var_u32_LC.eh
_Xor__uXor_param_u32_LC.eh = 0
.no_dead_strip _Xor__uXor_param_u32_LC.eh
_Xor__uXor_var_u32_I.eh = 0
.no_dead_strip _Xor__uXor_var_u32_I.eh
_Xor__uXor_param_u32_I.eh = 0
.no_dead_strip _Xor__uXor_param_u32_I.eh
_Xor__uXor_var_u32_i16.eh = 0
.no_dead_strip _Xor__uXor_var_u32_i16.eh
_Xor__uXor_param_u32_i16.eh = 0
.no_dead_strip _Xor__uXor_param_u32_i16.eh
_Xor__uXor_var_u32_u64.eh = 0
.no_dead_strip _Xor__uXor_var_u32_u64.eh
_Xor__uXor_param_u32_u64.eh = 0
.no_dead_strip _Xor__uXor_param_u32_u64.eh
_Xor__uXor_var_u32_i32.eh = 0
.no_dead_strip _Xor__uXor_var_u32_i32.eh
_Xor__uXor_param_u32_i32.eh = 0
.no_dead_strip _Xor__uXor_param_u32_i32.eh
_Xor__uXor_var_u32_C.eh = 0
.no_dead_strip _Xor__uXor_var_u32_C.eh
_Xor__uXor_param_u32_C.eh = 0
.no_dead_strip _Xor__uXor_param_u32_C.eh
_Xor__uXor_var_u32_u8.eh = 0
.no_dead_strip _Xor__uXor_var_u32_u8.eh
_Xor__uXor_param_u32_u8.eh = 0
.no_dead_strip _Xor__uXor_param_u32_u8.eh
_Xor__uXor_var_u32_i64.eh = 0
.no_dead_strip _Xor__uXor_var_u32_i64.eh
_Xor__uXor_param_u32_i64.eh = 0
.no_dead_strip _Xor__uXor_param_u32_i64.eh
_Xor__uXor_var_u32_L.eh = 0
.no_dead_strip _Xor__uXor_var_u32_L.eh
_Xor__uXor_param_u32_L.eh = 0
.no_dead_strip _Xor__uXor_param_u32_L.eh
_Xor__uXor_var_u32_i8.eh = 0
.no_dead_strip _Xor__uXor_var_u32_i8.eh
_Xor__uXor_param_u32_i8.eh = 0
.no_dead_strip _Xor__uXor_param_u32_i8.eh
_Xor__uXor_var_LC_u16.eh = 0
.no_dead_strip _Xor__uXor_var_LC_u16.eh
_Xor__uXor_param_LC_u16.eh = 0
.no_dead_strip _Xor__uXor_param_LC_u16.eh
_Xor__uXor_var_LC_u32.eh = 0
.no_dead_strip _Xor__uXor_var_LC_u32.eh
_Xor__uXor_param_LC_u32.eh = 0
.no_dead_strip _Xor__uXor_param_LC_u32.eh
_Xor__uXor_var_LC_LC.eh = 0
.no_dead_strip _Xor__uXor_var_LC_LC.eh
_Xor__uXor_param_LC_LC.eh = 0
.no_dead_strip _Xor__uXor_param_LC_LC.eh
_Xor__uXor_var_LC_I.eh = 0
.no_dead_strip _Xor__uXor_var_LC_I.eh
_Xor__uXor_param_LC_I.eh = 0
.no_dead_strip _Xor__uXor_param_LC_I.eh
_Xor__uXor_var_LC_i16.eh = 0
.no_dead_strip _Xor__uXor_var_LC_i16.eh
_Xor__uXor_param_LC_i16.eh = 0
.no_dead_strip _Xor__uXor_param_LC_i16.eh
_Xor__uXor_var_LC_u64.eh = 0
.no_dead_strip _Xor__uXor_var_LC_u64.eh
_Xor__uXor_param_LC_u64.eh = 0
.no_dead_strip _Xor__uXor_param_LC_u64.eh
_Xor__uXor_var_LC_i32.eh = 0
.no_dead_strip _Xor__uXor_var_LC_i32.eh
_Xor__uXor_param_LC_i32.eh = 0
.no_dead_strip _Xor__uXor_param_LC_i32.eh
_Xor__uXor_var_LC_C.eh = 0
.no_dead_strip _Xor__uXor_var_LC_C.eh
_Xor__uXor_param_LC_C.eh = 0
.no_dead_strip _Xor__uXor_param_LC_C.eh
_Xor__uXor_var_LC_u8.eh = 0
.no_dead_strip _Xor__uXor_var_LC_u8.eh
_Xor__uXor_param_LC_u8.eh = 0
.no_dead_strip _Xor__uXor_param_LC_u8.eh
_Xor__uXor_var_LC_i64.eh = 0
.no_dead_strip _Xor__uXor_var_LC_i64.eh
_Xor__uXor_param_LC_i64.eh = 0
.no_dead_strip _Xor__uXor_param_LC_i64.eh
_Xor__uXor_var_LC_L.eh = 0
.no_dead_strip _Xor__uXor_var_LC_L.eh
_Xor__uXor_param_LC_L.eh = 0
.no_dead_strip _Xor__uXor_param_LC_L.eh
_Xor__uXor_var_LC_i8.eh = 0
.no_dead_strip _Xor__uXor_var_LC_i8.eh
_Xor__uXor_param_LC_i8.eh = 0
.no_dead_strip _Xor__uXor_param_LC_i8.eh
_Xor__uXor_var_I_u16.eh = 0
.no_dead_strip _Xor__uXor_var_I_u16.eh
_Xor__uXor_param_I_u16.eh = 0
.no_dead_strip _Xor__uXor_param_I_u16.eh
_Xor__uXor_var_I_u32.eh = 0
.no_dead_strip _Xor__uXor_var_I_u32.eh
_Xor__uXor_param_I_u32.eh = 0
.no_dead_strip _Xor__uXor_param_I_u32.eh
_Xor__uXor_var_I_LC.eh = 0
.no_dead_strip _Xor__uXor_var_I_LC.eh
_Xor__uXor_param_I_LC.eh = 0
.no_dead_strip _Xor__uXor_param_I_LC.eh
_Xor__uXor_var_I_I.eh = 0
.no_dead_strip _Xor__uXor_var_I_I.eh
_Xor__uXor_param_I_I.eh = 0
.no_dead_strip _Xor__uXor_param_I_I.eh
_Xor__uXor_var_I_i16.eh = 0
.no_dead_strip _Xor__uXor_var_I_i16.eh
_Xor__uXor_param_I_i16.eh = 0
.no_dead_strip _Xor__uXor_param_I_i16.eh
_Xor__uXor_var_I_u64.eh = 0
.no_dead_strip _Xor__uXor_var_I_u64.eh
_Xor__uXor_param_I_u64.eh = 0
.no_dead_strip _Xor__uXor_param_I_u64.eh
_Xor__uXor_var_I_i32.eh = 0
.no_dead_strip _Xor__uXor_var_I_i32.eh
_Xor__uXor_param_I_i32.eh = 0
.no_dead_strip _Xor__uXor_param_I_i32.eh
_Xor__uXor_var_I_C.eh = 0
.no_dead_strip _Xor__uXor_var_I_C.eh
_Xor__uXor_param_I_C.eh = 0
.no_dead_strip _Xor__uXor_param_I_C.eh
_Xor__uXor_var_I_u8.eh = 0
.no_dead_strip _Xor__uXor_var_I_u8.eh
_Xor__uXor_param_I_u8.eh = 0
.no_dead_strip _Xor__uXor_param_I_u8.eh
_Xor__uXor_var_I_i64.eh = 0
.no_dead_strip _Xor__uXor_var_I_i64.eh
_Xor__uXor_param_I_i64.eh = 0
.no_dead_strip _Xor__uXor_param_I_i64.eh
_Xor__uXor_var_I_L.eh = 0
.no_dead_strip _Xor__uXor_var_I_L.eh
_Xor__uXor_param_I_L.eh = 0
.no_dead_strip _Xor__uXor_param_I_L.eh
_Xor__uXor_var_I_i8.eh = 0
.no_dead_strip _Xor__uXor_var_I_i8.eh
_Xor__uXor_param_I_i8.eh = 0
.no_dead_strip _Xor__uXor_param_I_i8.eh
_Xor__uXor_var_i16_u16.eh = 0
.no_dead_strip _Xor__uXor_var_i16_u16.eh
_Xor__uXor_param_i16_u16.eh = 0
.no_dead_strip _Xor__uXor_param_i16_u16.eh
_Xor__uXor_var_i16_u32.eh = 0
.no_dead_strip _Xor__uXor_var_i16_u32.eh
_Xor__uXor_param_i16_u32.eh = 0
.no_dead_strip _Xor__uXor_param_i16_u32.eh
_Xor__uXor_var_i16_LC.eh = 0
.no_dead_strip _Xor__uXor_var_i16_LC.eh
_Xor__uXor_param_i16_LC.eh = 0
.no_dead_strip _Xor__uXor_param_i16_LC.eh
_Xor__uXor_var_i16_I.eh = 0
.no_dead_strip _Xor__uXor_var_i16_I.eh
_Xor__uXor_param_i16_I.eh = 0
.no_dead_strip _Xor__uXor_param_i16_I.eh
_Xor__uXor_var_i16_i16.eh = 0
.no_dead_strip _Xor__uXor_var_i16_i16.eh
_Xor__uXor_param_i16_i16.eh = 0
.no_dead_strip _Xor__uXor_param_i16_i16.eh
_Xor__uXor_var_i16_u64.eh = 0
.no_dead_strip _Xor__uXor_var_i16_u64.eh
_Xor__uXor_param_i16_u64.eh = 0
.no_dead_strip _Xor__uXor_param_i16_u64.eh
_Xor__uXor_var_i16_i32.eh = 0
.no_dead_strip _Xor__uXor_var_i16_i32.eh
_Xor__uXor_param_i16_i32.eh = 0
.no_dead_strip _Xor__uXor_param_i16_i32.eh
_Xor__uXor_var_i16_C.eh = 0
.no_dead_strip _Xor__uXor_var_i16_C.eh
_Xor__uXor_param_i16_C.eh = 0
.no_dead_strip _Xor__uXor_param_i16_C.eh
_Xor__uXor_var_i16_u8.eh = 0
.no_dead_strip _Xor__uXor_var_i16_u8.eh
_Xor__uXor_param_i16_u8.eh = 0
.no_dead_strip _Xor__uXor_param_i16_u8.eh
_Xor__uXor_var_i16_i64.eh = 0
.no_dead_strip _Xor__uXor_var_i16_i64.eh
_Xor__uXor_param_i16_i64.eh = 0
.no_dead_strip _Xor__uXor_param_i16_i64.eh
_Xor__uXor_var_i16_L.eh = 0
.no_dead_strip _Xor__uXor_var_i16_L.eh
_Xor__uXor_param_i16_L.eh = 0
.no_dead_strip _Xor__uXor_param_i16_L.eh
_Xor__uXor_var_i16_i8.eh = 0
.no_dead_strip _Xor__uXor_var_i16_i8.eh
_Xor__uXor_param_i16_i8.eh = 0
.no_dead_strip _Xor__uXor_param_i16_i8.eh
_Xor__uXor_var_u64_u16.eh = 0
.no_dead_strip _Xor__uXor_var_u64_u16.eh
_Xor__uXor_param_u64_u16.eh = 0
.no_dead_strip _Xor__uXor_param_u64_u16.eh
_Xor__uXor_var_u64_u32.eh = 0
.no_dead_strip _Xor__uXor_var_u64_u32.eh
_Xor__uXor_param_u64_u32.eh = 0
.no_dead_strip _Xor__uXor_param_u64_u32.eh
_Xor__uXor_var_u64_LC.eh = 0
.no_dead_strip _Xor__uXor_var_u64_LC.eh
_Xor__uXor_param_u64_LC.eh = 0
.no_dead_strip _Xor__uXor_param_u64_LC.eh
_Xor__uXor_var_u64_I.eh = 0
.no_dead_strip _Xor__uXor_var_u64_I.eh
_Xor__uXor_param_u64_I.eh = 0
.no_dead_strip _Xor__uXor_param_u64_I.eh
_Xor__uXor_var_u64_i16.eh = 0
.no_dead_strip _Xor__uXor_var_u64_i16.eh
_Xor__uXor_param_u64_i16.eh = 0
.no_dead_strip _Xor__uXor_param_u64_i16.eh
_Xor__uXor_var_u64_u64.eh = 0
.no_dead_strip _Xor__uXor_var_u64_u64.eh
_Xor__uXor_param_u64_u64.eh = 0
.no_dead_strip _Xor__uXor_param_u64_u64.eh
_Xor__uXor_var_u64_i32.eh = 0
.no_dead_strip _Xor__uXor_var_u64_i32.eh
_Xor__uXor_param_u64_i32.eh = 0
.no_dead_strip _Xor__uXor_param_u64_i32.eh
_Xor__uXor_var_u64_C.eh = 0
.no_dead_strip _Xor__uXor_var_u64_C.eh
_Xor__uXor_param_u64_C.eh = 0
.no_dead_strip _Xor__uXor_param_u64_C.eh
_Xor__uXor_var_u64_u8.eh = 0
.no_dead_strip _Xor__uXor_var_u64_u8.eh
_Xor__uXor_param_u64_u8.eh = 0
.no_dead_strip _Xor__uXor_param_u64_u8.eh
_Xor__uXor_var_u64_i64.eh = 0
.no_dead_strip _Xor__uXor_var_u64_i64.eh
_Xor__uXor_param_u64_i64.eh = 0
.no_dead_strip _Xor__uXor_param_u64_i64.eh
_Xor__uXor_var_u64_L.eh = 0
.no_dead_strip _Xor__uXor_var_u64_L.eh
_Xor__uXor_param_u64_L.eh = 0
.no_dead_strip _Xor__uXor_param_u64_L.eh
_Xor__uXor_var_u64_i8.eh = 0
.no_dead_strip _Xor__uXor_var_u64_i8.eh
_Xor__uXor_param_u64_i8.eh = 0
.no_dead_strip _Xor__uXor_param_u64_i8.eh
_Xor__uXor_var_i32_u16.eh = 0
.no_dead_strip _Xor__uXor_var_i32_u16.eh
_Xor__uXor_param_i32_u16.eh = 0
.no_dead_strip _Xor__uXor_param_i32_u16.eh
_Xor__uXor_var_i32_u32.eh = 0
.no_dead_strip _Xor__uXor_var_i32_u32.eh
_Xor__uXor_param_i32_u32.eh = 0
.no_dead_strip _Xor__uXor_param_i32_u32.eh
_Xor__uXor_var_i32_LC.eh = 0
.no_dead_strip _Xor__uXor_var_i32_LC.eh
_Xor__uXor_param_i32_LC.eh = 0
.no_dead_strip _Xor__uXor_param_i32_LC.eh
_Xor__uXor_var_i32_I.eh = 0
.no_dead_strip _Xor__uXor_var_i32_I.eh
_Xor__uXor_param_i32_I.eh = 0
.no_dead_strip _Xor__uXor_param_i32_I.eh
_Xor__uXor_var_i32_i16.eh = 0
.no_dead_strip _Xor__uXor_var_i32_i16.eh
_Xor__uXor_param_i32_i16.eh = 0
.no_dead_strip _Xor__uXor_param_i32_i16.eh
_Xor__uXor_var_i32_u64.eh = 0
.no_dead_strip _Xor__uXor_var_i32_u64.eh
_Xor__uXor_param_i32_u64.eh = 0
.no_dead_strip _Xor__uXor_param_i32_u64.eh
_Xor__uXor_var_i32_i32.eh = 0
.no_dead_strip _Xor__uXor_var_i32_i32.eh
_Xor__uXor_param_i32_i32.eh = 0
.no_dead_strip _Xor__uXor_param_i32_i32.eh
_Xor__uXor_var_i32_C.eh = 0
.no_dead_strip _Xor__uXor_var_i32_C.eh
_Xor__uXor_param_i32_C.eh = 0
.no_dead_strip _Xor__uXor_param_i32_C.eh
_Xor__uXor_var_i32_u8.eh = 0
.no_dead_strip _Xor__uXor_var_i32_u8.eh
_Xor__uXor_param_i32_u8.eh = 0
.no_dead_strip _Xor__uXor_param_i32_u8.eh
_Xor__uXor_var_i32_i64.eh = 0
.no_dead_strip _Xor__uXor_var_i32_i64.eh
_Xor__uXor_param_i32_i64.eh = 0
.no_dead_strip _Xor__uXor_param_i32_i64.eh
_Xor__uXor_var_i32_L.eh = 0
.no_dead_strip _Xor__uXor_var_i32_L.eh
_Xor__uXor_param_i32_L.eh = 0
.no_dead_strip _Xor__uXor_param_i32_L.eh
_Xor__uXor_var_i32_i8.eh = 0
.no_dead_strip _Xor__uXor_var_i32_i8.eh
_Xor__uXor_param_i32_i8.eh = 0
.no_dead_strip _Xor__uXor_param_i32_i8.eh
_Xor__uXor_var_C_u16.eh = 0
.no_dead_strip _Xor__uXor_var_C_u16.eh
_Xor__uXor_param_C_u16.eh = 0
.no_dead_strip _Xor__uXor_param_C_u16.eh
_Xor__uXor_var_C_u32.eh = 0
.no_dead_strip _Xor__uXor_var_C_u32.eh
_Xor__uXor_param_C_u32.eh = 0
.no_dead_strip _Xor__uXor_param_C_u32.eh
_Xor__uXor_var_C_LC.eh = 0
.no_dead_strip _Xor__uXor_var_C_LC.eh
_Xor__uXor_param_C_LC.eh = 0
.no_dead_strip _Xor__uXor_param_C_LC.eh
_Xor__uXor_var_C_I.eh = 0
.no_dead_strip _Xor__uXor_var_C_I.eh
_Xor__uXor_param_C_I.eh = 0
.no_dead_strip _Xor__uXor_param_C_I.eh
_Xor__uXor_var_C_i16.eh = 0
.no_dead_strip _Xor__uXor_var_C_i16.eh
_Xor__uXor_param_C_i16.eh = 0
.no_dead_strip _Xor__uXor_param_C_i16.eh
_Xor__uXor_var_C_u64.eh = 0
.no_dead_strip _Xor__uXor_var_C_u64.eh
_Xor__uXor_param_C_u64.eh = 0
.no_dead_strip _Xor__uXor_param_C_u64.eh
_Xor__uXor_var_C_i32.eh = 0
.no_dead_strip _Xor__uXor_var_C_i32.eh
_Xor__uXor_param_C_i32.eh = 0
.no_dead_strip _Xor__uXor_param_C_i32.eh
_Xor__uXor_var_C_C.eh = 0
.no_dead_strip _Xor__uXor_var_C_C.eh
_Xor__uXor_param_C_C.eh = 0
.no_dead_strip _Xor__uXor_param_C_C.eh
_Xor__uXor_var_C_u8.eh = 0
.no_dead_strip _Xor__uXor_var_C_u8.eh
_Xor__uXor_param_C_u8.eh = 0
.no_dead_strip _Xor__uXor_param_C_u8.eh
_Xor__uXor_var_C_i64.eh = 0
.no_dead_strip _Xor__uXor_var_C_i64.eh
_Xor__uXor_param_C_i64.eh = 0
.no_dead_strip _Xor__uXor_param_C_i64.eh
_Xor__uXor_var_C_L.eh = 0
.no_dead_strip _Xor__uXor_var_C_L.eh
_Xor__uXor_param_C_L.eh = 0
.no_dead_strip _Xor__uXor_param_C_L.eh
_Xor__uXor_var_C_i8.eh = 0
.no_dead_strip _Xor__uXor_var_C_i8.eh
_Xor__uXor_param_C_i8.eh = 0
.no_dead_strip _Xor__uXor_param_C_i8.eh
_Xor__uXor_var_u8_u16.eh = 0
.no_dead_strip _Xor__uXor_var_u8_u16.eh
_Xor__uXor_param_u8_u16.eh = 0
.no_dead_strip _Xor__uXor_param_u8_u16.eh
_Xor__uXor_var_u8_u32.eh = 0
.no_dead_strip _Xor__uXor_var_u8_u32.eh
_Xor__uXor_param_u8_u32.eh = 0
.no_dead_strip _Xor__uXor_param_u8_u32.eh
_Xor__uXor_var_u8_LC.eh = 0
.no_dead_strip _Xor__uXor_var_u8_LC.eh
_Xor__uXor_param_u8_LC.eh = 0
.no_dead_strip _Xor__uXor_param_u8_LC.eh
_Xor__uXor_var_u8_I.eh = 0
.no_dead_strip _Xor__uXor_var_u8_I.eh
_Xor__uXor_param_u8_I.eh = 0
.no_dead_strip _Xor__uXor_param_u8_I.eh
_Xor__uXor_var_u8_i16.eh = 0
.no_dead_strip _Xor__uXor_var_u8_i16.eh
_Xor__uXor_param_u8_i16.eh = 0
.no_dead_strip _Xor__uXor_param_u8_i16.eh
_Xor__uXor_var_u8_u64.eh = 0
.no_dead_strip _Xor__uXor_var_u8_u64.eh
_Xor__uXor_param_u8_u64.eh = 0
.no_dead_strip _Xor__uXor_param_u8_u64.eh
_Xor__uXor_var_u8_i32.eh = 0
.no_dead_strip _Xor__uXor_var_u8_i32.eh
_Xor__uXor_param_u8_i32.eh = 0
.no_dead_strip _Xor__uXor_param_u8_i32.eh
_Xor__uXor_var_u8_C.eh = 0
.no_dead_strip _Xor__uXor_var_u8_C.eh
_Xor__uXor_param_u8_C.eh = 0
.no_dead_strip _Xor__uXor_param_u8_C.eh
_Xor__uXor_var_u8_u8.eh = 0
.no_dead_strip _Xor__uXor_var_u8_u8.eh
_Xor__uXor_param_u8_u8.eh = 0
.no_dead_strip _Xor__uXor_param_u8_u8.eh
_Xor__uXor_var_u8_i64.eh = 0
.no_dead_strip _Xor__uXor_var_u8_i64.eh
_Xor__uXor_param_u8_i64.eh = 0
.no_dead_strip _Xor__uXor_param_u8_i64.eh
_Xor__uXor_var_u8_L.eh = 0
.no_dead_strip _Xor__uXor_var_u8_L.eh
_Xor__uXor_param_u8_L.eh = 0
.no_dead_strip _Xor__uXor_param_u8_L.eh
_Xor__uXor_var_u8_i8.eh = 0
.no_dead_strip _Xor__uXor_var_u8_i8.eh
_Xor__uXor_param_u8_i8.eh = 0
.no_dead_strip _Xor__uXor_param_u8_i8.eh
_Xor__uXor_var_i64_u16.eh = 0
.no_dead_strip _Xor__uXor_var_i64_u16.eh
_Xor__uXor_param_i64_u16.eh = 0
.no_dead_strip _Xor__uXor_param_i64_u16.eh
_Xor__uXor_var_i64_u32.eh = 0
.no_dead_strip _Xor__uXor_var_i64_u32.eh
_Xor__uXor_param_i64_u32.eh = 0
.no_dead_strip _Xor__uXor_param_i64_u32.eh
_Xor__uXor_var_i64_LC.eh = 0
.no_dead_strip _Xor__uXor_var_i64_LC.eh
_Xor__uXor_param_i64_LC.eh = 0
.no_dead_strip _Xor__uXor_param_i64_LC.eh
_Xor__uXor_var_i64_I.eh = 0
.no_dead_strip _Xor__uXor_var_i64_I.eh
_Xor__uXor_param_i64_I.eh = 0
.no_dead_strip _Xor__uXor_param_i64_I.eh
_Xor__uXor_var_i64_i16.eh = 0
.no_dead_strip _Xor__uXor_var_i64_i16.eh
_Xor__uXor_param_i64_i16.eh = 0
.no_dead_strip _Xor__uXor_param_i64_i16.eh
_Xor__uXor_var_i64_u64.eh = 0
.no_dead_strip _Xor__uXor_var_i64_u64.eh
_Xor__uXor_param_i64_u64.eh = 0
.no_dead_strip _Xor__uXor_param_i64_u64.eh
_Xor__uXor_var_i64_i32.eh = 0
.no_dead_strip _Xor__uXor_var_i64_i32.eh
_Xor__uXor_param_i64_i32.eh = 0
.no_dead_strip _Xor__uXor_param_i64_i32.eh
_Xor__uXor_var_i64_C.eh = 0
.no_dead_strip _Xor__uXor_var_i64_C.eh
_Xor__uXor_param_i64_C.eh = 0
.no_dead_strip _Xor__uXor_param_i64_C.eh
_Xor__uXor_var_i64_u8.eh = 0
.no_dead_strip _Xor__uXor_var_i64_u8.eh
_Xor__uXor_param_i64_u8.eh = 0
.no_dead_strip _Xor__uXor_param_i64_u8.eh
_Xor__uXor_var_i64_i64.eh = 0
.no_dead_strip _Xor__uXor_var_i64_i64.eh
_Xor__uXor_param_i64_i64.eh = 0
.no_dead_strip _Xor__uXor_param_i64_i64.eh
_Xor__uXor_var_i64_L.eh = 0
.no_dead_strip _Xor__uXor_var_i64_L.eh
_Xor__uXor_param_i64_L.eh = 0
.no_dead_strip _Xor__uXor_param_i64_L.eh
_Xor__uXor_var_i64_i8.eh = 0
.no_dead_strip _Xor__uXor_var_i64_i8.eh
_Xor__uXor_param_i64_i8.eh = 0
.no_dead_strip _Xor__uXor_param_i64_i8.eh
_Xor__uXor_var_L_u16.eh = 0
.no_dead_strip _Xor__uXor_var_L_u16.eh
_Xor__uXor_param_L_u16.eh = 0
.no_dead_strip _Xor__uXor_param_L_u16.eh
_Xor__uXor_var_L_u32.eh = 0
.no_dead_strip _Xor__uXor_var_L_u32.eh
_Xor__uXor_param_L_u32.eh = 0
.no_dead_strip _Xor__uXor_param_L_u32.eh
_Xor__uXor_var_L_LC.eh = 0
.no_dead_strip _Xor__uXor_var_L_LC.eh
_Xor__uXor_param_L_LC.eh = 0
.no_dead_strip _Xor__uXor_param_L_LC.eh
_Xor__uXor_var_L_I.eh = 0
.no_dead_strip _Xor__uXor_var_L_I.eh
_Xor__uXor_param_L_I.eh = 0
.no_dead_strip _Xor__uXor_param_L_I.eh
_Xor__uXor_var_L_i16.eh = 0
.no_dead_strip _Xor__uXor_var_L_i16.eh
_Xor__uXor_param_L_i16.eh = 0
.no_dead_strip _Xor__uXor_param_L_i16.eh
_Xor__uXor_var_L_u64.eh = 0
.no_dead_strip _Xor__uXor_var_L_u64.eh
_Xor__uXor_param_L_u64.eh = 0
.no_dead_strip _Xor__uXor_param_L_u64.eh
_Xor__uXor_var_L_i32.eh = 0
.no_dead_strip _Xor__uXor_var_L_i32.eh
_Xor__uXor_param_L_i32.eh = 0
.no_dead_strip _Xor__uXor_param_L_i32.eh
_Xor__uXor_var_L_C.eh = 0
.no_dead_strip _Xor__uXor_var_L_C.eh
_Xor__uXor_param_L_C.eh = 0
.no_dead_strip _Xor__uXor_param_L_C.eh
_Xor__uXor_var_L_u8.eh = 0
.no_dead_strip _Xor__uXor_var_L_u8.eh
_Xor__uXor_param_L_u8.eh = 0
.no_dead_strip _Xor__uXor_param_L_u8.eh
_Xor__uXor_var_L_i64.eh = 0
.no_dead_strip _Xor__uXor_var_L_i64.eh
_Xor__uXor_param_L_i64.eh = 0
.no_dead_strip _Xor__uXor_param_L_i64.eh
_Xor__uXor_var_L_L.eh = 0
.no_dead_strip _Xor__uXor_var_L_L.eh
_Xor__uXor_param_L_L.eh = 0
.no_dead_strip _Xor__uXor_param_L_L.eh
_Xor__uXor_var_L_i8.eh = 0
.no_dead_strip _Xor__uXor_var_L_i8.eh
_Xor__uXor_param_L_i8.eh = 0
.no_dead_strip _Xor__uXor_param_L_i8.eh
_Xor__uXor_var_i8_u16.eh = 0
.no_dead_strip _Xor__uXor_var_i8_u16.eh
_Xor__uXor_param_i8_u16.eh = 0
.no_dead_strip _Xor__uXor_param_i8_u16.eh
_Xor__uXor_var_i8_u32.eh = 0
.no_dead_strip _Xor__uXor_var_i8_u32.eh
_Xor__uXor_param_i8_u32.eh = 0
.no_dead_strip _Xor__uXor_param_i8_u32.eh
_Xor__uXor_var_i8_LC.eh = 0
.no_dead_strip _Xor__uXor_var_i8_LC.eh
_Xor__uXor_param_i8_LC.eh = 0
.no_dead_strip _Xor__uXor_param_i8_LC.eh
_Xor__uXor_var_i8_I.eh = 0
.no_dead_strip _Xor__uXor_var_i8_I.eh
_Xor__uXor_param_i8_I.eh = 0
.no_dead_strip _Xor__uXor_param_i8_I.eh
_Xor__uXor_var_i8_i16.eh = 0
.no_dead_strip _Xor__uXor_var_i8_i16.eh
_Xor__uXor_param_i8_i16.eh = 0
.no_dead_strip _Xor__uXor_param_i8_i16.eh
_Xor__uXor_var_i8_u64.eh = 0
.no_dead_strip _Xor__uXor_var_i8_u64.eh
_Xor__uXor_param_i8_u64.eh = 0
.no_dead_strip _Xor__uXor_param_i8_u64.eh
_Xor__uXor_var_i8_i32.eh = 0
.no_dead_strip _Xor__uXor_var_i8_i32.eh
_Xor__uXor_param_i8_i32.eh = 0
.no_dead_strip _Xor__uXor_param_i8_i32.eh
_Xor__uXor_var_i8_C.eh = 0
.no_dead_strip _Xor__uXor_var_i8_C.eh
_Xor__uXor_param_i8_C.eh = 0
.no_dead_strip _Xor__uXor_param_i8_C.eh
_Xor__uXor_var_i8_u8.eh = 0
.no_dead_strip _Xor__uXor_var_i8_u8.eh
_Xor__uXor_param_i8_u8.eh = 0
.no_dead_strip _Xor__uXor_param_i8_u8.eh
_Xor__uXor_var_i8_i64.eh = 0
.no_dead_strip _Xor__uXor_var_i8_i64.eh
_Xor__uXor_param_i8_i64.eh = 0
.no_dead_strip _Xor__uXor_param_i8_i64.eh
_Xor__uXor_var_i8_L.eh = 0
.no_dead_strip _Xor__uXor_var_i8_L.eh
_Xor__uXor_param_i8_L.eh = 0
.no_dead_strip _Xor__uXor_param_i8_L.eh
_Xor__uXor_var_i8_i8.eh = 0
.no_dead_strip _Xor__uXor_var_i8_i8.eh
_Xor__uXor_param_i8_i8.eh = 0
.no_dead_strip _Xor__uXor_param_i8_i8.eh
.globl _Xor_M3.eh
_Xor_M3.eh = 0
.no_dead_strip _Xor_M3.eh
	.subsections_via_symbols
