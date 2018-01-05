	.globl main
	.section .text
int_to_str:
	# (rax:*u8,rbx:i64) -> eax:*u8
mod:
	# (rax:i64,rbx:i64) -> eax:i64
	# x-y*x/y
	pushq %rbp
	movq  %rsp, %rbp
	pushq %rax       # # ← rax
	movq  %rax, %rdx # rdx ← rax
	cqto             # rax ← sign(rdx)
	idivq %rbx       # rax ← rax,rdx / rbx
	movq  %rax, %rdx # rdx ← rax
	imulq %rbx, %rdx # rdx ← rbx * rdx
	popq %rax        # rax ← #
	subq %rdx, %rax  # rax ← rax - rdx

	popq %rbp
	ret
main:
	pushq %rbp
	movq %rsp, %rbp

	movq $10, %rax # rax ← 10
	movq $3,  %rbx # rbx ← 3
	call mod

	popq %rbp
	ret
