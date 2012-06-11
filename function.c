#include "lib.h"
#include "sparse/linearize.h"
#include <assert.h>
#include <string.h>

#define blockof(bb)	((bb)->priv)

#define swap(a, b) ({		\
	typeof(a) tmp = (a);	\
	(a) = (b);		\
	(b) = tmp; })

static value_t function;

static value_t emit_tentative_pseudo(struct pseudo *pseudo)
{
	if (!pseudo->priv) {
		type_t type = emit_type(pseudo->ctype);
		block_t blk = blockof(pseudo->def->bb);

		pseudo->priv = alloc_placeholder(type, blk);
	}
	return pseudo->priv;
}

static void complete_pseudo(struct pseudo *pseudo, value_t v)
{
	if (pseudo->priv)
		free_placeholder(pseudo->priv, v);
	pseudo->priv = v;
}

static value_t emit_symbol(struct symbol *sym)
{
	if (sym->ctype.modifiers & (MOD_TOPLEVEL | MOD_STATIC))
		return emit_toplevel(LLVMGetGlobalParent(function), sym);
	if (sym->aux)
		return sym->aux;
	sym->aux = alloc_alloca(emit_type(sym), function);
	return sym->aux;
}

static value_t emit_pseudo(pseudo_t pseudo)
{
	switch (pseudo->type) {
	default: assert(0 && "Unknown pseduo!");
	case PSEUDO_REG:
		// Refers to another instruction.
		// If it hasn't been emitted, use a placeholder.
		// ->def gives the def instruction.
		assert(pseudo->def->target->type == PSEUDO_REG);
		return emit_tentative_pseudo(pseudo->def->target);
	case PSEUDO_SYM:
		return emit_symbol(pseudo->sym);
	case PSEUDO_VAL:
		// Integer or pointer.
		return get_integer_value(emit_type(pseudo->ctype), pseudo->value);
	case PSEUDO_ARG:
		// ->def gives the OP_ENTRY instruction.
		// ->bb->ep gives the entry point.
		// ->name is the function.
		// ->aux points to llvm::Function.
		return LLVMGetParam(pseudo->def->bb->ep->name->aux, pseudo->nr - 1);
	case PSEUDO_PHI:
		assert(0 && "Unreachable (by emit_phi) !");
	}
}

static int is_main(struct symbol *sym)
{
	return !strcmp("main", show_ident(sym->ident)) && !(sym->ctype.modifiers & MOD_STATIC);
}

static void emit_ret(builder_t builder, struct instruction *insn)
{
	// insn->target may be null, so we use function type instead.
	struct symbol *func = insn->bb->ep->name;
	type_t type = LLVMGetReturnType(get_function_type(function));
	value_t v;

	if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
		LLVMBuildRetVoid(builder);
		return;
	}

	if (insn->src && insn->src != VOID)
		v = emit_pseudo(insn->src);
	else if (is_main(func))
		v = LLVMConstNull(type); // Return 0 for main.
	else
		v = LLVMGetUndef(type);
	assert(LLVMTypeOf(v) == type);
	LLVMBuildRet(builder, v);
}

static void emit_br(builder_t builder, struct instruction *insn)
{
	struct basic_block *bb_true = insn->bb_true, *bb_false = insn->bb_false;

	if (bb_true && bb_false) {
		value_t cond = build_is_not_null(builder, emit_pseudo(insn->cond));

		LLVMBuildCondBr(builder, cond, blockof(bb_true), blockof(bb_false));
	} else {
		struct basic_block *succ = bb_true ? bb_true : bb_false;

		LLVMBuildBr(builder, blockof(succ));
	}
}

static value_t emit_binop(builder_t builder, struct instruction *insn)
{
	static const LLVMOpcode iops[] = {
		[OP_ADD]	= LLVMAdd,
		[OP_SUB]	= LLVMSub,
		[OP_MULU]	= LLVMMul,
		[OP_MULS]	= LLVMMul,
		[OP_DIVU]	= LLVMUDiv,
		[OP_DIVS]	= LLVMSDiv,
		[OP_MODU]	= LLVMURem,
		[OP_MODS]	= LLVMSRem,
		[OP_SHL]	= LLVMShl,
		[OP_LSR]	= LLVMLShr,
		[OP_ASR]	= LLVMAShr,
		[OP_AND]	= LLVMAnd,
		[OP_OR]		= LLVMOr,
		[OP_XOR]	= LLVMXor,
		[OP_AND_BOOL]	= LLVMAnd,
		[OP_OR_BOOL]	= LLVMOr,
	};
	static const LLVMOpcode fops[] = {
		[OP_ADD]	= LLVMFAdd,
		[OP_SUB]	= LLVMFSub,
		[OP_MULU]	= LLVMFMul,
		[OP_MULS]	= LLVMFMul,
		[OP_DIVU]	= LLVMFDiv,
		[OP_DIVS]	= LLVMFDiv,
		[OP_MODU]	= LLVMFRem,
		[OP_MODS]	= LLVMFRem,
	};
	unsigned opcode = insn->opcode;
	value_t lhs, rhs;
	struct symbol *ctype = insn->target->ctype;
	type_t type = emit_type(ctype);

	// Reduce operands to 1 bit for logical operations.
	if (opcode == OP_AND_BOOL || opcode == OP_OR_BOOL) {
		// The return type should always be bool.
		assert(is_integer_type(type, 1));
		// lhs's and rhs's types are unknown.
		lhs = build_is_not_null(builder, emit_pseudo(insn->src1));
		rhs = build_is_not_null(builder, emit_pseudo(insn->src2));
	} else {
		lhs = emit_pseudo(insn->src1);
		rhs = emit_pseudo(insn->src2);
	}

	if (is_floating_point_type(type)) {
		return LLVMBuildBinOp(builder, fops[opcode], lhs, rhs, "");
	} else {
		type_t lhs_type = LLVMTypeOf(lhs);
		type_t rhs_type = LLVMTypeOf(rhs);
		value_t v;

		if (is_pointer_type(rhs_type)) {
			swap(lhs, rhs);
			swap(lhs_type, rhs_type);
		}
		assert(is_integer_type(rhs_type, 0));
		// Emit GEP for p + n and p - n.
		if (is_pointer_type(lhs_type)) {
			type_t charp = LLVMPointerType(LLVMInt8Type(), 0);

			lhs = LLVMBuildPointerCast(builder, lhs, charp, "");
			switch (opcode) {
			default: assert(0 && "Unknown pointer operation!");
			case OP_SUB: rhs = LLVMBuildNeg(builder, rhs, "");
			case OP_ADD: break;
			}
			v = LLVMBuildGEP(builder, lhs, &rhs, 1, "");
			assert(is_pointer_type(type));
			return LLVMBuildPointerCast(builder, v, type, "");
		}
		assert(is_integer_type(lhs_type, 0));
		// For x << y, y may not be the same type of x.
		// Truncate or zero-extend y.
		rhs = build_integer_cast(builder, rhs, LLVMTypeOf(lhs), 0);
		v = LLVMBuildBinOp(builder, iops[opcode], lhs, rhs, "");
		switch (opcode) {
		default: break;
		case OP_ADD: case OP_SUB:
			if (!(ctype->ctype.modifiers & MOD_SIGNED))
				break;
			// Fall through.
		case OP_MULS: case OP_SHL:
			set_no_signed_wrap(v);
			break;
		}
		return v;
	}
}

static value_t emit_cmp(builder_t builder, struct instruction *insn)
{
	static const LLVMIntPredicate iops[] = {
		[OP_SET_EQ]	= LLVMIntEQ,
		[OP_SET_NE]	= LLVMIntNE,
		[OP_SET_LE]	= LLVMIntSLE,
		[OP_SET_GE]	= LLVMIntSGE,
		[OP_SET_LT]	= LLVMIntSLT,
		[OP_SET_GT]	= LLVMIntSGT,
		[OP_SET_B]	= LLVMIntULT,
		[OP_SET_A]	= LLVMIntUGT,
		[OP_SET_BE]	= LLVMIntULE,
		[OP_SET_AE]	= LLVMIntUGE,
	};
	static const LLVMRealPredicate fops[] = {
		[OP_SET_EQ]	= LLVMRealOEQ,
		[OP_SET_NE]	= LLVMRealONE,
		[OP_SET_LE]	= LLVMRealOLE,
		[OP_SET_GE]	= LLVMRealOGE,
		[OP_SET_LT]	= LLVMRealOLT,
		[OP_SET_GT]	= LLVMRealOGT,
	};
	unsigned opcode = insn->opcode;
	// insn->type is the type of the operands.
	type_t type = emit_type(insn->target->ctype);
	value_t lhs = emit_pseudo(insn->src1);
	value_t rhs = emit_pseudo(insn->src2);
	value_t v;

	// This should only happen for p == 0 or p != 0.
	if (LLVMTypeOf(lhs) != type) {
		assert(LLVMIsNull(rhs));
		switch (opcode) {
		default: assert(0 && "Illegal integer-pointer comparison!");
		case OP_SET_EQ:
			v = build_is_null(builder, lhs);
			break;
		case OP_SET_NE:
			v = build_is_not_null(builder, lhs);
			break;
		}
	} else {
		assert(LLVMTypeOf(rhs) == type);
		if (is_floating_point_type(type))
			v = LLVMBuildFCmp(builder, fops[opcode], lhs, rhs, "");
		else
			v = LLVMBuildICmp(builder, iops[opcode], lhs, rhs, "");
	}
	// The return type could be something like i32.
	return build_integer_cast(builder, v, type, 0);
}

static value_t emit_select(builder_t builder, struct instruction *insn)
{
	value_t cond, true_val, false_val;

	cond = build_is_not_null(builder, emit_pseudo(insn->src1));
	true_val = emit_pseudo(insn->src2);
	false_val = emit_pseudo(insn->src3);
	return LLVMBuildSelect(builder, cond, true_val, false_val, "");
}

static value_t emit_gep(builder_t builder, struct pseudo *src, unsigned int offset, struct pseudo *dst)
{
	type_t charp = LLVMPointerType(LLVMInt8Type(), 0);
	value_t base = LLVMBuildPointerCast(builder, emit_pseudo(src), charp, "");
	value_t idx = LLVMConstInt(LLVMIntType(bits_in_pointer), offset, 0);
	value_t gep = LLVMBuildGEP(builder, base, &idx, 1, "");
	type_t type = LLVMPointerType(emit_type(dst->ctype), 0);

	return LLVMBuildPointerCast(builder, gep, type, "");
}

// LLVM's phi requires one entry for each predecessor.  It is difficult
// to directly translate sparse's phi nodes to LLVM.  Instead translate
// phi into load/store.
static value_t emit_phi(builder_t builder, struct instruction *insn)
{
	struct pseudo *target = insn->target;
	type_t type = emit_type(target->ctype);
	value_t ptr;

	if (target->priv) {
		// Extract the address from load if generated (by some phisrc).
		ptr = LLVMGetOperand(target->priv, 0);
	} else {
		// Add alloca to entry.
		ptr = alloc_alloca(type, function);
	}
	return LLVMBuildLoad(builder, ptr, "");
}

static void emit_phisrc(builder_t builder, struct instruction *insn)
{
	struct pseudo_user *pu;
	value_t v = emit_pseudo(insn->phi_src);
	block_t blk = blockof(insn->bb);

	FOR_EACH_PTR(insn->target->users, pu) {
		struct instruction *phi = pu->insn;
		struct pseudo *target = phi->target;
		value_t ptr = target->priv;

		assert(phi->opcode == OP_PHI);
		// Test if load <alloca> already emitted at phi.
		if (!ptr) {
			// If not, generate now & restore builder position.
			target->priv = emit_phi(builder, phi);
			LLVMPositionBuilderAtEnd(builder, blk);
		}
		// Extract address from load.
		ptr = LLVMGetOperand(target->priv, 0);
		LLVMBuildStore(builder, v, ptr);
	} END_FOR_EACH_PTR(pu);
}

static value_t emit_cast(builder_t builder, struct instruction *insn)
{
	value_t src = emit_pseudo(insn->src);
	type_t orig_type, type;
	value_t v;

	// Converting to _Bool needs a zero test rather than a truncation.
	if (is_bool_type(insn->target->ctype))
		return build_is_not_null(builder, src);

	orig_type = LLVMTypeOf(src);
	type = emit_type(insn->target->ctype);
	if (is_pointer_type(type)) {
		if (is_pointer_type(orig_type))
			return LLVMBuildPointerCast(builder, src, type, "");
		return build_inttoptr(builder, src, type);
	}
	if (is_pointer_type(orig_type))
		v = build_ptrtoint(builder, src);
	else
		v = src;
	return build_integer_cast(builder, v, type, insn->opcode == OP_SCAST);
}

static LLVMValueRef emit_call(builder_t builder, struct instruction *insn)
{
	int n = pseudo_list_size(insn->arguments), i;
	value_t args[n];
	value_t func = emit_pseudo(insn->func);
	type_t func_type = get_function_type(func);
	int nparams = LLVMCountParamTypes(func_type);
	type_t param_types[nparams];
	struct pseudo *arg;

	LLVMGetParamTypes(func_type, param_types);
	i = 0;
	FOR_EACH_PTR(insn->arguments, arg) {
		value_t v = emit_pseudo(arg);

		// Cast pointer argument type.
		if (i < nparams && is_pointer_type(LLVMTypeOf(v)))
			v = build_pointer_cast(builder, v, param_types[i]);
		args[i++] = v;
	} END_FOR_EACH_PTR(arg);
	return LLVMBuildCall(builder, func, args, n, "");
}

static value_t emit_instruction(builder_t builder, struct instruction *insn)
{
	unsigned opcode = insn->opcode;

	switch (opcode) {
	default: die("Unknown instruction!\n%s", show_instruction(insn));
	case OP_ENTRY:
		// Ignore.
		break;
	case OP_RET:
		emit_ret(builder, insn);
		break;
	case OP_BR:
		emit_br(builder, insn);
		break;
	case OP_BINARY ... OP_BINARY_END:
		return emit_binop(builder, insn);
	case OP_BINCMP ... OP_BINCMP_END:
		return emit_cmp(builder, insn);
	case OP_NOT:
		return LLVMBuildNot(builder, emit_pseudo(insn->src1), "");
	case OP_NEG:
		return LLVMBuildNeg(builder, emit_pseudo(insn->src1), "");
	case OP_SEL:
		return emit_select(builder, insn);
	case OP_LOAD:
		return LLVMBuildLoad(builder, emit_gep(builder, insn->src, insn->offset, insn->target), "");
	case OP_STORE:
		LLVMBuildStore(builder, emit_pseudo(insn->target), emit_gep(builder, insn->src, insn->offset, insn->target));
		break;
	case OP_PHI:
		return emit_phi(builder, insn);
	case OP_PHISOURCE:
		emit_phisrc(builder, insn);
		break;
	case OP_CAST:
	case OP_SCAST:
	case OP_PTRCAST:
		return emit_cast(builder, insn);
	case OP_INLINED_CALL:
	case OP_CALL:
		return emit_call(builder, insn);
	}
	return NULL;
}

void emit_function(module_t m, struct entrypoint *ep)
{
	struct symbol *sym = ep->name;
	struct basic_block *bb;
	builder_t builder;

	assert(sym->type == SYM_NODE);
	assert(is_function(sym->ctype.base_type));

	// Emit the declaration first.
	function = emit_toplevel(m, sym);

	// Emit empty basic blocks first.
	FOR_EACH_PTR(ep->bbs, bb) {
		const char *name = (bb == ep->entry->bb) ? "entry" : "bb";

		blockof(bb) = LLVMAppendBasicBlock(function, name);
	} END_FOR_EACH_PTR(bb);

	// Emit instructions.
	builder = LLVMCreateBuilder();
	FOR_EACH_PTR(ep->bbs, bb) {
		struct instruction *insn;

		LLVMPositionBuilderAtEnd(builder, blockof(bb));
		FOR_EACH_PTR(bb->insns, insn) {
			value_t v;

			// Skip dead instructions.
			if (!insn->bb)
				continue;
			v = emit_instruction(builder, insn);
			if (v) {
				struct pseudo *target = insn->target;
				if (target && target->type != PSEUDO_VOID)
					complete_pseudo(target, v);
			}
		} END_FOR_EACH_PTR(insn);
	} END_FOR_EACH_PTR(bb);
	LLVMDisposeBuilder(builder);

	function = NULL;
}
