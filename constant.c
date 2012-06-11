#include "lib.h"
#include "sparse/expression.h"
#include "sparse/linearize.h"
#include "sparse/parse.h"
#include "sparse/symbol.h"
#include <assert.h>

static value_t emit_function_declaration(module_t m, struct symbol *sym)
{
	struct symbol *base_type = sym->ctype.base_type;
	type_t type;
	value_t func;
	struct symbol *arg;
	int i;

	assert(sym->type == SYM_NODE);
	assert(is_function(base_type));
	type = emit_type(base_type);
	func = LLVMAddFunction(m, show_ident(sym->ident), type);

	// Set argument names.
	i = 0;
	FOR_EACH_PTR(base_type->arguments, arg) {
		if (arg->ident) {
			LLVMSetValueName(LLVMGetParam(func, i),
				show_ident(arg->ident));
		}
		++i;
	} END_FOR_EACH_PTR(arg);

	// TODO: set attributes.
	return func;
}

static value_t emit_subexpr(module_t m, struct expression *expr)
{
	struct expression *subexpr;

	switch (expr->type) {
	default: assert(0 && "Unknown initializer expression!");
	case EXPR_POS:
		subexpr = expr->init_expr;
		break;
	case EXPR_INITIALIZER:
		subexpr = expr;
		break;
	}
	return emit_constant(m, subexpr);
}

static value_t emit_array(module_t m, struct expression *expr)
{
	struct expression *entry;
	int n = expression_list_size(expr->expr_list), i;
	type_t type = LLVMGetElementType(emit_type(expr->ctype));
	value_t elems[n];

	i = 0;
	FOR_EACH_PTR(expr->expr_list, entry) {
		elems[i++] = emit_subexpr(m, entry);
	} END_FOR_EACH_PTR(entry);

	return LLVMConstArray(type, elems, n);
}

// Any easier way to determine a struct member's index?
static int get_member_index(struct symbol *haystack, struct symbol *needle)
{
	struct symbol *member;
	int i;

	if (haystack->type == SYM_NODE)
		haystack = haystack->ctype.base_type;
	assert(haystack->type == SYM_STRUCT);
	assert(needle->type == SYM_NODE);
	i = 0;
	FOR_EACH_PTR(haystack->symbol_list, member) {
		if (member == needle)
			return i;
		++i;
	} END_FOR_EACH_PTR(member);
	assert(0 && "Not found!");
}

static value_t emit_struct(module_t m, struct expression *expr)
{
	type_t type = emit_type(expr->ctype);
	int n = LLVMCountStructElementTypes(type), i;
	type_t subtypes[n];
	value_t elems[n];
	struct expression *entry;

	// Get member types.
	LLVMGetStructElementTypes(type, subtypes);
	// Fill default values.
	for (i = 0; i != n; ++i)
		elems[i] = LLVMConstNull(subtypes[i]);
	// Fill initialization values.
	FOR_EACH_PTR(expr->expr_list, entry) {
		// Recover the index (e.g., for designated initializer).
		int idx = get_member_index(expr->ctype, entry->ctype);

		elems[idx] = emit_subexpr(m, entry);
	} END_FOR_EACH_PTR(entry);
	return LLVMConstNamedStruct(type, elems, n);
}

static value_t emit_string(struct expression *expr) {
	return LLVMConstString(expr->string->data, expr->string->length, 1); 
}

value_t emit_constant(module_t m, struct expression *expr)
{
	type_t type = emit_type(expr->ctype);

	if (expr->type == EXPR_VALUE)
		return get_integer_value(type, expr->value);

	if (expr->type == EXPR_FVALUE)
		return LLVMConstReal(type, expr->fvalue);

	// Aggregate.
	if (expr->type == EXPR_INITIALIZER) {
		switch (LLVMGetTypeKind(type)) {
		default: assert(0 && "Unknown aggregate!");
		case LLVMArrayTypeKind:
			return emit_array(m, expr);
		case LLVMStructTypeKind:
			return emit_struct(m, expr);
		}
	}

	if (expr->type == EXPR_STRING)
		return emit_string(expr);

	if (expr->type == EXPR_SYMBOL)
		return emit_toplevel(m, expr->symbol);

	// char s[] = "abc";
	if (expr->type == EXPR_PREOP) {
		if (expr->op == '*' && expr->unop->type == EXPR_SYMBOL) {
			struct expression *initializer = expr->unop->symbol->initializer;

			if (initializer)
				return emit_constant(m, initializer);
		}
	}

	if (expr->type == EXPR_LABEL) {
		block_t blk = expr->symbol->bb_target->priv;

		assert(blk && "Basic blocks should have been visited!");
		return get_block_address(blk);
	}

	show_expression(expr);
	assert(0 && "Unknown expression!");
}

static value_t array_to_pointer(value_t v)
{
	value_t zero = LLVMConstNull(LLVMIntType(bits_in_pointer));
	value_t indices[] = {zero, zero};

	return LLVMConstGEP(v, indices, ARRAY_SIZE(indices));
}

static value_t emit_global_variable(module_t m, struct symbol *sym)
{
	type_t type = emit_type(sym), orig_type = type;
	value_t v, initializer = NULL;

	assert(sym->type == SYM_NODE);

	if (sym->initializer) {
		initializer = emit_constant(m, sym->initializer);
		orig_type = LLVMTypeOf(initializer);
		if (type != orig_type) {
			if (is_array_type(type)) {
				unsigned int size, orig_size;

				assert(is_array_type(orig_type));
				size = LLVMGetArrayLength(type);
				orig_size = LLVMGetArrayLength(orig_type);
				assert(size != orig_size);
				if (size == 0)
					type = orig_type;
			}
		}
	}

	v = LLVMAddGlobal(m, type, sym->ident ? show_ident(sym->ident) : "");
	sym->aux = v;

	// Set attributes.
	if (sym->ctype.alignment)
		LLVMSetAlignment(v, sym->ctype.alignment);
	if (sym->ctype.modifiers & MOD_CONST)
		LLVMSetGlobalConstant(v, 1);
	if (!sym->initializer) {
		if (sym->ctype.modifiers & MOD_EXTERN)
			LLVMSetLinkage(v, LLVMExternalLinkage);
		else
			LLVMSetLinkage(v, LLVMCommonLinkage);
	}
	if (sym->ctype.modifiers & MOD_STATIC)
		LLVMSetLinkage(v, LLVMInternalLinkage);
	if (sym->ctype.modifiers & MOD_TLS)
		LLVMSetThreadLocal(v, 1);

	// External declaration, no initializer.
	if (sym->ctype.modifiers & MOD_EXTERN) {
		assert(!initializer);
		return v;
	}

	if (!initializer) {
		initializer = LLVMConstNull(type);
	} else if (is_array_type(type)) {
		unsigned int size = LLVMGetArrayLength(type);

		// Reset array size.
		if (size != LLVMGetArrayLength(orig_type))
			initializer = resize_constant_array(initializer, size);
		
	}
	LLVMSetInitializer(v, initializer);
	// Convert array type to pointer.
	return is_array_type(type) ? array_to_pointer(v) : v;
}

value_t emit_toplevel(module_t m, struct symbol *sym)
{
	// ->aux points to llvm::Value.
	// SYM_LABEL may use ->bb_target, in the same union of ->aux.
	assert(sym->type != SYM_LABEL);
	if (sym->aux)
		return sym->aux;

	if (is_function(sym->ctype.base_type))
		sym->aux = emit_function_declaration(m, sym);
	else
		sym->aux = emit_global_variable(m, sym);
	return sym->aux;
}
