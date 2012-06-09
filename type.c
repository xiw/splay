#include "lib.h"
#include "sparse/expression.h"
#include "sparse/symbol.h"
#include <assert.h>
#include <string.h>

static type_t emit_type_begin(struct symbol *sym)
{
	struct symbol *base_type = sym->ctype.base_type;

	if (sym == &void_ctype)
		return LLVMVoidType();

	// int_type is the base type of all [INTEGER]_ctype.
	if (base_type == &int_type)
		return LLVMIntType(sym->bit_size);

	if (sym->type == SYM_ENUM)
		return emit_type(base_type);

	if (sym->type == SYM_BITFIELD)
		return LLVMIntType(sym->bit_size);

	if (sym->type == SYM_PTR) {
		type_t elem_type = emit_type(base_type);

		// Fix up void * to i8 *.
		if (LLVMGetTypeKind(elem_type) == LLVMVoidTypeKind)
			elem_type = LLVMInt8Type();
		return LLVMPointerType(elem_type, 0);
	}

	if (sym->type == SYM_ARRAY) {
		type_t elem_type = emit_type(base_type);
		unsigned n = get_expression_value(sym->array_size);

		return LLVMArrayType(elem_type, n);
	}

	if (sym->type == SYM_STRUCT) {
		const char *prefix = "struct.";
		const char *name = sym->ident ? show_ident(sym->ident) : "anno";
		char buf[strlen(name) + strlen(prefix) + 1];

		strcpy(buf, prefix);
		strcat(buf, name);
		return LLVMStructCreateNamed(LLVMGetGlobalContext(), buf);
	}

	if (sym->type == SYM_FN) {
		struct symbol *arg;
		int n = symbol_list_size(sym->arguments), i;
		type_t ret_type, arg_types[n];

		ret_type = emit_type(base_type);
		i = 0;
		FOR_EACH_PTR(sym->arguments, arg) {
			arg_types[i++] = emit_type(arg->ctype.base_type);
		} END_FOR_EACH_PTR(arg);
		return LLVMFunctionType(ret_type, arg_types, n, sym->variadic);
	}

	if (sym == &float_ctype)
		return LLVMFloatType();

	if (sym == &double_ctype)
		return LLVMDoubleType();

	if (sym == &ldouble_ctype) {
		switch (sym->bit_size) {
		default: assert(0 && "Unknown long double size!");
		case 64: return LLVMDoubleType();
		case 80: return LLVMX86FP80Type();
		case 128: return LLVMFP128Type();
		}
	}

	show_symbol(sym);
	assert(0 && "Unknown type!");
}

static void emit_type_end(struct symbol *sym, type_t type)
{
	// Fill in struct body.
	if (sym->type == SYM_STRUCT) {
		int n = symbol_list_size(sym->symbol_list), i;
		type_t elem_types[n];
		struct symbol *member;

		assert(LLVMGetTypeKind(type) == LLVMStructTypeKind);
		if (n == 0)
			return;
		i = 0;
		FOR_EACH_PTR(sym->symbol_list, member) {
			elem_types[i++] = emit_type(member);
		} END_FOR_EACH_PTR(member);
		LLVMStructSetBody(type, elem_types, n, 0);
	}
}

type_t emit_type(struct symbol *sym)
{
	type_t type;

	if (sym->type == SYM_NODE)
		sym = sym->ctype.base_type;
	// ->aux points to llvm::Type.
	if (sym->aux)
		return sym->aux;
	type = emit_type_begin(sym);
	sym->aux = type;
	emit_type_end(sym, type);
	return type;
}
