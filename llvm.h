#pragma once

#include <llvm-c/Core.h>

typedef LLVMBasicBlockRef	block_t;
typedef LLVMBuilderRef		builder_t;
typedef LLVMModuleRef		module_t;
typedef LLVMTypeRef		type_t;
typedef LLVMValueRef		value_t;

#ifdef __cplusplus
extern "C" {
#endif

extern int bits_in_pointer;

module_t alloc_module(const char *name);

static inline void free_module(module_t m)
{
	return LLVMDisposeModule(m);
}

void verify_module(module_t m);

void print_module(module_t m, int fd);

static inline int is_array_type(type_t type)
{
	return LLVMGetTypeKind(type) == LLVMArrayTypeKind;
}

static inline int is_pointer_type(type_t type)
{
	return LLVMGetTypeKind(type) == LLVMPointerTypeKind;
}

int is_integer_type(type_t type, unsigned nbits);

int is_floating_point_type(type_t type);

type_t get_function_type(value_t v);

value_t alloc_alloca(type_t type, value_t func);

value_t alloc_placeholder(type_t type, block_t blk);

void free_placeholder(value_t ph, value_t v);

void set_no_signed_wrap(value_t binop);

// Create integer/pointer/vector.
value_t get_integer_value(type_t type, long long value);

static inline value_t get_block_address(block_t blk)
{
	return LLVMBlockAddress(LLVMGetBasicBlockParent(blk), blk);
}

value_t resize_constant_array(value_t v, unsigned int size);

value_t build_integer_cast(builder_t builder, value_t src, type_t type, int is_signed);

static inline value_t build_pointer_cast(builder_t builder, value_t src, type_t type)
{
	return LLVMBuildPointerCast(builder, src, type, "");
}

static inline value_t build_ptrtoint(builder_t builder, value_t src)
{
	return LLVMBuildPtrToInt(builder, src, LLVMIntType(bits_in_pointer), "");
}

static inline value_t build_inttoptr(builder_t builder, value_t src, type_t type)
{
	return LLVMBuildIntToPtr(builder, src, type, "");
}

static inline value_t build_is_null(builder_t builder, value_t v)
{
	return LLVMBuildIsNull(builder, v, "");
}

static inline value_t build_is_not_null(builder_t builder, value_t v)
{
	return LLVMBuildIsNotNull(builder, v, "");
}

value_t build_gep(builder_t builder, value_t base, value_t offset);

void add_switch_cases(value_t v, long long begin, long long end, block_t blk);

#ifdef __cplusplus
}
#endif
