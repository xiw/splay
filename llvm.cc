/* LLVM utility functions.
 *
 * It is written in C++ and cannot include sparse headers.
 */
#include "llvm.h"
#include <llvm/BasicBlock.h>
#include <llvm/Constant.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Instructions.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/InstIterator.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/SSAUpdater.h>
#include <assert.h>

// Forward declarations in sparse/lib.h.
extern int bits_in_pointer;

static const char *get_data_layout(const llvm::Triple &triple)
{
	static const char *I386_LINUX =
		"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-"
		"f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-"
		"f80:32:32-n8:16:32-S128";
	static const char *I386_MACOSX =
		"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-"
		"f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-"
		"f80:128:128-n8:16:32-S128";
	static const char *X86_64_LINUX =
		"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-"
		"f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-"
		"f80:128:128-n8:16:32:64-S128";
	static const char *X86_64_MACOSX =
		"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-"
		"f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-"
		"f80:128:128-n8:16:32:64-S128";

	switch (triple.getArch()) {
	default: assert(0 && "Unsupported architecture!");
	case llvm::Triple::x86:
		switch (triple.getOS()) {
		default: break;
		case llvm::Triple::Linux:
			return I386_LINUX;
		case llvm::Triple::MacOSX:
		case llvm::Triple::Darwin:
			return I386_MACOSX;
		}
	case llvm::Triple::x86_64:
		switch (triple.getOS()) {
		default: break;
		case llvm::Triple::Linux:
			return X86_64_LINUX;
		case llvm::Triple::MacOSX:
		case llvm::Triple::Darwin:
			return X86_64_MACOSX;
		}		
	}
	assert(0 && "Unsupported OS!");
}

module_t alloc_module(const char *name)
{
	module_t m = LLVMModuleCreateWithName(name);
	llvm::Triple triple(llvm::sys::getDefaultTargetTriple());

	// Set target triple.
	switch (triple.getArch()) {
	default: break;
	case llvm::Triple::x86:
		if (bits_in_pointer == 64)
			triple.setArch(llvm::Triple::x86_64);
		break;
	case llvm::Triple::x86_64:
		if (bits_in_pointer == 32)
			triple.setArch(llvm::Triple::x86);
		break;
	}
	llvm::unwrap(m)->setTargetTriple(triple.str());

	// Set data layout.
	llvm::unwrap(m)->setDataLayout(get_data_layout(triple));

	return m;
}

void verify_module(module_t m)
{
	llvm::verifyModule(*llvm::unwrap(m));
}

void print_module(module_t m, int fd)
{
	llvm::raw_fd_ostream os(fd, false);
	llvm::unwrap(m)->print(os, NULL);
}

int is_integer_type(type_t type, unsigned nbits)
{
	llvm::Type *ty = llvm::unwrap(type);

	if (nbits == 0)
		return ty->isIntegerTy();
	return ty->isIntegerTy(nbits);
}

int is_floating_point_type(type_t type)
{
	return llvm::unwrap(type)->isFloatingPointTy();
}

type_t get_function_type(value_t v)
{
	return llvm::wrap(llvm::unwrap<llvm::Function>(v)->getFunctionType());
}

value_t alloc_alloca(type_t type, value_t func)
{
	llvm::BasicBlock &bb = llvm::unwrap<llvm::Function>(func)->getEntryBlock();
	llvm::Instruction *ip, *v;
	llvm::Type *ty = llvm::unwrap(type);

	if (bb.empty())
		ip = bb.end();
	else
		ip = bb.getFirstNonPHI();
	v = new llvm::AllocaInst(ty, "");
	bb.getInstList().insert(ip, v);
	if (ty->isArrayTy()) {
		llvm::Type *intptr = llvm::Type::getIntNTy(llvm::getGlobalContext(), bits_in_pointer);
		llvm::Value *zero = llvm::Constant::getNullValue(intptr);
		llvm::Value *indices[] = {zero, zero};

		v = llvm::GetElementPtrInst::Create(v, indices);
		bb.getInstList().insert(ip, v);
	}
	
	return llvm::wrap(v);
}

value_t alloc_placeholder(type_t type, block_t blk)
{
	return llvm::wrap(llvm::PHINode::Create(llvm::unwrap(type),
		0, "", llvm::unwrap(blk)));
}

void free_placeholder(value_t ph, value_t v)
{
	LLVMReplaceAllUsesWith(ph, v);
	LLVMInstructionEraseFromParent(ph);
}

value_t first_non_phi(block_t blk)
{
	llvm::BasicBlock *bb = llvm::unwrap(blk);
	llvm::Instruction *i = bb->getFirstNonPHI();

	if (i == bb->end())
		return NULL;
	return llvm::wrap(i);
}

void set_no_signed_wrap(value_t binop)
{
	llvm::unwrap<llvm::BinaryOperator>(binop)->setHasNoSignedWrap();
}

void set_no_unsigned_wrap(value_t binop)
{
	llvm::unwrap<llvm::BinaryOperator>(binop)->setHasNoUnsignedWrap();
}

value_t get_integer_value(type_t type, long long value)
{
	llvm::Type *ty = llvm::unwrap(type);
	int nbits = ty->isPointerTy() ? bits_in_pointer : ty->getScalarSizeInBits();
	llvm::APInt apv(nbits, value);

	return llvm::wrap(llvm::Constant::getIntegerValue(ty, apv));
}

value_t resize_constant_array(value_t v, unsigned int size)
{
	llvm::Constant *c = llvm::unwrap<llvm::Constant>(v);
	llvm::ArrayType *ty = llvm::cast<llvm::ArrayType>(c->getType());
	unsigned int n = ty->getNumElements(), i;
	llvm::SmallVector<llvm::Constant *, 16> elems(n);
	llvm::Type *elemty = ty->getElementType();
	llvm::Constant *zero = llvm::Constant::getNullValue(elemty);

	for (i = 0; i != n; ++i)
		elems[i] = c->getAggregateElement(i);
	elems.resize(size, zero);
	return llvm::wrap(llvm::ConstantArray::get(llvm::ArrayType::get(elemty, size), elems));
}

value_t build_integer_cast(builder_t builder, value_t src, type_t type, int is_signed)
{
	return llvm::wrap(llvm::unwrap(builder)->CreateIntCast(
		llvm::unwrap(src), llvm::unwrap(type), is_signed, ""));
}
