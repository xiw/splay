#pragma once

#include "llvm.h"

struct entrypoint;
struct symbol;

extern void emit_function(module_t m, struct entrypoint *ep);

extern value_t emit_toplevel(module_t m, struct symbol *sym);

extern type_t emit_type(struct symbol *sym);
