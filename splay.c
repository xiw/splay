#include "lib.h"
#include "sparse/expression.h"
#include "sparse/symbol.h"
#include "sparse/lib.h"
#include "sparse/linearize.h"
#include <unistd.h>

static void compile(module_t m, struct symbol_list *list)
{
	struct symbol *sym;

	FOR_EACH_PTR(list, sym) {
		struct entrypoint *ep;

		expand_symbol(sym);
		ep = linearize_symbol(sym);
		if (ep)
			emit_function(m, ep);
		else
			emit_toplevel(m, sym);
	} END_FOR_EACH_PTR(sym);
}

int main(int argc, char **argv)
{
	struct string_list *filelist = NULL;
	struct symbol_list *builtins;
	char *file;

	builtins = sparse_initialize(argc, argv, &filelist);
	FOR_EACH_PTR_NOTAG(filelist, file) {
		struct symbol_list *syms;
		module_t m;

		syms = sparse(file);
		if (die_if_error)
			return 1;
		m = alloc_module(file);
		compile(m, builtins);
		compile(m, syms);
		verify_module(m);
		print_module(m, STDOUT_FILENO);
		free_module(m);
	} END_FOR_EACH_PTR_NOTAG(file);
	return 0;
}
