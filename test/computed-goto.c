// RUN: %splay %s | FileCheck %s

#include <stdio.h>
#include <stdlib.h>

int foo(int x)
{
	void *array[] = {&&fatal, &&warning, &&good};
	// CHECK: indirectbr i8* {{.*}} [label %[[FATAL:.*]], label %[[WARNING:.*]], label %[[GOOD:.*]]]
	goto *array[x];
fatal:
	// CHECK: [[FATAL]]:
	fprintf(stderr, "faltal!\n");
	exit(1);
warning:
	// CHECK: [[WARNING]]:
	fprintf(stderr, "warning!\n");
	return 1;
good:
	// CHECK: [[GOOD]]:
	return 0;
}
