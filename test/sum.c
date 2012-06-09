// RUN: %splay %s | lli | FileCheck %s

#include <stdio.h>
#include <stdlib.h>

static int sum(int n)
{
	int i, result = 0;

	for (i = 1; i <= n; ++i)
 		result += i;
	return result;
}

int main(int argc, char **argv)
{
	// CHECK: 15
	printf("%d\n", sum(5));
	// CHECK: 5050
	printf("%d\n", sum(100));
}
