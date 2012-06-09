// RUN: %splay %s | lli | FileCheck %s

#include <stdio.h>

int main(int argc, char **argv)
{
	// CHECK: Aloha!
	printf("Aloha!\n");
	return 0;
}
