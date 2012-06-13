// RUN: %splay %s | lli | FileCheck %s
// modified from http://theory.stanford.edu/~amitp/rants/c++-vs-c/test4.c

#include <stdio.h>

typedef int T;

static void mysort(T* data, int N)
{
	int i, j;
	T v, t;

	if (N <= 1) return;

	// Partition elements
	v = data[0];
	i = 0;
	j = N;
	for (;;)
	{
		while (data[++i] < v && i < N) { }
		while (data[--j] > v) { }
		if (i >= j) break;
		t = data[i]; data[i] = data[j]; data[j] = t;
	}
	t = data[i-1]; data[i-1] = data[0]; data[0] = t;
	mysort(data, i-1);
	mysort(data+i, N-i);
}

static void quicksort(T *data, int N)
{
	int i;

	mysort(data, N);
	for (i = 0; i != N; ++i)
		printf("%d ", data[i]);
	printf("\n");
}

#define qsort(a) quicksort(a, sizeof(a) / sizeof(*(a)))
int main(void)
{
	int a0[] = {3, 2, 1};
	int a1[] = {1, 3, 5, 7, 9, 8, 6, 4, 2, 0};
	int a2[] = {1, 9, 1, 5, 4, 6, 7, 8, 12, 25};
	// CHECK: 1 2 3
	qsort(a0);
	// CHECK: 0 1 2 3 4 5 6 7 8 9
	qsort(a1);
	// CHECK: 1 1 4 5 6 7 8 9 12 25
	qsort(a2);
}
