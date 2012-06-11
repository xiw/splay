// RUN: %splay %s | lli | FileCheck %s

// http://www.ic-net.or.jp/home/takaken/e/queen/index.html

#include <stdio.h>

int SIZE, MASK, COUNT;

static void backtrack(int y, int left, int down, int right)
{
	int bitmap, bit;

	if (y == SIZE) {
		COUNT++;
	} else {
		bitmap = MASK & ~(left | down | right);
		while (bitmap) {
			bit = -bitmap & bitmap;
			bitmap ^= bit;
			backtrack(y+1, (left | bit)<<1, down | bit, (right | bit)>>1);
		}
	}
}

static void nqueens(int n)
{
	SIZE = n;  /*  <- N  */
	COUNT = 0; /* result */
	MASK = (1 << SIZE) - 1;
	backtrack(0, 0, 0, 0);
	printf("N=%d -> %d\n", SIZE, COUNT);
}

int main(void)
{
	// CHECK: N=8 -> 92
	nqueens(8);
	// CHECK: N=9 -> 352
	nqueens(9);
	// CHECK: N=10 -> 724
	nqueens(10);
	return 0;
}
