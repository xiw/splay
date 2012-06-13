// RUN: %splay %s | lli | FileCheck %s
// modified from http://sudharsh.wordpress.com/2009/04/09/yet-another-brainfuck-interpreter/

#include <stdio.h>

static char cellspace[30000];
static char *data_pointer = cellspace;

static const char *process_command(char cmd, const char *s)
{
	char c;
	const char *p;

	switch (cmd) {
	case '>':
		++data_pointer;
		break;
	case '<':
		--data_pointer;
		break;
	case '+':
		++*data_pointer;
		break;
	case '-':
		--*data_pointer;
		break;
	case '.':
		putchar(*data_pointer);
		break;
	case ',':
		*data_pointer = getchar();
		break;
	case '[':
		while (*data_pointer) {
			p = s;
			c = *p++;
			while (c && c != ']') {
				p = process_command(c, p);
				c = *p++;
			}
		}
		return p;

	}
	return s;
}

static void eval(const char *s) {

	/*
	 *  Allowed brainfuck commands are,
	 *  > 	increment the data pointer (to point to the next cell to the right).
	 *  < 	decrement the data pointer (to point to the next cell to the left).
	 *  + 	increment (increase by one) the byte at the data pointer.
	 *  - 	decrement (decrease by one) the byte at the data pointer.
	 *  . 	output the value of the byte at the data pointer.
	 *  , 	accept one byte of input, storing its value in the byte at the data pointer.
	 *  [ 	if the byte at the data pointer is zero,
	 *      then instead of moving the instruction pointer forward to the next command,
	 *      jump it forward to the command after the matching ] command.
	 *  ] 	if the byte at the data pointer is nonzero,
	 *      then instead of moving the instruction pointer forward to the next command,
         *      jump it back to the command after the matching [ command.
	 */

	char cmd;

	while ((cmd = *s++))
		s = process_command(cmd, s);
}

int main(void)
{
	// CHECK: Hello World!
	eval("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.");

	return 0;
}
