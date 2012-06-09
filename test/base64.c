// RUN: %splay %s | lli | FileCheck %s

/* public domain */

/*
 * arbitrary data on stdin -> BASE64 data on stdout
 *
 * UNIX's newline convention is used, i.e. one ASCII control-j (10 decimal).
 */

#include <stdio.h>

static unsigned char alphabet[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static void base64(const unsigned char *p)
{
	int bits = 0, char_count = 0;

	for (; *p; ++p) {
		bits += *p;
		char_count++;
		if (char_count == 3) {
			putchar(alphabet[bits >> 18]);
			putchar(alphabet[(bits >> 12) & 0x3f]);
			putchar(alphabet[(bits >> 6) & 0x3f]);
			putchar(alphabet[bits & 0x3f]);
			bits = 0;
			char_count = 0;
		} else {
			bits <<= 8;
		}
	}
	if (char_count != 0) {
		bits <<= 16 - (8 * char_count);
		putchar(alphabet[bits >> 18]);
		putchar(alphabet[(bits >> 12) & 0x3f]);
		if (char_count == 1) {
			putchar('=');
			putchar('=');
		} else {
			putchar(alphabet[(bits >> 6) & 0x3f]);
			putchar('=');
		}
	}
	putchar('\n');
}

int main(void)
{
	// CHECK: MTIz
	base64("123");
	// CHECK: aGVsbG8=
	base64("hello");
	// CHECK: VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=
	base64("The quick brown fox jumps over the lazy dog.");
	return 0;
}
