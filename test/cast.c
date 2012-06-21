// RUN: %splay -m32 %s | FileCheck %s

// CHECK: icmp ne i16 %x, 0
_Bool inttobool(short x)		{ return x; }
// CHECK: fcmp une float %x, 0.000000e+00
_Bool fptobool(float x)			{ return x; }
// CHECK: icmp ne i8* %x, null
_Bool ptrtobool(void *x)		{ return x; }

// CHECK: sext
long ssext(short x)			{ return x; }
// CHECK: zext
long suext(unsigned short x)		{ return x; }
// CHECK: sext
unsigned long usext(short x)		{ return x; }
// CHECK: zext
unsigned long uuext(unsigned short x)	{ return x; }

// CHECK: trunc
short itrunc(long x)			{ return x; }

// CHECK: %[[S2PTR:.*]] = sext
// CHECK: inttoptr {{.*}} %[[S2PTR]] to i8*
void *s2ptr(short x)			{ return x; }
// CHECK: %[[U2PTR:.*]] = zext
// CHECK: inttoptr {{.*}} %[[U2PTR]] to i8*
void *u2ptr(unsigned short x)		{ return x; }

// CHECK: sitofp
float s2fp(short x)			{ return x; }
// CHECK: uitofp
float u2fp(unsigned short x)		{ return x; }

// CHECK: fptosi
short fp2s(float x)			{ return x; }
// CHECK: fptoui
unsigned short fp2u(float x)		{ return x; }

// CHECK: ptrtoint
short ptr2i(void *x)			{ return x; }

// CHECK: fptrunc
float d2f(double x)			{ return x; }
// CHECK: fpext
double f2d(float x)			{ return x; }
