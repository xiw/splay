// RUN: %splay -m32 %s | FileCheck %s

// CHECK: @s = global i8 120
char s = 'x';

// CHECK: @shrt = global i16 -1
unsigned short shrt = 65535;

// CHECK: def = common global i32 0
int def;

// CHECK: @opt = constant i32 42
const int opt = 42;

// CHECK: @bigv = global i64 -1
unsigned long long bigv = -1;

// CHECK: @ratio = global float 1.000000e+00
float ratio = 1.0f;

// CHECK: @half = global double 5.000000e-01
double half = 0.5;
