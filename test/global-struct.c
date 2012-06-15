// RUN: %splay %s | FileCheck %s

struct X {
	int x;
	int y;
};

// CHECK: @gx0 = common global %struct.X zeroinitializer
struct X gx0;

// CHECK: @gx1 = global %struct.X { i32 1, i32 0 }
struct X gx1 = {1};

// CHECK: @gx2 = global %struct.X { i32 123, i32 456 }
struct X gx2 = {123, 456};

// CHECK: @gx3 = global %struct.X { i32 0, i32 2 }
struct X gx3 = {.y = 2};

// CHECK: @gxarr = global [2 x %struct.X] [%struct.X { i32 1, i32 2 }, %struct.X { i32 3, i32 4 }]
struct X gxarr[] = {{1, 2}, {3, 4}};

struct A {
	int a;
	struct {
		int b, c;
	};
};

// CHECK: @ga1 = global %struct.A { i32 1, %struct.{{.*}} { i32 2, i32 3 } }
struct A ga1 = {1, {2, 3}};

// CHECK: @ga2 = global %struct.A { i32 1, %struct.{{.*}} zeroinitializer }
struct A ga2 = {1};

struct B {
	struct B *next;
};

struct B self = {.next = &self};
