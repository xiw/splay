// RUN: %splay -m32 %s | FileCheck %s

// CHECK: @buf = common global [64 x i8] zeroinitializer
char buf[64];

// CHECK: @fib = global [5 x i32] [i32 1, i32 1, i32 2, i32 3, i32 5]
int fib[] = {1, 1, 2, 3, 5};

// CHECK: @fib_less = global [2 x i32] [i32 1, i32 1]
int fib_less[2] = {1, 1, 2, 3, 5};

// CHECK: @fib_more = global [6 x i32] [i32 1, i32 1, i32 2, i32 3, i32 5, i32 0]
int fib_more[6] = {1, 1, 2, 3, 5};

// CHECK: @str = global [4 x i8] c"abc\00"
char str[] = "abc";

// CHECK: @str_less = global [1 x i8] c"a"
char str_less[1] = "abc";

// CHECK: @str_more = global [5 x i8] c"abc\00\00"
char str_more[5] = "abc";

// CHECK: @version = global i8* getelementptr inbounds ([4 x i8]* @[[VERSION:.*]], i32 0, i32 0)
// CHECK: @[[VERSION]] = internal global [4 x i8] c"3.0\00"
char *version = "3.0";
