// RUN: %splay -m64 %s | FileCheck %s

#include <string.h>

// CVE-2012-2122

typedef char bad_bool;

bad_bool memcmp_bad_bool(const void *s1, const void *s2, size_t n)
{
	// CHECK: %[[R0:.*]] = call i32 @memcmp(i8* %s1, i8* %s2, i64 %n)
	// CHECK: %[[R1:.*]] = trunc i32 %[[R0]] to i8
	// CHECK: ret i8 %[[R1]]
	return memcmp(s1, s2, n);
}

_Bool memcmp_bool(const void *s1, const void *s2, size_t n)
{
	// CHECK: %[[R0:.*]] = call i32 @memcmp(i8* %s1, i8* %s2, i64 %n)
	// CHECK: %[[R1:.*]] = icmp ne i32 %[[R0]], 0
	// CHECK: ret i1 %[[R1]]
	return memcmp(s1, s2, n);
}
