#include "tack_runtime.h"
#include <stdio.h>

struct str {
	u8* data;
	i32 size;
};

void* builtin_malloc(uptr size) {
	void* malloc(uptr);
	return malloc(size);
}
void builtin_free(void* ptr) {
	void free(void*);
	free(ptr);
}
void* builtin_memcpy(void* dst, void* src, uptr size) {
	void* memcpy(void*, const void*, uptr);
	return memcpy(dst, src, size);
}
void builtin_print(struct str s) { fwrite(s.data, 1, s.size, stdout); }