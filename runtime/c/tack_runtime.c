#include "tack_runtime.h"
#include <stdio.h>

struct str {
	u8* data;
	i32 size;
};

void* tack_malloc(uptr size) {
	void* malloc(uptr);
	return malloc(size);
}
void tack_free(void* ptr) {
	void free(void*);
	free(ptr);
}
void* tack_memcpy(void* dst, void* src, uptr size) {
	void* memcpy(void*, const void*, uptr);
	return memcpy(dst, src, size);
}
void tack_print(struct str s) { fwrite(s.data, 1, s.size, stdout); }