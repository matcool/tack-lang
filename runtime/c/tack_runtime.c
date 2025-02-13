#include "tack_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct str {
	u8* data;
	i32 size;
};

void* tack_malloc(uptr size) { return malloc(size); }
void tack_free(void* ptr) { free(ptr); }
void* tack_memcpy(void* dst, void* src, uptr size) {
	return memcpy(dst, src, size);
}
void tack_print(struct str s) { fwrite(s.data, 1, s.size, stdout); }