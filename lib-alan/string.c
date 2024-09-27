#include<stdint.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"

int32_t strlen(const uint8_t *str) {
    int32_t len = 0;
    while (*str++) ++len;
    return len;
}

int32_t strcmp (const uint8_t *s1, const uint8_t *s2) {
	while (*s1 && *s2 && *s1++==*s2++);
	if (!(*s1) && !(*s2)) return 0;
	else if (*s2) return -1;
	else if (*s1) return 1;
	else if (*s1 < *s2) return -1;
	else return 1;
}

void strcpy(uint8_t *trg, const uint8_t *src) {
	while ((*trg++ = *src++));
}

void strcat(uint8_t *trg, const uint8_t *src) {
	while (*trg++);
	while ((*trg++ = *src++));
}

