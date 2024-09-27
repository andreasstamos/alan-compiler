#include<stdio.h>
#include <stdint.h> 

void writeByte(uint8_t x) {
	printf("%hhd", x);
}

void writeInteger(int32_t x) {
	printf("%d", x);
}

void writeChar(uint8_t x) {
	printf("%c", x);
}

void writeString(uint8_t *x) {
	printf("%s", x);
}

int8_t readByte(void) {
	int8_t x;
	scanf("%hhd", &x);
	return x;
}

int32_t readInteger(void) {
	int32_t x; 
	scanf("%d", &x);
	return x; 
}

int32_t readChar(void) {
	return getchar();
}

void readString(int32_t n, uint8_t *s) {
	uint8_t *s1;
	s1 = (uint8_t *) fgets((char *) s, n, stdin);
	if (!s1 && n>0) s[0] = '\0';
	
	// according to given semantics, readString must not return the newline
	else if (s1) {
		uint8_t *send = s;
		while (*send && *send != '\n' && *send != '\r') ++send;
		*send = '\0';
	}
}

