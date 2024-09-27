#include <stdint.h> 

int8_t shrink(int32_t input) {
    return (int8_t)(input & 0xFF); 
}

int32_t extend(int8_t input) {
	return (int32_t) input;
}

