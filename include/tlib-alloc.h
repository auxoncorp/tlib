#pragma once

#include <stdint.h>

extern uint8_t *rw_buffer;
extern uint8_t *rx_buffer;

bool alloc_code_gen_buf(uint64_t size);
void free_code_gen_buf();
