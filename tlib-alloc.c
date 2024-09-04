#if defined(__linux__)
#define _GNU_SOURCE
#endif
#if defined(__linux__) || defined(__APPLE__)
#include <sys/mman.h>
#elif defined(_WIN32)
#include <memoryapi.h>
#include <handleapi.h>
#endif
#if defined(__APPLE__)
#include <fcntl.h>
#include <stdio.h>
#endif
#include <unistd.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

#include "infrastructure.h"
#include "tlib-alloc.h"

uint8_t *tcg_rw_buffer;
uint8_t *tcg_rx_buffer;

uint64_t code_gen_buffer_size;

intptr_t tcg_wx_diff;

const void* rw_ptr_to_rx(void *ptr)
{
    if (ptr == NULL) {
        // null pointers should not be changed
        return ptr;
    }
    return ptr - tcg_wx_diff;
}
void* rx_ptr_to_rw(const void *ptr)
{
    if (ptr == NULL) {
        // null pointers should not be changed
        return (void*) ptr;
    }
    return (void*) (ptr + tcg_wx_diff);
}

#if defined(__linux__) || defined(__APPLE__)
static bool alloc_code_gen_buf_unified(uint64_t size)
{
    // No write/execute splitting
    int flags = MAP_ANON | MAP_PRIVATE;
    void *rwx = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, flags, -1, 0);
    if (rwx == MAP_FAILED) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to mmap rwx buffer, error: %s", strerror(errno));
        return false;
    }
    tcg_rx_buffer = tcg_rw_buffer = rwx;
    code_gen_buffer_size = size;
    tcg_wx_diff = 0;
    return true;
}
void free_code_gen_buf()
{
    // If not using split buffers the second one will fail, but this causes no issues
    munmap(tcg_rw_buffer, code_gen_buffer_size);
    munmap(tcg_rx_buffer, code_gen_buffer_size);
}
#endif

#if defined(__linux__)
static bool alloc_code_gen_buf_split(uint64_t size)
{
    // Split writable and executable mapping
    int fd = memfd_create("code_gen_buffer", 0);
    if (fd == -1) {
        tlib_abortf("Failed to create backing file for code_gen_buffer, error: %s", strerror(errno));
    }
    if (ftruncate(fd, size) == -1) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to allocate %u bytes for codegen buffer, error: %s", size, strerror(errno));
        // Cleanup the fd
        close(fd);
        return false;
    }
    // Backing file creation succeded, mmap buffers
    int flags = MAP_SHARED;
    void *rw = mmap(NULL, size, PROT_READ | PROT_WRITE, flags, fd, 0);
    if (rw == MAP_FAILED) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to mmap rw buffer, error: %s", strerror(errno));
        close(fd);
        return false;
    }
    void *rx = mmap(NULL, size, PROT_READ | PROT_EXEC, flags, fd, 0);
    if (rw == MAP_FAILED) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to mmap rx buffer, error: %s", strerror(errno));
        close(fd);
        munmap(rw, size);
        return false;
    }
    // Mapping succeded, we can now close the fd safely
    close(fd);
    tcg_rw_buffer = (uint8_t*) rw;
    tcg_rx_buffer = (uint8_t*) rx;
    code_gen_buffer_size = size;
    tcg_wx_diff = tcg_rw_buffer - tcg_rx_buffer;
    return true;
}
#elif defined(__APPLE__)
static bool alloc_code_gen_buf_split(uint64_t size)
{
    return false;
}
#elif defined(_WIN32)
static void map_exec(void *addr, long size)
{
    DWORD old_protect;
    int temp = VirtualProtect(addr, size, PAGE_EXECUTE_READWRITE, &old_protect);
    temp++;
}
static bool alloc_code_gen_buf_split(uint64_t size)
{
    // Split buffer not supported on Windows
    tlib_abort("WX split buffer not supported on Windows");
    return false;
}
static bool alloc_code_gen_buf_unified(uint64_t size)
{
    // No seperate buffer views on windows for now
    uint8_t *buf = tlib_malloc(size);
    if (buf == NULL) {
        return false;
    }
    map_exec(buf, size);
    tcg_rw_buffer = tcg_rx_buffer = buf;
    code_gen_buffer_size = size;
    tcg_wx_diff = 0;
    return true;
}
void free_code_gen_buf()
{
    tlib_free(tcg_rw_buffer);
}
#endif
bool alloc_code_gen_buf(uint64_t size)
{
    bool split_wx = true;
    if (split_wx) {
        return alloc_code_gen_buf_split(size);
    } else {
        return alloc_code_gen_buf_unified(size);
    }
}
