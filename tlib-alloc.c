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

uint8_t *rw_buffer;
uint8_t *rx_buffer;
static uint64_t buffer_size;

// old buffer to test the refactor
uint8_t *code_gen_buffer;
uint64_t code_gen_buffer_size;

#if defined(__linux__)
static int create_backing_fd() 
{
    return memfd_create("code_gen_buffer", 0);
}
static void cleanup() // No extra cleanup needed on linux
{
}
#elif defined(__APPLE__)
static int create_backing_fd() 
{
    // Since shm_open requires a unique name, use current pid to not step on other tlib instances
    char pid_str[25];
    int pid = getpid();
    sprintf(pid_str, "/%i", pid); // POSIX standard recomends the name start with a slash
    tlib_printf(LOG_LEVEL_WARNING, "pid_str: %s", pid_str);
    return shm_open(pid_str, O_RDWR | O_CREAT | O_TRUNC, 0777);
}
static void cleanup()
{
    char pid_str[25];
    int pid = getpid();
    sprintf(pid_str, "/%i", pid); // Get the buffer name same way as in create_backing_fd()
    shm_unlink(pid_str);
}
#endif

#if defined(__linux__) || defined(__APPLE__)
bool alloc_code_gen_buf(uint64_t size)
{
    int fd = create_backing_fd();
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
        cleanup();
        return false;
    }
    void *rx = mmap(NULL, size, PROT_READ | PROT_EXEC, flags, fd, 0);
    if (rw == MAP_FAILED) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to mmap rx buffer, error: %s", strerror(errno));
        close(fd);
        munmap(rw, size);
        cleanup();
        return false;
    }
    void *rwx = mmap(NULL, size, PROT_READ | PROT_EXEC | PROT_WRITE, flags, fd, 0);
    if (rwx == MAP_FAILED) {
        tlib_printf(LOG_LEVEL_DEBUG, "Failed to mmap rwx buffer, error: %s", strerror(errno));
        close(fd);
        munmap(rw, size);
        munmap(rx, size);
        cleanup();
        return false;
    }
    // Mapping succeded, we can now close the fd safely
    close(fd);
    rw_buffer = (uint8_t*) rw;
    rx_buffer = (uint8_t*) rx;
    code_gen_buffer = rwx;
    buffer_size = size;
    return true;
}
void free_code_gen_buf()
{
    if (munmap(rw_buffer, buffer_size) == -1) {
        tlib_abort("Failed to free rw_buffer");
    }
    if (munmap(rx_buffer, buffer_size) == -1) {
        tlib_abort("Failed to free rx_buffer");
    }
    if (munmap(code_gen_buffer, buffer_size) == -1) {
        tlib_abort("Failed to free rwx_buffer");
    }
    cleanup();
}
#elif defined(_WIN32)
static void map_exec(void *addr, long size)
{
    DWORD old_protect;
    int temp = VirtualProtect(addr, size, PAGE_EXECUTE_READWRITE, &old_protect);
    temp++;

}
bool alloc_code_gen_buf(uint64_t size)
{
    // No seperate buffer views on windows for now
    uint8_t *buf = tlib_malloc(size);
    if (buf == NULL) {
        return false;
    }
    map_exec(buf, size);
    rw_buffer = buf;
    rx_buffer = buf;
    code_gen_buffer = buf;
    buffer_size = size;
    return true;
}
void free_code_gen_buf()
{
    tlib_free(rw_buffer);
}
#endif
