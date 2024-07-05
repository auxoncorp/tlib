#ifndef _DISAS_CONTEXT_BASE_GUARD_
#define _DISAS_CONTEXT_BASE_GUARD_

typedef struct DisasContextBase {
    struct TranslationBlock *tb;
    target_ulong pc;
    int mem_idx;
    int is_jmp;
    int guest_profile;
    bool generate_block_exit_check;
} DisasContextBase;

#endif /* _DISAS_CONTEXT_BASE_GUARD_ */
