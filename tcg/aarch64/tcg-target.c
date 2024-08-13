#include "tcg-target.h" //Not actually needed here, but keeps the editor happy

// Order registers get picked in
static const int tcg_target_reg_alloc_order[] = {
    TCG_REG_R8,
    TCG_REG_R9,
    TCG_REG_R10,
    TCG_REG_R11,
    TCG_REG_R12,
    TCG_REG_R13,
    TCG_REG_R14,
    TCG_REG_R15,
    TCG_REG_R16,
    TCG_REG_R17,
    TCG_REG_R18,
    TCG_REG_R19,
    TCG_REG_R20,
    TCG_REG_R0,
    TCG_REG_R1,
    TCG_REG_R2,
    TCG_REG_R3,
    TCG_REG_R4,
    TCG_REG_R5,
    TCG_REG_R6,
    TCG_REG_R7,
    TCG_REG_R21,
    TCG_REG_R22,
    TCG_REG_R23,
    TCG_REG_R24,
    TCG_REG_R25,
    TCG_REG_R26,
    TCG_REG_R27,
    TCG_REG_R28,
    TCG_REG_R29,
    TCG_REG_R30, // LR
};

// Registers that can be used for input functions arguments
static const int tcg_target_call_iarg_regs[8] = {
    TCG_REG_R0,
    TCG_REG_R1,
    TCG_REG_R2,
    TCG_REG_R3,
    TCG_REG_R4,
    TCG_REG_R5,
    TCG_REG_R6,
    TCG_REG_R7,
};
static inline int tcg_target_get_call_iarg_regs_count(int flags)
{
    return 8;
}
// Registers that can be used for output functions arguments
static const int tcg_target_call_oarg_regs[8] = {
    TCG_REG_R0,
    TCG_REG_R1,
    TCG_REG_R2,
    TCG_REG_R3,
    TCG_REG_R4,
    TCG_REG_R5,
    TCG_REG_R6,
    TCG_REG_R7,
};

static int target_parse_constraint(TCGArgConstraint *ct, const char **pct_str)
{
    // This is just taken from the arm target, might want to refactor to something cleaner
    const char *ct_str;
    ct_str = *pct_str;
    switch (ct_str[0]) {
        case 'r':
            ct->ct |= TCG_CT_REG;
            tcg_regset_set32(ct->u.regs, 0, (1 << 30) - 1);
            break;
        default:
            tcg_abortf("Constraint %c not implemented", ct_str[0]);
            return -1;
    }
    ct_str++;
    *pct_str = ct_str;

    return 0;
}
static inline int tcg_target_const_match(tcg_target_long val, const TCGArgConstraint *arg_ct)
{
    // Don't really understand this function, so just return 0 for now
    return 0;
}

static void patch_reloc(uint8_t *code_ptr, int type, tcg_target_long value, tcg_target_long addend)
{
    tcg_abortf("patch_reloc not implemented");
}

static inline void tcg_out_bl(TCGContext *s, int offset)
{
    tcg_out32(s, 0x94000000 | (offset << 0));
}
static inline void tcg_out_blr(TCGContext *s, int reg)
{
    tcg_out32(s, 0xd63f0000 | (reg << 0));
}
// Heper to generate function calls to constant address
static inline void tcg_out_calli(TCGContext *s, tcg_target_ulong addr) 
{
    // Offset is only 26-bits, so we can't jump further than that without storing it in a reg first
    int offset = addr - (tcg_target_long)s->code_ptr;
    if (abs(offset) > 0xfffff) {
        tcg_abortf("calls to addresses further away than %u not implemented", abs(offset));
    } else {
        // Offset fits in immediate, so we just branch and link
        tcg_out_bl(s, offset);
    }
}
// Helper function to emit STP, store pair instructions with offset adressing mode (i.e no changing the base)
static inline void tcg_out_stp(TCGContext *s, int reg1, int reg2, int reg_base, tcg_target_long offset)
{
    tcg_out32(s, 0xa9000000 | (offset << 15) | (reg2 << 10) | (reg_base << 5) | (reg1 << 0));
}

static inline void tcg_out_str_reg_offset(TCGContext *s, int reg, int base_reg, int offset_reg)
{
    tcg_out32(s, 0xf8200800 | (offset_reg << 16) | (base_reg << 5) | (reg << 0));
}

// Helper function to emit LDP, load pair instructions with offset adressing mode (i.e no changing the base)
static inline void tcg_out_ldp(TCGContext *s, int reg1, int reg2, int reg_base, tcg_target_long offset)
{
    tcg_out32(s, 0xa9400000 | (offset << 15) | (reg2 << 10) | (reg_base << 5) | (reg1 << 0));
}

static inline void tcg_out_ld(TCGContext *s, TCGType type, TCGReg arg, TCGReg arg1, tcg_target_long arg2)
{
    tcg_abortf("tcg_out_ld not implemented");
}

static inline void tcg_out_mov(TCGContext *s, TCGType type, TCGReg ret, TCGReg arg)
{
    tcg_out32(s, 0xaa000000 | (arg << 16) | (0b11111 << 5) | (ret << 0));
}

static const int SHIFT_0  = 0b00;
static const int SHIFT_16 = 0b01;
static const int SHIFT_32 = 0b10;
static const int SHIFT_48 = 0b11;
static inline void tcg_out_movi64(TCGContext *s, int reg1, tcg_target_long imm) 
{
    tcg_out32(s, 0xd2800000 | (SHIFT_0 << 21)  | ((0xffff & imm) << 5) | (reg1 << 0)); // MOVZ
    imm = imm >> 16;
    tcg_out32(s, 0xf2800000 | (SHIFT_16 << 21) | ((0xffff & imm) << 5) | (reg1 << 0)); // MOVK
    imm = imm >> 16;
    tcg_out32(s, 0xf2800000 | (SHIFT_32 << 21) | ((0xffff & imm) << 5) | (reg1 << 0)); // MOVK
    imm = imm >> 16;
    tcg_out32(s, 0xf2800000 | (SHIFT_48 << 21) | ((0xffff & imm) << 5) | (reg1 << 0)); // MOVK
}

static inline void tcg_out_st(TCGContext *s, TCGType type, TCGReg arg, TCGReg arg1, tcg_target_long offset)
{
    // Write the content of arg to the address in arg1 + offset
    // For offsets that fit in 9-bits this could be one instruction, future optimization work

    // Move offset into designated tmp reg
    tcg_out_movi64(s, TCG_TMP_REG, offset);
    tcg_out_str_reg_offset(s, arg, arg1, TCG_TMP_REG);
}

static inline void tcg_out_movi(TCGContext *s, TCGType type, TCGReg ret, tcg_target_long arg)
{
    // Since aarch64 can only load 16-bit immediate, 
    // we need to have logic to split it into an MOVZ, and an apropriate number of MOVK instructions

    // For the mvp I just always output 4 instructions, which is really only needed for 64-bit immediates
    tcg_out_movi64(s, ret, arg);
}

// Helper function to emit SUB, with immediate
static inline void tcg_out_subi(TCGContext *s, int reg1, int reg2, tcg_target_long imm)
{
    tcg_out32(s, 0xd1000000 | (imm << 10) | (reg2 << 5) | (reg1 << 0));
}
// Helper function to emit ADD, with immediate
static inline void tcg_out_addi(TCGContext *s, int reg1, int reg2, tcg_target_long imm)
{
    tcg_out32(s, 0x91000000 | (imm << 10) | (reg2 << 5) | (reg1 << 0));
}

static inline void tcg_out_op(TCGContext *s, TCGOpcode opc, const TCGArg *args, const int *const_args)
{
    switch (opc) {
    case INDEX_op_exit_tb:
        tcg_abortf("op_exit_tb not implemented");
        break;
    case INDEX_op_goto_tb:
        tcg_abortf("op_goto_tb not implemented");
        break;
    case INDEX_op_call:
        // Logic taken from the other targets, apparently const_args[0] tells if it is a imm or a reg target
        if (const_args[0]) {
            tcg_out_calli(s, args[0]);
        } else {
            // Register target we can just branch and link directly
            tcg_out_blr(s, args[0]);
        }
        break;
    case INDEX_op_jmp:
        tcg_abortf("op_jmp not implemented");
        break;
    case INDEX_op_br:
        tcg_abortf("op_br not implemented");
        break;
    case INDEX_op_mb:
        tcg_abortf("op_mp not implemented");
        break;
    case INDEX_op_ld8u_i32:
        tcg_abortf("op_ld8u_i32 not implemented");
        break;
    case INDEX_op_qemu_ld8u:
        tcg_abortf("op_ld8u not implemented");
        break;
    case INDEX_op_ld16s_i32:
        tcg_abortf("op_ld16s_i32 not implemented");
        break;
    case INDEX_op_ld_i32:
        tcg_abortf("op_ld_i32 not implemented");
        break;
    case INDEX_op_st8_i32:
        tcg_abortf("op_st8_i32 not implemented");
        break;
    case INDEX_op_st16_i32:
        tcg_abortf("op_st16_i32 not implemented");
        break;
    case INDEX_op_st_i32:
        tcg_abortf("op_st_i32 not implemented");
        break;
    case INDEX_op_mov_i32:
        tcg_abortf("op_mov_i32 not implemented");
        break;
    case INDEX_op_movi_i32:
        tcg_abortf("op_movi_i32 not implemented");
        break;
    case INDEX_op_add_i32:
        tcg_abortf("op_add_i32 not implemented");
        break;
    case INDEX_op_sub_i32:
        tcg_abortf("op_sub_i32 not implemented");
        break;
    case INDEX_op_and_i32:
        tcg_abortf("op_and_i32 not implemented");
        break;
    case INDEX_op_andc_i32:
        tcg_abortf("op_andc_i32 not implemented");
        break;
    case INDEX_op_or_i32:
        tcg_abortf("op_or_i32 not implemented");
        break;
    case INDEX_op_xor_i32:
        tcg_abortf("op_xor_i32 not implemented");
        break;
    case INDEX_op_add2_i32:
        tcg_abortf("op_add2_i32 not implemented");
        break;
    case INDEX_op_sub2_i32:
        tcg_abortf("op_sub2_i32 not implemented");
        break;
    case INDEX_op_neg_i32:
        tcg_abortf("op_neg_i32 not implemented");
        break;
    case INDEX_op_not_i32:
        tcg_abortf("op_not_i32 not implemented");
        break;
    case INDEX_op_mul_i32:
        tcg_abortf("op_mul_i32 not implemented");
        break;
    case INDEX_op_mulu2_i32:
        tcg_abortf("op_mulu2_i32 not implemented");
        break;
    case INDEX_op_muls2_i32:
        tcg_abortf("op_muls2_i32 not implemented");
        break;
    case INDEX_op_shl_i32:
        tcg_abortf("op_shl_i32 not implemented");
        break;
    case INDEX_op_shr_i32:
        tcg_abortf("op_shr_i32 not implemented");
        break;
    case INDEX_op_sar_i32:
        tcg_abortf("op_sar_i32 not implemented");
        break;
    case INDEX_op_rotr_i32:
        tcg_abortf("op_rotr_i32 not implemented");
        break;
    case INDEX_op_rotl_i32:
        tcg_abortf("op_rotl_i32 not implemented");
        break;
    case INDEX_op_brcond_i32:
        tcg_abortf("op_brcond_i32 not implemented");
        break;
    case INDEX_op_brcond2_i32:
        tcg_abortf("op_brcond2_i32 not implemented");
        break;
    case INDEX_op_setcond_i32:
        tcg_abortf("op_setcond_i32 not implemented");
        break;
    case INDEX_op_setcond2_i32:
        tcg_abortf("op_setcond2_i32 not implemented");
        break;
    case INDEX_op_qemu_ld8s:
        tcg_abortf("op_qemu_ld8s not implemented");
        break;
    case INDEX_op_qemu_ld16u:
        tcg_abortf("op_qemu_ld16u not implemented");
        break;
    case INDEX_op_qemu_ld16s:
        tcg_abortf("op_qemu_ld16s not implemented");
        break;
    case INDEX_op_qemu_ld32:
        tcg_abortf("op_qemu_ld32 not implemented");
        break;
    case INDEX_op_qemu_ld64:
        tcg_abortf("op_qemu_ld64 not implemented");
        break;
    case INDEX_op_qemu_st8:
        tcg_abortf("op_qemu_st8 not implemented");
        break;
    case INDEX_op_qemu_st16:
        tcg_abortf("op_qemu_st16 not implemented");
        break;
    case INDEX_op_qemu_st32:
        tcg_abortf("op_qemu_st32 not implemented");
        break;
    case INDEX_op_qemu_st64:
        tcg_abortf("op_qemu_st64 not implemented");
        break;
    case INDEX_op_bswap16_i32:
        tcg_abortf("op_bswap16_i32 not implemented");
        break;
    case INDEX_op_bswap32_i32:
        tcg_abortf("op_bswap32_i32 not implemented");
        break;
    case INDEX_op_ext8s_i32:
        tcg_abortf("op_ext8s_i32 not implemented");
        break;
    case INDEX_op_ext16s_i32:
        tcg_abortf("op_ext16s_i32 not implemented");
        break;
    case INDEX_op_ext16u_i32:
        tcg_abortf("op_ext16u_i32 not implemented");
        break;
    default:
        tcg_abortf("TCGOpcode %u not implemented", opc);
    }
}

static const TCGTargetOpDef arm_op_defs[] = {
    { INDEX_op_call, { "ri" } },

    { INDEX_op_brcond_i32, { "r", "r" } },
    { -1 },
};

static void tcg_target_init(TCGContext *s) 
{
    /* fail safe */ if ((1 << CPU_TLB_ENTRY_BITS) != sizeof_CPUTLBEntry) {
        tcg_abort();
    }

    tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I32], 0, 0xffff);
    //tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I64], 0, 0xffff);
    // Set wich registers can get clobbered by a function call
    tcg_regset_set32(tcg_target_call_clobber_regs, 0,
                    // Parameter and results registers
                     (1 << TCG_REG_R0) | 
                     (1 << TCG_REG_R1) | 
                     (1 << TCG_REG_R2) | 
                     (1 << TCG_REG_R3) | 
                     (1 << TCG_REG_R4) | 
                     (1 << TCG_REG_R5) | 
                     (1 << TCG_REG_R6) | 
                     (1 << TCG_REG_R7) | 
                     // Indirect result registers
                     (1 << TCG_REG_R8) | 
                     // Corruptable registers
                     (1 << TCG_REG_R9) | 
                     (1 << TCG_REG_R10) | 
                     (1 << TCG_REG_R11) | 
                     (1 << TCG_REG_R12) | 
                     (1 << TCG_REG_R13) | 
                     (1 << TCG_REG_R14) | 
                     (1 << TCG_REG_R15) | 
                     // Intra-procedure-call corruptable registers
                     (1 << TCG_REG_R16) |
                     (1 << TCG_REG_R17) |
                     (1 << TCG_REG_R18) |
                     // Frame pointer
                     (1 << TCG_REG_R29) |
                     // Link register
                     (1 << TCG_REG_R30));

    // Setup reserved registers
    tcg_regset_clear(s->reserved_regs);
    tcg_regset_set_reg(s->reserved_regs, TCG_REG_CALL_STACK); // SP
    tcg_regset_set_reg(s->reserved_regs, TCG_TMP_REG); // R28 to use as an intermediate
    tcg_regset_set_reg(s->reserved_regs, TCG_REG_PC); // PC

    tcg_add_target_add_op_defs(arm_op_defs);
    tcg_set_frame(s, TCG_AREG0, /*offsetof(CPUState, temp_buf)*/ temp_buf_offset, CPU_TEMP_BUF_NLONGS * sizeof(long));
}

static void tcg_target_qemu_prologue(TCGContext *s)
{
    // Prologue
    // aarch64 calling conventions needs us to save R19-R28
    // In 64-bit arm we can only save a pair of registers, so here be a lot of instructions
    tcg_out_subi(s, TCG_REG_SP, TCG_REG_SP, 80); // Make space on the stack
    tcg_out_stp(s, TCG_REG_R19, TCG_REG_R20, TCG_REG_SP, 0);
    tcg_out_stp(s, TCG_REG_R21, TCG_REG_R22, TCG_REG_SP, 16);
    tcg_out_stp(s, TCG_REG_R23, TCG_REG_R24, TCG_REG_SP, 32);
    tcg_out_stp(s, TCG_REG_R25, TCG_REG_R26, TCG_REG_SP, 48);
    tcg_out_stp(s, TCG_REG_R27, TCG_REG_R28, TCG_REG_SP, 64);
    tcg_out_stp(s, TCG_REG_R29, TCG_REG_R30, TCG_REG_SP, 80);


    // Branch to code
    tcg_out_mov(s, TCG_TYPE_PTR, TCG_AREG0, tcg_target_call_iarg_regs[0]);
    tcg_out_blr(s, tcg_target_call_iarg_regs[1]);

    // Epilogue
    // Load all the registers we saved above to restore system state
    tcg_out_ldp(s, TCG_REG_R29, TCG_REG_R30, TCG_REG_SP, 80);
    tcg_out_ldp(s, TCG_REG_R27, TCG_REG_R28, TCG_REG_SP, 64);
    tcg_out_ldp(s, TCG_REG_R25, TCG_REG_R26, TCG_REG_SP, 48);
    tcg_out_ldp(s, TCG_REG_R23, TCG_REG_R24, TCG_REG_SP, 32);
    tcg_out_ldp(s, TCG_REG_R21, TCG_REG_R22, TCG_REG_SP, 16);
    tcg_out_ldp(s, TCG_REG_R19, TCG_REG_R20, TCG_REG_SP, 0);
    tcg_out_addi(s, TCG_REG_SP, TCG_REG_SP, 80); // Pop the stack
}