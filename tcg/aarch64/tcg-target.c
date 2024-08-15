#include <elf.h>

#include "tcg-target.h" //Not actually needed here, but keeps the editor happy
#include "additional.h"

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

// Taken from the arm32 target
enum arm_cond_code_e {
    COND_EQ = 0x0,
    COND_NE = 0x1,
    COND_CS = 0x2,      /* Unsigned greater or equal */
    COND_CC = 0x3,      /* Unsigned less than */
    COND_MI = 0x4,      /* Negative */
    COND_PL = 0x5,      /* Zero or greater */
    COND_VS = 0x6,      /* Overflow */
    COND_VC = 0x7,      /* No overflow */
    COND_HI = 0x8,      /* Unsigned greater than */
    COND_LS = 0x9,      /* Unsigned less or equal */
    COND_GE = 0xa,
    COND_LT = 0xb,
    COND_GT = 0xc,
    COND_LE = 0xd,
    COND_AL = 0xe,
};
static const uint8_t tcg_cond_to_arm_cond[] = {
    [TCG_COND_EQ] = COND_EQ,
    [TCG_COND_NE] = COND_NE,
    [TCG_COND_LT] = COND_LT,
    [TCG_COND_GE] = COND_GE,
    [TCG_COND_LE] = COND_LE,
    [TCG_COND_GT] = COND_GT,
    /* unsigned */
    [TCG_COND_LTU] = COND_CC,
    [TCG_COND_GEU] = COND_CS,
    [TCG_COND_LEU] = COND_LS,
    [TCG_COND_GTU] = COND_HI,
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
    // Don't really understand this function, so just return true for now
    return 1;
}


static void reloc_pc32(void *code_ptr, tcg_target_long target)
{
    // code_ptr should be set to target - current PC
    // Adapted from arm32 target
    uint32_t offset = target - ((tcg_target_long)code_ptr + 8);
    *(uint32_t *)code_ptr = ((*(uint32_t *)code_ptr)) | (offset);
}
static void patch_reloc(uint8_t *code_ptr, int type, tcg_target_long value, tcg_target_long addend)
{
    switch (type) {
        case R_AARCH64_PREL32: 
            /* PC-relative 32-bit.	*/
            reloc_pc32(code_ptr, value);
            break;
        default:
            tcg_abortf("patch reloc for type %i not implemented", type);
            break;
    }
}

static inline void tcg_out_br(TCGContext *s, int addr_reg)
{
    tcg_out32(s,  0xd61f0000 | (addr_reg << 5));
}
static inline void tcg_out_bl(TCGContext *s, int offset)
{
    tcg_out32(s, 0x94000000 | (offset << 0));
}
static inline void tcg_out_blr(TCGContext *s, int reg)
{
    tcg_out32(s, 0xd63f0000 | (reg << 5));
}

static inline void tcg_out_goto_label(TCGContext *s, int label_index)
{
    TCGLabel *l = &s->labels[label_index];

    if (l->has_value) {
        // Label has a target address so we just branch to it
        tcg_out_movi(s, TCG_TYPE_PTR, TCG_TMP_REG, l->u.value);
        tcg_out_br(s, TCG_TMP_REG);
    } else {
        // Label does not have the address so we need a reloc
        // This is mostly just taken from the arm32 target, I do not fully understand it
        // The reloc type is just the one i found in elf.h that seemed most appropriate
        tcg_out_reloc(s, s->code_ptr, R_AARCH64_PREL32, label_index, 31337); // IDK what this constant it
        tcg_out8(s, 0b00010100); // branch instruction and 2 bits of padding
    }
}

// Heper to generate function calls to constant address
static inline void tcg_out_calli(TCGContext *s, tcg_target_ulong addr) 
{
    // TODO: This needs to handle the arguments I think?
    // Offset is only 26-bits, so we can't jump further than that without storing it in a reg first
    int offset = addr - (tcg_target_long)s->code_ptr;
    if (abs(offset) > 0xfffff) {
        // Jump is too long, store it in a register first and then branch and link
        tcg_out_movi(s, 0, TCG_TMP_REG, offset);
        tcg_out_blr(s, TCG_TMP_REG);
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

// Helper function to emit LDP, load pair instructions with offset adressing mode (i.e no changing the base)
static inline void tcg_out_ldp(TCGContext *s, int reg1, int reg2, int reg_base, tcg_target_long offset)
{
    tcg_out32(s, 0xa9400000 | (offset << 15) | (reg2 << 10) | (reg_base << 5) | (reg1 << 0));
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

// Fills reg_dest with nbit of data from reg_base + offset_reg
static inline void tcg_out_ld_reg_offset(TCGContext *s, int bits, int reg_dest, int reg_base, int offset_reg)
{
    switch (bits)
    {
    case 32:
        tcg_out32(s, 0x78600800 | (offset_reg << 16) | (reg_base << 5) | (reg_dest << 0));
        break;
    case 64:
        tcg_out32(s, 0xf8600800 | (offset_reg << 16) | (reg_base << 5) | (reg_dest << 0));
        break;
    default:
        tcg_abortf("ld %s bits wide not implemented", bits);
        break;
    }
}

static inline void tcg_out_ld_offset(TCGContext *s, int bits, int reg_dest, int reg_base, tcg_target_long offset)
{
    tcg_out_movi64(s, TCG_TMP_REG, offset);
    tcg_out_ld_reg_offset(s, bits, reg_dest, reg_base, TCG_TMP_REG);
}

// Read 64-bits from base + offset to dest
static inline void tcg_out_ld(TCGContext *s, TCGType type, TCGReg dest, TCGReg base, tcg_target_long offset)
{
    tcg_out_movi64(s, TCG_TMP_REG, offset);
    tcg_out_ld_reg_offset(s, 64, dest, base, TCG_TMP_REG);
}

// Stores n-bits of data from reg_src to reg_base + offset_reg
static inline void tcg_out_st_reg_offset(TCGContext *s, int bits, int reg_src, int reg_base, int offset_reg)
{
    switch (bits)
    {
    case 32:
        tcg_out32(s, 0x78200800 | (offset_reg << 16) | (reg_base << 5) | (reg_src << 0));
        break;
    case 64:
        tcg_out32(s, 0xf8200800 | (offset_reg << 16) | (reg_base << 5) | (reg_src << 0));
        break;
    default:
        tcg_abortf("st %s bits wide not implemented", bits);
        break;
    }
}
static inline void tcg_out_st_offset(TCGContext *s, int bits, int reg_src, int reg_base, tcg_target_long offset)
{
    tcg_out_movi64(s, TCG_TMP_REG, offset);
    tcg_out_st_reg_offset(s, bits, reg_src, reg_base, TCG_TMP_REG);
}

static inline void tcg_out_st(TCGContext *s, TCGType type, TCGReg arg, TCGReg arg1, tcg_target_long offset)
{
    // Write the content of arg to the address in arg1 + offset
    // For offsets that fit in 9-bits this could be one instruction, future optimization work

    // Move offset into designated tmp reg
    tcg_out_movi64(s, TCG_TMP_REG, offset);
    tcg_out_st_reg_offset(s, 64, arg, arg1, TCG_TMP_REG);
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

static inline void tcg_out_add_reg(TCGContext *s, int reg_dest, int reg1, int reg2) 
{
    tcg_out32(s, 0x8b200000 | (reg2 << 16) | (reg1 << 5) | (reg_dest << 0));
}
static inline void tcg_out_add_imm(TCGContext *s, int reg_dest, int reg_in, tcg_target_long imm) 
{
    tcg_out_movi64(s, TCG_TMP_REG, imm);
    tcg_out_add_reg(s, reg_dest, reg_in, TCG_TMP_REG);
}
static inline void tcg_out_cmp(TCGContext *s, int reg1, int reg2)
{
    // CMP is actually just subs with the zero register as the destination
    tcg_out32(s, 0xeb200000 | (reg2 << 16) | (reg1 << 5) | (TCG_REG_RZR << 0 ));
}
static inline void tcg_out_cmpi(TCGContext *s, int reg, tcg_target_long imm)
{
    // Mov immediate into a register
    tcg_out_movi(s, 0, TCG_TMP_REG, imm);
    tcg_out_cmp(s, reg, TCG_TMP_REG);
}
// Helper for qemu's conditional branch. branch if arg1 COND arg2 == true
static inline void tcg_out_br_cond(TCGContext *s, int arg1, int cond, int arg2, int addr)
{
    tcg_out_cmp(s, arg1, arg2);
    tcg_out32(s, 0x54000000 | (addr << 5) | (tcg_cond_to_arm_cond[cond] << 0));
}
// Version of above but with an immediate as the second argument
static inline void tcg_out_br_condi(TCGContext *s, int arg1, int cond, int imm, int addr)
{
    tcg_out_cmpi(s, arg1, imm);
    tcg_out32(s, 0x54000000 | (addr << 5) | (tcg_cond_to_arm_cond[cond] << 0));
}

static uint8_t *tb_ret_addr;

static inline void tcg_out_op(TCGContext *s, TCGOpcode opc, const TCGArg *args, const int *const_args)
{
    switch (opc) {
    case INDEX_op_exit_tb:
        tcg_out_movi(s, TCG_TYPE_PTR, TCG_REG_R0, args[0]); // Put return value in output register
        tcg_out_movi(s, TCG_TYPE_PTR, TCG_TMP_REG, (tcg_target_ulong)tb_ret_addr);
        tcg_out_br(s, TCG_TMP_REG);
        break;
    case INDEX_op_goto_tb:
        if (s->tb_jmp_offset) {
            // Direct jump
            // I do not fully understand how this works, so basicly just copying arm32 target
            // I think the lower 24-bits contain the jmp target, so we just write the start
            // of of the branch immediate instruction and the imm will already be filled in
            s->tb_jmp_offset[args[0]] = s->code_ptr - s->code_buf;
            tcg_out8(s, 0b00010100); // branch instruction and 2 bits of padding
        } else {
            // Indirect jump
            tcg_abortf("op_goto_tb indirect jump not implemented");
        }
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
        tcg_out_goto_label(s, args[0]);
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
        if (const_args[1]) {
            // Second arg is an immediate
            tcg_out_br_condi(s, args[0], args[2], args[1], args[3]);
        } else {
            // Second arg is a register
            tcg_out_br_cond(s, args[0], args[2], args[1], args[3]);
        }
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
    case INDEX_op_ld32u_i64:
        tcg_out_ld_offset(s, 32, args[0], args[1], args[2]);
        break;
    case INDEX_op_ld_i64:
        tcg_out_ld_offset(s, 32, args[0], args[1], args[2]);
        break;
    case INDEX_op_st32_i64:
        tcg_out_st_offset(s, 32, args[0], args[1], args[2]);
        break;
    case INDEX_op_st_i64:
        tcg_out_st_offset(s, 32, args[0], args[1], args[2]);
        break;
    case INDEX_op_add_i64:
        if (const_args[2]) {
            // Add with immediate
            tcg_out_add_imm(s, args[0], args[1], args[2]);
        } else {
            tcg_out_add_reg(s, args[0], args[1], args[2]);
            // Add with registers
        }
        break;
    case INDEX_op_sub_i64:
        tcg_abortf("op_sub_i64 not implemented");
        break;
    default:
        tcg_abortf("TCGOpcode %u not implemented", opc);
    }
}

static const TCGTargetOpDef arm_op_defs[] = {
    { INDEX_op_call, { "ri" } },

    { INDEX_op_brcond_i32, { "r", "r" } },
    { INDEX_op_ld32u_i64,  { "r", "r" } },
    { INDEX_op_ld_i64,     { "r", "r" }},
    { INDEX_op_st32_i64,   { "r", "r" } },
    { INDEX_op_st_i64,     { "r", "r" }},
    { INDEX_op_add_i64,    { "r", "r" , "r"} },
    { INDEX_op_sub_i64,    { "r", "r" , "r"} },
    { -1 },
};

static void tcg_target_init(TCGContext *s) 
{
    /* fail safe */ if ((1 << CPU_TLB_ENTRY_BITS) != sizeof_CPUTLBEntry) {
        tcg_abort();
    }

    tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I32], 0, 0xffff);
    tcg_regset_set32(tcg_target_available_regs[TCG_TYPE_I64], 0, 0xffff);
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
    tb_ret_addr = s->code_ptr;

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