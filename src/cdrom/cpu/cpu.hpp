#ifndef __PSXACT_CDROM_CPU_HPP__
#define __PSXACT_CDROM_CPU_HPP__

#include <cstdint>

namespace cdrom {
  namespace cpu {
    struct state_t {
      uint8_t a;
      uint8_t x;
      uint16_t pc;
      uint16_t sp;

      struct {
        bool h;
        bool i;
        bool n;
        bool z;
        bool c;
      } ccr;

      bool stop;
      bool wait;
    };

    void tick(state_t *state);

    // addressing modes

    uint16_t am_dir(state_t *state);
    uint16_t am_ext(state_t *state);
    uint16_t am_imm(state_t *state);
    uint16_t am_ix2(state_t *state);
    uint16_t am_ix1(state_t *state);
    uint16_t am_ix (state_t *state);

    // instructions

    void op_asra(state_t *state);
    void op_clra(state_t *state);
    void op_coma(state_t *state);
    void op_deca(state_t *state);
    void op_inca(state_t *state);
    void op_lsla(state_t *state);
    void op_lsra(state_t *state);
    void op_nega(state_t *state);
    void op_rola(state_t *state);
    void op_rora(state_t *state);
    void op_tsta(state_t *state);

    void op_asrx(state_t *state);
    void op_clrx(state_t *state);
    void op_comx(state_t *state);
    void op_decx(state_t *state);
    void op_incx(state_t *state);
    void op_lslx(state_t *state);
    void op_lsrx(state_t *state);
    void op_negx(state_t *state);
    void op_rolx(state_t *state);
    void op_rorx(state_t *state);
    void op_tstx(state_t *state);

    void op_brset0(state_t *state);
    void op_brclr0(state_t *state);
    void op_brset1(state_t *state);
    void op_brclr1(state_t *state);
    void op_brset2(state_t *state);
    void op_brclr2(state_t *state);
    void op_brset3(state_t *state);
    void op_brclr3(state_t *state);
    void op_brset4(state_t *state);
    void op_brclr4(state_t *state);
    void op_brset5(state_t *state);
    void op_brclr5(state_t *state);
    void op_brset6(state_t *state);
    void op_brclr6(state_t *state);
    void op_brset7(state_t *state);
    void op_brclr7(state_t *state);

    void op_bset0(state_t *state);
    void op_bclr0(state_t *state);
    void op_bset1(state_t *state);
    void op_bclr1(state_t *state);
    void op_bset2(state_t *state);
    void op_bclr2(state_t *state);
    void op_bset3(state_t *state);
    void op_bclr3(state_t *state);
    void op_bset4(state_t *state);
    void op_bclr4(state_t *state);
    void op_bset5(state_t *state);
    void op_bclr5(state_t *state);
    void op_bset6(state_t *state);
    void op_bclr6(state_t *state);
    void op_bset7(state_t *state);
    void op_bclr7(state_t *state);

    void op_bra(state_t *state);
    void op_brn(state_t *state);
    void op_bhi(state_t *state);
    void op_bls(state_t *state);
    void op_bcc(state_t *state);
    void op_bcs(state_t *state);
    void op_bne(state_t *state);
    void op_beq(state_t *state);
    void op_bhcc(state_t *state);
    void op_bhcs(state_t *state);
    void op_bpl(state_t *state);
    void op_bmi(state_t *state);
    void op_bmc(state_t *state);
    void op_bms(state_t *state);
    void op_bil(state_t *state);
    void op_bih(state_t *state);

    void op_asr(state_t *state, uint16_t address);
    void op_clr(state_t *state, uint16_t address);
    void op_com(state_t *state, uint16_t address);
    void op_dec(state_t *state, uint16_t address);
    void op_inc(state_t *state, uint16_t address);
    void op_lsl(state_t *state, uint16_t address);
    void op_lsr(state_t *state, uint16_t address);
    void op_neg(state_t *state, uint16_t address);
    void op_rol(state_t *state, uint16_t address);
    void op_ror(state_t *state, uint16_t address);
    void op_tst(state_t *state, uint16_t address);

    void op_sub(state_t *state, uint16_t address);
    void op_cmp(state_t *state, uint16_t address);
    void op_sbc(state_t *state, uint16_t address);
    void op_cpx(state_t *state, uint16_t address);
    void op_and(state_t *state, uint16_t address);
    void op_bit(state_t *state, uint16_t address);
    void op_lda(state_t *state, uint16_t address);
    void op_sta(state_t *state, uint16_t address);
    void op_eor(state_t *state, uint16_t address);
    void op_adc(state_t *state, uint16_t address);
    void op_ora(state_t *state, uint16_t address);
    void op_add(state_t *state, uint16_t address);
    void op_jmp(state_t *state, uint16_t address);
    void op_jsr(state_t *state, uint16_t address);
    void op_ldx(state_t *state, uint16_t address);
    void op_stx(state_t *state, uint16_t address);

    void op_clc(state_t *state);
    void op_cli(state_t *state);
    void op_mul(state_t *state);
    void op_nop(state_t *state);
    void op_rsp(state_t *state);
    void op_rti(state_t *state);
    void op_rts(state_t *state);
    void op_sec(state_t *state);
    void op_sei(state_t *state);
    void op_stop(state_t *state);
    void op_swi(state_t *state);
    void op_tax(state_t *state);
    void op_txa(state_t *state);
    void op_wait(state_t *state);
  }
}

#endif // __PSXACT_CPU_HPP__
