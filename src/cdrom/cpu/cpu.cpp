#include "cpu.hpp"

static uint8_t read_byte(uint16_t address) {
  return 0;
}

static uint8_t read_pc(cdrom::cpu::state_t *state) {
  uint8_t data = read_byte(state->pc);
  state->pc++;

  return data;
}

static void write_byte(uint16_t address, uint8_t data) { }

static void branch(cdrom::cpu::state_t *state, bool condition) {
  uint16_t target = cdrom::cpu::am_rel(state);

  if (condition) {
    state->pc = target;
  }
}

void cdrom::cpu::tick(state_t *state) {
  auto code = read_byte(state->pc);
  state->pc++;

  switch (code) {
    case 0x00: op_brset(state, 0x01, am_dir(state)); break; // brset0 (btb)
    case 0x01: op_brclr(state, 0x01, am_dir(state)); break; // brclr0 (btb)
    case 0x02: op_brset(state, 0x02, am_dir(state)); break; // brset1 (btb)
    case 0x03: op_brclr(state, 0x02, am_dir(state)); break; // brclr1 (btb)
    case 0x04: op_brset(state, 0x04, am_dir(state)); break; // brset2 (btb)
    case 0x05: op_brclr(state, 0x04, am_dir(state)); break; // brclr2 (btb)
    case 0x06: op_brset(state, 0x08, am_dir(state)); break; // brset3 (btb)
    case 0x07: op_brclr(state, 0x08, am_dir(state)); break; // brclr3 (btb)
    case 0x08: op_brset(state, 0x10, am_dir(state)); break; // brset4 (btb)
    case 0x09: op_brclr(state, 0x10, am_dir(state)); break; // brclr4 (btb)
    case 0x0a: op_brset(state, 0x20, am_dir(state)); break; // brset5 (btb)
    case 0x0b: op_brclr(state, 0x20, am_dir(state)); break; // brclr5 (btb)
    case 0x0c: op_brset(state, 0x40, am_dir(state)); break; // brset6 (btb)
    case 0x0d: op_brclr(state, 0x40, am_dir(state)); break; // brclr6 (btb)
    case 0x0e: op_brset(state, 0x80, am_dir(state)); break; // brset7 (btb)
    case 0x0f: op_brclr(state, 0x80, am_dir(state)); break; // brclr7 (btb)

    case 0x10: op_bset(state, 0x01, am_dir(state)); break; // bset0 (bsc)
    case 0x11: op_bclr(state, 0x01, am_dir(state)); break; // bclr0 (bsc)
    case 0x12: op_bset(state, 0x02, am_dir(state)); break; // bset1 (bsc)
    case 0x13: op_bclr(state, 0x02, am_dir(state)); break; // bclr1 (bsc)
    case 0x14: op_bset(state, 0x04, am_dir(state)); break; // bset2 (bsc)
    case 0x15: op_bclr(state, 0x04, am_dir(state)); break; // bclr2 (bsc)
    case 0x16: op_bset(state, 0x08, am_dir(state)); break; // bset3 (bsc)
    case 0x17: op_bclr(state, 0x08, am_dir(state)); break; // bclr3 (bsc)
    case 0x18: op_bset(state, 0x10, am_dir(state)); break; // bset4 (bsc)
    case 0x19: op_bclr(state, 0x10, am_dir(state)); break; // bclr4 (bsc)
    case 0x1a: op_bset(state, 0x20, am_dir(state)); break; // bset5 (bsc)
    case 0x1b: op_bclr(state, 0x20, am_dir(state)); break; // bclr5 (bsc)
    case 0x1c: op_bset(state, 0x40, am_dir(state)); break; // bset6 (bsc)
    case 0x1d: op_bclr(state, 0x40, am_dir(state)); break; // bclr6 (bsc)
    case 0x1e: op_bset(state, 0x80, am_dir(state)); break; // bset7 (bsc)
    case 0x1f: op_bclr(state, 0x80, am_dir(state)); break; // bclr7 (bsc)

    case 0x20: op_bra (state); break; // bra  (rel)
    case 0x21: op_brn (state); break; // brn  (rel)
    case 0x22: op_bhi (state); break; // bhi  (rel)
    case 0x23: op_bls (state); break; // bls  (rel)
    case 0x24: op_bcc (state); break; // bcc  (rel)
    case 0x25: op_bcs (state); break; // bcs  (rel)
    case 0x26: op_bne (state); break; // bne  (rel)
    case 0x27: op_beq (state); break; // beq  (rel)
    case 0x28: op_bhcc(state); break; // bhcc (rel)
    case 0x29: op_bhcs(state); break; // bhcs (rel)
    case 0x2a: op_bpl (state); break; // bpl  (rel)
    case 0x2b: op_bmi (state); break; // bmi  (rel)
    case 0x2c: op_bmc (state); break; // bmc  (rel)
    case 0x2d: op_bms (state); break; // bms  (rel)
    case 0x2e: op_bil (state); break; // bil  (rel)
    case 0x2f: op_bih (state); break; // bih  (rel)

    case 0x30: op_neg(state, am_dir(state)); break; // neg (dir)
    case 0x31: break; //
    case 0x32: break; //
    case 0x33: op_com(state, am_dir(state)); break; // com (dir)
    case 0x34: op_lsr(state, am_dir(state)); break; // lsr (dir)
    case 0x35: break; //
    case 0x36: op_ror(state, am_dir(state)); break; // ror (dir)
    case 0x37: op_asr(state, am_dir(state)); break; // asr (dir)
    case 0x38: op_lsl(state, am_dir(state)); break; // lsl (dir)
    case 0x39: op_rol(state, am_dir(state)); break; // rol (dir)
    case 0x3a: op_dec(state, am_dir(state)); break; // dec (dir)
    case 0x3b: break; //
    case 0x3c: op_inc(state, am_dir(state)); break; // inc (dir)
    case 0x3d: op_tst(state, am_dir(state)); break; // tst (dir)
    case 0x3e: break; //
    case 0x3f: op_clr(state, am_dir(state)); break; // clr (dir)

    case 0x40: op_nega(state); break; // nega (inh)
    case 0x41: break; //
    case 0x42: op_mul (state); break; // mul  (inh)
    case 0x43: op_coma(state); break; // coma (inh)
    case 0x44: op_lsra(state); break; // lsra (inh)
    case 0x45: break; //
    case 0x46: op_rora(state); break; // rora (inh)
    case 0x47: op_asra(state); break; // asra (inh)
    case 0x48: op_lsla(state); break; // lsla (inh)
    case 0x49: op_rola(state); break; // rola (inh)
    case 0x4a: op_deca(state); break; // deca (inh)
    case 0x4b: break; //
    case 0x4c: op_inca(state); break; // inca (inh)
    case 0x4d: op_tsta(state); break; // tsta (inh)
    case 0x4e: break; //
    case 0x4f: op_clra(state); break; // clra (inh)

    case 0x50: op_negx(state); break; // negx (inh)
    case 0x51: break; //
    case 0x52: break; //
    case 0x53: op_comx(state); break; // comx (inh)
    case 0x54: op_lsrx(state); break; // lsrx (inh)
    case 0x55: break; //
    case 0x56: op_rorx(state); break; // rorx (inh)
    case 0x57: op_asrx(state); break; // asrx (inh)
    case 0x58: op_lslx(state); break; // lslx (inh)
    case 0x59: op_rolx(state); break; // rolx (inh)
    case 0x5a: op_decx(state); break; // decx (inh)
    case 0x5b: break; //
    case 0x5c: op_incx(state); break; // incx (inh)
    case 0x5d: op_tstx(state); break; // tstx (inh)
    case 0x5e: break; //
    case 0x5f: op_clrx(state); break; // clrx (inh)

    case 0x60: op_neg(state, am_ix1(state)); break; // neg (ix1)
    case 0x61: break; //
    case 0x62: break; //
    case 0x63: op_com(state, am_ix1(state)); break; // com (ix1)
    case 0x64: op_lsr(state, am_ix1(state)); break; // lsr (ix1)
    case 0x65: break; //
    case 0x66: op_ror(state, am_ix1(state)); break; // ror (ix1)
    case 0x67: op_asr(state, am_ix1(state)); break; // asr (ix1)
    case 0x68: op_lsl(state, am_ix1(state)); break; // lsl (ix1)
    case 0x69: op_rol(state, am_ix1(state)); break; // rol (ix1)
    case 0x6a: op_dec(state, am_ix1(state)); break; // dec (ix1)
    case 0x6b: break; //
    case 0x6c: op_inc(state, am_ix1(state)); break; // inc (ix1)
    case 0x6d: op_tst(state, am_ix1(state)); break; // tst (ix1)
    case 0x6e: break; //
    case 0x6f: op_clr(state, am_ix1(state)); break; // clr (ix1)

    case 0x70: op_neg(state, am_ix(state)); break; // neg (ix)
    case 0x71: break; //
    case 0x72: break; //
    case 0x73: op_com(state, am_ix(state)); break; // com (ix)
    case 0x74: op_lsr(state, am_ix(state)); break; // lsr (ix)
    case 0x75: break; //
    case 0x76: op_ror(state, am_ix(state)); break; // ror (ix)
    case 0x77: op_asr(state, am_ix(state)); break; // asr (ix)
    case 0x78: op_lsl(state, am_ix(state)); break; // lsl (ix)
    case 0x79: op_rol(state, am_ix(state)); break; // rol (ix)
    case 0x7a: op_dec(state, am_ix(state)); break; // dec (ix)
    case 0x7b: break; //
    case 0x7c: op_inc(state, am_ix(state)); break; // inc (ix)
    case 0x7d: op_tst(state, am_ix(state)); break; // tst (ix)
    case 0x7e: break; //
    case 0x7f: op_clr(state, am_ix(state)); break; // clr (ix)

    case 0x80: op_rti(state); break; // rti (inh)
    case 0x81: op_rts(state); break; // rts (inh)
    case 0x82: break; //
    case 0x83: op_swi(state); break; // swi (inh)
    case 0x84: break; //
    case 0x85: break; //
    case 0x86: break; //
    case 0x87: break; //
    case 0x88: break; //
    case 0x89: break; //
    case 0x8a: break; //
    case 0x8b: break; //
    case 0x8c: break; //
    case 0x8d: break; //
    case 0x8e: op_stop(state); break; // stop (inh)
    case 0x8f: op_wait(state); break; // wait (inh)

    case 0x90: break; //
    case 0x91: break; //
    case 0x92: break; //
    case 0x93: break; //
    case 0x94: break; //
    case 0x95: break; //
    case 0x96: break; //
    case 0x97: op_tax(state); break; // tax (inh)
    case 0x98: op_clc(state); break; // clc (inh)
    case 0x99: op_sec(state); break; // sec (inh)
    case 0x9a: op_cli(state); break; // cli (inh)
    case 0x9b: op_sei(state); break; // sei (inh)
    case 0x9c: op_rsp(state); break; // rsp (inh)
    case 0x9d: op_nop(state); break; // nop (inh)
    case 0x9e: break; //
    case 0x9f: op_txa(state); break; // txa (inh)

    case 0xa0: op_sub(state, am_imm(state)); break; // sub (imm)
    case 0xa1: op_cmp(state, am_imm(state)); break; // cmp (imm)
    case 0xa2: op_sbc(state, am_imm(state)); break; // sbc (imm)
    case 0xa3: op_cpx(state, am_imm(state)); break; // cpx (imm)
    case 0xa4: op_and(state, am_imm(state)); break; // and (imm)
    case 0xa5: op_bit(state, am_imm(state)); break; // bit (imm)
    case 0xa6: op_lda(state, am_imm(state)); break; // lda (imm)
    case 0xa7: break; //
    case 0xa8: op_eor(state, am_imm(state)); break; // eor (imm)
    case 0xa9: op_adc(state, am_imm(state)); break; // adc (imm)
    case 0xaa: op_ora(state, am_imm(state)); break; // ora (imm)
    case 0xab: op_add(state, am_imm(state)); break; // add (imm)
    case 0xac: break; //
    case 0xad: op_jsr(state, am_imm(state)); break; // bsr (imm)
    case 0xae: op_ldx(state, am_imm(state)); break; // ldx (imm)
    case 0xaf: break; //

    case 0xb0: op_sub(state, am_dir(state)); break; // sub (dir)
    case 0xb1: op_cmp(state, am_dir(state)); break; // cmp (dir)
    case 0xb2: op_sbc(state, am_dir(state)); break; // sbc (dir)
    case 0xb3: op_cpx(state, am_dir(state)); break; // cpx (dir)
    case 0xb4: op_and(state, am_dir(state)); break; // and (dir)
    case 0xb5: op_bit(state, am_dir(state)); break; // bit (dir)
    case 0xb6: op_lda(state, am_dir(state)); break; // lda (dir)
    case 0xb7: op_sta(state, am_dir(state)); break; // sta (dir)
    case 0xb8: op_eor(state, am_dir(state)); break; // eor (dir)
    case 0xb9: op_adc(state, am_dir(state)); break; // adc (dir)
    case 0xba: op_ora(state, am_dir(state)); break; // ora (dir)
    case 0xbb: op_add(state, am_dir(state)); break; // add (dir)
    case 0xbc: op_jmp(state, am_dir(state)); break; // jmp (dir)
    case 0xbd: op_jsr(state, am_dir(state)); break; // jsr (dir)
    case 0xbe: op_ldx(state, am_dir(state)); break; // ldx (dir)
    case 0xbf: op_stx(state, am_dir(state)); break; // stx (dir)

    case 0xc0: op_sub(state, am_ext(state)); break; // sub (ext)
    case 0xc1: op_cmp(state, am_ext(state)); break; // cmp (ext)
    case 0xc2: op_sbc(state, am_ext(state)); break; // sbc (ext)
    case 0xc3: op_cpx(state, am_ext(state)); break; // cpx (ext)
    case 0xc4: op_and(state, am_ext(state)); break; // and (ext)
    case 0xc5: op_bit(state, am_ext(state)); break; // bit (ext)
    case 0xc6: op_lda(state, am_ext(state)); break; // lda (ext)
    case 0xc7: op_sta(state, am_ext(state)); break; // sta (ext)
    case 0xc8: op_eor(state, am_ext(state)); break; // eor (ext)
    case 0xc9: op_adc(state, am_ext(state)); break; // adc (ext)
    case 0xca: op_ora(state, am_ext(state)); break; // ora (ext)
    case 0xcb: op_add(state, am_ext(state)); break; // add (ext)
    case 0xcc: op_jmp(state, am_ext(state)); break; // jmp (ext)
    case 0xcd: op_jsr(state, am_ext(state)); break; // jsr (ext)
    case 0xce: op_ldx(state, am_ext(state)); break; // ldx (ext)
    case 0xcf: op_stx(state, am_ext(state)); break; // stx (ext)

    case 0xd0: op_sub(state, am_ix2(state)); break; // sub (ix2)
    case 0xd1: op_cmp(state, am_ix2(state)); break; // cmp (ix2)
    case 0xd2: op_sbc(state, am_ix2(state)); break; // sbc (ix2)
    case 0xd3: op_cpx(state, am_ix2(state)); break; // cpx (ix2)
    case 0xd4: op_and(state, am_ix2(state)); break; // and (ix2)
    case 0xd5: op_bit(state, am_ix2(state)); break; // bit (ix2)
    case 0xd6: op_lda(state, am_ix2(state)); break; // lda (ix2)
    case 0xd7: op_sta(state, am_ix2(state)); break; // sta (ix2)
    case 0xd8: op_eor(state, am_ix2(state)); break; // eor (ix2)
    case 0xd9: op_adc(state, am_ix2(state)); break; // adc (ix2)
    case 0xda: op_ora(state, am_ix2(state)); break; // ora (ix2)
    case 0xdb: op_add(state, am_ix2(state)); break; // add (ix2)
    case 0xdc: op_jmp(state, am_ix2(state)); break; // jmp (ix2)
    case 0xdd: op_jsr(state, am_ix2(state)); break; // jsr (ix2)
    case 0xde: op_ldx(state, am_ix2(state)); break; // ldx (ix2)
    case 0xdf: op_stx(state, am_ix2(state)); break; // stx (ix2)

    case 0xe0: op_sub(state, am_ix1(state)); break; // sub (ix1)
    case 0xe1: op_cmp(state, am_ix1(state)); break; // cmp (ix1)
    case 0xe2: op_sbc(state, am_ix1(state)); break; // sbc (ix1)
    case 0xe3: op_cpx(state, am_ix1(state)); break; // cpx (ix1)
    case 0xe4: op_and(state, am_ix1(state)); break; // and (ix1)
    case 0xe5: op_bit(state, am_ix1(state)); break; // bit (ix1)
    case 0xe6: op_lda(state, am_ix1(state)); break; // lda (ix1)
    case 0xe7: op_sta(state, am_ix1(state)); break; // sta (ix1)
    case 0xe8: op_eor(state, am_ix1(state)); break; // eor (ix1)
    case 0xe9: op_adc(state, am_ix1(state)); break; // adc (ix1)
    case 0xea: op_ora(state, am_ix1(state)); break; // ora (ix1)
    case 0xeb: op_add(state, am_ix1(state)); break; // add (ix1)
    case 0xec: op_jmp(state, am_ix1(state)); break; // jmp (ix1)
    case 0xed: op_jsr(state, am_ix1(state)); break; // jsr (ix1)
    case 0xee: op_ldx(state, am_ix1(state)); break; // ldx (ix1)
    case 0xef: op_stx(state, am_ix1(state)); break; // stx (ix1)

    case 0xf0: op_sub(state, am_ix(state)); break; // sub (ix)
    case 0xf1: op_cmp(state, am_ix(state)); break; // cmp (ix)
    case 0xf2: op_sbc(state, am_ix(state)); break; // sbc (ix)
    case 0xf3: op_cpx(state, am_ix(state)); break; // cpx (ix)
    case 0xf4: op_and(state, am_ix(state)); break; // and (ix)
    case 0xf5: op_bit(state, am_ix(state)); break; // bit (ix)
    case 0xf6: op_lda(state, am_ix(state)); break; // lda (ix)
    case 0xf7: op_sta(state, am_ix(state)); break; // sta (ix)
    case 0xf8: op_eor(state, am_ix(state)); break; // eor (ix)
    case 0xf9: op_adc(state, am_ix(state)); break; // adc (ix)
    case 0xfa: op_ora(state, am_ix(state)); break; // ora (ix)
    case 0xfb: op_add(state, am_ix(state)); break; // add (ix)
    case 0xfc: op_jmp(state, am_ix(state)); break; // jmp (ix)
    case 0xfd: op_jsr(state, am_ix(state)); break; // jsr (ix)
    case 0xfe: op_ldx(state, am_ix(state)); break; // ldx (ix)
    case 0xff: op_stx(state, am_ix(state)); break; // stx (ix)
  }
}

uint8_t cdrom::cpu::pack_flags(cdrom::cpu::state_t *state){
  return uint8_t(
    (1 << 7) |
    (1 << 6) |
    (1 << 5) |
    (state->ccr.h << 4) |
    (state->ccr.i << 3) |
    (state->ccr.n << 2) |
    (state->ccr.z << 1) |
    (state->ccr.c << 0)
  );
}

void ::cdrom::cpu::unpack_flags(cdrom::cpu::state_t *state, uint8_t data) {
  state->ccr.h = (data >> 4) & 1;
  state->ccr.i = (data >> 3) & 1;
  state->ccr.n = (data >> 2) & 1;
  state->ccr.z = (data >> 1) & 1;
  state->ccr.c = (data >> 0) & 1;
}

uint16_t cdrom::cpu::am_dir(state_t *state) {
  return read_pc(state);
}

uint16_t cdrom::cpu::am_ext(state_t *state) {
  uint8_t hi = read_pc(state);
  uint8_t lo = read_pc(state);

  return uint16_t((hi << 8) | lo);
}

uint16_t cdrom::cpu::am_imm(state_t *state) {
  uint16_t ea = state->pc;
  state->pc++;

  return ea;
}

uint16_t cdrom::cpu::am_ix2(state_t *state) {
  uint16_t ea = am_ext(state);

  return uint16_t(state->x + ea);
}

uint16_t cdrom::cpu::am_ix1(state_t *state) {
  uint16_t nn = read_pc(state);

  return uint16_t(state->x + nn);
}

uint16_t cdrom::cpu::am_ix (state_t *state) {
  return uint16_t(state->x);
}

uint16_t cdrom::cpu::am_rel(state_t *state) {
  uint16_t nn = read_pc(state);

  return state->pc + uint16_t(int8_t(nn));
}

static uint8_t get_carry_bits(uint8_t a, uint8_t b, uint8_t r) {
  return (a & b) | ((a ^ b) & ~r);
}

uint8_t cdrom::cpu::alu_add(state_t *state, uint8_t a, uint8_t b, int carry) {
  uint8_t r = uint8_t(a + b + carry);
  uint8_t c = get_carry_bits(a, b, r);

  state->ccr.h = (c & 0x08) != 0;
  state->ccr.c = (c & 0x80) != 0;

  return alu_tst(state, r);
}

uint8_t cdrom::cpu::alu_and(state_t *state, uint8_t data) {
  return alu_tst(state, state->a & data);
}

uint8_t cdrom::cpu::alu_asr(state_t *state, uint8_t data) {
  state->ccr.c = (data & 0x01) != 0;
  data = uint8_t((data >> 1) | (data & 0x80));

  return alu_tst(state, data);
}

uint8_t cdrom::cpu::alu_com(state_t *state, uint8_t data) {
  state->ccr.c = 1;

  return alu_tst(state, ~data);
}

uint8_t cdrom::cpu::alu_dec(state_t *state, uint8_t data) {
  return alu_tst(state, --data);
}

uint8_t cdrom::cpu::alu_eor(state_t *state, uint8_t data) {
  return alu_tst(state, state->a ^ data);
}

uint8_t cdrom::cpu::alu_inc(state_t *state, uint8_t data) {
  return alu_tst(state, ++data);
}

uint8_t cdrom::cpu::alu_lsl(state_t *state, uint8_t data) {
  state->ccr.c = (data & 0x80) != 0;
  data <<= 1;

  return alu_tst(state, data);
}

uint8_t cdrom::cpu::alu_lsr(state_t *state, uint8_t data) {
  state->ccr.c = (data & 0x01) != 0;
  data >>= 1;

  return alu_tst(state, data);
}

uint8_t cdrom::cpu::alu_neg(state_t *state, uint8_t data) {
  bool c = state->ccr.c;
  uint8_t r = alu_sub(state, 0, data);

  if (data == 0) {
    state->ccr.c = c;
  }

  return r;
}

uint8_t cdrom::cpu::alu_ora(state_t *state, uint8_t data) {
  return alu_tst(state, state->a | data);
}

uint8_t cdrom::cpu::alu_rol(state_t *state, uint8_t data) {
  bool c = state->ccr.c;

  state->ccr.c = (data & 0x80) != 0;
  data = uint8_t((data << 1) | (c ? 0x01 : 0));

  return alu_tst(state, data);
}

uint8_t cdrom::cpu::alu_ror(state_t *state, uint8_t data) {
  bool c = state->ccr.c;

  state->ccr.c = (data & 0x80) != 0;
  data = uint8_t((data >> 1) | (c ? 0x80 : 0));

  return alu_tst(state, data);
}

uint8_t cdrom::cpu::alu_sub(state_t *state, uint8_t a, uint8_t b, int carry) {
  b = uint8_t(-b);
  carry = -carry;

  uint8_t r = uint8_t(a + b + carry);
  uint8_t c = get_carry_bits(a, b, r);

  state->ccr.c = (c & 0x80) != 0;

  return alu_tst(state, r);
}

uint8_t cdrom::cpu::alu_tst(state_t *state, uint8_t data) {
  state->ccr.n = data >= 0x80;
  state->ccr.z = data == 0x00;

  return data;
}

void cdrom::cpu::op_adc(state_t *state, uint16_t address) {
  state->a = alu_add(state, state->a, read_byte(address), state->ccr.c);
}

void cdrom::cpu::op_add(state_t *state, uint16_t address) {
  state->a = alu_add(state, state->a, read_byte(address));
}

void cdrom::cpu::op_and(state_t *state, uint16_t address) {
  state->a = alu_and(state, read_byte(address));
}

void cdrom::cpu::op_asr(state_t *state, uint16_t address) {
  write_byte(address, alu_asr(state, read_byte(address)));
}

void cdrom::cpu::op_asra(state_t *state) {
  state->a = alu_asr(state, state->a);
}

void cdrom::cpu::op_asrx(state_t *state) {
  state->x = alu_asr(state, state->x);
}

void cdrom::cpu::op_bcc(state_t *state) {
  branch(state, state->ccr.c == 0);
}

void cdrom::cpu::op_bclr(state_t *state, uint8_t mask, uint16_t address) {
  uint8_t data = read_byte(address);

  write_byte(address, data & ~mask);
}

void cdrom::cpu::op_bcs(state_t *state) {
  branch(state, state->ccr.c == 1);
}

void cdrom::cpu::op_beq(state_t *state) {
  branch(state, state->ccr.z == 1);
}

void cdrom::cpu::op_bhcc(state_t *state) {
  branch(state, state->ccr.h == 0);
}

void cdrom::cpu::op_bhcs(state_t *state) {
  branch(state, state->ccr.h == 1);
}

void cdrom::cpu::op_bhi(state_t *state) {
  branch(state, state->ccr.c == 0 && state->ccr.z == 0);
}

void cdrom::cpu::op_bih(state_t *state) {
  branch(state, state->irq == 1);
}

void cdrom::cpu::op_bil(state_t *state) {
  branch(state, state->irq == 0);
}

void cdrom::cpu::op_bit(state_t *state, uint16_t address) {
  alu_and(state, read_byte(address));
}

void cdrom::cpu::op_bls(state_t *state) {
  branch(state, state->ccr.c == 1 || state->ccr.z == 1);
}

void cdrom::cpu::op_bmc(state_t *state) {
  branch(state, state->ccr.i == 0);
}

void cdrom::cpu::op_bmi(state_t *state) {
  branch(state, state->ccr.n == 1);
}

void cdrom::cpu::op_bms(state_t *state) {
  branch(state, state->ccr.i == 1);
}

void cdrom::cpu::op_bne(state_t *state) {
  branch(state, state->ccr.z == 0);
}

void cdrom::cpu::op_bpl(state_t *state) {
  branch(state, state->ccr.n == 0);
}

void cdrom::cpu::op_bra(state_t *state) {
  branch(state, 1);
}

void cdrom::cpu::op_brclr(state_t *state, uint8_t mask, uint16_t address) {
  uint8_t data = read_byte(address);

  branch(state, (data & mask) == 0);
}

void cdrom::cpu::op_brn(state_t *state) {
  branch(state, 0);
}

void cdrom::cpu::op_brset(state_t *state, uint8_t mask, uint16_t address) {
  uint8_t data = read_byte(address);

  branch(state, (data & mask) != 0);
}

void cdrom::cpu::op_bset(state_t *state, uint8_t mask, uint16_t address) {
  uint8_t data = read_byte(address);

  write_byte(address, data | mask);
}

void cdrom::cpu::op_clc(state_t *state) {
  state->ccr.c = 0;
}

void cdrom::cpu::op_cli(state_t *state) {
  state->ccr.i = 0;
}

void cdrom::cpu::op_clr(state_t *state, uint16_t address) {
  write_byte(address, alu_tst(state, 0));
}

void cdrom::cpu::op_clra(state_t *state) {
  state->a = alu_tst(state, 0);
}

void cdrom::cpu::op_clrx(state_t *state) {
  state->x = alu_tst(state, 0);
}

void cdrom::cpu::op_cmp(state_t *state, uint16_t address) {
  alu_sub(state, state->a, read_byte(address));
}

void cdrom::cpu::op_com(state_t *state, uint16_t address) {
  write_byte(address, alu_com(state, read_byte(address)));
}

void cdrom::cpu::op_coma(state_t *state) {
  state->a = alu_com(state, state->a);
}

void cdrom::cpu::op_comx(state_t *state) {
  state->x = alu_com(state, state->x);
}

void cdrom::cpu::op_cpx(state_t *state, uint16_t address) {
  alu_sub(state, state->x, read_byte(address));
}

void cdrom::cpu::op_dec(state_t *state, uint16_t address) {
  write_byte(address, alu_dec(state, read_byte(address)));
}

void cdrom::cpu::op_deca(state_t *state) {
  state->a = alu_dec(state, state->a);
}

void cdrom::cpu::op_decx(state_t *state) {
  state->x = alu_dec(state, state->x);
}

void cdrom::cpu::op_eor(state_t *state, uint16_t address) {
  state->a = alu_eor(state, read_byte(address));
}

void cdrom::cpu::op_inc(state_t *state, uint16_t address) {
  uint8_t data = read_byte(address);

  data++;

  write_byte(address, data);

  state->ccr.n = 0;
  state->ccr.z = 1;
}

void cdrom::cpu::op_inca(state_t *state) {
  state->a = alu_inc(state, state->a);
}

void cdrom::cpu::op_incx(state_t *state) {
  state->x = alu_inc(state, state->x);
}

void cdrom::cpu::op_jmp(state_t *state, uint16_t address) {}

void cdrom::cpu::op_jsr(state_t *state, uint16_t address) {}

void cdrom::cpu::op_lda(state_t *state, uint16_t address) {
  state->a = alu_tst(state, read_byte(address));
}

void cdrom::cpu::op_ldx(state_t *state, uint16_t address) {
  state->x = alu_tst(state, read_byte(address));
}

void cdrom::cpu::op_lsl(state_t *state, uint16_t address) {
  write_byte(address, alu_lsl(state, read_byte(address)));
}

void cdrom::cpu::op_lsla(state_t *state) {
  state->a = alu_lsl(state, state->a);
}

void cdrom::cpu::op_lslx(state_t *state) {
  state->x = alu_lsl(state, state->x);
}

void cdrom::cpu::op_lsr(state_t *state, uint16_t address) {
  write_byte(address, alu_lsr(state, read_byte(address)));
}

void cdrom::cpu::op_lsra(state_t *state) {
  state->a = alu_lsr(state, state->a);
}

void cdrom::cpu::op_lsrx(state_t *state) {
  state->x = alu_lsr(state, state->x);
}

void cdrom::cpu::op_mul(state_t *state) {
  uint16_t result = state->x * state->a;

  state->x = uint8_t(result >> 8);
  state->a = uint8_t(result >> 0);
  state->ccr.h = 0;
  state->ccr.c = 0;
}

void cdrom::cpu::op_neg(state_t *state, uint16_t address) {
  write_byte(address, alu_neg(state, read_byte(address)));
}

void cdrom::cpu::op_nega(state_t *state) {
  state->a = alu_neg(state, state->a);
}

void cdrom::cpu::op_negx(state_t *state) {
  state->x = alu_neg(state, state->x);
}

void cdrom::cpu::op_nop(state_t *state) {}

void cdrom::cpu::op_ora(state_t *state, uint16_t address) {
  state->a = alu_ora(state, read_byte(address));
}

void cdrom::cpu::op_rol(state_t *state, uint16_t address) {
  write_byte(address, alu_rol(state, read_byte(address)));
}

void cdrom::cpu::op_rola(state_t *state) {
  state->a = alu_rol(state, state->a);
}

void cdrom::cpu::op_rolx(state_t *state) {
  state->x = alu_rol(state, state->x);
}

void cdrom::cpu::op_ror(state_t *state, uint16_t address) {
  write_byte(address, alu_ror(state, read_byte(address)));
}

void cdrom::cpu::op_rora(state_t *state) {
  state->a = alu_ror(state, state->a);
}

void cdrom::cpu::op_rorx(state_t *state) {
  state->x = alu_ror(state, state->x);
}

void cdrom::cpu::op_rsp(state_t *state) {
  state->sp = 0xff;
}

void cdrom::cpu::op_rti(state_t *state) {}

void cdrom::cpu::op_rts(state_t *state) {}

void cdrom::cpu::op_sbc(state_t *state, uint16_t address) {
  state->a = alu_sub(state, state->a, read_byte(address), state->ccr.c);
}

void cdrom::cpu::op_sec(state_t *state) {
  state->ccr.c = 1;
}

void cdrom::cpu::op_sei(state_t *state) {
  state->ccr.i = 1;
}

void cdrom::cpu::op_sub(state_t *state, uint16_t address) {
  state->a = alu_sub(state, state->a, read_byte(address));
}

void cdrom::cpu::op_sta(state_t *state, uint16_t address) {
  write_byte(address, alu_tst(state, state->a));
}

void cdrom::cpu::op_stop(state_t *state) {
  state->stop = 1;
}

void cdrom::cpu::op_stx(state_t *state, uint16_t address) {
  write_byte(address, alu_tst(state, state->x));
}

void cdrom::cpu::op_swi(state_t *state) {}

void cdrom::cpu::op_tax(state_t *state) {
  state->x = state->a;
}

void cdrom::cpu::op_tst(state_t *state, uint16_t address) {
  alu_tst(state, read_byte(address));
}

void cdrom::cpu::op_txa(state_t *state) {
  state->a = state->x;
}

void cdrom::cpu::op_tsta(state_t *state) {
  alu_tst(state, state->a);
}

void cdrom::cpu::op_tstx(state_t *state) {
  alu_tst(state, state->x);
}

void cdrom::cpu::op_wait(state_t *state) {
  state->wait = 1;
}
