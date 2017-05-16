#include "cpu.hpp"

static uint8_t read_byte(uint16_t address) {
  return 0;
}

void cdrom::cpu::tick(state_t *state) {
  auto code = read_byte(state->pc);
  state->pc++;

  switch (code) {
    case 0x00: break; // brset0 (btb)
    case 0x01: break; // brclr0 (btb)
    case 0x02: break; // brset1 (btb)
    case 0x03: break; // brclr1 (btb)
    case 0x04: break; // brset2 (btb)
    case 0x05: break; // brclr2 (btb)
    case 0x06: break; // brset3 (btb)
    case 0x07: break; // brclr3 (btb)
    case 0x08: break; // brset4 (btb)
    case 0x09: break; // brclr4 (btb)
    case 0x0a: break; // brset5 (btb)
    case 0x0b: break; // brclr5 (btb)
    case 0x0c: break; // brset6 (btb)
    case 0x0d: break; // brclr6 (btb)
    case 0x0e: break; // brset7 (btb)
    case 0x0f: break; // brclr7 (btb)

    case 0x10: break; // bset0 (bsc)
    case 0x11: break; // bclr0 (bsc)
    case 0x12: break; // bset1 (bsc)
    case 0x13: break; // bclr1 (bsc)
    case 0x14: break; // bset2 (bsc)
    case 0x15: break; // bclr2 (bsc)
    case 0x16: break; // bset3 (bsc)
    case 0x17: break; // bclr3 (bsc)
    case 0x18: break; // bset4 (bsc)
    case 0x19: break; // bclr4 (bsc)
    case 0x1a: break; // bset5 (bsc)
    case 0x1b: break; // bclr5 (bsc)
    case 0x1c: break; // bset6 (bsc)
    case 0x1d: break; // bclr6 (bsc)
    case 0x1e: break; // bset7 (bsc)
    case 0x1f: break; // bclr7 (bsc)

    case 0x20: break; // bra  (rel)
    case 0x21: break; // brn  (rel)
    case 0x22: break; // bhi  (rel)
    case 0x23: break; // bls  (rel)
    case 0x24: break; // bcc  (rel)
    case 0x25: break; // bcs  (rel)
    case 0x26: break; // bne  (rel)
    case 0x27: break; // beq  (rel)
    case 0x28: break; // bhcc (rel)
    case 0x29: break; // bhcs (rel)
    case 0x2a: break; // bpl  (rel)
    case 0x2b: break; // bmi  (rel)
    case 0x2c: break; // bmc  (rel)
    case 0x2d: break; // bms  (rel)
    case 0x2e: break; // bil  (rel)
    case 0x2f: break; // bih  (rel)

    case 0x30: break; // neg (dir)
    case 0x31: break; //
    case 0x32: break; //
    case 0x33: break; // com (dir)
    case 0x34: break; // lsr (dir)
    case 0x35: break; //
    case 0x36: break; // ror (dir)
    case 0x37: break; // asr (dir)
    case 0x38: break; // lsl (dir)
    case 0x39: break; // rol (dir)
    case 0x3a: break; // dec (dir)
    case 0x3b: break; //
    case 0x3c: break; // inc (dir)
    case 0x3d: break; // tst (dir)
    case 0x3e: break; //
    case 0x3f: break; // clr (dir)

    case 0x40: break; // nega (inh)
    case 0x41: break; //
    case 0x42: break; // mul  (inh)
    case 0x43: break; // coma (inh)
    case 0x44: break; // lsra (inh)
    case 0x45: break; //
    case 0x46: break; // rora (inh)
    case 0x47: break; // asra (inh)
    case 0x48: break; // lsla (inh)
    case 0x49: break; // rola (inh)
    case 0x4a: break; // deca (inh)
    case 0x4b: break; //
    case 0x4c: break; // inca (inh)
    case 0x4d: break; // tsta (inh)
    case 0x4e: break; //
    case 0x4f: break; // clra (inh)

    case 0x50: break; // negx (inh)
    case 0x51: break; //
    case 0x52: break; //
    case 0x53: break; // comx (inh)
    case 0x54: break; // lsrx (inh)
    case 0x55: break; //
    case 0x56: break; // rorx (inh)
    case 0x57: break; // asrx (inh)
    case 0x58: break; // lslx (inh)
    case 0x59: break; // rolx (inh)
    case 0x5a: break; // decx (inh)
    case 0x5b: break; //
    case 0x5c: break; // incx (inh)
    case 0x5d: break; // tstx (inh)
    case 0x5e: break; //
    case 0x5f: break; // clrx (inh)

    case 0x60: break; // neg (ix1)
    case 0x61: break; //
    case 0x62: break; //
    case 0x63: break; // com (ix1)
    case 0x64: break; // lsr (ix1)
    case 0x65: break; //
    case 0x66: break; // ror (ix1)
    case 0x67: break; // asr (ix1)
    case 0x68: break; // lsl (ix1)
    case 0x69: break; // rol (ix1)
    case 0x6a: break; // dec (ix1)
    case 0x6b: break; //
    case 0x6c: break; // inc (ix1)
    case 0x6d: break; // tst (ix1)
    case 0x6e: break; //
    case 0x6f: break; // clr (ix1)

    case 0x70: break; // neg (ix)
    case 0x71: break; //
    case 0x72: break; //
    case 0x73: break; // com (ix)
    case 0x74: break; // lsr (ix)
    case 0x75: break; //
    case 0x76: break; // ror (ix)
    case 0x77: break; // asr (ix)
    case 0x78: break; // lsl (ix)
    case 0x79: break; // rol (ix)
    case 0x7a: break; // dec (ix)
    case 0x7b: break; //
    case 0x7c: break; // inc (ix)
    case 0x7d: break; // tst (ix)
    case 0x7e: break; //
    case 0x7f: break; // clr (ix)

    case 0x80: break; // rti (inh)
    case 0x81: break; // rts (inh)
    case 0x82: break; //
    case 0x83: break; // swi (inh)
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
    case 0x8e: stop(state); break; // stop (inh)
    case 0x8f: wait(state); break; // wait (inh)

    case 0x90: break; //
    case 0x91: break; //
    case 0x92: break; //
    case 0x93: break; //
    case 0x94: break; //
    case 0x95: break; //
    case 0x96: break; //
    case 0x97: tax(state); break; // tax (inh)
    case 0x98: clc(state); break; // clc (inh)
    case 0x99: sec(state); break; // sec (inh)
    case 0x9a: cli(state); break; // cli (inh)
    case 0x9b: sei(state); break; // sei (inh)
    case 0x9c: rsp(state); break; // rsp (inh)
    case 0x9d: break; // nop (inh)
    case 0x9e: break; //
    case 0x9f: txa(state); break; // txa (inh)

    case 0xa0: break; // sub (imm)
    case 0xa1: break; // cmp (imm)
    case 0xa2: break; // sbc (imm)
    case 0xa3: break; // cpx (imm)
    case 0xa4: break; // and (imm)
    case 0xa5: break; // bit (imm)
    case 0xa6: break; // lda (imm)
    case 0xa7: break; //
    case 0xa8: break; // eor (imm)
    case 0xa9: break; // adc (imm)
    case 0xaa: break; // ora (imm)
    case 0xab: break; // add (imm)
    case 0xac: break; //
    case 0xad: break; // bsr (imm)
    case 0xae: break; // ldx (imm)
    case 0xaf: break; //

    case 0xb0: break; // sub (dir)
    case 0xb1: break; // cmp (dir)
    case 0xb2: break; // sbc (dir)
    case 0xb3: break; // cpx (dir)
    case 0xb4: break; // and (dir)
    case 0xb5: break; // bit (dir)
    case 0xb6: break; // lda (dir)
    case 0xb7: break; // sta (dir)
    case 0xb8: break; // eor (dir)
    case 0xb9: break; // adc (dir)
    case 0xba: break; // ora (dir)
    case 0xbb: break; // add (dir)
    case 0xbc: break; // jmp (dir)
    case 0xbd: break; // jsr (dir)
    case 0xbe: break; // ldx (dir)
    case 0xbf: break; // stx (dir)

    case 0xc0: break; // sub (ext)
    case 0xc1: break; // cmp (ext)
    case 0xc2: break; // sbc (ext)
    case 0xc3: break; // cpx (ext)
    case 0xc4: break; // and (ext)
    case 0xc5: break; // bit (ext)
    case 0xc6: break; // lda (ext)
    case 0xc7: break; // sta (ext)
    case 0xc8: break; // eor (ext)
    case 0xc9: break; // adc (ext)
    case 0xca: break; // ora (ext)
    case 0xcb: break; // add (ext)
    case 0xcc: break; // jmp (ext)
    case 0xcd: break; // jsr (ext)
    case 0xce: break; // ldx (ext)
    case 0xcf: break; // stx (ext)

    case 0xd0: break; // sub (ix2)
    case 0xd1: break; // cmp (ix2)
    case 0xd2: break; // sbc (ix2)
    case 0xd3: break; // cpx (ix2)
    case 0xd4: break; // and (ix2)
    case 0xd5: break; // bit (ix2)
    case 0xd6: break; // lda (ix2)
    case 0xd7: break; // sta (ix2)
    case 0xd8: break; // eor (ix2)
    case 0xd9: break; // adc (ix2)
    case 0xda: break; // ora (ix2)
    case 0xdb: break; // add (ix2)
    case 0xdc: break; // jmp (ix2)
    case 0xdd: break; // jsr (ix2)
    case 0xde: break; // ldx (ix2)
    case 0xdf: break; // stx (ix2)

    case 0xe0: break; // sub (ix1)
    case 0xe1: break; // cmp (ix1)
    case 0xe2: break; // sbc (ix1)
    case 0xe3: break; // cpx (ix1)
    case 0xe4: break; // and (ix1)
    case 0xe5: break; // bit (ix1)
    case 0xe6: break; // lda (ix1)
    case 0xe7: break; // sta (ix1)
    case 0xe8: break; // eor (ix1)
    case 0xe9: break; // adc (ix1)
    case 0xea: break; // ora (ix1)
    case 0xeb: break; // add (ix1)
    case 0xec: break; // jmp (ix1)
    case 0xed: break; // jsr (ix1)
    case 0xee: break; // ldx (ix1)
    case 0xef: break; // stx (ix1)

    case 0xf0: break; // sub (ix)
    case 0xf1: break; // cmp (ix)
    case 0xf2: break; // sbc (ix)
    case 0xf3: break; // cpx (ix)
    case 0xf4: break; // and (ix)
    case 0xf5: break; // bit (ix)
    case 0xf6: break; // lda (ix)
    case 0xf7: break; // sta (ix)
    case 0xf8: break; // eor (ix)
    case 0xf9: break; // adc (ix)
    case 0xfa: break; // ora (ix)
    case 0xfb: break; // add (ix)
    case 0xfc: break; // jmp (ix)
    case 0xfd: break; // jsr (ix)
    case 0xfe: break; // ldx (ix)
    case 0xff: break; // stx (ix)
  }
}

void cdrom::cpu::clc(state_t *state) {
  state->ccr.c = 0;
}

void cdrom::cpu::cli(state_t *state) {
  state->ccr.i = 0;
}

void cdrom::cpu::rsp(state_t *state) {
  state->sp = 0xff;
}

void cdrom::cpu::sec(state_t *state) {
  state->ccr.c = 1;
}

void cdrom::cpu::sei(state_t *state) {
  state->ccr.i = 1;
}

void cdrom::cpu::stop(state_t *state) {
  state->stop = 1;
}

void cdrom::cpu::tax(state_t *state) {
  state->x = state->a;
}

void cdrom::cpu::txa(state_t *state) {
  state->a = state->x;
}

void cdrom::cpu::wait(state_t *state) {
  state->wait = 1;
}
