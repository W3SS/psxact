#ifndef __PSXACT_CDROM_CPU_HPP__
#define __PSXACT_CDROM_CPU_HPP__

#include <cstdint>

namespace cdrom::cpu {
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

  // instructions

  void clc(state_t *state);
  void cli(state_t *state);
  void rsp(state_t *state);
  void sec(state_t *state);
  void sei(state_t *state);
  void stop(state_t *state);
  void tax(state_t *state);
  void txa(state_t *state);
  void wait(state_t *state);
}

#endif // __PSXACT_CPU_HPP__
