#ifndef __psxact_mdec_core_hpp__
#define __psxact_mdec_core_hpp__


#include "../console.hpp"


struct mdec_t {
  uint32_t io_read(bus_width_t width, uint32_t address);

  void io_write(bus_width_t width, uint32_t address, uint32_t data);
};


#endif // __psxact_mdec_core_hpp__
