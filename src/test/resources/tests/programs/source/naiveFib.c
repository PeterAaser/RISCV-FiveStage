
#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O0"
// rmsbolt-disassemble: nil
// End:

int f(int x){
  if (x == 0) return 0;
  if (x == 1) return 1;
  return f(x-1) + f(x-2);
}


int main() {
  return f(4);
}
