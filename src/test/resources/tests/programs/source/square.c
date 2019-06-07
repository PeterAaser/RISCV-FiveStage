
#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O0"
// rmsbolt-disassemble: nil
// End:


int mul(int a, int b) {
  int c = 0;
  int ii = 0;
  for(int ii = 0; ii < a; ii++){
    c += b;
  }
  return c;
}

int square(int a){
  return mul(a, a);
}

int main() {
  int a = 6;
  int b = 0xFFFFFFFE; // MAXVAL - 2, (a + b) = -2
  int c = -1; //-1
  int d = 7;       //0x4D2 (c + d) = 0x4D1

  if(square(a+b) > square(c + d))
    return a;
  else
    return c;
}
