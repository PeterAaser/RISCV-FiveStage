#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O3"
// rmsbolt-disassemble: nil
// End:


int f(int x, int* isMemoized, int* memoizedVal){
  if(isMemoized[x])
    return memoizedVal[x];

  if (x == 0) return 0;
  if (x == 1) return 1;

  int next = f(x-1, isMemoized, memoizedVal) + f(x-2, isMemoized, memoizedVal);
  isMemoized[x] = 1;
  memoizedVal[x] = next;

  return next;
}

void setupmem(int n, int* m) {
  for(int ii = 0; ii < n; ii++){
    m[ii] = 0;
  }
}

int main() {
  int* isMemoized  = (int*)0;
  int* memoizedVal = (int*)100;
  setupmem(11, isMemoized);
  setupmem(11, memoizedVal);
  int r = f(10, isMemoized, memoizedVal);
  return r;
}
