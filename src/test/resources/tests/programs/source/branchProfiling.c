#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O1"
// rmsbolt-disassemble: nil
// End:

int rem(int a, int b){
  if(a < b)
    return a;
  else 
    return rem(a - b, b);
}

int f1(int arg1){
  int acc = 0;
  // the fish operator
  while(arg1 --> 0){
    if(arg1 == 241)
      return acc;
    if(rem(arg1, 10) == 0)
       acc += arg1;
  }
  return acc;
}


int f2(int arg1){
  int ii;
  int acc = 0;

  // <3 <3
  for(ii = 0; ii <3 ; ii++){
    acc += f1(arg1 - ii) + f1(arg1 + ii);
  }
  return acc;
}


int f3(int arg1){
  if(rem(arg1, 10) == 0)
    return f2(arg1);
  else if(rem(arg1, 20) == 0)
    return f1(arg1);

  return f1(arg1) + f2(arg1);
}

int getCall(int op, int arg){
  if(op == 0)
    return f1(arg);
  else if(op == 1)
    return f2(arg);
  else
    return f3(arg);
}

int run() {
  int ii;
  int* arr1 = (int*)0;
  int* arr2 = (int*)32;
  int acc = 0;
  int cnt = 0;
  for(ii = 0; ii < 6; ii++){
    int mem1 = arr1[ii];
    int mem2 = arr2[6 - ii];
    if(cnt <3)
      cnt++;
    if(cnt == 3)
      cnt = 0;
    acc += getCall(cnt, mem1) - getCall(cnt, mem2);
  }
  return acc;
}

int main(){
  return run();
}
