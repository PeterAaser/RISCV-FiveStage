// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O0"
// rmsbolt-disassemble: nil
// End:


int lookup(int x, int y, int dim){
  int t = 0;
  int ii;
  for(ii = 0; ii < y; ii++){
    t += dim;
  }
  return t + x;
}

void convolutePixel(int x, int y, int* image, int* output, int* kernel){
  int acc = 0;
  acc += image[lookup( x - 1 , y - 1 , 32)] << kernel[0];
  acc += image[lookup( x     , y - 1 , 32)] << kernel[1];
  acc += image[lookup( x + 1 , y - 1 , 32)] << kernel[2];

  acc += image[lookup( x - 1 , y     , 32)] << kernel[3];
  acc += image[lookup( x     , y     , 32)] << kernel[4];
  acc += image[lookup( x + 1 , y     , 32)] << kernel[5];

  acc += image[lookup( x - 1 , y + 1 , 32)] << kernel[6];
  acc += image[lookup( x     , y + 1 , 32)] << kernel[7];
  acc += image[lookup( x + 1 , y + 1 , 32)] << kernel[8];

  output[lookup(x, y, 30)] = acc;
}

int run() {

  int* image = (int*)0;
  int* output = (int*)(1024);
  int* kernel = (int*)(1924);

  int ii;
  int kk;
  for(ii = 1; ii < 31; ii++){
    for(kk = 1; kk < 31; kk++){
      convolutePixel(ii, kk, image, output, kernel);
    }
  }
  return 0;
}

int main(){
  run();
}
