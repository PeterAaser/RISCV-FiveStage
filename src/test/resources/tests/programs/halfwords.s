main:	
  li x1, 0x11223344
  li x2, 0x55667788
  li x10, 0x100
  sw x1,  0(x10)
  sw x2,  4(x10)
  lw x3,  2(x10)
  lh x4,  3(x10)
  lb x5,  3(x10)
  lhu x6, 3(x10)
  lbu x7, 3(x10)
  sw x1, 8(x10)
  sw x1, 9(x10)
  sh x2, 9(x10)
  sb x2, 11(x10)
  lw x12, 8(x10)
  lw x13, 12(x10)
  done
