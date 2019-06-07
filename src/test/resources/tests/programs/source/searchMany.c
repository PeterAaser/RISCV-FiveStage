// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O1"
// rmsbolt-disassemble: nil
// End:

/**
 * Represents a binary tree
 */
typedef struct {
  unsigned int hasLeft : 1;
  unsigned int leftIndex : 7;
  unsigned int hasRight : 1;
  unsigned int rightIndex : 7;
  int value : 16;
} node;


int find(int findMe){
  int found = 0;
  node* currentNodeIdx = (node*)4;
  int currentValue = 0;

  while(!found){
    currentValue = currentNodeIdx->value;
    if(currentValue == findMe){
      return (int)currentNodeIdx;
    }

    if((currentValue > findMe) && currentNodeIdx->hasLeft){
      int nextNodeIdx = currentNodeIdx->leftIndex;
      currentNodeIdx = (node*)(4 + (nextNodeIdx << 2));
      continue;
    }

    if((currentValue < findMe) && currentNodeIdx->hasRight){
      int nextNodeIdx = currentNodeIdx->rightIndex;
      currentNodeIdx = (node*)(4 + (nextNodeIdx << 2));
      continue;
    }

    return -1;
  }
}

int main() {
  // Where does the needle list start?
  int needles = *(int*)0;
  int sum = 0;

  // How many needles are there?
  int numNeedles = *(int*)needles;
  int nextNeedle = (int)needles + 4;

  // Some useless calculations to make gcc O3 happy
  for(int ii = 0; ii < numNeedles; ii++){
    nextNeedle += 4;
    int needle = *(int*)(nextNeedle);
    sum += find(needle);
  }
  
  return sum;
}
