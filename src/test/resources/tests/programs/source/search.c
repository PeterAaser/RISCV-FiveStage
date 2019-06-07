// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O3"
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
  int needle = *(int*)0;
  return find(needle);
}
