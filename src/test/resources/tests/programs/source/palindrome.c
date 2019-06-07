// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O3"
// rmsbolt-disassemble: nil
// End:

int main() {

  /* int palindrome[8]; */
  /* int notAPalindrome[16]; */

  // Set up "heap" addresses
  int palindrome = (int*)0;
  int notAPalindrome = (int*)32;

  return isPalindrome(palindrome, 0, 7) && isPalindrome(notAPalindrome, 0, 15);
}

int isPalindrome(int* word, int start, int stop){
  if(start >= stop){
    return 1;
  }
  else{
    int currentIsPalindrome = (word[start] == word[stop]);
    return currentIsPalindrome && isPalindrome(word, start + 1, stop - 1);
  }
}
