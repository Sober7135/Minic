
int putchar(int c);
int test() { return 1; }

int main() {
  // int test();
  putchar(test() + 48);
}