int putchar(int c);

int test(int x, int y, int z) { return 1; }

int main() {
  int x = test(1, 2, 3);
  putchar('c');
  return 0;
}