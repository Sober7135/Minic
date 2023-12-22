int putchar(int c);

int test(int x, int y, int z) { return 50; }

int main() {
  putchar(test(0, 0, 0));
  return 0;
}