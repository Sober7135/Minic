int putchar(int c);

int test(int x, int y, int z) { return 50; }
int x = 57;
int main() {
  putchar(test(0, 0, 0));
  putchar(x);
  return 0;
}