int x = 6;

int test() { return 1; }
int putchar(int x);
int main() {
  int y = 57;
  float f = 9.0;
  char z = '9';
  x = 9;
  y = test();
  x = 9;
  putchar(x + 48);
  putchar(y + 48);
  putchar(z);
  putchar(f + 48);

  return 0;
}