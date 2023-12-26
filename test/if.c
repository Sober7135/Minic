int putchar(int c);
void printInt(int x) {
  if (x < 0) {
    x = -x;
    putchar('-');
  }
  if (x == 0) {
    return;
  }
  printInt(x / 10);
  putchar(x % 10 ^ 48);
}

int x = 114514;

int main() {
  if (x) {
    printInt(x);
  }
  return 0;
}