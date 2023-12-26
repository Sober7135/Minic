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

int main() {
  int x = 9, y = 0;
  printInt(-x);
  printInt(y + -x);
  printInt(!y);
  printInt(~x);
}