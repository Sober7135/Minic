
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
  int i = 0;
  while (i < 20) {
    i = i + 1;
    printInt(i);
  }
}