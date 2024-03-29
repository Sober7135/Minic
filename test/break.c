
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
  int i = 10;
  while (i < 100) {
    printInt(i);
    i = i + 1;
    if (i == 50) {
      break;
    }
  }
  return 0;
}