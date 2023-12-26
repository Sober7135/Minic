int putchar(int x);

void printInt(int x) {
  if (x == 0) {
    return;
  }
  printInt(x / 10);
  putchar((x % 10) + 48);
}

int main() {
  int x = 14, y = 7;
  printInt(x | y);
  printInt(x & y);
  printInt(x ^ y);
}