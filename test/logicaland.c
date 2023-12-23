int putchar(int c);

int main() {
  int x = 0, y = 1, z = 2;
  if (x && y) {
    putchar(1 + 48);
  } else {
    putchar(48);
  }
}