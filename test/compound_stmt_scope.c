int putchar(int c);

int main() {
  int x = 6;
  {
    int x = 9;
    putchar(x + 48);
  }
  putchar(x + 48);
  return 0;
}