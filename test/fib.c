
int putchar(int c);

int printInt(int n) {
  if (n == 0) {
    return 0;
  }
  printInt(n / 10);
  putchar((n % 10) ^ 48);
  return 0;
}

int fib(int x) {
  if (x < 3) {
    return 1;
  }
  return fib(x - 2) + fib(x - 1);
}

int main() { printInt(fib(20)); }