int putchar(int c);

void print(int c);

void print(int c) { putchar(c); }

int main() {
  print(48);
  return 0;
}