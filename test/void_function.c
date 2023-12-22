int putchar(int c);

void test() {}

void print() {
  int x = 1;
  if (x) {
    putchar(50);
    // return;
  } else {
    putchar(97);
    // return;
  }
  return;
}

int main() {
  print();
  return 0;
}