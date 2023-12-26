int putchar(int c);
void print(int x) {
  if (x) {
    print(x - 1);
    putchar(x + 48);
  }
}

int main() { print(9); }