int putchar(int c);
int x = 1, y = 8;

int main() {
  if (x == y) {
    putchar(1 + 48);
  } else {
    putchar(48);
  }
  if (x != y) {
    putchar(49);
  } else {
    putchar(48);
  }
  {
    int x = 1, y = 1;
    if (x == y) {
      putchar(1 + 48);
    } else {
      putchar(48);
    }
    if (x != y) {
      putchar(49);
    } else {
      putchar(48);
    }
  }
  {
    float x = 1, y = 0;
    if (x == y) {
      putchar(1 + 48);
    } else {
      putchar(48);
    }
    if (x != y) {
      putchar(49);
    } else {
      putchar(48);
    }
  }
  {
    float x = 1, y = 1;
    if (x == y) {
      putchar(1 + 48);
    } else {
      putchar(48);
    }
    if (x != y) {
      putchar(49);
    } else {
      putchar(48);
    }
  }
}