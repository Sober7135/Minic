int putchar(int c);
int main() {
  if (1 > 2) {
    putchar(48 + 3);
  } else {
    putchar(48);
  }

  int x = 1, y = 2;
  if (x > y) {
    putchar(48 + 3);
  } else {
    putchar(48);
  }
  {
    float x = 1, y = 2;
    if (x > y) {
      putchar(48 + 3);
    } else {
      putchar(48);
    }
  }
  {
    float x = 1;
    int y = 2;
    if (x > y) {
      putchar(48 + 3);
    } else {
      putchar(48);
    }
  }
  {
    if (1 > 1) {
      putchar(48 + 3);
    } else {
      putchar(48);
    }

    int x = 1, y = 1;
    if (x >= y) {
      putchar(48 + 3);
    } else {
      putchar(48);
    }
    {
      float x = 1, y = 2;
      if (x >= y) {
        putchar(48 + 3);
      } else {
        putchar(48);
      }
    }
    {
      float x = 1;
      int y = 1;
      if (x >= y) {
        putchar(48 + 3);
      } else {
        putchar(48);
      }
    }
  }
}