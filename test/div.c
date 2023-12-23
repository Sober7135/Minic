int putchar(int c);

int main() {
  int x = 48 * 5, y = 49 * 5, z = 50 * 5;
  putchar(x / 5);
  putchar(y / 5);
  putchar(z / 5);
  {
    float x = 48.0 * 5, y = 49.0 * 5, z = 50.0 * 5;
    putchar(x / 5);
    putchar(y / 5);
    putchar(z / 5);
  }
}