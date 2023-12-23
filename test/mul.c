int putchar(int c);
void putInt(int x) { putchar(x + 48); }

int main() {
  int x = 7, y = 7, z = 8;
  putInt(x * y);
  putInt(x * y * 1);
  putInt(z * x);
  {
    float x = 7, y = 7, z = 8;
    putInt(x * y);
    putInt(x * y * 1);
    putInt(z * x);
  }
}