int putchar(int c);
void putInt(int x) { putchar(x + 48); }

int x = 199;

int main() {
  putInt(x - 199);
  putInt(x - 198);
  putInt(x - 197);
  putInt(x - 196);
  putInt(x - 195);
  {
    float x = 200;
    putInt(x - 199);
    putInt(x - 198);
    putInt(x - 197);
    putInt(x - 196);
    putInt(x - 195);
  }
}