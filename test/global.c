int x, y = 114541, z = 1.3;
float ff, kk = 44, kkk = 44.0, ll = 9.0;

int putchar(int c);
void printInt(int x) { putchar(x + 48); }
int main() {
  printInt(x = 1);
  printInt(y = 2);
  printInt(z = 3);
  printInt(ff = 4);
  printInt(kk = 5);
  printInt(kkk = 6);
  printInt(kkk = 7);
}