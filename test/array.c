int putchar(int c);
int a[3][4];
int test(int x) { return x; }
int main() {
  a[1][2] = 4;
  putchar(a[1][2] + 48);
  {
    // int b[2][3][4][5];
    // b[1][1][1][1] = 5;
    // putchar(b[1][1][1][1] + 48);
  }
}
