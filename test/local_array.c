int putchar(int c);
int test(int x) { return x; }
int main() {
  // char a[2][3];
  // a[1][1] = 49;
  // a[1][2] = '6';
  // putchar(a[1][1]);
  // putchar(a[1][2]);
  {
    // float a[2][3] = {{67.44, test(9)}, {101.22, 102.99}};
    // a[1][1] = 67.44;
    // a[1][2] = 100.99;
    // putchar(a[1][1]);
    // putchar(a[1][2]);
    int a[2][3] = {{00, test(01), 02}, {10, 11, 12}};
    putchar(a[0][0] + 48);
  }
}