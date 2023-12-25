int putchar(int c);
int a[2][2] = {{1, 2}, {3, 4}};
int test(int x) { return x; }
int main() {
  putchar(a[1][1] + 48);
  a[1][1] = 8;
  putchar(a[1][1] + 48);
  putchar((a[1][1] = 17) + 48);
  {
    int b[2][3][4][5];
    b[1][1][1][1] = 5;
    putchar(b[1][1][1][1] + 48);
  }
}
