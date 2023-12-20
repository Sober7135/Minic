int x = 10;

int test() {
  int x = 10;
  {
    int x = 100;
  }
  return x;
}
