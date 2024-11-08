int main(void) {
  int a = 0;
  int b = 1;

  int n = 10;
  loop: if (n == 0) goto exit;

  int temp = a;
  a = b;
  b += temp;
  n -= 1;
  goto loop;

  exit: return a;
}
