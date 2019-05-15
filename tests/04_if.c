int main() {
  int a = 3;
  if (a) {
    a++;
  } else {
    a--;
  }

  int b = 0;
  if (b) {
    b++;
  } else {
    if (a) {
      b += 1;
    }
  }

}
