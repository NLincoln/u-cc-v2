int main() {
  int name[3];
  name[0] = 1;
  name[2] = 2;

  int other[3][4];
  other[0][0] = 1;
  other[0][1] = other[0][0];

  int* ptr = 1;
  ptr[0] = 3;
}
