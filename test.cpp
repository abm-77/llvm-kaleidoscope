#include <stdio.h>

extern "C" {
double hey(double);
double dosmth(double);
}

int main() {
  printf("hey: %f\n", hey(5));
  printf("dosmth: %f\n", dosmth(13));
}
