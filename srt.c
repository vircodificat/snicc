#include <unistd.h>
#include <stdio.h>

void snicc_print(int x) {
    printf("%d\n", x);
}

void snicc_exit() {
    _exit(0);
}
