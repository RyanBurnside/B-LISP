#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
 
void print_analyzed_double(double x)
{
    uint64_t val = *(uint64_t*)&x;
    uint64_t mask = ((uint64_t)0xFF) << 56;
    int i;
 
    printf("Analyzed double\n");
    printf(" Double representation\n");
    printf("  Original double value: %.16f\n", x);
    printf("  Sign bit: %ld\n", val >> 63);
    printf("  Exponent: 0x%03lx\n", (val >> 52) & 0x7FF);
    printf("  Mantissa: 0x%013lx\n", val & 0xFFFFFFFFFFFFFUL);
    printf("  Is NaN: %s\n", x == x ? "no" : "yes");
    printf(" Raw Bytes From MSB to LSB\n");
    printf("  0x ");
    for (i = 0; i < sizeof(uint64_t); i++) {
        printf("%02lx ", (val & mask) >> ((sizeof(uint64_t) - 1 - i) * 8));
        mask >>= 8;
    }
    printf("\n");
    printf(" Individual bits\n");
    printf("  ");
    mask = ((uint64_t)1) << 63;
    for (i = 0; i < sizeof(uint64_t) * 8; i++) {
        switch(i) {
            case 1:
            case 12:
                printf(" ");
            break;
        }
        printf("%d", (val & mask) ? 1 : 0);
        mask >>= 1;
    }
    printf("\n");
    printf("  ");
    for (i = 0; i < sizeof(uint64_t) * 8; i++) {
        switch(i) {
            case 1:
            case 12:
                printf(" ");
            break;
        }
        if (i == 0) {
            printf("s");
        } else if (i > 0 && i < 12) {
            printf("e");
        } else {
            printf("m");
        }
    }
    printf("\n");
    printf(" TinyLisp representation\n");
    printf("  Tag Bits: 0x%04lx\n", val >> 48);
    printf("  Storage Bits: 0x%012lx\n", val & 0xFFFFFFFFFFFFUL);
}
 
 
int main(void)
{
    double x;
    uint16_t t;
    uint64_t i;
 
    // https://lukaskollmer.de/ieee-754-visualizer/ to verify
    print_analyzed_double(-45.0);
    print_analyzed_double(45.0);
 
    // Build an example representation
    t = 0x7ffc; // NIL tag
    i = 0x112233445566UL; // Some 48-bits long value
    *(uint64_t*)&x = (uint64_t)t<<48 | i;
 
    print_analyzed_double(x);
 
    return 0;
}
