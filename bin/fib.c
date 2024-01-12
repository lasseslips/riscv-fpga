// This is our minimal startup code (usually in _start)
#define ADDR 0x40000000
asm("nop");
asm("li sp, 0x100000"); // SP set to 1 MB
asm("jal main");        // call main
asm("li a7, 10");       // prepare ecall exit
asm("ecall");           // now your simulator should stop


int fib(int n) {
    if (n <= 1) return n;

    int a = 0, b = 1, c, i;
    for(i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}

int main(void) {
    int res = fib(10);
    int* output_ptr = (int*)ADDR;
    *output_ptr = res;
    return res;
}

