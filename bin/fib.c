// This is our minimal startup code (usually in _start)
#define ADDR 0x40000000
#define FREQ 20000000
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

    int res;
    int* ledPtr = (int*)ADDR;
    int* sevenSegPtr = (int*)(ADDR + 32);
    while(1) {
        for(int i = 0; i < 45; i++) {
            for(int j = 0; j < FREQ/8; j++) {
                asm("nop");
            }
            res = fib(i);
            *ledPtr = (res << 9);
            *sevenSegPtr = res;
        }

    }
    return res;
}

