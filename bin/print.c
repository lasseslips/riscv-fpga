// This is our minimal startup code (usually in _start)
#define ADDR 0x40000000
#define FREQ 20000000
asm("nop");
asm("li sp, 0x100000"); // SP set to 1 MB
asm("jal main");        // call main
asm("li a7, 10");       // prepare ecall exit
asm("ecall");           // now your simulator should stop


int main() {
    int *led = (int *) ADDR;
    int *sevenSeg = (int *) (ADDR + 32);
    volatile int i = 5123;
    *led = i;
    *sevenSeg = i;
    return 0;
}
