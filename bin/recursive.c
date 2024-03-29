// This is our minimal startup code (usually in _start)
asm("nop"); 
asm("li sp, 0x100000"); // SP set to 1 MB
asm("jal main");        // call main
asm("li a7, 10");       // prepare ecall exit
asm("ecall");           // now your simulator should stop

int recursive(int iter)
{
	if(iter < 1)
		return 1;
	return recursive(iter-1) + 1;
}

int main(void) 
{
	return recursive(100);
}
