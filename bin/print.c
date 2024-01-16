// This is our minimal startup code (usually in _start)
#define ADDR 0x40000000
#define LCD 0x50000000
#define FREQ 20000000
#include "math.h"
asm("nop");
asm("li sp, 0x100000"); // SP set to 1 MB
asm("jal main");        // call main
asm("li a7, 10");       // prepare ecall exit
asm("ecall");           // now your simulator should stop

/*
void reverse(char *str, int length) {
    int start = 0;
    int end = length - 1;
    while (start < end) {
        char temp = str[start];
        str[start] = str[end];
        str[end] = temp;
        start++;
        end--;
    }
}

char* itoa(int num, char* str) {
    int i = 0;

    if (num == 0) {
        str[i++] = '0';
        str[i] = '\0';
        return str;
    }



    while (num != 0) {
        int rem = num % 10;
        if(rem > 9) {
            str[i++] = rem - 10;
        } else {
            rem + '0';
        }
        num = division(num,10);
    }


    str[i] = '\0';

    reverse(str, i);

    return str;
}

void delay(int n) {
    for (int i = 0; i < n; i++) {
        asm("nop");
    }
}

void printToLcd(char str[], int size) {
   int *lcdPtr = (int *) LCD; 
   //Return home
   *lcdPtr = 0b1110000110000;
   delay(5);
   //Display on/off
   *lcdPtr = 0b1110000001100;
   delay(5);
   //Display on/off
   *lcdPtr = 0b1110000000110;
   delay(5);
   //Datawrite
   for (int i = 0; i < size; i++) {
       *lcdPtr = (0b111001 << 7) | str[i];
       delay(5);
   }
}

*/


int main() {
    int *led = (int *) ADDR;
    int *sevenSeg = (int *) (ADDR + 32);
    volatile int i = 5123;
    *led = i;
    *sevenSeg = i;
    /*
    char str[15];
    itoa(5123,str);
    while(1) {
        printToLcd(str, sizeof(str));
    }
    */
    return 0;
}
