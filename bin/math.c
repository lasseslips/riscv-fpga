#include "math.h"
int flipSign(int a)
{
	int neg = 0;

	// If sign is + ve turn it -ve
	// and vice-versa
    int tmp;
    if (a < 0) {
        tmp = 1;
    } else {
        tmp = -1;
    }
	while (a != 0)
	{
		neg += tmp;
		a += tmp;
	}
	return neg;
}

// Check if a and b are of different signs
int areDifferentSign(int a, int b)
{
	return ((a<0 && b> 0) || (a > 0 && b < 0));
}


// Function to multiply a by b by
// adding a to itself b times
int mul(int a, int b)
{
	// because algo is faster if b<a
	if (a < b)
		return mul(b, a);

	// Adding a to itself b times
	int sum = 0;
	for (int i = b; i > 0; i--)
		sum += a;

	// Check if final sign must be -ve or + ve
	if (b < 0)
		sum = flipSign(sum);

	return sum;
}

// Function to divide a by b by counting how many
// times 'b' can be subtracted from 'a' before
// getting 0
int division(int a, int b)
{
	// Raise exception if b is 0
	int quotient = 0, dividend;

	// Negating b to subtract from a
	int divisor = flipSign(b);

	// Subtracting divisor from dividend
	for (dividend = a; dividend >= divisor;
								dividend += divisor)
		quotient++;

	// Check if a and b are of similar symbols or not
	if (areDifferentSign(a, b))
		quotient = flipSign(quotient);
	return quotient;
}

