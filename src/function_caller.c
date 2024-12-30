#include <stdio.h>

typedef double (*func_t)(double);

void call_function(func_t f, double x)
{
    printf("Result: %f\n", f(x));
}
