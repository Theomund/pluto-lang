#include "stdio.h"

/** 
 * Compile the runtime with either of the following commands:
 * gcc -fPIC -shared libpluto.c -o libpluto.so
 * clang -fPIC -shared libpluto.c -o libpluto.so
 */

int print(int n) 
{
    printf("%d\n", n);
    return 0;
}

int input(void)
{
    int a = 0;
    scanf("%d", &a);
    return a;
}
