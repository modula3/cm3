/*
This program passes floats and doubles from Modula-3 to C code to check
that they have the correct value. Strings are used as a presumed working
transport.
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


const double Doubles[] =  {  0.0,   -0.0,   1.0,   -1.0,   0.5,   -0.5,   1.5,   -1.5  };
const float  Floats[]  =  {  0.0F,  -0.0F,  1.0F,  -1.0F,  0.5F,  -0.5F,  1.5F,  -1.5F };
const char*  Strings[]  = { "0.0", "-0.0", "1.0", "-1.0", "0.5", "-0.5", "1.5", "-1.5", 0 };

int LittleEndian(void)
{
    union
    {
        unsigned char a[sizeof(unsigned)];
        unsigned b;
    } u = { 1 };
    return (u.b == 1);
}

const char* M3toC__SharedTtoS(void* Text);

void Check(unsigned Type, void* Float, void* Text)
/* Type: 0 for float, 1 for double */
{
    unsigned i = { 0 };
    unsigned char* b = { 0 };
    unsigned char* c = { 0 };

    /* We don't bother freeing this. */
    const char* String = M3toC__SharedTtoS(Text);

    /* Change the indices to always print the bytes in little endian order, for portable output. */
    unsigned x = (LittleEndian() ? 0 : (Type ? 7 : 3));

    if (sizeof(float) != 4)
    {
        printf("double not 8 bytes\n");
        exit(EXIT_FAILURE);
    }

    if (sizeof(double) != 8)
    {
        printf("double not 8 bytes\n");
        exit(EXIT_FAILURE);
    }

    b = (unsigned char*) Float;
    String += strspn(String, " ");
    for (i = 0 ; Strings[i] ; ++i)
    {
        if (strcmp(String, Strings[i]) == 0)
        {
            c = (unsigned char*) (Type ? &Doubles[i] : (void*) &Floats[i]);
            if (memcmp(b, c, (Type ? sizeof(double) : sizeof(float))) != 0)
            {
                if (Type)
                {
                    printf("double mismatch string %s test %x Modula-3 value %f C value %f Modula-3 bytes 0x%02x%02x%02x%02x%02x%02x%02x%02x C bytes 0x%02x%02x%02x%02x%02x%02x%02x%02x\n",
                        String, i, (*(double*) Float), Doubles[i],
                        b[0 ^ x], b[1 ^ x], b[2 ^ x], b[3 ^ x], b[4 ^ x], b[5 ^ x], b[6 ^ x], b[7 ^ x],
                        c[0 ^ x], c[1 ^ x], c[2 ^ x], c[3 ^ x], c[4 ^ x], c[5 ^ x], c[6 ^ x], c[7 ^ x]);
                }
                else
                {
                    printf("float mismatch string %s test %x Modula-3 value %f C value %f Modula-3 bytes  0x%02x%02x%02x%02x C bytes 0x%02x%02x%02x%02x\n",
                        String, i, (*(float*) Float), Floats[i],
                        b[0 ^ x], b[1 ^ x], b[2 ^ x], b[3 ^ x],
                        c[0 ^ x], c[1 ^ x], c[2 ^ x], c[3 ^ x]);
                    exit(EXIT_FAILURE);
                }
            }
            else
            {
                if (Type)
                {
                    printf("double match string %s test %x Modula-3 value %f C value %f bytes Modula-3 bytes 0x%02x%02x%02x%02x%02x%02x%02x%02x C bytes 0x%02x%02x%02x%02x%02x%02x%02x%02x\n",
                        String, i, (*(double*) Float), Doubles[i],
                        b[0 ^ x], b[1 ^ x], b[2 ^ x], b[3 ^ x], b[4 ^ x], b[5 ^ x], b[6 ^ x], b[7 ^ x],
                        c[0 ^ x], c[1 ^ x], c[2 ^ x], c[3 ^ x], c[4 ^ x], c[5 ^ x], c[6 ^ x], c[7 ^ x]);
                }
                else
                {
                    printf("float match string %s test %x Modula-3 value %f C value %f Modula-3 bytes 0x%02x%02x%02x%02x C bytes 0x%02x%02x%02x%02x\n",
                        String, i, (*(float*) Float), Floats[i],
                        b[0 ^ x], b[1 ^ x], b[2 ^ x], b[3 ^ x],
                        c[0 ^ x], c[1 ^ x], c[2 ^ x], c[3 ^ x]);
                }
            }
            fflush(stdout);
            return;
        }
    }
    printf("string not found %s\n", (Type ? "double" : "float", String));
    exit(EXIT_FAILURE);
}

void CheckD(double Double, void* Text)
{
    Check(1, &Double, Text);
}

void CheckF(float Float, void* Text)
{
    Check(0, &Float, Text);
}

#ifdef __cplusplus
}
#endif

