#include <assert.h>
#include <stdio.h>

typedef struct {
        unsigned rest : 31;
        unsigned sign : 1;
} float_t;

typedef struct {
        unsigned long long rest : 63;
        unsigned long long sign : 1;
} double_t;

float copy_sign_f(float from, float to)
{
        float res = to;
        ((float_t*)&res)->sign = ((float_t*)&from)->sign;
        return res;
}

double copy_sign_d(double from, double to)
{
        double res = to;
        ((double_t*)&res)->sign = ((double_t*)&from)->sign;
        return res;
}

void print_f(float f)
{
  printf("%f %x %x\n", f, *(int*)&f, 0x80000000 & *(int*)&f);
}

void print_d(double f)
{
  printf("%f %llx %llx\n",
         f,
         *(long long*)&f,
         0x8000000000000000LL & *(long long*)&f);
}

int main()
{
        int z32 = 0;
        int n1_32 = 1UL << 31;
        long long z64 = 0;
        long long n1_64 = 1ULL << 63;
        float fz = *(float*)&z32;
        double dz = *(double*)&z64;
        float fn1 = *(float*)&n1_32;
        double dn1 = *(double*)&n1_64;        
        float data[] = {1,-1,2,-2,1000.1123,-1000.123e12 };
        unsigned i;

        print_f(0);
        print_f(1);
        print_f(-1);
        print_d(0);
        print_d(1);
        print_d(-1);
        
/*      assert(fz >= 0);
        assert(dz >= 0);
        assert(fn1 < 0);
        assert(dn1 < 0);
*/        
        for (i = 0; i < sizeof(data)/sizeof(data[0]); ++i)
        {
                float f = data[i];
                double d = data[i];
                printf("%f\n", f);                
                printf("%f\n", copy_sign_f(fz, f));
                printf("%f\n", copy_sign_f(fn1, f));
                printf("%f\n", copy_sign_d(dz, d));
                printf("%f\n\n", copy_sign_d(dn1, d));

                assert(copy_sign_f(fz, f) >= 0);
                assert(copy_sign_f(fn1, f) < 0);
                assert(copy_sign_d(dz, d) >= 0);
                assert(copy_sign_d(dn1, d) < 0);
        }
        
        return 0;
}
