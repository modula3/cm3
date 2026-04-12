#include "TimePosixC.c"

int main()
{
    LONGREAL sample = ComputeGrainViaSampling();
    struct timespec real = { 0 };
    struct timespec high = { 0 };
    int i = { 0 };
#ifdef CLOCK_HIGHRES
    i = clock_getres(CLOCK_HIGHRES, &high);
    assert(i == 0);
#endif
#ifdef CLOCK_REALTIME
    i = clock_getres(CLOCK_REALTIME, &real);
    assert(i == 0);
#endif
    printf("real: %lu.%lu %e\n", real.tv_sec, real.tv_nsec, TimePosix__FromNanotime(&real));
    printf("high: %lu.%lu %e\n", high.tv_sec, high.tv_nsec, TimePosix__FromNanotime(&high));
    printf("sample: %e\n", sample);
    return 0;
}
