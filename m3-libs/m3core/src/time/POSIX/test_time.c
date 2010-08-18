#include "TimePosixC.c"

int main()
{
    LONGREAL sample = ComputeGrainViaSampling();
    LONGREAL res = ComputeGrainViaClockGetRes();
    printf("%e %e\n", ComputeGrainViaSampling(), ComputeGrainViaClockGetRes());
    return 0;
}
