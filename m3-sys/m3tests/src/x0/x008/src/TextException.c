/* jay.krell@cornell.edu November 20 2026 */

#if __cplusplus
extern "C" {
#endif

void TestException_Raise(void)
{
  *(volatile char*)0;
}

#if __cplusplus
}
#endif
