/*
If this program compiles and links and exits with 0, the __thread works.
If it fails any step, then __thread does not work.
Usual cross build pluses and minuses -- just because it compiles, or
compiles and links, or runs, does not mean it works.
We know that __declspecl(thread) "always" works #ifdef _WIN32/_MSC_VER,
but not adequately prior to Vista.
*/

#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

__thread int thread_local;

void* thread_function(void* arg)
{
    return &thread_local;
}

int main()
{
    pthread_t thread = { 0 };
    void* thread_result = { 0 };
    int r = { 0 };
    int ExitCode = EXIT_FAILURE;
    const char* Message = "no";

    if (&thread_local == NULL)
        goto Exit;

    r = pthread_create(&thread, NULL, thread_function, NULL);
    if (r != 0)
        goto Exit;

    r = pthread_join(thread, &thread_result);
    if (r != 0)
        goto Exit;

    if (thread_result == &thread_local)
        goto Exit;

    ExitCode = EXIT_SUCCESS;
    Message = "yes";
Exit:
    printf("does __thread work..%s\n", Message);
    return ExitCode;
}
