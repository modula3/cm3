/* code from MSDN to show MAC address
http://msdn.microsoft.com/en-us/library/aa365943(VS.85).aspx
The output shows it is clearly somewhat buggy.
*/

#include <winsock2.h>
#pragma comment(lib, "iphlpapi.lib")
#include <iphlpapi.h>
#include <stdio.h>
#include <stdlib.h>

int main()
{
    DWORD dwRetVal = { 0 };
    DWORD dwSize = { 0 };
    MIB_IFTABLE* Table = { 0 };
    MIB_IFROW* Row = { 0 };
    DWORD i = { 0 };

    /* Call until it fits, typically twice. */
    dwSize = 0;
    while ((dwRetVal = GetIfTable(Table, &dwSize, FALSE)) == ERROR_INSUFFICIENT_BUFFER)
    {
        free(Table);
        Table = (MIB_IFTABLE *)malloc(dwSize);
        if (Table == NULL)
        {
            printf("Error allocating memory needed to call GetIfTable\n");
            dwRetVal = 1;
            goto Exit;
        }
    }
    if (dwRetVal)
        goto Exit;

    for (i = 0; i < Table->dwNumEntries; ++i)
    {
        Row = &Table->table[i];
        if (Row->dwType != IF_TYPE_ETHERNET_CSMACD || Row->dwPhysAddrLen != 6)
            continue;
        printf("%.2X-%.2X-%.2X-%.2X-%.2X-%.2X\n",
               Row->bPhysAddr[0],
               Row->bPhysAddr[1],
               Row->bPhysAddr[2],
               Row->bPhysAddr[3],
               Row->bPhysAddr[4],
               Row->bPhysAddr[5]);
    }
    dwRetVal = 0;
Exit:
    if (dwRetVal)
        printf("error %u\n", dwRetVal);
    free(Table);
    return dwRetVal;
}
