#include <stdio.h>
#include <string>
#include <vector>

using namespace std;

string string_replace(string a,
                      const char* from,
                      size_t from_length,
                      const char* to,
                      size_t to_length)
{
    size_t const a_length = a.length();
    if (a_length < from_length || from_length < 1 || a_length < 1
        || (from_length == to_length
            && from[0] == to[0]
            && memcmp(from, to, from_length) == 0))
        return a;
    size_t count = 0;
    const char* const a_cstr = a.c_str();
    char const from0 = from[0];
    size_t i = 0;
    size_t first = 0;
    // a_length == 2; from_length == 2
    //  i = [0..0]
    // a_length == 3; from_length == 2
    //  i = [0..1]
    for (i = 0; i <= a_length - from_length; ++i)
    {
        if (a_cstr[i] == from0 && memcmp(&a_cstr[i], from, from_length) == 0)
        {
            first = count ? first : i;
            count += 1;
            i += from_length - 1;
        }
    }
    if (count < 1)
        return a;
    string result;
    size_t result_length = a_length - (from_length - to_length) * count;
    if (result_length < 1)
        return string();
    if (0)
        printf("replace(%s, %s, %s) count=%d first=%d result_length:%d\n", a_cstr, from, to,
        (int)count, (int)first, (int)result_length);
    result.reserve(result_length);
    result.append(a_cstr, a_cstr + first);
    //printf("%s(%d) first:%d %s\n", __FILE__, __LINE__, (int)first, result.c_str());
    for (i = first; i <= a_length - from_length; ++i)
    {
        if (count == 0) // copy the tail
        {
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
            result.append(a_cstr + i, a_cstr + a_length);
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
            break;
        }
        if (a_cstr[i] == from0 && memcmp(&a_cstr[i], from, from_length) == 0)
        {
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
            count -= 1;
            i += from_length - 1;
            result.append(to, to + to_length);
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
        }
        else
        {
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
            result += a_cstr[i];
            //printf("%s(%d) %s\n", __FILE__, __LINE__, result.c_str());
        }
    }
    return result;
}

string string_replace(string a,
                      const char* from,
                      const char* to)
{
    return string_replace(a, from, strlen(from), to, strlen(to));
}

void test_string_replace(const char* a, const char* from, const char* to)
{
    printf("string_replace(%s, %s, %s):%s\n", a, from, to,
        string_replace(a, from, to) .c_str());
}

void test_string_replace()
{
    const char* data[] = {"", "abc", "a", "aaaa", "abcaaaababccc", "aaa", 0};
    const char* from[] = {"a", "ab", "abc", 0};
    const char* to[] = {"","a", "ab", "abc", "z", "xy", 0};
    for (size_t i = 0; data[i]; ++i)
        for (size_t j = 0; from[j]; ++j)
            for (size_t k = 0; to[k]; ++k)
                test_string_replace(data[i], from[j], to[k]);
}

string string_replace_d(const char* a, const char* from, int to_d)
{
    char to_s[65];
    sprintf(to_s, "%d", to_d);
    return string_replace(a, from, to_s);
}

int main()
{
    //test_string_replace();
char template1[] =
"PROCEDURE Fx() =\n"
"TYPE T = SET OF[0..x];\n"
"VAR a:T;\n"
"BEGIN\n"
"   FOR count := 0 TO MIN(2, x + 1) DO\n"
"       FOR offset := 0 TO MAX(0, x - count) DO\n"
"           a := T{};\n"
"           IF count > 0 THEN\n"
"               a := T{offset..offset + count - 1};\n"
"           END;\n"
"           Dump.Dump(x, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));\n"
"       END;\n"
"   END;\n"
"END Fx;\n\n";

    fputs(
"(* Test shift_right with a signed integer in the IR.\n"
"   Specifically m3front/src/misc/cg/Setrange\n"
"   This is machine-generated. Do not edit\n"
"*)\n"
"UNSAFE MODULE Main;\n"
"IMPORT Dump;\n"
"\n"
, stdout);
    for (int a = 0; a < 65; ++a)
        fputs(string_replace_d(template1, "x", a).c_str(), stdout);
    fputs("BEGIN\n", stdout);
    for (int a = 0; a < 65; ++a)
        fputs(string_replace_d("FA();\n", "A", a).c_str(), stdout);
    fputs("END Main.\n", stdout);
    return 0;
}
