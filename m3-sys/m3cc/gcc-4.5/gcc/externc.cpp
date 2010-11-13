#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <string>
using namespace std;
typedef unsigned UINT;

static void
read_entire_file (FILE* file, vector<char>& buffer)
{
  size_t buffer_size = { 0 };
  size_t bytes_read = { 0 };
  size_t total_bytes_read = { 0 };
  size_t bytes_to_read = { 0 };

  while (bytes_read == bytes_to_read)
  {
    buffer_size += buffer_size ? buffer_size : 0x10000;
    buffer.resize(buffer_size);
    bytes_to_read = (buffer_size - total_bytes_read);
    bytes_read = fread (&buffer[total_bytes_read],
                        1,
                        buffer_size - total_bytes_read,
                        file);
    total_bytes_read += bytes_read;
  }
  /* shrink it down (or up by 1) and add terminal nul */
  buffer.resize(total_bytes_read + 1);
  buffer[total_bytes_read] = 0;
}

char* find_first(char* s, char* t)
{
  return strstr(s, t);
}

char* find_next(char* s, char* t)
{
  return s ? strstr(s + 1, t) : s;
}

char* find_last(char* s, char* t)
{
  char* a = { 0 };
  char* b = { 0 };
  for (a = find_first(s, t); a; a = find_next(b = a, t)) { }
  return b;
}

int main()
{
  vector<char> buffer;
  DIR* a = opendir(".");
  struct dirent* b = { 0 };
  FILE* file = { 0 };
  if (!a)
    return 1;
  system("mkdir out");
  while ((b = readdir(a)))
  {
    if (file)
      fclose(file);
    file = 0;
    char* name = b->d_name;
    char* dot = strchr(name, '.');
    if (!dot)
      continue;
    bool c = (strcmp(dot, ".c") == 0);
    bool h = (strcmp(dot, ".h") == 0);
    if (!c && !h)
      continue;
    if (h && (strstr(name, "system") || strstr(name, "unwind")))
      continue;
    FILE* file = fopen(name, "rb");
    if (!file)
      continue;
    read_entire_file(file, buffer);
    fclose(file);
    file = 0;
    if (buffer.size() < 1)
      continue;

    char* p = &buffer[0];
    string original = p;
    char* q = p;
    vector<string> lines;
    for (; *p; p += 1)
    {
      if (*p == '\n')
      {
        lines.push_back(string(q, p - q + 1));
        q = p + 1;
      }
    }
    if (p != q)
      lines.push_back(string(q, p - q));
      
    bool seen_if = { 0 };
    bool seen_define = { 0 };
    bool seen_endif = { 0 };
    bool seen_include = { 0 };
    size_t x_if = { 0 };
    size_t x_define = { 0 };
    size_t x_endif = { 0 };
    size_t x_include = { 0 };
    size_t i = { 0 };
    bool skipfile = false;
    bool mod3 = { 0 };

    for (i = 0; i < lines.size(); ++i)
    {
      const char* s = lines[i].c_str();
      if (strstr(s, "Modula-3") && strstr(s, "modified") && strstr(s, "/*") && strstr(s, "*/"))
        mod3 = true;
      if (strstr(s, "__cplusplus") || strstr(s, "extern \"C)"))
      {
        skipfile = true;
        break;
      }
      if (h && !seen_if && strncmp(s, "#if", 3) == 0 && !strstr(s, "ENABLE_"))
      {
        x_if = i;
        seen_if = true;
      }
      if (h && !seen_define && strncmp(s, "#define ", 8) == 0 && !strstr(s, "assert"))
      {
        x_define = i;
        seen_define = true;
      }
      if (h && strncmp(s, "#endif", 6) == 0)
      {
        x_endif = i;
        seen_endif = true;
      }
      if (strncmp(s, "#include ", 9) == 0 && !strstr(s, "gt-") && !strstr(s, "gtype") && !strstr(s, "unwind.inc"))
      {
        x_include = i;
        seen_include = true;
      }
    }

    if (skipfile)
    {
      printf("%s: skipping\n", name);
      continue;
    }

    if (h && (!seen_if || !seen_define || !seen_endif))
      printf("warning: %s: #if/#define/#endif not seen\n", name);

    if (h && seen_if && seen_define && (x_if + 1) != x_define)
      printf("warning: %s: #if/#define not adjacent\n", name);
      
    if (!seen_if || !seen_define || seen_include || (x_if + 1) != x_define)
      seen_if = seen_define = false;

    FILE* outfile = fopen(("out/" + string(name)).c_str(), "wb");

    printf("%s: if:%u, define:%u endif:%u include:%u\n",
           name, (UINT)x_if, (UINT)x_define, (UINT)x_endif, (UINT)x_include);

    if (!mod3)
      fputs("/* Modula-3: modified */\n", outfile);
    
    for (i = 0; i < lines.size(); ++i)
    {
      const char* format = "%s";
      if (h && i == 0 && !seen_if && !seen_include)
        format = "\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n%s";
      else if ((seen_include && i == x_include) || (seen_define && i == x_define))
        format = "%s\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";
      else if (seen_endif && i == x_endif)
        format = "\n#ifdef __cplusplus\n} /* extern \"C\" */\n#endif\n\n%s";
      fprintf(outfile, format, lines[i].c_str());
    }
    if (!seen_endif && !seen_if && !seen_define)
      fprintf(outfile, "\n\n#ifdef __cplusplus\n} /* extern \"C\" */\n#endif\n");
    fclose(outfile);
  }
  return 0;
}
