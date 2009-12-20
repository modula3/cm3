while (<>)
{
    s/\s//g;
    print("#ifdef $_\nX($_)\n#endif\n");
}
