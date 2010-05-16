#! /usr/bin/env python

makefile = open("m3makefile", "w")
main = open("Main.m3", "w")

main.write("MODULE Main EXPORTS ")

for a in range(1,201):
  open("I" + str(a) + ".i3", "w").write("INTERFACE I" + str(a) + ";END I" + str(a) + ".\n")  
  makefile.write("interface(\"I" + str(a) + "\")\n");
  main.write("I" + str(a) + ",");
  if (a % 10) == 9:
    main.write("\n")

makefile.write("\nimport(\"m3core\")\nimplementation(\"Main\")\nprogram(\"a\")\n""")
main.write("\nMain;BEGIN END Main.\n")
