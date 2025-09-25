

MODULE Hellocp866 EXPORTS Main;
(* Each module must have a name, which is declared in the "MODULE"
   statement. By convention, the main module for an executable 
   program exports the interface "Main", as does the "Hello" module here.

   Each module can also import interfaces exported by other modules.
   This is how you reuse code from libraries or your own modules. 
   Here, we have imported interface "IO" which is a simple 
   input/output interface.

   From the browser, you can learn what the imported interfaces do
   by following the link associated with their name. *)

IMPORT IO;

(* The main body of a module or the initialization section includes
   statements that are executed at the begining of the program. 
   In this case, we are the main module, and all we do is print 
   "Hello World!" on standard output.

    Demo of using:

D:\cm3\src\examples\hellocp866>chcp 866
Active code page: 866

D:\cm3\src\examples\hellocp866>.\AMD64_NT\hellocp866.exe

���-����? ���� �����. ���� ������ ��� ����!
���-���? ���� �����. ��� �㦤� 業 墮�!
��-���? ���� �����. ��� �㦤� 業 墮�!

�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �
�    �-->�    �

Box drawing alignment tests:                                          �
                                      ~
  �����ͻ  �����Ŀ  ~�����~  ~�����~  ~~~~~~~  ~~~~   ~  ~ ~~~ �~�    ~ ~~~~~~~
  ����Ŀ�  ����ͻ�  ����͸�  ���~ķ�  ~��~Ŀ~  ~~~~  ~�~~~~~�~ ~~~    ~ ~~~~~~~
  ��~ ~��  ��   ��  �� � ��  �� ~ ��  ~� ~ �~  ~~~~   ~  ~ ~~~ �~�    � ~~~~~~~
  ̵ ~ ƹ  ö   Ǵ  �����Ŵ  ���~�״  ~~~�~~~  ~~~~     �~~� ~ ~~~~ ~ ~ ~~~~~~~
  ��~ ~��  ��   ��  �� � ��  �� ~ ��  ~� ~ �~  �������� ~  ~ ~ ~  ~ ~ ~
  �����ٺ  ����ͼ�  ����;�  ���~Ľ�  ~��~��~  �������� ~  ~ ~ ~  ~ ~ ~
  �����ͼ  �������  ~�����~  ~�����~  ~~~~~~~           �~~� ~ ~~~~ ~  ~~~�~~~�


D:\cm3\src\examples\hellocp866>
*)

BEGIN

  IO.Put ("                                           \n");
  IO.Put ("���-����? ���� �����. ���� ������ ��� ����!\n");
  IO.Put ("���-���? ���� �����. ��� �㦤� 業 墮�!\n");
  IO.Put ("��-���? ���� �����. ��� �㦤� 業 墮�!\n");
  IO.Put ("                                           \n");

  
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
  IO.PutChar('�'); IO.Put ("    "); IO.PutChar('�'); IO.Put ("-->");  IO.PutChar(W'�'); IO.Put ("    "); IO.PutChar(W'�'); IO.Put ("\n");
                                                                                                            
  IO.Put ("                                                                               \n");             
  IO.Put ("Box drawing alignment tests:                                          �        \n");             
  IO.Put ("                                      ~           \n");             
  IO.Put ("  �����ͻ  �����Ŀ  ~�����~  ~�����~  ~~~~~~~  ~~~~   ~  ~ ~~~ �~�    ~ ~~~~~~~\n");             
  IO.Put ("  ����Ŀ�  ����ͻ�  ����͸�  ���~ķ�  ~��~Ŀ~  ~~~~  ~�~~~~~�~ ~~~    ~ ~~~~~~~\n");             
  IO.Put ("  ��~ ~��  ��   ��  �� � ��  �� ~ ��  ~� ~ �~  ~~~~   ~  ~ ~~~ �~�    � ~~~~~~~\n");             
  IO.Put ("  ̵ ~ ƹ  ö   Ǵ  �����Ŵ  ���~�״  ~~~�~~~  ~~~~     �~~� ~ ~~~~ ~ ~ ~~~~~~~\n");             
  IO.Put ("  ��~ ~��  ��   ��  �� � ��  �� ~ ��  ~� ~ �~  �������� ~  ~ ~ ~  ~ ~ ~        \n");             
  IO.Put ("  �����ٺ  ����ͼ�  ����;�  ���~Ľ�  ~��~��~  �������� ~  ~ ~ ~  ~ ~ ~        \n");             
  IO.Put ("  �����ͼ  �������  ~�����~  ~�����~  ~~~~~~~           �~~� ~ ~~~~ ~  ~~~�~~~�\n");             
  IO.Put ("                                                                               \n");             
                                                                                                            
END Hellocp866.                                                                                             
                                                                                                            
(* Don't forget to include the module name in the last "END" in your
   program. *)

                                                                                                            