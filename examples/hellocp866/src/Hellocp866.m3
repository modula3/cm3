

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

ЭКС-ГРАФ? ПЛЮШ ИЗЪЯТ. БЬЁМ ЧУЖДЫЙ ЦЕН ХВОЩ!
Экс-граф? Плюш изъят. Бьём чуждый цен хвощ!
экс-граф? плюш изъят. бьём чуждый цен хвощ!

А    а-->А    а
Б    б-->Б    б
В    в-->В    в
Г    г-->Г    г
Д    д-->Д    д
Е    е-->Е    е
Ё    ё-->Ё    ё
Ж    ж-->Ж    ж
З    з-->З    з
И    и-->И    и
Й    й-->Й    й
К    к-->К    к
Л    л-->Л    л
М    м-->М    м
Н    н-->Н    н
О    о-->О    о
П    п-->П    п
Р    р-->Р    р
С    с-->С    с
Т    т-->Т    т
У    у-->У    у
Ф    ф-->Ф    ф
Х    х-->Х    х
Ц    ц-->Ц    ц
Ч    ч-->Ч    ч
Ш    ш-->Ш    ш
Щ    щ-->Щ    щ
Ъ    ъ-->Ъ    ъ
Ы    ы-->Ы    ы
Ь    ь-->Ь    ь
Э    э-->Э    э
Ю    ю-->Ю    ю
Я    я-->Я    я

Box drawing alignment tests:                                          █
                                      ~
  ╔══╦══╗  ┌──┬──┐  ~──┬──~  ~──┬──~  ~~~~~~~  ~~~~   ~  ~ ~~~ ┌~┐    ~ ~~~~~~~
  ║┌─╨─┐║  │╔═╧═╗│  │╒═╪═╕│  │╓─~─╖│  ~┌─~─┐~  ~~~~  ~┼~~~~~┼~ ~~~    ~ ~~~~~~~
  ║│~ ~│║  │║   ║│  ││ │ ││  │║ ~ ║│  ~│ ~ │~  ~~~~   ~  ~ ~~~ └~┘    ▌ ~~~~~~~
  ╠╡ ~ ╞╣  ├╢   ╟┤  ├┼─┼─┼┤  ├╫─~─╫┤  ~~~┼~~~  ~~~~     ┌~~┐ ~ ~~~~ ~ ~ ~~~~~~~
  ║│~ ~│║  │║   ║│  ││ │ ││  │║ ~ ║│  ~│ ~ │~  ░░▒▒▓▓██ ~  ~ ~ ~  ~ ~ ~
  ║└─╥─┘║  │╚═╤═╝│  │╘═╪═╛│  │╙─~─╜│  ~└─~─┘~  ░░▒▒▓▓██ ~  ~ ~ ~  ~ ~ ~
  ╚══╩══╝  └──┴──┘  ~──┴──~  ~──┴──~  ~~~~~~~           └~~┘ ~ ~~~~ ~  ~~~▄~~~█


D:\cm3\src\examples\hellocp866>
*)

BEGIN

  IO.Put ("                                           \n");
  IO.Put ("ЭКС-ГРАФ? ПЛЮШ ИЗЪЯТ. БЬЁМ ЧУЖДЫЙ ЦЕН ХВОЩ!\n");
  IO.Put ("Экс-граф? Плюш изъят. Бьём чуждый цен хвощ!\n");
  IO.Put ("экс-граф? плюш изъят. бьём чуждый цен хвощ!\n");
  IO.Put ("                                           \n");

  
  IO.PutChar('А'); IO.Put ("    "); IO.PutChar('а'); IO.Put ("-->");  IO.PutChar(W'А'); IO.Put ("    "); IO.PutChar(W'а'); IO.Put ("\n");
  IO.PutChar('Б'); IO.Put ("    "); IO.PutChar('б'); IO.Put ("-->");  IO.PutChar(W'Б'); IO.Put ("    "); IO.PutChar(W'б'); IO.Put ("\n");
  IO.PutChar('В'); IO.Put ("    "); IO.PutChar('в'); IO.Put ("-->");  IO.PutChar(W'В'); IO.Put ("    "); IO.PutChar(W'в'); IO.Put ("\n");
  IO.PutChar('Г'); IO.Put ("    "); IO.PutChar('г'); IO.Put ("-->");  IO.PutChar(W'Г'); IO.Put ("    "); IO.PutChar(W'г'); IO.Put ("\n");
  IO.PutChar('Д'); IO.Put ("    "); IO.PutChar('д'); IO.Put ("-->");  IO.PutChar(W'Д'); IO.Put ("    "); IO.PutChar(W'д'); IO.Put ("\n");
  IO.PutChar('Е'); IO.Put ("    "); IO.PutChar('е'); IO.Put ("-->");  IO.PutChar(W'Е'); IO.Put ("    "); IO.PutChar(W'е'); IO.Put ("\n");
  IO.PutChar('Ё'); IO.Put ("    "); IO.PutChar('ё'); IO.Put ("-->");  IO.PutChar(W'Ё'); IO.Put ("    "); IO.PutChar(W'ё'); IO.Put ("\n");
  IO.PutChar('Ж'); IO.Put ("    "); IO.PutChar('ж'); IO.Put ("-->");  IO.PutChar(W'Ж'); IO.Put ("    "); IO.PutChar(W'ж'); IO.Put ("\n");
  IO.PutChar('З'); IO.Put ("    "); IO.PutChar('з'); IO.Put ("-->");  IO.PutChar(W'З'); IO.Put ("    "); IO.PutChar(W'з'); IO.Put ("\n");
  IO.PutChar('И'); IO.Put ("    "); IO.PutChar('и'); IO.Put ("-->");  IO.PutChar(W'И'); IO.Put ("    "); IO.PutChar(W'и'); IO.Put ("\n");
  IO.PutChar('Й'); IO.Put ("    "); IO.PutChar('й'); IO.Put ("-->");  IO.PutChar(W'Й'); IO.Put ("    "); IO.PutChar(W'й'); IO.Put ("\n");
  IO.PutChar('К'); IO.Put ("    "); IO.PutChar('к'); IO.Put ("-->");  IO.PutChar(W'К'); IO.Put ("    "); IO.PutChar(W'к'); IO.Put ("\n");
  IO.PutChar('Л'); IO.Put ("    "); IO.PutChar('л'); IO.Put ("-->");  IO.PutChar(W'Л'); IO.Put ("    "); IO.PutChar(W'л'); IO.Put ("\n");
  IO.PutChar('М'); IO.Put ("    "); IO.PutChar('м'); IO.Put ("-->");  IO.PutChar(W'М'); IO.Put ("    "); IO.PutChar(W'м'); IO.Put ("\n");
  IO.PutChar('Н'); IO.Put ("    "); IO.PutChar('н'); IO.Put ("-->");  IO.PutChar(W'Н'); IO.Put ("    "); IO.PutChar(W'н'); IO.Put ("\n");
  IO.PutChar('О'); IO.Put ("    "); IO.PutChar('о'); IO.Put ("-->");  IO.PutChar(W'О'); IO.Put ("    "); IO.PutChar(W'о'); IO.Put ("\n");
  IO.PutChar('П'); IO.Put ("    "); IO.PutChar('п'); IO.Put ("-->");  IO.PutChar(W'П'); IO.Put ("    "); IO.PutChar(W'п'); IO.Put ("\n");
  IO.PutChar('Р'); IO.Put ("    "); IO.PutChar('р'); IO.Put ("-->");  IO.PutChar(W'Р'); IO.Put ("    "); IO.PutChar(W'р'); IO.Put ("\n");
  IO.PutChar('С'); IO.Put ("    "); IO.PutChar('с'); IO.Put ("-->");  IO.PutChar(W'С'); IO.Put ("    "); IO.PutChar(W'с'); IO.Put ("\n");
  IO.PutChar('Т'); IO.Put ("    "); IO.PutChar('т'); IO.Put ("-->");  IO.PutChar(W'Т'); IO.Put ("    "); IO.PutChar(W'т'); IO.Put ("\n");
  IO.PutChar('У'); IO.Put ("    "); IO.PutChar('у'); IO.Put ("-->");  IO.PutChar(W'У'); IO.Put ("    "); IO.PutChar(W'у'); IO.Put ("\n");
  IO.PutChar('Ф'); IO.Put ("    "); IO.PutChar('ф'); IO.Put ("-->");  IO.PutChar(W'Ф'); IO.Put ("    "); IO.PutChar(W'ф'); IO.Put ("\n");
  IO.PutChar('Х'); IO.Put ("    "); IO.PutChar('х'); IO.Put ("-->");  IO.PutChar(W'Х'); IO.Put ("    "); IO.PutChar(W'х'); IO.Put ("\n");
  IO.PutChar('Ц'); IO.Put ("    "); IO.PutChar('ц'); IO.Put ("-->");  IO.PutChar(W'Ц'); IO.Put ("    "); IO.PutChar(W'ц'); IO.Put ("\n");
  IO.PutChar('Ч'); IO.Put ("    "); IO.PutChar('ч'); IO.Put ("-->");  IO.PutChar(W'Ч'); IO.Put ("    "); IO.PutChar(W'ч'); IO.Put ("\n");
  IO.PutChar('Ш'); IO.Put ("    "); IO.PutChar('ш'); IO.Put ("-->");  IO.PutChar(W'Ш'); IO.Put ("    "); IO.PutChar(W'ш'); IO.Put ("\n");
  IO.PutChar('Щ'); IO.Put ("    "); IO.PutChar('щ'); IO.Put ("-->");  IO.PutChar(W'Щ'); IO.Put ("    "); IO.PutChar(W'щ'); IO.Put ("\n");
  IO.PutChar('Ъ'); IO.Put ("    "); IO.PutChar('ъ'); IO.Put ("-->");  IO.PutChar(W'Ъ'); IO.Put ("    "); IO.PutChar(W'ъ'); IO.Put ("\n");
  IO.PutChar('Ы'); IO.Put ("    "); IO.PutChar('ы'); IO.Put ("-->");  IO.PutChar(W'Ы'); IO.Put ("    "); IO.PutChar(W'ы'); IO.Put ("\n");
  IO.PutChar('Ь'); IO.Put ("    "); IO.PutChar('ь'); IO.Put ("-->");  IO.PutChar(W'Ь'); IO.Put ("    "); IO.PutChar(W'ь'); IO.Put ("\n");
  IO.PutChar('Э'); IO.Put ("    "); IO.PutChar('э'); IO.Put ("-->");  IO.PutChar(W'Э'); IO.Put ("    "); IO.PutChar(W'э'); IO.Put ("\n");
  IO.PutChar('Ю'); IO.Put ("    "); IO.PutChar('ю'); IO.Put ("-->");  IO.PutChar(W'Ю'); IO.Put ("    "); IO.PutChar(W'ю'); IO.Put ("\n");
  IO.PutChar('Я'); IO.Put ("    "); IO.PutChar('я'); IO.Put ("-->");  IO.PutChar(W'Я'); IO.Put ("    "); IO.PutChar(W'я'); IO.Put ("\n");
                                                                                                            
  IO.Put ("                                                                               \n");             
  IO.Put ("Box drawing alignment tests:                                          █        \n");             
  IO.Put ("                                      ~           \n");             
  IO.Put ("  ╔══╦══╗  ┌──┬──┐  ~──┬──~  ~──┬──~  ~~~~~~~  ~~~~   ~  ~ ~~~ ┌~┐    ~ ~~~~~~~\n");             
  IO.Put ("  ║┌─╨─┐║  │╔═╧═╗│  │╒═╪═╕│  │╓─~─╖│  ~┌─~─┐~  ~~~~  ~┼~~~~~┼~ ~~~    ~ ~~~~~~~\n");             
  IO.Put ("  ║│~ ~│║  │║   ║│  ││ │ ││  │║ ~ ║│  ~│ ~ │~  ~~~~   ~  ~ ~~~ └~┘    ▌ ~~~~~~~\n");             
  IO.Put ("  ╠╡ ~ ╞╣  ├╢   ╟┤  ├┼─┼─┼┤  ├╫─~─╫┤  ~~~┼~~~  ~~~~     ┌~~┐ ~ ~~~~ ~ ~ ~~~~~~~\n");             
  IO.Put ("  ║│~ ~│║  │║   ║│  ││ │ ││  │║ ~ ║│  ~│ ~ │~  ░░▒▒▓▓██ ~  ~ ~ ~  ~ ~ ~        \n");             
  IO.Put ("  ║└─╥─┘║  │╚═╤═╝│  │╘═╪═╛│  │╙─~─╜│  ~└─~─┘~  ░░▒▒▓▓██ ~  ~ ~ ~  ~ ~ ~        \n");             
  IO.Put ("  ╚══╩══╝  └──┴──┘  ~──┴──~  ~──┴──~  ~~~~~~~           └~~┘ ~ ~~~~ ~  ~~~▄~~~█\n");             
  IO.Put ("                                                                               \n");             
                                                                                                            
END Hellocp866.                                                                                             
                                                                                                            
(* Don't forget to include the module name in the last "END" in your
   program. *)

                                                                                                            