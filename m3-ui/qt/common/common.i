
//include the basic c typemaps

%include "cmaps.i"

//defines for aliases in qt

#define qreal double
#define uint unsigned int
//#define uchar unsigned char
#define uint unsigned int
#define ushort unsigned short
#define ulong unsigned long
#define qlonglong  long long
#define qulonglong  unsigned long long

#define qint64 unsigned long int
#define WId int


// Common typemaps

//the default self parm is a VAR which is wrong this overrides it in raw and wrap
%typemap("m3wrapinmode")       SWIGTYPE * ""
%typemap("m3rawinmode")        SWIGTYPE * ""

//override the const for methods putting in readonly
%typemap("m3rawinmode")    const SWIGTYPE * ""
%typemap("m3wrapinmode")   const SWIGTYPE * ""

//this is raw should be untraced
//%typemap(m3rawrettype)       SWIGTYPE * "REF $1_basetype"
//this is wrap should be traced but not a ref
%typemap("m3wraprettype")      SWIGTYPE * "$1_basetype"

//same for raw not a ref
%typemap("m3rawrettype")       SWIGTYPE * "$1_basetype"

//put the LOOPHOLE(self.cxxObj,ADDRESS) in the call to the raw

//the self var needs a couple of maps
%typemap("m3wrapargvar")       SWIGTYPE *self  %{selfAdr: ADDRESS := LOOPHOLE($1_name.cxxObj,ADDRESS);%}
%typemap("m3wrapargraw")       SWIGTYPE *self  %{selfAdr%}

//cope with the constructor conversion is this correct just for self parms
%typemap("m3wrapretraw")       SWIGTYPE * %{result%}
/*
  used to be this but added m3rawrettype above to remove the untraced ref from raw
%typemap("m3wrapretvar")    SWIGTYPE * %{ret := NEW($1_basetype);
result : UNTRACED REF rectangleRaw.$1_basetype;%};
*/

//no need to allocate a new type we use the method self
//%typemap("m3wrapretvar")     SWIGTYPE * %{ret := NEW($1_basetype);

%typemap("m3wrapretconv")      SWIGTYPE *  %{self%} //was ret see above

%typemap("m3wrapretcheck")     SWIGTYPE *  %{
  self.cxxObj := result;
  EVAL WeakRef.FromRef(self,Cleanup_$1_basetype);
%};

//typemap for constructors needing local return var from raw
%typemap("m3wrapretvar")       SWIGTYPE * %{result : ADDRESS;%}


//------  end constructor typemaps


//define macro for enums

//original ones
/*
%define EnumMaps(enumIN,M3Enum,RunCheck)

%typemap("m3rawintype")    enumIN   %{C.int%} //removes comment from modula3.swg
%typemap("m3rawrettype")   enumIN   %{C.int%} // not really necessary
%typemap("m3wraprettype")  enumIN   %{M3Enum%}
%typemap("m3wrapretvar")   enumIN   %{ret:INTEGER; result : M3Enum;%}
%typemap("m3wrapretraw")   enumIN   %{ret%};
%typemap("m3wrapretconv")  enumIN   %{result%}
%typemap("m3wrapretcheck") enumIN   %{result := VAL(ret,M3Enum);  %};
%typemap("m3wrapintype")   enumIN   %{M3Enum%}
%typemap("m3wrapargraw")   enumIN   %{ORD($1_name)%}
%typemap("m3wrapouttype")  enumIN   %{M3Enum%} //fixes .i3 not return correct type
%typemap(in)               enumIN = enum SWIGTYPE; //fixes wrong typemap in wrap.cxx

#if RunCheck > 0
%typemap("m3wrapenumcheck") enumIN  %{M3Enum%};
#endif

%enddef

//if an enum is defined in another module need an import

%define EnumImport(enumIN,M3Enum,importMod)

%typemap("m3wrapintype:import") enumIN  %{importMod M3Enum%}
%typemap("m3wrapretvar:import") enumIN  %{importMod M3Enum%}

%enddef
*/

//New Macros

%define EnumMaps(enumPrefix, enumIn, RunCheck)

%typemap("m3rawintype")    enumPrefix::enumIn   %{C.int%} //removes comment from modula3.swg
%typemap("m3rawrettype")   enumPrefix::enumIn   %{C.int%} // not really necessary
%typemap("m3wraprettype")  enumPrefix::enumIn   %{enumIn%}
%typemap("m3wrapretvar")   enumPrefix::enumIn   %{ret:INTEGER; result : enumIn;%}
%typemap("m3wrapretraw")   enumPrefix::enumIn   %{ret%};
%typemap("m3wrapretconv")  enumPrefix::enumIn   %{result%}
%typemap("m3wrapretcheck") enumPrefix::enumIn   %{result := VAL(ret,enumIn);  %};
%typemap("m3wrapintype")   enumPrefix::enumIn   %{enumIn%}
%typemap("m3wrapargraw")   enumPrefix::enumIn   %{ORD($1_name)%}
%typemap("m3wrapouttype")  enumPrefix::enumIn   %{enumIn%} //fixes .i3 not return correct type
%typemap(in)               enumPrefix::enumIn = enum SWIGTYPE; //fixes wrong typemap in wrap.cxx

#if RunCheck > 0
%typemap("m3wrapenumcheck")  enumPrefix::enumIn  %{enumIn%}
#endif

%enddef

//if an enum is defined in another module need an import

%define EnumImport(enumPrefix,enumIn,importMod,RunCheck)

#if RunCheck > 0
%typemap("m3wrapintype:import") enumPrefix::enumIn  %{importMod enumIn&enumIn##_ErrSet%}
%typemap("m3wrapretvar:import") enumPrefix::enumIn  %{importMod enumIn&enumIn##_ErrSet%}
#else
%typemap("m3wrapintype:import") enumPrefix::enumIn  %{importMod enumIn%}
%typemap("m3wrapretvar:import") enumPrefix::enumIn  %{importMod enumIn%}
#endif

%enddef


//flags are bitmaps defined on enums the m3 base type is INTEGER
//but we need tmaps for parameters

%define EnumFlags(enumIN,M3Enum)

%typemap("m3rawintype")    enumIN   %{C.int%} //removes comment from modula3.swg
%typemap("m3rawrettype")   enumIN   %{C.int%} // not really necessary
%typemap("m3wraprettype")  enumIN   %{M3Enum%}
%typemap("m3wrapretvar")   enumIN   %{ret:INTEGER; result : M3Enum;%}
%typemap("m3wrapretraw")   enumIN   %{ret%};
%typemap("m3wrapretconv")  enumIN   %{result%}
%typemap("m3wrapretcheck") enumIN   %{result := VAL(ret,M3Enum);  %};
%typemap("m3wrapintype")   enumIN   %{M3Enum%}
%typemap("m3wrapargraw")   enumIN   %{ORD($1_name)%}
%typemap("m3wrapouttype")  enumIN   %{M3Enum%} //fixes .i3 not return correct type
%typemap(in)               enumIN = enum SWIGTYPE; //fixes wrong typemap in wrap.cxx

%enddef

//if a flag is defined in another module we need an import

%define EnumFlagsImport(enumIN,M3Enum,importMod)

%typemap("m3wrapintype:import") enumIN  %{importMod M3Enum%}
%typemap("m3wrapretvar:import") enumIN  %{importMod M3Enum%}

%enddef


//Define a set of tmaps which can be used in an apply
//Self returns are used where a class has a return type of itself
//we dont want to crash collecting ourselves twice
/*
Could do away with the statcheck in the modula3.cxx which is a real kludge
since we know how to apply a typemap to a static return member
and indeed to a static parm??
as in %apply SelfReturn   {QWidget *QWidget::mouseGrabber};
so we could have 2 sets of SelfReturns one for member functions and one for statics
and its only the m2wrapretcheck tmap which changes
*/
%typemap("ctype")          SelfReturn       %{$1_basetype *%}  //class returns are pointers
%typemap("m3rawrettype")   SelfReturn       %{ADDRESS%}
%typemap("m3wraprettype")  SelfReturn       %{$1_basetype%}
%typemap("m3wrapretvar")   SelfReturn       %{ret:ADDRESS; result : $1_basetype;%}
%typemap("m3wrapretraw")   SelfReturn       %{ret%};
%typemap("m3wrapretconv")  SelfReturn       %{result%}
//%typemap("m3wrapargvar")   SelfReturn       %{selfAdr :=  LOOPHOLE($1_name.cxxObj,ADDRESS);%}

//the modula3 swig code strips off the comments in the case of statcheck
//was this IF ISTYPE(result,$1_basetype) AND ret = selfAdr
//but I think selfAdr was changed at one point to arg1tmp
//m3wrapargvar in classin was added for some reason and it overrides the selfaddr
//change the current ret = arg1tmp back to selfAdr as it should be
//for qcolor etc and see which modules fail later. I thinkg
//maybe a question of ordering ie that there is a classin after a selfreturn
//check it out
%typemap("m3wrapretcheck",statcheck="1") SelfReturn       %{
(*IF ISTYPE(result,$1_basetype) AND ret = selfAdr THEN
  result := LOOPHOLE(self,$1_basetype);
ELSE*)
  result := NEW($1_basetype);
  result.cxxObj := ret;
  result.destroyCxx();
(*END;*)
%};

//test of a set of tmaps for a static self return
%typemap("ctype")          StaticSelfReturn     %{$1_basetype *%}  //class returns are pointers
%typemap("m3rawrettype")   StaticSelfReturn     %{ADDRESS%}
%typemap("m3wraprettype")  StaticSelfReturn     %{$1_basetype%}
%typemap("m3wrapretvar")   StaticSelfReturn     %{ret:ADDRESS; result : $1_basetype;%}
%typemap("m3wrapretraw")   StaticSelfReturn     %{ret%};
%typemap("m3wrapretconv")  StaticSelfReturn     %{result%}

%typemap("m3wrapretcheck") StaticSelfReturn     %{
  result := NEW($1_basetype);
  result.cxxObj := ret;
  result.destroyCxx();
%};
//end static test

//Define a set of tmaps which can be used in an apply
//Class returns (normal case) are used where a class has
//a return type different from itself so no checking if it can be destroyed

%typemap("ctype")          ClassReturn        %{$1_basetype *%}  //class returns are pointers
%typemap("m3rawrettype")   ClassReturn        %{ADDRESS%}
%typemap("m3wraprettype")  ClassReturn        %{$1_basetype%}
%typemap("m3wrapretvar")   ClassReturn        %{ret:ADDRESS; result : $1_basetype;%}
%typemap("m3wrapretraw")   ClassReturn        %{ret%};
%typemap("m3wrapretconv")  ClassReturn        %{result%}
%typemap("m3wrapretcheck") ClassReturn        %{
  result := NEW($1_basetype);
  result.cxxObj := ret;
  result.destroyCxx();
%};

//this seems to fix some compile errors in cxx but stuffs up others
%typemap("out",optimal="1") SWIGTYPE %{*($&1_ltype*)&$result = new $1_ltype((const $1_ltype &)$1);%}

//Define a set of tmaps which can be used in an apply
//ClassIn tmaps are for parameters for a method call

//%typemap("ctype")          ClassIn            %{$1_basetype *%}  // not really needed
%typemap("m3rawintype")    ClassIn            %{ADDRESS%}
%typemap("m3rawinmode")    ClassIn            %{%}
%typemap("m3wrapinmode")   ClassIn            %{%}
%typemap("m3wrapargvar")   ClassIn            %{$1tmp :=  LOOPHOLE($1_name.cxxObj,ADDRESS);%}
%typemap("m3wrapargraw")   ClassIn            %{$1tmp%}


//Some special tmaps for QString which we want to convert to TEXT
/*
Notes
  I have made some changes
  added ctype for qstring which changes the self and other parms
  to a pointer in the wrap.cxx
  also have reverted to the %typemap("in")  const SWIGTYPE &   %{$1 = *($1_basetype **)&$input;%} which is the same as in modula3.swg except for the comments so
  maybe fix that and remove the in tmap below then test it all again
  also ignored a constructor in qstring.i which was taking a string as parm
  since we already had one the ascii one with ch as a parm
  so basically the problem was that we didnt have a ctype for qstring as we had for
  everything else and the in tmap sort of disguised the problem
*/
//QString input
%typemap("ctype")          QString            %{QString *%}
%typemap("m3wrapinmode")   QString            %{%}
%typemap("m3rawinmode")    QString            %{%}
%typemap("m3wrapintype",impleonly="1")   QString            %{TEXT%}
%typemap("m3rawintype")    QString            %{ADDRESS%}
%typemap("m3wrapargraw")   QString            %{$1tmp%}
%typemap("m3wrapretraw")   QString            %{ret%}

//the init name here is special but very general and should be something
//like initQString and change the QString.i file in m3core to agree since
//what i did was rename one of the widget constructors to init and it
//interferred with this init and segv in the program
%typemap("m3wrapargvar")   QString            %{qstr_$1_name := NEW(QString).initQString($1_name);
$1tmp :=  LOOPHOLE(qstr_$1_name.cxxObj,ADDRESS);%}


//QString return
//dont use qstr.destroyCxx(); after tolocal8bit getting glibc double free
%typemap("m3rawrettype")    QString           %{ADDRESS%}
%typemap("m3wraprettype")   QString           %{TEXT%}
%typemap("m3wrapouttype")   QString           %{TEXT%}
%typemap("m3wrapretvar",impleonly="1")    QString           %{ret : ADDRESS;
qstr := NEW(QString);
ba : QByteArray;
result : TEXT;%};
%typemap("m3wrapretconv")   QString           %{result%}
%typemap("m3wrapretcheck")  QString           %{
  qstr.cxxObj := ret;
  ba := qstr.toLocal8Bit();
  result := ba.data();
%}

//also commented out the qstr.destroyCxx since getting mem corruption think
//the qstr is being destroyed in to local8bit but needs testing

//Did have this originally
//ba.destroyCxx(); destroyed in tolocal8bit. Raises assert in collector

%apply QString{const QString &};
%apply QString{QString &};
//if qstring gets this apply it barfs - fixme
//%apply QString{QString *};

//this import useful but not in qstring itself needs fix
//import qstring but stubbed at moment see below
//%typemap("m3wrapintype:import")  QString  %{QtString $1_basetype%}

//%typemap("in")  QObj      %{$1 = ($1_basetype *)&$input;%}

//this works so test if can do away with QObj
//%typemap("in")  const SWIGTYPE &   %{$1 = ($1_basetype *)&$input;%}
//think should be this since does not produce correct results actually i must have
//been getting segv with this before see comment below
%typemap("in")  const SWIGTYPE &   %{$1 = *($1_basetype **)&$input;%}


//The default cast in the wrap.cxx is  eg arg1 = *(QPoint **)&p; but this causes segv
//and it should be   arg1 = (QPoint *)&p; to work correctly
//so need an apply for every class that could be a ref input
/*
%apply QObj {const QPoint    &};
*/

%define DoType(typeIN,importMod)

%typemap("ctype") typeIN     %{typeIN *%}
%apply ClassIn     {const typeIN &};
%apply ClassIn     {typeIN &};
%apply ClassIn     {typeIN};
%apply ClassIn     {typeIN *};
%apply ClassReturn {typeIN};
%apply ClassReturn {typeIN *};
%apply ClassReturn {typeIN &};
%apply ClassReturn {const typeIN &};

%typemap("m3wrapintype:import") const typeIN & %{importMod  $1_basetype%}
%typemap("m3wrapintype:import")       typeIN   %{importMod  $1_basetype%}
%typemap("m3wrapintype:import")       typeIN * %{importMod  $1_basetype%}
%typemap("m3wrapretvar:import")       typeIN   %{importMod  $1_basetype%}
%typemap("m3wrapretvar:import")       typeIN * %{importMod  $1_basetype%}
%typemap("m3wrapretvar:import")       typeIN & %{importMod  $1_basetype%}
%typemap("m3wrapretvar:import") const typeIN & %{importMod  $1_basetype%}

%enddef

