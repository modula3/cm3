/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * This file is not intended to be easily readable and contains a number of
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG
 * interface file instead.
 * ----------------------------------------------------------------------------- */

#define SWIGMODULA3


#ifdef __cplusplus
/* SwigValueWrapper is described in swig.swg */
template<typename T> class SwigValueWrapper {
  struct SwigMovePointer {
    T *ptr;
    SwigMovePointer(T *p) : ptr(p) { }
    ~SwigMovePointer() { delete ptr; }
    SwigMovePointer& operator=(SwigMovePointer& rhs) { T* oldptr = ptr; ptr = 0; delete oldptr; ptr = rhs.ptr; rhs.ptr = 0; return *this; }
  } pointer;
  SwigValueWrapper& operator=(const SwigValueWrapper<T>& rhs);
  SwigValueWrapper(const SwigValueWrapper<T>& rhs);
public:
  SwigValueWrapper() : pointer(0) { }
  SwigValueWrapper& operator=(const T& t) { SwigMovePointer tmp(new T(t)); pointer = tmp; return *this; }
  operator T&() const { return *pointer.ptr; }
  T *operator&() { return pointer.ptr; }
};

template <typename T> T SwigValueInit() {
  return T();
}
#endif

/* -----------------------------------------------------------------------------
 *  This section contains generic SWIG labels for method/variable
 *  declarations/attributes, and other compiler dependent labels.
 * ----------------------------------------------------------------------------- */

/* template workaround for compilers that cannot correctly implement the C++ standard */
#ifndef SWIGTEMPLATEDISAMBIGUATOR
# if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x560)
#  define SWIGTEMPLATEDISAMBIGUATOR template
# elif defined(__HP_aCC)
/* Needed even with `aCC -AA' when `aCC -V' reports HP ANSI C++ B3910B A.03.55 */
/* If we find a maximum version that requires this, the test would be __HP_aCC <= 35500 for A.03.55 */
#  define SWIGTEMPLATEDISAMBIGUATOR template
# else
#  define SWIGTEMPLATEDISAMBIGUATOR
# endif
#endif

/* inline attribute */
#ifndef SWIGINLINE
# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#   define SWIGINLINE inline
# else
#   define SWIGINLINE
# endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
# if defined(__GNUC__)
#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#     define SWIGUNUSED __attribute__ ((__unused__))
#   else
#     define SWIGUNUSED
#   endif
# elif defined(__ICC)
#   define SWIGUNUSED __attribute__ ((__unused__))
# else
#   define SWIGUNUSED
# endif
#endif

#ifndef SWIG_MSC_UNSUPPRESS_4505
# if defined(_MSC_VER)
#   pragma warning(disable : 4505) /* unreferenced local function has been removed */
# endif
#endif

#ifndef SWIGUNUSEDPARM
# ifdef __cplusplus
#   define SWIGUNUSEDPARM(p)
# else
#   define SWIGUNUSEDPARM(p) p SWIGUNUSED
# endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
# define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* exporting methods */
#if defined(__GNUC__)
#  if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#    ifndef GCC_HASCLASSVISIBILITY
#      define GCC_HASCLASSVISIBILITY
#    endif
#  endif
#endif

#ifndef SWIGEXPORT
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   if defined(STATIC_LINKED)
#     define SWIGEXPORT
#   else
#     define SWIGEXPORT __declspec(dllexport)
#   endif
# else
#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#     define SWIGEXPORT __attribute__ ((visibility("default")))
#   else
#     define SWIGEXPORT
#   endif
# endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   define SWIGSTDCALL __stdcall
# else
#   define SWIGSTDCALL
# endif
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
# define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
# define _SCL_SECURE_NO_DEPRECATE
#endif

/* Deal with Apple's deprecated 'AssertMacros.h' from Carbon-framework */
#if defined(__APPLE__) && !defined(__ASSERT_MACROS_DEFINE_VERSIONS_WITHOUT_UNDERSCORES)
# define __ASSERT_MACROS_DEFINE_VERSIONS_WITHOUT_UNDERSCORES 0
#endif

/* Intel's compiler complains if a variable which was never initialised is
 * cast to void, which is a common idiom which we use to indicate that we
 * are aware a variable isn't used.  So we just silence that warning.
 * See: https://github.com/swig/swig/issues/192 for more discussion.
 */
#ifdef __INTEL_COMPILER
# pragma warning disable 592
#endif



#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include <QtCore/qsignalmapper.h>


#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT QSignalMapper * New_QSignalMapper0(QObject * parent) {
  QObject *arg1 = (QObject *) 0 ;
  QSignalMapper *result = 0 ;
  QSignalMapper * cresult ;
  
  arg1 = *(QObject **)&parent; 
  result = (QSignalMapper *)new QSignalMapper(arg1);
  *(QSignalMapper **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QSignalMapper * New_QSignalMapper1() {
  QSignalMapper *result = 0 ;
  QSignalMapper * cresult ;
  
  result = (QSignalMapper *)new QSignalMapper();
  *(QSignalMapper **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void Delete_QSignalMapper(QSignalMapper * self) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  delete arg1;
}


SWIGEXPORT void QSignalMapper_setMapping(QSignalMapper * self, QObject * sender, int id) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  int arg3 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  arg3 = (int)id; 
  (arg1)->setMapping(arg2,arg3);
}


SWIGEXPORT void QSignalMapper_setMapping1(QSignalMapper * self, QObject * sender, QString * text) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  QString *arg3 = 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  arg3 = *(QString **)&text;
  (arg1)->setMapping(arg2,(QString const &)*arg3);
}


SWIGEXPORT void QSignalMapper_setMapping2(QSignalMapper * self, QObject * sender, QWidget * widget) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  QWidget *arg3 = (QWidget *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  arg3 = *(QWidget **)&widget; 
  (arg1)->setMapping(arg2,arg3);
}


SWIGEXPORT void QSignalMapper_setMapping3(QSignalMapper * self, QObject * sender, QObject * object) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  QObject *arg3 = (QObject *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  arg3 = *(QObject **)&object; 
  (arg1)->setMapping(arg2,arg3);
}


SWIGEXPORT void QSignalMapper_removeMappings(QSignalMapper * self, QObject * sender) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  (arg1)->removeMappings(arg2);
}


SWIGEXPORT QObject * QSignalMapper_mapping(QSignalMapper const * self, int id) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  int arg2 ;
  QObject *result = 0 ;
  QObject * cresult ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = (int)id; 
  result = (QObject *)((QSignalMapper const *)arg1)->mapping(arg2);
  *(QObject **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QObject * QSignalMapper_mapping1(QSignalMapper const * self, QString * text) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QString *arg2 = 0 ;
  QObject *result = 0 ;
  QObject * cresult ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QString **)&text;
  result = (QObject *)((QSignalMapper const *)arg1)->mapping((QString const &)*arg2);
  *(QObject **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QObject * QSignalMapper_mapping2(QSignalMapper const * self, QWidget * widget) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QWidget *arg2 = (QWidget *) 0 ;
  QObject *result = 0 ;
  QObject * cresult ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QWidget **)&widget; 
  result = (QObject *)((QSignalMapper const *)arg1)->mapping(arg2);
  *(QObject **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QObject * QSignalMapper_mapping3(QSignalMapper const * self, QObject * object) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  QObject *result = 0 ;
  QObject * cresult ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&object; 
  result = (QObject *)((QSignalMapper const *)arg1)->mapping(arg2);
  *(QObject **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void QSignalMapper_map(QSignalMapper * self) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  (arg1)->map();
}


SWIGEXPORT void QSignalMapper_map1(QSignalMapper * self, QObject * sender) {
  QSignalMapper *arg1 = (QSignalMapper *) 0 ;
  QObject *arg2 = (QObject *) 0 ;
  
  arg1 = *(QSignalMapper **)&self; 
  arg2 = *(QObject **)&sender; 
  (arg1)->map(arg2);
}


#ifdef __cplusplus
}
#endif

