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


#include <QtGui/qmdisubwindow.h>


#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT QMdiSubWindow * New_QMdiSubWindow0(QWidget * parent, int flags) {
  QWidget *arg1 = (QWidget *) 0 ;
  Qt::WindowFlags arg2 ;
  QMdiSubWindow *result = 0 ;
  QMdiSubWindow * cresult ;
  
  arg1 = *(QWidget **)&parent; 
  arg2 = (Qt::WindowFlags)flags; 
  result = (QMdiSubWindow *)new QMdiSubWindow(arg1,arg2);
  *(QMdiSubWindow **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QMdiSubWindow * New_QMdiSubWindow1(QWidget * parent) {
  QWidget *arg1 = (QWidget *) 0 ;
  QMdiSubWindow *result = 0 ;
  QMdiSubWindow * cresult ;
  
  arg1 = *(QWidget **)&parent; 
  result = (QMdiSubWindow *)new QMdiSubWindow(arg1);
  *(QMdiSubWindow **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QMdiSubWindow * New_QMdiSubWindow2() {
  QMdiSubWindow *result = 0 ;
  QMdiSubWindow * cresult ;
  
  result = (QMdiSubWindow *)new QMdiSubWindow();
  *(QMdiSubWindow **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void Delete_QMdiSubWindow(QMdiSubWindow * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  delete arg1;
}


SWIGEXPORT QSize * QMdiSubWindow_sizeHint(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QSize * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  *(QSize **)&cresult = new QSize((const QSize &)((QMdiSubWindow const *)arg1)->sizeHint());
  return cresult;
}


SWIGEXPORT QSize * QMdiSubWindow_minimumSizeHint(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QSize * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  *(QSize **)&cresult = new QSize((const QSize &)((QMdiSubWindow const *)arg1)->minimumSizeHint());
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_setWidget(QMdiSubWindow * self, QWidget * widget) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QWidget *arg2 = (QWidget *) 0 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = *(QWidget **)&widget; 
  (arg1)->setWidget(arg2);
}


SWIGEXPORT QWidget * QMdiSubWindow_widget(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QWidget *result = 0 ;
  QWidget * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (QWidget *)((QMdiSubWindow const *)arg1)->widget();
  *(QWidget **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QWidget * QMdiSubWindow_maximizedButtonsWidget(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QWidget *result = 0 ;
  QWidget * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (QWidget *)((QMdiSubWindow const *)arg1)->maximizedButtonsWidget();
  *(QWidget **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QWidget * QMdiSubWindow_maximizedSystemMenuIconWidget(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QWidget *result = 0 ;
  QWidget * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (QWidget *)((QMdiSubWindow const *)arg1)->maximizedSystemMenuIconWidget();
  *(QWidget **)&cresult = result; 
  return cresult;
}


SWIGEXPORT bool QMdiSubWindow_isShaded(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (bool)((QMdiSubWindow const *)arg1)->isShaded();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_setOption(QMdiSubWindow * self, QMdiSubWindow::SubWindowOption option, bool on) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMdiSubWindow::SubWindowOption arg2 ;
  bool arg3 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = (QMdiSubWindow::SubWindowOption)option; 
  arg3 = on ? true : false; 
  (arg1)->setOption(arg2,arg3);
}


SWIGEXPORT void QMdiSubWindow_setOption1(QMdiSubWindow * self, QMdiSubWindow::SubWindowOption option) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMdiSubWindow::SubWindowOption arg2 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = (QMdiSubWindow::SubWindowOption)option; 
  (arg1)->setOption(arg2);
}


SWIGEXPORT bool QMdiSubWindow_testOption(QMdiSubWindow const * self, QMdiSubWindow::SubWindowOption m3arg2) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMdiSubWindow::SubWindowOption arg2 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = (QMdiSubWindow::SubWindowOption)m3arg2; 
  result = (bool)((QMdiSubWindow const *)arg1)->testOption(arg2);
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_setKeyboardSingleStep(QMdiSubWindow * self, int step) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  int arg2 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = (int)step; 
  (arg1)->setKeyboardSingleStep(arg2);
}


SWIGEXPORT int QMdiSubWindow_keyboardSingleStep(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  int result;
  int cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (int)((QMdiSubWindow const *)arg1)->keyboardSingleStep();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_setKeyboardPageStep(QMdiSubWindow * self, int step) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  int arg2 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = (int)step; 
  (arg1)->setKeyboardPageStep(arg2);
}


SWIGEXPORT int QMdiSubWindow_keyboardPageStep(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  int result;
  int cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (int)((QMdiSubWindow const *)arg1)->keyboardPageStep();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_setSystemMenu(QMdiSubWindow * self, QMenu * systemMenu) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMenu *arg2 = (QMenu *) 0 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  arg2 = *(QMenu **)&systemMenu; 
  (arg1)->setSystemMenu(arg2);
}


SWIGEXPORT QMenu * QMdiSubWindow_systemMenu(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMenu *result = 0 ;
  QMenu * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (QMenu *)((QMdiSubWindow const *)arg1)->systemMenu();
  *(QMenu **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QMdiArea * QMdiSubWindow_mdiArea(QMdiSubWindow const * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  QMdiArea *result = 0 ;
  QMdiArea * cresult ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  result = (QMdiArea *)((QMdiSubWindow const *)arg1)->mdiArea();
  *(QMdiArea **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void QMdiSubWindow_showSystemMenu(QMdiSubWindow * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  (arg1)->showSystemMenu();
}


SWIGEXPORT void QMdiSubWindow_showShaded(QMdiSubWindow * self) {
  QMdiSubWindow *arg1 = (QMdiSubWindow *) 0 ;
  
  arg1 = *(QMdiSubWindow **)&self; 
  (arg1)->showShaded();
}


#ifdef __cplusplus
}
#endif

