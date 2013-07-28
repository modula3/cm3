%module QtPolygon

%include "m3qt.i"
%include "common.i"

%import "QtObject.i"

%{
#include <QtGui/qpolygon.h>
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapintf) %{
TYPE
  T = QPolygon;
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

/*
%nodefaultctor;
%nodefaultdtor;

template<class T> class QVector {
private:
    T *data;
public:
//    List() {
//    };
//    ~List() {
//      delete [] data;
    };
  int ignoreme(int i);
};
*/
//this not working since qvector not defined but hard to get qvector to stop crash swig
//need to test the template stuff and these go after the .h file below
//%template(VectorList) QVector<QPoint>;
//%template(VectorListF) QVector<QPointF>;


%ignore QPolygon::QPolygon(const QVector<QPoint> &v);
%ignore QPolygonF::QPolygonF(const QVector<QPointF> &v);

//prob with ... args
%ignore setPoints(int nPoints, int firstx, int firsty, ...);
%ignore putPoints(int index, int nPoints, int firstx, int firsty, ...);

%ignore operator QVariant;
%ignore operator==;

//int * in 2 contexts
%apply  intarr* {const int *points};
%apply  intvar* {int *x, int *y};

//Local classes
%apply ClassIn {QPolygon &};
%apply ClassIn {const QPolygon &};
%apply ClassIn {QPolygonF &};
%apply ClassIn {const QPolygonF &};
%apply SelfReturn {QPolygon};
%apply SelfReturn {QPolygonF};


DoType(QRect,QtRect)
DoType(QRectF,QtRect)
DoType(QPoint,QtPoint)
DoType(QPointF,QtPoint)

%include <QtGui/qpolygon.h>

