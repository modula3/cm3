#ifndef DYNAMICQOBJECT_H
#define DYNAMICQOBJECT_H

#include <QtCore/QHash>
#include <QtCore/QList>
#include <QtCore/QMetaObject>
#include <QtCore/QObject>

#include <stdio.h>

typedef void (*callbackfn)(void *object,void ** args);

class AbstractDynamicSlot
{
public:

    virtual void call(QObject *sender, void **arguments) = 0;
};

class AbstractDynamicQObject: public QObject
{
public:
    AbstractDynamicQObject(QObject *parent = 0) : QObject(parent) {
    }

    virtual int qt_metacall(QMetaObject::Call c, int id, void **arguments);

    bool emitDynamicSignal(char *signal, void **arguments);
    bool connectDynamicSlot(QObject *obj, char *signal, char *slot);
    bool connectDynamicSignal(char *signal, QObject *obj, char *slot);

    bool disConnectDynamicSlot(QObject *obj, char *signal, char *slot);
    bool disConnectDynamicSignal(char *signal, QObject *obj, char *slot);

    virtual AbstractDynamicSlot *createSlot(char *slot) = 0;

private:
    QHash<QByteArray, int> slotIndices;
    QList<AbstractDynamicSlot *> slotList;
    QHash<QByteArray, int> signalIndices;
};

//----------------  subclass stuff --------------------------

class DynamicQObject : public AbstractDynamicQObject
{
public:
    DynamicQObject(callbackfn fn, void *obj );

    AbstractDynamicSlot *createSlot(char *slot);

private:

  void *instance;
  callbackfn m3fn;

};

class Slot : public AbstractDynamicSlot
{
public:

    Slot(DynamicQObject *parent, callbackfn fn, void *obj );

    void call(QObject *sender, void **arguments);

private:

  DynamicQObject *parent;

  void *instance;
  callbackfn m3fn;
};

#endif
