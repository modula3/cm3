/****************************************************************************
**
** Copyright (C) 2006 Trolltech AS. All rights reserved.
**
** This file is part of the documentation of Qt. It was originally
** published as part of Qt Quarterly.
**
** This file may be used under the terms of the GNU General Public License
** version 2.0 as published by the Free Software Foundation or under the
** terms of the Qt Commercial License Agreement. The respective license
** texts for these are provided with the open source and commercial
** editions of Qt.
**
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://www.trolltech.com/products/qt/licensing.html or contact the
** sales department at sales@trolltech.com.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#include "dynamicqobject.h"
#include <stdio.h>

bool AbstractDynamicQObject::connectDynamicSlot(QObject *obj, char *signal, char *slot) {

    QByteArray theSignal = QMetaObject::normalizedSignature(signal);
    QByteArray theSlot = QMetaObject::normalizedSignature(slot);
    if (!QMetaObject::checkConnectArgs(theSignal, theSlot))
        return false;

    int signalId = obj->metaObject()->indexOfSignal(theSignal);
    if (signalId < 0)
        return false;

    int slotId = slotIndices.value(theSlot, -1);
    if (slotId < 0) {
        slotId = slotList.size();
        slotIndices[theSlot] = slotId;
        slotList.append(createSlot(theSlot.data()));
    }

    return QMetaObject::connect(obj, signalId, this, slotId + metaObject()->methodCount());
}

bool AbstractDynamicQObject::connectDynamicSignal(char *signal, QObject *obj, char *slot) {

    QByteArray theSignal = QMetaObject::normalizedSignature(signal);
    QByteArray theSlot = QMetaObject::normalizedSignature(slot);
    if (!QMetaObject::checkConnectArgs(theSignal, theSlot))
        return false;

    int slotId = obj->metaObject()->indexOfSlot(theSlot);
    if (slotId < 0)
        return false;

    int signalId = signalIndices.value(theSignal, -1);
    if (signalId < 0) {
        signalId = signalIndices.size();
        signalIndices[theSignal] = signalId;
    }

    return QMetaObject::connect(this, signalId + metaObject()->methodCount(), obj, slotId);
}


int AbstractDynamicQObject::qt_metacall(QMetaObject::Call c, int id, void **arguments) {

//printf("in qt_metacall\n");

    id = QObject::qt_metacall(c, id, arguments);
    if (id < 0 || c != QMetaObject::InvokeMetaMethod)
        return id;
    Q_ASSERT(id < slotList.size());

    slotList[id]->call(sender(), arguments);
    return -1;
}

bool AbstractDynamicQObject::emitDynamicSignal(char *signal, void **arguments) {

    QByteArray theSignal = QMetaObject::normalizedSignature(signal);
    int signalId = signalIndices.value(theSignal, -1);
    if (signalId >= 0) {
        QMetaObject::activate(this, metaObject(), signalId + metaObject()->methodCount(),
            arguments);
        return true;
    } else {
        return false;
    }
}

//-------------------- the subclass stuff -------------------

//MyDynamicQObject::MyDynamicQObject(int local) : DynamicQObject(), dynLocal(local)
//MyDynamicQObject::MyDynamicQObject(void (*fn)(int)) : DynamicQObject(), m3fn(fn)

DynamicQObject::DynamicQObject(callbackfn fn, void *obj) : AbstractDynamicQObject(), m3fn(fn), instance(obj) {
//    printf("DynamicQObject constructor\n");

}

AbstractDynamicSlot *DynamicQObject::createSlot(char *slot) {
//    printf("createSlot %s\n",slot);

    //return new Slot(this,dynLocal);
    return new Slot(this,m3fn,instance);
}

//Slot::Slot(MyDynamicQObject *_parent, int local) : parent(_parent), slotLocal(local)
//Slot::Slot(MyDynamicQObject *_parent, void (*fn)(int) ) : parent(_parent), m3fn(fn)
Slot::Slot(DynamicQObject *_parent, callbackfn fn, void *obj) : parent(_parent), m3fn(fn), instance(obj) {

Q_ASSERT(parent != 0);

    printf("Slot constructor\n");

}

void Slot::call(QObject *sender, void ** arguments) {

    //(parent->m3fn)(22);

    //printf("slot callback slotLocal %d \n",slotLocal);
    printf("slot callback \n");

    //testing numArgs 2 usually its 1
    /*
    int numArgs = 2;
    int i = 1;
    for (i = 1; i <= numArgs; i++) {
      int *ptr = reinterpret_cast<int *>(arguments[i]);
      printf("arg1 %d = %d \n",i,*ptr);
    }
*/

    //int *ptr = reinterpret_cast<int *>(arguments[1]);
    //printf("arg1 %d \n",*ptr);

    //m3fn(*ptr);
    m3fn(instance,arguments);

}
