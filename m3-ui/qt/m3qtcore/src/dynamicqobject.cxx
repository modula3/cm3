
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

bool AbstractDynamicQObject::disConnectDynamicSlot(QObject *obj, char *signal, char *slot) {

    QByteArray theSignal = QMetaObject::normalizedSignature(signal);
    QByteArray theSlot = QMetaObject::normalizedSignature(slot);
    if (!QMetaObject::checkConnectArgs(theSignal, theSlot))
        return false;

    int signalId = obj->metaObject()->indexOfSignal(theSignal);
    if (signalId < 0)
        return false;

    int slotId = slotIndices.value(theSlot, -1);
    if (slotId < 0) {
        return false;
    } else {
        slotIndices.remove(theSlot);
        slotList.removeAt(slotId);
    }

    return QMetaObject::disconnect(obj, signalId, this, slotId + metaObject()->methodCount());
}

bool AbstractDynamicQObject::disConnectDynamicSignal(char *signal, QObject *obj, char *slot) {

    QByteArray theSignal = QMetaObject::normalizedSignature(signal);
    QByteArray theSlot = QMetaObject::normalizedSignature(slot);
    if (!QMetaObject::checkConnectArgs(theSignal, theSlot))
        return false;

    int slotId = obj->metaObject()->indexOfSlot(theSlot);
    if (slotId < 0)
        return false;

    int signalId = signalIndices.value(theSignal, -1);
    if (signalId < 0) {
        return false;
    } else {
        signalIndices.remove(theSignal);
    }

    return QMetaObject::disconnect(this, signalId + metaObject()->methodCount(), obj, slotId);
}

int AbstractDynamicQObject::qt_metacall(QMetaObject::Call c, int id, void **arguments) {

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


DynamicQObject::DynamicQObject(callbackfn fn, void *obj) : AbstractDynamicQObject(), m3fn(fn), instance(obj) {

}

AbstractDynamicSlot *DynamicQObject::createSlot(char *slot) {

    return new Slot(this,m3fn,instance);
}

Slot::Slot(DynamicQObject *_parent, callbackfn fn, void *obj) : parent(_parent), m3fn(fn), instance(obj) {

Q_ASSERT(parent != 0);

}

void Slot::call(QObject *sender, void ** arguments) {

    m3fn(instance,arguments);

}
