
TARGET=LINUXELF
INCLUDES=/usr/local/postgres95/include
LIBRARIES=/usr/local/postgres95/lib

all: sql.c 
	ar crus ../$(TARGET)/libpgodbc.a ../$(TARGET)/sql.o
	ranlib ../$(TARGET)/libpgodbc.a

sql.c: sql1.h sql1ext.h
	cc -c -I$(INCLUDES) sql.c -o ../$(TARGET)/sql.o

sql1.h:

sql1ext.h:

