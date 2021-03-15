NAME=geocoding_drv
TARGET_DIR=.
TARGET=${TARGET_DIR}/${NAME}

OS=$(shell uname)
SRC_DIR=.
SRC=$(wildcard ${SRC_DIR}/*.cpp)
OBJ=$(patsubst ${SRC_DIR}/%.cpp,${TARGET_DIR}/%.o,${SRC})

LIBS=

ifeq (${OS},FreeBSD)
CPPFLAGS=-I/usr/local/include
CFLAGS=-fpic -g -O2 -Wall
LDFLAGS=-fpic -L/usr/local/lib -lm
CC=cc
CXX=c++
else
CPPFLAGS=-I/opt/local/include
CFLAGS=-fPIC -g -O2 -Wall
CC=gcc
CXX=g++
endif

CPPFLAGS+=-I${SRC_DIR}/
LDFLAGS+=-lstdc++

all : ${TARGET}
	mkdir -p ../priv/
	cp ${TARGET} ../priv/

clean :
	rm -rf ${TARGET_DIR}/*.o ${TARGET_DIR}/kdtree++/*.[od] ${TARGET}

${TARGET_DIR}/%.o : ${SRC_DIR}/%.cpp ${TARGET_DIR}
	${CC} ${CFLAGS} ${CPPFLAGS} -o $@ -c $<

${TARGET_DIR}/%.o : ${SRC_DIR}/%.c ${TARGET_DIR}
	${CC} ${CFLAGS} ${CPPFLAGS} -o $@ -c $<

${TARGET} : ${OBJ} ${TARGET_DIR}
	${CC} ${LDFLAGS} -o $@ $< ${LIBS}

${TARGET_DIR} :
	mkdir ${TARGET_DIR}
