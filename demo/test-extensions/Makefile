PKG_MODULES := gobject-2.0 webkit2gtk-web-extension-4.0

CFLAGS += ${shell pkg-config ${PKG_MODULES} --cflags}
LDLIBS += ${shell pkg-config ${PKG_MODULES} --libs}

all: test-extension.so

test-extension.so: test-extension.o
	${LD} ${LDFLAGS} -fPIC -shared -o $@ $^ ${LDLIBS}

test-extension.o:
	${CC} -fPIC -o $@ -c ${CFLAGS} test-extension.c

clean:
	${RM} test-extension.so test-extension.o
