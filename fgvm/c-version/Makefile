SOURCE     = $(wildcard *.c)
HEADER     = $(wildcard *.h) $(wildcard fgvm/*.h)
OBJECT     = $(SOURCE:%.c=%.o)
LIBNAME    = libfgvm
STATIC_LIB = $(LIBNAME).a
SHARED_LIB = $(LIBNAME).so
TARGET     = $(STATIC_LIB) $(SHARED_LIB)
CFLAGS     = -Wall -Wextra -Wno-unused-parameter -O3 -std=c99 -shared -fPIC -I.
LDFLAGS    = -lpthread

all : $(TARGET)

$(STATIC_LIB) : $(OBJECT)
	ar rcs $(STATIC_LIB) $(OBJECT)

$(SHARED_LIB) : $(OBJECT)
	$(CC) $(OBJECT) -o $(SHARED_LIB) $(CFLAGS) $(LDFLAGS)

%.o : %.c $(HEADER)
	$(CC) $< -c $(CFLAGS)

clean :
	rm -f $(TARGET) $(OBJECT)
