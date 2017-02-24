TARGET  = jog
SOURCE  = $(TARGET).c
CFLAGS  = $(shell pkg-config --cflags libgit2 openssl) -Wall -std=gnu99
LDFLAGS = $(shell pkg-config --libs libgit2 openssl) -lm
DESTDIR = /usr/local/bin

$(TARGET) : $(SOURCE)
	$(CC) $(SOURCE) -o $(TARGET) $(CFLAGS) $(LDFLAGS)

install : $(TARGET)
	install -m 755 $(TARGET) $(DESTDIR)

uninstall :
	rm -f $(DESTDIR)/$(TARGET)

clean :
	rm $(TARGET)
