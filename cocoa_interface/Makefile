MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

%.o: %.c
	gcc -c -m32 -g -Wall -fPIC\
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include $<

all: beam  cocoa_nifs.so
	erl -s cocoa_test start

beam: ${MODS:%.erl=%.beam}

tests.o: tests.m
	gcc -arch i386 -c tests.m\
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include

cocoa_nifs.o: cocoa_nifs.m
	gcc -arch i386 -c cocoa_nifs.m\
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include

cocoa_nifs.so: cocoa_nifs.o tests.o
	cc cocoa_nifs.o tests.o -arch i386 \
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include\
                       -framework CoreMIDI\
                       -framework CoreAudio\
                       -framework AudioUnit\
                       -framework CoreServices\
                       -framework Cocoa\
                       -bundle -flat_namespace -undefined suppress\
                       -L/usr/local/lib/erlang/lib/erl_interface-3.7.6/lib\
                       -lerl_interface -lei -o cocoa_nifs.so
clean:
	rm -rf *.beam *.so *.o *~ erl_crash.dump



