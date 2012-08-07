MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

%.o: %.c
	gcc -c -m32 -g -Wall -fPIC\
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include $<

all: beam  midi_nifs.so midi_nifs.c
	erl -s midi_test start

beam: ${MODS:%.erl=%.beam}

midi_nifs.so: midi_nifs.o 
	cc midi_nifs.o -arch i386\
                       -framework CoreMIDI\
                       -framework CoreAudio\
                       -framework AudioUnit\
                       -framework CoreServices\
                       -bundle -flat_namespace -undefined suppress\
                       -L/usr/local/lib/erlang/lib/erl_interface-3.7.6/lib\
                       -lerl_interface -lei -o midi_nifs.so
clean:
	rm -rf *.beam *.so *.o *~ erl_crash.dump



