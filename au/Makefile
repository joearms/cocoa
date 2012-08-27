MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

%.o: %.c
	gcc -c -m32 -g -Wall -fPIC\
            -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include\
            -I/usr/local/lib/erlang/erts-5.9/include $<

all: beam  au_nifs.so au_nifs.c
	erl -s au_nifs test1 -s init stop


beam: ${MODS:%.erl=%.beam}

au_nifs.so: au_nifs.o 
	cc au_nifs.o -arch i386\
                       -framework AudioUnit -framework CoreServices -framework CoreMidi\
                       -bundle -flat_namespace -undefined suppress\
                       -L/usr/local/lib/erlang/lib/erl_interface-3.7.6/lib\
                       -lerl_interface -lei -o au_nifs.so
clean:
	rm -rf *.beam *.so *.o *~ erl_crash.dump



