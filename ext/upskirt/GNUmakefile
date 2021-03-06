# Makefile

# Copyright (c) 2009, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

DEPDIR=depends
CFLAGS=-c -g -O3 -Wall -Werror
LDFLAGS=-g -O3 -Wall -Werror
CC=gcc

all:		libupskirt.so lace kilt

.PHONY:		all clean


# libraries

libupskirt.so:	libupskirt.so.2
	ln -s $^ $@

libupskirt.so.2: markdown.o array.o buffer.o renderers.o
	$(CC) $(LDFLAGS) -shared -Wl,-soname=$@ $^ -o $@


# executables

lace:		lace.o libupskirt.so
	$(CC) $(LDFLAGS) $^ -o $@

kilt:		kilt.o libupskirt.so
	$(CC) $(LDFLAGS) $^ -o $@


# housekeeping

benchmark:	benchmark.o libupskirt.so
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -f *.o
	rm -f libupskirt.so libupskirt.so.* lace kilt benchmark
	rm -rf $(DEPDIR)


# dependencies

include $(wildcard $(DEPDIR)/*.d)


# generic object compilations

%.o:	%.c
	@mkdir -p $(DEPDIR)
	@$(CC) -MM $< > $(DEPDIR)/$*.d
	$(CC) $(CFLAGS) -o $@ $<

%.o:	%.m
	@mkdir -p $(DEPDIR)
	@$(CC) -MM $< > depends/$*.d
	$(CC) $(CFLAGS) -o $@ $<
