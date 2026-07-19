ff: ff.c
	cc -fuse-ld=mold -std=c11 -Wall -Wextra -Wpedantic -O3 -flto -DNDEBUG -o $@ $<
	strip $@

.PHONY: clean
clean:
	rm -f ff
