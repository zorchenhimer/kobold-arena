
CFLAGS=-x

rpg: main.cbl
	cobc $(CFLAGS) -o $@ $^

clean:
	-rm rpg
