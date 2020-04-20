
CFLAGS=-x

kobold: kobold.cbl
	cobc $(CFLAGS) -o $@ $^

clean:
	-rm kobold
