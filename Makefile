.PHONY: clean

ifeq ($(OS),Windows_NT)
EXT=.exe
endif

kobold$(EXT): kobold.cbl
	cobc -x -o $@ $<

clean:
	-rm kobold kobold.exe
