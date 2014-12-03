CC = ocamlfind ocamlopt
	#ocamlopt -I +lablgl lablgl.cmxa lablglut.cmxa unix.cmxa maze.ml -o maze
#IFLAGS = -S -annot -package batteries -package fftw3 -linkpkg -I +lablgl lablgl.cmxa lablglut.cmxa #unix.cmxa
IFLAGS = -unsafe -annot -package batteries -package fftw3 -linkpkg -I +lablgl lablgl.cmxa lablglut.cmxa #unix.cmxa
TARGET = mpd

all: $(TARGET)

$(TARGET): $(TARGET).ml
	$(CC) $(IFLAGS) -o $@ $^

clean:
	rm $(TARGET) *.cmi

