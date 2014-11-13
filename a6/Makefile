QUEUE_FILES = timing.mli timing.ml queue.ml

queue: $(QUEUE_FILES)
	ocamlbuild -libs unix queue.d.byte

streams: streams.ml
	ocamlbuild -libs nums streams.d.byte

theory: theory.ml
	ocamlbuild theory.d.byte

clean:
	ocamlbuild -clean

