frag_src = $(wildcard *.frag)
vert_src = $(wildcard *.vert)
spv = $(frag_src:.frag=.frag.spv) $(vert_src:.vert=.vert.spv)

default: $(spv)

%.spv: %
	# glslangValidator -V -o $@ $<
	glslc -o $@ $<

.PHONY: clean
clean:
	rm $(spv)
