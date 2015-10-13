ECC = erlc

OUTDIR = ./
RCS = $(wildcard *.erl)
OBJS = $(patsubst %.erl,$(OUTDIR)/%.beam,$(RCS))

all:$(OBJS)
	
%.beam:%.erl
	$(ECC) $^

clean: 
	rm  *.beam
