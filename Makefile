##########################
#### System Variables ####
##########################

INCLUDES=-I +res -I +lablgl


WITHUNIX=unix.cma -cclib -lunix
WITHSTR=str.cma -cclib -lstr
WITHGL =lablgl.cma -cclib -lGL
WITHRES=res.cma
WITHGLUT=lablglut.cma -cclib -lglut
WITHDYNLINK=dynlink.cma

CCLIB= 

CAMLOPT=ocamlopt
OPTFLAGS=-noassert -unsafe $(INCLUDES)
#CAMLC=ocamlcp
CAMLC=ocamlcp
FLAGS=$(INCLUDES)
CAMLDEP=ocamldep


JERBOADEP=$(WITHUNIX) $(WITHGL) $(WITHGLUT) $(WITHDYNLINK) $(WITHRES) $(WITHSTR)
JERBOAXDEP=$(JERBOADEP:.cma=.cmxa) 

##########################
#### Specific Rule    ####
##########################

all: .depend jerboa
opt: jerboax

libjerboa.cma:float_triplet.cmo gmap.cmo rule.cmo sig_mod.cmo
	$(CAMLC) $(FLAGS) -a -o $@ $^

jerboa: libjerboa.cma gldisplay.cmo main_mod.cmo 
	$(CAMLC) $(FLAGS) $(JERBOADEP) -o $@  $^

libjerboa.cmxa: float_triplet.cmx gmap.cmx rule.cmx sig_mod.cmx
	$(CAMLOPT) -a $(OPTFLAGS) -o $@ $^

jerboax: libjerboa.cmxa gldisplay.cmx main_mod.cmx
	$(CAMLOPT) $(OPTFLAGS) -o $@ $(JERBOAXDEP)  $^

distrib: jerboa
	rm -fr distrib
	mkdir distrib
	mkdir distrib/gen
	cp float_triplet.cmi gmap.cmi rule.cmi sig_mod.cmi distrib/gen/
	cp jerboa libjerboa.cma distrib/
##########################
#### Generic Rule     ####
##########################

%.cmi: %.mli
	$(CAMLC) $(FLAGS) -c $<

%.cmo: %.ml
	$(CAMLC) $(FLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(OPTFLAGS) -c $<


#### independant rule 

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa .a


clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *~

cleanall:
	rm -f goku *.cmi *.cmo *.cma *.cmx *.o *~ .depend jerboa jerboax

.PHONY: all clean cleanall distrib ocaml doc

depend: 
	$(CAMLDEP) *.mli *.ml > .depend 
.depend: *.mli *.ml
	$(CAMLDEP) *.mli *.ml > .depend 

doc: float_triplet.ml gmap.ml rule.ml sig_mod.ml
	mkdir -p doc
	ocamldoc -html $(INCLUDES) -hide Res,Res.Array -inv-merge-ml-mli -m A -sort -stars -d doc float_triplet.ml gmap_sig.mli gmap.ml rule.ml sig_mod.ml

ocaml:
	ocaml float_triplet.cmo $(INCLUDES) res.cma gmap.cmo rule.cmo


include .depend
