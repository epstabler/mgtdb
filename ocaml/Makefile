OCAMLBYTE = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -I +labltk 

LIBS = labltk.cma str.cma
OPTLIBS = labltk.cmxa str.cmxa

GRAMMAR = g0

ML_SOURCES  = makeq.ml  tktree.ml  mgtdbp.ml  dt2st.ml  dt2bts.ml  dt2xbs.ml  dt2vt.ml  dt2mcfg.ml  $(GRAMMAR).ml
MLI_SOURCES = makeq.mli tktree.mli mgtdbp.mli dt2st.mli dt2bts.mli dt2xbs.mli dt2vt.mli dt2mcfg.mli $(GRAMMAR).mli

CMIFILES = makeq.cmi tktree.cmi mgtdbp.cmi dt2st.cmi dt2bts.cmi dt2xbs.cmi dt2vt.cmi dt2mcfg.cmi $(GRAMMAR).cmi
CMOFILES = makeq.cmo tktree.cmo mgtdbp.cmo dt2st.cmo dt2bts.cmo dt2xbs.cmo dt2vt.cmo dt2mcfg.cmo $(GRAMMAR).cmo
CMXFILES = makeq.cmx tktree.cmx mgtdbp.cmx dt2st.cmx dt2bts.cmx dt2xbs.cmx dt2vt.cmx dt2mcfg.cmx $(GRAMMAR).cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

# I set the default compilation to be opt (native). Use "make byte" to get byte code
all: opt

byte:  $(CMIFILES) $(CMOFILES) 
	$(OCAMLBYTE) $(OCAMLFLAGS) $(LIBS) $(CMOFILES) top.ml -o mgtdbp

opt: $(CMIFILES) $(CMXFILES) 
	$(OCAMLOPT) $(OCAMLFLAGS) $(OPTLIBS) $(CMXFILES) top.ml -o mgtdbp

%.cmo: %.ml %.cmi
	$(OCAMLBYTE) $(OCAMLFLAGS) $(LIBS) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) $(OPTLIBS) -c $<

%.cmi: %.mli 
	$(OCAMLBYTE) $(OCAMLFLAGS) -c $<

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *~

realclean: clean
	rm -f mgtdbp
	rm -f doc/*

doc:    all
	ocamldoc -html -d doc $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -dot -o doc/tools.dot $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -texi -o doc/tools.texinfo $(ML_SOURCES) $(MLI_SOURCES)
