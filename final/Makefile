all: cs51_final

FILES = order.ml dict.ml myset.ml prio_q.ml graphs.ml new_dij.ml

cs51_final: $(FILES)
	ocamlc -g -o cs51_final str.cma $(FILES)
#unix.cma
clean: 
	rm -f *.cmi *.cmo cs51_final *.cma
