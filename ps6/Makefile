# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean mlish

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

mlish: 
	$(COMPILER) -c mlish_ast.ml
	$(COMPILER) -c scish_ast.ml
	$(COMPILER) -c scish_eval.ml
	ocamlyacc ml_parse.mly
	$(COMPILER) -c ml_parse.mli
	$(COMPILER) -c ml_parse.ml
	ocamllex ml_lex.mll
	$(COMPILER) -c ml_lex.ml
	$(COMPILER) -c mlish_type_check.ml
	$(COMPILER) -c mlish_compile.ml
	$(COMPILER) -c mlish.ml
	$(COMPILER) -o ps6_mlish mlish_ast.cmo scish_ast.cmo scish_eval.cmo ml_parse.cmo ml_lex.cmo mlish_type_check.cmo mlish_compile.cmo mlish.cmo

f=/home/zhidian/comc/ps6/extra_tests_ps6/auto-simple-007.ml
check:
	./ps6_mlish typecheck $(f)

comp:
	./ps6_mlish compile $(f)

clean:
	-rm *.cmo *.cmi ps6_mlish ml_parse.ml ml_parse.mli ml_lex.ml

POLY=$(wildcard extra_tests_ps6/auto-poly*)
SIMPLE=$(wildcard extra_tests_ps6/auto-simple*)

list_files:
	@echo $(POLY) $(SIMPLE)

extra_tests_ps6/auto-simple%:
	@echo ""
	./ps6_mlish typecheck $@ > submit.s
	./ps6_mlish_correct typecheck $@ > ans.s
	diff submit.s ans.s

extra_tests_ps6/auto-poly%:
	@echo ""
	./ps6_mlish typecheck $@ > submit.s
	./ps6_mlish_correct typecheck $@ > ans.s
	diff submit.s ans.s

# test_simple: $(patsubst extra_tests_ps6/%, extra_tests_ps6/%, $(SIMPLE))
test_simple: $(SIMPLE)

test_poly: $(POLY)