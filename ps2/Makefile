# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean comb yacc

utility:
	$(COMPILER) -c explode.ml

com:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c eval.ml

comb: utility com
	$(COMPILER) -c lcombinators.ml
	$(COMPILER) -c comblexer.ml
	$(COMPILER) -c combparser.ml
	$(COMPILER) -c fishcomb.ml
	$(COMPILER) -o ps2comb ast.cmo eval.cmo explode.cmo lcombinators.cmo comblexer.cmo combparser.cmo fishcomb.cmo
	
yacc: com
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c fishyacc.ml
	$(COMPILER) -o ps2yacc ast.cmo parse.cmo lex.cmo eval.cmo fishyacc.cmo

clean:
	-rm *.cmo *.cmi ps2comb ps2yacc parse.ml parse.mli lex.ml

FILES := 01cexpr_01add.fish 01cexpr_09lt.fish 02vexpr_01add.fish 02vexpr_09lt.fish 03stmt_02if.fish 09all_02fibo.fish \
         01cexpr_02sub.fish 01cexpr_10lte.fish 02vexpr_02sub.fish 02vexpr_10lte.fish 03stmt_03while.fish \
         01cexpr_03mul.fish 01cexpr_11uminus.fish 02vexpr_03mul.fish 02vexpr_11uminus.fish 03stmt_04for.fish \
         01cexpr_04div.fish 01cexpr_12not.fish 02vexpr_04div.fish 02vexpr_12not.fish 03stmt_05if.fish \
         01cexpr_05eq.fish 01cexpr_13and.fish 02vexpr_05eq.fish 02vexpr_13and.fish 03stmt_06for.fish \
         01cexpr_06neq.fish 01cexpr_14or.fish 02vexpr_06neq.fish 02vexpr_14or.fish 04opt_01cfoldif.fish \
         01cexpr_07gt.fish 01cexpr_15bigcon.fish 02vexpr_07gt.fish 02vexpr_15assignval.fish 04opt_02cfoldif.fish \
         01cexpr_08gte.fish 01cexpr_16bigcon.fish 02vexpr_08gte.fish 03stmt_01if.fish 09all_01adder.fish


run: $(FILES)

$(FILES):
	./ps2yacc test/$@ 
