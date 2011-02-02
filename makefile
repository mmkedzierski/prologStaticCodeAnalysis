PL = sicstus
SPLD = spld
SPLDFLAGS = --static --exechome=/opt/sicstus4.1.3/bin/
PROGRAM = metaprogram
MODULES = analysing.sav graphs.sav info.sav layers.sav \
 metaprogram.sav parsing.sav printing.sav program.sav \
 utils.sav verifying.sav


$(PROGRAM): $(MODULES)
	$(SPLD) $(SPLDFLAGS) --output $(PROGRAM) $(PROGRAM).sav

$(MODULES) : %.sav : %.pl
	@echo "compile('"$<"'). save_program('"$@"'). halt. " | $(PL)

clean:
	rm -f $(MODULES) $(PROGRAM) *~
