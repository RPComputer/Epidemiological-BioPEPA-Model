MLTON=/home/stg/pub/MLton/usr/bin
MLJ=/home/stg/pub/MLj/version-0.1/bin/mlj

RELEASE=RELEASE/LittleFrance/pwb
#MLJ=/home/stg/pub/MLj/version-0.2/bin/mlj

# A makefile for the PEPA Workbench.  The default is to compile
# with Moscow ML, but compile targets for Standard ML of New Jersey,
# MLton and MLj are also available

mosml:
	rm -f *.uo *.ui
	echo "structure LocalTime = Time;" > LocalTime.sml
	mosmlc -c Version.sig
	mosmlc -c Version.sml
	mosmlc -c Help.sig
	mosmlc -c Help.sml
	mosmlc -c JobName.sig
	mosmlc -c JobName.sml
	mosmlc -c LocalTime.sml
	mosmlc -c Reporting.sig
	mosmlc -c Reporting.sml
	mosmlc -c Error.sig
	mosmlc -c Error.sml
	mosmlc -c Debugging.sig
	mosmlc -c Debugging.sml
	mosmlc -c Options.sig
	mosmlc -c Options.sml
	mosmlc -c FileUtils.sig
	mosmlc -c FileUtils.sml
	mosmlc -c Configuration.sig
	mosmlc -c Configuration.sml
	mosmlc -c Files.sig
	mosmlc -c Files.sml
	mosmlc -c BioPEPA.sig
	mosmlc -c BioPEPA.sml
	mosmlc -c Lexer.sig
	mosmlc -c Lexer.sml
	mosmlc -c Sort.sig
	mosmlc -c Sort.sml
	mosmlc -c Prettyprinter.sig
	mosmlc -c Prettyprinter.sml
	mosmlc -c XMLPrettyprinter.sig
	mosmlc -c XMLPrettyprinter.sml
	mosmlc -c Stack.sig
	mosmlc -c Stack.sml
	mosmlc -c Sugar.sig
	mosmlc -c Sugar.sml
	mosmlc -c Preparser.sig
	mosmlc -c Preparser.sml
	mosmlc -c Parser.sig
	mosmlc -c Parser.sml
	mosmlc -c Table.sig
	mosmlc -c Table.sml
	mosmlc -c Statespace.sig
	mosmlc -c Statespace.sml
	mosmlc -c DerivativeSets.sig
	mosmlc -c DerivativeSets.sml
	mosmlc -c Environment.sig
	mosmlc -c Environment.sml
	mosmlc -c CSVIterator.sig
	mosmlc -c CSVIterator.sml
	mosmlc -c Species.sig
	mosmlc -c Species.sml
	mosmlc -c StochKit.sig
	mosmlc -c StochKit.sml
	mosmlc -c PlotFile.sig
	mosmlc -c PlotFile.sml
	mosmlc -c Model.sig
	mosmlc -c Model.sml
	mosmlc -c Gnuplot.sig
	mosmlc -c Gnuplot.sml
	mosmlc -c Semantic.sig
	mosmlc -c Semantic.sml
	mosmlc -c Dizzy.sig
	mosmlc -c Dizzy.sml
	mosmlc -c Reactions.sig
	mosmlc -c Reactions.sml
	mosmlc -c Dot.sig
	mosmlc -c Dot.sml
	mosmlc -c StyleSheet.sig
	mosmlc -c StyleSheet.sml
	mosmlc -c VFgen.sig
	mosmlc -c VFgen.sml
	mosmlc -c PRISM.sig
	mosmlc -c PRISM.sml
	mosmlc -c CSL.sig
	mosmlc -c CSL.sml
	mosmlc -c BPWB.sig
	mosmlc -c BPWB.sml
	mosmlc -standalone -o biopepawb mosmlbpwb.sml
	mv biopepawb ../bin
	mosmlc -c Highslide.sig
	mosmlc -c Highslide.sml
	mosmlc -c LaTeXprinter.sig
	mosmlc -c LaTeXprinter.sml
	mosmlc -c BPReporter.sig
	mosmlc -c BPReporter.sml
	mosmlc -standalone -o biopeparep mosmlbpr.sml
	mv biopeparep ../bin

mlton:
	echo "structure LocalTime = Time;" > LocalTime.sml
	${MLTON}/mlton -output biopepawb bpwb_mlton.cm

nj:
	echo "structure LocalTime = Time;" > LocalTime.sml
	cat njpwb.sml | sml

mlj:
	echo "structure LocalTime = struct fun now () = () end;" > LocalTime.sml
	${MLJ} pepa

release:
	cp README ${RELEASE}
	cp LICENSE ${RELEASE}
	cp Makefile ${RELEASE}
	cp pwb_mlton.cm ${RELEASE}
	cp pwb_mlton.buildfile ${RELEASE}
	cp *.{sml,sig} ${RELEASE}
	cp pwb ${RELEASE}
	cp pwb.exe ${RELEASE}
	cp doc/pwbpn.pdf ${RELEASE}/doc
	cp TESTS/*.pepa ${RELEASE}/TESTS
	(cd ${RELEASE} ; cd .. ; tar -zcvf pwb.tar.gz pwb)

clean:	
	rm -f *.ui *.uo


