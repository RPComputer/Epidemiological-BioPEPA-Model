all:
	grep "val version" ../src/Version.sml | sed -f sedfile > version.tex
	grep -v "#" ../biopepa.cfg > cfg/biopepa.cfg
	grep replications ../biopepa.cfg > cfg/biopepa.replications.cfg
	grep simul ../biopepa.cfg | grep -v report > cfg/biopepa.simulations.cfg
	grep report ../biopepa.cfg > cfg/biopepa.report.cfg
	grep stochkit.opt ../biopepa.cfg > cfg/stochkit.opt.cfg
	cat ~/export/software/biopepa/download.xml | sed -f xmltolatex.sed > download.tex
	pdflatex biopepawb.tex
	dos2unix biopepawb.idx
	makeindex biopepawb.idx
	pdflatex biopepawb.tex
	pdflatex biopepawb.tex
