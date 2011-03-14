TEXFILE=lio_sos
TEX=pdflatex
VIEWER=evince
SP=aspell -c


TEMPLATE='\\documentclass[12pt]{article}\
\n\\usepackage[margin=1in]{geometry}\
\n\\title{TITLE}\
\n\\author{YOUR NAME}\
\n\\date{\\today}\n\
\n\\begin{document}\
\n\\maketitle\
\n\n\\begin{abstract}\
\nSome sweet text\
\n\\end{abstract}\
\n\n\\section{Introduction}\
\n\n\\end{document}\
\n$$Header$$'

$(TEXFILE).pdf: $(TEXFILE).ltx
	 $(TEX) $(TEXFILE).ltx

show: $(TEXFILE).pdf
	 $(VIEWER) ./$(TEXFILE).pdf  &

spell: $(TEXFILE).ltx
	 $(SP) $(TEXFILE).ltx

clean:
	 -rm  $(TEXFILE).pdf
	 -rm  $(TEXFILE).aux
	 -rm  $(TEXFILE).dvi
	 -rm  $(TEXFILE).log

template:
	 if [ -e "$(TEXFILE).ltx" ] ; then  cp $(TEXFILE).ltx $(TEXFILE).ltx.bak ;  fi 
	 echo $(TEMPLATE) > $(TEXFILE).ltx

checkin: 
	-mkdir RCS 
	 ci -l $(TEXFILE).ltx

checkout: 
	 co -l $(TEXFILE).ltx

babies:
	 echo  -e "You know it!!\a"
