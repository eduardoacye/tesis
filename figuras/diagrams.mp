prologues := 3;
outputtemplate := "%j-%c.mps";

verbatimtex
%&latex
  \documentclass{minimal}
  \usepackage{amsmath, amssymb}
  \usepackage{kpfonts}
  \begin{document}
etex

beginfig(1)
  label(btex $M$ etex, (0,0));
  label(btex $M_{1}$ etex, (-40,-30));
  label(btex $M_{2}$ etex, (40,-30));
  label(btex $M_{3}$ etex, (0,-60));

  draw (0-6,0-6)--(-40+6,-30+6);
  draw (0+6,0-6)--(40-6,-30+6);
  draw (-40+6,-30-6)--(0-6,-60+6) dashed evenly;
  draw (40-6,-30-6)--(0+6,-60+6) dashed evenly;

  label(btex $\leftarrowtail$ etex, (-20-5,-15+5));
  label(btex $\rightarrowtail$ etex, (20+5,-15+5));
  label(btex $\rightarrowtail$ etex, (-20-5,-45-5));
  label(btex $\leftarrowtail$ etex, (20+5,-45-5));
endfig;

beginfig(2)
  draw (0,0)--(-10,20)--(-20,40);
  draw (0,0)--(10,20)--(20,40);
  draw (-10,20)--(0,40)--(10,20);

  label(btex $Z$ etex, (0,0-5));
  label(btex $Z_{1}$ etex, (-10-8,20));
  label(btex $Z_{2}$ etex, (10+8,20));
  label(btex $M$ etex, (-20,40+5));
  label(btex $L$ etex, (0,40+5));
  label(btex $N$ etex, (20,40+5));
endfig;

end;
