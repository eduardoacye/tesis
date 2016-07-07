prologues := 3;
outputtemplate := "%j-%c.mps";

beginfig(1)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (20,0);

  pickup pencircle scaled .5;
  drawarrow (0+3,0)--(20-3,0);
endfig;

beginfig(2)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (20,0);
  draw (40,0);

  pickup pencircle scaled .5;
  drawarrow (0+3,0)--(20-3,0);
  drawarrow (0+3,0+3)--(20-3,0+3);
  drawarrow (20+3,0)--(40-3,0);
endfig;

beginfig(3)
  pickup pencircle scaled 3;
  draw (0,0);

  pickup pencircle scaled .5;
  drawarrow (0,0){dir(-30)}..{up}(30,0)..{dir(-150)}cycle;
endfig;

beginfig(4)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (30,0);
  draw (30,30);
  draw (0,30);

  pickup pencircle scaled .5;
  drawarrow (0+3,0)--(30-3,0);
  drawarrow (30-3,0-3)--(0+3,0-3);
  drawarrow (30,0+3)--(30,30-3);
  drawarrow (30-3,30)--(0+3,30);
  drawarrow (0,30-3)--(0,0+3);
  drawarrow (0-3,0+3)--(0-3,30-3);
endfig;

beginfig(5)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (40,0);
  draw (40,40);
  draw (0,40);

  pickup pencircle scaled .5;
  drawarrow (0+3,0)--(40-3,0);
  drawarrow (0+3,0-3)--(40-3,0-3);
  drawarrow (40,0){dir(-90)}..{up}(50,0)..{dir(-90)}cycle;
  drawarrow (40,40-3)--(40,0+3);
  drawarrow (0+3,40)--(40-3,40);
  drawarrow (0+3,40+3)--(40-3,40+3);
  drawarrow (40-3,40+6)--(0+3,40+6);
  drawarrow (0,40-3)--(0,0+3);
endfig;

beginfig(6)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (20,0);
  draw (40,0);
  draw (60,0);

  pickup pencircle scaled .5;
  drawarrow (0+3,0)--(20-3,0);
  drawarrow (20+3,0)--(40-3,0);
  drawarrow (40+3,0)--(60-3,0);
  drawarrow (60+3,0)--(80-3,0);

  label(btex ... etex, (85,0));
endfig;

beginfig(7)
  pickup pencircle scaled 3;
  draw (0,0);
  draw (20,0);
  draw (40,0);
  draw (60,0);
  draw (80,0);
  draw (100,0);
  draw (120,0);
  draw (140,0);
  draw (0,-50);

  pickup pencircle scaled .5;
  drawarrow (0,0)--(20,0);
  drawarrow (20,0)--(40,0);
  drawarrow (40,0)--(60,0);
  drawarrow (60,0)--(80,0);
  drawarrow (80,0)--(100,0);
  drawarrow (100,0)--(120,0);
  drawarrow (120,0)--(140,0);
  drawarrow (140,0)--(160,0);
    
  drawarrow (0,0)--(0,-50);
  drawarrow (20,0)--(0,-50);
  drawarrow (40,0)--(0,-50);
  drawarrow (60,0)--(0,-50);
  drawarrow (80,0)--(0,-50);
  drawarrow (100,0)--(0,-50);
  drawarrow (120,0)--(0,-50);
  drawarrow (140,0)--(0,-50);

  label(btex ... etex, (170,0));
endfig;

end;
