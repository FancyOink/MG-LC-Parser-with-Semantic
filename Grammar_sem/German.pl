startCategory(cfin).
[drei] :: ([c1, -c, -b, -a], 3).
[zehn] :: ([c2, -s1], 10).
[elf] :: ([c2, -s1], 11).
[zwölf] :: ([c2, -s1], 12).
[zehn] :: ([=c1, +a, c2, -s1],abst(x,'1X+10'(x))).
[sechzehn] :: ([c2, -s1], 16).
[siebzehn] :: ([c2, -s1], 17).
[zwanzig] :: ([c2, -s1], 20).
[einundzwanzig] :: ([c2, -s1], 21).
[undzwanzig] :: ([=c1, +b, c2, -s1],abst(x, '1X+20'(x))).
[ßig] :: ([=c1, +c, c2, -s1],abst(x, '0X+30'(x))).
[einund] :: ([=c21s, c2, -s1],abst(x, '1X+31'(x))).
[ßig] :: ([=c1, +c, c21s],abst(x, '0X'(x))).
[und] :: ([=c22s, =c2b, c2, -s1],abst(x, abst(y, '1X+1Y+3'(x,y)))).
[ßig] :: ([=c1, +c, c22s],abst(x, '9X'(x))).
[zig] :: ([=c1, +d, c2, -s1],abst(x, '10X'(x))).
[einund] :: ([=c23s, c2, -s1],abst(x, '1X+1'(x))).
[zig] :: ([=c1, +d, c23s],abst(x,'10X+1'(x))).
[und] :: ([=c24s, =c2b, c2, -s1],abst(x, abst(y,'1X+1Y'(x,y)))).
[zig] :: ([=c1, +d, c24s],abst(x,'10X+N'(x))).
[sechzig] :: ([c2, -s1], 60).
[einundsechzig] :: ([c2, -s1], 61).
[undsechzig] :: ([=c1, +b, c2, -s1],abst(x, plus60(x))).
[siebzig] :: ([c2, -s1], 70).
[einundsiebzig] :: ([c2, -s1], 71).
[undsiebzig] :: ([=c1, +b, c2, -s1],abst(x, plus70(x))).
[eins] :: ([c1], 1).
[zwei] :: ([c1, -b], 2).
[vier] :: ([c1, -b, -a, -d], 4).
[fünf] :: ([c1, -b, -a, -d], 5).
[sechs] :: ([c1, -b], 6).
[sieben] :: ([c1, -b], 7).
[acht] :: ([c1, -b, -a, -d], 8).
[neun] :: ([c1, -b, -a, -d], 9).
[hundert] :: ([c3], 100).
[hundert] :: ([=c2, c3],abst(x,'1X+100'(x))).
[hundert] :: ([=c1, +b, c3],abst(x,'100X'(x))).
[hundert] :: ([=c2, =c2b, c3],abst(x, abst(y,'1X+100Y'(x,y)))).
[] :: ([=c1, +b, c2b],abst(x, x)).
[] :: ([=c1, c2],abst(x, x)).
[] :: ([=c3, cfin],abst(x, x)).
[] :: ([=c1, cfin],abst(x, x)). % Brücken LI
[] :: ([=c2, cfin],abst(x, x)). % Brücken LI
[] :: ([=c1, c3],abst(x, x)).
[] :: ([=c2, +s1, c3],abst(x, x)).
[] :: ([=c1, +d, c1],abst(x, x)).
[] :: ([=c1, +c, c1],abst(x, x)).
[] :: ([=c2, +s1, c2],abst(x, x)).
[] :: ([=c1, +a, c1],abst(x, x)).
[] :: ([=c1, +b, c1],abst(x, x)).
[] :: ([=c1, +b, +a, +d, c1, -d],abst(x, x)).
[] :: ([=c1, +b, +a, +d, c1, -b],abst(x, x)).
[] :: ([=c1, +a, +d, c1, -a],abst(x, x)).
[] :: ([=c1, +c, +b, +a, c1, -c],abst(x, x)).
[] :: ([=c1, +b, +a, c1, -b],abst(x, x)).
