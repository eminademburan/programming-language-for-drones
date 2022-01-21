parser: y.tab.c lex.yy.c
	gcc -o parser y.tab.c
y.tab.c: CS315f20_team40.yacc.y
	yacc CS315f20_team40.yacc.y
lex.yy.c: CS315f20_team40.lex.l
	lex CS315f20_team40.lex.l
