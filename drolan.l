%option main
COMMENT ##.*
AND and
OR or
VAR var
DOES does
BEGIN begin
END end
IF if
ELSE else
FOR for
WHILE while
RETURN return
FUNCTION function
INPUT input
OUTPUT output
CONST const
TRUE true
FALSE false
VOID void
ASSIGNOP \= 
SC \;
LP \(
RP \)
EQ \=\=
GT \>
GTE \>\=
LT \<
LTE \<\=
NE \!\=
PLUS \+
MINUS \-
DIVISION \\
MULT \*
COMMA \,
LETTER [a-zA-Z]
DIGIT [0-9]
SIGN [+-]
INT {SIGN}?{DIGIT}+
DOUBLE {SIGN}?{DIGIT}*(\.){DIGIT}+
STRING \".*\"
CHAR \'.\'
READINC readInc
READALT readAlt
READTEMP readTemp
READACC readAcc
READTIME readTime
TOGGLECAM toggleCam
TAKEPIC takePic 
CONNECTWIFI connectWifi
TAKEOF takeOf
LAND land 
CHANGEALT changeAlt
ROTATE rotate
GOFORWARD goForward
GOBACKWARD goBackward
IDENTIFIER {LETTER}({LETTER}|\_|{DIGIT})*
%%
{COMMENT} printf("COMMENT ");
{AND} printf("AND ");
{OR} printf("OR ");
{VAR} printf( "VAR ");
{DOES} printf("DOES ");
{BEGIN} printf("BEGIN ");
{END} printf("END ");
{IF} printf("IF ");
{ELSE} printf("ELSE ");
{FOR} printf("FOR ");
{WHILE} printf("WHILE ");
{RETURN} printf("RETURN ");
{FUNCTION} printf("FUNCTION ");
{INPUT} printf("INPUT ");
{OUTPUT} printf("OUTPUT ");
{CONST} printf("CONST ");
{TRUE} printf("TRUE ");
{FALSE} printf("FALSE ");
{VOID} printf("VOID ");
{ASSIGNOP} printf("ASSIGN_OP ");
{SC} printf("SC ");
{LP} printf("LP ");
{RP} printf("RP ");
{EQ} printf("EQ ");
{GT} printf("GT ");
{GTE} printf("GTE ");
{LT} printf("LT ");
{LTE} printf("LTE ");
{NE} printf("NE ");
{PLUS} printf("PLUS ");
{MINUS} printf("MINUS ");
{DIVISION} printf("DIV "); 
{MULT} printf("MULT ");
{COMMA} printf("COMMA ");
{INT} printf("INT ");
{DOUBLE} printf("DOUBLE ");
{STRING} printf("STRING ");
{CHAR} printf("CHAR ");
{READINC} printf("READINC ");
{READALT} printf("READALT ");
{READTEMP} printf("READTEMP ");
{READACC} printf("READACC ");
{READTIME} printf("READTIME ");
{TOGGLECAM} printf("TOGGLECAM ");
{TAKEPIC} printf("TAKEPIC ");
{CONNECTWIFI} printf("CONNECTWIFI ");
{TAKEOF} printf("TAKEOF ");
{LAND} printf("LAND ");
{CHANGEALT} printf("CHANGEALT ");
{ROTATE} printf("ROTATE ");
{GOFORWARD} printf("GOFORWARD ");
{GOBACKWARD} printf("GOBACKWARD ");
{IDENTIFIER} printf("IDENTIFIER ");
[ \t]* ;
. ;

