grammar FuzzyFortranFast;

inputFile : ( subprogram | module | . )*
	//{System.out.println("subprogram: " + $subprogram.start);}
;

module : MODULE ID ( subprogram | . ) * END (MODULE (ID)?)?  (NL|EOF);

subprogram : ( COMMENT | OLD_COMMENT | EMPTY_LINE )* prefix* subType ID body END (subType (ID)?)? (NL|EOF)
	//{System.out.println("subprogram: " + $start+ $stop);}
;


body : ( ifStatement | ifOneLine | callStatement | . )*?
	//{System.out.println("body: " + $ctx.start);}
;

callStatement :
	CALL (callPrefix)? ID (LPAREN)?
;

callPrefix : ID '%';

ifOneLine : IF LPAREN block RPAREN (callStatement | ~THEN )
	//{System.out.println("ifonelineblock: " + $start + $stop);}
;

block : ( ID | '()' | LPAREN block RPAREN | ~( LPAREN | RPAREN ) )+?
	//{System.out.println("block: " + $ID.text);}
;

ifStatement : IF LPAREN block RPAREN ('&' | NL | '1')* THEN
	body
	((ELSE IF | ELSEIF) LPAREN block RPAREN ('&' | NL | '1')* THEN body)*
	(ELSE body)? 
	(END IF | ENDIF )
	//{System.out.println("if block: " + $start + $stop);}
;


prefix : PURE | ELEMENTAL | dataType;

subType : FUNCTION | SUBROUTINE | PROGRAM;

dataType: REAL (LPAREN NUMBER RPAREN)? | INTEGER (LPAREN NUMBER RPAREN)? | LOGICAL (LPAREN NUMBER RPAREN)?;

PURE : P U R E;

ELEMENTAL : E L E M E N T A L;

FUNCTION : F U N C T I O N;

SUBROUTINE : S U B R O U T I N E;

PROGRAM : P R O G R A M;

REAL : R E A L;

INTEGER : I N T E G E R;

LOGICAL : L O G I C A L;

ELSEIF : E L S E I F;

ENDIF : E N D I F;

END : E N D;

IF : I F;

ELSE : E L S E;

THEN : T H E N;

MODULE : M O D U L E;

CALL : C A L L;

LPAREN : '(';

RPAREN : ')';

// hack for pipelay
ONEH : '1h\'' | '1H\'';
CONTAINS : C O N T A I N S NL;
EMPTY_LINE :{getCharPositionInLine() == 0}?[ \t]* NL;
LITERAL : ('"' .*? '"' | '\'' .*? '\'');// {System.out.println("LITERAL" + getText());};
MACRO : '#'{getCharPositionInLine() == 1}? .*? NL -> skip;
COMMENT : ('!') .*? NL;
OLD_COMMENT : C{getCharPositionInLine() == 1}? .*? NL;
INTERFACE : I N T E R F A C E .*? END [ \t]+ I N T E R F A C E -> skip;
NUMBER: [0-9]+;
ID : [a-zA-Z][a-zA-Z0-9_]*;
WS : [ \t]+ -> skip ;
NL : '\r'? '\n';
ANY : . ;

fragment A : [aA]; // match either an 'a' or 'A'
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];