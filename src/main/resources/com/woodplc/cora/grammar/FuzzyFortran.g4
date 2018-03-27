grammar FuzzyFortran;

inputFile : ( subprogram | . )*
	//{System.out.println("subprogram: " + $subprogram.start);}
;

subprogram : ( COMMENT | OLD_COMMENT | EMPTY_LINE )* prefix* subType ID body 'end' (subType (ID)?)? (NL|EOF)
	//{System.out.println("subprogram: " + $start+ $stop);}
;


body : ( ifStatement | ifOneLine | callStatement | . )*?
	//{System.out.println("body: " + $ctx.start);}
;

callStatement :
	'call' ID ('(')?
;

ifOneLine : 'if' '(' block ')' (callStatement | ~'then' )
	//{System.out.println("ifonelineblock: " + $start + $stop);}
;

block : ( ID | '()' | '(' block ')' | ~( '(' | ')' ) )+?
	//{System.out.println("block: " + $ID.text);}
;

ifStatement : 'if' '(' block ')' ('&' | NL | '1')* 'then'
	body
	(('else' 'if' | 'elseif') '(' block ')' ('&' | NL | '1')* 'then' body)*
	('else' body)? 
	('end' 'if' | 'endif' )
	//{System.out.println("if block: " + $start + $stop);}
;


//endIfStatement : ('end' 'if' | 'endif') (ID)?;

prefix : 'pure' | 'elemental' | dataType;

subType : 'function' | 'subroutine' | 'program';

dataType: 'real' ('(' NUMBER ')')? | 'integer' ('(' NUMBER ')')? | 'logical' ('(' NUMBER ')')?;

// hack for pipelay
ONEH : '1h\'';
CONTAINS : 'contains' NL;
EMPTY_LINE :{getCharPositionInLine() == 0}?[ \t]* NL;
LITERAL : ('"' .*? '"' | '\'' .*? '\'');// {System.out.println("LITERAL" + getText());};
MACRO : '#'{getCharPositionInLine() == 1}? .*? NL -> skip;
COMMENT : ('!') .*? NL;
OLD_COMMENT : 'c'{getCharPositionInLine() == 1}? .*? NL;
INTERFACE : 'interface' .*? 'end interface' -> skip;
//ELSEIF : 'else if' -> skip;
NUMBER: [0-9]+;
ID : [a-zA-Z][a-zA-Z0-9_]*;
WS : [ \t]+ -> skip ;
NL : '\r'? '\n';
ANY : . ;