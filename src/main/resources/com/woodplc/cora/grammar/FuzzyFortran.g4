grammar FuzzyFortran;

import FuzzyFortranFast;

module : moduleStatement (typeStatement | publicStatement | privateStatement | subprogram | . )*? moduleEndStatement;

typeStatement
   : typename (',' (dimensionStatement|intentStatement| PARAMETER| ALLOCATABLE| accessSpecifier | OPTIONAL | SAVE | EXTERNAL | TARGET | POINTER | PROTECTED))* '::'? typeStatementNameList
   ;

publicStatement : PUBLIC ('::'? identifier (',' identifier)*)?;

privateStatement : PRIVATE ('::'? identifier (',' identifier)*)?;

allocateStatement : ALLOCATE LPAREN (typename '::')? exprList1 (',' allocOpt)* RPAREN;

allocOpt : optKeywords '=' identifier;

optKeywords : STAT | ERRMSG | SOURCE | MOLD;

typeStatementNameList
   : assignmentStatement (',' assignmentStatement)*
   | typeStatementName (',' typeStatementName)*
   ;
   
typeStatementName
   : arrayDeclarator
   |identifier 
   ;
   
assignmentStatement
   : expression1 '=' expression1
   ;
   
accessSpecifier : PUBLIC | PRIVATE;

intentStatement
	: INTENT LPAREN INTENTION RPAREN
	| INTENT LPAREN INTENTION RPAREN '::'? arrayDeclaratorExtents
	;
	
dimensionStatement
   : DIMENSION arrayDeclarators
   | DIMENSION LPAREN arrayDeclaratorExtents RPAREN
   ;

arrayDeclarator
   : identifier LPAREN arrayDeclaratorExtents RPAREN
   ;

arrayDeclarators
   : arrayDeclarator (',' arrayDeclarator)*
   ;

arrayDeclaratorExtents
   : arrayDeclaratorExtent (',' arrayDeclaratorExtent)*
   ;

arrayDeclaratorExtent
   : expression1
   ;
   
typename
   : REAL (LPAREN expression1 RPAREN)? 
   | COMPLEX (LPAREN expression1 RPAREN) 
   | DOUBLE COMPLEX 
   | DOUBLE PRECISION 
   | INTEGER (LPAREN expression1 RPAREN)?
   | LOGICAL (LPAREN expression1 RPAREN)?
   | CHARACTER (LPAREN ((KIND|LEN) '=')? (expression1|'*') RPAREN)?
   | CHARACTER '*' expression1
   | TYPE LPAREN identifier RPAREN
   | CLASS LPAREN identifier RPAREN
   ;

expression1 
	: identifier LPAREN exprList1 RPAREN expression1
	| identifier LPAREN exprList1? RPAREN
	| expression1 (EQ | NE | GT | GE | LT | LE | LOR | LAND ) expression1
	| (LNOT) expression1
	| expression1 POWER expression1
	| expression1 ('*' | DIV) expression1
	| expression1 ('+' | '-') expression1
	| expression1 DIV DIV expression1
	| ('+'|'-') expression1
	| identifier
	| NUMBER
	| logicalConstant
	| LPAREN expression1 RPAREN
	| expression1 ':' expression1
	| ':'
	| expression1 ':'
	| ':' expression1
	| expression1 '%' expression1
	| '%' expression1
	;

logicalConstant : (TRUE | FALSE)
   ;

exprList1 : expression1 ((','|':') expression1)*;

moduleStatement : MODULE identifier;

moduleEndStatement : END MODULE identifier?;

subprogram : ( COMMENT | OLD_COMMENT | EMPTY_LINE )* prefix* subType identifier body END (subType (identifier)?)? (NL|EOF)
	//{System.out.println("subprogram: " + $start+ $stop);}
;

body : ( ifStatement | ifOneLine | callStatement | allocateStatement | . )*?
	//{System.out.println("body: " + $ctx.start);}
;


callStatement :
	CALL (callPrefix)? identifier (LPAREN)?
;

callPrefix : identifier '%';

identifier
   : ID
   | INTENTION
   | NUMBER
   | ERRMSG
   | ALLOCATE
   ;

ALLOCATABLE : A L L O C A T A B L E;

ALLOCATE : A L L O C A T E;

PARAMETER : P A R A M E T E R;

PUBLIC : P U B L I C;

PRIVATE : P R I V A T E;

OPTIONAL : O P T I O N A L;

EXTERNAL : E X T E R N A L;

INTRINSIC : I N T R I N S I C;

SAVE : S A V E ;

TARGET : T A R G E T;

POINTER : P O I N T E R ;

PROTECTED : P R O T E C T E D ;

INTENT : I N T E N T;

CLASS : C L A S S;

DOUBLE : D O U B L E;

COMPLEX : C O M P L E X;

PRECISION : P R E C I S I O N;

CHARACTER : C H A R A C T E R;

KIND : K I N D;

LEN : L E N;

fragment IN : I N;

fragment OUT : O U T;

fragment INOUT : I N [ \t]* O U T;

INTENTION : IN | OUT | INOUT;

POWER : '**' ;

LNOT : '.not.' | '.NOT.' ;

LAND : '.and.' | '.AND.' ;

LOR : '.or.' | '.OR.' ;

EQV : '.eqv.' | '.EQV.' ;

NEQV : '.neqv.' | '.NEQV.' ;

XOR : '.xor.' | '.XOR.' ;

EOR : '.eor.' | '.EOR.' ;

LT : '.lt.' | '.LT.' | '<' ;

LE : '.le.' | '.LE.' | '<=' ;

GT : '.gt.' | '.GT.' | '>' ;

GE : '.ge.' | '.GE.' | '>=' ;

NE : '.ne.' | '.NE.' | '/=' ;

EQ : '.eq.' | '.EQ.' | '==' ;

TRUE : '.true.' | '.TRUE.' ;

FALSE : '.false.' | '.FALSE.' ;

DIMENSION : D I M E N S I O N ;

ERRMSG : E R R M S G;

SOURCE : S O U R C E;

MOLD : M O L D;

STAT : S T A T;

TYPE_SKIP : T Y P E .*? END [ \t]+ T Y P E -> skip;

TYPE : T Y P E;