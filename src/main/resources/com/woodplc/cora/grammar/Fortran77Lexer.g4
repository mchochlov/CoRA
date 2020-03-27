/*	
 * Fortran 77 grammar for ANTLR 2.7.5
 * Adadpted from Fortran 77 PCCTS grammar by Olivier Dragon
 * Original PCCTS grammar by Terence Parr
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *
 */

/**
 * ported to Antlr4 by Tom Everett
 */

/*
 * Updated by Tom Everett, 2018
 */
lexer grammar Fortran77Lexer;
@lexer::members {
    public boolean fixedForm = true;
	int chars = 0;
}

tokens {FORMAT_STRING}

OPTIONAL : O P T I O N A L;

SUBMODULE : S U B M O D U L E;

MODULE : M O D U L E;

CONTAINS : 'contains' | 'CONTAINS';

PUBLIC : 'public' | 'PUBLIC';

PRIVATE : 'private' | 'PRIVATE';

PROGRAM : 'program' | 'PROGRAM' ;

ENTRY : 'entry' | 'ENTRY' ;

FUNCTION : 'function' | 'FUNCTION' ;

BLOCK : 'block' | 'BLOCK' ;

SUBROUTINE : 'subroutine' | 'SUBROUTINE' ;

END : E N D ;

DIMENSION : 'dimension' | 'DIMENSION' ;

DIM : 'dim' | 'DIM';

MASK : 'mask' | 'MASK';

REAL : 'REAL' | 'real' ;

EQUIVALENCE : 'EQUIVALENCE' | 'equivalence' ;

LEN : L E N;

COMMON : C O M M O N ;

POINTER : 'pointer' | 'POINTER' ;

BUFFERED : 'buffered' | 'BUFFERED' ;

IMPLICIT : 'implicit' | 'IMPLICIT' ;

NONE : N O N E ;

CHARACTER : C H A R A C T E R ;

ALLOCATABLE : 'allocatable' | 'ALLOCATABLE';

ALLOCATED : 'allocated' | 'ALLOCATED';

ALLOCATE : 'allocate' | 'ALLOCATE';

DEALLOCATE : 'deallocate' | 'DEALLOCATE';

PARAMETER : 'parameter' | 'PARAMETER';

EXTERNAL : 'external' | 'EXTERNAL';

NON_INTRINSIC : N O N UNDERSCORE I N T R I N S I C;

INTRINSIC : I N T R I N S I C;

SAVE : S A V E ;

DATA : D A T A ;

GO : G O ;

GOTO : G O T O ;

IF : I F ;

THEN : T H E N ;

FPP_ELSE : '#' E L S E;

ELSE : E L S E ;

ENDIF : E N D I F ;

SHARE : S H A R E;

ELSEIF : E L S E I F ;

DO : D O ;

CONTINUE : C O N T I N U E ;

STOP : S T O P;

ENDDO : E N D D O;

PAUSE : P A U S E ;

WRITE : W R I T E ;

READ : R E A D;

TARGET : T A R G E T;

PRINT : P R I N T ;

OPEN : O P E N ;

FMT : F M T ;

UNIT : U N I T ;

FLUSH : F L U S H;

IOMSG : I O M S G;

ERRMSG : 'ERRMSG' | 'errmsg';

SOURCE : 'SOURCE' | 'source';

MOLD : 'MOLD' | 'mold';

ERR : 'err' | 'ERR' ;

IOSTAT : 'IOSTAT' | 'iostat' ;
 
FORMAT : F O R M A T [ \t]* LPAREN -> pushMode(FORMAT_MODE) ;

LET : 'LET' | 'let' ;

CALL : 'CALL' | 'call' ;

RETURN : R E T U R N ;

ACTION : A C T I O N;

CLOSE : 'CLOSE' | 'close' ;

DOUBLE : 'DOUBLE' | 'double' ;

IOSTART : 'IOSTART' | 'iostart' ;

SEQUENTIAL : 'SEQUENTIAL' | 'sequential' ;

FPP_IFNDEF : '#' I F N D E F (~[\r\n])*;

FPP_IFDEF : ('#IFDEF' | '#ifdef') (~[\r\n])*;

FPP_ENDIF : '#endif' | '#ENDIF';

FPP_INCLUDE : '#include' | '#INCLUDE';

ICON : 'ICON' | 'icon' ;

LABEL : 'LABEL' | 'label' ;

FILE : 'file' | 'FILE' ;

CLASS : C L A S S;

STATUS : 'STATUS' | 'status' ;

STAT : 'STAT' | 'stat';

ACCESS : 'ACCESS' | 'access' ;

POSITION : 'POSITION' | 'position' ;

FORM : 'FORM' | 'form' ;

RECL : 'RECL' | 'recl' ;

BLANK : 'BLANK' | 'blank' ;

EXIST : 'EXIST' | 'exist' ;

OPENED : 'OPENED' | 'opened' ;

NUMBER : 'NUMBER' | 'number' ;

NAMED : 'NAMED' | 'named' ;

NAME_ : 'NAME' | 'name' ;

FORMATTED : 'FORMATTED' | 'formatted' ;

UNFORMATTED : 'UNFORMATTED' | 'unformatted' ;

NEXTREC : 'NEXTREC' | 'nextrec' ;

INQUIRE : 'INQUIRE' | 'inquire' ;

BACKSPACE : 'BACKSPACE' | 'backspace' ;

ENDFILE : 'ENDFILE' | 'endfile' ;

REWIND : R E W I N D ;

USE : 'USE' | 'use';

ONLY : 'ONLY' | 'only';

IMPURE : 'impure' | 'IMPURE';

PURE : 'pure' | 'PURE';

ELEMENTAL : 'elemental' | 'ELEMENTAL' ;

NON_RECURSIVE : 'non_recursive' | 'NON_RECURSIVE';

RECURSIVE : 'recursive' | 'RECURSIVE';

MINVAL : 'minval' | 'MINVAL';

MAXVAL : 'maxval' | 'MAXVAL';

RESULT : 'result' | 'RESULT';

ABSTRACT : 'abstract' | 'ABSTRACT';

EXTENDS : 'extends' | 'EXTENDS';

BINDC : B I N D [ \t]* LPAREN C;

PROCEDURE : 'procedure' | 'PROCEDURE';

NOPASS : 'nopass' | 'NOPASS';

PASS : 'pass' | 'PASS';

NON_OVERRIDABLE : 'non_overridable' | 'NON_OVERRIDABLE';

DEFERRED : 'deferred' | 'DEFERRED';

DOLLAR : '$' ;

DOT : '.';

COMMA : ',' ;

LPAREN : '(' ;

RPAREN : ')' ;

LBR : '[';

RBR : ']';

DOUBLE_COLON : '::';

COLON : ':' ;

SEMI_COLON : ';';

PERCENT : '%';

BOP : '=>';

PROTECTED : P R O T E C T E D ;

ASSIGNMENT : A S S I G N M E N T;

ASSIGN : '=' ;

REALC : NUM* RCON 
	| NUM+ (D | E) SIGN? NUM+
	;

MINUS : '-' ;

PLUS : '+' ;

DIV : '/' ;

STAR : '*' ;

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

XCON : 'XCON' ;

PCON : 'PCON' ;

FCON : 'FCON' ;

CCON : 'CCON' ;

HOLLERITH : 'HOLLERITH' ;

CONCATOP : 'CONCATOP' ;

CTRLDIRECT : 'CTRLDIRECT' ;

CTRLREC : 'CTRLREC' ;

TO : 'TO' | 'to' ;

SUBPROGRAMBLOCK : 'SUBPROGRAMBLOCK' ;

EXIT : 'exit' | 'EXIT';

DOBLOCK : 'DOBLOCK' ;

AIF : 'AIF' ;

THENBLOCK : 'THENBLOCK' ;

ELSEBLOCK : 'ELSEBLOCK' ;

CODEROOT : 'CODEROOT' ;

COMPLEX : 'COMPLEX' | 'complex' ;

PRECISION : 'PRECISION' | 'precision' ;

INTEGER : I N T E G E R ;

LOGICAL : 'LOGICAL' | 'logical' ;

INTENT : I N T E N T;

fragment IN : I N;

fragment OUT : O U T;

fragment INOUT : I N [ \t]* O U T;

INTENTION : IN | OUT | INOUT;

REC : 'REC' | 'rec';

CYCLE : 'CYCLE' | 'cycle';

SELECT : 'select' | 'SELECT';

CASE : 'case' | 'CASE';

DEFAULT : 'default' | 'DEFAULT';

WHILE : 'while' | 'WHILE';

TYPE : 'type' | 'TYPE';

INTERFACE :  I N T E R F A C E;

FINAL : F I N A L ;

OPERATOR : O P E R A T O R;

SUM : S U M;

SEQUENCE : 'sequence' | 'SEQUENCE';

KIND : K I N D ;

fragment CONTINUATION : ~ ('0' | ' ') ;

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

fragment ALNUM : (ALPHA | NUM) ;

fragment HEX : (NUM | 'a' .. 'f') ;

fragment SIGN : ('+' | '-') ;

fragment EXPON
   : (E | D ) (SIGN)? (NUM) + KIND_TYPE?
   ;

fragment KIND_TYPE : UNDERSCORE ('4' | '8' | '16');

fragment ALPHA
   : ('a' .. 'z') | ('A' .. 'Z')
   ;

fragment NUM : ('0' .. '9') ;

// '' is used to drop the charater when forming the lexical token
// Strings are assumed to start with a single quote (') and two
// single quotes is meant as a literal single quote

SCON : '\'' .*? '\'' C?;
//   : '\'' ('\'' '\'' | ~ ('\'' | '\n' | '\r') | (('\n' | '\r' ('\n')?) '     ' CONTINUATION) ('\n' | '\r' ('\n')?) '     ' CONTINUATION)* '\''
//   ;


RCON
   : '.' (NUM)* (EXPON)
   | '.' (NUM)+ (EXPON)?
   | '.' (NUM)* (KIND_TYPE | UNDERSCORE 'ReKi')
   ;

DEFINED_OPERATOR : '.' NAME '.';

UNDERSCORE : '_' ;

COMMENT
   : (C{getCharPositionInLine() == 1}? (~ [\r\n])* {fixedForm}?
   | '!' (~ [\r\n])*) -> channel(HIDDEN)
   ;
   
FORMAT_H : NUM+ H ~(',' | ')')+;

INTEGERC : NUM+;
	
NAME
   : (ALPHA | DOLLAR) (ALNUM | UNDERSCORE | DOLLAR)*
   ;

STRINGLITERAL
   : '"' .*? '"' C?
   ;

//comments after & get discarded
CON_EOL_FREE : '&' .*? '\r'? '\n' ([&\t ]* COMMENT '\r'? '\n')* [&\t ]* -> channel(HIDDEN);

CON_EOL : '\r'? '\n' [\t ]* ~(' '|'0'){getCharPositionInLine() == 6 && fixedForm}? -> channel(HIDDEN);

EOL : [\r\n] + ;
    
WS : [\t ] + -> channel(HIDDEN) ;


mode FORMAT_MODE;

F_SCON : '\'' .*? '\'' C?;

F_DIV : '/' ;

F_COLON : ':' ;

F_COMMA : ',' ;

F_DOLLAR : '$' ;

AWD : NUM* A NUM*;

FWD : NUM* F LT ('+'|ALNUM)+ GT '.' LT ('+'|ALNUM)+ GT
	| NUM* F NUM+ '.' NUM+;

EWD: NUM* E NUM+ '.' NUM+ (E NUM+)?;

IWD : NUM* I NUM+ ('.' NUM+)?;

F_H : F_INTEGERC H ({chars = Integer.parseInt(getText().substring(0, getText().length() - 1));}) -> pushMode(FH_STRING);

F_X : F_INTEGERC? X;

F_LPAREN : '(' -> pushMode(FORMAT_MODE);

F_RPAREN : ')' -> popMode;

F_COMMENT
   : (C{getCharPositionInLine() == 1}? (~ [\r\n])* {fixedForm}?
   | '!' (~ [\r\n])*) -> channel(HIDDEN)
   ;
   
F_INTEGERC : NUM+;
	
F_NAME
   : (ALPHA | DOLLAR) (ALNUM | UNDERSCORE | DOLLAR)*
   ;

F_STRINGLITERAL
   : '"' .*? '"' C?
   ;

//comments after & get discarded
F_CON_EOL_FREE : '&' .*? '\r'? '\n' ([&\t ]* COMMENT '\r'? '\n')* [&\t ]* -> channel(HIDDEN);

F_CON_EOL : '\r'? '\n' [\t ]* ~(' '|'0'){getCharPositionInLine() == 6 && fixedForm}? -> channel(HIDDEN);

F_EOL : [\r\n] + ;
    
F_WS : [\t ] + -> channel(HIDDEN) ;


mode FH_STRING;

fragment FHSTR_COMMENT
   : C{getCharPositionInLine() == 1}? (~ [\r\n])* {fixedForm}?
   | '!' (~ [\r\n])*
   ;

FHSTR_CON_EOL_FREE : '&' .*? '\r'? '\n' ([&\t ]* FHSTR_COMMENT '\r'? '\n')* [&\t ]* -> channel(HIDDEN);

FHSTR_CON_EOL : '\r'? '\n' [\t ]* ~(' '|'0'){getCharPositionInLine() == 6 && fixedForm}? -> channel(HIDDEN);

STR : . {chars == 1}? -> type(FORMAT_STRING), popMode;

STR_MORE : . {chars--;} -> type(FORMAT_STRING); 