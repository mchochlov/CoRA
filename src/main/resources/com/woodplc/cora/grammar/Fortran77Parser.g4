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
parser grammar Fortran77Parser;

@parser::members {

	java.util.Set<String> macros = new java.util.HashSet<String>(){{add("ASSERT");}};
}

options
   { tokenVocab = Fortran77Lexer; }

program
   : (executableUnit | EOL | commentStatement | includeStatement)+ EOF
   ;

executableUnit
   : functionSubprogram
   | mainProgram
   | subroutineSubprogram
   | blockdataSubprogram
   | module
   | submodule
   ;

mainProgram
   : (programStatement)? subprogramBody programEndStatement
   ;

functionSubprogram
   : functionStatement subprogramBody functionEndStatement
   ;

subroutineSubprogram
   : subroutineStatement subprogramBody subroutineEndStatement
   ;

blockdataSubprogram
   : blockdataStatement subprogramBody
   ;

module : moduleStatement moduleBody? moduleEndStatement;

otherSpecificationStatement
   : dimensionStatement
   | equivalenceStatement
   | intrinsicStatement
   | saveStatement
   ;

executableStatement
   : (gotoStatement | ifStatement | assignmentStatement | doStatement | selectStatement | continueStatement | stopStatement | 
   pauseStatement | readStatement | writeStatement | printStatement | rewindStatement | backspaceStatement | openStatement | 
   closeStatement | endfileStatement | inquireStatement | callStatement | returnStatement | formatStatement | exitStatement | 
   bindingStatement | flushStatement | cycleStatement | allocateStatement | deallocateStatement | allocatedStatement | 
   doWhileStatement | macroExecStatement)
   ;

programStatement : PROGRAM NAME EOL
   ;

programEndStatement : END (PROGRAM (NAME)?)?;

entryStatement
   : ENTRY NAME (LPAREN namelist RPAREN)?
   ;

functionStatement
   : (subPrefix | type)* FUNCTION subName (LPAREN (namelist)? RPAREN)? funcSuffix? EOL
   ;

funcSuffix : RESULT LPAREN identifier RPAREN;

blockdataStatement : BLOCK NAME
   ;

subroutineStatement
   : subPrefix? SUBROUTINE subName (LPAREN (namelist)? RPAREN bindTypeDeclStatement?)? EOL
   ;

subName : NAME | ALLOCATE | DEALLOCATE | READ | WRITE | NAME_;

subPrefix : MODULE | IMPURE | PURE | ELEMENTAL | NON_RECURSIVE | RECURSIVE;

namelist
   : identifier (COMMA identifier)*
   ;

moduleStatement : MODULE identifier;

moduleEndStatement : END (MODULE identifier?)?;

moduleBody : (wholeStatement | typeDefinition | interfaceDefinition)+ moduleContainsStatement?;

statement
   : entryStatement
   | implicitStatement
   | parameterStatement
   | intentStatement
   | typeStatement
   | commonStatement
   | useStatement
   | pointerStatement
   | externalStatement
   | otherSpecificationStatement
   | dataStatement
   | (statementFunctionStatement) statementFunctionStatement
   | executableStatement
   | commentStatement
   | fppStatement
   | publicStatement
   | privateStatement
   | containsStatement
   | procBindingStatement
   | optionalStatement
   | protectedStatement
   | finalStatement
   //| interfaceProcStatement
   ;

flushStatement : FLUSH LPAREN (UNIT ASSIGN)? expression1 (COMMA flushSpec)* RPAREN;

flushSpec : 
	ERR ASSIGN expression1
	| IOSTAT ASSIGN expression1
	| IOMSG ASSIGN expression1
	;

fppStatement : ifdefStatement | includeStatement | ifndefStatement;

includeStatement : FPP_INCLUDE expression1;

ifndefStatement : FPP_IFNDEF (wholeStatement| FPP_ELSE)+ FPP_ENDIF;

ifdefStatement : FPP_IFDEF (wholeStatement| FPP_ELSE)+ FPP_ENDIF;

publicStatement : PUBLIC (DOUBLE_COLON? namelist)?;

privateStatement : PRIVATE (DOUBLE_COLON? namelist)?;

subprogramBody : (wholeStatement|interfaceDefinition)*
   ;
   
containsStatement : CONTAINS;

optionalStatement : OPTIONAL DOUBLE_COLON? namelist;

protectedStatement : PROTECTED DOUBLE_COLON? namelist;

finalStatement : FINAL DOUBLE_COLON? namelist;

moduleContainsStatement : containsStatement EOL (subroutineSubprogram | functionSubprogram | EOL)*;

wholeStatement
   : (LABEL|INTEGERC)? statement (SEMI_COLON statement?)* EOL
   | EOL
   ;

subroutineEndStatement
   : LABEL? END (SUBROUTINE subName?)?
   ;

functionEndStatement : LABEL? END (FUNCTION NAME?)?;

macroExecStatement : {macros.contains(getCurrentToken().getText())}? identifier LPAREN exprList1? RPAREN;

exitStatement : EXIT identifier?;

cycleStatement : CYCLE identifier?;

allocateStatement : ALLOCATE LPAREN (typename DOUBLE_COLON)? exprList1 (COMMA allocOpt)* RPAREN;

allocOpt : optKeywords ASSIGN identifier;

optKeywords : STAT | ERRMSG | SOURCE | MOLD; 

deallocateStatement : DEALLOCATE LPAREN exprList1? (COMMA deallocOpt)* RPAREN;

deallocOpt : deoptKeywords ASSIGN identifier;

deoptKeywords : STAT | ERRMSG;

allocatedStatement : ALLOCATED LPAREN expression1 RPAREN;

interfaceProcStatement : MODULE? PROCEDURE DOUBLE_COLON? namelist;

interfaceDefinition : interfaceStatement ((subroutineSubprogram|functionSubprogram|interfaceProcStatement) EOL)+ interfaceEndStatement;

interfaceStatement : INTERFACE genericSpec? EOL;

genericSpec : identifier 
	| OPERATOR LPAREN (intrinsicOperator | DEFINED_OPERATOR ) RPAREN
	| ASSIGNMENT LPAREN ASSIGN RPAREN
	;

intrinsicOperator : PLUS | MINUS | '*' | POWER | DIV | LT | LE | LAND | LNOT | GT | GE | LOR;

interfaceEndStatement :  END INTERFACE genericSpec?;

submodule : submoduleStatement (wholeStatement | subroutineSubprogram | functionSubprogram)* submoduleEndStatement;

submoduleStatement : SUBMODULE LPAREN ancestorName (COLON parentName)? RPAREN identifier EOL;

ancestorName : identifier;

parentName : identifier;

submoduleEndStatement : END (SUBMODULE identifier?)?;

typeDefinition : typeDefStatement
	 wholeStatement* ((PRIVATE | SEQUENCE ) EOL)? typeContainsStatement? typeDefEndStatement;

typeDefStatement : TYPE ( typeAttrList? DOUBLE_COLON)? identifier (LPAREN namelist RPAREN)? EOL;

typeDefEndStatement : END TYPE identifier?;

typeAttrList : (COMMA typeAttribute)+;

typeAttribute :  (accessSpecifier | bindTypeDeclStatement | extendsStatement | ABSTRACT);

bindTypeDeclStatement : BINDC (COMMA NAME_ ASSIGN expression1)? RPAREN;

extendsStatement : EXTENDS LPAREN identifier RPAREN;

typeContainsStatement : wholeStatement+;

procBindingStatement : 
	PROCEDURE LPAREN identifier RPAREN COMMA bindingAttrList DOUBLE_COLON namelist
	| PROCEDURE ((COMMA bindingAttrList)? DOUBLE_COLON)? bindingList
	;

bindingList : (identifier | bindingStatement) (COMMA (identifier| bindingStatement))*;

bindingAttrList : bindingAttribute (COMMA bindingAttribute)*;

bindingAttribute : PASS | NOPASS | accessSpecifier | NON_OVERRIDABLE | DEFERRED; 

accessSpecifier : PUBLIC | PRIVATE;

bindingStatement : expression1 BOP expression1;

intentStatement
	: INTENT LPAREN INTENTION RPAREN DOUBLE_COLON? arrayDeclaratorExtents
	;
	
dimensionStatement
   : DIMENSION arrayDeclarators
   | DIMENSION LPAREN arrayDeclaratorExtents RPAREN
   ;

arrayDeclarator
   : identifier LPAREN arrayDeclaratorExtents RPAREN
   ;

arrayDeclarators
   : arrayDeclarator (COMMA arrayDeclarator)*
   ;

arrayDeclaratorExtents
   : arrayDeclaratorExtent (COMMA arrayDeclaratorExtent)*
   ;

arrayDeclaratorExtent
   : iexprCode (COLON (iexprCode | STAR))?
   | expression1
   | STAR
   ;

equivalenceStatement
   : EQUIVALENCE equivEntityGroup (COMMA equivEntityGroup)*
   ;

equivEntityGroup
   : LPAREN equivEntity (COMMA equivEntity)* RPAREN
   ;

equivEntity : varRef
   ;

commonStatement
   : COMMON (commonBlock (COMMA commonBlock)* | commonItems)
   ;

commonName
   : DIV ((NAME|INTENTION|PRINT|CASE| TYPE) DIV | DIV)
   ;

commonItem
   : NAME
   | arrayDeclarator
   ;

commonItems
   : commonItem (COMMA commonItem)*
   ;

commonBlock
   : commonName commonItems
   ;

commentStatement
    : COMMENT
    ;

useStatement : USE ((COMMA (INTRINSIC|NON_INTRINSIC))? DOUBLE_COLON)? identifier (COMMA ONLY COLON onlyList | COMMA renameList)?;

renameList : bindingStatement (COMMA bindingStatement)*;

onlyList : onlyListItem (COMMA onlyListItem)*;

onlyListItem : identifier | bindingStatement;

typeStatement
   : typename (COMMA (dimensionStatement|intentAttribute| PARAMETER| ALLOCATABLE| accessSpecifier | OPTIONAL | SAVE | EXTERNAL | TARGET | POINTER | PROTECTED))* DOUBLE_COLON? typeStatementNameList
//| characterWithLen typeStatementNameCharList
   ;

intentAttribute : INTENT LPAREN INTENTION RPAREN;

typeStatementNameList
   : typeStatementName (COMMA typeStatementName)*
   | assignmentStatement (COMMA assignmentStatement)*
   ;

typeStatementName
   : identifier
   | arrayDeclarator
   ;

typeStatementNameCharList
   : typeStatementNameChar (COMMA typeStatementNameChar)*
   ;

typeStatementNameChar
   : typeStatementName (typeStatementLenSpec)?
   ;

typeStatementLenSpec
   : STAR lenSpecification
   ;

typename
   : REAL (LPAREN expression1 RPAREN)? 
   | COMPLEX (LPAREN expression1 RPAREN) 
   | DOUBLE COMPLEX 
   | DOUBLE PRECISION 
   | INTEGER (LPAREN expression1 RPAREN)?
   | LOGICAL (LPAREN expression1 RPAREN)?
   | CHARACTER (LPAREN ((KIND|LEN) ASSIGN)? (expression1|STAR) RPAREN)?
   | CHARACTER STAR expression1
   | TYPE LPAREN identifier RPAREN
   | CLASS LPAREN identifier RPAREN
   ;

type
   : typename
   | characterWithLen
   ;

typenameLen : STAR ICON
   ;

pointerStatement
   : POINTER pointerDecl (COMMA pointerDecl)*
   ;

pointerDecl
   : LPAREN NAME COMMA NAME RPAREN
   ;

implicitStatement
   : IMPLICIT (implicitNone | implicitSpecs)
   ;

implicitSpec
   : type LPAREN implicitLetters RPAREN
   ;

implicitSpecs
   : implicitSpec (COMMA implicitSpec)*
   ;

implicitNone : NONE
   ;

implicitLetter : NAME
   ;

implicitRange
   : implicitLetter (MINUS implicitLetter)?
   ;

implicitLetters
   : implicitRange (COMMA implicitRange)*
   ;

lenSpecification
   : (LPAREN STAR RPAREN) LPAREN STAR RPAREN
   | ICON
   | LPAREN intConstantExpr RPAREN
   ;

characterWithLen
   : characterExpression (cwlLen)?
   ;

cwlLen
   : STAR lenSpecification
   ;

parameterStatement
   : PARAMETER LPAREN paramlist RPAREN
   ;

paramlist
   : paramassign (COMMA paramassign)*
   ;

paramassign
   : NAME ASSIGN constantExpr
   ;

externalStatement
   : EXTERNAL DOUBLE_COLON? namelist
   ;

intrinsicStatement
   : INTRINSIC namelist
   ;

saveStatement
   : SAVE (saveEntity (COMMA saveEntity)*)?
   ;

saveEntity
   : (NAME | DIV NAME DIV)
   ;

dataStatement
   : DATA dataStatementEntity ((COMMA)? dataStatementEntity)*
   ;

dataStatementItem
   : exprList1
   | dataImpliedDo
   ;

dataStatementMultiple
   : ((ICON | NAME) STAR)? (expression1 | NAME)
   ;

dataStatementEntity
   : varlist clist
   ;

varlist
   : dataStatementItem (COMMA dataStatementItem)* DIV
   ;

clist
   : dataStatementMultiple (COMMA dataStatementMultiple)* DIV
   ;

dataImpliedDo
   : LPAREN dataImpliedDoList COMMA dataImpliedDoRange RPAREN
   ;

dataImpliedDoRange
   : NAME ASSIGN expression1 COMMA expression1 (COMMA expression1)?
   ;

dataImpliedDoList
   : dataImpliedDoListWhat
   | COMMA dataImpliedDoList
   ;

dataImpliedDoListWhat
   : (expression1 | dataImpliedDo)
   ;

gotoStatement
   : ((GO | GOTO | GO TO) to?) (unconditionalGoto | computedGoto | assignedGoto)
   ;

unconditionalGoto : lblRef
   ;

computedGoto
   : LPAREN labelList RPAREN (COMMA)? integerExpr
   ;

lblRef
   : ICON
   | INTEGERC
   ;

labelList
   : lblRef (COMMA lblRef)*
   ;

assignedGoto
   : NAME ((COMMA)? LPAREN labelList RPAREN)?
   ;

selectStatement : (selectName COLON)? SELECT CASE LPAREN expression1 RPAREN EOL* (caseStatement)* defaultCaseStatement? endSelectStatement;

endSelectStatement : END SELECT (selectName)?;

caseStatement: CASE LPAREN exprList1 RPAREN (selectName COLON)? wholeStatement+;

defaultCaseStatement: CASE DEFAULT (selectName COLON)? wholeStatement+; 

selectName : NAME;

ifStatement
   : (ifName COLON)? IF LPAREN expression1 RPAREN (blockIfStatement | logicalIfStatement | arithmeticIfStatement)
   ;

ifName : NAME;

arithmeticIfStatement
   : lblRef COMMA lblRef COMMA lblRef
   ;

logicalIfStatement : executableStatement
   ;

blockIfStatement
   : firstIfBlock (: elseIfStatement)* (elseStatement)? endIfStatement
   ;

firstIfBlock
   : THEN wholeStatement +
   ;

elseIfStatement
   : (ELSEIF | (ELSE IF)) LPAREN logicalExpression RPAREN THEN (ifName)? wholeStatement +
   ;

elseStatement
   : ELSE (ifName)? wholeStatement +
   ;

endIfStatement : (ENDIF | (END IF)) (ifName)?;

doWhileStatement : (doName COLON)? DO (lblRef (COMMA)?)? WHILE LPAREN expression1 RPAREN wholeStatement+ (enddoStatement doName?)?;

doStatement
   : (doName COLON)? DO (doWithLabel | doWithEndDo)
   ;

doName : NAME;

doVarArgs
   : identifier ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)?
   ;

doWithLabel
   : lblRef (COMMA)? doVarArgs
   ;

doBody : (wholeStatement) +
   ;

doWithEndDo
   : doVarArgs? doBody enddoStatement
   ;

enddoStatement : (ENDDO | (END DO)) (doName)?;

continueStatement : CONTINUE ;

stopStatement
   : STOP (STRINGLITERAL | ICON | HOLLERITH)?
   ;

pauseStatement
   : PAUSE (ICON | HOLLERITH)
   ;

writeStatement : WRITE LPAREN controlInfoList RPAREN (COMMA? ioList2)? ;

readStatement : READ LPAREN controlInfoList RPAREN (COMMA? ioList2)?;

ioList2 : ioListItem2 (COMMA ioListItem2)*;

ioListItem2 :
	LPAREN ioList2 RPAREN
	| expression1
	| assignmentStatement
	;

printStatement
   : PRINT (formatIdentifier ((COMMA ioList) +)?)
   ;

assignmentStatement
   : lhsExpression ASSIGN rhsExpression
   ;

lhsExpression : expression1;

rhsExpression : expression1;

controlInfoList
   : controlInfoListItem (COMMA controlInfoListItem)*
   ;

controlErrSpec
   : controlErr ASSIGN (lblRef | NAME)
   ;

controlInfoListItem
   : unitIdentifier
   | expression1
   | (HOLLERITH | SCON)
   | controlFmt ASSIGN formatIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlRec ASSIGN integerExpr
   | REC ASSIGN expression1
   | controlEnd ASSIGN lblRef
   | controlErrSpec
   | controlIostat ASSIGN varRef
   ;

ioList
   : (ioListItem COMMA NAME ASSIGN) ioListItem
   | (ioListItem COMMA ioListItem) ioListItem COMMA ioList
   | ioListItem
   ;

ioListItem
   : (LPAREN ioList COMMA NAME ASSIGN) ioImpliedDoList
   | expression
   | expression1
   ;

ioImpliedDoList
   : LPAREN ioList COMMA NAME ASSIGN intRealDpExpr COMMA intRealDpExpr (COMMA intRealDpExpr)? RPAREN
   ;

openStatement
   : OPEN LPAREN openControl (COMMA openControl)* RPAREN
   ;

openControl
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlFile ASSIGN characterExpression
   | controlStatus ASSIGN characterExpression
   | (controlAccess | controlPosition) ASSIGN characterExpression
   | controlForm ASSIGN characterExpression
   | controlRecl ASSIGN integerExpr
   | controlBlank ASSIGN characterExpression
   | controlIostat ASSIGN varRef
   | controlBuffered ASSIGN characterExpression
   | SHARE ASSIGN characterExpression
   | ACTION ASSIGN characterExpression
   ;

controlBuffered : BUFFERED;

controlFmt : FMT ;

controlUnit : UNIT ;

controlRec : NAME ;

controlEnd : END ;

controlErr : ERR ;

controlIostat : IOSTAT ;

controlFile : FILE ;

controlStatus : STATUS ;

controlAccess : ACCESS ;

controlPosition : POSITION ;

controlForm : FORM ;

controlRecl : RECL ;

controlBlank : BLANK ;

controlExist : EXIST ;

controlOpened : OPENED ;

controlNumber : NUMBER ;

controlNamed : NAMED ;

controlName : NAME ;

controlSequential : SEQUENTIAL ;

controlDirect : NAME ;

controlFormatted : FORMATTED ;

controlUnformatted : UNFORMATTED ;

controlNextrec : NEXTREC ;

closeStatement
   : CLOSE LPAREN closeControl (COMMA closeControl)* RPAREN
   ;

closeControl
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlStatus ASSIGN characterExpression
   | controlIostat ASSIGN varRef
   ;

inquireStatement
   : INQUIRE LPAREN inquireControl (COMMA inquireControl)* RPAREN
   ;

inquireControl
   : controlUnit ASSIGN unitIdentifier
   | controlFile ASSIGN characterExpression
   | controlErrSpec
   | (controlIostat | controlExist | controlOpened | controlNumber | controlNamed | controlName | controlAccess | controlSequential | controlDirect | controlForm | controlFormatted | controlUnformatted | controlRecl | controlNextrec | controlBlank) ASSIGN varRef
   | unitIdentifier
   ;

backspaceStatement : BACKSPACE berFinish ;

endfileStatement : ENDFILE berFinish ;

rewindStatement : REWIND berFinish ;

berFinish
   : unitIdentifier 
   | LPAREN berFinishItem (COMMA berFinishItem)* RPAREN
   ;

berFinishItem
   : unitIdentifier
   | controlUnit ASSIGN unitIdentifier
   | controlErrSpec
   | controlIostat ASSIGN varRef
   ;

unitIdentifier
   : expression1
   | STAR
   ;

formatIdentifier
   : (SCON | HOLLERITH)
   | iexpr
   | expression1
   | STAR
   ;

formatStatement
   : FORMAT fmtSpec F_RPAREN
   ;

fmtSpec
   : (formatedit | formatsep (formatedit)?) (formatsep (formatedit)? | F_COMMA (formatedit | formatsep (formatedit)?))*
   ;

formatsep
   : F_DIV
   | F_COLON
   | F_DOLLAR
   ;

formatedit
   : editElement;
   //XCON
   //| editElement
   //| ICON editElement
   //| (PLUS | MINUS)? PCON ((ICON)? editElement)?
   //;

editElement
   : (stringEditElement | dataEditElement | controlEditElement | F_INTEGERC)
   | F_INTEGERC? F_LPAREN fmtSpec F_RPAREN
   ;

dataEditElement : FWD | EWD | IWD | AWD;

controlEditElement: (F_SCON)? F_X | F_X (F_SCON)?;

stringEditElement :  F_H FORMAT_STRING* | F_STRINGLITERAL | F_SCON;

statementFunctionStatement
   : LET sfArgs ASSIGN expression
   ;

sfArgs
   : NAME LPAREN namelist RPAREN
   ;

callStatement
   : CALL subroutineCall
   ;

subroutineCall
   : identifier (LPAREN (callArgumentList)? RPAREN)? expression1?
   ;

callArgumentList
   : callArgument (COMMA callArgument)*
   ;

callArgument
   : expression1
   | STAR lblRef
   ;

returnStatement
   : RETURN (integerExpr)?
   ;

expression1 
	: intrinsicFunction
	| identifier LPAREN exprList1 RPAREN expression1
	| arrayOrFunctionExpression				
	| expression1 (EQ | NE | GT | GE | LT | LE | LOR | LAND | DEFINED_OPERATOR) expression1
	| (LNOT | DEFINED_OPERATOR) expression1
	| expression1 POWER expression1
	| expression1 ('*' | DIV) expression1
	| expression1 ('+' | '-') expression1
	| expression1 DIV DIV expression1
	| ('+'|'-') expression1
	| identifier
	| INTEGERC DOT?
	| REALC
	| (STRINGLITERAL | SCON)
	| logicalConstant
	| LPAREN DIV exprList1 DIV RPAREN
	| LBR exprList1 RBR
	| LPAREN expression1 RPAREN
	| expression1 COLON expression1
	| COLON
	| expression1 COLON
	| COLON expression1
	| expression1 PERCENT expression1
	| PERCENT expression1
	;
	
arrayOrFunctionExpression : identifier LPAREN exprList1? RPAREN;

intrinsicFunction : allocatedStatement
	| allocateStatement
	| deallocateStatement
	| minvalStatement 
	| maxvalStatement
	| sumStatement;

minvalStatement : MINVAL LPAREN expression1 dimParam? maskParam? RPAREN;

maxvalStatement : MAXVAL LPAREN expression1 dimParam? maskParam? RPAREN;

sumStatement : SUM LPAREN expression1 dimParam? maskParam? RPAREN;

dimParam : COMMA DIM ASSIGN expression1;

maskParam : COMMA MASK ASSIGN expression1;

exprList1 : expression1 ((COMMA|COLON) expression1)*;

expression
   : ncExpr (COLON ncExpr)?
   ;

ncExpr
   : lexpr0 (concatOp lexpr0)*
   ;

lexpr0
   : lexpr1 ((NEQV | EQV) lexpr1)*
   ;

lexpr1
   : lexpr2 (LOR lexpr2)*
   ;

lexpr2
   : lexpr3 (LAND lexpr3)*
   ;

lexpr3
   : LNOT lexpr3
   | lexpr4
   | INTEGERC
   ;

lexpr4
   : aexpr0 ((LT | LE | EQ | NE | GT | GE) aexpr0)?
   ;

aexpr0
   : aexpr1 ((PLUS | MINUS) aexpr1)*
   ;

aexpr1
   : aexpr2 ((STAR | DIV) aexpr2)*
   ;

aexpr2
   : (PLUS | MINUS)* aexpr3
   ;

aexpr3
   : aexpr4 (POWER aexpr4)*
   ;

aexpr4
   : (unsignedArithmeticConstant) unsignedArithmeticConstant
   | (HOLLERITH | SCON)
   | logicalConstant
   | varRef
   | LPAREN expression RPAREN
   ;

iexpr
   : iexpr1 ((PLUS | MINUS) iexpr1)*
   ;

iexprCode
   : iexpr1 ((PLUS | MINUS) iexpr1)*
   | INTEGERC
   ;

iexpr1
   : iexpr2 ((STAR | DIV) iexpr2)*
   ;

iexpr2
   : (PLUS | MINUS)* iexpr3
   ;

iexpr3
   : iexpr4 (POWER iexpr3)?
   ;

iexpr4
   : ICON
   | varRefCode
   | LPAREN iexprCode RPAREN
   ;

constantExpr
   : expression
   ;

arithmeticExpression
   : expression
   ;

integerExpr
   : expression1
   ;

intRealDpExpr
   : expression1
   | expression
   ;

arithmeticConstExpr
   : expression
   ;

intConstantExpr
   : expression
   ;

characterExpression
   : expression1
   ;

concatOp
   : DIV DIV
   ;

logicalExpression
   : expression
   | expression1
   ;

logicalConstExpr
   : expression
   ;

subscripts
   : LPAREN (expression (COMMA expression)*)? RPAREN
   ;

varRef
   : (identifier | REAL) (subscripts (substringApp)?)?
   ;

varRefCode
   : NAME (subscripts (substringApp)?)?
   ;

substringApp
   : LPAREN (ncExpr)? COLON (ncExpr)? RPAREN
   ;



unsignedArithmeticConstant
   : (ICON | RCON)
   | complexConstant
   ;

complexConstant
   : LPAREN ((PLUS | MINUS))? (ICON | RCON) COMMA ((PLUS | MINUS))? (ICON | RCON) RPAREN
   ;

logicalConstant : (TRUE | FALSE)
   ;

// needed because Fortran doesn't have reserved keywords. Putting the rule
// 'keyword" instead of a few select keywords breaks the parser with harmful
// non-determinisms
identifier
   : NAME
   | REAL
   | ERR
   | INTENTION
   | NONE
   | NAME_
   | ICON
   | NUMBER
   | LEN
   | IF
   | DEFAULT
   | LABEL
   | DIM
   | EXIT
   | ERRMSG
   | STATUS
   | READ
   | WRITE
   | POSITION
   | ALLOCATE
   | DEALLOCATE
   | SUM
   | KIND
   ;

to : NAME
   ;