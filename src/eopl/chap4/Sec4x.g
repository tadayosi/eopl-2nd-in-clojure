grammar Sec4x;

options {
  output = AST;
  ASTLabelType = CommonTree;
}

tokens {
  A_PROGRAM;
  LIT_EXP; VAR_EXP; PRIMAPP_EXP; IF_EXP; LET_EXP; PROC_EXP; APP_EXP; LETREC_EXP;
  VARASSIGN_EXP; BEGIN_EXP; TRUE_EXP; FALSE_EXP; LETTYPE_EXP;
  ID; IDS; OPERANDS; PROC_DECLS; PROC_DECL; EXPS;
  TYPED_ID; TYPED_IDS;
  INT_TYPE_EXP; BOOL_TYPE_EXP; PROC_TYPE_EXP; TID_TYPE_EXP;
  ARG_TYPE_EXPS;
}

@header {
  package eopl.chap4;
}
@lexer::header {
  package eopl.chap4;
}

// *** scanner spec ************************************************************
WHITE_SP
  : (' ' | '\t' | '\r' | '\n')+ { $channel = HIDDEN; }
  ;
COMMENT
  : '%' (~('\r' | '\n'))* { skip(); }
  ;
IDENTIFIER
  : ('a'..'z' | 'A'..'Z') ('a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '?')*
  ;
NUMBER
  : ('0'..'9')+
  ;

// *** grammar *****************************************************************
program
  : exp=expression -> ^(A_PROGRAM $exp)
  ;
expression
  : datum=NUMBER -> ^(LIT_EXP $datum)
  | id=IDENTIFIER -> ^(VAR_EXP $id)
  | prim=primitive '(' rands+=expression (',' rands+=expression)* ')'
    -> ^(PRIMAPP_EXP $prim ^(OPERANDS $rands+))
  | 'if' testExp=expression 'then' trueExp=expression 'else' falseExp=expression
    -> ^(IF_EXP $testExp $trueExp $falseExp)
  | 'let' (ids+=IDENTIFIER '=' rands+=expression)* 'in' body=expression
    -> ^(LET_EXP ^(IDS $ids*) ^(OPERANDS $rands*) $body)
  | 'proc' '(' typedIds ')' body=expression
    -> ^(PROC_EXP typedIds $body)
  | '(' rator=expression rands+=expression* ')'
    -> ^(APP_EXP $rator ^(OPERANDS $rands*))
  | 'letrec'
      procedureDeclaration*
    'in' letrecBody=expression
    -> ^(LETREC_EXP ^(PROC_DECLS procedureDeclaration*) $letrecBody)
  | 'set' id=IDENTIFIER '=' rhsExp=expression
    -> ^(VARASSIGN_EXP $id $rhsExp)
  | 'begin' exp=expression (';' exps+=expression)* 'end'
    -> ^(BEGIN_EXP $exp ^(EXPS $exps*))
  | 'true' -> ^(TRUE_EXP)
  | 'false' -> ^(FALSE_EXP)
  | 'lettype' typeName=IDENTIFIER '=' texp=typeExp
      procedureDeclaration*
    'in' lettypeBody=expression
    -> ^(LETTYPE_EXP $typeName $texp ^(PROC_DECLS procedureDeclaration*) $lettypeBody)
  ;
typedIds
  : -> ^(TYPED_IDS)
  | typedId (',' typedId)* -> ^(TYPED_IDS typedId+)
  ;
typedId
  : argTexp=typeExp id=IDENTIFIER -> ^(TYPED_ID $argTexp $id)
  ;
procedureDeclaration
  : resultTexp=typeExp procName=IDENTIFIER '(' ids=typedIds ')' '=' body=expression
    -> ^(PROC_DECL $resultTexp $procName $ids $body)
  ;
primitive
  : '+' | '-' | '*' | 'add1' | 'sub1' | 'zero?'
  ;
typeExp
  : 'int' -> ^(INT_TYPE_EXP)
  | 'bool' -> ^(BOOL_TYPE_EXP)
  | '(' argTexps=argTypeExps '->' resultTexp=typeExp ')'
    -> ^(PROC_TYPE_EXP $argTexps $resultTexp)
  | id=IDENTIFIER -> ^(TID_TYPE_EXP $id)
  ;
argTypeExps
  : -> ^(ARG_TYPE_EXPS)
  | argTexps+=typeExp ('*' argTexps+=typeExp)* -> ^(ARG_TYPE_EXPS $argTexps+)
  ;
