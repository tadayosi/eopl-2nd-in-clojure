grammar Sec31_7;

options {
  output = AST;
  ASTLabelType = CommonTree;
}

tokens {
  A_PROGRAM;
  LIT_EXP; VAR_EXP; PRIMAPP_EXP; IF_EXP; LET_EXP; PROC_EXP; APP_EXP; LETREC_EXP;
  VARASSIGN_EXP; BEGIN_EXP;
  IDS; OPERANDS; PROC_DECLS; PROC_DECL; EXPS;
}

@header {
  package eopl.chap3;
}
@lexer::header {
  package eopl.chap3;
}

// *** scanner spec ************************************************************
WHITE_SP
  : (' ' | '\t' | '\r' | '\n')+ { skip(); }
  ;
COMMENT
  : '%' (~('\r' | '\n'))* { skip(); }
  ;
IDENTIFIER
  : ('a'..'z' | 'A'..'Z') ('a'..'z' | 'A'..'Z' | '0'..'9' | '?')*
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
  | 'proc' '(' identifiers ')' body=expression
    -> ^(PROC_EXP identifiers $body)
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
  ;
identifiers
  : id=IDENTIFIER? -> ^(IDS $id?)
  | ids+=IDENTIFIER (',' ids+=IDENTIFIER)+ -> ^(IDS $ids+)
  ;
procedureDeclaration
  : procName+=IDENTIFIER '(' ids=identifiers ')' '=' body=expression
    -> ^(PROC_DECL $procName $ids $body)
  ;
primitive
  : '+' | '-' | '*' | 'add1' | 'sub1' | 'zero?'
  ;