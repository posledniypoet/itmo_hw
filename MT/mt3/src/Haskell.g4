grammar Haskell;

@header {

package haskell;

import java.util.*;
}


file: (line ('\n')+)*;

line: func_type
    | func_dec;

func_type: NAME '::' type;

type: type0 type_cont;

type0: TYPE
     | '(' type ')';

type_cont: '->' type | ;

func_dec: NAME value* func_body;

value: NAME | INT;

func_body: '=' let_in
         | '=' math
         | if_
         | '=' newif;

newif:
      'if' math 'then' math ifCont?;

ifCont:
       'else' (math | newif);

let_in:
    'let' NAME '=' math 'in' '\n'
        (let_in | math);

if_:
    if_block
    elif_block;

if_block:
    '|' math '=' (let_in | math);

elif_block: '\n'
    if_block
    elif_block | ;

math: math ('*' | '/') math
    | math ('+' | '-') math
    | math ('<' | '>') math
    | math ('==') math
    | math ('&') math
    | math ('|') math
    | '!' math
    | argument;

argument: INT
        | '(' math ')'
        | NAME ('(' math (',' math)* ')')?;

WS: [ \t\r]+ -> skip;

INT: [0-9]+;
NAME: [a-z]([a-z] | [0-9] | '-' | '_')*;
TYPE: [A-Z]([a-z] | [A-Z] | [0-9])*;