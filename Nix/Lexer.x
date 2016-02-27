{
module Lexer where

import Nix.Lexer2
}

@ID    = [a-zA-Z\_][a-zA-Z0-9\_\'\-]*
@INT   = [0-9]+
@FLOAT = (([1-9] [0-9]* \. [0-9]* ) | (0? \. [0-9]+)) ([Ee] [\+ \-]? [0-9]+)?
@PATH  = [a-zA-Z0-9\.\_\-\+]*(\/[a-zA-Z0-9\.\_\-\+]+)+
@HPATH = \~(\/[a-zA-Z0-9\.\_\-\+]+)+
@SPATH = \<[a-zA-Z0-9\.\_\-\+]+(\/[a-zA-Z0-9\.\_\-\+]+)*\>
@URI   = [a-zA-Z][a-zA-Z0-9\+\-\.]*\:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']+

@WHITESPACE     = [\ \t\r\n]+
@SINGLE_COMMENT = \#[^\r\n]*
@LONG_COMMENT   = \/\*([^\*]|\*[^\/])*\*\/

nix :-

<0,inside_dollar_curly> {
if          { return IF }
then        { return THEN }
else        { return ELSE }
assert      { return ASSERT }
with        { return WITH }
let         { return LET }
in          { return IN }
rec         { return REC }
inherit     { return INHERIT }
or          { return OR_KW }
\.\.\.      { return ELLIPSIS }

\=\=        { binaryOp NEq }
\!\=        { binaryOp NNeq }
\<\=        { binaryOp NLte }
\>\=        { binaryOp NGte }
\&\&        { binaryOp NAnd }
\|\|        { binaryOp NOr }
\-\>        { binaryOp NImpl }
\/\/        { binaryOp NUpdate }
\+\+        { binaryOp NConcat }

@ID        { return ID }
@INT       { return INT }
@FLOAT     { return FLOAT }

\$\{        { push_st inside_dollar_curly >> return DOLLAR_CURLY }
}

<inside_dollar_curly>\}      { pop_st >> return RBRACE }
\}                           { return RBRACE }
<inside_dollar_curly>\{      { push_st inside_dollar_curly >> return LBRACE }
\{                           { return LBRACE }

<0,inside_dollar_curly>\"          { push_st STRING >> return QUOTE }
<STRING>([^\$\"\\]|\$[^\{\"\\]|\\.|\$\\.)*\$ / \" { return STR }
<STRING>([^\$\"\\]|\$[^\{\"\\]|\\.|\$\\.)+ { return STR }
<STRING>\$\{  { push_st inside_dollar_curly >> return DOLLAR_CURLY }
<STRING>\"  { pop_st >> return QUOTE }
<STRING> .  { error "shouldn't be reached" }

<0,inside_dollar_curly>\'\'(\ *\n)?     { push_st IND_STRING >> return IND_STRING_OPEN }
<IND_STRING>([^\$\']|\$[^\{\']|\'[^\'\$])+ { return IND_STR }
<IND_STRING>\'\'\$ { return $ IND_STR "$" }
<IND_STRING>\'\'\' { return $ IND_STR "''" }
<IND_STRING>\'\'\\. { return $ IND_STR "+2" ) }
<IND_STRING>\$\{ { push_st inside_dollar_curly >> return DOLLAR_CURLY }
<IND_STRING>\'\' { pop_st >> return IND_STRING_CLOSE }
<IND_STRING>\'   { return $ IND_STR "'" }
<IND_STRING>.    { error "shouldn't be reached" }

<0,inside_dollar_curly>{

@PATH      { return PATH }
@HPATH     { return HPATH }
@SPATH     { return SPATH }
@URI       { return URI }

@WHITESPACE;
@SINGLE_COMMENT;
@LONG_COMMENT;

.          { yytext[0] }

}
