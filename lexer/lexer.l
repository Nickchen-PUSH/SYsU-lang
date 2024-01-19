/* Definitions Section */
%{
#include <cctype>
#include <cstdio>
#include <string>
#define YYEOF 0
int yylex();
int main() {
  do {
  } while (yylex() != YYEOF);
}
std::string yyloc = "<stdin>";
int yyrow = 1, yycolumn = 1, yycolpre = 1;
#define YY_USER_ACTION                                                         \
  do {                                                                         \
    yycolumn += yyleng;                                                        \
  } while (0);
%}
%option noyywrap
%%
    /* Rules Section */
#.* {
  std::string s(yytext);
  auto l = s.find("\""), r = s.rfind("\"");
  yyloc = s.substr(l + 1, r - l - 1);
  for (int i = 0; i < s.size(); ++i)
    if (std::isdigit(s[i])) {
      for (yyrow = 0; i < s.size() && std::isdigit(s[i]); ++i)
        yyrow = yyrow * 10 + s[i] - '0';
      --yyrow;
      break;
    }
}
\n {
  ++yyrow;
  yycolpre = yycolumn;
  yycolumn = 1;
}
[\f\r\t\v ] {}
int {
  std::fprintf(yyout, "int '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
char {
  std::fprintf(yyout, "char '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
long {
  std::fprintf(yyout, "long '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
float {
  std::fprintf(yyout, "float '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
void {
  std::fprintf(yyout, "void '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
const {
  std::fprintf(yyout, "const '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
static {
  std::fprintf(yyout, "static '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
auto {
  std::fprintf(yyout, "auto '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
break {
  std::fprintf(yyout, "break '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
continue {
  std::fprintf(yyout, "continue '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
return {
  std::fprintf(yyout, "return '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
if {
  std::fprintf(yyout, "if '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
else {
  std::fprintf(yyout, "else '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
do {
  std::fprintf(yyout, "do '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
while {
  std::fprintf(yyout, "while '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
for {
  std::fprintf(yyout, "for '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\( {
  std::fprintf(yyout, "l_paren '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\) {
  std::fprintf(yyout, "r_paren '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\[ {
  std::fprintf(yyout, "l_square '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\] {
  std::fprintf(yyout, "r_square '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\{ {
  std::fprintf(yyout, "l_brace '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\} {
  std::fprintf(yyout, "r_brace '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
, {
  std::fprintf(yyout, "comma '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
: {
  std::fprintf(yyout, "colon '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\? {
  std::fprintf(yyout, "quetion '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
; {
  std::fprintf(yyout, "semi '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\* {
  std::fprintf(yyout, "star '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
== {
  std::fprintf(yyout, "equalequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
= {
  std::fprintf(yyout, "equal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\<\<= {
  std::fprintf(yyout, "lesslessequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\<\< {
  std::fprintf(yyout, "lessless '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\<= {
  std::fprintf(yyout, "lessequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\< {
  std::fprintf(yyout, "less '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
>>= {
  std::fprintf(yyout, "greatergreaterequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
>> {
  std::fprintf(yyout, "greatergreater '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
>= {
  std::fprintf(yyout, "greaterequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
> {
  std::fprintf(yyout, "greater '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\+\+ {
  std::fprintf(yyout, "plusplus '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\+= {
  std::fprintf(yyout, "plusequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\+ {
  std::fprintf(yyout, "plus '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\-\- {
  std::fprintf(yyout, "minusminus '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\-= {
  std::fprintf(yyout, "minusequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\- {
  std::fprintf(yyout, "minus '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
&& {
  std::fprintf(yyout, "ampamp '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
&= {
  std::fprintf(yyout, "ampequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
& {
  std::fprintf(yyout, "amp '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\|\| {
  std::fprintf(yyout, "pipepipe '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\|= {
  std::fprintf(yyout, "pipeequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\| {
  std::fprintf(yyout, "pipe '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\^= {
  std::fprintf(yyout, "careteequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\^ {
  std::fprintf(yyout, "carete '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
~ {
  std::fprintf(yyout, "tilde '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
!= {
  std::fprintf(yyout, "exclaimequal '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
! {
  std::fprintf(yyout, "exclaim '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\% {
  std::fprintf(yyout, "percent '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\/ {
  std::fprintf(yyout, "slash '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\"([^"\\]|\\.)*\" {
  std::fprintf(yyout, "string_literal '%s'\t\tLoc=<%s:%d:%d>\n", yytext,
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
0x([0-9a-fA-F]*)\.?[0-9a-fA-F]*([pPeE][-+]?[0-9a-fA-F]+)?|0b[01]*\.?[01]*|[0-9]+(\.?)[0-9]*([eE][-+]?[0-9]*)?|[0-9]*\.?[0-9]+(\.?)[0-9]*([eE][-+]?[0-9]*)? {
  std::fprintf(yyout, "numeric_constant '%s'\t\tLoc=<%s:%d:%d>\n", yytext,
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\.\.\. {
  std::fprintf(yyout, "ellipsis '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\. {
  std::fprintf(yyout, "period '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
[a-zA-Z_][a-zA-Z_0-9]* {
  std::fprintf(yyout, "identifier '%s'\t\tLoc=<%s:%d:%d>\n", yytext,
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
<<EOF>> {
  std::fprintf(yyout, "eof ''\t\tLoc=<%s:%d:%d>\n", yyloc.c_str(), yyrow - 1,
               yycolpre - yyleng);
  return YYEOF;
}
. {
  std::fprintf(yyout, "unknown '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
%%
/* User Code Section */