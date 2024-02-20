%code requires{
  #include "parser.hh"
  #include <llvm/Support/JSON.h>
  #include <llvm/Support/MemoryBuffer.h>
  #include <llvm/Support/raw_ostream.h>
  #include "/root/SYsU-lang-latest/SYsU-lang/parser/asg.hpp"
  extern Obj* gRoot;
%}
%union {
  Obj* Obj;
  Expr* Expr;
  Decl* Decl;
  IntegerLiteral* IntegerLiteral;
  FloatingLiteral* FloatingLiteral;
  DeclRefExpr* DeclRefExpr;
  BinaryExpr* BinaryExpr;
  UnaryOp* UnaryOp;
}
%{
#define yyerror(x)                                                             \
  do {                                                                         \
    llvm::errs() << (x);                                                       \
  } while (0)
namespace {
auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
auto input = llvmin.get() -> getBuffer();
auto end = input.end(), it = input.begin();
auto wk_getline(char endline = "\n"[0]) {
  auto beg = it;
  while (it != end && *it != endline)
    ++it;
  auto len = it - beg;
  if (it != end && *it == endline)
    ++it;
  return llvm::StringRef(beg, len);
}
llvm::json::Array stak;
} // namespace
auto yylex() {
  auto tk = wk_getline();
  auto b = tk.find("'") + 1, e = tk.rfind("'");
  auto s = tk.substr(b, e - b).str(), t = tk.substr(0, tk.find(" ")).str();
  if (t == "numeric_constant") {
    // stak.push_back(
    //     llvm::json::Object{{"kind", "IntegerLiteral"}, {"value", s}});
    if (s.find('.') != std::string::npos || s.find('p') != std::string::npos ||
        s.find('e') != std::string::npos) {
      yylval->kind = "FloatingLiteral";
      yylval->value = s;
      llvm::StringRef str(yylval->value);
      llvm::APFloat apf(0.0);
      apf.convertFromString(str, llvm::APFloat::rmNearestTiesToEven);
      llvm::SmallString<16> Buffer;
      apf.toString(Buffer);
      yylval->value = Buffer.c_str();
      return T_NUMERIC_CONSTANT;
    }
    else{
      yylval.IntegerLiteral = Mgr::g.make<IntegerLiteral>();
      yylval.IntegerLiteral->val=atol(s);
      return T_NUMERIC_CONSTANT;
    }    
  }
  if (t == "identifier") {
    // stak.push_back(llvm::json::Object{{"value", s}});
    yylval.Decl = Mgr::g.make<Decl>();
    yylval.Decl->name = s;
    return T_IDENTIFIER;
  }

  if (t == "int")
    return T_INT;
  if (t == "char")
    return T_CHAR;
  if (t == "long")
    return T_LONG;
  if (t == "float")
    return T_FLOAT;

  if (t == "void")
    return T_VOID;
  if (t == "const")
    return T_CONST;
  if (t == "static")
    return T_STATIC;
  if (t == "auto")
    return T_AUTO;

  if (t == "continue")
    return T_CONTINUE;
  if (t == "break")
    return T_BREAK;

  if (t == "if")
    return T_IF;
  if (t == "else")
    return T_ELSE;
  if (t == "do")
    return T_DO;
  if (t == "while")
    return T_WHILE;
  if (t == "for")
    return T_FOR;
  if (t == "return")
    return T_RETURN;

  if (t == "equalequal")
    return T_EQUALEQUAL;
  if (t == "equal")
    return T_EQUAL;
  if (t == "lesslessequal")
    return T_LESSLESSEQUAL;
  if (t == "lessless")
    return T_LESSLESS;
  if (t == "lessequal")
    return T_LESSEQUAL;
  if (t == "less")
    return T_LESS;
  if (t == "greatergreaterequal")
    return T_GREATERGREATEREQUAL;
  if (t == "greatergreater")
    return T_GREATERGREATER;
  if (t == "greaterequal")
    return T_GREATEREQUAL;
  if (t == "greater")
    return T_GREATER;
  if (t == "plusplus")
    return T_PLUSPLUS;
  if (t == "plusequal")
    return T_PLUSEQUAL;
  if (t == "plus")
    return T_PLUS;
  if (t == "minusminus")
    return T_MINUSMINUS;
  if (t == "minusequal")
    return T_MINUSEQUAL;
  if (t == "minus")
    return T_MINUS;

  if (t == "ampamp")
    return T_AMPAMP;
  if (t == "ampequal")
    return T_AMPEQUAL;
  if (t == "amp")
    return T_AMP;
  if (t == "pipepipe")
    return T_PIPEPIPE;
  if (t == "pipeequal")
    return T_PIPEEQUAL;
  if (t == "pipe")
    return T_PIPE;  
  if (t == "careteequal")
    return T_CARETEEQUAL;
  if (t == "carete")
    return T_CARETE;
  if (t == "exclaimequal")
    return T_EXCLAIMEQUAL;
  if (t == "exclaim")
    return T_EXCLAIM;
    
  if (t == "tilde")
    return T_TILDE;
  if (t == "percent")
    return T_PERCENT;
  if (t == "slash")
    return T_SLASH;
  if (t == "ellipsis")
    return T_ELLIPSIS;
  if (t == "period")
    return T_PERIOD;
  
  if (t == "semi")
    return T_SEMI;
  if (t == "comma")
    return T_COMMA;
  if (t == "colon")
    return T_COLON;
  if (t == "question")
    return T_QUESTION;
  if (t == "l_paren")
    return T_L_PAREN;
  if (t == "r_paren")
    return T_R_PAREN;
  if (t == "l_brace")
    return T_L_BRACE;
  if (t == "r_brace")
    return T_R_BRACE;
  if (t == "l_square")
    return T_L_SQUARE;
  if (t == "r_square")
    return T_R_SQUARE;
  
  if (t == "star")
    return T_STAR;
  
  return YYEOF;
}
int main() {
  yyparse();
  llvm::outs() << stak.back() << "\n";
}
%}



%token <IntegerLiteral> T_NUMERIC_CONSTANT
%token <Decl> T_IDENTIFIER
%token T_INT
%token T_RETURN
%token T_SEMI
%token T_L_PAREN
%token T_R_PAREN
%token T_L_BRACE
%token T_R_BRACE
%token T_STATIC
%token T_AUTO
%token T_FOR
%token T_LESSLESSEQUAL
%token T_LESSLESS
%token T_GREATERGREATEREQUAL
%token T_GREATERGREATER
%token T_PLUSPLUS
%token T_PLUSEQUAL
%token T_MINUSMINUS
%token T_MINUSEQUAL
%token T_AMPEQUAL
%token T_AMP
%token T_PIPEEQUAL
%token T_PIPE
%token T_CARETEEQUAL
%token T_CARETE
%token T_TILDE
%token T_PERCENT
%token T_STAR
%token T_SLASH
%token T_L_SQUARE
%token T_R_SQUARE
%token T_PLUS
%token T_MINUS
%token T_EXCLAIM
%token T_EQUALEQUAL
%token T_EXCLAIMEQUAL
%token T_AMPAMP
%token T_PIPEPIPE
%token T_CONST
%token T_EQUAL
%token T_COMMA
%token T_CHAR
%token T_LONG
%token T_IF
%token T_ELSE
%token T_WHILE
%token T_DO
%token T_BREAK
%token T_CONTINUE
%token T_LESS
%token T_GREATER
%token T_LESSEQUAL
%token T_GREATEREQUAL
%token T_VOID
%token T_ELLIPSIS
%token T_PERIOD
%token T_COLON
%token T_QUESTION
%token T_FLOAT
%token T_STRING_LITERAL

/* %nterm VarDecl
%nterm ConstDecl
%nterm ConstDef
%nterm ConstInitVal */

/* %nterm PrimaryExp
%nterm UnaryExp
%nterm Expression
%nterm MulExpr
%nterm AddExpr
%nterm RelExp
%nterm EqExp
%nterm LAndExp
%nterm LOrExp
%nterm ConstExp
%nterm LVal */

%right THEN T_ELSE

%precedence PREC2
%precedence PREC1
%precedence PREC0

%type <Decl> Declaration
%type <Decl> VarDecl
%type <Decl> ConstDecl
%type <Decl> LVal
%type <Decl> Stmt

%type <Expr> ConstDef
%type <Expr> VarDef
%type <Expr> ConstInitVal
%type <Expr> InitVal
%type <Expr> ConstExp
%type <Expr> PrimaryExp
%type <Expr> UnaryExp
%type <Expr> Expression
%type <Expr> MulExpr
%type <Expr> AddExpr
%type <Expr> RelExp
%type <Expr> EqExp
%type <Expr> LAndExp
%type <Expr> LOrExp
%type <Expr> BType
%type <Obj> Block
%type <Obj> BlockItem
%type <Obj> CompUnit
%type <Obj> CompUnitItem

%start CompUnit
%%
CompUnit: 
  CompUnit CompUnitItem {
  // auto inner = stak.back();
  // stak.pop_back();
  // stak.back().getAsObject()->get("inner")->getAsArray()->push_back(inner);
  gRoot=$1;
}
| CompUnitItem {
  // auto inner = stak.back();
  // stak.back() = llvm::json::Object{{"kind", "TranslationUnitDecl"},
  //                                  {"inner", llvm::json::Array{inner}}};
  gRoot=$1;
}
;
CompUnitItem: 
  VarDecl {}
| FuncDef {}
;
/* VarDecl: 
  BType T_IDENTIFIER T_SEMI {
  // auto name = stak.back().getAsObject()->get("value")->getAsString()->str();
  // stak.back() = llvm::json::Object{{"kind", "VarDecl"}, {"name", name}};
}
; */

//Functions and global variable
FuncDef: 
  BType T_IDENTIFIER T_L_PAREN T_R_PAREN Block {
  // auto inner = stak.back();
  // stak.pop_back();
  // auto name = stak.back().getAsObject()->get("value")->getAsString()->str();
  // stak.back() = llvm::json::Object{{"kind", "FunctionDecl"},
  //                                  {"name", name},
  //                                  {"inner", llvm::json::Array{inner}}};

}
| BType T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN Block
;

FuncFParams:
  FuncFParam 
| FuncFParam MultiFuncFParams

MultiFuncFParams:
  T_COMMA FuncFParam
| T_COMMA FuncFParam MultiFuncFParams
;
FuncFParam:
  BType T_IDENTIFIER
;
FuncRParams:
  Expression  {

  }
| Expression MultiExpr
;
MultiExpr:
  T_COMMA Expression
| T_COMMA Expression MultiExpr
;
Block: 
  T_L_BRACE T_R_BRACE { $$=NULL; }
| T_L_BRACE BlockItem T_R_BRACE { $$ = $2;  }
;
BlockItem:
  Stmt {
  // auto inner = stak.back();
  // stak.back() = llvm::json::Object{{"kind", "CompoundStmt"},
  //                                  {"inner", llvm::json::Array{inner}}};
}
| Declaration
;

BType:
  T_INT {
    auto p = Mgr::g.make<Expr>();
    p->type = INT;
    $$ = p;
  }
| T_FLOAT {
    auto p = Mgr::g.make<Expr>();
    p->type = FLOAT;
    $$ = p;
  }
| T_LONG T_LONG {
    auto p = Mgr::g.make<Expr>();
    p->type = LONGLONG;
    $$ = p;
  }
| T_CHAR {
    auto p = Mgr::g.make<Expr>();
    p->type = CHAR;
    $$ = p;
  }
| T_VOID {
    auto p = Mgr::g.make<Expr>();
    p->type = VOID;
    $$ = p;
  }
;
Stmt: 
  T_RETURN T_NUMERIC_CONSTANT T_SEMI    %prec PREC0 {
  // auto inner = stak.back();
  // stak.back() = llvm::json::Object{{"kind", "ReturnStmt"},
  //                                  {"inner", llvm::json::Array{inner}}};
}  //return number statement
| T_RETURN Expression T_SEMI    %prec PREC1 {} 
| T_RETURN T_SEMI
| LVal T_EQUAL Expression T_SEMI
| Block 
| Expression T_SEMI
| T_SEMI
//if statement
| T_IF T_L_PAREN Expression T_R_PAREN Stmt %prec THEN
| T_IF T_L_PAREN Expression T_R_PAREN Stmt T_ELSE Stmt
//while statement
| T_WHILE T_L_PAREN Expression T_R_PAREN Stmt
| T_DO T_L_BRACE Stmt T_R_BRACE T_WHILE T_L_PAREN Stmt T_R_PAREN T_SEMI
| T_BREAK T_SEMI
| T_CONTINUE T_SEMI
;

Declaration:
  ConstDecl
| VarDecl
;
//常量
ConstDecl: 
  T_CONST BType ConstDef T_SEMI { 
    auto p = Mgr::g.make<Decl>();
    p->type = BType->type;
    
  }
| T_CONST BType ConstDef MultiConstDef T_SEMI
;
MultiConstDef:
  T_COMMA ConstDef
| T_COMMA ConstDef MultiConstDef
;
ConstDef:
  T_IDENTIFIER T_EQUAL ConstInitVal {}
| T_IDENTIFIER SquareExpr T_EQUAL ConstInitVal
;
SquareExpr:
  T_L_SQUARE Expression T_R_SQUARE
| T_L_SQUARE Expression T_R_SQUARE SquareExpr
;
ConstInitVal:
  ConstExp {}
;
LVal: 
  T_IDENTIFIER { 
    auto p = Mgr::g.make<Decl>(); 
    p->name = $1->name;
  }
;
ConstExp:
  Expression {}
;
//变量与赋值
VarDecl:
  BType VarDef T_SEMI
| BType VarDef MultiVarDecl T_SEMI
;
MultiVarDecl:
  T_COMMA VarDef
| T_COMMA VarDef MultiVarDecl
;
VarDef:
  T_IDENTIFIER
| T_IDENTIFIER T_EQUAL InitVal
;
InitVal:
  Expression
;
Expression: 
  /* UnaryExp
| AddExpr */
  LOrExp
;
//一元表达式
PrimaryExp:
  T_L_PAREN Expression T_R_PAREN { $$ = $2; }
| LVal { $$ = $1; }
| T_NUMERIC_CONSTANT { $$ = $1; }
;
/* Number:
  T_NUMERIC_CONSTANT{
    auto p = Mgr::g.make<IntegerLiteral>();
    p->val=$1;
  }
; */

UnaryExp:
  PrimaryExp { $$ = $1; }
| T_PLUS UnaryExp {
  auto p = Mgr::g.make<UnaryOp>();
  p->op = UnaryOp::kPos;
  p->sub = $2;
  $$ = p;
}
| T_MINUS UnaryExp {
  auto p = Mgr::g.make<UnaryOp>();
  p->op = UnaryOp::kNeg;
  p->sub = $2;
  $$ = p;
}
| T_EXCLAIM UnaryExp {
  auto p = Mgr::g.make<UnaryOp>();
  p->op = UnaryOp::kExc;
  p->sub = $2;
  $$ = p;
}
| T_IDENTIFIER T_L_PAREN FuncRParams T_R_PAREN {

}
;
//算术表达式
MulExpr:
  UnaryExp
| MulExpr T_STAR UnaryExp{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kMul;
    p->lft = $1;
    p->rht = $3;    
    $$ = p;
}
| MulExpr T_SLASH UnaryExp{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kDiv;
    p->lft = $1;
    p->rht = $3;
    $$ = p;
}
| MulExpr T_PERCENT UnaryExp{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kMod;
    p->lft = $1;
    p->rht = $3;
    $$ = p;
}
;
AddExpr:
  MulExpr
| AddExpr T_PLUS MulExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kAdd;
    p->lft = $1;
    p->rht = $3;
    $$ = p;
}
| AddExpr T_MINUS MulExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kSub;
    p->lft = $1;
    p->rht = $3;
    $$ = p;
}
;

//逻辑表达式
RelExp:
  AddExpr
| RelExp T_LESS AddExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kLes;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
| RelExp T_GREATER AddExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kGrt;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
| RelExp T_LESSEQUAL AddExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kLse;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
| RelExp T_GREATEREQUAL AddExpr{
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kGre;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
;
EqExp:
  RelExp  { $$ = $1 }
| EqExp T_EQUALEQUAL RelExp {
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kEqq;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
| EqExp T_EXCLAIMEQUAL RelExp {
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kExe;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
;
LAndExp:
  EqExp { $$ = $1 }
| LAndExp T_AMPAMP EqExp {
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kAnd;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
;
LOrExp:
  LAndExp { $$ = $1 }
| LOrExp T_PIPEPIPE LAndExp {
    auto p = Mgr::g.make<BinaryExpr>(); p->type=INT;
    p->op = kOr;
    p->lft = $1;
    p->rht = $3; 
    $$ = p;
}
;


%%