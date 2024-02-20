#pragma once

#include <memory>
#include <string>
#include <vector>

struct Obj {
    virtual ~Obj() = default;
};
typedef enum Types{
    INT,LONGLONG,CHAR,FLOAT,VOID,
    CompoundStmt,ReturnStmt
} Type;

struct Decl : public Obj {	// 表达一个符号声明（变量、参数等）
    std::string name;	// 符号名
    Type type;  		// 符号类型（这里省略了 struct Type 的定义）
};

struct Expr : public Obj {
    Type type;			// C语言的每个表达式都有类型
};

struct IntegerLiteral : public Expr {
    int64_t val;	// 整数字面量的值
};

struct FloatLiteral : public Expr {
    double val;	// 浮点数字面量的值
};

struct DeclRefExpr : public Expr {
    Decl* decl;			// 指向引用的声明
};

struct BinaryExpr : public Expr {
    enum {  kAdd, kSub, kMul, kDiv, kMod, //算术操作
            kGrt, kLes, kGre, kLse, //比较操作
            kEqq, kExe,  //相等比较操作
            kAnd,kOr
          } op;		// 操作符
    Expr *lft, *rht;						// 指向左右操作数
};

struct UnaryOp : public Expr
{
  enum
  {
    kPos,
    kNeg,
    kExc
  } op;
  Expr* sub;
};

class Mgr : public std::vector<std::unique_ptr<Obj>>
{
public:
  extern Mgr g;

public:
  template<typename T, typename... Args>
  T* make(Args... args)
  {
    auto ptr = std::make_unique<T>(args...);
    auto obj = ptr.get();
    emplace_back(std::move(ptr));
    return obj;
  }
};

