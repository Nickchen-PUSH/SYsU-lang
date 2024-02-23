#pragma once

#include <memory>
#include <string>
#include <vector>

struct Obj {
public:
  virtual ~Obj() = default;
public:
  template<typename T>
  T* dcast()
  {
    return dynamic_cast<T*>(this);
  }

  template<typename T>
  T& rcast()
  {
    return *reinterpret_cast<T*>(this);
  }
};
struct ObjList
  : public Obj
  , public std::vector<Obj*>
{};
typedef enum Types{
    INT,LONGLONG,CHAR,FLOAT,VOID,
    CompoundStmt,ReturnStmt
} Type;

struct Decl : public Obj {	// 表达一个符号声明（变量、参数等）
    std::string name;	// 符号名
    Type type;  		// 符号类型（这里省略了 struct Type 的定义）
    struct Expr* RefExpr;
};

struct DeclList
  : public Obj
  , public std::vector<Decl*>
{};

struct Expr : public Obj {
    Type type;			// C语言的每个表达式都有类型
};

struct ExprList
  : public Obj
  , public std::vector<Expr*>
{};

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
            kAnd,kOr, //逻辑操作
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

// class Mgr : public std::vector<std::unique_ptr<Obj>>
// {
// public:
//   static Mgr g;

// public:
//   template<typename T, typename... Args>
//   T* make(Args... args)
//   {
//     auto ptr = std::make_unique<T>(args...);
//     // auto ptr = new T;
//     auto obj = ptr.get();
//     emplace_back(std::move(ptr));
//     return obj;
//   }
// };
class Mgr : public std::vector<std::unique_ptr<Obj>>
{
public:
  // 第三个参数用于静态检查参数是否正确，也可以省略。
  template<typename T,
           typename... Args,
           typename = std::enable_if_t<std::is_constructible_v<T, Args...>>>
  T* make(Args... args)
  {
    auto ptr = std::make_unique<T>(args...);
    auto obj = ptr.get();
    emplace_back(std::move(ptr));
    return obj;
  }
};

