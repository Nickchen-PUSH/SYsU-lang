#include "/root/SYsU-lang-latest/SYsU-lang/parser/asg.hpp"
Mgr g;
int main(){
    auto p = g.make<ObjList>();
    auto v1 = g.make<Expr>();
    v1->type = INT;
    // v1->name = "Declaration";
    p->push_back(v1);
    return 0;
}