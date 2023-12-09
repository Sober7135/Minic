class ASTNode;

class ASTVisitor {
public:
  virtual auto visit(ASTNode *) -> void;
};