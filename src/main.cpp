
#include "codegen.h"
#include "lexer.h"
#include "mem.h"
#include "parser.h"

int main(void) {
  auto arena = Arena::init(MB(2));

  auto tokens = Lexer::lex(&arena, "programs/test.hex");
  for (auto it = tokens.make_iter(); !it.done(); it.next()) {
    const Token *token = it.value();
    char buf[64];
    memcpy(buf, token->lexeme.ptr, token->lexeme.len);
    buf[token->lexeme.len] = 0;
    printf("%s\n", buf);
  }
  printf("\n");

  auto ast_exprs = Parser::parse(&arena, tokens);
  printf("\n");

  Codegen::initialize_optimizations();
  for (auto it = ast_exprs.make_iter(); !it.done(); it.next()) {
    auto expr = *it.value();

    if (auto code = Codegen::generate(&arena, expr)) {
      code->print(llvm::outs());
      printf("\n");

      auto tmp = arena.begin_temp();
      if (expr->type == AST_FUNCTION_EXPR) {
        const char *name = reinterpret_cast<const char *>(materialize_slice(
            &arena, &expr->function_expr.prototype->prototype_expr.name));
        if (strcmp(name, "__anon_expr") == 0) {
          reinterpret_cast<llvm::Function *>(code)->eraseFromParent();
        }
      }
      tmp.reset();
    }
  }

  Codegen::compile();

  arena.deinit();
  return 0;
}
