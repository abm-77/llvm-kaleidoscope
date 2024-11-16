#pragma once

#include "collections.h"
#include "common.h"
#include "lexer.h"
#include "mem.h"

enum AstExprType {
  AST_NUMERIC_EXPR,
  AST_IDENTIFIER_EXPR,
  AST_INFIX_EXPR,
  AST_CALL_EXPR,
  AST_IF_EXPR,
  AST_PROTOTYPE_EXPR,
  AST_FUNCTION_EXPR,
  AST_FOR_EXPR,
};

enum Precedence {
  PRECEDENCE_LOWEST,
  PRECEDENCE_EQ,
  PRECEDENCE_LT_GT,
  PRECEDENCE_SUM,
  PRECEDENCE_PRODUCT,
  PRECEDENCE_PREFIX,
  PRECEDENCE_CALL,
};

struct AstExpr {
  AstExprType type;
  union {
    struct {
      f64 val;
    } numeric_expr;
    struct {
      Slice name;
    } ident_expr;
    struct {
      TokenType op;
      AstExpr *lhs, *rhs;
    } infix_expr;
    struct {
      Slice name;
      DynArray<AstExpr *> args;
    } call_expr;
    struct {
      Slice name;
      DynArray<AstExpr *> args;
    } prototype_expr;
    struct {
      AstExpr *prototype; // prototype_expr
      AstExpr *body;
    } function_expr;
    struct {
      AstExpr *cond;
      AstExpr *then;
      AstExpr *otherwise;
    } if_expr;
    struct {
      Slice var_name;
      AstExpr *start;
      AstExpr *end;
      AstExpr *step;
      AstExpr *body;
    } for_expr;
  };
};

static const char *op_to_str(TokenType ty) {
  switch (ty) {
  case TOKEN_PLUS:
    return "+";
  case TOKEN_MINUS:
    return "-";
  case TOKEN_STAR:
    return "*";
  case TOKEN_SLASH:
    return "/";
  case TOKEN_LESS:
    return "<";
  case TOKEN_GREATER:
    return ">";
  case TOKEN_LESS_EQUAL:
    return "<=";
  case TOKEN_GREATER_EQUAL:
    return ">=";
  default:
    return "unknown";
  }
}

static void print_ast_expr(Arena *arena, AstExpr *expr) {
  auto tmp = arena->begin_temp();
  switch (expr->type) {
  case AST_NUMERIC_EXPR: {
    printf("numeric_expr { val = %f }\n", expr->numeric_expr.val);
  } break;
  case AST_IDENTIFIER_EXPR: {
    u8 *name = materialize_slice(arena, &expr->ident_expr.name);
    printf("ident_expr { name = %s }\n", name);
  } break;
  case AST_INFIX_EXPR: {
    printf("infix_expr:\n");
    printf("op: %s\n", op_to_str(expr->infix_expr.op));
    printf("lhs:\n");
    print_ast_expr(arena, expr->infix_expr.lhs);
    printf("rhs:\n");
    print_ast_expr(arena, expr->infix_expr.rhs);
  } break;
  case AST_CALL_EXPR: {
    u8 *name = materialize_slice(arena, &expr->call_expr.name);
    printf("call_expr { name = %s }\n", name);
  } break;
  case AST_PROTOTYPE_EXPR: {
    u8 *name = materialize_slice(arena, &expr->prototype_expr.name);
    printf("proto_expr {\n");
    printf("name: %s\n", name);
    printf("args: \n");
    for (auto it = expr->prototype_expr.args.make_iter(); !it.done();
         it.next()) {
      print_ast_expr(arena, *it.value());
    }
    printf("}\n");
  } break;
  case AST_FUNCTION_EXPR: {
    u8 *name = materialize_slice(
        arena, &expr->function_expr.prototype->prototype_expr.name);
    printf("func_expr {\n");
    printf("name: %s\n", name);
    printf("args:\n");
    for (auto it =
             expr->function_expr.prototype->prototype_expr.args.make_iter();
         !it.done(); it.next()) {
      print_ast_expr(arena, *it.value());
    }
    printf("body:\n");
    print_ast_expr(arena, expr->function_expr.body);
    printf("}\n");
  } break;
  case AST_IF_EXPR: {
    printf("if_expr {\n");
    printf("cond: ");
    print_ast_expr(arena, expr->if_expr.cond);
    printf("then: ");
    print_ast_expr(arena, expr->if_expr.then);
    printf("else: ");
    print_ast_expr(arena, expr->if_expr.otherwise);
    printf("}\n");
  } break;
  case AST_FOR_EXPR: {
    u8 *name = materialize_slice(arena, &expr->for_expr.var_name);
    printf("for_expr {\n");
    printf("iter: %s \n", name);
    printf("start:\n");
    print_ast_expr(arena, expr->for_expr.start);
    printf("end:\n");
    print_ast_expr(arena, expr->for_expr.end);
    if (expr->for_expr.step) {
      printf("step: ");
      print_ast_expr(arena, expr->for_expr.step);
    }
    printf("body:\n");
    print_ast_expr(arena, expr->for_expr.body);
    printf("}\n");
  } break;
  }
  tmp.reset();
}

namespace Parser {
static Arena *arena;
static DynArray<Token>::Iterator token_iter;
static Token curr_token;

static AstExpr *ast_error(const char *msg) {
  fprintf(stdout, "error: %s\n", msg);
  return nullptr;
}

static const Token *token_error(const char *msg) {
  fprintf(stderr, "error: %s\n", msg);
  return nullptr;
}

static Precedence get_token_precedence(const Token *token) {
  switch (token->type) {
  case TOKEN_BANG_EQUAL:
  case TOKEN_LESS_EQUAL:
  case TOKEN_GREATER_EQUAL:
  case TOKEN_EQUAL_EQUAL:
    return PRECEDENCE_EQ;
  case TOKEN_LESS:
  case TOKEN_GREATER:
    return PRECEDENCE_LT_GT;
  case TOKEN_PLUS:
  case TOKEN_MINUS:
    return PRECEDENCE_SUM;
  case TOKEN_STAR:
  case TOKEN_SLASH:
    return PRECEDENCE_PRODUCT;
  case TOKEN_LPAREN:
    return PRECEDENCE_CALL;
  default:
    return PRECEDENCE_LOWEST;
  }
}

static const Token *next_token() {
  const Token *res = token_iter.value();
  token_iter.next();
  return res;
}

static Token *peek() { return token_iter.value(); }
static bool peek_is(TokenType type) { return peek()->type == type; }
static AstExpr *parse_expr(Precedence precedence);

static const Token *consume(TokenType type) {
  auto token = next_token();
  return (token->type == type) ? token : token_error("unexpected token");
}

static AstExpr *parse_numeric_expr() {
  const Token *token = consume(TOKEN_NUMERIC);
  char buf[64];
  memcpy(buf, token->lexeme.ptr, token->lexeme.len);
  buf[token->lexeme.len] = 0;
  f64 num_val = atof(buf);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_NUMERIC_EXPR;
  expr->numeric_expr = {.val = num_val};
  return expr;
}

static AstExpr *parse_if_expr() {
  consume(TOKEN_IF);
  auto cond = parse_expr(PRECEDENCE_LOWEST);
  if (!cond)
    return ast_error("expected condition");

  consume(TOKEN_THEN);
  auto then = parse_expr(PRECEDENCE_LOWEST);
  if (!then)
    return ast_error("expected then branch");

  consume(TOKEN_ELSE);
  auto otherwise = parse_expr(PRECEDENCE_LOWEST);
  if (!otherwise)
    return ast_error("expected else branch");

  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_IF_EXPR;
  expr->if_expr = {.cond = cond, .then = then, .otherwise = otherwise};
  return expr;
}

static AstExpr *parse_for_expr() {
  consume(TOKEN_FOR);
  auto var = consume(TOKEN_IDENTIFIER);
  consume(TOKEN_EQUAL);

  auto start = parse_expr(PRECEDENCE_LOWEST);
  if (!start)
    return nullptr;
  consume(TOKEN_COMMA);

  auto end = parse_expr(PRECEDENCE_LOWEST);
  if (!end)
    return nullptr;

  AstExpr *step = nullptr;
  if (peek_is(TOKEN_COMMA)) {
    consume(TOKEN_COMMA);
    step = parse_expr(PRECEDENCE_LOWEST);
    if (!step)
      return nullptr;
  }

  consume(TOKEN_IN);
  auto body = parse_expr(PRECEDENCE_LOWEST);
  if (!body)
    return nullptr;

  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_FOR_EXPR;
  expr->for_expr = {
      .var_name = var->lexeme,
      .start = start,
      .end = end,
      .step = step,
      .body = body,
  };
  return expr;
}
static AstExpr *parse_grouped_expr() {
  consume(TOKEN_LPAREN);
  auto v = parse_expr(PRECEDENCE_LOWEST);
  consume(TOKEN_RPAREN);
  return v;
}

static DynArray<AstExpr *> parse_expr_list(TokenType start, TokenType end) {
  DynArray<AstExpr *> exprs(arena, 1);
  consume(start);
  if (peek_is(end)) {
    consume(end);
  } else {
    exprs.push(parse_expr(PRECEDENCE_LOWEST));
    while (peek_is(TOKEN_COMMA)) {
      consume(TOKEN_COMMA);
      exprs.push(parse_expr(PRECEDENCE_LOWEST));
    }
    consume(end);
  }
  return exprs;
}

static AstExpr *parse_ident_expr() {
  auto token = consume(TOKEN_IDENTIFIER);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_IDENTIFIER_EXPR;
  expr->ident_expr = {.name = token->lexeme};
  return expr;
}

static AstExpr *parse_infix_expr(AstExpr *lhs) {
  auto op = peek();
  auto precedence = get_token_precedence(op);
  consume(op->type);
  auto rhs = parse_expr(precedence);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_INFIX_EXPR;
  expr->infix_expr = {.op = op->type, .lhs = lhs, .rhs = rhs};
  return expr;
}

static AstExpr *parse_call_expr(AstExpr *function) {
  auto args = parse_expr_list(TOKEN_LPAREN, TOKEN_RPAREN);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_CALL_EXPR;
  expr->call_expr = {.name = function->ident_expr.name, .args = args};
  return expr;
}

static AstExpr *parse_prototype_expr() {
  auto ident = consume(TOKEN_IDENTIFIER)->lexeme;
  auto args = parse_expr_list(TOKEN_LPAREN, TOKEN_RPAREN);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_PROTOTYPE_EXPR;
  expr->prototype_expr = {.name = ident, .args = args};
  return expr;
}

static AstExpr *parse_definition_expr() {
  consume(TOKEN_DEF);
  auto proto = parse_prototype_expr();
  auto body = parse_expr(PRECEDENCE_LOWEST);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_FUNCTION_EXPR;
  expr->function_expr = {.prototype = proto, .body = body};
  return expr;
}

static AstExpr *parse_extern_expr() {
  consume(TOKEN_EXTERN);
  return parse_prototype_expr();
}

static AstExpr *parse_top_level_expr() {
  if (auto body = parse_expr(PRECEDENCE_LOWEST)) {
    auto proto = arena->alloc<AstExpr>();
    const char *top_level_name = "__anon_expr";
    u32 len = strlen(top_level_name);
    auto buffer = arena->alloc(len);
    memcpy(buffer, top_level_name, len);
    proto->type = AST_PROTOTYPE_EXPR;
    proto->prototype_expr = {
        .name = Slice{.ptr = buffer, .len = len},
        .args = DynArray<AstExpr *>(arena, 0),
    };

    auto expr = arena->alloc<AstExpr>();
    expr->type = AST_FUNCTION_EXPR;
    expr->function_expr = {.prototype = proto, .body = body};
    return expr;
  }

  return nullptr;
}

static AstExpr *parse_expr(Precedence precedence) {
  AstExpr *lhs;
  switch (peek()->type) {
  case TOKEN_IDENTIFIER:
    lhs = parse_ident_expr();
    break;
  case TOKEN_NUMERIC:
    lhs = parse_numeric_expr();
    break;
  case TOKEN_LPAREN:
    lhs = parse_grouped_expr();
    break;
  case TOKEN_IF:
    lhs = parse_if_expr();
    break;
  case TOKEN_FOR:
    lhs = parse_for_expr();
    break;
  default:
    printf("unexpected token: %s\n", peek()->lexeme.ptr);
    lhs = nullptr;
    break;
  }

  if (!lhs)
    return ast_error("unknown token when parsing lhs expression");

  while (precedence < get_token_precedence(peek())) {
    AstExpr *infix;
    switch (peek()->type) {
    case TOKEN_EQUAL:
    case TOKEN_PLUS_EQUAL:
    case TOKEN_MINUS_EQUAL:
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_SLASH:
    case TOKEN_STAR:
    case TOKEN_EQUAL_EQUAL:
    case TOKEN_BANG_EQUAL:
    case TOKEN_GREATER_EQUAL:
    case TOKEN_LESS_EQUAL:
    case TOKEN_GREATER:
    case TOKEN_LESS:
      infix = parse_infix_expr(lhs);
      break;
    case TOKEN_LPAREN:
      infix = parse_call_expr(lhs);
      break;
    default:
      infix = nullptr;
    }

    if (!infix)
      return lhs;

    lhs = infix;
  }
  return lhs;
};

static DynArray<AstExpr *> parse(Arena *A, DynArray<Token> tokens) {
  arena = A;
  token_iter = tokens.make_iter();

  DynArray<AstExpr *> exprs(arena, 32);
  while (true) {
    switch (peek()->type) {
    case TOKEN_EXTERN:
      if (auto expr = parse_extern_expr()) {
        exprs.push(expr);
        print_ast_expr(arena, expr);
      } else {
        next_token();
      }
      break;
    case TOKEN_DEF:
      if (auto expr = parse_definition_expr()) {
        exprs.push(expr);
        print_ast_expr(arena, expr);
      } else {
        next_token();
      }
      break;
    case TOKEN_EOF:
      printf("reached eof\n");
      return exprs;
    default:
      if (auto expr = parse_top_level_expr()) {
        exprs.push(expr);
        print_ast_expr(arena, expr);
      } else {
        next_token();
      }
      break;
    }
  }
}
}; // namespace Parser
