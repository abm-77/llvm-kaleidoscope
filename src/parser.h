#pragma once

#include "collections.h"
#include "common.h"
#include "lexer.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>

enum AstExprType {
  AST_NUMERIC_EXPR,
  AST_VARIABLE_EXPR,
  AST_BINARY_EXPR,
  AST_CALL_EXPR,
};

enum BinOp {
  BINOP_ADD,
  BINOP_SUB,
  BINOP_MUL,
  BINOP_DIV,
};

struct AstExpr {
  AstExprType type;
  union {
    struct {
      f64 val;
    } numeric_expr;
    struct {
      const char *name;
    } variable_expr;
    struct {
      BinOp op;
      AstExpr *lhs, *rhs;
    } binary_expr;
    struct {
      const char *callee;
      DynArray<AstExpr *> args;
    } call_expr;
    struct {
      const char *name;
      const char **args;
    } prototype_expr;
    struct {
      AstExpr *prototype; // prototype_expr
      AstExpr *body;
    } function_expr;
  };
};

namespace Parser {

static Arena *arena;
static DynArray<Token>::Iterator token_iter;
static Token curr_token;

static AstExpr *ast_error(const char *msg) {
  fprintf(stderr, "error: %s\n", msg);
  return nullptr;
}

static const Token *token_error(const char *msg) {
  fprintf(stderr, "error: %s\n", msg);
  return nullptr;
}

static const Token *next_token() {
  const Token *res = token_iter.value();
  token_iter.next();
  return res;
}

[[maybe_unused]] static const Token *peek() { return token_iter.value(); }
static bool peek_is(TokenType type) { return token_iter.value()->type == type; }

static const Token *consume(TokenType type) {
  auto token = next_token();
  return (token->type == type) ? token : token_error("unexpected token");
}

[[maybe_unused]] static AstExpr *parse_number_expr() {
  const Token *token = consume(TOKEN_NUMERIC);
  char buf[64];
  memcpy(buf, token->lexeme.buffer + token->lexeme.start, token->lexeme.len);
  f64 num_val = atof(buf);
  auto expr = arena->alloc<AstExpr>();
  expr->type = AST_NUMERIC_EXPR;
  expr->numeric_expr = {.val = num_val};
  return expr;
}

[[maybe_unused]] static AstExpr *parse_expr() { return nullptr; };

[[maybe_unused]] static AstExpr *parse_paren_expr() {
  consume(TOKEN_LPAREN);
  auto v = parse_expr();
  consume(TOKEN_RPAREN);
  return v;
}

[[maybe_unused]] static AstExpr *parse_ident_expr() {
  auto token = consume(TOKEN_IDENTIFIER);
  auto expr = arena->alloc<AstExpr>();

  char name[64];
  memcpy(name, token->lexeme.buffer + token->lexeme.start, token->lexeme.len);

  if (peek_is(TOKEN_LPAREN)) {
    expr->type = AST_VARIABLE_EXPR;
    expr->variable_expr = {.name = name};
  } else {
    DynArray<AstExpr *> args(arena, 1);

    consume(TOKEN_LPAREN);
    while (true) {
      if (auto arg = parse_expr())
        args.push(arg);
      else
        return nullptr;

      if (peek_is(TOKEN_RPAREN))
        break;

      if (!peek_is(TOKEN_COMMA))
        return ast_error("Expected ')' or ',' in argument list");
    }
    consume(TOKEN_RPAREN);

    expr->type = AST_CALL_EXPR;
    expr->call_expr = {.callee = name, .args = args};
  }

  return expr;
}

[[maybe_unused]] static DynArray<AstExpr *> parse(Arena *A,
                                                  DynArray<Token> tokens) {
  arena = A;
  token_iter = tokens.make_iter();

  DynArray<AstExpr *> exprs(arena, 32);
  return exprs;
}

}; // namespace Parser
