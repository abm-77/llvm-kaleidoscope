#pragma once

#include "collections.h"
#include "common.h"
#include "file.h"
#include "mem.h"
#include <cctype>
#include <cstring>
#include <ctime>

#define make_equal_variant(T) make_token_if_else('=', T##_EQUAL, T)

struct SourceLocation {
  u32 byte_offset;
  u32 line;
  u32 column;
  const char *source_name;
};

struct SourceSpan {
  SourceLocation start;
  SourceLocation end;
};

enum TokenType {
  TOKEN_DEF,
  TOKEN_EXTERN,
  TOKEN_IDENTIFIER,
  TOKEN_VAR,
  TOKEN_IF,
  TOKEN_THEN,
  TOKEN_ELSE,
  TOKEN_FOR,
  TOKEN_IN,
  TOKEN_WHILE,
  TOKEN_BREAK,
  TOKEN_OR,
  TOKEN_AND,
  TOKEN_NOT,
  TOKEN_CONTINUE,
  TOKEN_RETURN,
  TOKEN_NUMERIC,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_SEMICOLON,
  TOKEN_DOT,
  TOKEN_EQUAL,
  TOKEN_EQUAL_EQUAL,
  TOKEN_PLUS,
  TOKEN_PLUS_EQUAL,
  TOKEN_MINUS,
  TOKEN_MINUS_EQUAL,
  TOKEN_STAR,
  TOKEN_STAR_EQUAL,
  TOKEN_SLASH,
  TOKEN_SLASH_EQUAL,
  TOKEN_LESS,
  TOKEN_GREATER,
  TOKEN_LESS_EQUAL,
  TOKEN_GREATER_EQUAL,
  TOKEN_BANG,
  TOKEN_BANG_EQUAL,
  TOKEN_COMMA,
  TOKEN_EOF,
  TOKEN_INVALID,
};

struct Token {
  TokenType type;
  Slice lexeme;
  SourceSpan span;
};

const static StaticStringMap<TokenType, 15> KEYWORDS({
    {"extern", TOKEN_EXTERN},
    {"def", TOKEN_DEF},
    {"var", TOKEN_VAR},
    {"for", TOKEN_FOR},
    {"while", TOKEN_WHILE},
    {"break", TOKEN_BREAK},
    {"continue", TOKEN_CONTINUE},
    {"if", TOKEN_IF},
    {"in", TOKEN_IN},
    {"then", TOKEN_THEN},
    {"else", TOKEN_ELSE},
    {"or", TOKEN_OR},
    {"and", TOKEN_AND},
    {"not", TOKEN_AND},
    {"return", TOKEN_RETURN},
});

class Lexer {
private:
  static bool eof() { return curr_pos.byte_offset >= src.len; }

  static u8 next_char() {
    if (eof())
      return '\0';
    u8 ch = src.content[curr_pos.byte_offset];
    if (ch == '\n') {
      curr_pos.line += 1;
      curr_pos.column = 1;
    } else {
      curr_pos.column += 1;
    }
    curr_pos.byte_offset += 1;
    return ch;
  }

public:
  static u8 peek() { return src.content[curr_pos.byte_offset]; }

  static bool peek_advance_if(u8 expected) {
    bool cond = peek() == expected;
    if (cond)
      next_char();
    return cond;
  }

  static Slice get_source_slice() {
    return Slice{
        .ptr = src.content + start_pos.byte_offset,
        .len = (curr_pos.byte_offset - start_pos.byte_offset),
    };
  }

  static Token make_token(TokenType type) {
    return Token{
        .type = type,
        .lexeme = get_source_slice(),
        .span =
            SourceSpan{
                .start = start_pos,
                .end = curr_pos,
            },
    };
  }

  static Token make_token_if_else(u8 expected, TokenType on_match,
                                  TokenType otherwise) {
    return peek_advance_if(expected) ? make_token(on_match)
                                     : make_token(otherwise);
  }

  static Token make_ident() {
    while (isalnum(peek()) && !eof())
      next_char();
    Slice lexeme = get_source_slice();
    char keyword[64];
    memcpy(keyword, lexeme.ptr, lexeme.len);
    keyword[lexeme.len] = 0;
    const TokenType *type = KEYWORDS.get(keyword);
    return (type) ? make_token(*type) : make_token(TOKEN_IDENTIFIER);
  }

  static Token make_numeric() {
    while (isdigit(peek()) || peek() == '.')
      next_char();
    return make_token(TOKEN_NUMERIC);
  }

  static Token consume_token() {
    u8 ch;
    do {
      start_pos = curr_pos;
      ch = next_char();
    } while (isspace(ch));

    if (ch == '#') {
      while (ch != '\n')
        ch = next_char();
      start_pos = curr_pos;
      ch = next_char();
    }

    if (ch == '\0')
      return make_token(TOKEN_EOF);

    if (isalpha(ch))
      return make_ident();

    if (isdigit(ch))
      return make_numeric();

    switch (ch) {
    case '(':
      return make_token(TOKEN_LPAREN);
    case ')':
      return make_token(TOKEN_RPAREN);
    case ';':
      return make_token(TOKEN_SEMICOLON);
    case '.':
      return make_token(TOKEN_DOT);
    case ',':
      return make_token(TOKEN_COMMA);
    case '+':
      return make_equal_variant(TOKEN_PLUS);
    case '-':
      return make_equal_variant(TOKEN_MINUS);
    case '*':
      return make_equal_variant(TOKEN_STAR);
    case '/':
      return make_equal_variant(TOKEN_SLASH);
    case '=':
      return make_equal_variant(TOKEN_EQUAL);
    case '<':
      return make_equal_variant(TOKEN_LESS);
    case '>':
      return make_equal_variant(TOKEN_GREATER);
    case '!':
      return make_equal_variant(TOKEN_BANG);
    }

    return make_token(TOKEN_INVALID);
  }

  static DynArray<Token> lex(Arena *A, const char *filename) {
    arena = A;
    src = read_entire_file(arena, filename);
    curr_pos = SourceLocation{
        .byte_offset = 0,
        .line = 1,
        .column = 0,
        .source_name = filename,
    };

    DynArray<Token> tokens(arena, 32);
    while (!eof()) {
      Token token = consume_token();
      tokens.push(token);
    }

    return tokens;
  }

private:
  inline static Arena *arena;
  inline static FileBuffer src;
  inline static SourceLocation start_pos;
  inline static SourceLocation curr_pos;
};
