//--------------------------------------------------------------------------------------------------
/// @brief SnuPL parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2019/09/15 Bernhard Egger added support for constant expressions
/// 2020/07/31 Bernhard Egger adapted to SnuPL/2
/// 2020/09/27 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2022, Computer Systems and Platforms Laboratory, SNU
/// All rights reserved.
///
/// Redistribution and use in source and binary forms, with or without modification, are permitted
/// provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice, this list of condi-
///   tions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice, this list of condi-
///   tions and the following disclaimer in the documentation and/or other materials provided with
///   the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
/// IMPLIED WARRANTIES,  INCLUDING, BUT NOT LIMITED TO,  THE IMPLIED WARRANTIES OF MERCHANTABILITY
/// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
/// CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING,  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
/// LOSS OF USE, DATA,  OR PROFITS;  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
/// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
/// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//--------------------------------------------------------------------------------------------------

#include "parser.h"

#include <errno.h>
#include <limits.h>

#include <cassert>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <vector>

using namespace std;

//--------------------------------------------------------------------------------------------------
// EBNF of SnuPL/2--
//   module            = "module" ident ";"
//                       { constDeclaration | varDeclaration }
//                       [ "begin" statSequence ] "end" ident ".".
//
//   constDeclaration  = [ "const" constDeclSequence ].
//   constDeclSequence = constDecl ";" { constDecl ";" }
//   constDecl         = varDecl "=" expression.
//
//   varDeclaration    = [ "var" varDeclSequence ].
//   varDeclSequence   = varDecl ";" { varDecl ";" }.
//   varDecl           = ident { "," ident } ":" type.
//
//   ident             = tIdent.
//   number            = tNumber.
//
//   qualident         = ident.
//   factOp            = "*" | "/".
//   termOp            = "+" | "-".
//   relOp             = "=" | "#".
//
//   factor            = qualident | number | "(" expression ")".
//   term              = factor { factOp factor }.
//   simpleexpr        = ["+" | "-"] term { termOp term }.
//   expression        = simpleexpr [ relOp simpleexpr ].
//   assignment        = ident ":=" expression.
//   statement         = assignment.
//   statSequence      = [ statement { ";" statement } ].

//--------------------------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode *CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) {
    delete _module;
    _module = NULL;
  }

  try {
    if (_scanner != NULL) _module = module();
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken *CParser::GetErrorToken(void) const
{
  if (_abort)
    return &_error_token;
  else
    return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort)
    return _message;
  else
    return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw CParseError();
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" + t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *st)
{
  // TODO (phase 2)
  // add predefined functions here
}

CAstModule *CParser::module(void)
{
  //
  // module ::= "module" ident ";"
  //            { constDeclaration | varDeclaration }
  //            [ "begin" statSequence ] "end" ident ".".
  //

  EToken tt;
  CToken t;
  CAstModule *m;
  CAstStatement *statseq = NULL;

  Consume(tModule);
  Consume(tIdent, &t);
  Consume(tSemicolon);

  m = new CAstModule(t, t.GetValue());
  InitSymbolTable(m->GetSymbolTable());

  while (tt = _scanner->Peek().GetType(), tt == tConst || tt == tVar) {
    if (tt == tConst) {
      constDeclaration(m);
    } else {
      varDeclaration(m);
    }
  }

  if (_scanner->Peek().GetType() == tBegin) {
    Consume(tBegin);
    statseq = statSequence(m);
  }

  Consume(tEnd);
  Consume(tIdent, &t);
  if (m->GetName() != t.GetValue()) {
    SetError(t, "unexpected module identifier");
  }

  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

void CParser::constDeclaration(CAstScope *s)
{
  //
  // constDeclaration ::= [ "const" constDeclSequence ].
  // constDeclSequence ::= constDecl ";" { constDecl ";" }.
  // constDecl ::= varDecl "=" expression.
  // varDecl ::= ident { "," ident } ":" type.
  //

  if (_scanner->Peek().GetType() == tConst) {
    CToken t;
    vector<string> ts{};
    CAstExpression *expr;
    const CType *type;
    Consume(tConst);
    do {
      type = varDecl(&ts);
      Consume(tRelOp, &t);
      if (t.GetValue() != "=") {
        SetError(t, "unexpected operator in constant initializer");
      }
      expr = expression(s);
      // TODO: for each ident in ts, s->CreateConst with the expr->Evaluate()'d values
      Consume(tSemicolon);
    } while (_scanner->Peek().GetType() == tIdent);
  }
}

void CParser::varDeclaration(CAstScope *s)
{
  //
  // varDeclaration ::= [ "var" varDeclSequence ].
  // varDeclSequence ::= varDecl ";" { varDecl ";" }.
  // varDecl ::= ident { "," ident } ":" type.
  //

  if (_scanner->Peek().GetType() == tVar) {
    CToken t;
    vector<string> ts{};
    const CType *type;
    Consume(tVar);
    do {
      type = varDecl(&ts);
      for (const string &ident : ts) {
        s->CreateVar(ident, type);
      }
      Consume(tSemicolon);
    } while (_scanner->Peek().GetType() == tIdent);
  }
}

const CType *CParser::varDecl(vector<string> *idents)
{
  //
  // varDecl ::= ident { "," ident } ":" type.
  //
  // FIRST(varDecl) = { tIdent }
  //

  CToken t;

  Consume(tIdent, &t);
  idents->push_back(t.GetValue());
  while (!_abort && _scanner->Peek().GetType() == tComma) {
    Consume(tComma);
    Consume(tIdent, &t);
    idents->push_back(t.GetValue());
  }
  Consume(tColon);

  switch (_scanner->Peek().GetType()) {  // TODO: add type parsing
    case tInteger: Consume(tInteger); return CTypeManager::Get()->GetInteger();
    default: SetError(_scanner->Get(), "unexpected token in type"); break;
  }

  return NULL;
}

CAstStatement *CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment.
  //
  // FIRST(statSequence) = { tIdent }
  // FOLLOW(statSequence) = { tEnd }
  //
  // FIRST(statement) = { tIdent }
  // FOLLOW(statement) = { tSemicolon, tEnd }
  //

  // The linking of statement sequences is a bit akward here because
  // we implement statSequence as a loop and not recursively.
  // We keep a 'head' that points to the first statement and is finally
  // returned at the end of the function. Head can be NULL if no statement
  // is present.
  // In the loop, we track the end of the linked list using 'tail' and
  // attach new statements to that tail.
  CAstStatement *head = NULL;

  if (_scanner->Peek().GetType() != tEnd) {
    CAstStatement *tail = NULL;

    do {
      CAstStatement *st = NULL;

      switch (_scanner->Peek().GetType()) {
        // statement ::= assignment
        case tIdent: st = assignment(s); break;

        default: SetError(_scanner->Peek(), "statement expected."); break;
      }

      assert(st != NULL);
      if (head == NULL)
        head = st;
      else
        tail->SetNext(st);
      tail = st;

      if (_scanner->Peek().GetType() == tEnd) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign *CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= ident ":=" expression.
  //
  CToken t;

  CAstDesignator *lhs = ident(s);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression *CParser::expression(CAstScope *s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop = opNop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")
      relop = opEqual;
    else if (t.GetValue() == "#")
      relop = opNotEqual;
    else
      SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression *CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  CToken t;
  CAstExpression *n = NULL;

  if (_scanner->Peek().GetType() == tPlusMinus) {
    Consume(tPlusMinus, &t);
  }

  n = term(s);

  if (t.GetType() == tPlusMinus) {
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, n);
  }

  while (_scanner->Peek().GetType() == tPlusMinus) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tPlusMinus, &t);

    r = term(s);

    n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
  }

  return n;
}

CAstExpression *CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while (tt == tMulDiv) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tMulDiv, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression *CParser::factor(CAstScope *s)
{
  //
  // factor ::= qualident | number | "(" expression ")"
  //
  // FIRST(factor) = { tIdent, tNumber, tLParen }
  //

  CToken t;
  CAstExpression *n = NULL;

  switch (_scanner->Peek().GetType()) {
    // factor ::= qualident
    case tIdent: n = qualident(s); break;

    // factor ::= number
    case tNumber: n = number(); break;

    // factor ::= "(" expression ")"
    case tLParen:
      Consume(tLParen);
      n = expression(s);
      Consume(tRParen);
      break;

    default: SetError(_scanner->Peek(), "factor expected."); break;
  }

  return n;
}

CAstDesignator *CParser::qualident(CAstScope *s)
{
  //
  // qualident ::= ident.
  //

  return ident(s);
}

CAstDesignator *CParser::ident(CAstScope *s)
{
  //
  // number ::= tIdent
  //

  CToken t;
  CSymtab *st = s->GetSymbolTable();

  Consume(tIdent, &t);

  // check if symbol exists in (local) symbol table
  const CSymbol *sym = st->FindSymbol(t.GetValue(), sLocal);

  if (sym == NULL) {
    // if not, create one and add it to the symbol table
    CSymbol *nsym = s->CreateVar(t.GetValue(), CTypeManager::Get()->GetInteger());
    st->AddSymbol(nsym);

    sym = nsym;
  }

  return new CAstDesignator(t, sym);
}

CAstConstant *CParser::number(void)
{
  //
  // number ::= tNumber
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInteger(), v);
}
