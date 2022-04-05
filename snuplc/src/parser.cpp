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
//                       { constDeclaration | varDeclaration | subrDeclaration }
//                       [ "begin" statSequence ] "end" ident ".".
//
//   ident             = tIdent.
//   number            = tNumber.
//   type              = basetype
//   basetype          = "boolean" | "char" | "integer" | "longint".
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
//
//   assignment        = ident ":=" expression.
//   returnStatement   = "return" [ expression ].
//
//   statement         = assignment | returnStatement.
//   statSequence      = [ statement { ";" statement } ].
//
//   constDeclaration  = [ "const" constDeclSequence ].
//   constDeclSequence = constDecl ";" { constDecl ";" }.
//   constDecl         = varDecl "=" expression.
//
//   varDeclaration    = [ "var" varDeclSequence ].
//   varDeclSequence   = varDecl ";" { varDecl ";" }.
//   varDecl           = ident { "," ident } ":" type.
//
//   subrDeclaration   = ( "procedure" procedureDecl | "function" functionDecl )
//                       subroutineBody ident ";".
//   procedureDecl     = ident [ formalParam ] ";".
//   functionDecl      = ident [ formalParam ] ":" type ";".
//   formalParam       = "(" [ paramDeclSequence ] ")".
//   paramDeclSequence = varDecl { ";" varDecl }.
//   subroutineBody    = constDeclaration varDeclaration
//                       "begin" statSequence "end".

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

EToken CParser::PeekType()
{
  return _scanner->Peek().GetType();
}

CToken CParser::Peek()
{
  return _scanner->Peek();
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
  // constDeclaration ::= [ "const" constDeclSequence ].
  // varDeclaration ::= [ "var" varDeclSequence ].
  //
  // FIRST(constDeclaration) = { tConst }
  // FIRST(varDeclaration) = { tVar }
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

  while (tt = PeekType(), tt == tConst || tt == tVar || tt == tProcedure || tt == tFunction) {
    if (tt == tConst) {
      Consume(tConst);
      constDeclSequence(m);
    } else if (tt == tVar) {
      Consume(tVar);
      varDeclSequence(m);
    } else if (tt == tProcedure || tt == tFunction) {
      subrDeclaration(m);
    } else {
      SetError(Peek(), "unreachable state");
    }
  }

  if (PeekType() == tBegin) {
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

void CParser::constDeclSequence(CAstScope *s)
{
  //
  // constDeclSequence ::= constDecl ";" { constDecl ";" }.
  // constDecl ::= varDecl "=" expression.
  //

  CToken t;
  vector<string> ts{};
  CAstExpression *expr;
  const CType *vt;

  do {
    vt = varDecl(&ts);
    Consume(tRelOp, &t);
    if (t.GetValue() != "=") {
      SetError(t, "unexpected operator in constant initializer");
    }
    expr = expression(s);
    // TODO: for each ident in ts, s->CreateConst with the expr->Evaluate()'d values
    Consume(tSemicolon);
  } while (PeekType() == tIdent);
}

void CParser::varDeclSequence(CAstScope *s)
{
  //
  // varDeclSequence ::= varDecl ";" { varDecl ";" }.
  //

  CToken t;
  vector<string> ts{};
  const CType *vt;
  CSymtab *st = s->GetSymbolTable();

  do {
    vt = varDecl(&ts);
    for (const string &ident : ts) {
      st->AddSymbol(s->CreateVar(ident, vt));
    }
    Consume(tSemicolon);
  } while (PeekType() == tIdent);
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
  while (!_abort && PeekType() == tComma) {
    Consume(tComma);
    Consume(tIdent, &t);
    idents->push_back(t.GetValue());
  }
  Consume(tColon);

  return type();
}

void CParser::subrDeclaration(CAstScope *s)
{
  //
  // subrDeclaration ::= ( "procedure" procedureDecl | "function" functionDecl )
  //                     subroutineBody ident ";".
  // subroutineBody ::= constDeclaration varDeclaration
  //                    "begin" statSequence "end".
  //

  CToken t;
  CAstProcedure *p;
  CAstStatement *statseq = NULL;

  switch (PeekType()) {
    case tProcedure:
      Consume(tProcedure);
      p = procedureDecl(s);
      break;
    case tFunction:
      Consume(tFunction);
      p = functionDecl(s);
      break;
    default: SetError(Peek(), "unexpected token in subroutine declaration"); return;
  }

  if (PeekType() == tConst) {
    Consume(tConst);
    constDeclSequence(p);
  }

  if (PeekType() == tVar) {
    Consume(tVar);
    varDeclSequence(p);
  }

  Consume(tBegin);
  statseq = statSequence(p);
  Consume(tEnd);
  Consume(tIdent, &t);
  if (p->GetName() != t.GetValue()) {
    SetError(t, "unexpected subroutine identifier");
  }
  Consume(tSemicolon);

  p->SetStatementSequence(statseq);
}

CAstProcedure *CParser::procedureDecl(CAstScope *s)
{
  //
  // procedureDecl ::= ident [ formalParam ] ";".
  //

  CToken t;
  CSymtab *st = s->GetSymbolTable();
  CSymProc *sym;
  vector<CSymParam *> params{};

  Consume(tIdent, &t);
  formalParam(&params);
  Consume(tSemicolon);

  sym = new CSymProc(t.GetValue(), CTypeManager::Get()->GetNull());
  st->AddSymbol(sym);
  for (CSymParam *param : params) {
    sym->AddParam(param);
  }

  return new CAstProcedure(t, t.GetValue(), s, sym);
}

CAstProcedure *CParser::functionDecl(CAstScope *s)
{
  //
  // functionDecl ::= ident [ formalParam ] ":" type ";".
  //

  CToken t;
  CSymtab *st = s->GetSymbolTable();
  CSymProc *sym;
  const CType *rt;
  vector<CSymParam *> params{};

  Consume(tIdent, &t);
  formalParam(&params);
  Consume(tColon);
  rt = type();
  Consume(tSemicolon);

  sym = new CSymProc(t.GetValue(), rt);
  st->AddSymbol(sym);
  for (CSymParam *param : params) {
    sym->AddParam(param);
  }

  return new CAstProcedure(t, t.GetValue(), s, sym);
}

void CParser::formalParam(vector<CSymParam *> *params)
{
  //
  // formalParam ::= "(" [ paramDeclSequence ] ")".
  // paramDeclSequence ::= varDecl { ";" varDecl }.
  //

  vector<string> ts{};
  const CType *type;
  int index = 0;

  Consume(tLParen);

  while (PeekType() != tRParen) {
    type = varDecl(&ts);
    for (const string &ident : ts) {
      params->push_back(new CSymParam(index++, ident, type));
    }
    if (PeekType() == tSemicolon) {
      Consume(tSemicolon);
    }
  }

  Consume(tRParen);
}

CAstStatement *CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | returnStatement.
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

  if (PeekType() != tEnd) {
    CAstStatement *tail = NULL;

    do {
      CAstStatement *st = NULL;

      switch (PeekType()) {
        // statement ::= assignment
        case tIdent: st = assignment(s); break;
        // statement ::= returnStatement
        case tReturn: st = returnStatement(s); break;
        default: SetError(Peek(), "statement expected."); break;
      }

      assert(st != NULL);
      if (head == NULL)
        head = st;
      else
        tail->SetNext(st);
      tail = st;

      if (PeekType() == tEnd) break;

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
  CAstDesignator *lhs;
  CAstExpression *rhs;

  lhs = ident(s);
  Consume(tAssign, &t);
  rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatReturn *CParser::returnStatement(CAstScope *s)
{
  //
  // returnStatement ::= "return" [ expression ].
  //
  // FIRST(returnStatement) = { tReturn }
  //

  CToken t;
  CAstExpression *retexpr;

  Consume(tReturn, &t);
  retexpr = expression(s);

  return new CAstStatReturn(t, s, retexpr);
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

  if (PeekType() == tRelOp) {
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

  if (PeekType() == tPlusMinus) {
    Consume(tPlusMinus, &t);
  }

  n = term(s);

  if (t.GetType() == tPlusMinus) {
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, n);
  }

  while (PeekType() == tPlusMinus) {
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

  EToken tt = PeekType();

  while (tt == tMulDiv) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tMulDiv, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

    tt = PeekType();
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

  switch (PeekType()) {
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

    default: SetError(Peek(), "factor expected."); break;
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

  // check if symbol exists in the symbol table
  const CSymbol *sym = st->FindSymbol(t.GetValue());
  if (sym == NULL) SetError(t, "unknown identifier");

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

const CType *CParser::type(void)
{
  //
  // type ::= basetype
  // basetype ::= "boolean" | "char" | "integer" | "longint".
  //

  CToken t;

  switch (PeekType()) {
    case tBoolean: Consume(tBoolean, &t); return CTypeManager::Get()->GetBool();
    case tChar: Consume(tChar, &t); return CTypeManager::Get()->GetChar();
    case tInteger: Consume(tInteger, &t); return CTypeManager::Get()->GetInteger();
    case tLongInt: Consume(tLongInt, &t); return CTypeManager::Get()->GetLongint();
    default: SetError(Peek(), "unexpected token in type"); break;
  }

  return NULL;
}
