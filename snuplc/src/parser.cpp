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
//   char              = tCharConst.
//   string            = tStringConst.
//   boolean           = tBoolConst.
//   type              = basetype { "[" simpleexpr "]" }.
//   basetype          = "boolean" | "char" | "integer" | "longint".
//
//   qualident         = ident { "[" simpleexpr "]" }.
//   factOp            = "*" | "/" | "&&".
//   termOp            = "+" | "-" | "||".
//   relOp             = "=" | "#" | "<" | "<=" | ">" | ">=".
//
//   factor            = qualident | number | boolean | char | string |
//                       "(" expression ")" | subroutineCall | "!" factor.
//   term              = factor { factOp factor }.
//   simpleexpr        = ["+" | "-"] term { termOp term }.
//   expression        = simpleexpr [ relOp simpleexpr ].
//
//   assignment        = qualident ":=" expression.
//   subroutineCall    = ident "(" [ expression {"," expression} ] ")".
//   ifStatement       = "if" "(" expression ")" "then" statSequence
//                       [ "else" statSequence ] "end".
//   whileStatement    = "while" "(" expression ")" "do" statSequence "end".
//   returnStatement   = "return" [ expression ].
//
//   statement         = assignment | subroutineCall | ifStatement
//                       | whileStatement | returnStatement.
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
  _token = _scanner->Get();
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

  CToken t = _token;
  _token = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" + t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

EToken CParser::PeekType()
{
  return _token.GetType();
}

CToken CParser::Peek()
{
  return _token;
}

EToken CParser::PeekNextType()
{
  return _scanner->Peek().GetType();
}

CToken CParser::PeekNext()
{
  return _scanner->Peek();
}

void CParser::InitSymbolTable(CSymtab *st)
{
  // add predefined functions here

  CTypeManager *tm = CTypeManager::Get();
  const CType *nulltype = tm->GetNull(), *anyarrtype = tm->GetVoidPtr(),
              *strtype = tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()));

  CSymProc *proc;

  // function DIM(array: pointer to array, dim: integer): integer;
  proc = new CSymProc("DIM", tm->GetInteger(), true);
  proc->AddParam(new CSymParam(0, "array", anyarrtype));
  proc->AddParam(new CSymParam(1, "dim", tm->GetInteger()));
  st->AddSymbol(proc);

  // function DOFS(array: pointer to array): integer;
  proc = new CSymProc("DOFS", tm->GetInteger(), true);
  proc->AddParam(new CSymParam(0, "array", anyarrtype));
  st->AddSymbol(proc);

  // function ReadInt(): integer;
  proc = new CSymProc("ReadInt", tm->GetInteger(), true);
  st->AddSymbol(proc);

  // function ReadLong(): longint;
  proc = new CSymProc("ReadLong", tm->GetLongint(), true);
  st->AddSymbol(proc);

  // procedure WriteInt(v: int);
  proc = new CSymProc("WriteInt", nulltype, true);
  proc->AddParam(new CSymParam(0, "v", tm->GetInteger()));
  st->AddSymbol(proc);

  // procedure WriteLong(v: longint);
  proc = new CSymProc("WriteLong", nulltype, true);
  proc->AddParam(new CSymParam(0, "v", tm->GetLongint()));
  st->AddSymbol(proc);

  // procedure WriteChar(c: char);
  proc = new CSymProc("WriteChar", nulltype, true);
  proc->AddParam(new CSymParam(0, "c", tm->GetChar()));
  st->AddSymbol(proc);

  // procedure WriteStr(string: char[]);
  proc = new CSymProc("WriteStr", nulltype, true);
  proc->AddParam(new CSymParam(0, "string", strtype));
  st->AddSymbol(proc);

  // procedure WriteLn();
  proc = new CSymProc("WriteLn", nulltype, true);
  st->AddSymbol(proc);
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
      SetError(Peek(), "unreachable token type encountered in module declaration.");
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
  CSymtab *st = s->GetSymbolTable();

  do {
    vt = varDecl(s, &ts);
    Consume(tRelOp, &t);
    if (t.GetValue() != "=") {
      SetError(t, "unexpected operator in constant initializer");
    }
    expr = expression(s);
    for (const string &ident : ts) {
      // TODO: create init values with expr->Evaluate()
      st->AddSymbol(s->CreateConst(ident, vt, new CDataInitInteger(0)));
    }
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
    vt = varDecl(s, &ts);
    for (const string &ident : ts) {
      st->AddSymbol(s->CreateVar(ident, vt));
    }
    Consume(tSemicolon);
  } while (PeekType() == tIdent);
}

const CType *CParser::varDecl(CAstScope *s, vector<string> *idents)
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

  return type(s);
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
  formalParam(s, &params);
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
  formalParam(s, &params);
  Consume(tColon);
  rt = type(s);
  Consume(tSemicolon);

  sym = new CSymProc(t.GetValue(), rt);
  st->AddSymbol(sym);
  for (CSymParam *param : params) {
    sym->AddParam(param);
  }

  return new CAstProcedure(t, t.GetValue(), s, sym);
}

void CParser::formalParam(CAstScope *s, vector<CSymParam *> *params)
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
    type = varDecl(s, &ts);
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
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
  //
  // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tEnd, tElse }
  //
  // FIRST(statement) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statement) = { tSemicolon, tEnd, tElse }
  //

  // The linking of statement sequences is a bit akward here because
  // we implement statSequence as a loop and not recursively.
  // We keep a 'head' that points to the first statement and is finally
  // returned at the end of the function. Head can be NULL if no statement
  // is present.
  // In the loop, we track the end of the linked list using 'tail' and
  // attach new statements to that tail.
  CAstStatement *head = NULL;
  CToken t;

  if (PeekType() != tEnd && PeekType() != tElse) {
    CAstStatement *tail = NULL;

    do {
      CAstStatement *st = NULL;

      switch (PeekType()) {
        // statement ::= assignment | subroutineCall
        case tIdent:
          if (PeekNextType() == tLParen) {
            t = Peek();
            st = new CAstStatCall(t, subroutineCall(s));
          } else if (PeekNextType() == tAssign || PeekNextType() == tLBrack) {
            st = assignment(s);
          } else {
            SetError(Peek(), "unexpected token after identifier.");
          }
          break;
        // statement ::= ifStatement
        case tIf: st = ifStatement(s); break;
        // statement ::= whileStatement
        case tWhile: st = whileStatement(s); break;
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

      if (PeekType() == tEnd || PeekType() == tElse) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign *CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= qualident ":=" expression.
  //

  CToken t;
  CAstDesignator *lhs;
  CAstExpression *rhs;

  lhs = qualident(s);
  Consume(tAssign, &t);
  rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatIf *CParser::ifStatement(CAstScope *s)
{
  //
  // ifStatement ::= "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end".
  //

  CToken t;
  CAstExpression *cond;
  CAstStatement *ifBody = NULL, *elseBody = NULL;

  Consume(tIf, &t);
  Consume(tLParen);
  cond = expression(s);
  Consume(tRParen);
  Consume(tThen);
  ifBody = statSequence(s);
  if (PeekType() == tElse) {
    Consume(tElse);
    elseBody = statSequence(s);
  }
  Consume(tEnd);

  return new CAstStatIf(t, cond, ifBody, elseBody);
}

CAstStatWhile *CParser::whileStatement(CAstScope *s)
{
  //
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end".
  //

  CToken t;
  CAstExpression *cond;
  CAstStatement *body;

  Consume(tWhile, &t);
  Consume(tLParen);
  cond = expression(s);
  Consume(tRParen);
  Consume(tDo);
  body = statSequence(s);
  Consume(tEnd);

  return new CAstStatWhile(t, cond, body);
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

CAstFunctionCall *CParser::subroutineCall(CAstScope *s)
{
  //
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")".
  //
  // FIRST(subroutineCall) = { tIdent }
  //

  CToken t;
  const CSymProc *proc;
  CAstFunctionCall *call;
  CSymtab *st = s->GetSymbolTable();

  Consume(tIdent, &t);
  proc = dynamic_cast<const CSymProc *>(st->FindSymbol(t.GetValue()));
  if (proc == NULL) {
    SetError(t, "unknown subroutine name.");
  }
  call = new CAstFunctionCall(&t, proc);

  Consume(tLParen);
  if (PeekType() != tRParen) {
    call->AddArg(expression(s));
    while (PeekType() == tComma) {
      Consume(tComma);
      call->AddArg(expression(s));
    }
  }
  Consume(tRParen);

  return call;
}

CAstExpression *CParser::expression(CAstScope *s)
{
  //
  // relOp ::= "=" | "#" | "<" | "<=" | ">" | ">=".
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //

  CToken t;
  EOperation op = opNop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (PeekType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=") {
      op = opEqual;
    } else if (t.GetValue() == "#") {
      op = opNotEqual;
    } else if (t.GetValue() == "<") {
      op = opLessThan;
    } else if (t.GetValue() == "<=") {
      op = opLessEqual;
    } else if (t.GetValue() == ">") {
      op = opBiggerThan;
    } else if (t.GetValue() == ">=") {
      op = opBiggerEqual;
    } else {
      SetError(Peek(), "unreachable value encountered in relation operator.");
    }

    return new CAstBinaryOp(t, op, left, right);
  } else {
    return left;
  }
}

CAstExpression *CParser::simpleexpr(CAstScope *s)
{
  //
  // termOp ::= "+" | "-" | "||".
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  CToken t;
  CAstExpression *n = NULL;
  EOperation op = opNop;

  if (PeekType() == tPlusMinus) {
    Consume(tPlusMinus, &t);
  }

  n = term(s);

  if (t.GetType() == tPlusMinus) {
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, n);
  }

  while (PeekType() == tPlusMinus || PeekType() == tOr) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(PeekType(), &t);
    r = term(s);

    if (t.GetValue() == "+") {
      op = opAdd;
    } else if (t.GetValue() == "-") {
      op = opSub;
    } else if (t.GetValue() == "||") {
      op = opOr;
    } else {
      SetError(Peek(), "unreachable value encountered in relation operator.");
    }

    n = new CAstBinaryOp(t, op, l, r);
  }

  return n;
}

CAstExpression *CParser::term(CAstScope *s)
{
  //
  // factOp ::= "*" | "/" | "&&".
  // term ::= factor { factOp factor }.
  //

  CAstExpression *n = NULL;
  EOperation op = opNop;

  n = factor(s);

  while (PeekType() == tMulDiv || PeekType() == tAnd) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(PeekType(), &t);
    r = factor(s);

    if (t.GetValue() == "*") {
      op = opMul;
    } else if (t.GetValue() == "/") {
      op = opDiv;
    } else if (t.GetValue() == "&&") {
      op = opAnd;
    } else {
      SetError(Peek(), "unreachable value encountered in relation operator.");
    }

    n = new CAstBinaryOp(t, op, l, r);
  }

  return n;
}

CAstExpression *CParser::factor(CAstScope *s)
{
  //
  // factor ::= qualident | number | boolean | char | string |
  //            "(" expression ")" | subroutineCall | "!" factor.
  //
  // FIRST(factor) = { tIdent, tNumber, tBoolConst, tCharConst, tStringConst, tLParen, tNot }
  // FOLLOW(factor) <= { tPlusMinus, tMulDiv, tAnd, tOr, tRelOp, tSemicolon, tComma, tRParen,
  //                     tRBrack, tEnd, tElse }
  //

  CToken t;
  CAstExpression *n = NULL;

  switch (PeekType()) {
    // factor ::= qualident
    case tIdent:
      // a factor can never be followed by an tLParen, so if we see one, we can assume that it gets
      // included in the current factor
      if (PeekNextType() == tLParen) {
        n = subroutineCall(s);
      } else {
        n = qualident(s);
      }
      break;

    // factor ::= number
    case tNumber: n = number(); break;

    // factor ::= boolean
    case tBoolConst: n = boolean(); break;

    // factor ::= char
    case tCharConst: n = charConst(); break;

    // factor ::= string
    case tStringConst: n = stringConst(s); break;

    // factor ::= "(" expression ")"
    case tLParen:
      Consume(tLParen);
      n = expression(s);
      Consume(tRParen);
      break;

    // factor ::= "!" factor
    case tNot:
      Consume(tNot, &t);
      n = new CAstUnaryOp(t, opNot, factor(s));
      break;

    default: SetError(Peek(), "factor expected."); break;
  }

  return n;
}

CAstDesignator *CParser::qualident(CAstScope *s)
{
  //
  // ident ::= tIdent.
  // qualident ::= ident { "[" simpleexpr "]" }.
  //

  CToken t;
  const CSymbol *sym;
  CSymtab *st = s->GetSymbolTable();
  CAstExpression *expr;
  CAstArrayDesignator *d;

  Consume(tIdent, &t);

  // check if symbol exists in the symbol table
  sym = st->FindSymbol(t.GetValue());
  if (sym == NULL) SetError(t, "unknown identifier.");

  if (PeekType() != tLBrack) {
    return new CAstDesignator(t, sym);
  }

  d = new CAstArrayDesignator(t, sym);
  while (PeekType() == tLBrack) {
    Consume(tLBrack);
    expr = simpleexpr(s);
    Consume(tRBrack);
    d->AddIndex(expr);
  }

  return d;
}

CAstConstant *CParser::number(void)
{
  //
  // number ::= tNumber.
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInteger(), v);
}

CAstConstant *CParser::boolean(void)
{
  //
  // boolean ::= tBoolConst.
  //

  CToken t;
  long long v;

  Consume(tBoolConst, &t);

  if (t.GetValue() == "true") {
    v = 1;
  } else {
    v = 0;
  }

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstConstant *CParser::charConst(void)
{
  //
  // char ::= tCharConst.
  //

  CToken t;
  char v;

  Consume(tCharConst, &t);
  v = CToken::unescape(t.GetValue())[0];

  return new CAstConstant(t, CTypeManager::Get()->GetChar(), v);
}

CAstStringConstant *CParser::stringConst(CAstScope *s)
{
  //
  // string ::= tStringConst.
  //

  CToken t;

  Consume(tStringConst, &t);
  return new CAstStringConstant(t, t.GetValue(), s);
}

const CType *CParser::type(CAstScope *s)
{
  //
  // type ::= basetype { "[" simpleexpr "]" }.
  //
  // FOLLOW(type) = { tSemicolon, tRParen }
  //

  CAstExpression *expr;
  const CType *type = basetype();

  while (PeekType() == tLBrack) {
    Consume(tLBrack);
    expr = simpleexpr(s);
    Consume(tRBrack);
    // TODO: find array size with expr->Evaluate()
    type = CTypeManager::Get()->GetArray(CArrayType::OPEN, type);
  }

  return type;
}

const CType *CParser::basetype(void)
{
  //
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
