//--------------------------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2019/09/15 Bernhard Egger added support for constant expressions
/// 2020/08/11 Bernhard Egger adapted to SnuPL/2
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

#include "ast.h"

#include <cassert>
#include <cstddef>
#include <cstring>
#include <iostream>
#include <typeinfo>

using namespace std;

//--------------------------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token) : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType *CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr *CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream &operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream &operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
    : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL), _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope *CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope *CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

bool CAstScope::RemoveChild(CAstScope *child)
{
  auto it = _children.begin();
  while ((it != _children.end()) && (*it != child)) it++;

  bool res = it != _children.end();

  if (res) _children.erase(it);
  return res;
}

CSymtab *CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

CSymbol *CAstScope::CreateConst(const string ident, const CType *type, const CDataInitializer *data)
{
  return new CSymConstant(ident, type, data);
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement *CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

  // TODO (phase 3)
  CAstStatement *s = _statseq;
  while (result && s != NULL) {
    result = s->TypeCheck(t, msg);
    s = s->GetNext();
  }

  vector<CAstScope *>::const_iterator it = _children.begin();
  while (result && it != _children.end()) {
    result = (*it)->TypeCheck(t, msg);
    it++;
  }

  return result;
}

ostream &CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent + 4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent + 4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i = 0; i < _children.size(); i++) {
      _children[i]->print(out, indent + 4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope *>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }
}

CTacAddr *CAstScope::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CCodeBlock *CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}

//--------------------------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name) : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol *CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}

//--------------------------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name, CAstScope *parent, CSymProc *symbol)
    : CAstScope(t, name, parent), _symbol(symbol)
{
  CSymtab *st;
  assert(GetParent() != NULL);
  st = new CSymtab(GetParent()->GetSymbolTable());
  assert(_symbol != NULL);
  for (int i = 0; i < _symbol->GetNParams(); i++) {
    st->AddSymbol(new CSymParam(*_symbol->GetParam(i)));
  }
  SetSymbolTable(st);
}

CSymProc *CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol *CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType *CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}

//--------------------------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type) : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType *CAstType::GetType(void) const
{
  return _type;
}

ostream &CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}

//--------------------------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token) : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement *CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr *CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(false);
  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t, CAstDesignator *lhs, CAstExpression *rhs)
    : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator *CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression *CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  if (GetLHS()->TypeCheck(t, msg) && GetRHS()->TypeCheck(t, msg)) {
    if (GetLHS()->GetType()->Match(GetRHS()->GetType())) {
      if (GetLHS()->GetType()->IsArray()) {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Array assignments not supported.";
        return false;
      }
      return true;
    } else {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "Incompatible types in assignment.";
      return false;
    }
  }
  return false;
}

const CType *CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream &CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":="
      << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent + 2);
  _rhs->print(out, indent + 2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr *CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call) : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall *CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg)
{
  return GetCall()->TypeCheck(t, msg);
}

ostream &CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr *CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
    : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope *CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression *CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();

  if (st->Match(CTypeManager::Get()->GetNull())) {
    if (e != NULL) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "Superfluous expression after return.";
      return false;
    }
  } else {
    if (e == NULL) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "Expression expected after return.";
      return false;
    }

    if (!e->TypeCheck(t, msg)) return false;

    if (!st->Match(e->GetType())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "Return type mismatch.";
      return false;
    }
  }

  return true;
}

const CType *CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream &CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return"
      << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent + 2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr *CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond, CAstStatement *ifBody,
                       CAstStatement *elseBody)
    : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression *CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement *CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement *CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  CAstExpression *e = GetCondition();

  if (e == NULL) {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) *msg = "No condition at if-body";
    return false;
  }
  if (!e->TypeCheck(t, msg)) return false;
  if (!e->GetType()->Match(CTypeManager::Get()->GetBool())) {
    if (t != NULL) *t = e->GetToken();
    if (msg != NULL) *msg = "Conditions are not boolean expression.";
    return false;
  }

  CAstStatement *ifbody = GetIfBody();
  CAstStatement *elsebody = GetElseBody();
  if ((ifbody == NULL || ifbody->TypeCheck(t, msg)) &&
      (elsebody == NULL || elsebody->TypeCheck(t, msg)))
    return true;
  return false;
}

ostream &CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else
    out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else
    out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr *CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t, CAstExpression *cond, CAstStatement *body)
    : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression *CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement *CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  CAstExpression *e = GetCondition();

  if (e == NULL) {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) *msg = "No condition at while-body";
    return false;
  }
  if (!e->TypeCheck(t, msg)) return false;
  if (!e->GetType()->Match(CTypeManager::Get()->GetBool())) {
    if (t != NULL) *t = e->GetToken();
    if (msg != NULL) *msg = "Conditions are not boolean expression.";
    return false;
  }

  CAstStatement *body = GetBody();
  if (body == NULL || body->TypeCheck(t, msg)) return true;
  return false;
}

ostream &CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else
    out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr *CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t) : CAstNode(t)
{
}

void CAstExpression::SetParenthesized(bool parenthesized)
{
  _parenthesized = parenthesized;
}

bool CAstExpression::GetParenthesized(void) const
{
  return _parenthesized;
}

const CDataInitializer *CAstExpression::Evaluate(void) const
{
  return NULL;
}

CTacAddr *CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr *CAstExpression::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper) : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}

//--------------------------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper, CAstExpression *l, CAstExpression *r)
    : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd) || (oper == opSub) || (oper == opMul) || (oper == opDiv) ||
         (oper == opAnd) || (oper == opOr) || (oper == opEqual) || (oper == opNotEqual) ||
         (oper == opLessThan) || (oper == opLessEqual) || (oper == opBiggerThan) ||
         (oper == opBiggerEqual));
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression *CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression *CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  if (!GetLeft()->TypeCheck(t, msg) || !GetRight()->TypeCheck(t, msg)) return false;

  const CType *lt = GetLeft()->GetType(), *rt = GetRight()->GetType();
  EOperation oper = GetOperation();
  if (!lt || !rt) {
    assert(false);
    return false;
  }

  if (lt->IsInt() && rt->IsInt()) {
    switch (oper) {
      case opAdd:
      case opSub:
      case opMul:
      case opDiv:
      case opEqual:
      case opNotEqual:
      case opLessThan:
      case opLessEqual:
      case opBiggerThan:
      case opBiggerEqual:
        if ((lt->IsInteger() && rt->IsInteger()) || (lt->IsLongint() && rt->IsLongint())) {
          return true;
        } else {
          if (t != NULL) *t = GetToken();
          if (msg != NULL) *msg = "Type mismatch.";
          return false;
        }
      default: {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Incompatible operator of integer/longint type.";
        return false;
      }
    }
  } else if (lt->IsBoolean() && rt->IsBoolean()) {
    switch (oper) {
      case opAnd:
      case opOr:
      case opEqual:
      case opNotEqual: return true;
      default: {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Incompatible operator of boolean type.";
        return false;
      }
    }
  } else if (lt->IsChar() && rt->IsChar()) {
    switch (oper) {
      case opEqual:
      case opNotEqual:
      case opLessThan:
      case opLessEqual:
      case opBiggerThan:
      case opBiggerEqual: return true;
      default: {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Incompatible operator of boolean type.";
        return false;
      }
    }
  } else {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) *msg = "Type mismatch.";
    return false;
  }
}

const CType *CAstBinaryOp::GetType(void) const
{
  const CType *lt = GetLeft()->GetType(), *rt = GetRight()->GetType();
  EOperation oper = GetOperation();
  CTypeManager *tm = CTypeManager::Get();

  if (!lt || !rt) {
    return NULL;
  }

  if (lt->IsInt() && rt->IsInt()) {
    switch (oper) {
      case opAdd:
      case opSub:
      case opMul:
      case opDiv:
        if (lt->IsInteger() && rt->IsInteger()) {
          return tm->GetInteger();
        } else {
          return tm->GetLongint();
        }
      case opEqual:
      case opNotEqual:
      case opLessThan:
      case opLessEqual:
      case opBiggerThan:
      case opBiggerEqual: return tm->GetBool();
      default: return NULL;
    }
  } else if (lt->IsBoolean() && rt->IsBoolean()) {
    switch (oper) {
      case opAnd:
      case opOr:
      case opEqual:
      case opNotEqual: return tm->GetBool();
      default: return NULL;
    }
  } else if (lt->IsChar() && rt->IsChar()) {
    switch (oper) {
      case opEqual:
      case opNotEqual:
      case opLessThan:
      case opLessEqual:
      case opBiggerThan:
      case opBiggerEqual: return tm->GetBool();
      default: return NULL;
    }
  }

  return NULL;
}

const CDataInitializer *CAstBinaryOp::Evaluate(void) const
{
  const CDataInitializer *li = GetLeft()->Evaluate(), *ri = GetRight()->Evaluate();

  if (!li || !ri) {
    return NULL;
  }

  if (li->IsInt() && ri->IsInt()) {
    EOperation oper = GetOperation();
    long long lv, rv, res;
    lv = li->GetIntData();
    rv = ri->GetIntData();
    switch (oper) {
      case opAdd: res = lv + rv; break;
      case opSub: res = lv - rv; break;
      case opMul: res = lv * rv; break;
      case opDiv:
        if (rv == 0) {
          return NULL;
        }
        res = lv / rv;
        break;
      case opEqual: res = lv == rv; break;
      case opNotEqual: res = lv != rv; break;
      case opLessThan: res = lv < rv; break;
      case opLessEqual: res = lv <= rv; break;
      case opBiggerThan: res = lv > rv; break;
      case opBiggerEqual: res = lv >= rv; break;
      default: return NULL;
    }
    if (oper == opEqual || oper == opNotEqual || oper == opLessThan || oper == opLessEqual ||
        oper == opBiggerThan || oper == opBiggerEqual) {
      return new CDataInitBoolean(res);
    } else if (li->IsInteger() && ri->IsInteger()) {
      return new CDataInitInteger(res);
    } else {
      return new CDataInitLongint(res);
    }
  } else if (li->IsBoolean() && ri->IsBoolean()) {
    EOperation oper = GetOperation();
    bool lv, rv, res;
    lv = dynamic_cast<const CDataInitBoolean *>(li)->GetData();
    rv = dynamic_cast<const CDataInitBoolean *>(ri)->GetData();
    switch (oper) {
      case opAnd: res = lv && rv; break;
      case opOr: res = lv || rv; break;
      case opEqual: res = lv == rv; break;
      case opNotEqual: res = lv != rv; break;
      default: return NULL;
    }
    return new CDataInitBoolean(res);
  } else if (li->IsChar() && ri->IsChar()) {
    EOperation oper = GetOperation();
    char lv, rv;
    bool res;
    lv = dynamic_cast<const CDataInitChar *>(li)->GetData();
    rv = dynamic_cast<const CDataInitChar *>(ri)->GetData();
    switch (oper) {
      case opEqual: res = lv == rv; break;
      case opNotEqual: res = lv != rv; break;
      case opLessThan: res = lv < rv; break;
      case opLessEqual: res = lv <= rv; break;
      case opBiggerThan: res = lv > rv; break;
      case opBiggerEqual: res = lv >= rv; break;
      default: return NULL;
    }
    return new CDataInitBoolean(res);
  }

  return NULL;
}

ostream &CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  _left->print(out, indent + 2);
  _right->print(out, indent + 2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr *CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstBinaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
    : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression *CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  const CType *type = GetOperand()->GetType();
  EOperation oper = GetOperation();

  if (type->IsBoolean()) {
    if (oper != opNot) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "Incompatible unary operator for boolean type.";
      return false;
    }
    return true;
  } else if (type->IsInt()) {
    switch (oper) {
      case opPos:
      case opNeg: return true;
      default:
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Incompatible unary operator for int type.";
        return false;
    }
  } else {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) *msg = "Incompatible type for unary operator.";
    return false;
  }
}

const CType *CAstUnaryOp::GetType(void) const
{
  const CType *t = GetOperand()->GetType();
  EOperation oper = GetOperation();
  CTypeManager *tm = CTypeManager::Get();

  if (t->IsBoolean()) {
    return oper == opNot ? tm->GetBool() : NULL;
  } else if (t->IsInt()) {
    switch (oper) {
      case opPos:
      case opNeg: return t;
      default: return NULL;
    }
  }

  return NULL;
}

const CDataInitializer *CAstUnaryOp::Evaluate(void) const
{
  const CDataInitializer *i = GetOperand()->Evaluate();
  EOperation oper = GetOperation();

  if (!i) {
    return NULL;
  }

  if (i->IsBoolean()) {
    if (oper != opNot) {
      return NULL;
    }
    return new CDataInitBoolean(!dynamic_cast<const CDataInitBoolean *>(i)->GetData());
  } else if (i->IsInt()) {
    long long v, res;

    v = i->GetIntData();
    switch (oper) {
      case opPos: res = v;
      case opNeg: res = -v;
      default: return NULL;
    }

    if (i->IsLongint()) {
      return new CDataInitLongint(res);
    } else {
      return new CDataInitInteger(res);
    }
  }

  return NULL;
}

ostream &CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr *CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstUnaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e, const CType *type)
    : CAstOperation(t, oper), _operand(e), _type(type)
{
  // supported operations
  assert((oper == opAddress) || (oper == opDeref) || (oper == opCast) || (oper == opWiden) ||
         (oper == opNarrow));

  assert(e != NULL);

  // opAddress and opDeref do not expect a type, all others require one
  assert((((oper == opAddress) || (oper == opDeref)) && (type == NULL)) || (type != NULL));
}

CAstExpression *CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  const CType *type = GetOperand()->GetType();
  EOperation oper = GetOperation();

  switch (oper) {
    case opAddress: return true;
    case opDeref:
      if (!type->IsPointer()) {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Cannot dereference non-pointer type.";
        return false;
      }
      return dynamic_cast<const CPointerType *>(type)->GetBaseType() != nullptr;
    case opCast:
    case opWiden:
    case opNarrow: assert(false); return true;
    default: assert(false); return false;
  }
}

const CType *CAstSpecialOp::GetType(void) const
{
  const CType *t = GetOperand()->GetType();
  EOperation oper = GetOperation();
  CTypeManager *tm = CTypeManager::Get();

  switch (oper) {
    case opAddress: return tm->GetPointer(t);
    case opDeref:
      if (!t->IsPointer()) {
        return NULL;
      }
      return dynamic_cast<const CPointerType *>(t)->GetBaseType();
    case opCast:
    case opWiden:
    case opNarrow: return _type;
    default: return NULL;
  }
}

const CDataInitializer *CAstSpecialOp::Evaluate(void) const
{
  // TODO (phase 3)
  assert(false);
  return NULL;
}

ostream &CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr *CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
    : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc *CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  // TODO (phase 3)
  // const CType *argt = arg->GetType();
  // if (argt && argt->IsArray()) {
  //   arg = new CAstSpecialOp(arg->GetToken(), opAddress, arg);
  // }
  _arg.push_back(arg);
}

unsigned int CAstFunctionCall::GetNArgs(void) const
{
  return _arg.size();
}

CAstExpression *CAstFunctionCall::GetArg(unsigned int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

void CAstFunctionCall::SetArg(unsigned int index, CAstExpression *arg)
{
  assert(arg != NULL);
  assert((index >= 0) && (index < _arg.size()));
  _arg[index] = arg;
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  const CSymProc *sym = GetSymbol();
  // function definition matches with function call
  if (sym->GetNParams() == GetNArgs()) {
    for (size_t i = 0; i < GetNArgs(); ++i) {
      if (!GetArg(i)->TypeCheck(t, msg)) return false;

      if (!sym->GetParam(i)->GetDataType()->Match(GetArg(i)->GetType())) {
        if (t != NULL) *t = GetToken();
        if (msg != NULL) *msg = "Argument " + to_string(i + 1) + " type mismatch.";
        return false;
      }

      // check arg is sub-array
      const CAstSpecialOp *sop = dynamic_cast<const CAstSpecialOp *>(GetArg(i));
      if (sop && sop->GetOperation() == opAddress) {
        const CAstExpression *expr = sop->GetOperand();
        if (expr) {
          const CAstArrayDesignator *arrdes = dynamic_cast<const CAstArrayDesignator *>(expr);
          if (arrdes && arrdes->GetNIndices() > 0) {
            if (t != NULL) *t = GetToken();
            if (msg != NULL) *msg = "Subarrays cannot be passed.";
            return false;
          }
        }
      }
    }
    return true;
  } else {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) {
      if (sym->GetNParams() > GetNArgs()) {
        *msg = "More parameters required.";
      } else {
        *msg = "Too many parameters.";
      }
    }
    return false;
  }
}

const CType *CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream &CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->print(out, indent + 2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr *CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstFunctionCall::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t) : CAstExpression(t)
{
}

//--------------------------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol) : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol *CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
}

const CType *CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

const CDataInitializer *CAstDesignator::Evaluate(void) const
{
  const CDataInitializer *data = GetSymbol()->GetData();

  if (!data) {
    return NULL;
  }

  return data->Clone();
}

ostream &CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr *CAstDesignator::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
    : CAstDesignator(t, symbol), _done(false), _offset(NULL)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

unsigned CAstArrayDesignator::GetNIndices(void) const
{
  return _idx.size();
}

CAstExpression *CAstArrayDesignator::GetIndex(unsigned int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg)
{
  assert(_done);

  // TODO (phase 3)
  const CType *type = GetSymbol()->GetDataType();
  const CPointerType *pt = dynamic_cast<const CPointerType *>(type);
  const CArrayType *at;

  if (pt) {
    type = pt->GetBaseType();
  }

  for (size_t i = 0; i < _idx.size(); i++) {
    at = dynamic_cast<const CArrayType *>(type);
    if (!at) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) {
        if (i == 0)
          *msg = "Not an array.";
        else
          *msg = "Dimension error.";
      }
      return false;
    }

    if (!CTypeManager::Get()->GetInteger()->Match(GetIndex(i)->GetType())) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "Array subscription is only allowed with integer type.";
      return false;
    }

    type = at->GetInnerType();
  }

  return true;
}

const CType *CAstArrayDesignator::GetType(void) const
{
  const CType *t = GetSymbol()->GetDataType();
  const CPointerType *pt = dynamic_cast<const CPointerType *>(t);
  const CArrayType *at;

  if (pt) {
    t = pt->GetBaseType();
  }

  for (size_t i = 0; i < _idx.size(); i++) {
    at = dynamic_cast<const CArrayType *>(t);
    if (!at) {
      return NULL;
    }
    t = at->GetInnerType();
  }
  return t;
}

const CDataInitializer *CAstArrayDesignator::Evaluate() const
{
  const CDataInitString *arr = dynamic_cast<const CDataInitString *>(GetSymbol()->GetData());
  const CDataInitInteger *idx;
  char ch;

  if (!arr || GetNIndices() != 1) {
    return NULL;
  }

  idx = dynamic_cast<const CDataInitInteger *>(GetIndex(0)->Evaluate());

  if (!idx) {
    return NULL;
  }

  ch = arr->GetData().at(idx->GetData());
  return new CDataInitChar(ch);
}

ostream &CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->print(out, indent + 2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr *CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstArrayDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
    : CAstOperand(t), _type(type), _value(value), _negated(false)
{
}

void CAstConstant::FoldNeg(void)
{
  _value = -_value;

  // The _negated flag indicates that the value has been negated.
  //
  // All constants in SnulPL/2 are positive numbers that can be folded (negated) by the '-' sign
  // of simpleexpr. To allow LLONG_MIN but still detect invalid positive longint constants with
  // the value LLONG_MAX+1, we need to keep track of whether the value has been folded (negated)
  // or not. The value LLONG_MAX+1, when set via the constructor as a long long becomes LLONG_MIN,
  // but will not have the _negated flag set.
  //
  // Of course, this is nitpicking, but exactness comes at a price...
  //
  _negated = !_negated;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
}

const CType *CAstConstant::GetType(void) const
{
  return _type;
}

const CDataInitializer *CAstConstant::Evaluate(void) const
{
  const CType *type = GetType();
  CTypeManager *tm = CTypeManager::Get();

  if (type == tm->GetBool()) {
    return new CDataInitBoolean(_value);
  } else if (type == tm->GetChar()) {
    return new CDataInitChar(_value);
  } else if (type == tm->GetInteger()) {
    return new CDataInitInteger(_value);
  } else if (type == tm->GetLongint()) {
    return new CDataInitLongint(_value);
  }

  return NULL;
}

ostream &CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr *CAstConstant::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value, CAstScope *s) : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();
  CSymtab *st = s->GetSymbolTable();

  _type = tm->GetArray(strlen(CToken::unescape(value).c_str()) + 1, tm->GetChar());
  _value = new CDataInitString(value);

  // in case of name clashes we simply iterate until we find a
  // name that has not yet been used
  _sym = NULL;
  do {
    ostringstream o;
    o << "_str_" << ++_idx;
    if (st->FindSymbol(o.str(), sGlobal) == NULL) {
      _sym = new CSymGlobal(o.str(), _type);
    }
  } while (_sym == NULL);

  _sym->SetData(_value);
  st->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg)
{
  return true;
}

const CType *CAstStringConstant::GetType(void) const
{
  return _type;
}

const CDataInitializer *CAstStringConstant::Evaluate(void) const
{
  return _value->Clone();
}

ostream &CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(tStringConst, GetValueStr()) << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr *CAstStringConstant::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr *CAstStringConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}
