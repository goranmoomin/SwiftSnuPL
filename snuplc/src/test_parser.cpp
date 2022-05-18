//--------------------------------------------------------------------------------------------------
/// @brief SNUPL parser test
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2014/11/04 Bernhard Egger print dot command instead of running it
/// 2019/09/15 Bernhard Egger support stdin input streams
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

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

#include "parser.h"
#include "scanner.h"

using namespace std;

int main(int argc, char *argv[])
{
  int i = 1;
  bool use_stdin = argc == 1;
  bool make_dot = true;
  char *fn;

  while ((i < argc) || (use_stdin)) {
    istream *in;

    if (use_stdin) {
      fn = strdup("stdin");
      make_dot = false;
      cout << "disabled dot output." << endl;
      in = &cin;
      cout << "parsing from standard input..." << endl;
    } else {
      fn = argv[i];
      if (*fn == ':') {
        make_dot = false;
        cout << "disabled dot output: use ./ to parse relative files starting with a colon."
             << endl;
        fn++;
      }
      in = new ifstream(fn);
      cout << "parsing '" << fn << "'..." << endl;
    }

    CScanner *s = new CScanner(in);
    CParser *p = new CParser(s);

    CAstNode *n = p->Parse();

    if (p->HasError()) {
      const CToken *error = p->GetErrorToken();
      cout << "syntax error at " << error->GetLineNumber() << ":" << error->GetCharPosition()
           << " : " << p->GetErrorMessage() << endl;

      delete n;
    } else {
      CAstModule *m = dynamic_cast<CAstModule *>(n);
      assert(m != NULL);

      cout << "successfully parsed." << endl << "  AST:" << endl;
      m->print(cout, 4);

      if (make_dot) {
        cout << endl << endl;
        string outf = string(fn) + ".ast.dot";
        ofstream out(outf.c_str());
        out << "digraph AST {" << endl
            << "  graph [fontname=\"Times New Roman\",fontsize=10];" << endl
            << "  node  [fontname=\"Courier New\",fontsize=10];" << endl
            << "  edge  [fontname=\"Times New Roman\",fontsize=10];" << endl
            << endl;
        m->toDot(out, 2);
        out << "}" << endl;
        out.flush();

        ostringstream cmd;
        cmd << "dot -Tpdf -o" << fn << ".ast.pdf " << fn << ".ast.dot";
        cout << "run the following command to convert the .dot file into a PDF:" << endl
             << "  " << cmd.str() << endl;
      }

      delete m;
    }

    cout << endl << endl;

    i++;

    delete p;
    delete s;
    if (!use_stdin) delete in;
    use_stdin = false;
  }

  cout << "Done." << endl;

  return EXIT_SUCCESS;
}
