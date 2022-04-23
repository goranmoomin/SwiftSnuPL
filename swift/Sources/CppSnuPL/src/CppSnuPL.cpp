#include "CppSnuPL.h"
#include "Scanner.hpp"

#include <cstring>

using namespace std;

const void *CreateSnuPLScanner(const unsigned char *line) {
  CScanner *scanner = new CScanner((const char *)line);
  return scanner;
}

void GetSnuPLToken(const void *s, char **value, int *type, int *line_num, int *char_pos) {
  CScanner *scanner = (CScanner *)s;
  CToken token = scanner->Get();
  *value = strdup(token.GetValue().c_str());
  *type = token.GetType();
  *char_pos = token.GetCharPosition();
  *line_num = token.GetLineNumber();
}

void DestroySnuPLScanner(const void *s) {
  CScanner *scanner = (CScanner *)s;
  delete scanner;
}

void UnescapedSnuPLTokenValue(const char *value, unsigned char **unescaped_value) {
  string unescaped_string = CToken::unescape(value);
  *unescaped_value = (unsigned char *)strdup(unescaped_string.c_str());
}
