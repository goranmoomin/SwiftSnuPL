#ifndef __CppSnuPL_H__
#define __CppSnuPL_H__

#ifdef __cplusplus
extern "C" {
#endif

const void *CreateSnuPLScanner(const unsigned char *line);
void GetSnuPLToken(const void *s, char **value, int *type, int *line_num, int *char_pos);
void DestroySnuPLScanner(const void *s);

void UnescapedSnuPLTokenValue(const char *value, unsigned char **unescaped_value);

#ifdef __cplusplus
}
#endif

#endif /* __CppSnuPL_H__ */
