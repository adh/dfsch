#include <dfsch/dfsch.h>
#include <dfsch/util.h>

DFSCH_DEFINE_PRIMITIVE(escape_string,
                       "Escape SQL string literal"){
  dfsch_strbuf_t* string;
  size_t res_len = 0;
  dfsch_object_t* res;
  char* obuf;
  size_t i;
  DFSCH_BUFFER_ARG(args, string);
  DFSCH_ARG_END(args);
  
  for (i = 0; i < string->len; i++){
    if (string->ptr[i] == '\''){
      res_len += 2;
    } else {
      res_len++;
    }
  }
  
  res = dfsch_make_string_for_write(res_len + 2, &obuf);
  *obuf = '\'';
  obuf++;
  for (i = 0; i < string->len; i++){
    *obuf = string->ptr[i];
    obuf++;
    if (string->ptr[i] == '\''){
      *obuf = string->ptr[i];
      obuf++;
    }
  }  
  *obuf = '\'';
  obuf++;
  *obuf = '\0';
  return res;
}

void dfsch_module_sql_support_register(dfsch_object_t* env){
  dfsch_package_t* sql_support = dfsch_make_package("sql-support",
                                                    "Native code for SQL "
                                                    "support");
  dfsch_provide(env, "sql-support");
  dfsch_defcanon_pkgcstr(env, sql_support, "escape-string",
                         DFSCH_PRIMITIVE_REF(escape_string));
}
