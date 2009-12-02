#include <dfsch/lib/json.h>

int main(){
  dfsch_json_parser_t* jp = dfsch_make_json_parser();
  char buf[1024];

  while (!feof(stdin)){
    fgets(buf, 1024, stdin);
    dfsch_json_parser_feed(jp, buf);
  }
}
