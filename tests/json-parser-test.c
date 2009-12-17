#include <dfsch/lib/json.h>

static int callback(dfsch_object_t* obj, void* baton){
  printf("%s\n", dfsch_object_2_string(obj, 100, 1));
}

int main(){
  dfsch_json_parser_t* jp = dfsch_make_json_parser();
  char buf[1024];

  dfsch_json_parser_set_callback(jp, callback, NULL);

  while (!feof(stdin)){
    if (!fgets(buf, 1024, stdin)){
      break;
    }
    dfsch_json_parser_feed(jp, buf);
  }
}
