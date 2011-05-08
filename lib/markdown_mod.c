#include <dfsch/dfsch.h>
#include "ext/upskirt/markdown.h"
#include "ext/upskirt/renderers.h"

typedef struct renderer_map_t {
  char* name;
  struct mkd_renderer* renderer;
} renderer_map_t;

static renderer_map_t names[] = {
  {"html", &mkd_html},
  {"xhtml", &mkd_xhtml},
  {"discount-html", &discount_html},
  {"discount-xhtml", &discount_xhtml},
  {"nat-html", &nat_html},
  {"nat-xhtml", &nat_xhtml},
};

static struct mkd_renderer* find_renderer(char* name){
  int i;
  for (i = 0; i < sizeof(names) / sizeof(renderer_map_t); i++){
    if (strcmp(name, names[i].name) == 0){
      return names[i].renderer;
    }
  }
  dfsch_error("No such renderer", dfsch_make_string_cstr(name));
}

static struct mkd_renderer* build_renderer(dfsch_object_t* args){
  char* name;
  DFSCH_STRING_OR_SYMBOL_ARG_OPT(args, name, "html");
  DFSCH_ARG_END(args);
  return find_renderer(name);
}

DFSCH_DEFINE_PRIMITIVE(markdown, "Transform text"){
  dfsch_strbuf_t* in;
  struct buf ib;
  struct buf* ob;
  dfsch_object_t* res;  
  DFSCH_BUFFER_ARG(args, in);
  struct mkd_renderer* rndr = build_renderer(args);
  
  ib.data = in->ptr;
  ib.size = in->len;
  ib.asize = 0;
  ib.unit = 0;
  ib.ref = 1;


  ob = bufnew(128);
  markdown(ob, &ib, &mkd_html);
  res = dfsch_make_string_buf(ob->data, ob->size);
  bufrelease(ob);

  return res;
}

void dfsch_module_markdown_register(dfsch_object_t* env){
  dfsch_package_t* markdown = dfsch_make_package("markdown",
                                                 "libupskirt based markdown renderer");
  dfsch_provide(env, "markdown");
  dfsch_defconst_pkgcstr(env, markdown, "markdown",
                         DFSCH_PRIMITIVE_REF(markdown));
}
