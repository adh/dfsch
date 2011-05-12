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

typedef struct callbacks_t {
  dfsch_object_t* html_block;
  dfsch_object_t* code_block;
  dfsch_object_t* blockquote;
  dfsch_object_t* link;
  dfsch_object_t* autolink;
  dfsch_object_t* html_tag;
  dfsch_object_t* code_span;
  dfsch_object_t* entity;
  dfsch_object_t* normal_text;
} callbacks_t;

#define SIMPLE_STUB(name)                                               \
  static void stub_##name(struct buf* ob, struct buf* ib, callbacks_t* cb){ \
    dfsch_object_t* res = dfsch_apply(cb->name,                         \
                                      dfsch_list(1,                     \
                                                 dfsch_make_string_buf(ib->data, \
                                                                       ib->size))); \
    if (res) {                                                          \
      dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   \
      bufput(ob, buf->ptr, buf->len);                                   \
    }                                                                   \
  }

#define SIMPLE_STUB_INT(name)                                           \
  static int stub_##name(struct buf* ob, struct buf* ib, callbacks_t* cb){ \
    dfsch_object_t* res = dfsch_apply(cb->name,                         \
                                      dfsch_list(1,                     \
                                                 dfsch_make_string_buf(ib->data, \
                                                                       ib->size))); \
    if (res) {                                                          \
      dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   \
      bufput(ob, buf->ptr, buf->len);                                   \
      return 1;                                                         \
    } else {                                                            \
      return 0;                                                         \
    }                                                                   \
  }

SIMPLE_STUB(html_block)
SIMPLE_STUB(blockquote)
SIMPLE_STUB(code_block)

SIMPLE_STUB(entity)
SIMPLE_STUB(normal_text)

SIMPLE_STUB_INT(code_span)
SIMPLE_STUB_INT(html_tag)

static void stub_link(struct buf* ob, struct buf* link, struct buf* title, 
                      struct buf* content, callbacks_t* cb){  
  dfsch_object_t* res = dfsch_apply(cb->link,                         
                                    dfsch_list(3,                     
                                               dfsch_make_string_buf(link->data, 
                                                                     link->size), 
                                               dfsch_make_string_buf(title->data, 
                                                                     title->size), 
                                               dfsch_make_string_buf(content->data, 
                                                                     content->size)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
    return 1;
  } else {
    return 0;
  }                                                                   
}
static void stub_autolink(struct buf* ob, struct buf* link,
                          enum mkd_autolink type, callbacks_t* cb){  
  dfsch_object_t* res = dfsch_apply(cb->link,                         
                                    dfsch_list(1,                     
                                               dfsch_make_string_buf(link->data, 
                                                                     link->size)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
    return 1;
  } else {
    return 0;
  }                                                                   
}


static struct mkd_renderer* build_renderer(dfsch_object_t* args){
  char* base_renderer_name;
  dfsch_object_t* html_block = DFSCH_INVALID_OBJECT;
  dfsch_object_t* code_block = DFSCH_INVALID_OBJECT;
  dfsch_object_t* blockquote = DFSCH_INVALID_OBJECT;
  dfsch_object_t* link = DFSCH_INVALID_OBJECT;
  dfsch_object_t* autolink = DFSCH_INVALID_OBJECT;
  dfsch_object_t* html_tag = DFSCH_INVALID_OBJECT;
  dfsch_object_t* code_span = DFSCH_INVALID_OBJECT;
  dfsch_object_t* entity = DFSCH_INVALID_OBJECT;
  dfsch_object_t* normal_text = DFSCH_INVALID_OBJECT;
  struct mkd_renderer* base;
  struct mkd_renderer* res;
  callbacks_t* cb = GC_NEW(callbacks_t);

  DFSCH_STRING_OR_SYMBOL_ARG_OPT(args, base_renderer_name, "html");
  base = find_renderer(base_renderer_name);
  if (!args){
    return base;
  }

  DFSCH_KEYWORD_PARSER_BEGIN(args);
  DFSCH_KEYWORD("html-block", html_block);
  DFSCH_KEYWORD("code-block", code_block);
  DFSCH_KEYWORD("blockquote", blockquote);
  DFSCH_KEYWORD("html-tag", html_tag);
  DFSCH_KEYWORD("link", link);
  DFSCH_KEYWORD("autolink", autolink);
  DFSCH_KEYWORD("code-span", code_span);
  DFSCH_KEYWORD("entity", entity);
  DFSCH_KEYWORD("normal_text", normal_text);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  res = GC_NEW(struct mkd_renderer);
  memcpy(res, base, sizeof(struct mkd_renderer));
  res->opaque = cb;

#define OVERRIDE(varname, methname)              \
  if (varname != DFSCH_INVALID_OBJECT){          \
    if (varname) {                               \
      cb->varname = varname;                     \
      res->methname = stub_##varname;            \
    } else {                                     \
      res->methname = NULL;                      \
    }                                            \
  }

  OVERRIDE(code_block, blockcode);
  OVERRIDE(html_block, blockhtml);
  OVERRIDE(blockquote, blockquote);
  OVERRIDE(link, link);
  OVERRIDE(autolink, autolink);
  OVERRIDE(code_span, codespan);
  OVERRIDE(entity, entity);
  OVERRIDE(normal_text, normal_text);


  return res;
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
  markdown(ob, &ib, rndr);
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
