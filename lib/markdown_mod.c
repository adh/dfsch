#include <dfsch/dfsch.h>
#include <dfsch/magic.h>
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
  dfsch_object_t* paragraph;
  dfsch_object_t* link; 
  dfsch_object_t* image;
  dfsch_object_t* autolink;
  dfsch_object_t* html_tag;
  dfsch_object_t* code_span;
  dfsch_object_t* entity;
  dfsch_object_t* normal_text;
  dfsch_object_t* header;
  dfsch_object_t* hrule;
  dfsch_object_t* list;
  dfsch_object_t* list_item;
  dfsch_object_t* line_break;
  dfsch_object_t* emphasis;
  dfsch_object_t* double_emphasis;
  dfsch_object_t* triple_emphasis;
  dfsch_object_t* table;
  dfsch_object_t* table_row;
  dfsch_object_t* table_cell;
} callbacks_t;

#define SIMPLE_STUB(name)                                               \
  static void stub_##name(struct buf* ob, struct buf* ib, callbacks_t* cb){ \
    dfsch_object_t* res =                                               \
      dfsch_apply(cb->name,                                             \
                  dfsch_list(1,                                         \
                             dfsch_make_string_buf(ib->data,            \
                                                   ib->size)));         \
    if (res) {                                                          \
      dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   \
      bufput(ob, buf->ptr, buf->len);                                   \
    }                                                                   \
  }

#define SIMPLE_STUB_INT(name)                                           \
  static int stub_##name(struct buf* ob, struct buf* ib, callbacks_t* cb){ \
    dfsch_object_t* res =                                               \
      dfsch_apply(cb->name,                                             \
                  dfsch_list(1,                                         \
                             dfsch_make_string_buf(ib->data,            \
                                                   ib->size)));         \
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
SIMPLE_STUB(paragraph)

SIMPLE_STUB(entity)
SIMPLE_STUB(normal_text)

SIMPLE_STUB_INT(code_span)
SIMPLE_STUB_INT(html_tag)

#define STUB_LINK_IMG(name) \
  static int stub_##name(struct buf* ob, struct buf* link, struct buf* title, \
                         struct buf* content, callbacks_t* cb){         \
    dfsch_object_t* res =                                               \
      dfsch_apply(cb->name,                                             \
                  dfsch_list(3,                                         \
                             dfsch_make_string_buf(link->data,          \
                                                   link->size),         \
                             dfsch_make_string_buf(title->data,         \
                                                   title->size),        \
                             dfsch_make_string_buf(content->data,       \
                                                   content->size)));    \
    if (res) {                                                          \
      dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   \
      bufput(ob, buf->ptr, buf->len);                                   \
      return 1;                                                         \
    } else {                                                            \
      return 0;                                                         \
    }                                                                   \
  }

STUB_LINK_IMG(link)
STUB_LINK_IMG(image)

static int stub_autolink(struct buf* ob, struct buf* link,
                         enum mkd_autolink type, callbacks_t* cb){  
  dfsch_object_t* res = 
    dfsch_apply(cb->link,                         
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

static void stub_header(struct buf* ob, struct buf* h,
                        int level, callbacks_t* cb){  
  dfsch_object_t* res = 
    dfsch_apply(cb->header,                         
                dfsch_list(2,                     
                           dfsch_make_string_buf(h->data, 
                                                 h->size),
                           DFSCH_MAKE_FIXNUM(level)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}

static void stub_hrule(struct buf* ob, callbacks_t* cb){  
  dfsch_object_t* res = dfsch_apply(cb->hrule, NULL);
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}

static void stub_list(struct buf* ob, struct buf* l,
                      int flags, callbacks_t* cb){  
  dfsch_object_t* res = 
    dfsch_apply(cb->list,                         
                dfsch_list(2,                     
                           dfsch_make_string_buf(l->data, 
                                                 l->size),
                           dfsch_bool(flags & MKD_LIST_ORDERED)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}

static void stub_list_item(struct buf* ob, struct buf* l,
                          int flags, callbacks_t* cb){  
  dfsch_object_t* res = 
    dfsch_apply(cb->list_item,                         
                dfsch_list(2,                     
                           dfsch_make_string_buf(l->data, 
                                                 l->size),
                           dfsch_bool(flags & MKD_LI_BLOCK)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}

static int stub_line_break(struct buf* ob, callbacks_t* cb){  
  dfsch_object_t* res = dfsch_apply(cb->line_break, NULL);
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
    return 1;
  } else {
    return 0;
  }                                                                   
}

#define STUB_EMPH(name)                                                 \
  static int stub_##name(struct buf* ob, struct buf* t,                 \
                         char ch, callbacks_t* cb){                     \
    dfsch_object_t* res =                                               \
      dfsch_apply(cb->name,                                             \
                  dfsch_list(2,                                         \
                             dfsch_make_string_buf(t->data,             \
                                                   t->size),            \
                             DFSCH_MAKE_CHARACTER(ch)));                \
    if (res) {                                                          \
      dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   \
      bufput(ob, buf->ptr, buf->len);                                   \
      return 1;                                                         \
    } else {                                                            \
      return 0;                                                         \
    }                                                                   \
  }

STUB_EMPH(emphasis)
STUB_EMPH(double_emphasis)
STUB_EMPH(triple_emphasis)

static void stub_table(struct buf* ob, struct buf* head_row,
                       struct buf* rows, callbacks_t* cb){
  dfsch_object_t* res = 
    dfsch_apply(cb->table,                         
                dfsch_list(2,                     
                           dfsch_make_string_buf(head_row->data, 
                                                 head_row->size),
                           dfsch_make_string_buf(rows->data, 
                                                 rows->size)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}
static void stub_table_row(struct buf* ob, struct buf* cells,
                           int flags, callbacks_t* cb){
  dfsch_object_t* res = 
    dfsch_apply(cb->table_row,                         
                dfsch_list(1,
                           dfsch_make_string_buf(cells->data, 
                                                 cells->size)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}

static dfsch_object_t* cell_align(int flags){
  flags &= MKD_CELL_ALIGN_MASK;
  switch (flags){
  case MKD_CELL_ALIGN_DEFAULT:
    return dfsch_intern_symbol(DFSCH_KEYWORD_PACKAGE, "default");
  case MKD_CELL_ALIGN_LEFT:
    return dfsch_intern_symbol(DFSCH_KEYWORD_PACKAGE, "left");
  case MKD_CELL_ALIGN_RIGHT:
    return dfsch_intern_symbol(DFSCH_KEYWORD_PACKAGE, "right");
  case MKD_CELL_ALIGN_CENTER:
    return dfsch_intern_symbol(DFSCH_KEYWORD_PACKAGE, "center");
  default:
    dfsch_error("Unsupported table cell alignment", DFSCH_MAKE_FIXNUM(flags));
  }
}

static void stub_table_cell(struct buf* ob, struct buf* cell,
                            int flags, callbacks_t* cb){
  dfsch_object_t* res = 
    dfsch_apply(cb->table_cell,                         
                dfsch_list(3,
                           dfsch_make_string_buf(cell->data, 
                                                 cell->size),
                           dfsch_bool(flags & MKD_CELL_HEAD),
                           cell_align(flags)));
  if (res) {                                                          
    dfsch_strbuf_t* buf = dfsch_string_to_buf(res);                   
    bufput(ob, buf->ptr, buf->len);                                   
  }
}



static struct mkd_renderer* build_renderer(dfsch_object_t* args){
  char* base_renderer_name;
  dfsch_object_t* html_block = DFSCH_INVALID_OBJECT;
  dfsch_object_t* code_block = DFSCH_INVALID_OBJECT;
  dfsch_object_t* blockquote = DFSCH_INVALID_OBJECT;
  dfsch_object_t* paragraph = DFSCH_INVALID_OBJECT;
  dfsch_object_t* link = DFSCH_INVALID_OBJECT;
  dfsch_object_t* image = DFSCH_INVALID_OBJECT;
  dfsch_object_t* autolink = DFSCH_INVALID_OBJECT;
  dfsch_object_t* html_tag = DFSCH_INVALID_OBJECT;
  dfsch_object_t* code_span = DFSCH_INVALID_OBJECT;
  dfsch_object_t* entity = DFSCH_INVALID_OBJECT;
  dfsch_object_t* normal_text = DFSCH_INVALID_OBJECT;
  dfsch_object_t* header = DFSCH_INVALID_OBJECT;
  dfsch_object_t* hrule = DFSCH_INVALID_OBJECT;
  dfsch_object_t* list = DFSCH_INVALID_OBJECT;
  dfsch_object_t* list_item = DFSCH_INVALID_OBJECT;
  dfsch_object_t* line_break = DFSCH_INVALID_OBJECT;
  dfsch_object_t* emphasis = DFSCH_INVALID_OBJECT;
  dfsch_object_t* double_emphasis = DFSCH_INVALID_OBJECT;
  dfsch_object_t* triple_emphasis = DFSCH_INVALID_OBJECT;
  dfsch_object_t* table = DFSCH_INVALID_OBJECT;
  dfsch_object_t* table_row = DFSCH_INVALID_OBJECT;
  dfsch_object_t* table_cell = DFSCH_INVALID_OBJECT;
  char* emphasis_chars = NULL;
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
  DFSCH_KEYWORD("paragraph", paragraph);
  DFSCH_KEYWORD("html-tag", html_tag);
  DFSCH_KEYWORD("link", link);
  DFSCH_KEYWORD("image", image);
  DFSCH_KEYWORD("autolink", autolink);
  DFSCH_KEYWORD("code-span", code_span);
  DFSCH_KEYWORD("entity", entity);
  DFSCH_KEYWORD("normal_text", normal_text);
  DFSCH_KEYWORD("header", header);
  DFSCH_KEYWORD("hrule", hrule);
  DFSCH_KEYWORD("list", list);
  DFSCH_KEYWORD("list-item", list_item);
  DFSCH_KEYWORD("line-break", line_break);
  DFSCH_KEYWORD("emphasis", emphasis);
  DFSCH_KEYWORD("double-emphasis", double_emphasis);
  DFSCH_KEYWORD("triple-emphasis", triple_emphasis);
  DFSCH_KEYWORD("table", table);
  DFSCH_KEYWORD("table-row", table_row);
  DFSCH_KEYWORD("table-cell", table_cell);
  DFSCH_KEYWORD_GENERIC("emphasis-chars", emphasis_chars, 
                        dfsch_string_to_cstr);
  DFSCH_KEYWORD_PARSER_END(args);
  DFSCH_ARG_END(args);

  res = GC_NEW(struct mkd_renderer);
  memcpy(res, base, sizeof(struct mkd_renderer));
  res->opaque = cb;

  if (emphasis_chars){
    res->emph_chars = emphasis_chars;
  }

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
  OVERRIDE(paragraph, paragraph);
  OVERRIDE(link, link);
  OVERRIDE(image, image);
  OVERRIDE(autolink, autolink);
  OVERRIDE(code_span, codespan);
  OVERRIDE(entity, entity);
  OVERRIDE(normal_text, normal_text);
  OVERRIDE(header, header);
  OVERRIDE(hrule, hrule);
  OVERRIDE(list, list);
  OVERRIDE(list_item, listitem);
  OVERRIDE(line_break, linebreak);
  OVERRIDE(emphasis, emphasis)
  OVERRIDE(double_emphasis, double_emphasis);
  OVERRIDE(triple_emphasis, triple_emphasis);
  OVERRIDE(table, table);
  OVERRIDE(table_row, table_row);
  OVERRIDE(table_cell, table_cell);

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
