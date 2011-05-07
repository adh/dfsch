#include <dfsch/dfsch.h>
#include "ext/upskirt/markdown.h"
#include "ext/upskirt/renderers.h"

DFSCH_DEFINE_PRIMITIVE(markdown, "Transform text"){
  dfsch_strbuf_t* in;
  struct buf ib;
  struct buf* ob;
  dfsch_object_t* res;
  DFSCH_BUFFER_ARG(args, in);
  DFSCH_ARG_END(args);
  
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
