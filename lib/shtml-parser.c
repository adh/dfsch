#include <dfsch/lib/shtml.h>
#include <dfsch/util.h>
#include <dfsch/ports.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <dfsch/hash.h>

#include <hubbub/parser.h>
#include <hubbub/tree.h>

static hubbub_error create_comment(void *ctx, const hubbub_string *data, 
		void **result);
static hubbub_error create_doctype(void *ctx, const hubbub_doctype *doctype,
		void **result);
static hubbub_error create_element(void *ctx, const hubbub_tag *tag, 
		void **result);
static hubbub_error create_text(void *ctx, const hubbub_string *data, 
		void **result);
static hubbub_error ref_node(void *ctx, void *node);
static hubbub_error unref_node(void *ctx, void *node);
static hubbub_error append_child(void *ctx, void *parent, void *child, 
		void **result);
static hubbub_error insert_before(void *ctx, void *parent, void *child, 
		void *ref_child, void **result);
static hubbub_error remove_child(void *ctx, void *parent, void *child, 
		void **result);
static hubbub_error clone_node(void *ctx, void *node, bool deep, void **result);
static hubbub_error reparent_children(void *ctx, void *node, void *new_parent);
static hubbub_error get_parent(void *ctx, void *node, bool element_only, 
		void **result);
static hubbub_error has_children(void *ctx, void *node, bool *result);
static hubbub_error form_associate(void *ctx, void *form, void *node);
static hubbub_error add_attributes(void *ctx, void *node,
		const hubbub_attribute *attributes, uint32_t n_attributes);
static hubbub_error set_quirks_mode(void *ctx, hubbub_quirks_mode mode);
static hubbub_error change_encoding(void *ctx, const char *charset);

/* Prototype tree handler struct */
static hubbub_tree_handler tree_handler = {
	create_comment,
	create_doctype,
	create_element,
	create_text,
	ref_node,
	unref_node,
	append_child,
	insert_before,
	remove_child,
	clone_node,
	reparent_children,
	get_parent,
	has_children,
	form_associate,
	add_attributes,
	set_quirks_mode,
	change_encoding,
	NULL
};

DFSCH_LOCAL_SYMBOL_CACHE(":attributes", at_symbol);
DFSCH_LOCAL_SYMBOL_CACHE(":literal-output", literal_symbol);

typedef struct context_t {
  dfsch_object_t* document_element;
  dfsch_hash_t* parent_map;
} context_t;

/**
 * Create a comment node
 *
 * \param ctx     Our context
 * \param data    The comment body
 * \param result  Location to receive manufactured node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count must be 1.
 */
hubbub_error create_comment(void *ctx, const hubbub_string *data, void **result){
  *result = dfsch_make_string_cstr("");
  
  return HUBBUB_OK;
}

/**
 * Create a doctype node
 *
 * \param ctx      Our context
 * \param doctype  Data for doctype node (name, public ID and system ID)
 * \param result   Location to receive manufactured node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count must be 1.
 */
hubbub_error create_doctype(void *ctx, const hubbub_doctype *doctype, void **result){
  *result = dfsch_make_string_cstr("");
  
  return HUBBUB_OK;
}

/**
 * Create an element node
 *
 * \param ctx     Our context
 * \param tag     Data for node
 * \param result  Location to receive manufactured node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count must be 1.
 */
hubbub_error create_element(void *ctx, const hubbub_tag *tag, void **result){
  dfsch_object_t* node = dfsch_cons(dfsch_make_string_buf(tag->name.ptr,
                                                          tag->name.len),
                                    NULL);
  /* TODO: Attempt to add attributes to node */
  if (tag->n_attributes > 0){
    dfsch_error("mnau?", NULL);
  }
  
  *result = (void *) node;
  
  return HUBBUB_OK;
}

/**
 * Create a text node
 *
 * \param ctx     Our context
 * \param data    Node data
 * \param result  Location to receive manufactured node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successfult, result's reference count must be 1.
 */
hubbub_error create_text(void *ctx, const hubbub_string *data, void **result){
  *result = (void *) dfsch_make_string_buf(data->ptr, data->len);

  return HUBBUB_OK;
}

/**
 * Increase a node's reference count
 *
 * \param ctx   Our context
 * \param node  The node to reference
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error ref_node(void *ctx, void *node){
  return HUBBUB_OK;
}

/**
 * Decrease a node's reference count
 *
 * \param ctx   Our context
 * \param node  The node to unreference
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: If the node's reference count becomes zero, and it has no 
 * parent, and it is not the document node, then it is destroyed.
 */
hubbub_error unref_node(void *ctx, void *node){
  return HUBBUB_OK;
}

/**
 * Append a node to the end of another's child list
 *
 * \param ctx     Our context
 * \param parent  The node to append to
 * \param child   The node to append
 * \param result  Location to receive appended node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count is increased by 1
 *
 * Important: *result may not == child (e.g. if text nodes got coalesced)
 */
hubbub_error append_child(void *ctx, void *parent, void *child, void **result){
  dfsch_object_t* chld = (dfsch_object_t*) child;
  dfsch_object_t* p = (dfsch_object_t*) parent;
  context_t* c = (context_t*)ctx;

  /* TODO: text node merging? */
  
  while (DFSCH_PAIR_P(p) && DFSCH_FAST_CDR(p)){
    p = DFSCH_FAST_CDR(p);
  }
  
  dfsch_set_cdr(p, dfsch_cons(chld, NULL));
  dfsch_idhash_set(c->parent_map, chld, p);

  *result = child;
  
  return HUBBUB_OK;
}

/**
 * Insert a node into another's child list
 *
 * \param ctx        Our context
 * \param parent     The node to insert into
 * \param child      The node to insert
 * \param ref_child  The node to insert before
 * \param result     Location to receive inserted node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count is increased by 1
 *
 * Important: *result may not == child (e.g. if text nodes got coalesced)
 */
hubbub_error insert_before(void *ctx, void *parent, void *child, void *ref_child,
                           void **result){
  dfsch_object_t* chld = (dfsch_object_t*) child;
  dfsch_object_t* p = (dfsch_object_t*) parent;
  dfsch_object_t* ref = (dfsch_object_t*) ref_child;
  dfsch_object_t* n;
  context_t* c = (context_t*)ctx;

  while (DFSCH_PAIR_P(p)){
    n = DFSCH_FAST_CDR(p);
    if (DFSCH_PAIR_P(n) && DFSCH_FAST_CAR(n) == ref){
      dfsch_set_cdr(p, dfsch_cons(chld, n));
      dfsch_idhash_set(c->parent_map, chld, p);
      *result = child;
      return;
    }
  }
  
  dfsch_error("Node not found", ref);
}

/**
 * Remove a node from another's child list
 *
 * \param ctx     Our context
 * \param parent  The node to remove from
 * \param child   The node to remove
 * \param result  Location to receive removed node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count is increased by 1
 */
hubbub_error remove_child(void *ctx, void *parent, void *child, void **result)
{
  dfsch_object_t* chld = (dfsch_object_t*) child;
  dfsch_object_t* p = (dfsch_object_t*) parent;
  dfsch_object_t* n;
  context_t* c = (context_t*)ctx;

  while (DFSCH_PAIR_P(p)){
    n = DFSCH_FAST_CDR(p);
    if (DFSCH_PAIR_P(n) && DFSCH_FAST_CAR(n) == chld){
      dfsch_set_cdr(p, DFSCH_FAST_CDR(n));
      dfsch_idhash_unset(c->parent_map, chld);
      *result = child;
      return;
    }
  }
  
  dfsch_error("Node not found", chld);
}

/**
 * Clone a node
 * 
 * \param ctx     Our context
 * \param node    The node to clone
 * \param deep    True to clone entire subtree, false to clone only the node
 * \param result  Location to receive clone
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if successful, result's reference count must be 1.
 */
hubbub_error clone_node(void *ctx, void *node, bool deep, void **result)
{
  dfsch_object_t* n = (dfsch_object_t*)node;
  if (deep){
    dfsch_error("mnau?", NULL);
  }

  if (DFSCH_PAIR_P(n)){
    *result = dfsch_cons(DFSCH_FAST_CAR(n), NULL);
  } else {
    *result = n;
  }
}

/**
 * Move all the children of one node to another
 *
 * \param ctx         Our context
 * \param node        The initial parent node
 * \param new_parent  The new parent node
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error reparent_children(void *ctx, void *node, void *new_parent){
  dfsch_object_t* old = (dfsch_object_t*)node;
  dfsch_object_t* new = (dfsch_object_t*)new_parent;
  
  dfsch_set_cdr(new, dfsch_cdr(old));
  dfsch_set_cdr(old, NULL);

  /* TODO: update parent_map*/
}

/**
 * Retrieve the parent of a node
 *
 * \param ctx           Our context
 * \param node          Node to retrieve the parent of
 * \param element_only  True if the parent must be an element, false otherwise
 * \param result        Location to receive parent node
 * \return HUBBUB_OK on success, appropriate error otherwise
 *
 * Postcondition: if there is a parent, then result's reference count must be
 * increased.
 */
hubbub_error get_parent(void *ctx, void *node, bool element_only, void **result){
  context_t* c = (context_t*)ctx;
  dfsch_object_t* n = (dfsch_object_t*)node;
  dfsch_object_t* p = dfsch_idhash_ref(c->parent_map, n);

  if (p = DFSCH_INVALID_OBJECT){
    p = NULL;
  }

  *result = p;
  return HUBBUB_OK;
}

/**
 * Determine if a node has children
 *
 * \param ctx     Our context
 * \param node    The node to inspect
 * \param result  Location to receive result
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error has_children(void *ctx, void *node, bool *result){
  dfsch_object_t* n = (dfsch_object_t*)node;
  
  *result = dfsch_cdr(n) != NULL;
  
  return HUBBUB_OK;
}

/**
 * Associate a node with a form
 *
 * \param ctx   Our context
 * \param form  The form to associate with
 * \param node  The node to associate
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error form_associate(void *ctx, void *form, void *node){
  return HUBBUB_OK;
}

/**
 * Add attributes to a node
 *
 * \param ctx           Our context
 * \param node          The node to add to
 * \param attributes    Array of attributes to add
 * \param n_attributes  Number of entries in array
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error add_attributes(void *ctx, void *node, 
		const hubbub_attribute *attributes, uint32_t n_attributes){
  return HUBBUB_OK;
}

/**
 * Notification of the quirks mode of a document
 *
 * \param ctx   Our context
 * \param mode  The quirks mode
 * \return HUBBUB_OK on success, appropriate error otherwise
 */
hubbub_error set_quirks_mode(void *ctx, hubbub_quirks_mode mode)
{
	/* In this implementation, we do nothing.
	 * 
	 * The quirks mode is really only of any use when applying CSS 
	 * to the resulting DOM tree.
	 */
	return HUBBUB_OK;
}

/**
 * Notification that a potential encoding change is required
 *
 * \param ctx      Our context
 * \param charset  The new charset for the source data
 * \return HUBBUB_OK to continue using the current input handler, 
 *         HUBBUB_ENCODINGCHANGE to stop processing immediately and 
 *                               return control to the client,
 *         appropriate error otherwise.
 */
hubbub_error change_encoding(void *ctx, const char *charset){
  return HUBBUB_OK;
}

static void* hb_realloc(void* ptr, size_t len, void* discard){
  puts("mnau!");
  return GC_REALLOC(ptr, len);
}

dfsch_object_t* dfsch_shtml_parse_buf(char* buf, size_t len, char* encoding){
  hubbub_error ret;
  hubbub_parser* parser;
  hubbub_tree_handler handler = tree_handler;
  hubbub_parser_optparams params;
  context_t context;

  ret = hubbub_parser_create(encoding, 1, hb_realloc, NULL, &parser);
  if (ret != HUBBUB_OK){
    if (ret == HUBBUB_BADENCODING){
      dfsch_error("Invalid encoding specified", 
                  encoding ? dfsch_make_string_cstr(encoding) : NULL);
    } else {
      dfsch_error("Unable to create parser", NULL);
    }
  }

  context.parent_map = dfsch_make_idhash();
  context.document_element = dfsch_cons(NULL,
                                        NULL);
  
  tree_handler.ctx = &context;
  params.tree_handler = &tree_handler;
  hubbub_parser_setopt(parser, HUBBUB_PARSER_TREE_HANDLER, &params);
  
  params.document_node = context.document_element;
  hubbub_parser_setopt(parser, HUBBUB_PARSER_DOCUMENT_NODE, &params);

  hubbub_parser_parse_chunk(parser, buf, len);
  hubbub_parser_completed(parser);

  return context.document_element;
}
