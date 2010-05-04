/*
 * dfsch - Scheme-like Lisp dialect
 * Copyright (C) 2005-2008 Ales Hakl
 *
 *
 */

/*
 * It's not really scheme but something slightly similar, features left out 
 * include:
 * - first class continuations
 * - and maybe something other 
 */

/** @file dfsch/dfsch.h
 *
 * dfsch is quick and dirty implementation of someting that resembles 
 * scheme. This file contains interface specification.
 *
 */


#ifndef H__dfsch__
#define H__dfsch__

#ifndef _REENTRANT
#  define _REENTRANT
#endif
#ifndef GC_THREADS
#  define GC_THREADS
#endif

#include <stdint.h>
#include <pthread.h>
#include <gc/gc.h>

#include <dfsch/defines.h>

#ifdef __cplusplus
extern "C" {
#endif

  /** Continuation used for tail-call elimination. */
  typedef struct dfsch_tail_escape_t dfsch_tail_escape_t;

  /** C datatype for scheme objects */
  typedef struct dfsch_object_t dfsch_object_t;

  /** Representation of scheme datatypes. */
  typedef struct dfsch_type_t dfsch_type_t;
  
  /**
   * C datatype for scheme objects. Used as abstract datatype and also 
   * first field of most objects.
   */
  struct dfsch_object_t {
    /** Pointer to datatype */
    dfsch_type_t *type;
  };

  /**
   * Native functions prototype.  
   *
   * @param baton Context pointer passed to callback
   * @param args Function arguments as scheme list
   * @param esc Tail call continuation - function should pass this value
   *            to functions which accept it and their return value is also
   *            return value of this native function.
   */
  typedef dfsch_object_t* (*dfsch_primitive_impl_t)(void* baton,
                                                    dfsch_object_t* args,
                                                    dfsch_tail_escape_t* esc,
                                                    dfsch_object_t* context);


#include <dfsch/number.h>
#include <dfsch/types.h>
#include <dfsch/writer.h>  

  /** Create object of given type. */
  extern dfsch_object_t* dfsch_make_object(const dfsch_type_t* type);
  extern dfsch_object_t* dfsch_make_object_var(const dfsch_type_t* type, 
                                               size_t size);

  /** @name Comparisons */
  /** @{ */
  /** Same object? (i.e. equal addresses) */
  extern int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b);
  /** Same object or number? */
  extern int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b);
  /** Equal object? (i.e. equal contents) */
  extern int dfsch_equal_p(dfsch_object_t *a, dfsch_object_t *b);
  /** Get object hash */
  extern uint32_t dfsch_hash(dfsch_object_t* obj);
  /** @} */

  extern dfsch_type_t* dfsch_object_as_type(dfsch_object_t* obj);


  /** Get object type */
  extern dfsch_type_t* dfsch_type_of(dfsch_object_t* obj);
  /* Is super superclass of sub */
  extern int dfsch_superclass_p(dfsch_type_t* sub, dfsch_type_t* super);
  /** Is object direct or indirect instance of given type? */
  extern int dfsch_instance_p(dfsch_object_t* obj, dfsch_type_t* type);
  extern void* dfsch_assert_type(dfsch_object_t* obj, dfsch_type_t* type);
  extern dfsch_object_t* dfsch_assert_instance(dfsch_object_t* obj, dfsch_type_t* type);
  /** Get superclass of given type */
  extern dfsch_object_t* dfsch_superclass(dfsch_object_t* obj);

  extern dfsch_slot_t* dfsch_find_slot(dfsch_type_t* type, char* name);
  extern dfsch_object_t* dfsch_get_slots(dfsch_type_t* type);

  extern dfsch_object_t* dfsch_slot_ref(dfsch_object_t* obj, 
                                        dfsch_slot_t* slot,
                                        int debug);
  extern void dfsch_slot_set(dfsch_object_t* obj, 
                             dfsch_slot_t* slot, 
                             dfsch_object_t* value,
                             int debug);
  extern dfsch_object_t* dfsch_slot_ref_by_name(dfsch_object_t* obj, 
                                                char* slot,
                                                int debug);
  extern void dfsch_slot_set_by_name(dfsch_object_t* obj, 
                                     char* slot, 
                                     dfsch_object_t* value,
                                     int debug);

  extern dfsch_object_t* dfsch_make_slot_accessor(dfsch_type_t* type,
                                                  char* slot);
  extern dfsch_object_t* dfsch_make_slot_reader(dfsch_type_t* type,
                                                char* slot);
  extern dfsch_object_t* dfsch_make_slot_writer(dfsch_type_t* type,
                                                char* slot);


  /** Is OBJ null? */
  extern int dfsch_null_p(dfsch_object_t* obj);
  /** Is OBJ a pair? */
  extern int dfsch_pair_p(dfsch_object_t* obj);
  /** Is OBJ a proper list? */
  extern int dfsch_list_p(dfsch_object_t* obj);
  /** Is OBJ an atom? (i.e. not pair) */
  extern int dfsch_atom_p(dfsch_object_t* obj); // i.e. not pair
  /** Is OBJ a symbol? */
  extern int dfsch_symbol_p(dfsch_object_t* obj);
  /** Is OBJ a keyword? */
  extern int dfsch_keyword_p(dfsch_object_t* obj);
  /** Is OBJ a a number? */
  extern int dfsch_number_p(dfsch_object_t* obj);
  /** Is OBJ a primitive (native) function? */
  extern int dfsch_primitive_p(dfsch_object_t* obj);
  /** Is OBJ a lambda-closure? */
  extern int dfsch_closure_p(dfsch_object_t* obj);
  /** Is OBJ an applicable procedure? */
  extern int dfsch_procedure_p(dfsch_object_t* obj);
  /** Is OBJ a macro? */
  extern int dfsch_macro_p(dfsch_object_t* obj);
  /** Is OBJ a special form? */
  extern int dfsch_form_p(dfsch_object_t* obj);
  /** Is OBJ an exception? */
  extern int dfsch_exception_p(dfsch_object_t* obj);
  /** Is OBJ a vector? */
  extern int dfsch_vector_p(dfsch_object_t* obj);

  /** Parse string into object. */
  extern dfsch_object_t* dfsch_string_2_object(char* str);
  /** Parse string into list of objects */
  extern dfsch_object_t* dfsch_string_2_object_list(char* str);
  
  extern char* dfsch_object_2_string(dfsch_object_t* obj, 
                                     int max_depth, int readable);
  
  /** Returns empty list, equivalent to NULL */
  extern dfsch_object_t* dfsch_nil();

  /** Construct pair object */
  extern dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr);
  /** Construct immutable pair object */
  extern dfsch_object_t* dfsch_cons_immutable(dfsch_object_t* car, 
                                              dfsch_object_t* cdr);

  /** Return first (car) item of pair */
  extern dfsch_object_t* dfsch_car(dfsch_object_t* pair);
  /** Return second (cdr) item of pair */
  extern dfsch_object_t* dfsch_cdr(dfsch_object_t* pair);

  /** Set first (car) item of pair */
  extern dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
				       dfsch_object_t* c);
  /** Set second (cdr) item of pair */
  extern dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
				       dfsch_object_t* c);

  /** Returns number of items in given list or -1 for infinite lists */
  extern long dfsch_list_length(dfsch_object_t* list, int* proper);
  /** Returns number of items in given finite list */
  extern long dfsch_list_length_fast(dfsch_object_t* list);
  /** Returns number of items in given list if less than 65536 */
  extern long dfsch_list_length_fast_bounded(dfsch_object_t* list);
  /** Returns number of items in given finite list, fail otherwise */
  extern long dfsch_list_length_check(dfsch_object_t* list);
  /** Check whenever list is mutable */
  extern int dfsch_list_mutable_p(dfsch_object_t* list);
  /** Return mutable list - argument if it is mutable, fresh copy if not */
  extern dfsch_object_t* dfsch_ensure_mutable_list(dfsch_object_t* list);

  typedef struct dfsch_list_collector_t dfsch_list_collector_t;
  extern dfsch_list_collector_t* dfsch_make_list_collector();
  extern void dfsch_list_collect(dfsch_list_collector_t* col,
                                 dfsch_object_t* item);
  extern dfsch_object_t* dfsch_collected_list(dfsch_list_collector_t* col);

  /** Returns given item of list. */
  extern dfsch_object_t* dfsch_list_item(dfsch_object_t* list, size_t index);
  extern void dfsch_set_list_item(dfsch_object_t* list, 
                                  size_t index,
                                  dfsch_object_t* value);
  /** Construct list from C array. */
  extern dfsch_object_t* dfsch_list_from_array(dfsch_object_t** array, 
                                               size_t length);
  /** Convert struct to C array */
  extern dfsch_object_t** dfsch_list_as_array(dfsch_object_t* list, 
                                              size_t* length);

  extern dfsch_object_t* dfsch_zip(dfsch_object_t* llist);
  /** Concatenate lists */
  extern dfsch_object_t* dfsch_append(dfsch_object_t* llist);
  /** Construct list from arguments */
  extern dfsch_object_t* dfsch_list(size_t count, ...);
  /** Construct list from arguments */
  extern dfsch_object_t* dfsch_immutable_list(size_t count, ...);
  extern dfsch_object_t* dfsch_immutable_list_cdr(dfsch_object_t* cdr,
                                                  size_t count, ...);
  /** Copy list. */
  extern dfsch_object_t* dfsch_list_copy(dfsch_object_t* list);
  extern dfsch_object_t* dfsch_list_copy_immutable(dfsch_object_t* list);
  extern dfsch_object_t* dfsch_null_immutable_list(size_t l);
  extern dfsch_object_t* dfsch_list_annotate(dfsch_object_t* list, 
                                             dfsch_object_t* source,
                                             dfsch_object_t* location);
  extern dfsch_object_t* dfsch_get_list_annotation(dfsch_object_t* list);

  

  /** Reverses list */
  extern dfsch_object_t* dfsch_reverse(dfsch_object_t* list);

  /** Return alist entry with car equal? to key */
  extern dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
				     dfsch_object_t *alist);
  /** Return alist entry with car eqv? to key */
  extern dfsch_object_t* dfsch_assv(dfsch_object_t *key,
                                    dfsch_object_t *alist);
  /** Return alist entry with car eq? to key */
  extern dfsch_object_t* dfsch_assq(dfsch_object_t *key,
                                    dfsch_object_t *alist);

  /** Return first cell of list with car equal? to key*/
  extern dfsch_object_t* dfsch_member(dfsch_object_t *key,
                                      dfsch_object_t *alist);
  /** Return first cell of list with car eqv? to key*/
  extern dfsch_object_t* dfsch_memv(dfsch_object_t *key,
                                    dfsch_object_t *alist);
  /** Return first cell of list with car eq? to key*/
  extern dfsch_object_t* dfsch_memq(dfsch_object_t *key,
                                    dfsch_object_t *alist);
  /** Sort list according to passed comparison function */
  dfsch_object_t* dfsch_sort_list(dfsch_object_t* list,
                                  dfsch_object_t* comp);


  /** Expand quasi-quoted expression */
  extern dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, 
                                          dfsch_object_t* arg);

  /** Makes symbol object from string. */
  extern dfsch_object_t* dfsch_make_symbol(char* symbol);

  /** Returns unique generated symbol. */
  extern dfsch_object_t* dfsch_gensym();

  /** Returns string representation of given symbol. */
  extern char* dfsch_symbol(dfsch_object_t* symbol);
  extern char* dfsch_symbol_qualified_name(dfsch_object_t* o);
  extern dfsch_package_t* dfsch_symbol_package(dfsch_object_t* symbol);

  /** Convert symbol into string usable as name of type 
   * (remove angle brackets) */
  extern char* dfsch_symbol_2_typename(dfsch_object_t* symbol);

  /** 
   * Return true if symbol name is same as passed string and symbol is 
   * interned in keyword package */
  extern int dfsch_compare_keyword(dfsch_object_t* symbol,
                                   char* string);
  /**
   * Return true if symbol name is same as given string and symbol is 
   * interned in given package */
  extern int dfsch_compare_symbol(dfsch_object_t* symbol,
                                  dfsch_package_t* package,
                                  char* name);
  /** Return package object matching given name */
  extern dfsch_package_t* dfsch_find_package(char* name);
  extern dfsch_object_t* dfsch_make_package(char* name);
  extern dfsch_package_t* dfsch_package_designator(dfsch_object_t* obj);
  /** Retrun current default package (as in CL's *package*) */
  extern dfsch_package_t* dfsch_get_current_package();
  /** Set new value of current default package */
  extern void dfsch_set_current_package(dfsch_package_t* package);
  /** Intern symbol in keyword package (name does not contain leading colon) */
  extern dfsch_object_t* dfsch_make_keyword(char* symbol);
  /** Intern unqualified symbol in given package, qualified symbols are 
   * interned in package denoted by their package qualifier.*/
  extern dfsch_object_t* dfsch_intern_symbol(dfsch_package_t* package,
                                             char* name);
  extern dfsch_object_t* dfsch_list_all_packages();
  /** Return name of given package */
  extern char* dfsch_package_name(dfsch_object_t* package);
  /** Intern symbol with same name in keyword package */
  extern dfsch_object_t* dfsch_symbol_2_keyword(dfsch_object_t* sym);

  /** Return true or nil depending on value of BOOL. */
  extern dfsch_object_t* dfsch_bool(int bool);

  typedef void (*dfsch_package_iteration_cb_t)(void* baton,
                                               dfsch_object_t* symbol);


  /** Create new lambda closure. */
  extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				      dfsch_object_t* args,
				      dfsch_object_t* code);

  /** Create new named lambda closure. */
  extern dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                            dfsch_object_t* args,
                                            dfsch_object_t* code,
                                            dfsch_object_t* name);


  /** Create native function object */
  extern dfsch_object_t* dfsch_make_primitive(dfsch_primitive_impl_t prim,
					      void *baton);

  /** Create native function object */
  extern dfsch_object_t* dfsch_make_primitive_flags(dfsch_primitive_impl_t prim,
                                                    void *baton,
                                                    int flags);


  // vectors

  /** Create vector */
  extern dfsch_object_t* dfsch_make_vector(size_t length, 
                                           dfsch_object_t *fill);
  /** Construct vector from arguments. */
  extern dfsch_object_t* dfsch_vector(size_t count, ...);
  /** Returns length of given vector. */
  extern size_t dfsch_vector_length(dfsch_object_t *vector);
  /** Returns pointer to internal object array of vector */
  extern dfsch_object_t** dfsch_vector_as_array(dfsch_object_t *vector,
                                                size_t *length);
  /** Creates vector from C array */
  extern dfsch_object_t* dfsch_vector_from_array(dfsch_object_t **array, 
                                                 size_t length);
  /** Returns contents of k-th slot of vector. */
  extern dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k);
  /** Sets value of k-th slot of vector to obj. */
  extern dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                          dfsch_object_t* obj);
  /** Converts vector to list. */
  extern dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector);
  /** Converts list to vector. */
  extern dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list);

  /** Wraps procedure for use as macro. */
  extern dfsch_object_t* dfsch_make_macro(dfsch_object_t *proc);


  /** Wraps procedure for use as special form. */
  extern dfsch_object_t* dfsch_make_form(dfsch_form_impl_t impl,
                                         void* baton,
                                         char* name);


  // error handling

  extern void dfsch_throw(dfsch_object_t* tag, dfsch_object_t* value);

  /** Convenience wrapper for signaling error condition from C code */
  extern void dfsch_error(char* type, 
                          dfsch_object_t* data);
  /** Convenience wrapper for signaling recoverable error condition */
  extern void dfsch_cerror(char* type, 
                           dfsch_object_t* data);

  /** Apply procedure later in evaluation (useful for signal handlers) */
  extern void dfsch_async_apply_self(dfsch_object_t* proc);
  /** Check for pending asynchronous apply */
  extern void dfsch_async_apply_check();


  // Lexical binding:
  /** Create new environment frame. */
  extern dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent);
  /** Create new environment frame. */
  extern dfsch_object_t* dfsch_new_frame_with_context(dfsch_object_t* parent,
                                                      dfsch_object_t* context);

  /** Get value of variable name in environment env. */
  extern dfsch_object_t* dfsch_lookup(dfsch_object_t* name, 
				      dfsch_object_t* env);
  extern dfsch_object_t* dfsch_compile_lambda_list(dfsch_object_t* list);
  extern dfsch_object_t* dfsch_destructuring_bind(dfsch_object_t* arglist, 
                                                  dfsch_object_t* list, 
                                                  dfsch_object_t* env);

  /**
   * Get value of variable name in environment env. Return empty list
   * in case of failure.
   */
  extern dfsch_object_t* dfsch_env_get(dfsch_object_t* name, 
                                       dfsch_object_t* env);

  extern dfsch_object_t* dfsch_variable_constant_value(dfsch_object_t* name, 
                                                       dfsch_object_t* env);

  /** Set value of variable name in environment env to value. */
  extern dfsch_object_t* dfsch_set(dfsch_object_t* name,
				   dfsch_object_t* value,
				   dfsch_object_t* env);
  /** Unset variable name in environment env */
  extern void dfsch_unset(dfsch_object_t* name, dfsch_object_t* env);

#define DFSCH_VAR_CONSTANT 1
  

  /** Define variable name in environment env */
  extern void dfsch_define(dfsch_object_t* name,
                           dfsch_object_t* value,
                           dfsch_object_t* env,
                           short flags);

  extern dfsch_object_t* dfsch_get_environment_variables(dfsch_object_t* env);

  extern dfsch_object_t* dfsch_find_lexical_context(dfsch_object_t* env,
                                                    dfsch_type_t* klass);

  extern dfsch_object_t* dfsch_macro_expand(dfsch_object_t* macro,
                                            dfsch_object_t* args);


  // EVAL+APPLY

  /** Evaluate expression. */
  extern dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env);
  /** Evaluate multiple expressions returning value of final one. */
  extern dfsch_object_t* dfsch_eval_proc(dfsch_object_t* exp, 
                                         dfsch_object_t* env);
  /** Evaluate multiple expressions and return values of all of them */
  extern dfsch_object_t* dfsch_eval_list(dfsch_object_t* list, 
					 dfsch_object_t* env);
  /** Apply procedure to given arguments*/
  extern dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args);
  /** */
  extern dfsch_object_t* dfsch_apply_with_context(dfsch_object_t* proc, 
                                                  dfsch_object_t* args,
                                                  dfsch_object_t* context,
                                                  dfsch_tail_escape_t* esc);
  
  /** Extended variant of dfsch_eval_proc with support for tail recursion */
  extern dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            dfsch_tail_escape_t* esc);
  /** Extended variant of dfsch_apply with support for tail recursion */
  extern dfsch_object_t* dfsch_apply_tr(dfsch_object_t* proc, 
                                        dfsch_object_t* args,
                                        dfsch_tail_escape_t* esc);
  /** Extended variant of dfsch_eval with support for tail recursion */
  extern dfsch_object_t* dfsch_eval_tr(dfsch_object_t* exp, 
                                       dfsch_object_t* env,
                                       dfsch_tail_escape_t* esc);


  // context

  /** Allocates new top-level environment. */
  extern dfsch_object_t* dfsch_make_top_level_environment();
  /** Define new variable in given context */
  extern void dfsch_define_cstr(dfsch_object_t *ctx, 
                                char *name, 
                                void *obj); /* to suppress warnings*/
  extern void dfsch_defconst_cstr(dfsch_object_t *ctx, 
                                  char *name, 
                                  void *obj); /* to suppress warnings*/
  extern void dfsch_define_pkgcstr(dfsch_object_t *ctx, 
				   dfsch_package_t* package,
				   char *name, 
				   void *obj); /* to suppress warnings*/
  extern void dfsch_defconst_pkgcstr(dfsch_object_t *ctx, 
				     dfsch_package_t* package,
				     char *name, 
				     void *obj); /* to suppress warnings*/

  /** Change value of variable. */
  extern void dfsch_set_cstr(dfsch_object_t *env, 
                             char *name, 
                             dfsch_object_t *obj);

  /** Looks up value of variable given by NAME. Throws 
      exception:unbound-variable if such variable doesn't exist */
  extern dfsch_object_t* dfsch_lookup_cstr(dfsch_object_t *ctx, char *name);
  /** Look up value of given variable, returns list of one item if sucessful,
      empty list if not*/
  extern dfsch_object_t* dfsch_env_get_cstr(dfsch_object_t *ctx, char *name);

  /** Return stack trace for running thread. */
  extern dfsch_object_t* dfsch_get_stack_trace();

  extern char* dfsch_get_version();
  extern char* dfsch_get_build_id();

  /** @name Generic collections */
  /** @{ */
  
  extern dfsch_object_t* dfsch_assert_collection(dfsch_object_t* obj);
  extern dfsch_object_t* dfsch_assert_mapping(dfsch_object_t* obj);
  extern dfsch_object_t* dfsch_assert_sequence(dfsch_object_t* obj);
  size_t dfsch_assert_sequence_index(dfsch_object_t* seq, size_t idx, size_t len);  

  dfsch_object_t* dfsch_collection_get_iterator(dfsch_object_t* col);
  dfsch_object_t* dfsch_sequence_ref(dfsch_object_t* seq,
                                     size_t k);
  void dfsch_sequence_set(dfsch_object_t* seq,
                          size_t k,
                          dfsch_object_t* value);
  size_t dfsch_sequence_length(dfsch_object_t* seq);
  

  dfsch_object_t* dfsch_iterator_next(dfsch_object_t* iterator);
  dfsch_object_t* dfsch_iterator_this(dfsch_object_t* iterator);

  dfsch_object_t* dfsch_mapping_ref(dfsch_object_t* map,
                                    dfsch_object_t* key);
  void dfsch_mapping_set(dfsch_object_t* map,
                         dfsch_object_t* key,
                         dfsch_object_t* value);
  int dfsch_mapping_unset(dfsch_object_t* map,
                          dfsch_object_t* key);
  int dfsch_mapping_set_if_exists(dfsch_object_t* map,
                                  dfsch_object_t* key,
                                  dfsch_object_t* value);
  int dfsch_mapping_set_if_not_exists(dfsch_object_t* map,
                                      dfsch_object_t* key,
                                      dfsch_object_t* value);



  /** @} */

  
#include <dfsch/strings.h>

#define DFSCH_TRUE_P(o) ((o) != NULL)

  /**
   * Parses one argument of no specific type from argument list and assigns it
   * to given variable (or l-value)
   *
   * @param al Argument list
   * @param name Variable or l-value (also used as argument name in exceptions)
   */
#define DFSCH_OBJECT_ARG(al, name)                      \
  if (DFSCH_UNLIKELY(!DFSCH_PAIR_P((al))))              \
    dfsch_error("exception:required-argument-missing",  \
                dfsch_make_string_cstr(#name));         \
  (name) = DFSCH_FAST_CAR((al));                        \
  (al) = DFSCH_FAST_CDR((al))

  /**
   * Parses one argument of no specific type from argument list and discards it
   *
   * @param al Argument list
   * @param name Argument name (used only in exceptions)
   */
#define DFSCH_DISCARD_ARG(al, name)                     \
  if (DFSCH_UNLIKELY(!DFSCH_PAIR_P((al))))              \
    dfsch_error("exception:required-argument-missing",  \
                dfsch_make_string_cstr(#name));         \
  (al) = DFSCH_FAST_CDR((al))

  /**
   * Parses one argument of no specific type from argument list and assigns it
   * to given variable (or l-value). Uses default value instead of throwing 
   * exception when there are no arguments left.
   *
   * @param al Argument list
   * @param name Variable or l-value.
   * @param default Default value 
   */
#define DFSCH_OBJECT_ARG_OPT(al, name,default)  \
  if (!DFSCH_PAIR_P((al)))                      \
    { (name) = (default);}else                  \
    {(name) = DFSCH_FAST_CAR((al));             \
      (al) = DFSCH_FAST_CDR((al));}

  /**
   * Parses one argument from arguments list and converts it using given 
   * function. Intended for use in other macros, such as DFSCH_STRING_ARG.
   *
   * @param al Argument list
   * @param name Variable or l-value.
   * @param type C type of result
   * @param conv Function for conversion from dfsch_object_t* to given type.
   */
#define DFSCH_GENERIC_ARG(al, name, type, conv)         \
  if (DFSCH_UNLIKELY(!DFSCH_PAIR_P((al))))              \
    dfsch_error("exception:required-argument-missing",  \
                dfsch_make_string_cstr(#name));         \
  { dfsch_object_t* dfsch___tmp = DFSCH_FAST_CAR((al)); \
    (name) = (type)(conv)(dfsch___tmp);                 \
    (al) = DFSCH_FAST_CDR((al));                        \
  }
  /**
   * Parses one argument from arguments list and converts it using given 
   * function. Intended for use in other macros, such as DFSCH_STRING_ARG.
   * Uses default value instead of throwing exception when no arguments are 
   * left.
   *
   * @param al Argument list
   * @param name Variable or l-value.
   * @param default Default value
   * @param type C type of result
   * @param conv Function for conversion from dfsch_object_t* to given type.
   */
#define DFSCH_GENERIC_ARG_OPT(al, name, default, type, conv)    \
  if (!DFSCH_PAIR_P((al)))                                      \
    {(name)=(default);} else                                    \
    { dfsch_object_t* dfsch___tmp = DFSCH_FAST_CAR((al));       \
      (name) = (type)(conv)(dfsch___tmp);                       \
      (al) = DFSCH_FAST_CDR((al));                              \
    }

  /**
   * Throws exception if arguments list contain any arguments.
   *
   * @param al Argument list
   */
#define DFSCH_ARG_END(al)                               \
  if ((al) != NULL)                                     \
    dfsch_error("exception:too-many-arguments",NULL)

  /**
   * Store all unprocessed arguments into rest. (Syntactic sugar 2.0 :))
   */
#define DFSCH_ARG_REST(al, rest)                \
  (rest) = dfsch_list_copy(al)


#define DFSCH_OBJECT_CACHE(constructor, name)                           \
  dfsch_object_t* name(){                                               \
    static dfsch_object_t* dfsch___cache = NULL;                        \
    if (!dfsch___cache)                                                 \
      dfsch___cache = constructor;                                      \
    return dfsch___cache;                                               \
  }// This depends on full-word stores being atomic (which they generally are)


#define DFSCH_SYMBOL_CACHE(symbol, name)                \
  DFSCH_OBJECT_CACHE(dfsch_make_symbol(symbol), name)

#define DFSCH_LOCAL_SYMBOL_CACHE(symbol, name)  \
  static DFSCH_SYMBOL_CACHE(symbol, name)


#define DFSCH_FLAG_PARSER_BEGIN(args)                           \
  while (DFSCH_PAIR_P((args))){                                 \
  dfsch_object_t* dfsch___flag = DFSCH_FAST_CAR((args));

#define DFSCH_FLAG_PARSER_BEGIN_SYM_ONLY(args)                  \
  while (DFSCH_PAIR_P((args))){                                 \
  dfsch_object_t* dfsch___flag = DFSCH_FAST_CAR((args));        \
  if (!dfsch_symbol_p(dfsch___flag)) break;
  
#define DFSCH_FLAG_PARSER_BEGIN_ONE(args, name)                 \
  if (!DFSCH_PAIR_P((args))){                                   \
    dfsch_error("exception:required-argument-missing", #name);  \
  }                                                             \
  {                                                             \
  dfsch_object_t* dfsch___flag;                                 \
  dfsch___flag = DFSCH_FAST_CAR((args));                        \
  if (!dfsch_symbol_p(dfsch___flag)) {                          \
    dfsch_error("exception:not-a-symbol", dfsch___flag);        \
  }

#define DFSCH_FLAG_PARSER_BEGIN_ONE_OPT(args, name)             \
  if (DFSCH_PAIR_P((args))){                                    \
  dfsch_object_t* dfsch___flag;                                 \
  dfsch___flag = DFSCH_FAST_CAR((args));                        \
  if (!dfsch_symbol_p(dfsch___flag)) {                          \
    dfsch_error("exception:not-a-symbol", dfsch___flag);        \
  }

      
#define DFSCH_FLAG_VALUE(name, value, variable)                         \
  if (dfsch_compare_keyword(dfsch___flag, (name))) (variable) = (value)    
    
#define DFSCH_FLAG_SET(name, value, variable)                           \
  if (dfsch_compare_keyword(dfsch___flag, (name))) (variable) |= (value)    
  
#define DFSCH_FLAG_UNSET(name, value, variable)                         \
  if (dfsch_compare_keyword(dfsch___flag ,(name))) (variable) &= ~(value)    
  
#define DFSCH_FLAG_FUNC(name)                           \
  if (dfsch_compare_keyword(dfsch___flag ,(name))) 
  
#define DFSCH_FLAG_PARSER_END(args)             \
  (args) = DFSCH_FAST_CDR((args));              \
}

#define DFSCH_KEYWORD_PARSER_BEGIN(args)                                \
  while (DFSCH_PAIR_P((args))){                                         \
  dfsch_object_t* dfsch___keyword;                                      \
  dfsch_object_t* dfsch___value;                                        \
  dfsch___keyword = DFSCH_FAST_CAR((args));                             \
  (args) = DFSCH_FAST_CDR((args));                                      \
  if (!DFSCH_PAIR_P((args))){                                           \
    dfsch_error("exception:keyword-without-arguemnt", dfsch___keyword); \
  }                                                                     \
  dfsch___value = DFSCH_FAST_CAR((args));                               \
  (args) = DFSCH_FAST_CDR((args));
  
#define DFSCH_KEYWORD(name, variable)                   \
  if (dfsch_compare_keyword(dfsch___keyword, (name))){  \
    (variable) = (dfsch___value);                       \
    continue;                                           \
  }
#define DFSCH_KEYWORD_GENERIC(name, variable, conv)     \
  if (dfsch_compare_keyword(dfsch___keyword, (name))){  \
    (variable) = conv(dfsch___value);                   \
    continue;                                           \
  }
  

#define DFSCH_KEYWORD_PARSER_END(args)                          \
  dfsch_error("exception:unknown-keyword", dfsch___keyword);    \
}

#define DFSCH_SYMBOL_ARG(al, name)                      \
  DFSCH_GENERIC_ARG(al, name, char*, dfsch_symbol)
#define DFSCH_SYMBOL_ARG_OPT(al, name, default)                 \
  DFSCH_GENERIC_ARG_OPT(al, name, default, char*, dfsch_symbol)
#define DFSCH_TYPE_ARG(al, name)                                        \
  DFSCH_GENERIC_ARG(al, name, dfsch_type_t*, dfsch_object_as_type)
#define DFSCH_TYPE_ARG_OPT(al, name, default)                           \
  DFSCH_GENERIC_ARG_OPT(al, name, default, dfsch_type_t*, dfsch_object_as_type)
#define DFSCH_PACKAGE_ARG(al, name)                                     \
  DFSCH_GENERIC_ARG(al, name, dfsch_package_t*, dfsch_package_designator)
#define DFSCH_PACKAGE_ARG_OPT(al, name, default)                \
  DFSCH_GENERIC_ARG_OPT(al, name, default, dfsch_package_t*,    \
                        dfsch_package_designator)

#ifdef __cplusplus
}
#endif


#endif
