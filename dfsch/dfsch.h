/*
 * dfsch - dfox's quick and dirty scheme implementation
 * Copyright (C) 2005 Ales Hakl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*
 * It's not really scheme but something slightly similar, features left out 
 * include:
 * - first class continuations
 * - I/O (left out on purpose)
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


#ifdef __cplusplus
extern "C" {
#endif

  /** Continuation used for tail-call elimination. */
  typedef struct dfsch_tail_escape_t dfsch_tail_escape_t;

  /** Meant for functions like caaddar, currently unused */
  typedef unsigned int dfsch_cXr_t;

  /** C datatype for scheme objects */
  typedef struct dfsch_object_t dfsch_object_t;

  /** Equivalence metod prototype */
  typedef int (*dfsch_type_equal_p_t)(dfsch_object_t*, dfsch_object_t*);
  /** Write / Display method prototype */
  typedef char* (*dfsch_type_write_t)(dfsch_object_t* obj, int depth, 
                                      int readable);
  /** Apply metod prototype */
  typedef dfsch_object_t* (*dfsch_type_apply_t)(dfsch_object_t* object, 
                                                dfsch_object_t* args,
                                                dfsch_tail_escape_t* esc);

  /** Representation of scheme datatypes. */
  typedef struct dfsch_type_t {
    /** Instance size */
    size_t size;
    /** Type name */
    char* name;
    /** Equal method - called with two instances of this type */
    dfsch_type_equal_p_t equal_p;
    /** 
     * Should return external representation of given object. In most cases
     * something like "#&gt;my-object bla bla bla&lt;"
     */
    dfsch_type_write_t write;
    /** 
     * Apply method - called when object of this type is applyed to 
     * something. Beware - primitives and closures are handled directly
     * in evaluator and have this field set to NULL
     */
    dfsch_type_apply_t apply;
  } dfsch_type_t;

  /**
   * C datatype for scheme objects. Used as abstract datatype and also 
   * first field of any non-generic object.
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
  typedef dfsch_object_t* (*dfsch_primitive_t)(void* baton,
					       dfsch_object_t* args,
                                               dfsch_tail_escape_t* esc);


#define DFSCH_CAR 0
#define DFSCH_CDR 0

  /** Create object of given type. */
  extern dfsch_object_t* dfsch_make_object(const dfsch_type_t* type);


  /** Same object? (i.e. equal addresses) */
  extern int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b);
  /** Same object or number? */
  extern int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b);
  /** Equal object? (i.e. equal contents) */
  extern int dfsch_eqaual_p(dfsch_object_t *a, dfsch_object_t *b);

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
  extern dfsch_object_t* dfsch_obj_read(char* str);
  /** Parse string into list of objects */
  extern dfsch_object_t* dfsch_list_read(char* str);
  /** Convert object to string */
  extern char* dfsch_obj_write(dfsch_object_t* obj, int max_depth, 
                               int readable);
  /** Convert exception object to descriptive message */
  extern char* dfsch_exception_write(dfsch_object_t* e);

  /** Returns empty list, equivalent to NULL */
  extern dfsch_object_t* dfsch_nil();

  /** Construct pair object */
  extern dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr);

  /** Return first (car) item of pair */
  extern dfsch_object_t* dfsch_car(dfsch_object_t* pair);
  /** Return second (cdr) item of pair */
  extern dfsch_object_t* dfsch_cdr(dfsch_object_t* pair);
  /** Unimplemented */
  extern dfsch_object_t* dfsch_cXr(dfsch_object_t* pair, dfsch_cXr_t x);

  /** Set first (car) item of pair */
  extern dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
				       dfsch_object_t* c);
  /** Set second (cdr) item of pair */
  extern dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
				       dfsch_object_t* c);

  /** Returns number of items in given list or -1 for infinite lists */
  extern long dfsch_list_length(dfsch_object_t* list);
  /** Returns number of items in given finite list */
  extern long dfsch_list_length_fast(dfsch_object_t* list);
  /** Returns number of items in given finite list, fail otherwise */
  extern long dfsch_list_length_check(dfsch_object_t* list);
  /** Returns given item of list. */
  extern dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index);

  /** Concatenate lists */
  extern dfsch_object_t* dfsch_append(dfsch_object_t* llist);
  /** Construct list from arguments */
  extern dfsch_object_t* dfsch_list(size_t count, ...);
  /** Copy list. */
  extern dfsch_object_t* dfsch_list_copy(dfsch_object_t* list);
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


  /** Expand quasi-quoted expression */
  extern dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, 
                                          dfsch_object_t* arg);

  /** Makes symbol object from string. */
  extern dfsch_object_t* dfsch_make_symbol(char* symbol);

  /** Dissociates symbol object from its name. */
  extern void dfsch_unintern(dfsch_object_t* symbol);

  /** Returns unique generated symbol. */
  extern dfsch_object_t* dfsch_gensym();

  /** Returns string representation of given symbol. */
  extern char* dfsch_symbol(dfsch_object_t* symbol);

  /** Returns symbol "true" */
  extern dfsch_object_t* dfsch_sym_true();

  /** Return true or nil depending on value of BOOL. */
  extern dfsch_object_t* dfsch_bool(int bool);

  /** Returns symbol "'" */
  extern dfsch_object_t* dfsch_sym_quote();
  /** Returns symbol "`" */
  extern dfsch_object_t* dfsch_sym_quasiquote();
  /** Returns symbol ","*/
  extern dfsch_object_t* dfsch_sym_unquote();
  /** Returns symbol ",@"*/
  extern dfsch_object_t* dfsch_sym_unquote_splicing();
  /** Returns symbol "else"*/
  extern dfsch_object_t* dfsch_sym_else();
  /** Returns symbol "=>"*/
  extern dfsch_object_t* dfsch_sym_bold_right_arrow();


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
  extern dfsch_object_t* dfsch_make_primitive(dfsch_primitive_t prim,
					      void *baton);


  // vectors

  /** Create vector */
  extern dfsch_object_t* dfsch_make_vector(size_t length, 
                                           dfsch_object_t *fill);
  /** Construct vector from arguments. */
  extern dfsch_object_t* dfsch_vector(size_t count, ...);
  /** Returns length of given vector. */
  extern size_t dfsch_vector_length(dfsch_object_t *vector);
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
  extern dfsch_object_t* dfsch_make_form(dfsch_object_t *proc);


  // error handling

  /** Creates exception object */
  extern dfsch_object_t* dfsch_make_exception(dfsch_object_t* type, 
					      dfsch_object_t* data,
                                              dfsch_object_t* stack_trace);
  
  /** Raises an exception (exception could be any object) */

  extern void dfsch_raise(dfsch_object_t* exception);

  /** Catches exceptions */
  extern dfsch_object_t* dfsch_try(dfsch_object_t* handler,
                                   dfsch_object_t* thunk);
  

  /** Convenience wrapper for throwing an exception from C code */
  extern dfsch_object_t* dfsch_throw(char* type, 
                                     dfsch_object_t* data);

  /** Set break flag (call this when throwing exception isn't safe) */
  extern dfsch_object_t* dfsch_break(char* type);

  /** Return exception's type. */
  extern dfsch_object_t* dfsch_exception_type(dfsch_object_t* e);

  /** Return data associated with given exception. */
  extern dfsch_object_t* dfsch_exception_data(dfsch_object_t* e);

  /** Call given function with escape continuation as sole argument. */
  extern dfsch_object_t* dfsch_call_ec(dfsch_object_t* proc);

  // Lexical binding:
  /** Create new environment frame. */
  extern dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent);
  /** Get value of variable name in environment env. */
  extern dfsch_object_t* dfsch_lookup(dfsch_object_t* name, 
				      dfsch_object_t* env);
  /** Look up value of given variable, returns list of one item if sucessful,
      empty list if not*/
  extern dfsch_object_t* dfsch_env_get(dfsch_object_t* name, 
                                       dfsch_object_t* env);

  /**
   * Get value of variable name in environment env. Return empty list
   * in case of failure.
   */
  extern dfsch_object_t* dfsch_env_get(dfsch_object_t* name, 
                                       dfsch_object_t* env);

  /** Set value of variable name in environment env to value. */
  extern dfsch_object_t* dfsch_set(dfsch_object_t* name,
				   dfsch_object_t* value,
				   dfsch_object_t* env);

  /** Define variable name in environment env */
  extern dfsch_object_t* dfsch_define(dfsch_object_t* name,
				      dfsch_object_t* value,
				      dfsch_object_t* env);


  // EVAL+APPLY

  /** Evaluate expression. */
  extern dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env);
  /** Evaluate multiple expressions returning value of final one. */
  extern dfsch_object_t* dfsch_eval_proc(dfsch_object_t* exp, 
                                         dfsch_object_t* env);
  /** Apply procedure to given arguments*/
  extern dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args);
  /** Extended variant of dfsch_eval_proc with support for tail recursion */
  extern dfsch_object_t* dfsch_eval_proc_tr(dfsch_object_t* code, 
                                            dfsch_object_t* env,
                                            dfsch_object_t* call_signature,
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

  /** Allocates new top-level context (environment). */
  extern dfsch_object_t* dfsch_make_context();
  /** Define new variable in given context */
  extern dfsch_object_t* dfsch_define_cstr(dfsch_object_t *ctx, 
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

  
  /** Iterator for dfsch_get_next_symbol */
  typedef struct dfsch_symbol_iter_t dfsch_symbol_iter_t;

  /**
   * Iterates over all symbols known by interpreter. And returns one 
   * per call, useful for things like completion. ITER is pointer to 
   * dfsch_symbol_iter_t*, when its value is NULL, new iterator is 
   * allocated. NULL is returned, when no more symbols are avaiable.
   */
  extern char* dfsch_get_next_symbol(dfsch_symbol_iter_t **iter);

#include <dfsch/strings.h>


  /**
   * Parses one argument of no specific type from argument list and assigns it
   * to given variable (or l-value)
   *
   * @param al Argument list
   * @param name Variable or l-value (also used as argument name in exceptions)
   */
#define DFSCH_OBJECT_ARG(al, name)\
  if (!dfsch_pair_p((al))) \
    dfsch_throw("exception:required-argument-missing",\
                dfsch_make_string_cstr(#name));\
  (name) = dfsch_car((al)); \
  (al) = dfsch_cdr((al))

  /**
   * Parses one argument of no specific type from argument list and discards it
   *
   * @param al Argument list
   * @param name Argument name (used only in exceptions)
   */
#define DFSCH_DISCARD_ARG(al, name)\
  if (!dfsch_pair_p((al))) \
    dfsch_throw("exception:required-argument-missing",\
                dfsch_make_string_cstr(#name));\
  (al) = dfsch_cdr((al))

  /**
   * Parses one argument of no specific type from argument list and assigns it
   * to given variable (or l-value). Uses default value instead of throwing 
   * exception when there are no arguments left.
   *
   * @param al Argument list
   * @param name Variable or l-value.
   * @param default Default value 
   */
#define DFSCH_OBJECT_ARG_OPT(al, name,default)\
  if (!dfsch_pair_p((al))) \
   { (name) = (default);}else\
  {(name) = dfsch_car((al)); \
  (al) = dfsch_cdr((al));}

  /**
   * Parses one argument from arguments list and converts it using given 
   * function. Intended for use in other macros, such as DFSCH_STRING_ARG.
   *
   * @param al Argument list
   * @param name Variable or l-value.
   * @param type C type of result
   * @param conv Function for conversion from dfsch_object_t* to given type.
   */
#define DFSCH_GENERIC_ARG(al, name, type, conv)\
  if (!dfsch_pair_p((al))) \
    dfsch_throw("exception:required-argument-missing",\
                dfsch_make_string_cstr(#name));\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    (name) = (type)(conv)(tmp); \
    (al) = dfsch_cdr((al));\
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
#define DFSCH_GENERIC_ARG_OPT(al, name, default, type, conv)\
  if (!dfsch_pair_p((al))) \
    {(name)=(default);} else\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    (name) = (type)(conv)(tmp); \
    (al) = dfsch_cdr((al));\
  }

  /**
   * Throws exception if arguments list contain any arguments.
   *
   * @param al Argument list
   */
#define DFSCH_ARG_END(al) \
  if (al != NULL) \
    dfsch_throw("exception:too-many-arguments",NULL)

#define DFSCH_SYMBOL_CACHE(symbol, name)\
  dfsch_object_t* name(){\
    static cache = NULL;\
    if (!cache)\
      cache = dfsch_make_symbol(symbol);\
    return cache;\
  }

#define DFSCH_LOCAL_SYMBOL_CACHE(symbol, name)\
  static DFSCH_SYMBOL_CACHE(symbol, name)

#ifdef __cplusplus
}
#endif


#endif
