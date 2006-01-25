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

/** @file
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

  /**
   * Interpreter context
   */
  typedef struct dfsch_ctx_t dfsch_ctx_t;

  /**
   * Meant for functions like caaddar, currently unused
   */
  typedef unsigned int dfsch_cXr_t;

  /**
   * C datatype for scheme objects
   */
  typedef struct dfsch_object_t dfsch_object_t;

  typedef struct dfsch_type_t {
    size_t size;
    char* name;
    int (*equal_p)(dfsch_object_t*, dfsch_object_t*);
    char* (*write)(dfsch_object_t*, int);
  } dfsch_type_t;

  /**
   * C datatype for scheme objects
   */
  struct dfsch_object_t {
    dfsch_type_t *type;
  };

  /**
   * Native functions prototype
   */
  typedef dfsch_object_t* (*dfsch_primitive_t)(void*,dfsch_object_t*);



#define DFSCH_CAR 0
#define DFSCH_CDR 0


  // object handling

  extern dfsch_object_t* dfsch_make_object(dfsch_type_t* type);


  extern int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b);
  extern int dfsch_eqv_p(dfsch_object_t *a, dfsch_object_t *b);
  extern int dfsch_eqaual_p(dfsch_object_t *a, dfsch_object_t *b);

  /**
   * Is A null?
   */
  extern int dfsch_object_null_p(dfsch_object_t* obj);

  /**
   * Is A a pair?
   */
  extern int dfsch_object_pair_p(dfsch_object_t* obj);

  /**
   * Is A an atom?
   */
  extern int dfsch_object_atom_p(dfsch_object_t* obj); // i.e. not pair

  /**
   * Is A a symbol?
   */
  extern int dfsch_object_symbol_p(dfsch_object_t* obj);

  /**
   * Is A a a number?
   */
  extern int dfsch_object_number_p(dfsch_object_t* obj);

  /**
   * Is A a string?
   */
  extern int dfsch_object_string_p(dfsch_object_t* obj);

  /**
   * Is A a primitive (native) function?
   */
  extern int dfsch_object_primitive_p(dfsch_object_t* obj);

  /**
   * Is A a lambda-closure??
   */
  extern int dfsch_object_closure_p(dfsch_object_t* obj);

  /**
   * Is A an applicable procedure?
   */
  extern int dfsch_object_procedure_p(dfsch_object_t* obj);

  /**
   * Is A an macro?
   */
  extern int dfsch_object_macro_p(dfsch_object_t* obj);

  /**
   * Is A an exception?
   */
  extern int dfsch_object_exception_p(dfsch_object_t* obj);

  /**
   * Is A an exception?
   */
  extern int dfsch_object_vector_p(dfsch_object_t* obj);

  /**
   * Is A a native data pointer?
   */
  extern int dfsch_object_native_p(dfsch_object_t* obj);



  /**
   * Parse ASCIIZ string into object
   */
  extern dfsch_object_t* dfsch_obj_read(char* str);

  /**
   * Parse ASCIIZ string into list of objects
   */
  extern dfsch_object_t* dfsch_list_read(char* str);


#define dfsch_number dfsch_number_to_double
#define dfsch_make_number dfsch_make_number_from_double
#include <dfsch/number.h>


  /**
   * Convert object to ASCIIZ string
   */
  extern char* dfsch_obj_write(dfsch_object_t* obj, int max_depth);
  /**
   * Convert object to ASCIIZ string
   */
  extern char* dfsch_exception_write(dfsch_object_t* e);

  // NIL

  /**
   * <code>'()</code>
   */
  extern dfsch_object_t* dfsch_nil();

  // pairs

  /**
   * <code>(cons CAR CDR)</code>
   */
  extern dfsch_object_t* dfsch_cons(dfsch_object_t* car, dfsch_object_t* cdr);

  /**
   * <code>(car PAIR)</code>
   */
  extern dfsch_object_t* dfsch_car(dfsch_object_t* pair);

  /**
   * <code>(cdr PAIR)</code>
   */
  extern dfsch_object_t* dfsch_cdr(dfsch_object_t* pair);

  /**
   * Unimplemented
   */
  extern dfsch_object_t* dfsch_cXr(dfsch_object_t* pair, dfsch_cXr_t x);

  /**
   * <code>(set-car! PAIR C)</code>
   */
  extern dfsch_object_t* dfsch_set_car(dfsch_object_t* pair,
				       dfsch_object_t* c);

  /**
   * <code>(set-cdr! PAIR C)</code>
   */
  extern dfsch_object_t* dfsch_set_cdr(dfsch_object_t* pair,
				       dfsch_object_t* c);


  /**
   * Return number of items in given list
   */
  extern int dfsch_list_length(dfsch_object_t* list);

  /**
   * Returns given item of list.
   */
  extern dfsch_object_t* dfsch_list_item(dfsch_object_t* list, int index);

  /**
   * <code>(append . LLIST)</code> 
   */
  extern dfsch_object_t* dfsch_append(dfsch_object_t* llist);

  /**
   * Construct list of count items from arguments.
   */

  extern dfsch_object_t* dfsch_list(size_t count, ...);

  /**
   * Allocates new list with same contents as given list.
   */
  extern dfsch_object_t* dfsch_list_copy(dfsch_object_t* list);

  // alists
  /**
   * <code>(assoc KEY ALIST)</code>
   */
  extern dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
				     dfsch_object_t *alist);


  /**
   * Perform quasi-quote (i.e. something like formating S-expressions)
   * See book on scheme for futher explaination.
   */
  extern dfsch_object_t* dfsch_quasiquote(dfsch_object_t* env, 
                                          dfsch_object_t* arg);

  // string

  /**
   * Makes string object from corresponding ASCIIZ string.
   */
  extern dfsch_object_t* dfsch_make_string(char* symbol);

  /**
   * Returns ASCIIZ string for given string object.
   */
  extern char* dfsch_string(dfsch_object_t* symbol);


  // symbols

  /**
   * Makes symbol object from corresponding ASCIIZ string.
   */
  extern dfsch_object_t* dfsch_make_symbol(char* symbol);

  /**
   * Returns string representation of given symbol.
   */
  extern char* dfsch_symbol(dfsch_object_t* symbol);

  /**
   * Performance hack: returns symbol <code>true</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_true();

  /**
   * Return true or nil depending on value of BOOL.
   */
  extern dfsch_object_t* dfsch_bool(int bool);

  /**
   * Performance hack: returns symbol <code>quote</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_quote();
  /**
   * Performance hack: returns symbol <code>quasiquote</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_quasiquote();
  /**
   * Performance hack: returns symbol <code>unquote</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_unquote();
  /**
   * Performance hack: returns symbol <code>unquote-splicing</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_unquote_splicing();
  /**
   * Performance hack: returns symbol <code>else</code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_else();
  /**
   * Performance hack: returns symbol <code>=></code> 
   * witout need for looking it up every time.
   */
  extern dfsch_object_t* dfsch_sym_bold_right_arrow();

  // closures

  /**
   * Creates new lambda closure bound to environment ENV,
   * with arguments ARGS and body containing CODE.
   */
  extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				      dfsch_object_t* args,
				      dfsch_object_t* code);

  /**
   * Creates new lambda closure bound to environment ENV,
   * with arguments ARGS and body containing CODE. Tagged
   * with NAME. (this value will be printed in tracebacks
   * on exception)
   */
  extern dfsch_object_t* dfsch_named_lambda(dfsch_object_t* env,
                                            dfsch_object_t* args,
                                            dfsch_object_t* code,
                                            dfsch_object_t* name);


  // native code

  /**
   * Makes primitive (native) procedure from pointer to implementing
   * function.
   */
  extern dfsch_object_t* dfsch_make_primitive(dfsch_primitive_t prim,
					      void *baton);


  // vectors

  /**
   * Creates vector of given length
   */
  extern dfsch_object_t* dfsch_make_vector(size_t length, 
                                           dfsch_object_t *fill);

  /**
   * Construct vector of count items from arguments.
   */

  extern dfsch_object_t* dfsch_vector(size_t count, ...);

  /**
   * Returns length of given vector
   */
  extern size_t dfsch_vector_length(dfsch_object_t *vector);

  /**
   * Returns contents of k-th slot of vector.
   */
  extern dfsch_object_t* dfsch_vector_ref(dfsch_object_t *vector, size_t k);

  /**
   * Sets value of k-th slot of vector to obj.
   */
  extern dfsch_object_t* dfsch_vector_set(dfsch_object_t* vector, size_t k, 
                                          dfsch_object_t* obj);

  /**
   * Converts vector into list.
   */
  extern dfsch_object_t* dfsch_vector_2_list(dfsch_object_t* vector);

  /**
   * Converts list into vector.
   */
  extern dfsch_object_t* dfsch_list_2_vector(dfsch_object_t* list);

  /**
   * Makes native object representing given pointer to native data with given
   * type.
   */
  extern dfsch_object_t* dfsch_make_native_data(void *data, 
						dfsch_object_t *type);

  /**
   * Returns pointer to arbitrary native data represented by given object.
   */
  extern void* dfsch_native_data(dfsch_object_t *object, dfsch_object_t *type);

  /**
   * Returns object uniquely identifing data-type of native object.
   */
  extern dfsch_object_t* dfsch_native_data_type(dfsch_object_t *object);


  // macros

  /**
   * Wraps procedure for use as macro.
   */
  extern dfsch_object_t* dfsch_make_macro(dfsch_object_t *proc);
  /**
   * Wraps procedure for use as special form.
   */
  extern dfsch_object_t* dfsch_make_form(dfsch_object_t *proc);


  // error handling

  /**
   * Makes exception symbol of given TYPE and DATA.
   */
  extern dfsch_object_t* dfsch_make_exception(dfsch_object_t* type, 
					      dfsch_object_t* data);
  
  /**
   * Raises an exception (exception could be any object)
   */

  extern void dfsch_raise(dfsch_object_t* exception);

  /**
   * Executes THUNK and in case of RAISE aborts it's execution and executes
   * handler with RAISE's argument.
   *
   * @arg thunk Procedure of zero arguments
   * @arg handler Procedure of one argument
   */
  extern dfsch_object_t* dfsch_try(dfsch_object_t* handler,
                                   dfsch_object_t* thunk);
  

  /**
   * Convenience wrapper.
   */
  extern dfsch_object_t* dfsch_throw(char* type, 
                                     dfsch_object_t* data);


  /**
   * And another convenience wrapper around dfsch_make_exeption()
   */
#define DFSCH_THROW(type,data) dfsch_throw(type, data)

  /**
   * If argument is exception, add current function into call trace and
   * return from current function.
   */
#define DFSCH_RETHROW(exception)  -- DEPRECATED --
              


  /**
   * Return exception's type.
   */
  extern dfsch_object_t* dfsch_exception_type(dfsch_object_t* e);

  /**
   * Return data associated with given exception.
   */
  extern dfsch_object_t* dfsch_exception_data(dfsch_object_t* e);



  // Lexical binding:
  /**
   * Create new environment frame.
   */
  extern dfsch_object_t* dfsch_new_frame(dfsch_object_t* parent);
  /**
   * Get value of variable name in environment env.
   */
  extern dfsch_object_t* dfsch_lookup(dfsch_object_t* name, 
				      dfsch_object_t* env);

  /**
   * Set value of variable name in environment env to value.
   */
  extern dfsch_object_t* dfsch_set(dfsch_object_t* name,
				   dfsch_object_t* value,
				   dfsch_object_t* env);

  /**
   * Define variable name in environment env with initial value of value 
   */
  extern dfsch_object_t* dfsch_define(dfsch_object_t* name,
				      dfsch_object_t* value,
				      dfsch_object_t* env);


  // EVAL+APPLY

  /**
   * Evaluates EXP in given binding environment ENV.
   */
  extern dfsch_object_t* dfsch_eval(dfsch_object_t* exp, dfsch_object_t* env);
  extern dfsch_object_t* dfsch_eval_proc(dfsch_object_t* exp, 
                                         dfsch_object_t* env);
  /**
   * Applyes procedure PROC to arguments ARGS. Obviously it doesn't work for 
   * macros.
   */
  extern dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args);

  // context

  /**
   * Allocates new context (i.e. top level environment frame wrapped into struct)
   */
  extern dfsch_ctx_t* dfsch_make_context();
  /**
   * Evaluates given expression EXP in global environment of given context CTX.
   */
  extern dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp);
  /**
   * Evaluates list of expressions in given context. Useful for evauating 
   * contents of files and other things.
   */
  extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					     dfsch_object_t* list);
  /**
   * Defines new variable NAME with value OBJ in global environment 
   * of context CTX.
   */
  extern dfsch_object_t* dfsch_ctx_define(dfsch_ctx_t *ctx, 
                                          char *name, 
                                          dfsch_object_t *obj);


  /**
   * Creates new closure with global environment of given context.
   */
  extern dfsch_object_t* dfsch_ctx_lambda(dfsch_ctx_t *ctx,
                                          dfsch_object_t* args,
                                          dfsch_object_t* code);
  /**
   * Looks up value of variable given by NAME.
   */
  extern dfsch_object_t* dfsch_ctx_lookup(dfsch_ctx_t *ctx, char *name);

  /**
   * Returns global environment associated with given context.
   */
  extern dfsch_object_t* dfsch_ctx_environment(dfsch_ctx_t *ctx);

  /**
   * Iterator for dfsch_get_next_symbol
   */
  typedef struct dfsch_symbol_iter_t dfsch_symbol_iter_t;

  /**
   * Iterates over all symbols known by interpreter. And returns one 
   * per call, useful for things like completion. ITER is pointer to 
   * dfsch_symbol_iter_t*, when its value is NULL, new iterator is 
   * allocated. NULL is returned, when no more symbols are avaiable.
   */

  extern char* dfsch_get_next_symbol(dfsch_symbol_iter_t **iter);


#define DFSCH_OBJECT_ARG(al, name)\
  if (!dfsch_pair_p((al))) \
    DFSCH_THROW("exception:required-argument-missing",\
                dfsch_make_string(#name));\
  (name) = dfsch_car((al)); \
  (al) = dfsch_cdr((al))

#define DFSCH_STRING_ARG(al, name)\
  if (!dfsch_pair_p((al))) \
    DFSCH_THROW("exception:required-argument-missing",\
                dfsch_make_string(#name));\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    if (!dfsch_string_p(tmp)) \
      DFSCH_THROW("exception:not-a-string",tmp);	\
    (name) = dfsch_string(tmp); \
    (al) = dfsch_cdr((al));\
  }

#define DFSCH_NUMBER_ARG(al, name, type)\
  if (!dfsch_pair_p((al))) \
    DFSCH_THROW("exception:required-argument-missing",\
                dfsch_make_string(#name));\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    if (!dfsch_number_p(tmp)) \
        DFSCH_THROW("exception:not-a-number",tmp); \
    (name) = (type)dfsch_number(tmp); \
    (al) = dfsch_cdr((al));\
  }

#define DFSCH_OBJECT_ARG_OPT(al, name,default)\
  if (!dfsch_pair_p((al))) \
   { (name) = (default);}else\
  {(name) = dfsch_car((al)); \
  (al) = dfsch_cdr((al));}

#define DFSCH_STRING_ARG_OPT(al, name, default)\
  if (!dfsch_pair_p((al))) \
    {(name)=(default);} else\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    if (!dfsch_string_p(tmp)) \
      DFSCH_THROW("exception:not-a-string",tmp);	\
    (name) = dfsch_string(tmp); \
    (al) = dfsch_cdr((al));\
  }

#define DFSCH_NUMBER_ARG_OPT(al, name, type, default)\
  if (!dfsch_pair_p((al))) \
  {(name) = (default);} else\
  { dfsch_object_t* tmp = dfsch_car((al)); \
    if (!dfsch_number_p(tmp)) \
      DFSCH_THROW("exception:not-a-number",tmp);	\
    (name) = (type)dfsch_number(tmp); \
    (al) = dfsch_cdr((al));\
  }

#define DFSCH_ARG_END(al) \
  if (al != NULL) \
    DFSCH_THROW("exception:too-many-arguments",NULL)

#ifdef __cplusplus
}
#endif


#endif
