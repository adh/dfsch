/*
 * dfsch - DFox's quick and dirty scheme implementation
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
 * - Tail recursion
 * - Continuations
 * - I/O (left out on purpose)
 * - and maybe something other 
 */

/** @file
 *
 * dfsch is quick and dirty implementation of someting that resembles 
 * scheme. This file contains interfacespecification.
 *
 */


#ifndef H__dfsch__
#define H__dfsch__

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Interpreter context
 */
typedef struct dfsch_ctx_t dfsch_ctx_t;

/**
 * C datatype for scheme objects
 */
typedef struct dfsch_object_t dfsch_object_t;

/**
 * Meant for functions like caaddar, currently unused
 */
typedef unsigned int dfsch_cXr_t;

/**
 * Native functions prototype
 */
typedef dfsch_object_t* (*dfsch_primitive_t)(dfsch_object_t*);

#define DFSCH_CAR 0
#define DFSCH_CDR 0


// object handling

/**
 * Perform garbage collection
 */
extern int dfsch_gc();

/**
 * Mark object as referenced from C code.
 */
extern void dfsch_object_ref(dfsch_object_t* obj);
/**
 * Unregister referenced object.
 */
extern void dfsch_object_unref(dfsch_object_t* obj);

/**
 * Are A and B equal objects?
 */
extern int dfsch_eq_p(dfsch_object_t *a, dfsch_object_t *b);

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
 * Is A a anumber?
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
 * Parse ASCIIZ string into object
 */
extern dfsch_object_t* dfsch_obj_read(char* str);

/**
 * Parse ASCIIZ string into list of objects
 */
extern dfsch_object_t* dfsch_list_read(char* str);


/**
 * Convert object to ASCIIZ string
 */
extern char* dfsch_obj_write(dfsch_object_t* obj, int max_depth);

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

// alists
/**
 * <code>(assoc KEY ALIST)</code>
 */
extern dfsch_object_t* dfsch_assoc(dfsch_object_t *key,
				   dfsch_object_t *alist);

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
extern dfsch_object_t* dfsch_true();

/**
 * Performance hack: returns symbol <code>quote</code> 
 * witout need for looking it up every time.
 */
extern dfsch_object_t* dfsch_quote();

// numbers

/**
 * Makes number object from given floating-point number.
 */
extern dfsch_object_t* dfsch_make_number(double n);
/**
 * Returns native representation of given number object;
 */
extern float dfsch_number(dfsch_object_t *n);

// closures


extern dfsch_object_t* dfsch_lambda(dfsch_object_t* env,
				    dfsch_object_t* args,
				    dfsch_object_t* code);


// native code

/**
 * Makes primitive (native) procedure from pointer to implementing
 * function.
 */
extern dfsch_object_t* dfsch_make_primitive(dfsch_primitive_t prim);

// macros

/**
 * Wraps procedure for use as macro.
 */
extern dfsch_object_t* dfsch_make_macro(dfsch_object_t *proc);


// error handling

/**
 * Makes exception symbol of given TYPE and DATA.
 */
extern dfsch_object_t* dfsch_make_exception(dfsch_object_t* type, 
					    dfsch_object_t* data);

// Lexical binding:
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
/**
 * Applyes procedure PROC to arguments ARGS. Obviously it doesn't work for 
 * macros.
 */
extern dfsch_object_t* dfsch_apply(dfsch_object_t* proc, dfsch_object_t* args);

// context

/**
 * Allocates new context and adds its top-level environment into list
 * of objects tat sould not be dealocated by garbage collector.
 */
extern dfsch_ctx_t* dfsch_make_context();
/**
 * Evaluates given expression EXP in global environment of given context CTX.
 */
extern dfsch_object_t* dfsch_ctx_eval(dfsch_ctx_t* ctx, dfsch_object_t* exp);
/**
 * Evaluates list of expressions in given context. Useful for evauating 
 * contents of files andother things.
 */
extern dfsch_object_t* dfsch_ctx_eval_list(dfsch_ctx_t* ctx, 
					   dfsch_object_t* list);
/**
 * Unregisters global environment of given context from list of 
 * non-deallocatable objects, runs garbage collector and dealocates
 * memory taken by given context CTX.
 */
extern void dfsch_destroy_context(dfsch_ctx_t* ctx);
/**
 * Defines new variable NAME with value OBJ in global environment 
 * of context CTX.
 */
extern void dfsch_ctx_define(dfsch_ctx_t *ctx, 
			     char *name, 
			     dfsch_object_t *obj);

/**
 * Returns global environment associated with given context.
 */
extern dfsch_object_t* dfsch_ctx_environment(dfsch_ctx_t *ctx);

#ifdef __cplusplus
}
#endif


#endif
