#include <dfsch/lib/collections.h>

#include <limits.h>
#include <assert.h>

/*
 * Priority queues
 */

typedef struct fib_node_t fib_node_t;

struct fib_node_t {
  dfsch_object_t* object;
  fib_node_t* children;
  fib_node_t* next;
  int degree;
};

typedef struct pqueue_t {
  dfsch_type_t* type;
  fib_node_t* head;
  dfsch_object_t* lt;
} pqueue_t;

dfsch_type_t dfsch_collections_priority_queue_type = {
  DFSCH_STANDARD_TYPE,
  NULL,
  sizeof(pqueue_t),
  "collections:priority-queue",
};

/* TODO: effective implementation */

dfsch_object_t* dfsch_collections_make_priority_queue(dfsch_object_t* lt){
  pqueue_t* pq = dfsch_make_object(DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE);

  pq->head = NULL;
  pq->lt = lt;
  
  return (dfsch_object_t*)pq;
}
void dfsch_collections_priority_queue_push(dfsch_object_t* q,
                                           dfsch_object_t* o){
  pqueue_t* pq;
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  pq = (pqueue_t*)q;

  fib_node_t* n = GC_NEW(fib_node_t);

  n->object = o;
  n->degree = 0;
  n->children = NULL;

  if (!pq->head){
    pq->head = n;
  } else if (dfsch_apply(pq->lt, 
                         dfsch_list(2, n->object, pq->head->object))){
    n->next = pq->head;
    pq->head = n;    
  } else {
    n->next = pq->head->next;
    pq->head->next = n;
  }
}

#define MAX_DEGREE (sizeof(size_t) << 3)

dfsch_object_t* dfsch_collections_priority_queue_pop(dfsch_object_t* q){
  pqueue_t* pq;
  fib_node_t* n;
  dfsch_object_t* res;
  fib_node_t* degree_map[MAX_DEGREE];
  fib_node_t* i;
  size_t j;
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  pq = q;

  if (!pq->head){
    dfsch_error("Priority queue is empty", pq);
  }

  n = pq->head;
  res = n->object;

  pq->head = n->next;

  i = n->children;
  if (i){
    while (i->next){
      i = i->next;
    }
    i->next = pq->head;
    pq->head = n->children;
  } 

  memset(degree_map, 0, MAX_DEGREE*sizeof(fib_node_t*));

  i = pq->head;
  while (i){
    if (!degree_map[i->degree]){
      degree_map[i->degree] = i;
      i = i->next;
    } else {
      fib_node_t* pn = degree_map[i->degree];
      fib_node_t* cn = i;
      if (dfsch_apply(pq->lt,
                      dfsch_list(2, i->object, pn->object))){
        cn = pn;
        pn = i;
      }
      degree_map[i->degree] = NULL;
      pn->next = i->next;
      i = pn;

      cn->next = pn->children;
      pn->children = cn;
      pn->degree++;
    }
  }

  pq->head = NULL;
  for (j = 0; j < MAX_DEGREE; j++){
    if (degree_map[j]){
      pq->head = degree_map[j];
      pq->head->next = NULL;
      break;
    }
  }
  for (j++; j < MAX_DEGREE; j++){
    if (degree_map[j]){
      if (dfsch_apply(pq->lt,
                      dfsch_list(2, pq->head->object, degree_map[j]->object))){
        degree_map[j]->next = pq->head->next;
        pq->head->next = degree_map[j];
      } else {
        degree_map[j]->next = pq->head;
        pq->head = degree_map[j];      
      }
    }
  }

  return res;
}
int dfsch_collections_priority_queue_empty_p(dfsch_object_t* q){
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  return ((pqueue_t*)q)->head == NULL;
}

/*
 * bit vectors
 */

#define WORD_BITS (CHAR_BIT * sizeof(unsigned int))
typedef struct bitvector_t {
  dfsch_type_t* type;
  size_t length;
  size_t num_words;
  unsigned int words[];
} bitvector_t;

static void mask_unused_bits(bitvector_t* bv){
  bv->words[bv->num_words - 1] &= (1 << (bv->length % WORD_BITS)) - 1;
}

static dfsch_object_t* bv_get_iterator(bitvector_t* b){
  size_t i;
  dfsch_object_t* head;
  dfsch_object_t* tail;

  if (b->length == 0){
    return NULL;
  }
  
  head = tail = dfsch_cons(dfsch_bool(b->words[0] & 0x01 != 0), NULL);

  for (i = 1; i < b->length; i++){
    dfsch_object_t* tmp = dfsch_cons(dfsch_bool((b->words[i / WORD_BITS] 
                                                 & 1 << (i % WORD_BITS)) 
                                                != 0),
                                     NULL);
    DFSCH_FAST_CDR_MUT(tail) = tmp;
    tail = tmp;
  }

  return head;
}
static dfsch_object_t* bv_ref(bitvector_t* b, size_t n){
  if (b->length <= n){
    dfsch_error("Index out of range", dfsch_make_number_from_long(n));
  }

  return dfsch_bool(b->words[n / WORD_BITS] & 1 << (n % WORD_BITS)) != 0;
}
static void bv_set(bitvector_t* b, size_t n, dfsch_object_t* val){
  if (b->length <= n){
    dfsch_error("Index out of range", dfsch_make_number_from_long(n));
  }

  if (val){
    b->words[n / WORD_BITS] |= (1 << (n % WORD_BITS));  
  } else {
    b->words[n / WORD_BITS] &= ~(1 << (n % WORD_BITS));
  }
}
static size_t bv_length(bitvector_t* b){
  return b->length;
}

static dfsch_collection_methods_t bv_collection = {
  .get_iterator = bv_get_iterator
};

static dfsch_sequence_methods_t bv_sequence = {
  .ref = bv_ref,
  .set = bv_set,
  .length = bv_length,
};

dfsch_type_t dfsch_collections_bitvector_type = {
  .type = DFSCH_STANDARD_TYPE,
  .superclass = NULL,
  .name = "collections:bit-vector",
  .size = sizeof(bitvector_t),
  .collection = &bv_collection,
  .sequence = &bv_sequence,
};

static bitvector_t* alloc_bitvector(size_t length){
  size_t num_words = length / WORD_BITS;
  bitvector_t* bv;
  if (num_words % WORD_BITS != 0){
    num_words++;
  }
  
  bv = GC_MALLOC_ATOMIC(sizeof(bitvector_t) + num_words * sizeof(unsigned int));
    
  bv->type = DFSCH_COLLECTIONS_BITVECTOR_TYPE;
  bv->length = length;
  bv->num_words = num_words;
  return bv;
}


dfsch_object_t* dfsch_collections_make_bitvector(size_t length){
  bitvector_t* bv;
  bv = alloc_bitvector(length);

  memset(bv->words, 0, bv->num_words * sizeof(unsigned int));
  return (dfsch_object_t*)bv;
}
dfsch_object_t* dfsch_collections_list_2_bitvector(dfsch_object_t* values){
  size_t length;
  size_t j;
  dfsch_object_t* i;
  bitvector_t* bv;

  length = dfsch_list_length_check(values);
  bv = alloc_bitvector(length);
  i = values;

  for (j = 0; j < length && DFSCH_PAIR_P(i); j++, i = DFSCH_FAST_CDR(i)){
    if (DFSCH_FAST_CAR(i)){
      bv->words[j / WORD_BITS] |= (1 << (j % WORD_BITS));  
    } else {
      bv->words[j / WORD_BITS] &= ~(1 << (j % WORD_BITS));
    }    
  }

  return (dfsch_object_t*) bv;
}

dfsch_object_t* dfsch_collections_bitvector_not(dfsch_object_t* bv){
  bitvector_t* b = DFSCH_ASSERT_TYPE(bv, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* r = alloc_bitvector(b->length);
  size_t i;

  for (i = 0; i < b->num_words; i++){
    r->words[i] = ~b->words[i];
  }

  mask_unused_bits(r);

  return (dfsch_object_t*)r;
}
dfsch_object_t* dfsch_collections_bitvector_or(dfsch_object_t* bva,
                                               dfsch_object_t* bvb){
  bitvector_t* a = DFSCH_ASSERT_TYPE(bva, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* b = DFSCH_ASSERT_TYPE(bvb, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* r;
  int i;

  if (a->length < b->length){
    bitvector_t* tmp = a;
    a = b;
    b = tmp;
  }

  r = alloc_bitvector(a->length);
  
  for (i = 0; i < b->num_words; i++){
    r->words[i] = a->words[i] | b->words[i]; 
  }

  for (; i < a->num_words; i++){
    r->words[i] = a->words[i]; 
  }

  return (dfsch_object_t*)r;
}
dfsch_object_t* dfsch_collections_bitvector_and(dfsch_object_t* bva,
                                               dfsch_object_t* bvb){
  bitvector_t* a = DFSCH_ASSERT_TYPE(bva, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* b = DFSCH_ASSERT_TYPE(bvb, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* r;
  int i;

  if (a->length < b->length){
    bitvector_t* tmp = a;
    a = b;
    b = tmp;
  }

  r = alloc_bitvector(a->length);
  
  for (i = 0; i < b->num_words; i++){
    r->words[i] = a->words[i] | b->words[i]; 
  }

  for (; i < a->num_words; i++){
    r->words[i] = 0; 
  }

  return (dfsch_object_t*)r;
}
dfsch_object_t* dfsch_collections_bitvector_xor(dfsch_object_t* bva,
                                                dfsch_object_t* bvb){
  bitvector_t* a = DFSCH_ASSERT_TYPE(bva, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* b = DFSCH_ASSERT_TYPE(bvb, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* r;
  int i;

  if (a->length < b->length){
    bitvector_t* tmp = a;
    a = b;
    b = tmp;
  }

  r = alloc_bitvector(a->length);
  
  for (i = 0; i < b->num_words; i++){
    r->words[i] = a->words[i] ^ b->words[i]; 
  }

  for (; i < a->num_words; i++){
    r->words[i] = a->words[i]; 
  }

  return (dfsch_object_t*)r;
}
dfsch_strbuf_t* dfsch_collections_bitvector_2_bytes(dfsch_object_t* bv){
}
dfsch_object_t* dfsch_collections_bytes_2_bitvector(char* buf, size_t len,
                                                    size_t res_len){
}
dfsch_object_t* dfsch_collections_bitvector_increment(dfsch_object_t* bv){
  bitvector_t* b = DFSCH_ASSERT_TYPE(bv, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  bitvector_t* r = alloc_bitvector(b->length);
  size_t i;

  for (i = 0; i < b->num_words; i++){
    r->words[i] = b->words[i];
  }

  mask_unused_bits(r);

  for (i = 0; i < b->num_words; i++){
    unsigned int t = r->words[i];
    r->words[i]++;
    if (t < r->words[i]){
      break;
    }
  }

  mask_unused_bits(r);

  return (dfsch_object_t*)r;
}

int dfsch_collections_bitvector_all_zeros_p(dfsch_object_t* bv){
  bitvector_t* b = DFSCH_ASSERT_TYPE(bv, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  size_t i;

  for (i = 0; i < b->num_words; i++){
    if (b->words[i]){
      return 0;
    }
  }
  
  return 1;
}
int dfsch_collections_bitvector_all_ones_p(dfsch_object_t* bv){
  bitvector_t* b = DFSCH_ASSERT_TYPE(bv, DFSCH_COLLECTIONS_BITVECTOR_TYPE);
  size_t i;

  for (i = 0; i < (b->num_words - 1); i++){
    if (b->words[i] != UINT_MAX){
      return 0;
    }
  }
  
  if (b->words[b->num_words -1] != ((1 << (b->length % WORD_BITS)) - 1)){
    return 0;
  }

  return 1;
}
