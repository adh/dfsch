#include <dfsch/lib/collections.h>

typedef struct pq_entry_t pq_entry_t;

struct pq_entry_t {
  dfsch_object_t* object;
  pq_entry_t* next;
};

typedef struct pqueue_t {
  dfsch_type_t* type;
  pq_entry_t* head;
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
  pq_entry_t* e;
  pq_entry_t* i;
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  pq = (pqueue_t*)q;

  e = GC_NEW(pq_entry_t);
  e->object = o;

  if (!pq->head){
    pq->head = e;
  } else if (dfsch_apply(pq->lt, dfsch_list(2,
                                            e->object,
                                            pq->head->object)) != NULL){
    e->next = pq->head;
    pq->head = e;
  } else {
    i = pq->head;
    while (i->next){
      if (dfsch_apply(pq->lt, dfsch_list(2, 
                                         e->object,
                                         i->next->object))){
        e->next = i->next;
        break;
      }
    }
    i->next = e;
  }
}
dfsch_object_t* dfsch_collections_priority_queue_pop(dfsch_object_t* q){
  pqueue_t* pq;
  pq_entry_t* e;
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  pq = q;
  if (!pq->head){
    dfsch_error("Priority queue is empty", q);
  }
  e = pq->head;
  pq->head = e->next;
  return e->object;
}
int dfsch_collections_priority_queue_empty_p(dfsch_object_t* q){
  if (!DFSCH_INSTANCE_P(q, DFSCH_COLLECTIONS_PRIORITY_QUEUE_TYPE)){
    dfsch_error("Not a priority queue", q);
  }
  return ((pqueue_t*)q)->head == NULL;
}
