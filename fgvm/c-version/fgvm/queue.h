#ifndef QUEUE_H
#define QUEUE_H

typedef struct queue_elem_rec {
    void *data;
    struct queue_elem_rec *next;
} queue_elem;

typedef struct {
    queue_elem *head;
    queue_elem *tail;
    pthread_mutex_t mutex_push;
    pthread_mutex_t mutex_pop;
} queue;

queue *queue_create();
void queue_free(queue *);
bool queue_is_empty(queue *);
void queue_push(queue *, void *);
void queue_push_multiple(queue *, void **);
void *queue_pop(queue *);

#endif
