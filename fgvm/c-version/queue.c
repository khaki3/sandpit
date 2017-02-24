#include <priv.h>

queue *queue_create() {
    queue *ret;

    ret = (queue *)malloc(sizeof(queue));
    ret->head = NULL;
    ret->tail = NULL;
    pthread_mutex_init(&(ret->mutex_push), NULL);
    pthread_mutex_init(&(ret->mutex_pop), NULL);
    return ret;
}

static queue_elem *queue_elem_create(void *data, queue_elem *next) {
    queue_elem *ret = (queue_elem *)malloc(sizeof(queue_elem));

    ret->data = data;
    ret->next = next;
    return ret;
}

void queue_free(queue *q) {
    queue_elem *head = q->head, *next;

    while (head != NULL) {
        next = head->next;
        free(head);
        head = next;
    }
    pthread_mutex_destroy(&(q->mutex_push));
    pthread_mutex_destroy(&(q->mutex_pop));
    free(q);
}

bool queue_is_empty(queue *q) {
    return (q->head == NULL);
}

void queue_push(queue *q, void *data) {
    queue_elem *elem = queue_elem_create(data, NULL);

    pthread_mutex_lock(&(q->mutex_push));
    if (queue_is_empty(q)) {
        pthread_mutex_lock(&(q->mutex_pop));
        q->head = elem;
        q->tail = elem;
        pthread_mutex_unlock(&(q->mutex_pop));
    }
    else {
        q->tail->next = elem;
        q->tail = elem;
    }
    pthread_mutex_unlock(&(q->mutex_push));
}

void queue_push_multiple(queue *q, void **mult) {
    void *data = *(mult++);
    queue_elem *elem;

    if (data == NULL)
        return;
    elem = queue_elem_create(data, NULL);

    pthread_mutex_lock(&(q->mutex_push));

    if (queue_is_empty(q)) {
        pthread_mutex_lock(&(q->mutex_pop));
        q->head = elem;
        q->tail = elem;
        pthread_mutex_unlock(&(q->mutex_pop));
    }
    else {
        q->tail->next = elem;
        q->tail = elem;
    }

    while ((data = *(mult++)) != NULL) {
        elem = queue_elem_create(data, NULL);
        q->tail->next = elem;
        q->tail = elem;
    }

    pthread_mutex_unlock(&(q->mutex_push));
}

void *queue_pop(queue *q) {
    void *ret;
    queue_elem *head;

    pthread_mutex_lock(&(q->mutex_pop));

    if (queue_is_empty(q)) {
        ret = NULL;
    }
    else {
        head = q->head;
        ret = head->data;
        q->head = head->next;
        free(head);
    }

    pthread_mutex_unlock(&(q->mutex_pop));

    return ret;
}
