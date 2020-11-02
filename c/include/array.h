#ifndef POLYEME_ARRAY_H
#define POLYEME_ARRAY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* TODO: Documentation */
typedef struct {
  size_t length;
  size_t size;
  size_t elem_size;
} po_array_impl_t;

/* TODO: Documentation */
#define po_array_t(t)                                                          \
  struct {                                                                     \
    po_array_impl_t impl;                                                      \
    t *data;                                                                   \
  }

typedef po_array_t(void) po_array_void_t;

/* TODO: Documentation */
po_array_impl_t po_array_impl_init(size_t size, size_t elem_size);

/* TODO: Documentation */
void po_array_impl_remove(po_array_void_t *arr, size_t at);

/* TODO: Documentation */
void po_array_impl_insert(po_array_void_t *arr, size_t at, void *elem);

/* TODO: Documentation */
po_array_void_t po_array_impl_slice(po_array_void_t *arr, size_t i, size_t j);

/* TODO: Documentation */
void po_array_impl_resize(po_array_void_t *arr, size_t ns);

/* TODO: Documentation */
#define po_array_init(c, t)                                                    \
  {                                                                            \
    .impl = po_array_impl_init(c, sizeof(t)),                                  \
    .data = (t *)malloc(c * sizeof(t))                                         \
  }

/* TODO: Documentation */
#define po_array_deinit(arr)                                                   \
  do {                                                                         \
    free(arr.data);                                                            \
  } while (false)

/* TODO: Documentation */
#define po_array_length(arr) (arr.impl.length)

/* TODO: Documentation */
#define po_array_size(arr) (arr.impl.size)

/* TODO: Documentation */
#define po_array_get(arr, i) (arr.data[i])

/* TODO: Documentation */
#define po_array_grow(arr)                                                     \
  do {                                                                         \
    po_array_resize(arr, arr.impl.size < 10 ? 10 : 2 * arr.impl.size);         \
  } while (false);

/* TODO: Documentation */
#define po_array_resize(arr, ns)                                               \
  do {                                                                         \
    po_array_impl_resize((po_array_void_t *)&arr, ns);                         \
  } while (false)

/* TODO: Documentation */
#define po_array_prepend(arr, elem)                                            \
  do {                                                                         \
    if (arr.impl.size == arr.impl.length) {                                    \
      po_array_grow(arr);                                                      \
    }                                                                          \
                                                                               \
    memcpy(arr.data + 1, arr.data, arr.impl.elem_size * arr.impl.length);      \
    arr.data[0] = elem;                                                        \
    arr.impl.length++;                                                         \
  } while (false)

/* TODO: Documentation */
#define po_array_append(arr, elem)                                             \
  do {                                                                         \
    if (arr.impl.size == arr.impl.length) {                                    \
      po_array_grow(arr);                                                      \
    }                                                                          \
                                                                               \
    arr.data[arr.impl.length] = elem;                                          \
    arr.impl.length++;                                                         \
  } while (false)

/* TODO: Documentation */
#define po_array_truncate(arr) ((arr.impl.length--, arr.data[arr.impl.length]))

/* TODO: Documentation */
#define po_array_behead(arr)                                                   \
  do {                                                                         \
    arr.impl.length--;                                                         \
    arr.data[0] = arr.data[arr.impl.length];                                   \
  } while (false)

/* TODO: Documentation */
#define po_array_remove(arr, at)                                               \
  (po_array_impl_remove((po_array_void_t *)&arr, at))

/* TODO: Documentation */
#define po_array_insert(arr, at, elem)                                         \
  (po_array_impl_insert((po_array_void_t *)&arr, at, (void *)elem))

#define po_array_iterate(arr, var)                                             \
  for (size_t i = 0; i < arr.impl.length && ((var = arr.data[i]), true); i++)

/* TODO: Documentation */
#define po_array_slice(arr, t, i, j)                                           \
  (*(t *)&po_array_impl_slice((po_array_void_t *)&arr, i, j))

#endif