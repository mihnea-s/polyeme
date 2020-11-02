#include "array.h"

/* TODO: Documentation */
po_array_impl_t po_array_impl_init(size_t size, size_t elem_size) {
  return (po_array_impl_t){
      .length = 0,
      .size = size,
      .elem_size = elem_size,
  };
}

/* TODO: Documentation */
void po_array_impl_remove(po_array_void_t *arr, size_t at) {}

/* TODO: Documentation */
void po_array_impl_insert(po_array_void_t *arr, size_t at, void *elem) {}

/* TODO: Documentation */
po_array_void_t po_array_impl_slice(po_array_void_t *old, size_t i, size_t j) {
  po_array_void_t arr = *old;
  void *new_data = malloc((j - i) * arr.impl.elem_size);
  memcpy(new_data, arr.data + i, (j - i) * arr.impl.elem_size);
  arr.data = new_data;
  return arr;
}

/* TODO: Documentation */
void po_array_impl_resize(po_array_void_t *arr, size_t ns) {
  arr->data = realloc(arr->data, arr->impl.elem_size * ns);
  arr->impl.size = ns;
}