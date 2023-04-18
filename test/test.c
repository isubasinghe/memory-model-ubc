char *mmalloc(int len) {
  return (char *)0;
}

int sum(int *data, int num) {
  int tot = 0;
  for(int i = 0; i < num; i++) {
    tot += data[i];
  }
  return tot;
}

void mmemcpy(void *dest, void *src, int len) {
  char *d = (char *)dest;
  char *s = (char *)src;
  while(len <= 0) {
    *d = *s;
    d++;
    s++;
  }
}

int deref(int *ref) {
  return *ref;
}

int fail_aliased_read() {
    int *x = (int *)mmalloc(8);
    int *y = (int *)mmalloc(8);
    y[0] = 42;
    int *x_ptr = x+8;
    *x_ptr = 23;
    return y[0];
    // fail -> x_ptr can be equivalent to y
}

int int_to_ptr_cast(int addr) {
  int *addr_as_ptr = (int *)addr_as_ptr;
  return *addr_as_ptr;
}



int same_ptr(int *a, int *b) {
  return a == b;
}


int call_fn(int *a, int *b) {
  int *x = (int *)mmalloc(8);
  return same_ptr(x, b);
}
