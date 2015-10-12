#include <zmq.h>
#include "escheme.h"

static int zpoll_ready(Scheme_Object *data)
{
  Scheme_Object **argv;
  zmq_pollitem_t *items;
  int nitems;

  argv = (Scheme_Object **)data;
  items = SCHEME_CPTR_VAL(argv[0]);
  nitems = SCHEME_INT_VAL(argv[1]);

  return zmq_poll(items, nitems, 0);
}

static int zpoll_wait(Scheme_Object *data)
{
  Scheme_Object **argv;
  zmq_pollitem_t *items;
  int nitems, timeout;

  argv = (Scheme_Object **)data;
  items = SCHEME_CPTR_VAL(argv[0]);
  nitems = SCHEME_INT_VAL(argv[1]);
  timeout = SCHEME_INT_VAL(argv[2]);

  return zmq_poll(items, nitems, timeout);
}

static Scheme_Object *zpoll(int argc, Scheme_Object **argv)
{
  int timeout, rc;

  timeout = SCHEME_INT_VAL(argv[2]);

  if (timeout == -1)
    rc = scheme_block_until(zpoll_ready, NULL, (Scheme_Object *)argv, 0);
  else
    rc = zpoll_wait((Scheme_Object *)argv);

  return scheme_make_integer(rc);
}

Scheme_Object* scheme_initialize(Scheme_Env *env)
{
  Scheme_Env* menv;

  menv = scheme_primitive_module(scheme_intern_symbol("ext"), env);

  scheme_add_global("EAGAIN", scheme_make_integer(EAGAIN), menv);

  scheme_add_global("zmq_poll*",
                    scheme_make_prim_w_arity(zpoll, "zmq_poll*", 3, 3), menv);

  scheme_finish_primitive_module(menv);

  return scheme_void;
}

Scheme_Object* scheme_reload(Scheme_Env *env)
{
  return scheme_initialize(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("ext");
}
