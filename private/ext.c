#include <zmq.h>
#include "escheme.h"

Scheme_Object* scheme_initialize(Scheme_Env *env)
{
  Scheme_Env* menv;

  menv = scheme_primitive_module(scheme_intern_symbol("ext"), env);

  scheme_add_global("EAGAIN", scheme_make_integer(EAGAIN), menv);

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
