#include "erl_nif.h"
#include <stdio.h> 
#include <unistd.h>               /* for sleep() function                */
#include <stdlib.h>
#include <string.h>

// This is an undocumented function ..

int erl_drv_steal_main_thread(char *name,
			      ErlNifTid *dtid,
			      void* (*func)(void*),
			      void* arg,
			      ErlDrvThreadOpts *opts);

int myrun();

// this is where I keep a copy of the pid to talk to erlang with
typedef struct
{
  ErlNifEnv *env;
  ErlNifPid* pid;
  int id;
} SaveEnv;

SaveEnv saved;

static ErlNifResourceType* mystate = NULL;

static void *start_thread(void *arg)
{
  myrun(); 
  return 0;
}

// send a message to a coca object

static ERL_NIF_TERM send_cocoa(
			       ErlNifEnv* env, 
			       int argc, 
			       const ERL_NIF_TERM argv[])
{
  long i;
  char selector[150], str[150];

  enif_get_long(env, argv[0], &i);
  enif_get_string(env, argv[1], selector, 100, ERL_NIF_LATIN1);
  enif_get_string(env, argv[2], str, 100, ERL_NIF_LATIN1);
  do_send_cocoa(i, selector, str);
}


static ERL_NIF_TERM connect_to_cocoa(
				     ErlNifEnv* env, 
				     int argc, 
				     const ERL_NIF_TERM argv[])
{
  int i;
  ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));
  
  if (!enif_get_local_pid(env, argv[0], pid)) //needs to be pid
    { 
      fprintf(stderr,"oopps1\r\n");
      return enif_make_badarg(env);
    };
  saved.env = enif_alloc_env(); 
  saved.pid = pid;
  saved.id  = 4321; // dummy no significance
  ErlNifTid tid;
  // we need to do this in the main thread
  erl_drv_steal_main_thread((char *)"blaa",
			    &tid,
			    start_thread,
			    NULL,NULL);

  return enif_make_int(env, enif_make_atom(env, "looking_good"));
}

static ErlNifFunc nif_funcs[] =
  {
  {"connect_to_cocoa", 1, connect_to_cocoa},
  {"send_cocoa", 3, send_cocoa}
};

ERL_NIF_INIT(cocoa_nifs,nif_funcs,NULL,NULL,NULL,NULL)
