/* niftest.c */

#include "erl_nif.h"
#include <stdio.h> 
#include <CoreMIDI/CoreMIDI.h>    /* interface to MIDI in Macintosh OS X */
#include <CoreServices/CoreServices.h>
#include <CoreMIDI/MIDIServices.h>
#include <CoreAudio/HostTime.h>
#include <AudioUnit/AudioComponent.h>
#include <unistd.h>               /* for sleep() function                */
#include <stdlib.h>

static ERL_NIF_TERM nif_list_type(ErlNifEnv* env, 
				  int argc, 
				  const ERL_NIF_TERM argv[])
{
  int type;
  ERL_NIF_TERM ret;

  if (!enif_get_int(env, argv[0], &type)) //needs to be an integer 
    { 
      return enif_make_badarg(env); 
    };
  
  ComponentDescription d;

  d.componentType = type;
  d.componentSubType = 0;
  d.componentManufacturer = 0;
  d.componentFlags = 0;
  d.componentFlagsMask = 0;

  Component c = FindNextComponent(NULL, &d);
  ret = enif_make_list(env, 0);
  while(c != NULL)
    {
      ComponentDescription found;
      
      Handle nameHandle = NewHandle(0);
      
      GetComponentInfo(c, &found, nameHandle, 0, 0);
      ret = enif_make_list_cell(env,
				enif_make_tuple4(env, 
						 enif_make_int(env,found.componentType),
						 enif_make_int(env,found.componentSubType),
						 enif_make_int(env,found.componentManufacturer),
						 enif_make_string(env, (*nameHandle)+1, ERL_NIF_LATIN1)),
				ret);
      c = FindNextComponent(c, &d);
    };
  return ret;
}

typedef struct {
  int len;
  ...
} Content;

static ERL_NIF_TERM make_au_graph(ErlNifEnv* env, 
				  int argc, 
				  const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;
  // AUGraph *outGraph;
  // allocate our resource thing

  ERL_NIF_TERM ret;
  return enif_make_int(env, 1234);
}


static ErlNifFunc nif_funcs[] =
{
  {"nif_list_type", 1, nif_list_type},
  {"make_au_graph", 0, make_au_graph}
};

ERL_NIF_INIT(au_nifs,nif_funcs,NULL,NULL,NULL,NULL)
