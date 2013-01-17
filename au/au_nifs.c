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
#include <AudioToolbox/AudioToolbox.h> //for AUGraph

ERL_NIF_TERM enif_make_handle(ErlNifEnv* env, void* obj);

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
  
} State;


static ErlNifResourceType* my_au_graph_type = NULL;
static ErlNifResourceType* my_au_node_type = NULL;
static ErlNifResourceType* my_audio_unit_type = NULL;

static void destroy_au_node(ErlNifEnv* env, void* obj)
{
  fprintf(stderr, "destroy au_node\r\n");
}

static void destroy_audio_unit(ErlNifEnv* env, void* obj)
{
  fprintf(stderr, "destroy audio_unit\r\n");
}

static void destroy_au_graph(ErlNifEnv* env, void* obj)
{
  fprintf(stderr, "destroy au_graph\r\n");
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

  // fprintf(stderr, "load\r\n");
  my_au_graph_type = enif_open_resource_type(env, NULL, "joesresource1",
					     destroy_au_graph,
					     ERL_NIF_RT_CREATE,
					     NULL); 
  my_au_node_type = enif_open_resource_type(env, NULL, "joesresource2",
					 destroy_au_node,
					 ERL_NIF_RT_CREATE,
					 NULL);
 
  my_audio_unit_type = enif_open_resource_type(env, NULL, "joesresource3",
					       destroy_audio_unit,
					       ERL_NIF_RT_CREATE,
					       NULL);
  return 0;
}

static ERL_NIF_TERM make_au_graph(ErlNifEnv* env, 
				  int argc, 
				  const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  OSStatus result;
  ERL_NIF_TERM handle;
  
  // allocate our resource thing
  outGraph = (AUGraph*)enif_alloc_resource(my_au_graph_type, sizeof(AUGraph));
  result = NewAUGraph(outGraph);
  //fprintf(stderr,"result1=%i\r\n", result);
  //fprintf(stderr,"xxx=%i\r\n", (int)(*outGraph));
  // CAShow(outGraph);
  handle = enif_make_handle(env, outGraph);
  // fprintf(stderr, "new au graph=%i\r\n", (int) outGraph);
  return handle;
}

static ERL_NIF_TERM music_device_midi_event(ErlNifEnv* env, 
					    int argc, 
					    const ERL_NIF_TERM argv[])
{
  AudioUnit *outNode;
  OSStatus result;
  int i,j,k,l;
  if (!enif_get_resource(env, argv[0], my_audio_unit_type, (void **)&outNode))
    return enif_make_badarg(env); 
  enif_get_int(env, argv[1], &i);
  enif_get_int(env, argv[2], &j);
  enif_get_int(env, argv[3], &k);
  enif_get_int(env, argv[4], &l);
  // fprintf(stderr, "music_midi ready =%i\r\n", i);
  result = MusicDeviceMIDIEvent(*outNode,i,j,k,l);
  // fprintf(stderr, "music_midi result=%i\r\n", (int) result);
  return enif_make_int(env, (int) result);
}


static ERL_NIF_TERM au_graph_add_node(ErlNifEnv* env, 
				      int argc, 
				      const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  AUNode *outNode;
  OSStatus result;
  ERL_NIF_TERM handle;
  const ERL_NIF_TERM* tuple;
  int arity;
  int type,subtype,man;
  AudioComponentDescription cd;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    {
      return enif_make_badarg(env); 
    };
  // fprintf(stderr, "recovered AUGraph pointer=%i\r\n", (int)outGraph);
  enif_get_tuple(env, argv[1], &arity, &tuple);
  enif_get_int(env, tuple[0], &type);
  enif_get_int(env, tuple[1], &subtype);
  enif_get_int(env, tuple[2], &man);
  cd.componentType = type;
  cd.componentSubType = subtype;
  cd.componentManufacturer = man;
  // fprintf(stderr, "recovered type sub man = %i %i %i\r\n", type, subtype, man);
  cd.componentFlags = 0;
  cd.componentFlagsMask = 0;
  
  // make space for the result
  outNode = (AUNode*)enif_alloc_resource(my_au_node_type, sizeof(AUNode));
  result = AUGraphAddNode(*outGraph, &cd, outNode);
  //fprintf(stderr, "result2 = %i\r\n", result);
  handle = enif_make_handle(env, outNode);
  //fprintf(stderr, "new au node=%i\r\n", (int) outNode);
  return handle;
}

static ERL_NIF_TERM au_graph_initialize(ErlNifEnv* env, 
					int argc, 
					const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  OSStatus result;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    return enif_make_badarg(env); 
  result = AUGraphInitialize(*outGraph);
  //fprintf(stderr, "au_graph_initialize result3=%i\r\n",result);
  return enif_make_int(env, (int) result);
}

static ERL_NIF_TERM au_graph_node_info(ErlNifEnv* env, 
				       int argc, 
				       const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  AUNode *a;
  AudioUnit *outNode;
  int i;
  ERL_NIF_TERM handle;
  OSStatus result;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    return enif_make_badarg(env); 
  if (!enif_get_resource(env, argv[1], my_au_node_type, (void **)&a))
    return enif_make_badarg(env); 
  if (!enif_get_int(env, argv[2], &i))
    return enif_make_badarg(env); 
  outNode = (AudioUnit*)enif_alloc_resource(my_audio_unit_type, sizeof(AudioUnit));
  result = AUGraphNodeInfo(*outGraph, *a, 0, outNode);
  // fprintf(stderr, "result3=%i\r\n",result);
  handle = enif_make_handle(env, outNode);
  // CAShow(outGraph);
  return handle;
}


static ERL_NIF_TERM au_graph_connect_node_input(ErlNifEnv* env, 
						int argc, 
						const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  AUNode *a,*b;
  int i,j;
  OSStatus result;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    return enif_make_badarg(env); 
  if (!enif_get_resource(env, argv[1], my_au_node_type, (void **)&a))
    return enif_make_badarg(env); 
  if (!enif_get_int(env, argv[2], &i))
    return enif_make_badarg(env); 
  if (!enif_get_resource(env, argv[3], my_au_node_type, (void **)&b))
    return enif_make_badarg(env); 
  if (!enif_get_int(env, argv[4], &j))
    return enif_make_badarg(env);
  result = AUGraphConnectNodeInput (*outGraph, *a, 0, *b, 0);
  //fprintf(stderr,"result5=%i\r\n",result);
  //fprintf(stderr, "recovered AUGraph pointer=%i\r\n", (int)outGraph);
  return enif_make_int(env, result);
}

static ERL_NIF_TERM au_graph_open(ErlNifEnv* env, 
				  int argc, 
				  const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  OSStatus result;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    {
      return enif_make_badarg(env); 
    };
  result = AUGraphOpen(*outGraph);
  //fprintf(stderr, "au_graph_open result4=%i\r\n", result);
  //fprintf(stderr, "recovered AUGraph pointer=%i\r\n", (int)outGraph);
  return enif_make_int(env, result);
}

static ERL_NIF_TERM au_graph_start(ErlNifEnv* env, 
				  int argc, 
				  const ERL_NIF_TERM argv[])
{
  AUGraph *outGraph;
  OSStatus result;
  if (!enif_get_resource(env, argv[0], my_au_graph_type, (void **)&outGraph))
    {
      return enif_make_badarg(env); 
    };
  result = AUGraphStart(*outGraph);
  //fprintf(stderr, "au_graph_start result4=%i\r\n", result);
  //fprintf(stderr, "recovered AUGraph pointer=%i\r\n", (int)outGraph);
  return enif_make_int(env, result);
} 

static void destroy_state(ErlNifEnv* env, void* obj)
{
  State* state = (State*)obj;
  /* add a printout here to see GC */
  // free(state->data);
}


static ErlNifFunc nif_funcs[] =
{
  {"nif_list_type", 1, nif_list_type},
  {"make_au_graph", 0, make_au_graph},
  {"au_graph_initialize", 1, au_graph_initialize},
  {"au_graph_add_node", 2, au_graph_add_node},
  {"au_graph_connect_node_input",5, au_graph_connect_node_input},
  {"au_graph_node_info",3, au_graph_node_info},
  {"au_graph_open", 1, au_graph_open},
  {"au_graph_start", 1, au_graph_start},
  {"music_device_midi_event", 5, music_device_midi_event}
};


ERL_NIF_TERM enif_make_handle(ErlNifEnv* env, void* obj)
{
  ERL_NIF_TERM handle;
  handle = enif_make_resource(env, obj);
  enif_release_resource(obj);     
  return handle;
}

ERL_NIF_INIT(au_nifs,nif_funcs,&load,NULL,NULL,NULL)
