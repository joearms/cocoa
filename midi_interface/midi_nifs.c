/* niftest.c */

#include "erl_nif.h"
#include <stdio.h> 
#include <CoreMIDI/CoreMIDI.h>    /* interface to MIDI in Macintosh OS X */
#include <CoreServices/CoreServices.h>
#include <CoreMIDI/MIDIServices.h>
#include <CoreAudio/HostTime.h>


#include <unistd.h>               /* for sleep() function                */

#include <stdlib.h>

//#define CM_DEBUG 1

#include <string.h>



#define MESSAGESIZE 3             /* byte count for MIDI note messages   */

typedef struct
{
  ErlNifEnv *env;
  ErlNifPid* pid;
  int id;
} SaveEnv;

static SaveEnv* saved = NULL;

typedef struct {
  int len;
  int *data;
} State;

static ErlNifResourceType* mystate = NULL;


static void print_error(OSStatus error, const char *operation)
{
  if (error == noErr) return;
	
  char str[20];
  // see if it appears to be a 4-char-code
  *(UInt32 *)(str + 1) = CFSwapInt32HostToBig(error);
  if (isprint(str[1]) && isprint(str[2]) && isprint(str[3]) && isprint(str[4])) {
    str[0] = str[5] = '\'';
    str[6] = '\0';
  } else
    // no, format it as an integer
    sprintf(str, "Error1 %d", (int)error);
  
  fprintf(stderr, "Error: %s (%s)\n", operation, str);
}

static ERL_NIF_TERM list_audio_components(ErlNifEnv* env, 
					  int argc, 
					  const ERL_NIF_TERM argv[])
{
  int n;
  n = AudioComponentCount("");
  return enif_make_int(env, n);
}

void print_sources(){
  int i, n;
  MIDIEndpointRef src;
  CFStringRef strRef;
  char str[255];
  n = MIDIGetNumberOfSources();
  fprintf(stderr, "number of sources = %d \r\n", n);
  for(i=0; i<n;i++){
    src = MIDIGetSource(i);
    MIDIObjectGetStringProperty(src, kMIDIPropertyName, &strRef);
    CFStringGetCString(strRef, str, 255, kCFStringEncodingUTF8);
    fprintf(stderr, "{device,%d,\"%s\"},\r\n", i, str);
  };
}

/*

struct MIDIPacket {
   MIDITimeStamp  timeStamp;
   UInt16         length;
   Byte           data[256];
};

typedef UInt64 MIDITimeStamp;

*/

void myReadProc(const MIDIPacketList *packetList, 
		int readProcRefCon,
		void* srcConnRefCon) {
  MIDIPacket *packet = (MIDIPacket*)packetList->packet;
  int i, j;
  ERL_NIF_TERM midi, event;

  // i is the device number = set to 3344 (don't ask why)
  i = readProcRefCon;   

  /* Alloc a new envirment to build the message in */
  ErlNifEnv* env = enif_alloc_env();
  ERL_NIF_TERM time, msg;
  
  msg = enif_make_list(env, 0);
  int count = packetList->numPackets;
  for (j=0; j<count; j++) {
    event = enif_make_list(env, 0);
    time = enif_make_uint64(env, packet->timeStamp);
    event = enif_make_list_cell(env, time, event);
    for(i=0; i<packet->length; i++){
      midi = enif_make_int(env, packet->data[i]); 
      event = enif_make_list_cell(env, midi, event);
    };
    msg =  enif_make_list_cell(env, event, msg);
    packet = MIDIPacketNext(packet);
  };
  // the message we want to send it in msg
  // saved->pid is a static Pid address where we want to
  // send the messsage to ...
  enif_send(NULL, saved->pid, env, msg);
  // clear the environment for the nect time round
  enif_clear_env(env);
}

void my_connect(int i){
  MIDIClientRef midiclient;
  MIDIPortRef   midiin;
  OSStatus status;
  MIDIEndpointRef src;
  int jj;
  status = MIDIClientCreate(CFSTR("TeStInG"), NULL, NULL, &midiclient);
  if (status != 0){
    print_error(status, "MIDICLientCreate");
    // exit(status);
  };
  jj = 3344;
  // send jj not a pointer 
  status = MIDIInputPortCreate(midiclient, CFSTR("InPuT"), myReadProc, 
			       jj, 
			       // NULL,
			       &midiin);
  if(status != 0){
    // fprintf(stderr,"%i\r\n", status);
    print_error(status, "MIDIInputPortCreate");
    // exit(status);
  };
  fprintf(stderr, "opening keyboard:%i\r\n", i);
  src = MIDIGetSource(i);
  MIDIPortConnectSource(midiin, src, NULL);
  fprintf(stderr, "keyboard connected to source %i\r\n", i);
}

static void *start_thread(void *arg)
{
  //TEnv *env = (TEnv *)arg;
  //ErlNifPid pid;
  int i;
  fprintf(stderr,"start thread\r\n");
  // print_sources();
  i = saved-> id;
  fprintf(stderr,"call connect(%i)\r\n", i);
  my_connect(saved->id);
  // enif_get_local_pid(env->env, env->pid, &pid);
  CFRunLoopRun();
  // enif_send(NULL, &pid, env->env, enif_make_int(env->env, keycode));
    // enif_clear_env(env->env);
  return 0;
}


static ERL_NIF_TERM connect_to_midi_source(
					   ErlNifEnv* env, 
					   int argc, 
					   const ERL_NIF_TERM argv[])
{
  int i;
  ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));
  
  if (!enif_get_int(env, argv[0], &i)) //needs to be an integer 
    { 
      return enif_make_badarg(env); 
    };
  if (!enif_get_local_pid(env, argv[1], pid)) //needs to be an pid 
    { 
      return enif_make_badarg(env); 
    };
  
  saved = (SaveEnv*) enif_alloc(sizeof(SaveEnv));
  saved->env = enif_alloc_env(); 
  saved->pid = pid;
  saved->id  = i;
  fprintf(stderr, "saving i=%i\r\n", i);
  ErlNifTid tid;
  enif_thread_create("redmidi", &tid, start_thread, NULL, NULL);
  return enif_make_int(env, 12345);
}


// list_midi_devices

static ERL_NIF_TERM list_midi_devices(ErlNifEnv* env, 
				      int argc, 
				      const ERL_NIF_TERM argv[])
{
  State *state;
  int i, n;
  char *str;
  CFStringRef strRef;
  MIDIEndpointRef endpoint;
  ERL_NIF_TERM dests, devices, sources;
  ERL_NIF_TERM s1;
  
  n = MIDIGetNumberOfDestinations();
  // fprintf(stderr, "number of destinations = %d \n", n);
  dests = enif_make_list(env, 0);
  for(i=0; i<n;i++){
    endpoint = MIDIGetDestination(i);
    MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &strRef);
    CFStringGetCString(strRef, str, 255, kCFStringEncodingUTF8);
    s1 = enif_make_string(env, str, ERL_NIF_LATIN1);
    dests = enif_make_list_cell(env, s1, dests);
    // fprintf(stderr, "{dest,%d,\"%s\"},\n", i, str);
  };

  n = MIDIGetNumberOfDevices();
  // printf("number of devices = %d \n", n);
  devices = enif_make_list(env, 0);
  for(i=0; i<n;i++){
    endpoint = (MIDIEndpointRef) MIDIGetDevice(i);
    // str = cm_get_full_endpoint_name(endpoint);
    MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &strRef);
    CFStringGetCString(strRef, str, 255, kCFStringEncodingUTF8);
    s1 = enif_make_string(env, str, ERL_NIF_LATIN1);
    devices = enif_make_list_cell(env, s1, devices);
  };

  n = MIDIGetNumberOfSources();
  // fprintf(stderr, "number of sources = %d \n", n);
  sources = enif_make_list(env, 0);
  for(i=0; i<n;i++){
    endpoint = (MIDIEndpointRef) MIDIGetSource(i);
    // str = cm_get_full_endpoint_name(endpoint);
    MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &strRef);
    CFStringGetCString(strRef, str, 255, kCFStringEncodingUTF8);
    s1 = enif_make_string(env, str, ERL_NIF_LATIN1);
    sources = enif_make_list_cell(env, s1, sources);
  };


  return enif_make_tuple3(env, sources, dests, devices);
  
}

static ErlNifFunc nif_funcs[] =
{
  {"list_midi_devices", 0, list_midi_devices},
  {"list_audio_components", 0, list_audio_components},
  {"connect_to_midi_source", 2, connect_to_midi_source}
};

ERL_NIF_INIT(midi_nifs,nif_funcs,NULL,NULL,NULL,NULL)
