#import <Cocoa/Cocoa.h>
#include "erl_nif.h"

// gcc -o test1 test1.m -framework Cocoa
// http://forums.macnn.com/t/209595/cocoa-without-nib-file-need-help
// modifed to record the buttons

typedef struct
{
  ErlNifEnv *env;
  ErlNifPid* pid;
  int id;
} SaveEnv;

extern SaveEnv saved;

@interface myView : NSView <NSWindowDelegate>
{

}

-(IBAction)clickedMe:(id) sender;
- (void)drawRect:(NSRect)rect;  // instance method interface

@end


@implementation myView

- (void)drawRect:(NSRect)rect
{
    NSBezierPath *path = [NSBezierPath bezierPathWithRect:NSMakeRect(10,10,180,180)];
    [[NSColor greenColor] set];
    [path fill];
    // NSLog(@"myView: drawRect:");
}

-(void)windowWillClose:(NSNotification *)notification
{
     [[NSApplication sharedApplication] terminate:self];
}

-(IBAction)clickedMe:(id) sender
{
  //printf("clickedMe:%li\r\n",(long)sender);
  //printf("saved address = %li\r\n",&saved);
  //printf("pid = %li\r\n",(long)(saved.pid));
  send_erlang("click", "nil", (long)sender);
}

@end
    
void do_send_cocoa(id i, char *select_str, char *str)
{
  // do_send_cocoa can be performed in any old thread
  // no necessarily the main thread
  // so I've enclosed this in
  //   [NSAutoreleasePool ...]
  //     ...
  //   [pool release]
  // without this I get an error
  
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSString *ns = [[NSString alloc] initWithUTF8String:str];
  NSString *sel = [[NSString alloc] initWithUTF8String:select_str];
  SEL selector = NSSelectorFromString(sel);
  [i performSelector:selector withObject:ns];
  // if there were several objects use
  // [i performSelector:selector withObject:x1 withObject:x2 ...]
  // should I use respondsToSelector first???
  [pool release];
}

int myrun()
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSWindow *window;
  myView *view;

  // just make a window with some stuff in it
  
  [NSApplication sharedApplication];
  view = [[myView alloc] initWithFrame:NSMakeRect(0,100,200,200) ];
  window = [[NSWindow alloc] 
	     initWithContentRect:NSMakeRect(50,100,200,400) 
	     styleMask:NSTitledWindowMask | NSResizableWindowMask
	     backing:NSBackingStoreBuffered
	     defer:TRUE];
  
  // create a text field
  NSTextField *text=[[NSTextField alloc]initWithFrame:NSMakeRect(10,60,180,22) ];
  // tell erlang the name of the text field
  // so it can manipulate it later
  send_erlang("register", "text", (long)text);
  [text setStringValue:@"sample text"];

  // create a couple of buttons
  // and send the addresses to erlang
  NSButton *button=[[NSButton alloc]initWithFrame:NSMakeRect(10,10,180,32)];
  send_erlang("register", "b0", (long)button);
  
  [button setBezelStyle:NSRoundedBezelStyle];
  [button setTitle:@"Click"];
  [button setAction:@selector(clickedMe:)];
  
  NSButton *button1=[[NSButton alloc]initWithFrame:NSMakeRect(10,220,180,32) ];
  send_erlang("register", "b1", (long)button1);
  [button1 setBezelStyle:NSRoundedBezelStyle];
  [button1 setTitle:@"Click123"];
  [button1 setAction:@selector(clickedMe:)];
  
  [window setTitle:@"test1"];
  
  [[window contentView] addSubview:view];
  [[window contentView] addSubview:text];
  [[window contentView] addSubview:button];
  [[window contentView] addSubview:button1];
  
  [window setDelegate:view];
  [window makeKeyAndOrderFront: nil];
  [pool release];
  
  [NSApp run];
  return 0;
}

// saved uses a global varibale for the pid
// question should I allocate a new env each time
// or reuse the old one - are there efficiency concerns here

int send_erlang(char *s1, char *s2, long xx){
  ErlNifEnv* env = enif_alloc_env();
  ERL_NIF_TERM i,msg;
  i = enif_make_uint64(env, xx);
  msg = enif_make_tuple3(env, 
			 enif_make_atom(env,s1), 
			 enif_make_atom(env,s2), 
			 i);
  enif_send(NULL, saved.pid, env, msg);
  enif_clear_env(env);
}
