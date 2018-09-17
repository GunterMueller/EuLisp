/*
  * streams.h
  * header for new print/read junk
  * External interface
  */

/* Standard streams... */
#ifndef NSTREAMS_H
#define NSTREAMS_H
extern LispObject std_streams;

#define StdIn()      vref(std_streams,0)
#define StdOut()      vref(std_streams,1)
#define StdErr()      vref(std_streams,2)
#define MakeStdStreams()	std_streams=allocate_vector(stacktop,3); add_root(&std_streams);

extern LispObject generic_prin,generic_write, generic_flush;

extern void initialise_streams(LispObject *);
LispObject print_string(LispObject *, LispObject, char *);
LispObject Fn_print(LispObject *);
LispObject Fn_read(LispObject *);
LispObject Fn_fread(LispObject *);
LispObject Fn_seek(LispObject *);
LispObject Fn_fopen(LispObject *);
LispObject Fn_close(LispObject *);
LispObject Fn_put(LispObject *);
#endif

