#include <string.h>
#include <ctype.h>

#ifndef SOCKET_BUFFER_SIZE
#define SOCKET_BUFFER_SIZE (10240)
#endif

extern SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,socket_buffer_form);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(char *,socket_buffer);
extern SYSTEM_THREAD_SPECIFIC_DECLARATION(int,socket_buffer_ptr);

#define BUFFER_LEFT() (SOCKET_BUFFER_SIZE - \
		       SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER_PTR() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer) +\
		  SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER_START() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer))
#define BUFFER_FORM() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_form))

extern void write_object(LispObject*,LispObject);
extern LispObject read_object(LispObject*);

