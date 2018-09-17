/* 
 * Eulisp version of malloc
 * may not be safe...
 */

/*
 * $Id: eumalloc.c,v 1.1 1994/04/06 14:39:52 djb Exp $
 *
 * $Log: eumalloc.c,v $
 * Revision 1.1  1994/04/06  14:39:52  djb
 * Initial revision
 *
 *
 */

#include "defs.h"
#include "structs.h"
#include "allocate.h"

#define PAGBITS 5
#define MAXHEAPLEN 16384 /* this is highly dubious, if not wildly inaccurate */
#define BIGINT 0xffffffff

char *feel_malloc(int size)
{
  char *data;
  int len;

  char *ptr;
   /* keep the numbers round */

  len = (((size + 2*sizeof(int)) >> PAGBITS) << PAGBITS) + (1 << PAGBITS);

#ifdef debug /* Tue Apr 23 16:45:03 1991 */
/**/  if (len < size)
/**/  {
/**/    fprintf(stderr,"Feel-malloc bug sz: %d len:%d \n",size,len);
/**/    exit(0);
/**/  }
#endif /* debug Tue Apr 23 16:45:03 1991 */

  if (len < MAXHEAPLEN)
    data = allocate_space(NULL,len);
  else
    data = allocate_stack(NULL,len);


#ifdef notdef /* Tue Apr 23 16:45:29 1991 */
/**/  fprintf(stderr,"Malloc: allocating %x bytes at %x\n",len,data+4);  
#endif /* notdef Tue Apr 23 16:45:29 1991 */

  if(data == NULL)
    {
      fprintf(stderr,"Malloc failed...panicing. Request was for %d(%d) bytes.",size,len);
      exit(0);
    }

  *((int *) (data + len - sizeof(int))) = 0xdeadbeef;
  *(int *) data = len;

#ifdef notdef /* Tue Apr 23 16:45:48 1991 */
/**/  for (ptr = data + sizeof(int) ; ptr < data + len - sizeof(int) ; ptr ++)
/**/    *ptr='x';
#endif /* notdef Tue Apr 23 16:45:48 1991 */

  return (data + sizeof(int));
}


void feel_free(char *data)
{
  int len;

#ifdef notdef /* Tue Apr 23 16:46:23 1991 */
/**/  printf("Malloc: freeing: %x bytes at %x\n",*((int *) (data-4)),data);
/**/  if ( *((int *) (data + len - 2*sizeof(int))) != 0xdeadbeef)
/**/    {
/**/      fprintf(stderr,"DataSpace overrun: 0x%x\n",data);
/*      exit(255);*/
/**/    }
#endif /* notdef Tue Apr 23 16:46:23 1991 */
  len = *((int *) (data - sizeof(int)));
  
  if (len < MAXHEAPLEN) /* Null is stack pointer --- pray for no errors */
    deallocate_space(NULL,data - sizeof(int), len);
  else
    deallocate_stack(NULL,data - sizeof(int), len);
  
  return;
}



