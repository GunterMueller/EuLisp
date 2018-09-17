/* 
 * swap streams in flex 
 */

/*
 * $Id: yyfudge.c,v 1.7 1995/01/04 16:25:44 djb Exp $
 *
 * $Log: yyfudge.c,v $
 * Revision 1.7  1995/01/04  16:25:44  djb
 * forgot to change 32 to LINENO in yy_savebuf_list and yy_save_lineno
 * initialisation
 *
 * Revision 1.6  1994/08/16  13:15:22  djb
 * 32 => NOFILE, which is the max number of open files per process
 * as #defined in <sys/param.h>
 *
 * Revision 1.5  1994/07/04  16:11:00  djb
 * removed obsolete CGC #ifdefs and (YY_CHAR *) casts are now just (char *)
 *
 * Revision 1.4  1994/06/14  12:59:17  djb
 * changes to handle flex-2.4.6 (Wolfgang's E1)
 *
 * Revision 1.3  1994/03/09  14:52:31  djb
 * added stacktop to non-STDC lex_getc declaration
 *
 * Revision 1.2  1994/02/08  12:11:35  djb
 * added jap's changes to pass stacktop down to low level reader code
 *
 * Revision 1.1  1994/02/08  12:10:57  djb
 * Initial revision
 *
 * Revision 1.3  1991/11/15  13:45:56  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:55  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:17  pab
 * Initial revision
 *
 * Revision 1.7  1991/05/15  09:18:14  pab
 * 'C' garbage collector changes
 *
 * Revision 1.6  1991/02/14  01:40:09  kjp
 * Added a modified version of yy_switch_to_buffer to make multi-file
 * reading work "correctly".
 *
 * Revision 1.5  1991/02/11  17:59:50  is
 * Added support for read-with-line-numbers
 *
*/

#include <sys/param.h>
static SYSTEM_GLOBAL_ARRAY1(YY_BUFFER_STATE,yy_savebuf_list,NOFILE);
static SYSTEM_GLOBAL_ARRAY1(int,yy_save_lineno,NOFILE);

#ifdef YY_USE_PROTOS
void my_yy_switch_to_buffer( YY_BUFFER_STATE new_buffer )
#else
void my_yy_switch_to_buffer( new_buffer )
YY_BUFFER_STATE new_buffer;
#endif

    {
    
    /* This line must be ignored for the buffer swapping to work */

    /*
    if ( yy_current_buffer == new_buffer )
	return;
    */

    if ( yy_current_buffer )
	{
	/* flush out information for old buffer */
	*yy_c_buf_p = yy_hold_char;
	yy_current_buffer->yy_buf_pos = yy_c_buf_p;
	yy_current_buffer->yy_n_chars = yy_n_chars;
	}

    yy_current_buffer = new_buffer;
    yy_load_buffer_state();

    /* we don't actually know whether we did this switch during
     * EOF (yywrap()) processing, but the only time this flag
     * is looked at is after yywrap() is called, so it's safe
     * to go ahead and always set it.
     */
    yy_did_buffer_switch_on_eof = 1;
  }

#ifdef __STDC__
void my_yyrestart( FILE *input_file )
#else
void my_yyrestart( input_file )
FILE *input_file;
#endif

{
    yyin = input_file;
    yy_init = 1;
  }

static FILE *yy_last_read_stream = (FILE *) -1;

#ifdef __STDC__
void yy_save_stream(FILE *f)
#else
void yy_save_stream(f)
FILE *f;
#endif
{
  int fn;
  extern int lex_input_line_number;

  fn = fileno(f);
  SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn)= YY_CURRENT_BUFFER;
  SYSTEM_GLOBAL_ARRAY1_VALUE(yy_save_lineno,fn)=lex_input_line_number;
}

#define FUDGEBUG(x) /* x;fflush(stdout); */


#ifdef YY_USE_PROTOS
static void my_copy(char *a,char *b,int n)
#else
static void my_copy(a,b,n)
char *a,*b;
int n;
#endif
{
  for (; n>0; --n) a[n-1] = b[n-1];
}

#ifdef YY_USE_PROTOS
YY_BUFFER_STATE my_yy_create_buffer( FILE *file, int size )
#else
YY_BUFFER_STATE my_yy_create_buffer( file, size )
FILE *file;
int size;
#endif
{
  YY_BUFFER_STATE b;

  b = (YY_BUFFER_STATE) allocate_stack(0, sizeof( struct yy_buffer_state ) );
  if ( ! b )
    YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

  b->yy_buf_size = size;

  /* yy_ch_buf has to be 2 characters longer than the size given because
   * we need to put in 2 end-of-buffer characters.
   */
  /* add 2 to align */
  b->yy_ch_buf = (char *) allocate_stack(0, (unsigned) (b->yy_buf_size + 2 + 2) );

  if ( ! b->yy_ch_buf )
    YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

  yy_init_buffer( b, file );
  
  return ( b );
}



#ifdef YY_USE_PROTOS
void my_yy_delete_buffer( YY_BUFFER_STATE b )
#else
void my_yy_delete_buffer( b )
YY_BUFFER_STATE b;
#endif

    {
    if ( b == yy_current_buffer )
	yy_current_buffer = (YY_BUFFER_STATE) 0;
    deallocate_stack(NULL, (char *) b->yy_ch_buf, b->yy_buf_size + 2 + 2);
    deallocate_stack(NULL, (char *) b, sizeof( struct yy_buffer_state ) );
    }

#ifdef __STDC__
void yy_set_stream(FILE *f)
#else
void yy_set_stream(f)
FILE *f;
#endif
{
  YY_BUFFER_STATE sin;
  int fn;

  fn = fileno(f);

  FUDGEBUG(printf("[Setting to %d]\n",fn));

				/* if (f == yy_last_read_stream) return; */

  if (yy_last_read_stream != (FILE *) -1)
    yy_save_stream(yy_last_read_stream);
  yy_last_read_stream = f;

  if (SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn)!= 0) {
    my_yy_switch_to_buffer(SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn));
    lex_input_line_number = SYSTEM_GLOBAL_ARRAY1_VALUE(yy_save_lineno,fn);

FUDGEBUG(printf("[Old %d]\n",SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn)));

  } else {
    sin = my_yy_create_buffer(f,YY_BUF_SIZE);
    SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn)= sin;
    my_yy_switch_to_buffer(sin);
    lex_input_line_number=1;
FUDGEBUG(  printf("[New %d]\n",SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,fn));)

  }
}

#ifdef __STDC__
int yy_close_stream(FILE *fd)
#else
int yy_close_stream(fd)
FILE *fd;
#endif
{
  int num;

  num = fileno(fd);

  FUDGEBUG(printf("[Closing %d]\n",num));

  if (SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,num)!= NULL) {

    FUDGEBUG(printf("[Was %d]\n",SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,num));)
    my_yy_delete_buffer(SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,num));
  }
  SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,num) = 0;

  FUDGEBUG(printf("[Zeroed %d]\n",SYSTEM_GLOBAL_ARRAY1_VALUE(yy_savebuf_list,num));)

  if (fd == yy_last_read_stream) yy_last_read_stream = (FILE *) -1;
  return system_fclose(fd);
}

#ifdef __STDC__
void yy_reset_stream(LispObject *stacktop, FILE *fd)
#else
void yy_reset_stream(stacktop, fd)
LispObject *stacktop;
FILE *fd;
#endif
{

#ifdef __STDC__
  extern void yy_init_buffer(YY_BUFFER_STATE,FILE *);
#else
  extern void yy_init_buffer();
#endif

  yy_set_stream(fd);
  yy_save_stream(fd);
  yyrestart(stacktop, fd);
/*  yy_save_stream(fd); */
}

void initialise_fudge()
{
  SYSTEM_INITIALISE_GLOBAL_ARRAY1(YY_BUFFER_STATE,yy_savebuf_list,NOFILE,NULL);
  SYSTEM_INITIALISE_GLOBAL_ARRAY1(int,yy_save_lineno,NOFILE,1);
}

#ifdef __STDC__
int lexer_getc(LispObject *stacktop,FILE *fd)
#else
int lexer_getc(stacktop, fd)
LispObject *stacktop;
FILE *fd;
#endif
{
  /* reset stream in here */
  yy_reset_stream(stacktop, fd);
  return (input(stacktop));
}

