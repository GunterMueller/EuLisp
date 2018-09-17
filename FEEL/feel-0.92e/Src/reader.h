/*
  * Header file for reader
  */

extern void initialise_input(LispObject *);
extern LispObject sys_read(LispObject *,FILE *);
extern int reader_fclose(LispObject *,FILE *);
extern LispObject q_eof; /* end of file character */
