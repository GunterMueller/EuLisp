/* $Id: lex_global.h,v 1.1 1994/01/25 13:45:36 djb Exp $ */
/* $Log: lex_global.h,v $
 * Revision 1.1  1994/01/25  13:45:36  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:52:20  pab
 * fixed trivial problem
 * 
 * Revision 1.2  1992/11/26  15:54:11  pab
 * Change for new reader
 *
 * Revision 1.1  1991/08/12  16:49:43  pab
 * Initial revision
 *
 * Revision 1.2  1991/02/11  17:56:59  is
 * Added ID and LOG headers
 *
*/

typedef struct 
{
  LispObject lispval;
} pptoken;

extern pptoken pptok;

#define	IDENTIFIER	258
#define	INTEGER		259
#define	RATIONAL	260
#define	FLOAT		261
#define	CHARACTER	262
#define	STRING		263
#define WRAPPER		264
#define	EXTENSION	268
#define	OPEN_PAIR	269
#define	CLOSE_PAIR	270
#define	DOT		271
#define	END_OF_STREAM	272
#define LISPNIL		273
