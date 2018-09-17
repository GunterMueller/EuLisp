/* -*- C -*- */

/* EuLisp lexer
 *
 * Russell Bradford, Bath 1990
 * Based on work by J. Bejar, A. Moreno, E. Sesa (FIB,UPC) - Bath, July 1990
 *
 */

/* $Id: feelflex.lex,v 1.3 1994/03/15 10:12:47 djb Exp $ */
/* $Log:
*/
%{

#include "defs.h"
/* We need to add an extra parameter */
#ifdef YY_DECL
#undef YY_DECL
#ifdef YY_USE_PROTOS
#define YY_DECL  int yylex YY_PROTO((LispObject *stacktop))
#else
#define YY_DECL  int yylex (stacktop) LispObject *stacktop;
#endif
#endif
/* Emergency fileno */

#ifndef HAS_READFILENO
#define fileno(fh) (fh == stdin ? 0 \
		    : (fh == stdout ? 1 \
                       : (fh == stderr ? 2 : 3)))
#endif /* HAS_READFILENO */

#include <string.h>
#include <ctype.h>
#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "symboot.h"
#include "error.h"

#include "lex_global.h"

#ifdef __STDC__
#define OF(ansi, kr, krargs) ansi
#define PROTO(args) args
#else
#define OF(ansi, kr, krargs) kr krargs
#define PROTO(args) ()
#endif

#ifdef __STDC__

/*
#ifndef DONT_HAVE_STDLIB_H
#include <stdlib.h>
#else
void *malloc( unsigned );
void free( void* );
#endif
*/

extern int read(int, char *, unsigned);

#endif

/* the values returned */
double flex_floatval;
long flex_ratnumval, flex_ratdenval;
long flex_intval;
char flex_charval;
char flex_stringval[1024];	/* I don't like fixed array sizes */
char flex_idval[256];
pptoken pptok;			/* for backward compatibility */
int lex_input_line_number=1;	/* Needs changing for shared memory */

static double convert_float PROTO((void));
static long convert_integer PROTO((char *, int));
static long int_with_base PROTO((char *, int *));
static void convert_rational PROTO((void));
static void convert_string PROTO((LispObject *stacktop));
static char convert_character PROTO((void));
static void tidy_id PROTO((void));
int escaped_id PROTO((char*));
char *visible PROTO((char));

/* Hack the lex reader */

#ifdef YY_INPUT
#undef YY_INPUT
#endif

#ifdef HAS_READFILENO
#define YY_INPUT(buf,result,max_size) \
  if ( (result = system_read(stacktop, fileno(yyin), \
			     (char *) buf, max_size )) < 1 ) \
    { 						\
      if(result == 0)\
	GC_sync_test();\
      else{  \
	perror("Some very bad thing happened");\
        system_lisp_exit(0);					\
	}\
    }			
#else
#define YY_INPUT(buf,result,max_size) \
{ 					\
  int len;				\
  char *ret;				\
  fprintf(stderr,"Reading from: %d %d\n",buf,max_size);	\
  if (fgets(buf,max_size,yyin)==NULL)			\
    result=0;				\
  else					\
    result=strlen(buf);			\
}

#endif /* HAS_READFILENO */

%}

/* forward references */

digit		[0-9]
alphabetic	[A-Za-z]
alphanumeric	[0-9A-Za-z]
special		[\\\#()"',;`]
non-alpha	[^0-9A-Za-z]
ordinary	[!$%&*/:<=>?@[\]^_{}~]
sign		[+-]

/* tokens */

dot		"."
pair-begin	"("
pair-end	")"
quotation	"'"
antiquotation	"`"
unquotation	","
unquotesplice	",@"
extension	"#"
space		[ ]
tab		\t
newline		\n
whitespace	{space}|{tab}|{newline}
comment-begin	";"
comment-end	{newline}
comment		{comment-begin}.*{comment-end}

/*delimeter	{whitespace}|{pair-begin}|{pair-end}|{string-begin}|{string-end}|{comment-begin}*/

/* number syntax */

/* integers */

digit2		[01]
digit8		[0-7]
digit10		[0-9]
digit16		[0-9A-Fa-f]
extended-digit	[0-9A-Za-z]

ubinary		{extension}[bB]{digit2}+
uoctal		{extension}[oO]{digit8}+
udecimal	{digit10}+
uhexadecimal	{extension}[xX]{digit16}+
uinteger-with-base	{extension}{udecimal}[rR]{extended-digit}+
uinteger	{ubinary}|{uoctal}|{udecimal}|{uhexadecimal}|{uinteger-with-base}

binary		{sign}?{ubinary}
octal		{sign}?{uoctal}
decimal		{sign}?{udecimal}
hexadecimal	{sign}?{uhexadecimal}
integer-with-base	{sign}?{uinteger-with-base}

/* rationals */

ratio-separator	"/"
urational	{uinteger}{ratio-separator}{uinteger}
rational	{sign}?{urational}

/* floats */

float-separator	"."
expt-mark	[edED]
exponent	{expt-mark}{sign}?{udecimal}
ufloat		{udecimal}{exponent}|{float-separator}{udecimal}{exponent}?|{udecimal}{float-separator}{udecimal}?{exponent}?
float		{sign}?{ufloat}

/* strings */
/* we do these by hand, as it's easier that way */

string-begin	\"

/* character syntax */

character-extension	"\\"
control-extension	"^"
/*special-name	alert|backspace|delete|formfeed|linefeed|newline|return|tab|space|vertical-tab*/
alert-name	[Aa][Ll][Ee][Rr][Tt]
backspace-name	[Bb][Aa][Cc][Kk][Ss][Pp][Aa][Cc][Ee]
delete-name	[Dd][Ee][Ll][Ee][Tt][Ee]
formfeed-name	[Ff][Oo][Rr][Mm][Ff][Ee][Ee][Dd]
linefeed-name	[Ll][Ii][Nn][Ee][Ff][Ee][Ee][Dd]
newline-name	[Nn][Ee][Ww][Ll][Ii][Nn][Ee]
return-name	[Rr][Ee][Tt][Uu][Rr][Nn]
tab-name	[Tt][Aa][Bb]
space-name	[Ss][Pp][Aa][Cc][Ee]
vtab-name	[Vv][Ee][Rr][Tt][Ii][Cc][Aa][Ll]-[Tt][Aa][Bb]
special-name	{alert-name}|{backspace-name}|{delete-name}|{formfeed-name}|{linefeed-name}|{newline-name}|{return-name}|{tab-name}|{space-name}|{vtab-name}
hex-name	[xX]{digit16}{1,4}
literal-name	{alphanumeric}|{non-alpha}
control-name	{control-extension}{literal-name}
character-name	{literal-name}|{control-name}|{special-name}|{hex-name}
caret		{extension}{character-extension}{control-extension}{whitespace}
character	{extension}{character-extension}{character-name}

/* identifiers */

multiple-escape	\|
single-escape	\\
point		\.
non-escape	[^|\\]
single-escaped	{single-escape}(.|\n)
multiple-escaped	{multiple-escape}({single-escaped}|{non-escape})*{multiple-escape}
escaped		{single-escaped}|{multiple-escaped}
non-digit	{alphabetic}|{ordinary}|{escaped}
constituent	{non-digit}|{digit}|{sign}|{point}
normal-identifier	{non-digit}{constituent}*
sign-identifier	{sign}|{sign}({non-digit}|{sign}|{point}){constituent}*
point-identifier	{point}({non-digit}|{sign}|{point}){constituent}*
peculiar-identifier	{sign-identifier}|{point-identifier}
identifier	{normal-identifier}|{peculiar-identifier}

/* Scheme support */
hash-t		{extension}[tT]
hash-f		{extension}[fF]

%%

<<EOF>>		{ return(END_OF_STREAM); }

{comment}	{lex_input_line_number++;}

{whitespace}	{if (yytext[0]=='\n') lex_input_line_number++;}

{dot}		{ return(DOT); }

{pair-begin}	{ return(OPEN_PAIR); }

{pair-end}	{ return(CLOSE_PAIR); }

{quotation}	{
                  pptok.lispval=get_symbol(stacktop,"quote");
		  return(WRAPPER);
		}

{antiquotation}	{ 
                  pptok.lispval=get_symbol(stacktop,"quasiquote");
		  return(WRAPPER);
		}

{unquotation}	{ pptok.lispval=get_symbol(stacktop,"unquote");
		  return(WRAPPER);
		}

{unquotesplice}	{ pptok.lispval=get_symbol(stacktop,"unquote-splicing");
		  return(WRAPPER);
		}

{extension}	{ return(EXTENSION); }

{float}		{ flex_floatval = convert_float();
		  pptok.lispval = allocate_float(stacktop,flex_floatval);
		  return(FLOAT); }

{rational}	{ convert_rational(); /* XXX: ?? */
		  return(RATIONAL); }

{binary}	{ flex_intval = convert_integer(yytext, 2);
		  pptok.lispval = allocate_integer(stacktop,flex_intval);
		  return(INTEGER); }

{octal}		{ flex_intval = convert_integer(yytext, 8);
		  pptok.lispval = allocate_integer(stacktop,flex_intval);
		  return(INTEGER); }

{decimal}	{ flex_intval = atol(yytext);
		  pptok.lispval = allocate_integer(stacktop,flex_intval);
		  return(INTEGER); }

{hexadecimal}	{ flex_intval = convert_integer(yytext, 16);
		  pptok.lispval = allocate_integer(stacktop,flex_intval);
		  return(INTEGER); }

{integer-with-base}	{ int returns;
			  flex_intval = int_with_base(yytext, &returns);
			  if (returns != yyleng) yyless(returns);
			  pptok.lispval = allocate_integer(stacktop,flex_intval);
			  return(INTEGER); }

{string-begin}	{ convert_string(stacktop);
		  pptok.lispval = allocate_string(stacktop,flex_stringval,
						     strlen(flex_stringval));
		  return(STRING); }

{caret}		{ flex_charval = '^';
                  pptok.lispval = allocate_char(stacktop,flex_charval);
                  return(CHARACTER); }

{character}	{ flex_charval = convert_character();
		  pptok.lispval = allocate_char(stacktop,flex_charval);
		  return(CHARACTER); }

{identifier}	{ tidy_id();
		  pptok.lispval = get_symbol(stacktop,flex_idval);
		  return(IDENTIFIER); }

{hash-t}	{ pptok.lispval = lisptrue;
		  return(IDENTIFIER); }

{hash-f}	{ pptok.lispval = nil;
		  return(LISPNIL); }

.		{ fprintf(stderr, "\n*** Illegal Character '%s' ignored\n",
			  visible(*yytext)); }

%%

/* +#o123 or -#o123 or #o123 or binary or hex */
static long convert_integer OF ((char *text, int base),
(text, base),
char *text;
int base;)
{
  switch (*text) {
  case '+':
    return strtol(text + 3, 0, base);
  case '-':
    return -strtol(text + 3, 0, base);
  default:
    return strtol(text + 2, 0, base);
  }
}

/* +#5r123 or -#5r123 or #5r123 */
static long int_with_base OF ((char *text, int *ret),
(text, ret),
char *text;
int *ret;)
{
  char *val;
  int base;
  long value;

  switch (*text) {
  case '+':
    base = (int)strtol(text + 2, &val, 10);
    value = strtol(val + 1, &val, base);
    break;
  case '-':
    base = (int)strtol(text + 2, &val, 10);
    value = -strtol(val + 1, &val, base);
    break;
  default:
    base = (int)strtol(text + 1, &val, 10);
    value = strtol(val + 1, &val, base);
    break;
  }

  /* all characters used? */
  *ret = (val - text)/sizeof(char);

  return value;
}

/* 123 or #o123 or #5r123 */
static long convert_unumber OF ((char *text),
(text),
char *text;)
{
  char *val;
  int base;

  if (*text == '#')
    switch (text[1]) {
    case 'b':
    case 'B':
      return convert_integer(text, 2);
    case 'o':
    case 'O':
      return convert_integer(text, 8);
    case 'x':
    case 'X':
      return convert_integer(text, 16);
    default:
      base = (int)strtol(text + 1, &val, 10);
      return strtol(val + 1, 0, base);
    }

  return atol(text);
}

/* +num/num or -num/num or num/num */
static void convert_rational(
#ifdef __STDC__
void
#endif
)
{
  char *text = yytext;
  int sign;

  if (*text == '+') {
    sign = 1;
    text++;
  }
  else if (*text == '-') {
    sign = -1;
    text++;
  }
  else {
    sign = 1;
  }

  flex_ratnumval = sign*convert_unumber(text);
  text = strchr(text, '/');
  flex_ratdenval = convert_unumber(text + 1);

}

/* #\a or #\^a or #\alert or #\x1234 */
/* ASCII dependent */
static char convert_character(
#ifdef __STDC__
void
#endif
)
{
  register int i;
  for(i=0;i<yyleng;i++)
    if (yytext[i]=='\n') lex_input_line_number++;

  if (yyleng > 3) {		/* #\^a, #\alert, #\x1234 */
    switch (yytext[2]) {
    case '^':			/* #\^a */
      if ('a' <= yytext[3] && yytext[3] <= 'z')
	return toupper(yytext[3]) & 077;
      else
	return yytext[3] & 077;

    case 'a':			/* alert 07 */
    case 'A':
      return 07;		/* Stardent is non-ansi here */

    case 'b':			/* backspace 010 */
    case 'B':
      return '\b';

    case 'd':			/* delete 0177 */
    case 'D':
      return 0177;

    case 'f':			/* formfeed 014 */
    case 'F':
      return '\f';

    case 'l':			/* linefeed 012 */
    case 'L':
    case 'n':                   /* newline 012 */
    case 'N':
      return '\n';

    case 'r':			/* return 015 */
    case 'R':
      return '\r';

    case 't':			/* tab 011 */
    case 'T':
      return '\t';

    case 's':			/* space 040 */
    case 'S':
      return ' ';

    case 'v':			/* vertical-tab 013 */
    case 'V':
      return '\v';

    case 'x':
    case 'X':
      return (char)strtol(&yytext[3],NULL,16);
    }
  }  
  return yytext[2];
}

/* get all the escapes out of the identifier: produce the internal form */
static void tidy_id(
#ifdef __STDC__
void
#endif
)
{
  int escaped = 0;
  int i, j;

  for (i = 0; yytext[i]; i++)
    if (yytext[i] == '|' ||
	yytext[i] == '\\') {
      escaped = 1;
      break;
    }
    
  if (!escaped) {
    strcpy(flex_idval, yytext);
    return;
  }

  i = 0;
  j = 0;
  while (yytext[j]) {
    if (yytext[j] == '\\') {
      if (yytext[j+1] == '|') {
        flex_idval[i++] = '|';
        j++;
      }
      else if (yytext[j+1] == '\\') {
        flex_idval[i++] = '\\';
        j++;
      }
      else if (yytext[j+1] == '\n') {
	lex_input_line_number++;
	flex_idval[i++] = '\n';
	j++;
      }
      j++;
    } else if (yytext[j] == '|') j++;
    else {			/* Copy the text, checking for newline */
      if ((flex_idval[i++] = yytext[j++])=='\n')
	lex_input_line_number++;
    }
  }

  flex_idval[i] = 0;
}

/* do we need to escape this id when printing?
 * yes if (1) it contains a dodgy character
 *        (2) it is the id of zero length
 *        (3) it starts with the syntax of a number
 *
 * ASCII dependent
 */
int escaped_id OF ((char *id),
(id),
char *id;)
{
  int i;

  for (i = 0; id[i]; i++)
    if (id[i] < 32 || id[i] > 126 || id[i] == '|' || id[i] == '\\') return 1;

  if (strpbrk(id, "|\\#()\"',;` ") ||
      id[0] == 0 ||		/* zero length id */
      isdigit(id[0]) ||					/* 123 */
      (id[0] == '.' && !id[1]) ||			/* |.| */
      (id[0] == '.' && id[1] && isdigit(id[1])) ||	/* .123 */
      ((id[0] == '+' || id[0] == '-') &&
	id[1] && (isdigit(id[1]) ||			/* +123 */
	          (id[1] == '.' && id[2] && isdigit(id[2]))))) /* +.123 */
    return 1;
  else
    return 0;
}

#ifdef __STDC__
static void convert_string(LispObject *stacktop)
#else
static void convert_string(stacktop)
LispObject *stacktop;
#endif
{
  int ch, prevch;
  int i;

  i = 0;
  while ((ch = input(stacktop)) != '"') {
    if (ch == '\\')
      switch (ch = input(stacktop)) {
      case '"':
      case '\\':
	flex_stringval[i++] = ch;
	break;

      case 'a':			/* alert */
      case 'A':
	flex_stringval[i++] = 07;
        break;

      case 'b':			/* backspace */
      case 'B':
	flex_stringval[i++] = '\b';
        break;
	
      case 'd':			/* delete */
      case 'D':
	flex_stringval[i++] = 0177;
        break;

      case 'f':			/* formfeed */
      case 'F':
	flex_stringval[i++] = '\f';
        break;

      case 'l':			/* linefeed */
      case 'L':
      case 'n':			/* newline */
      case 'N':
	flex_stringval[i++] = '\n';
	break;

      case 'r':			/* return */
      case 'R':
	flex_stringval[i++] = '\r';
        break;

      case 't':			/* tab */
      case 'T':
	flex_stringval[i++] = '\t';
        break;

      case 'v':			/* vertical tab */
      case 'V':
	flex_stringval[i++] = '\v';
        break;

      case 'x':
      case 'X':
	prevch = ch;
	if (isxdigit(ch = input(stacktop))) {
	  char val = 0;
	  int count;
	  for (count = 0; count < 4 && isxdigit(ch); count++, ch = input(stacktop)) {
	    val = 16*val;
	    if (isupper(ch)) val += ch - 'A' + 10;
	    else if (islower(ch)) val += ch - 'a' + 10;
	    else val += ch - '0';
	  }
	  flex_stringval[i++] = val;
	}
	else flex_stringval[i++] = prevch;
	unput(ch);
	break;

      default:
	flex_stringval[i++] = ch;
	break;
      }
    else flex_stringval[i++] = ch;
    if (ch == '\n') lex_input_line_number++;
  }

  flex_stringval[i] = 0;

}

static double convert_float(
#ifdef __STDC__
void
#endif
)
{
#ifndef atof
#ifdef __ANSI__
  extern double atof(char *);
#else
  extern double atof();
#endif
#endif

  char buf[256];
  int i;

  for (i = 0; yytext[i]; i++)
    buf[i] = (yytext[i] == 'd' || yytext[i] == 'D') ? 'E' : yytext[i];
  buf[i] = 0;

  return atof(buf);
}

/* more ASCII dependence */
char *visible OF ((char ch),
(ch),
char ch;)
{
  static char buf[10];
  char *ptr = buf;

  if (ch == 127) return "^?";

  if (ch > 127) {
    *ptr++ = 'M';
    *ptr++ = '-';
    ch = ch & 0x7f;
  }

  if (ch < 32) {
    *ptr++ = '^';
    ch += '@';
  }

  *ptr++ = ch;
  *ptr = 0;

  return buf;
}

#ifdef WITH_FUDGE
#include "yyfudge.c"
#endif
