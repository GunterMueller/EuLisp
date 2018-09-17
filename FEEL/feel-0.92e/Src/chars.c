/* ******************************************************************** */
/*  chars.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Basic character, string and symbol functions				*/
/* ******************************************************************** */

/*
 * $Id: chars.c,v 1.3 1995/02/20 10:50:59 djb Exp $
 *
 * $Log: chars.c,v $
 * Revision 1.3  1995/02/20  10:50:59  djb
 * increased buffer in substring to 1k (this is really dodgy,
 * as soon as someone tries to substring anything bigger, it segvs)
 *
 * Revision 1.2  1994/04/26  13:52:07  jap
 * commented out definition of explode.  It doesn't work and it's not
 * part of the definition anyway.  Also decremented entry count of
 * symbols module.
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 1.8  1992/05/19  11:15:39  pab
 * string-ref (int) added
 *
 * Revision 1.7  1992/04/26  20:59:29  pab
 * symbol fixes(symbol-name)
 *
 * Revision 1.6  1992/01/29  13:38:48  pab
 * sysV fixes
 *
 * Revision 1.5  1992/01/09  22:28:45  pab
 * Fixed for low tag ints
 *
 * Revision 1.4  1991/12/22  15:13:55  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:44:28  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:04  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:30  pab
 * Initial revision
 *
 * Revision 1.4  1991/02/13  18:18:07  kjp
 * Symbol and string allocation corrections + RCS log header.
 *
 */

/*
 * Change Log:
 *   Version 1, May 1989
 *	Checked for GC protection - JPff
 */

#include <string.h>
#include <ctype.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"

#include "modboot.h"
#include "symboot.h"
#include "calls.h"

#define MAX_STRING 1024

/* These functions are taken from the CHARACTERS AND STRINGS section */

EUFUN_1( Fn_characterp, form)
{
  return (is_char(form) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_int2char, form)
{
  while (typeof(form)!=TYPE_INT)
    form = CallError(stacktop,
		  "Not an integer in integer-to-character",form,CONTINUABLE);
  return allocate_char(stackbase, intval(form));
}
EUFUN_CLOSE

EUFUN_1( Fn_char2int, form)
{
  while (!is_char(form))
    form = CallError(stacktop,
		 "Not a character in character-to-integer",form,CONTINUABLE);
  return allocate_integer(stackbase, (int)(form->CHAR).code);
}
EUFUN_CLOSE

/* ******************************** */
/* Latin-character-operators module */
/* ******************************** */

EUFUN_1( Fn_charalphap, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-alphabetic-p",form,CONTINUABLE);
  return (isalpha((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charnump, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-numeric-p",form,CONTINUABLE);
  return (isdigit((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charwhitep, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-whitespace-p",form,CONTINUABLE);
  return (isspace((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charpuncp, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-punctuation-p",form,CONTINUABLE);
  return (ispunct((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charotherp, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-other-p",form,CONTINUABLE);
  return (isgraph((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charupperp, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-upper-case-p",form,CONTINUABLE);
  return (isupper((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charlowerp, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not a character in char-lower-case-p",form,CONTINUABLE);
  return (islower((form->CHAR).code) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_charupper, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not an character in char-upcase",form,CONTINUABLE);
  return allocate_char(stackbase, toupper(intval(form)));
}
EUFUN_CLOSE

EUFUN_1( Fn_charlower, form)
{
  while (!is_char(form))
    form = CallError(stacktop,"Not an character in char-downcase",form,CONTINUABLE);
  return allocate_char(stackbase, tolower(intval(form)));
}
EUFUN_CLOSE

/* ************************************ */
/* Universal-character-operators module */
/* ************************************ */

EUFUN_2( Fn_chareq, form1, form2)
{
  while (!is_char(form1))
    form1 = CallError(stacktop,"Not a character in char-equal",form1,CONTINUABLE);
  while (!is_char(form2))
    form2 = CallError(stacktop,"Not a character in char-equal",form2,CONTINUABLE);
  return ((form1->CHAR).code == (form2->CHAR).code ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_charls, form1, form2)
{
  while (!is_char(form1))
    form1 = CallError(stacktop,"Not a character in char<",form1,CONTINUABLE);
  while (!is_char(form2))
    form2 = CallError(stacktop,"Not a character in char<",form2,CONTINUABLE);
  return ((form1->CHAR).code < (form2->CHAR).code ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_chargt, form1, form2)
{
  while (!is_char(form1))
    form1 = CallError(stacktop,"Not a character in char>",form1,CONTINUABLE);
  while (!is_char(form2))
    form2 = CallError(stacktop,"Not a character in char>",form2,CONTINUABLE);
  return ((form1->CHAR).code > (form2->CHAR).code ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_charle, form1, form2)
{
  while (!is_char(form1))
    form1 = CallError(stacktop,"Not a character in char<=",form1,CONTINUABLE);
  while (!is_char(form2))
    form2 = CallError(stacktop,"Not a character in char<=",form2,CONTINUABLE);
  return ((form1->CHAR).code <= (form2->CHAR).code ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_charge, form1, form2)
{
  while (!is_char(form1))
    form1 = CallError(stacktop,"Not a character in char>=",form1,CONTINUABLE);
  while (!is_char(form2))
    form2 = CallError(stacktop,"Not a character in char>=",form2,CONTINUABLE);
  return ((form1->CHAR).code >= (form2->CHAR).code ? lisptrue : nil);
}
EUFUN_CLOSE

/* STRINGS */

EUFUN_1( Fn_stringp, form)
{
  return (is_string(form) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_string_copy, form)
{
  LispObject ans;
  while (!is_string(form)) 
    form = CallError(stacktop,"Not a string in string-copy",form,CONTINUABLE);
  ans = allocate_string(stackbase,
			stringof(form),strlen(stringof(form)));
  return ans;
}
EUFUN_CLOSE

EUFUN_1( Fn_string_length, form)
{
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in string-length",form,CONTINUABLE);
  return allocate_integer(stackbase, strlen(stringof(form)));
}
EUFUN_CLOSE

EUFUN_2( Fn_sref, form, off)
{
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in string-ref",form,CONTINUABLE);
  while (typeof(off)!=TYPE_INT)
    off = CallError(stacktop,"Not an integer in string-ref",form,CONTINUABLE);
  return allocate_char(stackbase, (stringof(form))[intval(off)]);
}
EUFUN_CLOSE

EUFUN_3( Fn_sref_setter, form, off, ch)
{
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in set-string-ref",form,CONTINUABLE);
  while (typeof(off)!=TYPE_INT)
    off = CallError(stacktop,"Not an integer in set-string-ref",form,CONTINUABLE);
  while (!is_char(ch))
    off = CallError(stacktop,"Not an character in set-string-ref",form,CONTINUABLE);
  stringof(form)[intval(off)] = (ch->CHAR).code;
  return nil;
}
EUFUN_CLOSE

EUFUN_2( Fn_int_sref, form, off)
{
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in i-string-ref",form,CONTINUABLE);
  while (typeof(off)!=TYPE_INT)
    off = CallError(stacktop,"Not an integer in i-string-ref",form,CONTINUABLE);
  return allocate_integer(stackbase, (stringof(form))[intval(off)]);
}
EUFUN_CLOSE

EUFUN_3( Fn_int_sref_setter, form, off, val)
{
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in set-i-string-ref",form,CONTINUABLE);
  while (typeof(off)!=TYPE_INT)
    off = CallError(stacktop,"Not an integer in set-i-string-ref",form,CONTINUABLE);
  while (!is_fixnum(val))
    off = CallError(stacktop,"Not a fixnum in set-i-string-ref",form,CONTINUABLE);
  stringof(form)[intval(off)] = (char) intval(val);
  return nil;
}
EUFUN_CLOSE

EUFUN_3( Fn_substring, str, start, end)
{
  int len;
  int istart;
  int iend;
  while (!is_string(str))
    str = CallError(stacktop,"Not a string in substring",str,CONTINUABLE);
  while (typeof(start)!=TYPE_INT)
    start = CallError(stacktop,"Not an integer in substring",start,CONTINUABLE);
  while (typeof(end)!=TYPE_INT)
    end = CallError(stacktop,"Not an integer in substring",end,CONTINUABLE);
  len = strlen(stringof(str));
  istart = intval(start);
  iend = intval(end);
  if (istart<0 || istart>=len || iend<0 || iend>=len || iend<istart) {
    CallError(stacktop,"Illegal args to substring",start,NONCONTINUABLE);
  }
  {
    char buff[MAX_STRING];
    for (len = 0 ; istart<=iend; istart++, len++)
      buff[len] = (stringof(str))[istart];
    buff[len] = '\0';
    return allocate_string(stackbase, buff,len);
  }
}
EUFUN_CLOSE

EUFUN_2( Fn_string_append, str1, str2)
{
  int len;
  LispObject ans;

  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string-append",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string-append",str2,CONTINUABLE);
  len = strlen(stringof(str1));
  ans=allocate_string(stacktop,"",len+strlen(stringof(str2)));
  strcpy(stringof(ans),stringof(str1));
  strcpy(stringof(ans)+len,stringof(str2));
  return ans;
}
EUFUN_CLOSE

/* **  String-operators module ** */
EUFUN_1( Fn_string_list, form)
{
  LispObject ans=nil;
  while (!is_string(form))
    form = CallError(stacktop,"Not a string in string-to-list",form,CONTINUABLE);
  {
    char *str = stringof(form);
    int n;
    for (n= strlen(str)-1; n>=0; n--) {
      LispObject x;
      STACK_TMP(ans);
      x = allocate_char(stacktop, str[n]);
      UNSTACK_TMP(ans);
      ARG_0(stacktop) = x;
      ARG_1(stacktop) = ans;
      ans = Fn_cons(stacktop);
    }
  }
  return ans;
}
EUFUN_CLOSE


EUFUN_2( Fn_string_equal, str1, str2)
{
  char *ss1;
  char *ss2;
  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string-equal",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string-equal",str2,CONTINUABLE);
  ss1 = stringof(str1);
  ss2 = stringof(str2);
  return (strcmp(ss1,ss2)==0 ? lisptrue: nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_string_lt, str1, str2)
{
  char *ss1;
  char *ss2;
  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string-lt",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string-lt",str2,CONTINUABLE);
  ss1 = stringof(str1);
  ss2 = stringof(str2);
  return (strcmp(ss1,ss2)<0 ? lisptrue: nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_string_gt, str1, str2)
{
  char *ss1;
  char *ss2;
  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string-gt",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string-gt",str2,CONTINUABLE);
  ss1 = stringof(str1);
  ss2 = stringof(str2);
  return (strcmp(ss1,ss2)>0 ? lisptrue: nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_string_le, str1, str2)
{
  char *ss1;
  char *ss2;
  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string-<=",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string-<=",str2,CONTINUABLE);
  ss1 = stringof(str1);
  ss2 = stringof(str2);
  return (strcmp(ss1,ss2)<=0 ? lisptrue: nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_string_ge, str1, str2)
{
  char *ss1;
  char *ss2;
  while (!is_string(str1))
    str1 = CallError(stacktop,"Not a string in string->=",str1,CONTINUABLE);
  while (!is_string(str2))
    str2 = CallError(stacktop,"Not a string in string->=",str2,CONTINUABLE);
  ss1 = stringof(str1);
  ss2 = stringof(str2);
  return (strcmp(ss1,ss2)>=0 ? lisptrue: nil);
}
EUFUN_CLOSE

/* SYMBOLS */

EUFUN_1( Fn_symbolp, form)
{
  return (is_symbol(form) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_make_symbol, str)
{
  while (!is_string(str))
    str = CallError(stacktop,"Not a string in make-symbol",str,CONTINUABLE);
  return (LispObject) get_symbol_by_copying(stackbase, stringof(str));
}
EUFUN_CLOSE

EUFUN_1( Fn_symbolname, form)
{
  while (!is_symbol(form))
    form = CallError(stacktop,"Not symbol in symbol-name",form,CONTINUABLE);
  return allocate_string(stackbase, stringof((form->SYMBOL).pname),strlen(stringof((form->SYMBOL).pname)));
}
EUFUN_CLOSE

EUFUN_1( Fn_symbolvalue, form)
{
  while (!is_symbol(form))
    form = CallError(stacktop,"symbol-value: non symbol",form,CONTINUABLE);
  if (form->SYMBOL.gvalue == NULL)
    CallError(stacktop,"symbol-value: globally unbound",form,NONCONTINUABLE);
  return (form->SYMBOL).gvalue;
}
EUFUN_CLOSE
  
EUFUN_2( Fn_symbolvalue_update, form, new)
{
  while (!is_symbol(form))
    form = CallError(stacktop,"symbol-value: non-symbol",form,CONTINUABLE);
  (form->SYMBOL).gvalue = new;
  return nil;
}
EUFUN_CLOSE
  
EUFUN_1( Fn_symbolglobal, form)
{
  while (!is_symbol(form))
    form = CallError(stacktop,"Not symbol in symbol-global",form,CONTINUABLE);
  return (form->SYMBOL).gvalue;
}
EUFUN_CLOSE
  
EUFUN_2( Fn_symbolglobal_update, form, new)
{
  while (!is_symbol(form))
    form = CallError(stacktop,"Not symbol in symbol-global",form,CONTINUABLE);
  (form->SYMBOL).gvalue = new;
  return nil;
}
EUFUN_CLOSE

/*
 * EUFUN_1( Fn_explode, sym)
 * {
 *   LispObject list,last;
 *   char *name;
 *   char temp[5];
 * 
 *   if (!is_symbol(sym))
 *     CallError(stacktop,"explode: not a symbol",sym,NONCONTINUABLE);
 * 
 *   name = stringof(sym->SYMBOL.pname);
 *   last = list = nil;
 * 
 *   while (*name != '\0') {
 *     LispObject symbit;
 * 
 *     temp[0] = *name; temp[1] = '\0';
 * 
 *     symbit = get_symbol_by_copying(stackbase, temp);
 * 
 *     if (last == nil) {
 *       ARG_0(stacktop) = symbit;
 *       ARG_1(stacktop) = nil;
 *       list = Fn_cons(stacktop);
 *       last = list;
 *       STACK_TMP(list);
 *     }
 *     else {
 *       LispObject x;
 *       STACK_TMP(last);
 *       ARG_0(stacktop) = symbit;
 *       ARG_1(stacktop) = last;
 *       x = Fn_cons(stacktop);
 *       UNSTACK_TMP(last);
 *       CDR(last) = x;
 *       last = x;
 *     }
 * 
 *     ++name;
 *   }
 *   UNSTACK_TMP(list);
 *   return(list);
 * }
 * EUFUN_CLOSE
 */

EUFUN_2( Fn_make_string, n, rest)
{
  LispObject ch,str;
  int i;
  char cch;

  if (consp(rest)) {
    ch = CAR(rest);

    if (!is_char(ch))
      CallError(stacktop,"make-string: bad character",ch,NONCONTINUABLE);

    cch = (char) (ch->CHAR.code);
  }
  else cch = ' ';

  if (!is_fixnum(n))
    CallError(stacktop,"make-string: bad length",n,NONCONTINUABLE);

  if (intval(n) < 0)
    CallError(stacktop,"make-string: bad length",n,NONCONTINUABLE);

  str = (LispObject) allocate_string(stackbase, "",intval(n)+1);

  for (i=0; i<intval(n); ++i) 
    stringof(str)[i] = cch;

  stringof(str)[i] = '\0';

  return(str);
}
EUFUN_CLOSE

static SYSTEM_GLOBAL(int,gensym_counter);

EUFUN_0( Fn_gensym)
{
  char buffer[100];

  sprintf(buffer,"G%05d\0",SYSTEM_GLOBAL_VALUE(gensym_counter));
  ++SYSTEM_GLOBAL_VALUE(gensym_counter);

  return((LispObject) get_symbol_by_copying(stackbase, buffer));
}
EUFUN_CLOSE

/* *************************************************************** */
/* This is not part of the real Eulisp definition                  */  
/* *************************************************************** */

EUFUN_1( Fn_mapoblist, fn)
{	/* And would not work in any case --- pab */
  LispObject ob = (LispObject) (ObList);

#ifdef nope /* Mon May 17 11:34:25 1993 */
/**/  while (ob!=NULL) {
/**/    STACK_TMP(ob);
/**/    EUCALL_2(apply1, fn, ob);
/**/    UNSTACK_TMP(ob);
/**/    EUCALL_1
/**/    ob = (LispObject) (ARG_1(stackbase)->SYMBOL).left;
/**/  }
#endif /* nope Mon May 17 11:34:25 1993 */

  return nil;
}
EUFUN_CLOSE

/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

#define STRINGS_ENTRIES 16
MODULE Module_strings;
LispObject Module_strings_values[STRINGS_ENTRIES];

#define CHARACTERS_ENTRIES 17
MODULE Module_characters;
LispObject Module_characters_values[CHARACTERS_ENTRIES];

#define SYMBOLS_ENTRIES 9
MODULE Module_symbols;
LispObject Module_symbols_values[SYMBOLS_ENTRIES];

void initialise_chars(LispObject *stacktop)
{
  LispObject fun,upd;

  open_module(stacktop,
	      &Module_characters,
	      Module_characters_values,
	      "characters",
	      CHARACTERS_ENTRIES);

  (void) make_module_function(stacktop,"characterp",Fn_characterp,1);
  (void) make_module_function(stacktop,"integer-to-character",Fn_int2char,1);
  (void) make_module_function(stacktop,"character-to-integer",Fn_char2int,1);
  (void) make_module_function(stacktop,"char-alphabetic-p",Fn_charalphap,1);
  (void) make_module_function(stacktop,"char-numeric-p",Fn_charnump,1);
  (void) make_module_function(stacktop,"char-whitespace-p",Fn_charwhitep,1);
  (void) make_module_function(stacktop,"char-punctuation-p",Fn_charpuncp,1);
  (void) make_module_function(stacktop,"char-other-p",Fn_charotherp,1);
  (void) make_module_function(stacktop,"char-upper-case-p",Fn_charupperp,1);
  (void) make_module_function(stacktop,"char-lower-case-p",Fn_charlowerp,1);
  (void) make_module_function(stacktop,"char-upcase",Fn_charupper,1);
  (void) make_module_function(stacktop,"char-downcase",Fn_charlower,1);
  (void) make_module_function(stacktop,"char-equal",Fn_chareq,2);
  (void) make_module_function(stacktop,"char<",Fn_charls,2);
  (void) make_module_function(stacktop,"char>",Fn_chargt,2);
  (void) make_module_function(stacktop,"char<=",Fn_charle,2);
  (void) make_module_function(stacktop,"char>=",Fn_charge,2);

  close_module();

  open_module(stacktop,
	      &Module_strings,
	      Module_strings_values,
	      "strings",
	      STRINGS_ENTRIES);

  (void) make_module_function(stacktop,"make-string",Fn_make_string,-2);
  (void) make_module_function(stacktop,"stringp",Fn_stringp,1);
  (void) make_module_function(stacktop,"string-length",Fn_string_length,1);
  fun = make_module_function(stacktop,"string-ref",Fn_sref,2);
  STACK_TMP(fun);
  upd = make_module_function(stacktop,"string-ref-updator",Fn_sref_setter,3);
  UNSTACK_TMP(fun);
  set_anon_associate(stacktop,fun,upd);
  fun = make_module_function(stacktop,"i-string-ref",Fn_int_sref,2);
  STACK_TMP(fun);
  upd = make_module_function(stacktop,"i-string-ref-updator",Fn_int_sref_setter,3);
  UNSTACK_TMP(fun);
  (void) make_module_function(stacktop,"string-copy",Fn_string_copy,1);
  (void) make_module_function(stacktop,"string-to-list",Fn_string_list,1);
  (void) make_module_function(stacktop,"string-equal",Fn_string_equal,2);
  (void) make_module_function(stacktop,"string-lt",Fn_string_lt,2);
  (void) make_module_function(stacktop,"string-gt",Fn_string_gt,2);
  (void) make_module_function(stacktop,"substring",Fn_substring,3);
  (void) make_module_function(stacktop,"string-append",Fn_string_append,2);
  (void) make_module_function(stacktop,"string-<=",Fn_string_le,2);
  (void) make_module_function(stacktop,"string->=",Fn_string_ge,2);

  close_module();

  open_module(stacktop,
	      &Module_symbols,
	      Module_symbols_values,
	      "symbols",
	      SYMBOLS_ENTRIES);

  (void) make_module_function(stacktop,"symbolp",Fn_symbolp,1);
  (void) make_module_function(stacktop,"make-symbol",Fn_make_symbol,1);
  (void) make_module_function(stacktop,"symbol-name",Fn_symbolname,1);
  fun = make_module_function(stacktop,"symbol-value",Fn_symbolvalue,1);
  STACK_TMP(fun);
  upd = make_module_function(stacktop,"symbol-value-updator",Fn_symbolvalue_update,2);
  UNSTACK_TMP(fun);
  set_anon_associate(stacktop,fun,upd);
  fun = make_module_function(stacktop,"symbol-global",Fn_symbolglobal,1);
  STACK_TMP(fun);
  upd = make_module_function(stacktop,"symbol-global-updator",Fn_symbolglobal_update,2);
  UNSTACK_TMP(fun);
  set_anon_associate(stacktop,fun,upd);
  (void) make_module_function(stacktop,"mapoblist",Fn_mapoblist,1);
  
/*
 *   (void) make_module_function(stacktop,"explode",Fn_explode,1);
 */

  SYSTEM_INITIALISE_GLOBAL(int,gensym_counter,0);
  (void) make_module_function(stacktop,"gensym",Fn_gensym,0);

  close_module();
}

