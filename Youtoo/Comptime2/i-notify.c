/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module i-notify
 **  Copyright: See file i-notify.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_level1();
extern void initialize_module_i_param();
extern LispRef i_level1_bindings[];
extern LispRef stream_bindings[];
extern LispRef string_bindings[];
extern LispRef boot_bindings[];
extern LispRef stream2_bindings[];
extern LispRef i_param_bindings[];
extern LispRef format_bindings[];
extern LispRef dynamic_bindings[];

/* Module bindings with size 34 */
LispRef i_notify_bindings[34];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module i-notify */
void initialize_module_i_notify()
{
  if (is_initialized) return;
  initialize_module_i_level1();
  initialize_module_i_param();
  eul_fast_table_set(eul_modules,"i_notify",(LispRef) i_notify_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_322, sym_321, sym_320, sym_319, sym_318, sym_317, G00316, G00314, key_312, G00310, G00307, G00305, sym_303, G00302, sym_298, sym_295, sym_294, sym_293, sym_292, G00291, G00289;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 6 is_init: 0 index: 8 binding: anonymous */
  static const void *G00288[] = {I(a9,83,24,00),B(dynamic ,8),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_296, "[~a]", 4);
  eul_allocate_static_string(str_297, "", 0);
  eul_allocate_static_string(str_299, "~a[~a]", 6);
  eul_allocate_static_string(str_300, "*** ~a ~a: ~a", 13);
  /* Byte-vector with size: 64 is_init: 0 index: 18 binding: basic-warning */
  static const void *G00290[] = {I(43,fc,23,00),B(i_notify ,9),I(83,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_notify ,10),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(i_notify ,11),I(23,00,00,00),B(i_notify ,8),I(3b,00,1c,0f),I(23,00,00,00),B(i_notify ,10),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_notify ,12),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,44),I(1a,1b,82,02),I(23,00,00,00),B(i_notify ,13),I(1c,24,00,00),B(format ,2),I(3c,02,22,01),I(36,07,23,00),B(i_notify ,14),I(23,00,00,00),B(i_notify ,15),I(24,00,00,00),B(dynamic ,3),I(3c,01,24,00),B(i_param ,39),I(3c,01,1b,12),I(1b,44,04,86),I(36,1c,23,00),B(i_notify ,16),I(1f,03,1f,03),I(24,00,00,00),B(format ,2),I(3c,03,1b,20),I(04,1f,03,22),I(01,2a,23,00),B(i_notify ,17),I(1f,09,1f,04),I(1f,0a,24,00),B(format ,2),I(3c,04,24,00),B(i_notify ,7),I(24,00,00,00),B(stream2 ,9),I(1f,0c,1f,03),I(1f,0b,24,00),B(boot ,5),I(3c,05,83,24),B(dynamic ,8),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1b),I(45,0c,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 20 binding: notify */
  static const void *G00301[] = {I(a7,24,00,00),B(i_param ,20),I(12,23,00,00),B(i_notify ,19),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,1f),I(04,24,00,00),B(string ,11),I(3c,02,24,00),B(i_notify ,7),I(24,00,00,00),B(stream2 ,9),I(1f,04,1f,03),I(1f,07,24,00),B(boot ,5),I(3d,05,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 21 binding: notify0 */
  static const void *G00304[] = {I(a7,24,00,00),B(i_notify ,7),I(24,00,00,00),B(stream2 ,9),I(24,00,00,00),B(i_param ,51),I(1f,04,1f,04),I(24,00,00,00),B(boot ,5),I(3d,05,02,00)};

  eul_allocate_static_string(str_308, "WARNING", 7);
  /* Byte-vector with size: 15 is_init: 0 index: 23 binding: ct-warning */
  static const void *G00306[] = {I(43,fd,24,00),B(i_param ,40),I(83,14,1b,89),B(i_param ,40),I(2a,24,00,00),B(i_notify ,2),I(24,00,00,00),B(i_param ,58),I(23,00,00,00),B(i_notify ,22),I(1f,05,1f,05),I(24,00,00,00),B(boot ,5),I(3c,05,2a,1f),I(03,45,04,00)};

  eul_allocate_static_string(str_311, "ERROR", 5);
  /* Byte-vector with size: 38 is_init: 0 index: 26 binding: ct-serious-warning */
  static const void *G00309[] = {I(43,fd,24,00),B(i_param ,46),I(83,14,1b,89),B(i_param ,46),I(2a,24,00,00),B(i_param ,38),I(3c,00,2a,24),B(i_param ,10),I(12,1b,44,2c),I(24,00,00,00),B(i_notify ,2),I(24,00,00,00),B(i_param ,25),I(23,00,00,00),B(i_notify ,24),I(1f,06,1f,06),I(24,00,00,00),B(boot ,5),I(3c,05,2a,1f),I(04,36,47,86),I(24,00,00,00),B(i_param ,4),I(3c,01,24,00),B(format ,2),I(1f,05,1f,05),I(24,00,00,00),B(boot ,5),I(3c,03,23,00),B(i_notify ,12),I(24,00,00,00),B(dynamic ,3),I(3c,01,1d,1d),I(23,00,00,00),B(i_notify ,25),I(1f,03,24,00),B(boot ,13),I(3d,04,08,22),I(03,45,05,00)};

  /* Byte-vector with size: 15 is_init: 0 index: 27 binding: basic-notify */
  static const void *G00313[] = {I(43,fc,1d,12),I(1b,44,04,86),I(36,2e,24,00),B(format ,4),I(1f,05,1f,04),I(1f,04,24,00),B(boot ,5),I(3c,04,2a,1f),I(04,27,0a,24),B(stream ,8),I(3c,02,2a,1f),I(04,24,00,00),B(stream ,16),I(3d,01,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 49 is_init: 1 index: 0 binding: initialize-i-notify */
  static const void *G00315[] = {I(87,25,00,00),B(i_notify ,1),I(24,00,00,00),B(i_param ,1),I(3e,0b,24,00),B(i_param ,0),I(3c,00,21,01),I(24,00,00,00),B(i_level1 ,1),I(3e,0b,24,00),B(i_level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(i_notify ,28),I(23,00,00,00),B(i_notify ,27),I(3b,fc,25,00),B(i_notify ,7),I(23,00,00,00),B(i_notify ,29),I(23,00,00,00),B(i_notify ,26),I(3b,fd,25,00),B(i_notify ,6),I(23,00,00,00),B(i_notify ,30),I(23,00,00,00),B(i_notify ,23),I(3b,fd,25,00),B(i_notify ,5),I(23,00,00,00),B(i_notify ,31),I(23,00,00,00),B(i_notify ,21),I(3b,fe,25,00),B(i_notify ,4),I(23,00,00,00),B(i_notify ,32),I(23,00,00,00),B(i_notify ,20),I(3b,fe,25,00),B(i_notify ,3),I(23,00,00,00),B(i_notify ,33),I(23,00,00,00),B(i_notify ,18),I(3b,fc,25,00),B(i_notify ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00289,G00288);
  eul_intern_symbol(sym_292,"*pprint*");
  eul_intern_symbol(sym_293,"*clean-ups*");
  eul_intern_symbol(sym_294,"anonymous");
  eul_intern_symbol(sym_295,"*actual-module*");
  object_class(str_296) = eul_static_string_class;
  object_class(str_297) = eul_static_string_class;
  eul_intern_symbol(sym_298,"*encl-lambda*");
  object_class(str_299) = eul_static_string_class;
  object_class(str_300) = eul_static_string_class;
  eul_allocate_bytevector( G00291,G00290);
  eul_intern_symbol(sym_303,"*indent*");
  eul_allocate_bytevector( G00302,G00301);
  eul_allocate_bytevector( G00305,G00304);
  object_class(str_308) = eul_static_string_class;
  eul_allocate_bytevector( G00307,G00306);
  object_class(str_311) = eul_static_string_class;
  eul_intern_keyword(key_312,"ct-error-value");
  eul_allocate_bytevector( G00310,G00309);
  eul_allocate_bytevector( G00314,G00313);
  eul_intern_symbol(sym_317,"basic-notify");
  eul_intern_symbol(sym_318,"ct-serious-warning");
  eul_intern_symbol(sym_319,"ct-warning");
  eul_intern_symbol(sym_320,"notify0");
  eul_intern_symbol(sym_321,"notify");
  eul_intern_symbol(sym_322,"basic-warning");
  eul_allocate_bytevector( G00316,G00315);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 8; i++)
      i_notify_bindings[i] = eul_nil;
  }

  i_notify_bindings[ 8] = G00289;
  i_notify_bindings[ 9] = sym_292;
  i_notify_bindings[ 10] = sym_293;
  i_notify_bindings[ 11] = sym_294;
  i_notify_bindings[ 12] = sym_295;
  i_notify_bindings[ 13] = str_296;
  i_notify_bindings[ 14] = str_297;
  i_notify_bindings[ 15] = sym_298;
  i_notify_bindings[ 16] = str_299;
  i_notify_bindings[ 17] = str_300;
  i_notify_bindings[ 18] = G00291;
  i_notify_bindings[ 19] = sym_303;
  i_notify_bindings[ 20] = G00302;
  i_notify_bindings[ 21] = G00305;
  i_notify_bindings[ 22] = str_308;
  i_notify_bindings[ 23] = G00307;
  i_notify_bindings[ 24] = str_311;
  i_notify_bindings[ 25] = key_312;
  i_notify_bindings[ 26] = G00310;
  i_notify_bindings[ 27] = G00314;
  i_notify_bindings[ 1] = eul_nil;
  i_notify_bindings[ 28] = sym_317;
  i_notify_bindings[ 29] = sym_318;
  i_notify_bindings[ 30] = sym_319;
  i_notify_bindings[ 31] = sym_320;
  i_notify_bindings[ 32] = sym_321;
  i_notify_bindings[ 33] = sym_322;
  eul_allocate_lambda( i_notify_bindings[0], "initialize-i-notify", 0, G00316);

  }
}


/* eof */
