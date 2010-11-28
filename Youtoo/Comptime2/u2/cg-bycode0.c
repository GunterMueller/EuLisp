/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module cg-bycode0
 **  Copyright: See file cg-bycode0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level_1();
extern LispRef level_1_bindings[];

/* Module bindings with size 18 */
LispRef cg_bycode0_bindings[18];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module cg-bycode0 */
void initialize_module_cg_bycode0()
{
  if (is_initialized) return;
  initialize_module_level_1();
  eul_fast_table_set(eul_modules,"cg_bycode0",(LispRef) cg_bycode0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1263, sym_1262, G001261, sym_1259, sym_1258, key_1257, key_1256, key_1255, key_1254, sym_1253, G001252, sym_1250, sym_1249, sym_1248, G001247;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 11 is_init: 0 index: 7 binding: def-register */
  static const void *G001246[] = {I(ab,23,00,00),B(cg_bycode0 ,4),I(86,0f,23,00),B(cg_bycode0 ,5),I(1c,0f,1f,03),I(86,0f,23,00),B(cg_bycode0 ,6),I(1c,0f,1f,04),I(86,0f,1c,1c),I(0f,1f,04,1c),I(0f,45,08,00)};

  /* Byte-vector with size: 36 is_init: 0 index: 15 binding: def-bytecode */
  static const void *G001251[] = {I(43,fc,23,00),B(cg_bycode0 ,8),I(86,0f,23,00),B(cg_bycode0 ,5),I(1c,0f,1f,05),I(86,0f,23,00),B(cg_bycode0 ,6),I(1c,0f,1f,07),I(86,0f,23,00),B(cg_bycode0 ,6),I(1c,0f,1f,08),I(86,0f,23,00),B(cg_bycode0 ,6),I(1c,0f,1f,08),I(86,0f,23,00),B(cg_bycode0 ,6),I(1c,0f,1b,86),I(0f,23,00,00),B(cg_bycode0 ,9),I(1c,0f,1f,0d),I(1c,0f,23,00),B(cg_bycode0 ,10),I(1c,0f,1f,06),I(1c,0f,23,00),B(cg_bycode0 ,11),I(1c,0f,1f,0a),I(1c,0f,23,00),B(cg_bycode0 ,12),I(1c,0f,23,00),B(cg_bycode0 ,13),I(1c,0f,23,00),B(cg_bycode0 ,14),I(1c,0f,1b,86),I(0f,1f,11,1c),I(0f,1f,14,1c),I(0f,45,1a,00)};

  /* Byte-vector with size: 20 is_init: 1 index: 0 binding: initialize-cg-bycode0 */
  static const void *G001260[] = {I(87,25,00,00),B(cg_bycode0 ,1),I(24,00,00,00),B(level_1 ,1),I(3e,0b,24,00),B(level_1 ,0),I(3c,00,21,01),I(23,00,00,00),B(cg_bycode0 ,16),I(23,00,00,00),B(cg_bycode0 ,15),I(3b,fc,25,00),B(cg_bycode0 ,3),I(23,00,00,00),B(cg_bycode0 ,17),I(23,00,00,00),B(cg_bycode0 ,7),I(3b,02,25,00),B(cg_bycode0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_1248,"get-register");
  eul_intern_symbol(sym_1249,"setter");
  eul_intern_symbol(sym_1250,"quote");
  eul_allocate_bytevector( G001247,G001246);
  eul_intern_symbol(sym_1253,"get-bytecode");
  eul_intern_keyword(key_1254,"properties");
  eul_intern_keyword(key_1255,"code");
  eul_intern_keyword(key_1256,"args");
  eul_intern_keyword(key_1257,"name");
  eul_intern_symbol(sym_1258,"<bytecode>");
  eul_intern_symbol(sym_1259,"make");
  eul_allocate_bytevector( G001252,G001251);
  eul_intern_symbol(sym_1262,"def-bytecode");
  eul_intern_symbol(sym_1263,"def-register");
  eul_allocate_bytevector( G001261,G001260);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 4; i++)
      cg_bycode0_bindings[i] = eul_nil;
  }

  cg_bycode0_bindings[ 4] = sym_1248;
  cg_bycode0_bindings[ 5] = sym_1249;
  cg_bycode0_bindings[ 6] = sym_1250;
  cg_bycode0_bindings[ 7] = G001247;
  cg_bycode0_bindings[ 8] = sym_1253;
  cg_bycode0_bindings[ 9] = key_1254;
  cg_bycode0_bindings[ 10] = key_1255;
  cg_bycode0_bindings[ 11] = key_1256;
  cg_bycode0_bindings[ 12] = key_1257;
  cg_bycode0_bindings[ 13] = sym_1258;
  cg_bycode0_bindings[ 14] = sym_1259;
  cg_bycode0_bindings[ 15] = G001252;
  cg_bycode0_bindings[ 1] = eul_nil;
  cg_bycode0_bindings[ 16] = sym_1262;
  cg_bycode0_bindings[ 17] = sym_1263;
  eul_allocate_lambda( cg_bycode0_bindings[0], "initialize-cg-bycode0", 0, G001261);

  }
}


/* eof */