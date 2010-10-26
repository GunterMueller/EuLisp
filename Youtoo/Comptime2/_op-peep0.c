/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module _op-peep0
 **  Copyright: See file _op-peep0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level1();
extern LispRef level1_bindings[];
extern LispRef boot_bindings[];
extern LispRef collect_bindings[];

/* Module bindings with size 18 */
LispRef _op_peep0_bindings[18];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module _op-peep0 */
void initialize_module__op_peep0()
{
  if (is_initialized) return;
  initialize_module_level1();
  eul_fast_table_set(eul_modules,"_op_peep0",(LispRef) _op_peep0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_100, G0099, sym_97, G0096, sym_94, sym_93, G0092, sym_90, G0089, sym_87, sym_86, G0085, sym_83, sym_82, G0081;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 8 is_init: 0 index: 6 binding: anonymous */
  static const void *G0080[] = {I(aa,1b,10,1b),I(86,0f,23,00),B(_op_peep0 ,4),I(1c,0f,1f,03),I(11,1c,1c,0f),I(23,00,00,00),B(_op_peep0 ,5),I(1c,0f,45,06)};

  /* Byte-vector with size: 9 is_init: 0 index: 9 binding: anonymous */
  static const void *G0084[] = {I(aa,1b,7c,12),I(1b,44,04,1b),I(36,08,1c,23),B(_op_peep0 ,7),I(50,1b,44,0b),I(23,00,00,00),B(_op_peep0 ,8),I(36,02,1d,45),I(03,00,00,00)};

  /* Byte-vector with size: 16 is_init: 0 index: 11 binding: anonymous */
  static const void *G0088[] = {I(aa,1b,12,1b),I(44,04,86,36),I(35,1c,10,1b),I(11,23,00,00),B(_op_peep0 ,10),I(23,00,00,00),B(_op_peep0 ,9),I(3b,01,1c,24),B(collect ,2),I(3c,02,1f,04),I(11,1b,47,00),I(00,3c,01,1d),I(1c,24,00,00),B(boot ,23),I(3d,02,07,22),I(05,45,02,00)};

  /* Byte-vector with size: 43 is_init: 0 index: 14 binding: guarded-rule */
  static const void *G0091[] = {I(43,03,46,01),I(86,1b,48,00),I(00,23,00,00),B(_op_peep0 ,10),I(23,00,00,00),B(_op_peep0 ,11),I(3b,01,48,00),I(00,1f,03,86),I(0f,23,00,00),B(_op_peep0 ,4),I(1c,0f,1f,04),I(44,1d,1f,05),I(47,00,00,3c),I(01,1f,05,86),I(0f,1c,1c,0f),I(23,00,00,00),B(_op_peep0 ,12),I(1c,0f,22,03),I(36,02,86,1f),I(06,47,00,00),I(3c,01,23,00),B(_op_peep0 ,10),I(23,00,00,00),B(_op_peep0 ,6),I(3b,01,1f,06),I(24,00,00,00),B(collect ,2),I(3c,02,23,00),B(_op_peep0 ,5),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_op_peep0 ,12),I(1c,0f,1f,0c),I(24,00,00,00),B(collect ,9),I(3c,01,1b,86),I(0f,1d,1c,0f),I(1f,09,1c,0f),I(1f,0b,1c,0f),I(23,00,00,00),B(_op_peep0 ,13),I(1c,0f,45,12)};

  /* Byte-vector with size: 5 is_init: 0 index: 16 binding: simple-rule */
  static const void *G0095[] = {I(ab,86,0f,86),I(1c,0f,1d,1c),I(0f,23,00,00),B(_op_peep0 ,15),I(1c,0f,45,04)};

  /* Byte-vector with size: 20 is_init: 1 index: 0 binding: initialize-_op-peep0 */
  static const void *G0098[] = {I(87,25,00,00),B(_op_peep0 ,1),I(24,00,00,00),B(level1 ,1),I(3e,0b,24,00),B(level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(_op_peep0 ,17),I(23,00,00,00),B(_op_peep0 ,16),I(3b,02,25,00),B(_op_peep0 ,3),I(23,00,00,00),B(_op_peep0 ,15),I(23,00,00,00),B(_op_peep0 ,14),I(3b,03,25,00),B(_op_peep0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_82,"quote");
  eul_intern_symbol(sym_83,"list");
  eul_allocate_bytevector( G0081,G0080);
  eul_intern_symbol(sym_86,"*");
  eul_intern_symbol(sym_87,"*no-variable*");
  eul_allocate_bytevector( G0085,G0084);
  eul_intern_symbol(sym_90,"anonymous");
  eul_allocate_bytevector( G0089,G0088);
  eul_intern_symbol(sym_93,"lambda");
  eul_intern_symbol(sym_94,"add-rule");
  eul_allocate_bytevector( G0092,G0091);
  eul_intern_symbol(sym_97,"guarded-rule");
  eul_allocate_bytevector( G0096,G0095);
  eul_intern_symbol(sym_100,"simple-rule");
  eul_allocate_bytevector( G0099,G0098);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 4; i++)
      _op_peep0_bindings[i] = eul_nil;
  }

  _op_peep0_bindings[ 4] = sym_82;
  _op_peep0_bindings[ 5] = sym_83;
  _op_peep0_bindings[ 6] = G0081;
  _op_peep0_bindings[ 7] = sym_86;
  _op_peep0_bindings[ 8] = sym_87;
  _op_peep0_bindings[ 9] = G0085;
  _op_peep0_bindings[ 10] = sym_90;
  _op_peep0_bindings[ 11] = G0089;
  _op_peep0_bindings[ 12] = sym_93;
  _op_peep0_bindings[ 13] = sym_94;
  _op_peep0_bindings[ 14] = G0092;
  _op_peep0_bindings[ 15] = sym_97;
  _op_peep0_bindings[ 16] = G0096;
  _op_peep0_bindings[ 1] = eul_nil;
  _op_peep0_bindings[ 17] = sym_100;
  eul_allocate_lambda( _op_peep0_bindings[0], "initialize-_op-peep0", 0, G0099);

  }
}


/* eof */
