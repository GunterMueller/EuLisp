/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module _mop-meth0
 **  Copyright: See file _mop-meth0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level1();
extern LispRef level1_bindings[];
extern LispRef collect_bindings[];
extern LispRef mop_key_bindings[];

/* Module bindings with size 46 */
LispRef _mop_meth0_bindings[46];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module _mop-meth0 */
void initialize_module__mop_meth0()
{
  if (is_initialized) return;
  initialize_module_level1();
  eul_fast_table_set(eul_modules,"_mop_meth0",(LispRef) _mop_meth0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_634, sym_633, sym_632, sym_631, sym_630, sym_629, sym_628, G00627, sym_625, sym_624, G00623, G00621, G00619, G00617, sym_615, sym_614, G00613, sym_611, sym_610, sym_609, sym_608, sym_607, sym_605, G00603, G00601, sym_599, sym_598, sym_597, sym_596, sym_595, key_594, G00592, G00590;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 20 is_init: 0 index: 11 binding: defmethod-args */
  static const void *G00589[] = {I(aa,1b,12,1b),I(44,04,86,36),I(43,1c,7a,12),I(1b,44,04,1d),I(36,38,1d,10),I(7a,12,1b,44),I(19,1f,03,10),I(1f,04,11,1b),I(24,00,00,00),B(_mop_meth0 ,2),I(3c,01,1d,1c),I(0f,22,03,36),I(17,1f,03,72),I(1f,04,11,1b),I(24,00,00,00),B(_mop_meth0 ,2),I(3c,01,1d,1c),I(0f,22,03,22),I(01,22,01,45),I(02,00,00,00)};

  eul_allocate_static_cons(cons_593, NULL, NULL);
  /* Byte-vector with size: 39 is_init: 0 index: 19 binding: method-lambda */
  static const void *G00591[] = {I(a8,1b,24,00),B(_mop_meth0 ,9),I(3c,01,1c,24),B(_mop_meth0 ,8),I(3c,01,1d,24),B(_mop_meth0 ,7),I(3c,01,1d,23),B(_mop_meth0 ,13),I(24,00,00,00),B(mop_key ,3),I(3c,02,23,00),B(_mop_meth0 ,12),I(1f,04,23,00),B(_mop_meth0 ,14),I(24,00,00,00),B(mop_key ,2),I(3c,03,1f,03),I(24,00,00,00),B(_mop_meth0 ,2),I(3c,01,1f,04),I(24,00,00,00),B(_mop_meth0 ,4),I(3c,01,1b,24),B(collect ,9),I(3c,01,1b,1d),I(0f,23,00,00),B(_mop_meth0 ,15),I(1c,0f,1f,04),I(1f,08,0f,23),B(_mop_meth0 ,16),I(1c,0f,23,00),B(_mop_meth0 ,17),I(1f,09,0f,1b),I(86,0f,1d,1c),I(0f,1f,05,1c),I(0f,1f,0b,1c),I(0f,23,00,00),B(_mop_meth0 ,18),I(1c,0f,45,12)};

  /* Byte-vector with size: 16 is_init: 0 index: 20 binding: defmethod-domain */
  static const void *G00600[] = {I(aa,1b,7a,12),I(1b,44,04,86),I(36,34,1c,10),I(7a,12,1b,44),I(15,1d,11,1b),I(24,00,00,00),B(_mop_meth0 ,4),I(3c,01,86,1c),I(0f,22,02,36),I(17,1d,10,1b),I(73,1f,04,11),I(1b,24,00,00),B(_mop_meth0 ,4),I(3c,01,1d,1c),I(0f,22,04,22),I(01,45,02,00)};

  eul_allocate_static_cons(cons_604, NULL, NULL);
  eul_allocate_static_cons(cons_606, NULL, NULL);
  /* Byte-vector with size: 60 is_init: 0 index: 29 binding: defmethod */
  static const void *G00602[] = {I(a7,23,00,00),B(_mop_meth0 ,22),I(1c,24,00,00),B(_mop_meth0 ,9),I(3c,01,1d,24),B(_mop_meth0 ,8),I(3c,01,1f,03),I(24,00,00,00),B(_mop_meth0 ,7),I(3c,01,1d,23),B(_mop_meth0 ,23),I(24,00,00,00),B(mop_key ,3),I(3c,02,23,00),B(_mop_meth0 ,12),I(1f,04,1f,06),I(24,00,00,00),B(mop_key ,2),I(3c,03,1f,03),I(24,00,00,00),B(_mop_meth0 ,2),I(3c,01,1f,04),I(24,00,00,00),B(_mop_meth0 ,4),I(3c,01,1d,1f),I(08,50,1b,44),I(12,1f,0a,86),I(0f,23,00,00),B(_mop_meth0 ,24),I(1c,0f,22,01),I(36,03,1f,03),I(1d,24,00,00),B(collect ,9),I(3c,01,1b,1f),I(04,0f,23,00),B(_mop_meth0 ,15),I(1c,0f,1f,06),I(1f,0a,0f,1f),I(0f,1c,0f,23),B(_mop_meth0 ,25),I(1c,0f,23,00),B(_mop_meth0 ,17),I(1f,0c,0f,1f),I(12,86,0f,23),B(_mop_meth0 ,26),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_mop_meth0 ,27),I(1c,0f,1b,86),I(0f,1f,07,1c),I(0f,1f,0b,1c),I(0f,1f,0f,1c),I(0f,23,00,00),B(_mop_meth0 ,18),I(1c,0f,1b,86),I(0f,1f,1d,1c),I(0f,23,00,00),B(_mop_meth0 ,28),I(1c,0f,45,1f)};

  /* Byte-vector with size: 7 is_init: 0 index: 32 binding: method-function-lambda */
  static const void *G00612[] = {I(a7,23,00,00),B(_mop_meth0 ,30),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_mop_meth0 ,31),I(1c,0f,45,05)};

  /* Byte-vector with size: 8 is_init: 0 index: 33 binding: defmethod-body */
  static const void *G00616[] = {I(aa,1b,10,7a),I(12,1b,44,14),I(1c,11,1b,11),I(24,00,00,00),B(_mop_meth0 ,7),I(3d,01,03,22),I(01,36,03,1c),I(11,45,02,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 34 binding: defmethod-sig */
  static const void *G00618[] = {I(aa,1b,10,7a),I(12,1b,44,14),I(1c,11,1b,11),I(24,00,00,00),B(_mop_meth0 ,8),I(3d,01,03,22),I(01,36,03,1c),I(10,45,02,00)};

  /* Byte-vector with size: 12 is_init: 0 index: 35 binding: defmethod-keywords */
  static const void *G00620[] = {I(aa,1b,10,7a),I(12,1b,44,23),I(1c,10,1d,11),I(1b,10,1f,04),I(11,1b,11,1b),I(24,00,00,00),B(_mop_meth0 ,9),I(3c,01,1f,03),I(1c,0f,1f,06),I(1c,0f,22,07),I(36,02,86,45),I(02,00,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 38 binding: named-method-function-lambda */
  static const void *G00622[] = {I(43,fd,1d,86),I(0f,23,00,00),B(_mop_meth0 ,36),I(1c,0f,23,00),B(_mop_meth0 ,30),I(1f,03,0f,1b),I(86,0f,1f,05),I(1c,0f,1f,03),I(1c,0f,23,00),B(_mop_meth0 ,37),I(1c,0f,45,09)};

  /* Byte-vector with size: 62 is_init: 1 index: 0 binding: initialize-_mop-meth0 */
  static const void *G00626[] = {I(87,25,00,00),B(_mop_meth0 ,1),I(24,00,00,00),B(level1 ,1),I(3e,0b,24,00),B(level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(_mop_meth0 ,25),I(23,00,00,00),B(_mop_meth0 ,38),I(3b,fd,25,00),B(_mop_meth0 ,10),I(23,00,00,00),B(_mop_meth0 ,39),I(23,00,00,00),B(_mop_meth0 ,35),I(3b,01,25,00),B(_mop_meth0 ,9),I(23,00,00,00),B(_mop_meth0 ,40),I(23,00,00,00),B(_mop_meth0 ,34),I(3b,01,25,00),B(_mop_meth0 ,8),I(23,00,00,00),B(_mop_meth0 ,41),I(23,00,00,00),B(_mop_meth0 ,33),I(3b,01,25,00),B(_mop_meth0 ,7),I(23,00,00,00),B(_mop_meth0 ,16),I(23,00,00,00),B(_mop_meth0 ,32),I(3b,fe,25,00),B(_mop_meth0 ,6),I(23,00,00,00),B(_mop_meth0 ,42),I(23,00,00,00),B(_mop_meth0 ,29),I(3b,fe,25,00),B(_mop_meth0 ,5),I(23,00,00,00),B(_mop_meth0 ,43),I(23,00,00,00),B(_mop_meth0 ,20),I(3b,01,25,00),B(_mop_meth0 ,4),I(23,00,00,00),B(_mop_meth0 ,44),I(23,00,00,00),B(_mop_meth0 ,19),I(3b,ff,25,00),B(_mop_meth0 ,3),I(23,00,00,00),B(_mop_meth0 ,45),I(23,00,00,00),B(_mop_meth0 ,11),I(3b,01,25,00),B(_mop_meth0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00590,G00589);
  eul_intern_keyword(key_594,"class");
  object_class(cons_593) = eul_static_cons_class;
  eul_car(cons_593) = key_594;
  eul_cdr(cons_593) = eul_nil;
  eul_intern_symbol(sym_595,"<simple-method>");
  eul_intern_symbol(sym_596,"make-vector");
  eul_intern_symbol(sym_597,"method-function-lambda");
  eul_intern_symbol(sym_598,"list");
  eul_intern_symbol(sym_599,"make-method");
  eul_allocate_bytevector( G00592,G00591);
  eul_allocate_bytevector( G00601,G00600);
  eul_intern_symbol(sym_605,"absent");
  object_class(cons_604) = eul_static_cons_class;
  eul_car(cons_604) = sym_605;
  eul_cdr(cons_604) = eul_nil;
  object_class(cons_606) = eul_static_cons_class;
  eul_car(cons_606) = key_594;
  eul_cdr(cons_606) = eul_nil;
  eul_intern_symbol(sym_607,"generic-function-method-class");
  eul_intern_symbol(sym_608,"named-method-function-lambda");
  eul_intern_symbol(sym_609,"generic-function-method-keywords");
  eul_intern_symbol(sym_610,"append");
  eul_intern_symbol(sym_611,"stable-add-method");
  eul_allocate_bytevector( G00603,G00602);
  eul_intern_symbol(sym_614,"progn");
  eul_intern_symbol(sym_615,"lambda");
  eul_allocate_bytevector( G00613,G00612);
  eul_allocate_bytevector( G00617,G00616);
  eul_allocate_bytevector( G00619,G00618);
  eul_allocate_bytevector( G00621,G00620);
  eul_intern_symbol(sym_624,"method");
  eul_intern_symbol(sym_625,"named-lambda");
  eul_allocate_bytevector( G00623,G00622);
  eul_intern_symbol(sym_628,"defmethod-keywords");
  eul_intern_symbol(sym_629,"defmethod-sig");
  eul_intern_symbol(sym_630,"defmethod-body");
  eul_intern_symbol(sym_631,"defmethod");
  eul_intern_symbol(sym_632,"defmethod-domain");
  eul_intern_symbol(sym_633,"method-lambda");
  eul_intern_symbol(sym_634,"defmethod-args");
  eul_allocate_bytevector( G00627,G00626);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      _mop_meth0_bindings[i] = eul_nil;
  }

  _mop_meth0_bindings[ 11] = G00590;
  _mop_meth0_bindings[ 12] = key_594;
  _mop_meth0_bindings[ 13] = cons_593;
  _mop_meth0_bindings[ 14] = sym_595;
  _mop_meth0_bindings[ 15] = sym_596;
  _mop_meth0_bindings[ 16] = sym_597;
  _mop_meth0_bindings[ 17] = sym_598;
  _mop_meth0_bindings[ 18] = sym_599;
  _mop_meth0_bindings[ 19] = G00592;
  _mop_meth0_bindings[ 20] = G00601;
  _mop_meth0_bindings[ 21] = sym_605;
  _mop_meth0_bindings[ 22] = cons_604;
  _mop_meth0_bindings[ 23] = cons_606;
  _mop_meth0_bindings[ 24] = sym_607;
  _mop_meth0_bindings[ 25] = sym_608;
  _mop_meth0_bindings[ 26] = sym_609;
  _mop_meth0_bindings[ 27] = sym_610;
  _mop_meth0_bindings[ 28] = sym_611;
  _mop_meth0_bindings[ 29] = G00603;
  _mop_meth0_bindings[ 30] = sym_614;
  _mop_meth0_bindings[ 31] = sym_615;
  _mop_meth0_bindings[ 32] = G00613;
  _mop_meth0_bindings[ 33] = G00617;
  _mop_meth0_bindings[ 34] = G00619;
  _mop_meth0_bindings[ 35] = G00621;
  _mop_meth0_bindings[ 36] = sym_624;
  _mop_meth0_bindings[ 37] = sym_625;
  _mop_meth0_bindings[ 38] = G00623;
  _mop_meth0_bindings[ 1] = eul_nil;
  _mop_meth0_bindings[ 39] = sym_628;
  _mop_meth0_bindings[ 40] = sym_629;
  _mop_meth0_bindings[ 41] = sym_630;
  _mop_meth0_bindings[ 42] = sym_631;
  _mop_meth0_bindings[ 43] = sym_632;
  _mop_meth0_bindings[ 44] = sym_633;
  _mop_meth0_bindings[ 45] = sym_634;
  eul_allocate_lambda( _mop_meth0_bindings[0], "initialize-_mop-meth0", 0, G00627);

  }
}


/* eof */
