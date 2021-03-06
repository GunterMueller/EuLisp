/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module ex-aux0
 **  Copyright: See file ex-aux0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level_1();
extern LispRef level_1_bindings[];

/* Module bindings with size 48 */
LispRef ex_aux0_bindings[48];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module ex-aux0 */
void initialize_module_ex_aux0()
{
  if (is_initialized) return;
  initialize_module_level_1();
  eul_fast_table_set(eul_modules,"ex_aux0",(LispRef) ex_aux0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1613, sym_1612, sym_1611, sym_1610, sym_1609, sym_1608, sym_1607, sym_1606, G001605, G001603, G001601, G001599, sym_1596, G001595, sym_1592, sym_1591, sym_1590, G001589, G001587, sym_1585, sym_1584, sym_1583, sym_1582, sym_1581, sym_1579, sym_1578, sym_1577, sym_1576, sym_1575, sym_1574, sym_1573, sym_1572, sym_1571, G001570, sym_1568, G001567;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 3 is_init: 0 index: 11 binding: get-params */
  static const void *G001566[] = {I(aa,86,0f,23),B(ex_aux0 ,10),I(1c,0f,45,01)};

  eul_allocate_static_string(str_1580, "bad value ~a", 12);
  /* Byte-vector with size: 58 is_init: 0 index: 27 binding: get-name */
  static const void *G001569[] = {I(aa,86,0f,23),B(ex_aux0 ,12),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,14),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,15),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,16),I(1c,0f,23,00),B(ex_aux0 ,17),I(86,0f,23,00),B(ex_aux0 ,18),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(ex_aux0 ,19),I(1c,0f,1b,86),I(0f,1f,08,1c),I(0f,23,00,00),B(ex_aux0 ,20),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,21),I(1c,0f,23,00),B(ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,23),I(1c,0f,23,00),B(ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(ex_aux0 ,25),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1f,18),I(1c,0f,23,00),B(ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,1d,1c),I(0f,23,00,00),B(ex_aux0 ,26),I(1c,0f,45,23)};

  /* Byte-vector with size: 3 is_init: 0 index: 28 binding: get-directives */
  static const void *G001586[] = {I(aa,86,0f,23),B(ex_aux0 ,10),I(1c,0f,45,01)};

  eul_allocate_static_string(str_1593, "body ~a not a list", 18);
  /* Byte-vector with size: 40 is_init: 0 index: 33 binding: get-lambda-body */
  static const void *G001588[] = {I(aa,86,0f,23),B(ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,30),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,15),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(ex_aux0 ,31),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,32),I(1c,0f,23,00),B(ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,23),I(1c,0f,23,00),B(ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,12,1c),I(0f,23,00,00),B(ex_aux0 ,26),I(1c,0f,45,18)};

  eul_allocate_static_string(str_1597, "body ~a not a list", 18);
  /* Byte-vector with size: 43 is_init: 0 index: 36 binding: get-body */
  static const void *G001594[] = {I(aa,86,0f,23),B(ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,34),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,30),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,15),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(ex_aux0 ,31),I(1c,0f,23,00),B(ex_aux0 ,13),I(86,0f,23,00),B(ex_aux0 ,35),I(1c,0f,23,00),B(ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,23),I(1c,0f,23,00),B(ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,12,1c),I(0f,23,00,00),B(ex_aux0 ,26),I(1c,0f,45,1a)};

  /* Byte-vector with size: 3 is_init: 0 index: 37 binding: get-lambda-params */
  static const void *G001598[] = {I(aa,86,0f,23),B(ex_aux0 ,12),I(1c,0f,45,01)};

  /* Byte-vector with size: 6 is_init: 0 index: 38 binding: get-top-level-forms */
  static const void *G001600[] = {I(aa,86,0f,23),B(ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(ex_aux0 ,34),I(1c,0f,45,03)};

  /* Byte-vector with size: 3 is_init: 0 index: 39 binding: get-value */
  static const void *G001602[] = {I(aa,86,0f,23),B(ex_aux0 ,10),I(1c,0f,45,01)};

  /* Byte-vector with size: 56 is_init: 1 index: 0 binding: initialize-ex-aux0 */
  static const void *G001604[] = {I(87,25,00,00),B(ex_aux0 ,1),I(24,00,00,00),B(level_1 ,1),I(3e,0b,24,00),B(level_1 ,0),I(3c,00,21,01),I(23,00,00,00),B(ex_aux0 ,40),I(23,00,00,00),B(ex_aux0 ,39),I(3b,01,25,00),B(ex_aux0 ,9),I(23,00,00,00),B(ex_aux0 ,41),I(23,00,00,00),B(ex_aux0 ,38),I(3b,01,25,00),B(ex_aux0 ,8),I(23,00,00,00),B(ex_aux0 ,42),I(23,00,00,00),B(ex_aux0 ,37),I(3b,01,25,00),B(ex_aux0 ,7),I(23,00,00,00),B(ex_aux0 ,43),I(23,00,00,00),B(ex_aux0 ,36),I(3b,01,25,00),B(ex_aux0 ,6),I(23,00,00,00),B(ex_aux0 ,44),I(23,00,00,00),B(ex_aux0 ,33),I(3b,01,25,00),B(ex_aux0 ,5),I(23,00,00,00),B(ex_aux0 ,45),I(23,00,00,00),B(ex_aux0 ,28),I(3b,01,25,00),B(ex_aux0 ,4),I(23,00,00,00),B(ex_aux0 ,46),I(23,00,00,00),B(ex_aux0 ,27),I(3b,01,25,00),B(ex_aux0 ,3),I(23,00,00,00),B(ex_aux0 ,47),I(23,00,00,00),B(ex_aux0 ,11),I(3b,01,25,00),B(ex_aux0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_1568,"caddr");
  eul_allocate_bytevector( G001567,G001566);
  eul_intern_symbol(sym_1571,"cadr");
  eul_intern_symbol(sym_1572,"x");
  eul_intern_symbol(sym_1573,"symbol?");
  eul_intern_symbol(sym_1574,"cons?");
  eul_intern_symbol(sym_1575,"car");
  eul_intern_symbol(sym_1576,"setter");
  eul_intern_symbol(sym_1577,"quote");
  eul_intern_symbol(sym_1578,"eq");
  eul_intern_symbol(sym_1579,"and");
  object_class(str_1580) = eul_static_string_class;
  eul_intern_symbol(sym_1581,"fmt");
  eul_intern_symbol(sym_1582,"<condition>");
  eul_intern_symbol(sym_1583,"error");
  eul_intern_symbol(sym_1584,"if");
  eul_intern_symbol(sym_1585,"let");
  eul_allocate_bytevector( G001570,G001569);
  eul_allocate_bytevector( G001587,G001586);
  eul_intern_symbol(sym_1590,"cddr");
  eul_intern_symbol(sym_1591,"null?");
  eul_intern_symbol(sym_1592,"or");
  object_class(str_1593) = eul_static_string_class;
  eul_allocate_bytevector( G001589,G001588);
  eul_intern_symbol(sym_1596,"cdr");
  object_class(str_1597) = eul_static_string_class;
  eul_allocate_bytevector( G001595,G001594);
  eul_allocate_bytevector( G001599,G001598);
  eul_allocate_bytevector( G001601,G001600);
  eul_allocate_bytevector( G001603,G001602);
  eul_intern_symbol(sym_1606,"get-value");
  eul_intern_symbol(sym_1607,"get-top-level-forms");
  eul_intern_symbol(sym_1608,"get-lambda-params");
  eul_intern_symbol(sym_1609,"get-body");
  eul_intern_symbol(sym_1610,"get-lambda-body");
  eul_intern_symbol(sym_1611,"get-directives");
  eul_intern_symbol(sym_1612,"get-name");
  eul_intern_symbol(sym_1613,"get-params");
  eul_allocate_bytevector( G001605,G001604);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 10; i++)
      ex_aux0_bindings[i] = eul_nil;
  }

  ex_aux0_bindings[ 10] = sym_1568;
  ex_aux0_bindings[ 11] = G001567;
  ex_aux0_bindings[ 12] = sym_1571;
  ex_aux0_bindings[ 13] = sym_1572;
  ex_aux0_bindings[ 14] = sym_1573;
  ex_aux0_bindings[ 15] = sym_1574;
  ex_aux0_bindings[ 16] = sym_1575;
  ex_aux0_bindings[ 17] = sym_1576;
  ex_aux0_bindings[ 18] = sym_1577;
  ex_aux0_bindings[ 19] = sym_1578;
  ex_aux0_bindings[ 20] = sym_1579;
  ex_aux0_bindings[ 21] = str_1580;
  ex_aux0_bindings[ 22] = sym_1581;
  ex_aux0_bindings[ 23] = sym_1582;
  ex_aux0_bindings[ 24] = sym_1583;
  ex_aux0_bindings[ 25] = sym_1584;
  ex_aux0_bindings[ 26] = sym_1585;
  ex_aux0_bindings[ 27] = G001570;
  ex_aux0_bindings[ 28] = G001587;
  ex_aux0_bindings[ 29] = sym_1590;
  ex_aux0_bindings[ 30] = sym_1591;
  ex_aux0_bindings[ 31] = sym_1592;
  ex_aux0_bindings[ 32] = str_1593;
  ex_aux0_bindings[ 33] = G001589;
  ex_aux0_bindings[ 34] = sym_1596;
  ex_aux0_bindings[ 35] = str_1597;
  ex_aux0_bindings[ 36] = G001595;
  ex_aux0_bindings[ 37] = G001599;
  ex_aux0_bindings[ 38] = G001601;
  ex_aux0_bindings[ 39] = G001603;
  ex_aux0_bindings[ 1] = eul_nil;
  ex_aux0_bindings[ 40] = sym_1606;
  ex_aux0_bindings[ 41] = sym_1607;
  ex_aux0_bindings[ 42] = sym_1608;
  ex_aux0_bindings[ 43] = sym_1609;
  ex_aux0_bindings[ 44] = sym_1610;
  ex_aux0_bindings[ 45] = sym_1611;
  ex_aux0_bindings[ 46] = sym_1612;
  ex_aux0_bindings[ 47] = sym_1613;
  eul_allocate_lambda( ex_aux0_bindings[0], "initialize-ex-aux0", 0, G001605);

  }
}


/* eof */
