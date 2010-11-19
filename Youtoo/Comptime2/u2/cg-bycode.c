/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module cg-bycode
 **  Copyright: See file cg-bycode.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_level_1();
extern LispRef i_level_1_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];

/* Module bindings with size 75 */
LispRef cg_bycode_bindings[75];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module cg-bycode */
void initialize_module_cg_bycode()
{
  if (is_initialized) return;
  initialize_module_i_level_1();
  eul_fast_table_set(eul_modules,"cg_bycode",(LispRef) cg_bycode_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4622, sym_4621, sym_4620, sym_4619, sym_4618, sym_4617, sym_4616, sym_4615, sym_4614, sym_4613, sym_4612, sym_4611, sym_4610, G004609, G004607, G004605, G004603, G004601, G004599, G004597, G004595, G004593, G004591, G004589, G004587, G004585, sym_4583, sym_4582, key_4575, key_4574, key_4573, sym_4572, key_4571, sym_4570, key_4569, sym_4568, key_4567, sym_4566, key_4565, sym_4564, key_4563, sym_4562, key_4561, sym_4560, key_4559, sym_4558, key_4557, G004556, G004554, G004552, G004550, G004548, G004546, G004544, G004542, G004540;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 1 is_init: 0 index: 19 binding: (method-bytecode?) */
  static const void *G004539[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 20 binding: (method-bytecode?) */
  static const void *G004541[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 21 binding: anonymous */
  static const void *G004543[] = {I(a9,86,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 22 binding: anonymous */
  static const void *G004545[] = {I(a9,86,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 23 binding: anonymous */
  static const void *G004547[] = {I(a9,86,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 24 binding: anonymous */
  static const void *G004549[] = {I(a9,86,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 25 binding: anonymous */
  static const void *G004551[] = {I(a9,86,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 26 binding: anonymous */
  static const void *G004553[] = {I(a9,86,45,00)};

  eul_allocate_static_cons(cons_4581, NULL, NULL);
  eul_allocate_static_cons(cons_4580, NULL, eul_as_static(cons_4581));
  eul_allocate_static_cons(cons_4579, NULL, eul_as_static(cons_4580));
  eul_allocate_static_cons(cons_4578, NULL, eul_as_static(cons_4579));
  eul_allocate_static_cons(cons_4577, NULL, eul_as_static(cons_4578));
  eul_allocate_static_cons(cons_4576, NULL, eul_as_static(cons_4577));
  /* Byte-vector with size: 211 is_init: 0 index: 49 binding: top-level */
  static const void *G004555[] = {I(a9,24,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,28),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,26),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,27),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,32),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,25),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,33),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,34),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,24),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,35),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,36),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,23),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,37),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,38),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,22),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,39),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,40),I(23,00,00,00),B(cg_bycode ,29),I(23,00,00,00),B(cg_bycode ,30),I(23,00,00,00),B(cg_bycode ,21),I(3b,00,23,00),B(cg_bycode ,31),I(23,00,00,00),B(cg_bycode ,41),I(24,00,00,00),B(boot1 ,26),I(3c,06,1f,05),I(1f,05,1f,05),I(1f,05,1f,05),I(1f,05,24,00),B(boot1 ,26),I(3c,06,24,00),B(mop_class ,81),I(23,00,00,00),B(cg_bycode ,27),I(23,00,00,00),B(cg_bycode ,42),I(23,00,00,00),B(cg_bycode ,43),I(1f,0b,23,00),B(cg_bycode ,44),I(1f,06,23,00),B(cg_bycode ,45),I(23,00,00,00),B(cg_bycode ,46),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(cg_bycode ,5),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(cg_bycode ,18),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(cg_bycode ,6),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(cg_bycode ,8),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(cg_bycode ,47),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(cg_bycode ,16),I(2a,24,00,00),B(cg_bycode ,16),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(cg_bycode ,16),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(cg_bycode ,48),I(23,00,00,00),B(cg_bycode ,20),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(cg_bycode ,16),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(cg_bycode ,16),I(8a,03,02,83),I(24,00,00,00),B(cg_bycode ,5),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(cg_bycode ,16),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(cg_bycode ,48),I(23,00,00,00),B(cg_bycode ,19),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(cg_bycode ,16),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(cg_bycode ,16),I(2a,24,00,00),B(cg_bycode ,5),I(45,1c,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 50 binding: bytecode-args! */
  static const void *G004584[] = {I(ab,1c,8a,04),I(1d,24,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 51 binding: bytecode-properties? */
  static const void *G004586[] = {I(aa,84,24,00),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 52 binding: bytecode-size! */
  static const void *G004588[] = {I(ab,1c,83,1d),I(24,00,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 53 binding: bytecode-args? */
  static const void *G004590[] = {I(aa,8a,04,24),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 54 binding: bytecode-modus! */
  static const void *G004592[] = {I(ab,1c,82,1d),I(24,00,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 55 binding: bytecode-properties! */
  static const void *G004594[] = {I(ab,1c,84,1d),I(24,00,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 56 binding: bytecode-name? */
  static const void *G004596[] = {I(aa,8a,05,24),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 57 binding: bytecode-size? */
  static const void *G004598[] = {I(aa,83,24,00),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 58 binding: bytecode-code? */
  static const void *G004600[] = {I(aa,8a,03,24),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 59 binding: bytecode-code! */
  static const void *G004602[] = {I(ab,1c,8a,03),I(1d,24,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 60 binding: bytecode-modus? */
  static const void *G004604[] = {I(aa,82,24,00),B(cg_bycode ,5),I(08,45,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 61 binding: bytecode-name! */
  static const void *G004606[] = {I(ab,1c,8a,05),I(1d,24,00,00),B(cg_bycode ,5),I(09,45,02,00)};

  /* Byte-vector with size: 95 is_init: 1 index: 0 binding: initialize-cg-bycode */
  static const void *G004608[] = {I(87,25,00,00),B(cg_bycode ,1),I(24,00,00,00),B(i_level_1 ,1),I(3e,0b,24,00),B(i_level_1 ,0),I(3c,00,21,01),I(86,25,00,00),B(cg_bycode ,18),I(23,00,00,00),B(cg_bycode ,62),I(23,00,00,00),B(cg_bycode ,61),I(3b,02,25,00),B(cg_bycode ,17),I(86,25,00,00),B(cg_bycode ,16),I(23,00,00,00),B(cg_bycode ,63),I(23,00,00,00),B(cg_bycode ,60),I(3b,01,25,00),B(cg_bycode ,15),I(23,00,00,00),B(cg_bycode ,64),I(23,00,00,00),B(cg_bycode ,59),I(3b,02,25,00),B(cg_bycode ,14),I(23,00,00,00),B(cg_bycode ,65),I(23,00,00,00),B(cg_bycode ,58),I(3b,01,25,00),B(cg_bycode ,13),I(23,00,00,00),B(cg_bycode ,66),I(23,00,00,00),B(cg_bycode ,57),I(3b,01,25,00),B(cg_bycode ,12),I(23,00,00,00),B(cg_bycode ,67),I(23,00,00,00),B(cg_bycode ,56),I(3b,01,25,00),B(cg_bycode ,11),I(23,00,00,00),B(cg_bycode ,68),I(23,00,00,00),B(cg_bycode ,55),I(3b,02,25,00),B(cg_bycode ,10),I(23,00,00,00),B(cg_bycode ,69),I(23,00,00,00),B(cg_bycode ,54),I(3b,02,25,00),B(cg_bycode ,9),I(86,25,00,00),B(cg_bycode ,8),I(23,00,00,00),B(cg_bycode ,70),I(23,00,00,00),B(cg_bycode ,53),I(3b,01,25,00),B(cg_bycode ,7),I(86,25,00,00),B(cg_bycode ,6),I(86,25,00,00),B(cg_bycode ,5),I(23,00,00,00),B(cg_bycode ,71),I(23,00,00,00),B(cg_bycode ,52),I(3b,02,25,00),B(cg_bycode ,4),I(23,00,00,00),B(cg_bycode ,72),I(23,00,00,00),B(cg_bycode ,51),I(3b,01,25,00),B(cg_bycode ,3),I(23,00,00,00),B(cg_bycode ,73),I(23,00,00,00),B(cg_bycode ,50),I(3b,02,25,00),B(cg_bycode ,2),I(23,00,00,00),B(cg_bycode ,74),I(23,00,00,00),B(cg_bycode ,49),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G004540,G004539);
  eul_allocate_bytevector( G004542,G004541);
  eul_allocate_bytevector( G004544,G004543);
  eul_allocate_bytevector( G004546,G004545);
  eul_allocate_bytevector( G004548,G004547);
  eul_allocate_bytevector( G004550,G004549);
  eul_allocate_bytevector( G004552,G004551);
  eul_allocate_bytevector( G004554,G004553);
  eul_intern_keyword(key_4557,"name");
  eul_intern_symbol(sym_4558,"name");
  eul_intern_keyword(key_4559,"default");
  eul_intern_symbol(sym_4560,"anonymous");
  eul_intern_keyword(key_4561,"keyword");
  eul_intern_symbol(sym_4562,"args");
  eul_intern_keyword(key_4563,"args");
  eul_intern_symbol(sym_4564,"code");
  eul_intern_keyword(key_4565,"code");
  eul_intern_symbol(sym_4566,"properties");
  eul_intern_keyword(key_4567,"properties");
  eul_intern_symbol(sym_4568,"size");
  eul_intern_keyword(key_4569,"size");
  eul_intern_symbol(sym_4570,"modus");
  eul_intern_keyword(key_4571,"modus");
  eul_intern_symbol(sym_4572,"bytecode");
  eul_intern_keyword(key_4573,"direct-superclasses");
  eul_intern_keyword(key_4574,"direct-slots");
  eul_intern_keyword(key_4575,"direct-keywords");
  object_class(cons_4581) = eul_static_cons_class;
  eul_car(cons_4581) = key_4557;
  eul_cdr(cons_4581) = eul_nil;
  object_class(cons_4580) = eul_static_cons_class;
  eul_car(cons_4580) = key_4563;
  object_class(cons_4579) = eul_static_cons_class;
  eul_car(cons_4579) = key_4565;
  object_class(cons_4578) = eul_static_cons_class;
  eul_car(cons_4578) = key_4567;
  object_class(cons_4577) = eul_static_cons_class;
  eul_car(cons_4577) = key_4569;
  object_class(cons_4576) = eul_static_cons_class;
  eul_car(cons_4576) = key_4571;
  eul_intern_symbol(sym_4582,"bytecode?");
  eul_intern_symbol(sym_4583,"(method bytecode?)");
  eul_allocate_bytevector( G004556,G004555);
  eul_allocate_bytevector( G004585,G004584);
  eul_allocate_bytevector( G004587,G004586);
  eul_allocate_bytevector( G004589,G004588);
  eul_allocate_bytevector( G004591,G004590);
  eul_allocate_bytevector( G004593,G004592);
  eul_allocate_bytevector( G004595,G004594);
  eul_allocate_bytevector( G004597,G004596);
  eul_allocate_bytevector( G004599,G004598);
  eul_allocate_bytevector( G004601,G004600);
  eul_allocate_bytevector( G004603,G004602);
  eul_allocate_bytevector( G004605,G004604);
  eul_allocate_bytevector( G004607,G004606);
  eul_intern_symbol(sym_4610,"bytecode-name!");
  eul_intern_symbol(sym_4611,"bytecode-modus?");
  eul_intern_symbol(sym_4612,"bytecode-code!");
  eul_intern_symbol(sym_4613,"bytecode-code?");
  eul_intern_symbol(sym_4614,"bytecode-size?");
  eul_intern_symbol(sym_4615,"bytecode-name?");
  eul_intern_symbol(sym_4616,"bytecode-properties!");
  eul_intern_symbol(sym_4617,"bytecode-modus!");
  eul_intern_symbol(sym_4618,"bytecode-args?");
  eul_intern_symbol(sym_4619,"bytecode-size!");
  eul_intern_symbol(sym_4620,"bytecode-properties?");
  eul_intern_symbol(sym_4621,"bytecode-args!");
  eul_intern_symbol(sym_4622,"top-level");
  eul_allocate_bytevector( G004609,G004608);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 19; i++)
      cg_bycode_bindings[i] = eul_nil;
  }

  cg_bycode_bindings[ 19] = G004540;
  cg_bycode_bindings[ 20] = G004542;
  cg_bycode_bindings[ 21] = G004544;
  cg_bycode_bindings[ 22] = G004546;
  cg_bycode_bindings[ 23] = G004548;
  cg_bycode_bindings[ 24] = G004550;
  cg_bycode_bindings[ 25] = G004552;
  cg_bycode_bindings[ 26] = G004554;
  cg_bycode_bindings[ 27] = key_4557;
  cg_bycode_bindings[ 28] = sym_4558;
  cg_bycode_bindings[ 29] = key_4559;
  cg_bycode_bindings[ 30] = sym_4560;
  cg_bycode_bindings[ 31] = key_4561;
  cg_bycode_bindings[ 32] = sym_4562;
  cg_bycode_bindings[ 33] = key_4563;
  cg_bycode_bindings[ 34] = sym_4564;
  cg_bycode_bindings[ 35] = key_4565;
  cg_bycode_bindings[ 36] = sym_4566;
  cg_bycode_bindings[ 37] = key_4567;
  cg_bycode_bindings[ 38] = sym_4568;
  cg_bycode_bindings[ 39] = key_4569;
  cg_bycode_bindings[ 40] = sym_4570;
  cg_bycode_bindings[ 41] = key_4571;
  cg_bycode_bindings[ 42] = sym_4572;
  cg_bycode_bindings[ 43] = key_4573;
  cg_bycode_bindings[ 44] = key_4574;
  cg_bycode_bindings[ 45] = key_4575;
  cg_bycode_bindings[ 46] = cons_4576;
  cg_bycode_bindings[ 47] = sym_4582;
  cg_bycode_bindings[ 48] = sym_4583;
  cg_bycode_bindings[ 49] = G004556;
  cg_bycode_bindings[ 50] = G004585;
  cg_bycode_bindings[ 51] = G004587;
  cg_bycode_bindings[ 52] = G004589;
  cg_bycode_bindings[ 53] = G004591;
  cg_bycode_bindings[ 54] = G004593;
  cg_bycode_bindings[ 55] = G004595;
  cg_bycode_bindings[ 56] = G004597;
  cg_bycode_bindings[ 57] = G004599;
  cg_bycode_bindings[ 58] = G004601;
  cg_bycode_bindings[ 59] = G004603;
  cg_bycode_bindings[ 60] = G004605;
  cg_bycode_bindings[ 61] = G004607;
  cg_bycode_bindings[ 1] = eul_nil;
  cg_bycode_bindings[ 62] = sym_4610;
  cg_bycode_bindings[ 63] = sym_4611;
  cg_bycode_bindings[ 64] = sym_4612;
  cg_bycode_bindings[ 65] = sym_4613;
  cg_bycode_bindings[ 66] = sym_4614;
  cg_bycode_bindings[ 67] = sym_4615;
  cg_bycode_bindings[ 68] = sym_4616;
  cg_bycode_bindings[ 69] = sym_4617;
  cg_bycode_bindings[ 70] = sym_4618;
  cg_bycode_bindings[ 71] = sym_4619;
  cg_bycode_bindings[ 72] = sym_4620;
  cg_bycode_bindings[ 73] = sym_4621;
  cg_bycode_bindings[ 74] = sym_4622;
  eul_allocate_lambda( cg_bycode_bindings[0], "initialize-cg-bycode", 0, G004609);

  }
}


/* eof */
