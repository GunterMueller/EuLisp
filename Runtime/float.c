/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module float
 **  Copyright: See file float.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_number();
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot1_bindings[];
extern LispRef number_bindings[];

/* Module bindings with size 35 */
LispRef float_bindings[35];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module float */
void initialize_module_float()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_number();
  eul_fast_table_set(eul_modules,"float",(LispRef) float_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_3267, G003266, sym_3264, sym_3262, sym_3260, sym_3259, sym_3258, sym_3257, sym_3256, sym_3255, sym_3254, sym_3253, sym_3252, key_3251, key_3250, key_3249, key_3248, sym_3247, key_3246, G003245, G003243, G003241, G003239, G003237;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 1 is_init: 0 index: 11 binding: (method-doublep) */
  static const void *G003236[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 12 binding: (method-doublep) */
  static const void *G003238[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-floatp) */
  static const void *G003240[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 14 binding: (method-floatp) */
  static const void *G003242[] = {I(aa,86,45,01)};

  eul_allocate_static_cons(cons_3263, NULL, NULL);
  eul_allocate_static_cons(cons_3261, NULL, eul_as_static(cons_3263));
  /* Byte-vector with size: 329 is_init: 0 index: 33 binding: top-level */
  static const void *G003244[] = {I(a9,24,00,00),B(number ,5),I(24,00,00,00),B(boot1 ,26),I(3c,01,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,71),I(23,00,00,00),B(float ,15),I(23,00,00,00),B(float ,16),I(23,00,00,00),B(float ,17),I(1f,05,23,00),B(float ,18),I(1f,06,23,00),B(float ,19),I(86,23,00,00),B(float ,20),I(87,24,00,00),B(mop_gf ,2),I(3c,0b,1b,89),B(float ,4),I(2a,28,06,1b),I(89,00,00,00),B(float ,6),I(2a,24,00,00),B(float ,6),I(89,00,00,00),B(float ,8),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,21),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,2),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,22),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,10),I(2a,83,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,23),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,9),I(2a,83,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,24),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,7),I(2a,83,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,25),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,5),I(2a,83,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,26),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(float ,3),I(2a,24,00,00),B(float ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(float ,27),I(23,00,00,00),B(float ,14),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(float ,27),I(23,00,00,00),B(float ,13),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,2),I(2a,24,00,00),B(float ,4),I(2a,24,00,00),B(float ,6),I(24,00,00,00),B(mop_class ,71),I(05,2a,24,00),B(float ,4),I(24,00,00,00),B(boot1 ,26),I(3c,01,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,15),I(23,00,00,00),B(float ,28),I(23,00,00,00),B(float ,17),I(1f,04,23,00),B(float ,18),I(1f,05,23,00),B(float ,19),I(86,24,00,00),B(boot1 ,26),I(3c,08,24,00),B(float ,6),I(1c,24,00,00),B(mop_gf ,12),I(3c,02,2a,24),B(float ,10),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,10),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(float ,29),I(23,00,00,00),B(float ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,10),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,6),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,10),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(float ,29),I(23,00,00,00),B(float ,11),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,10),I(2a,24,00,00),B(float ,6),I(2a,24,00,00),B(float ,9),I(2a,24,00,00),B(float ,7),I(2a,24,00,00),B(float ,5),I(2a,24,00,00),B(float ,3),I(2a,24,00,00),B(mop_class ,13),I(24,00,00,00),B(boot1 ,41),I(3c,01,83,24),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(float ,32),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(float ,6),I(1c,1f,06,3c),I(02,2a,24,00),B(float ,6),I(24,00,00,00),B(mop_class ,13),I(3d,01,3c,45),I(3c,00,00,00)};

  /* Byte-vector with size: 36 is_init: 1 index: 0 binding: initialize-float */
  static const void *G003265[] = {I(87,25,00,00),B(float ,1),I(24,00,00,00),B(number ,1),I(3e,0b,24,00),B(number ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(float ,10),I(86,25,00,00),B(float ,9),I(86,25,00,00),B(float ,8),I(86,25,00,00),B(float ,7),I(86,25,00,00),B(float ,6),I(86,25,00,00),B(float ,5),I(86,25,00,00),B(float ,4),I(86,25,00,00),B(float ,3),I(86,25,00,00),B(float ,2),I(23,00,00,00),B(float ,34),I(23,00,00,00),B(float ,33),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G003237,G003236);
  eul_allocate_bytevector( G003239,G003238);
  eul_allocate_bytevector( G003241,G003240);
  eul_allocate_bytevector( G003243,G003242);
  eul_intern_keyword(key_3246,"name");
  eul_intern_symbol(sym_3247,"float");
  eul_intern_keyword(key_3248,"direct-superclasses");
  eul_intern_keyword(key_3249,"direct-slots");
  eul_intern_keyword(key_3250,"direct-keywords");
  eul_intern_keyword(key_3251,"abstractp");
  eul_intern_symbol(sym_3252,"floatp");
  eul_intern_symbol(sym_3253,"doublep");
  eul_intern_symbol(sym_3254,"ceiling");
  eul_intern_symbol(sym_3255,"floor");
  eul_intern_symbol(sym_3256,"round");
  eul_intern_symbol(sym_3257,"truncate");
  eul_intern_symbol(sym_3258,"(method floatp)");
  eul_intern_symbol(sym_3259,"double");
  eul_intern_symbol(sym_3260,"(method doublep)");
  eul_intern_symbol(sym_3262,"converter");
  eul_intern_symbol(sym_3264,"<double>");
  object_class(cons_3263) = eul_static_cons_class;
  eul_car(cons_3263) = sym_3264;
  eul_cdr(cons_3263) = eul_nil;
  object_class(cons_3261) = eul_static_cons_class;
  eul_car(cons_3261) = sym_3262;
  eul_allocate_bytevector( G003245,G003244);
  eul_intern_symbol(sym_3267,"top-level");
  eul_allocate_bytevector( G003266,G003265);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      float_bindings[i] = eul_nil;
  }

  float_bindings[ 11] = G003237;
  float_bindings[ 12] = G003239;
  float_bindings[ 13] = G003241;
  float_bindings[ 14] = G003243;
  float_bindings[ 15] = key_3246;
  float_bindings[ 16] = sym_3247;
  float_bindings[ 17] = key_3248;
  float_bindings[ 18] = key_3249;
  float_bindings[ 19] = key_3250;
  float_bindings[ 20] = key_3251;
  float_bindings[ 21] = sym_3252;
  float_bindings[ 22] = sym_3253;
  float_bindings[ 23] = sym_3254;
  float_bindings[ 24] = sym_3255;
  float_bindings[ 25] = sym_3256;
  float_bindings[ 26] = sym_3257;
  float_bindings[ 27] = sym_3258;
  float_bindings[ 28] = sym_3259;
  float_bindings[ 29] = sym_3260;
  float_bindings[ 30] = sym_3262;
  float_bindings[ 31] = sym_3264;
  float_bindings[ 32] = cons_3261;
  float_bindings[ 33] = G003245;
  float_bindings[ 1] = eul_nil;
  float_bindings[ 34] = sym_3267;
  eul_allocate_lambda( float_bindings[0], "initialize-float", 0, G003266);

  }
}


/* eof */
