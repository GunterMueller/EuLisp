/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module callback
 **  Copyright: See file callback.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_condition();
extern LispRef telos_bindings[];
extern LispRef boot_bindings[];
extern LispRef condition_bindings[];
extern LispRef boot1_bindings[];

/* Module bindings with size 43 */
LispRef callback_bindings[43];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module callback */
void initialize_module_callback()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_condition();
  eul_fast_table_set(eul_modules,"callback",(LispRef) callback_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1115, sym_1114, sym_1113, G001112, G001110, G001108, G001106, sym_1104, G001077, key_1075, G001072;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_1073, "integer underflow", 17);
  eul_allocate_static_string(str_1074, "integer overerflow", 18);
  /* Byte-vector with size: 17 is_init: 0 index: 8 binding: anonymous */
  static const void *G001071[] = {I(ab,1b,34,00),I(00,00,00,16),I(23,00,00,00),B(callback ,5),I(32,00,00,00),I(00,00,00,10),I(23,00,00,00),B(callback ,6),I(1d,24,00,00),B(boot1 ,23),I(3c,01,1c,24),B(condition ,5),I(23,00,00,00),B(callback ,7),I(1f,03,24,00),B(boot ,22),I(3d,04,04,00)};

  eul_allocate_static_string(str_1078, "unspecified system error", 24);
  eul_allocate_static_string(str_1079, "bad car domain", 14);
  eul_allocate_static_string(str_1080, "bad cdr domain", 14);
  eul_allocate_static_string(str_1081, "bad assq domain", 15);
  eul_allocate_static_string(str_1082, "bad iniq domain", 15);
  eul_allocate_static_string(str_1083, "argument number mismatch", 24);
  eul_allocate_static_string(str_1084, "bad stream primitive", 20);
  eul_allocate_static_string(str_1085, "last argument of apply must be a list", 37);
  eul_allocate_static_string(str_1086, "bad setter argument", 19);
  eul_allocate_static_string(str_1087, "bad setter", 10);
  eul_allocate_static_string(str_1088, "cannot set setter", 17);
  eul_allocate_static_string(str_1089, "bad (setter car) domain", 23);
  eul_allocate_static_string(str_1090, "bad (setter cdr) domain", 23);
  eul_allocate_static_string(str_1091, "no next method available", 24);
  eul_allocate_static_string(str_1092, "user interrupt", 14);
  eul_allocate_static_string(str_1093, "segmentation fault", 18);
  eul_allocate_static_string(str_1094, "bus error", 9);
  eul_allocate_static_string(str_1095, "bad foreign function conversion", 31);
  eul_allocate_static_string(str_1096, "bad operator", 12);
  eul_allocate_static_string(str_1097, "bad slot access", 15);
  eul_allocate_static_string(str_1098, "bad string access", 17);
  eul_allocate_static_string(str_1099, "cannot set class", 16);
  eul_allocate_static_string(str_1100, "integer overflow", 16);
  eul_allocate_static_string(str_1101, "integer underflow", 17);
  eul_allocate_static_string(str_1102, "integer overflow", 16);
  eul_allocate_static_string(str_1103, "division by zero", 16);
  /* Byte-vector with size: 228 is_init: 0 index: 36 binding: top-level */
  static const void *G001076[] = {I(a9,28,0e,1b),I(89,00,00,00),B(callback ,3),I(2a,23,00,00),B(callback ,9),I(24,00,00,00),B(callback ,2),I(3c,01,82,1c),I(24,00,00,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0a),I(82,14,23,00),B(callback ,10),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0a),I(2b,23,00,00),B(callback ,11),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0a),I(84,14,23,00),B(callback ,12),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0a),I(26,00,00,00),I(00,00,00,03),I(14,23,00,00),B(callback ,13),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0f),I(82,14,23,00),B(callback ,14),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0f),I(2b,23,00,00),B(callback ,15),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,0f),I(84,14,23,00),B(callback ,16),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,2d),I(82,14,23,00),B(callback ,17),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,2d),I(2b,23,00,00),B(callback ,18),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,2d),I(84,14,23,00),B(callback ,19),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,2d),I(26,00,00,00),I(00,00,00,03),I(14,23,00,00),B(callback ,20),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,2d),I(26,00,00,00),I(00,00,00,04),I(14,23,00,00),B(callback ,21),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,23),B(callback ,22),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,10),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,14),I(82,14,23,00),B(callback ,23),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,14),I(2b,23,00,00),B(callback ,24),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,14),I(84,14,23,00),B(callback ,25),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,14),I(26,00,00,00),I(00,00,00,03),I(14,23,00,00),B(callback ,26),I(24,00,00,00),B(callback ,2),I(3c,01,24,00),B(callback ,4),I(3c,02,2a,23),B(callback ,27),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,23),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,28),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,24),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,29),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,19),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,30),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,1a),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,31),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,1e),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,32),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,1f),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,33),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,20),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,23),B(callback ,34),I(24,00,00,00),B(callback ,2),I(3c,01,26,00),I(00,00,00,21),I(1c,24,00,00),B(callback ,4),I(3c,02,2a,26),I(00,00,00,22),I(23,00,00,00),B(callback ,35),I(23,00,00,00),B(callback ,8),I(3b,02,24,00),B(callback ,4),I(3d,02,0b,45),I(0b,00,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 37 binding: anonymous */
  static const void *G001105[] = {I(a8,47,00,00),I(24,00,00,00),B(condition ,5),I(23,00,00,00),B(callback ,7),I(1f,03,24,00),B(boot ,22),I(3d,04,01,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 38 binding: callback-thunk */
  static const void *G001107[] = {I(aa,46,01,1b),I(48,00,00,23),B(callback ,35),I(23,00,00,00),B(callback ,37),I(3b,ff,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 39 binding: install-callback */
  static const void *G001109[] = {I(ab,24,00,00),B(callback ,3),I(1d,1d,03,45),I(02,00,00,00)};

  /* Byte-vector with size: 32 is_init: 1 index: 0 binding: initialize-callback */
  static const void *G001111[] = {I(87,25,00,00),B(callback ,1),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(callback ,40),I(23,00,00,00),B(callback ,39),I(3b,02,25,00),B(callback ,4),I(86,25,00,00),B(callback ,3),I(23,00,00,00),B(callback ,41),I(23,00,00,00),B(callback ,38),I(3b,01,25,00),B(callback ,2),I(23,00,00,00),B(callback ,42),I(23,00,00,00),B(callback ,36),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_1073) = eul_static_string_class;
  object_class(str_1074) = eul_static_string_class;
  eul_intern_keyword(key_1075,"arguments");
  eul_allocate_bytevector( G001072,G001071);
  object_class(str_1078) = eul_static_string_class;
  object_class(str_1079) = eul_static_string_class;
  object_class(str_1080) = eul_static_string_class;
  object_class(str_1081) = eul_static_string_class;
  object_class(str_1082) = eul_static_string_class;
  object_class(str_1083) = eul_static_string_class;
  object_class(str_1084) = eul_static_string_class;
  object_class(str_1085) = eul_static_string_class;
  object_class(str_1086) = eul_static_string_class;
  object_class(str_1087) = eul_static_string_class;
  object_class(str_1088) = eul_static_string_class;
  object_class(str_1089) = eul_static_string_class;
  object_class(str_1090) = eul_static_string_class;
  object_class(str_1091) = eul_static_string_class;
  object_class(str_1092) = eul_static_string_class;
  object_class(str_1093) = eul_static_string_class;
  object_class(str_1094) = eul_static_string_class;
  object_class(str_1095) = eul_static_string_class;
  object_class(str_1096) = eul_static_string_class;
  object_class(str_1097) = eul_static_string_class;
  object_class(str_1098) = eul_static_string_class;
  object_class(str_1099) = eul_static_string_class;
  object_class(str_1100) = eul_static_string_class;
  object_class(str_1101) = eul_static_string_class;
  object_class(str_1102) = eul_static_string_class;
  object_class(str_1103) = eul_static_string_class;
  eul_intern_symbol(sym_1104,"anonymous");
  eul_allocate_bytevector( G001077,G001076);
  eul_allocate_bytevector( G001106,G001105);
  eul_allocate_bytevector( G001108,G001107);
  eul_allocate_bytevector( G001110,G001109);
  eul_intern_symbol(sym_1113,"install-callback");
  eul_intern_symbol(sym_1114,"callback-thunk");
  eul_intern_symbol(sym_1115,"top-level");
  eul_allocate_bytevector( G001112,G001111);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 5; i++)
      callback_bindings[i] = eul_nil;
  }

  callback_bindings[ 5] = str_1073;
  callback_bindings[ 6] = str_1074;
  callback_bindings[ 7] = key_1075;
  callback_bindings[ 8] = G001072;
  callback_bindings[ 9] = str_1078;
  callback_bindings[ 10] = str_1079;
  callback_bindings[ 11] = str_1080;
  callback_bindings[ 12] = str_1081;
  callback_bindings[ 13] = str_1082;
  callback_bindings[ 14] = str_1083;
  callback_bindings[ 15] = str_1084;
  callback_bindings[ 16] = str_1085;
  callback_bindings[ 17] = str_1086;
  callback_bindings[ 18] = str_1087;
  callback_bindings[ 19] = str_1088;
  callback_bindings[ 20] = str_1089;
  callback_bindings[ 21] = str_1090;
  callback_bindings[ 22] = str_1091;
  callback_bindings[ 23] = str_1092;
  callback_bindings[ 24] = str_1093;
  callback_bindings[ 25] = str_1094;
  callback_bindings[ 26] = str_1095;
  callback_bindings[ 27] = str_1096;
  callback_bindings[ 28] = str_1097;
  callback_bindings[ 29] = str_1098;
  callback_bindings[ 30] = str_1099;
  callback_bindings[ 31] = str_1100;
  callback_bindings[ 32] = str_1101;
  callback_bindings[ 33] = str_1102;
  callback_bindings[ 34] = str_1103;
  callback_bindings[ 35] = sym_1104;
  callback_bindings[ 36] = G001077;
  callback_bindings[ 37] = G001106;
  callback_bindings[ 38] = G001108;
  callback_bindings[ 39] = G001110;
  callback_bindings[ 1] = eul_nil;
  callback_bindings[ 40] = sym_1113;
  callback_bindings[ 41] = sym_1114;
  callback_bindings[ 42] = sym_1115;
  eul_allocate_lambda( callback_bindings[0], "initialize-callback", 0, G001112);

  }
}


/* eof */