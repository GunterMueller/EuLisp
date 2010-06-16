/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module condition
 **  Copyright: See file condition.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_thread();
extern void initialize_module_dynamic();
extern void initialize_module_let_cc();
extern LispRef telos_bindings[];
extern LispRef mop_defcl_bindings[];
extern LispRef thread_bindings[];
extern LispRef let_cc_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot1_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_inspect_bindings[];

/* Module bindings with size 55 */
LispRef condition_bindings[55];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module condition */
void initialize_module_condition()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_thread();
  initialize_module_dynamic();
  initialize_module_let_cc();
  eul_fast_table_set(eul_modules,"condition",(LispRef) condition_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_998, sym_997, sym_996, sym_995, sym_994, G00993, G00991, G00989, G00987, G00985, G00982, G00979, G00977, sym_975, G00974, sym_972, G00971, G00969, sym_967, sym_966, sym_965, sym_964, sym_963, sym_961, key_960, sym_959, key_957, key_956, key_955, sym_954, key_953, sym_952, key_951, G00950, G00946, G00944, G00942, G00940, key_938, G00937;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 38 is_init: 0 index: 11 binding: error */
  static const void *G00936[] = {I(43,fd,1c,24),B(mop_inspect ,4),I(3c,01,1b,34),I(00,00,00,1d),I(1d,24,00,00),B(condition ,9),I(24,00,00,00),B(mop_inspect ,2),I(3c,02,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,32),I(24,00,00,00),B(mop_gf ,2),I(1f,04,23,00),B(condition ,10),I(1f,07,1f,06),I(24,00,00,00),B(boot ,9),I(3c,05,86,24),B(condition ,2),I(3d,02,05,32),I(00,00,00,3e),I(24,00,00,00),B(mop_gf ,17),I(86,1f,06,1f),I(06,1f,06,24),B(boot ,9),I(3c,05,24,00),B(condition ,9),I(23,00,00,00),B(condition ,10),I(1d,24,00,00),B(mop_gf ,2),I(3c,03,86,24),B(condition ,2),I(3d,02,06,22),I(01,45,05,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 12 binding: (method-condition?) */
  static const void *G00939[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-condition?) */
  static const void *G00941[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 14 binding: (setter-condition-message) */
  static const void *G00943[] = {I(ab,1c,82,1d),I(24,00,00,00),B(condition ,9),I(09,45,02,00)};

  eul_allocate_static_string(str_947, "***    Do you want to continue? (y/n) ", 38);
  eul_allocate_static_string(str_948, "***    See Backtrace? (y/n) ", 28);
  /* Byte-vector with size: 36 is_init: 0 index: 17 binding: default-error-handler */
  static const void *G00945[] = {I(ab,1c,24,00),B(condition ,4),I(3c,01,2a,1b),I(12,1b,34,00),I(00,00,00,0e),I(86,32,00,00),I(00,00,00,3a),I(84,23,00,00),B(condition ,15),I(24,00,00,00),B(mop_gf ,17),I(3c,02,2a,41),B(boot ,32),I(27,79,50,1b),I(34,00,00,00),I(00,00,00,14),I(1f,03,1f,03),I(3c,01,32,00),I(00,00,00,07),I(86,22,01,2a),I(84,23,00,00),B(condition ,16),I(24,00,00,00),B(mop_gf ,17),I(3c,02,2a,41),B(boot ,32),I(27,79,50,1b),I(34,00,00,00),I(00,00,00,18),I(24,00,00,00),B(boot ,13),I(3c,00,32,00),I(00,00,00,07),I(86,2a,85,24),B(boot1 ,19),I(3d,01,04,00)};

  eul_allocate_static_cons(cons_958, NULL, NULL);
  eul_allocate_static_cons(cons_962, NULL, NULL);
  /* Byte-vector with size: 173 is_init: 0 index: 35 binding: top-level */
  static const void *G00949[] = {I(a9,24,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,24),I(3c,01,23,00),B(condition ,18),I(23,00,00,00),B(condition ,19),I(23,00,00,00),B(condition ,20),I(23,00,00,00),B(condition ,10),I(24,00,00,00),B(boot1 ,24),I(3c,04,1b,24),B(boot1 ,24),I(3c,01,24,00),B(mop_class ,70),I(23,00,00,00),B(condition ,18),I(23,00,00,00),B(condition ,21),I(23,00,00,00),B(condition ,22),I(1f,06,23,00),B(condition ,23),I(1f,06,23,00),B(condition ,24),I(23,00,00,00),B(condition ,25),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(condition ,9),I(2a,24,00,00),B(condition ,9),I(24,00,00,00),B(boot1 ,24),I(3c,01,23,00),B(condition ,18),I(23,00,00,00),B(condition ,26),I(23,00,00,00),B(condition ,20),I(23,00,00,00),B(condition ,27),I(24,00,00,00),B(boot1 ,24),I(3c,04,1b,24),B(boot1 ,24),I(3c,01,24,00),B(mop_class ,70),I(23,00,00,00),B(condition ,18),I(23,00,00,00),B(condition ,28),I(23,00,00,00),B(condition ,22),I(1f,06,23,00),B(condition ,23),I(1f,06,23,00),B(condition ,24),I(23,00,00,00),B(condition ,29),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(condition ,5),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(condition ,30),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(condition ,8),I(2a,23,00,00),B(condition ,31),I(23,00,00,00),B(condition ,17),I(3b,02,89,00),B(condition ,3),I(2a,24,00,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(condition ,7),I(23,00,00,00),B(condition ,32),I(23,00,00,00),B(condition ,14),I(3b,02,1d,3c),I(02,2a,24,00),B(condition ,8),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(condition ,8),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(condition ,33),I(23,00,00,00),B(condition ,13),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(condition ,8),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(condition ,8),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(condition ,9),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(condition ,8),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(condition ,33),I(23,00,00,00),B(condition ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(condition ,8),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(condition ,8),I(2a,24,00,00),B(condition ,9),I(2a,24,00,00),B(condition ,5),I(2a,23,00,00),B(condition ,34),I(23,00,00,00),B(condition ,11),I(3b,fd,89,00),B(boot ,23),I(45,19,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 36 binding: anonymous */
  static const void *G00968[] = {I(ab,1c,48,02),I(00,1b,48,02),I(01,86,47,00),I(00,3d,01,02)};

  /* Byte-vector with size: 15 is_init: 0 index: 38 binding: call/ep-lambda */
  static const void *G00970[] = {I(aa,46,01,1b),I(48,00,00,23),B(condition ,37),I(23,00,00,00),B(condition ,36),I(3b,02,24,00),B(dynamic ,5),I(3c,01,2a,47),I(01,00,10,47),I(02,00,47,02),I(01,1d,3c,02),I(83,24,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,03,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 40 binding: anonymous */
  static const void *G00973[] = {I(aa,46,01,1b),I(48,00,00,47),I(00,00,12,1b),I(34,00,00,00),I(00,00,00,1c),I(47,01,00,47),I(01,01,24,00),B(condition ,3),I(3d,02,02,32),I(00,00,00,2b),I(23,00,00,00),B(condition ,39),I(23,00,00,00),B(condition ,38),I(3b,01,24,00),B(let_cc ,2),I(3c,01,47,00),I(00,11,47,01),I(02,3d,01,03),I(22,01,45,02)};

  /* Byte-vector with size: 25 is_init: 0 index: 41 binding: signal */
  static const void *G00976[] = {I(43,fd,46,04),I(1d,48,00,00),I(1c,48,00,01),I(1b,34,00,00),I(00,00,00,0f),I(1b,10,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,11),I(24,00,00,00),B(thread ,22),I(3c,00,26,00),I(00,00,00,03),I(24,00,00,00),B(thread ,8),I(08,86,1b,48),I(00,02,23,00),B(condition ,37),I(23,00,00,00),B(condition ,40),I(3b,01,48,00),I(02,1c,47,00),I(02,3d,01,06)};

  eul_allocate_static_string(str_980, "    ~a: ~a\n", 11);
  /* Byte-vector with size: 17 is_init: 0 index: 43 binding: anonymous */
  static const void *G00978[] = {I(aa,1b,34,00),I(00,00,00,3e),I(1b,10,1b,84),I(02,1c,47,00),I(00,24,00,00),B(mop_defcl ,10),I(3c,02,84,23),B(condition ,42),I(1f,03,1f,03),I(24,00,00,00),B(mop_gf ,17),I(3c,04,2a,1f),I(03,11,47,00),I(01,3d,01,04),I(22,03,32,00),I(00,00,00,07),I(86,45,01,00)};

  eul_allocate_static_string(str_983, "\n*** ERROR [~a]: ~a\n", 20);
  /* Byte-vector with size: 23 is_init: 0 index: 45 binding: output-condition-contents */
  static const void *G00981[] = {I(aa,46,03,1b),I(48,00,00,47),I(00,00,04,1b),I(82,02,47,00),I(00,82,24,00),B(condition ,9),I(08,86,1b,48),I(00,01,23,00),B(condition ,37),I(23,00,00,00),B(condition ,43),I(3b,01,48,00),I(01,84,23,00),B(condition ,44),I(1f,04,1f,04),I(24,00,00,00),B(mop_gf ,17),I(3c,04,2a,1f),I(03,26,00,00),I(00,00,00,04),I(02,1b,11,47),I(00,01,3d,01),I(06,45,06,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 46 binding: call/ep-lambda */
  static const void *G00984[] = {I(aa,24,00,00),B(mop_gf ,17),I(86,47,00,00),I(47,00,01,47),I(00,02,24,00),B(boot ,9),I(3c,05,24,00),B(condition ,9),I(23,00,00,00),B(condition ,10),I(1d,24,00,00),B(mop_gf ,2),I(3c,03,1b,1f),I(03,24,00,00),B(condition ,2),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 12 is_init: 0 index: 47 binding: call/ep-lambda */
  static const void *G00986[] = {I(aa,24,00,00),B(mop_gf ,2),I(47,00,01,23),B(condition ,10),I(47,00,00,47),I(00,02,24,00),B(boot ,9),I(3c,05,1b,1d),I(24,00,00,00),B(condition ,2),I(3d,02,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 33 is_init: 0 index: 48 binding: cerror */
  static const void *G00988[] = {I(43,fd,46,03),I(1d,48,00,00),I(1c,48,00,01),I(1b,48,00,02),I(47,00,01,24),B(mop_inspect ,4),I(3c,01,1b,34),I(00,00,00,1d),I(47,00,01,24),B(condition ,9),I(24,00,00,00),B(mop_inspect ,2),I(3c,02,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,2a),I(23,00,00,00),B(condition ,39),I(23,00,00,00),B(condition ,47),I(3b,01,24,00),B(let_cc ,2),I(3c,01,1b,22),I(01,32,00,00),I(00,00,00,24),I(23,00,00,00),B(condition ,39),I(23,00,00,00),B(condition ,46),I(3b,01,24,00),B(let_cc ,2),I(3c,01,1b,22),I(01,45,05,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 49 binding: condition-message */
  static const void *G00990[] = {I(aa,82,24,00),B(condition ,9),I(08,45,00,00)};

  /* Byte-vector with size: 60 is_init: 1 index: 0 binding: initialize-condition */
  static const void *G00992[] = {I(87,25,00,00),B(condition ,1),I(24,00,00,00),B(let_cc ,1),I(3e,0b,24,00),B(let_cc ,0),I(3c,00,21,01),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(condition ,9),I(86,25,00,00),B(condition ,8),I(23,00,00,00),B(condition ,50),I(23,00,00,00),B(condition ,49),I(3b,01,25,00),B(condition ,7),I(23,00,00,00),B(condition ,51),I(23,00,00,00),B(condition ,48),I(3b,fd,25,00),B(condition ,6),I(86,25,00,00),B(condition ,5),I(23,00,00,00),B(condition ,52),I(23,00,00,00),B(condition ,45),I(3b,01,25,00),B(condition ,4),I(86,25,00,00),B(condition ,3),I(23,00,00,00),B(condition ,53),I(23,00,00,00),B(condition ,41),I(3b,fd,25,00),B(condition ,2),I(23,00,00,00),B(condition ,54),I(23,00,00,00),B(condition ,35),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_keyword(key_938,"message");
  eul_allocate_bytevector( G00937,G00936);
  eul_allocate_bytevector( G00940,G00939);
  eul_allocate_bytevector( G00942,G00941);
  eul_allocate_bytevector( G00944,G00943);
  object_class(str_947) = eul_static_string_class;
  object_class(str_948) = eul_static_string_class;
  eul_allocate_bytevector( G00946,G00945);
  eul_intern_keyword(key_951,"name");
  eul_intern_symbol(sym_952,"message");
  eul_intern_keyword(key_953,"keyword");
  eul_intern_symbol(sym_954,"condition");
  eul_intern_keyword(key_955,"direct-superclasses");
  eul_intern_keyword(key_956,"direct-slots");
  eul_intern_keyword(key_957,"direct-keywords");
  object_class(cons_958) = eul_static_cons_class;
  eul_car(cons_958) = key_938;
  eul_cdr(cons_958) = eul_nil;
  eul_intern_symbol(sym_959,"arguments");
  eul_intern_keyword(key_960,"arguments");
  eul_intern_symbol(sym_961,"general-condition");
  object_class(cons_962) = eul_static_cons_class;
  eul_car(cons_962) = key_960;
  eul_cdr(cons_962) = eul_nil;
  eul_intern_symbol(sym_963,"condition?");
  eul_intern_symbol(sym_964,"default-error-handler");
  eul_intern_symbol(sym_965,"(setter condition-message)");
  eul_intern_symbol(sym_966,"(method condition?)");
  eul_intern_symbol(sym_967,"error");
  eul_allocate_bytevector( G00950,G00949);
  eul_allocate_bytevector( G00969,G00968);
  eul_intern_symbol(sym_972,"anonymous");
  eul_allocate_bytevector( G00971,G00970);
  eul_intern_symbol(sym_975,"call/ep-lambda");
  eul_allocate_bytevector( G00974,G00973);
  eul_allocate_bytevector( G00977,G00976);
  object_class(str_980) = eul_static_string_class;
  eul_allocate_bytevector( G00979,G00978);
  object_class(str_983) = eul_static_string_class;
  eul_allocate_bytevector( G00982,G00981);
  eul_allocate_bytevector( G00985,G00984);
  eul_allocate_bytevector( G00987,G00986);
  eul_allocate_bytevector( G00989,G00988);
  eul_allocate_bytevector( G00991,G00990);
  eul_intern_symbol(sym_994,"condition-message");
  eul_intern_symbol(sym_995,"cerror");
  eul_intern_symbol(sym_996,"output-condition-contents");
  eul_intern_symbol(sym_997,"signal");
  eul_intern_symbol(sym_998,"top-level");
  eul_allocate_bytevector( G00993,G00992);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 10; i++)
      condition_bindings[i] = eul_nil;
  }

  condition_bindings[ 10] = key_938;
  condition_bindings[ 11] = G00937;
  condition_bindings[ 12] = G00940;
  condition_bindings[ 13] = G00942;
  condition_bindings[ 14] = G00944;
  condition_bindings[ 15] = str_947;
  condition_bindings[ 16] = str_948;
  condition_bindings[ 17] = G00946;
  condition_bindings[ 18] = key_951;
  condition_bindings[ 19] = sym_952;
  condition_bindings[ 20] = key_953;
  condition_bindings[ 21] = sym_954;
  condition_bindings[ 22] = key_955;
  condition_bindings[ 23] = key_956;
  condition_bindings[ 24] = key_957;
  condition_bindings[ 25] = cons_958;
  condition_bindings[ 26] = sym_959;
  condition_bindings[ 27] = key_960;
  condition_bindings[ 28] = sym_961;
  condition_bindings[ 29] = cons_962;
  condition_bindings[ 30] = sym_963;
  condition_bindings[ 31] = sym_964;
  condition_bindings[ 32] = sym_965;
  condition_bindings[ 33] = sym_966;
  condition_bindings[ 34] = sym_967;
  condition_bindings[ 35] = G00950;
  condition_bindings[ 36] = G00969;
  condition_bindings[ 37] = sym_972;
  condition_bindings[ 38] = G00971;
  condition_bindings[ 39] = sym_975;
  condition_bindings[ 40] = G00974;
  condition_bindings[ 41] = G00977;
  condition_bindings[ 42] = str_980;
  condition_bindings[ 43] = G00979;
  condition_bindings[ 44] = str_983;
  condition_bindings[ 45] = G00982;
  condition_bindings[ 46] = G00985;
  condition_bindings[ 47] = G00987;
  condition_bindings[ 48] = G00989;
  condition_bindings[ 49] = G00991;
  condition_bindings[ 1] = eul_nil;
  condition_bindings[ 50] = sym_994;
  condition_bindings[ 51] = sym_995;
  condition_bindings[ 52] = sym_996;
  condition_bindings[ 53] = sym_997;
  condition_bindings[ 54] = sym_998;
  eul_allocate_lambda( condition_bindings[0], "initialize-condition", 0, G00993);

  }
}


/* eof */
