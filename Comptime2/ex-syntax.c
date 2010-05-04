/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module ex-syntax
 **  Copyright: See file ex-syntax.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_i_modify();
extern void initialize_module_p_env();
extern void initialize_module_sx_obj();
extern void initialize_module_sx_node();
extern void initialize_module_cg_interf();
extern LispRef cg_interf_bindings[];
extern LispRef i_modify_bindings[];
extern LispRef i_all_bindings[];
extern LispRef thread_bindings[];
extern LispRef convert_bindings[];
extern LispRef collect_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef number_bindings[];
extern LispRef sx_obj_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot_bindings[];
extern LispRef i_error_bindings[];
extern LispRef i_param_bindings[];
extern LispRef p_env_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef sx_node_bindings[];

/* Module bindings with size 75 */
LispRef ex_syntax_bindings[75];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module ex-syntax */
void initialize_module_ex_syntax()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_i_modify();
  initialize_module_p_env();
  initialize_module_sx_obj();
  initialize_module_sx_node();
  initialize_module_cg_interf();
  eul_fast_table_set(eul_modules,"ex_syntax",(LispRef) ex_syntax_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4998, sym_4997, sym_4996, sym_4995, sym_4994, sym_4993, sym_4992, sym_4991, sym_4990, G004989, G004987, G004985, G004982, G004980, G004976, G004974, G004972, G004970, G004968, sym_4966, G004964, sym_4962, G004960, G004958, G004956, G004953, sym_4951, sym_4950, sym_4949, sym_4948, G004947, sym_4945, G004944, G004941, G004939, G004937, sym_4935, G004934, G004931, G004929, G004927, sym_4925, G004924, G004921, G004919, G004917, G004915, sym_4913, sym_4912, G004911, key_4909, G004907, G004905, G004903, G004901;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 18 is_init: 0 index: 11 binding: anonymous */
  static const void *G004900[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_syntax ,2),I(3c,02,24,00),B(sx_node ,20),I(3c,01,47,00),I(02,1d,24,00),B(ex_syntax ,5),I(3c,02,1c,1c),I(1c,26,00,00),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,44),I(09,22,02,2a),I(1c,24,00,00),B(p_env ,15),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 12 binding: anonymous */
  static const void *G004902[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_syntax ,2),I(3c,02,24,00),B(p_env ,15),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-G004743) */
  static const void *G004904[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4908, "bad syntax prefix syntax", 24);
  /* Byte-vector with size: 16 is_init: 0 index: 16 binding: (method-G004743) */
  static const void *G004906[] = {I(ab,24,00,00),B(i_param ,56),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_syntax ,14),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_syntax ,15),I(47,00,00,24),B(boot ,22),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 109 is_init: 0 index: 19 binding: anonymous */
  static const void *G004910[] = {I(ab,46,07,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(ex_syntax ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,18),I(23,00,00,00),B(ex_syntax ,16),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,18),I(23,00,00,00),B(ex_syntax ,13),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,77,24),B(p_env ,13),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,60),I(08,47,00,00),I(73,1b,48,00),I(02,1c,24,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,27),I(1c,24,00,00),B(boot ,15),I(3c,02,47,00),I(00,76,24,00),B(number ,7),I(3c,02,23,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,12),I(3b,01,1c,24),B(boot ,4),I(3c,02,2a,47),I(00,00,76,23),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,11),I(3b,01,1c,24),B(boot ,4),I(3c,02,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,1b,00,00)};

  /* Byte-vector with size: 15 is_init: 0 index: 20 binding: anonymous */
  static const void *G004914[] = {I(aa,1b,10,1b),I(47,00,01,24),B(ex_syntax ,2),I(3c,02,24,00),B(sx_node ,20),I(3c,01,1d,73),I(1c,1c,1c,26),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,44),I(09,22,02,2a),I(1c,24,00,00),B(p_env ,15),I(3d,01,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 21 binding: anonymous */
  static const void *G004916[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_syntax ,2),I(3c,02,24,00),B(p_env ,15),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 22 binding: (method-G004714) */
  static const void *G004918[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4922, "bad syntax rename syntax", 24);
  /* Byte-vector with size: 16 is_init: 0 index: 24 binding: (method-G004714) */
  static const void *G004920[] = {I(ab,24,00,00),B(i_param ,56),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_syntax ,23),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_syntax ,15),I(47,00,00,24),B(boot ,22),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 112 is_init: 0 index: 26 binding: anonymous */
  static const void *G004923[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(ex_syntax ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,25),I(23,00,00,00),B(ex_syntax ,24),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,25),I(23,00,00,00),B(ex_syntax ,22),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,24),B(p_env ,13),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,60),I(08,24,00,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,27),I(1c,24,00,00),B(boot ,15),I(3c,02,47,00),I(00,73,24,00),B(boot1 ,31),I(1c,24,00,00),B(boot ,15),I(3c,02,1d,1c),I(24,00,00,00),B(number ,7),I(3c,02,23,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,21),I(3b,01,1c,24),B(boot ,4),I(3c,02,2a,47),I(00,00,73,23),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,20),I(3b,01,1c,24),B(boot ,4),I(3c,02,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,1c,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 27 binding: anonymous */
  static const void *G004926[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_syntax ,2),I(3c,02,24,00),B(p_env ,15),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 28 binding: (method-G004689) */
  static const void *G004928[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4932, "bad syntax except syntax", 24);
  /* Byte-vector with size: 16 is_init: 0 index: 30 binding: (method-G004689) */
  static const void *G004930[] = {I(ab,24,00,00),B(i_param ,56),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_syntax ,29),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_syntax ,15),I(47,00,00,24),B(boot ,22),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 100 is_init: 0 index: 32 binding: anonymous */
  static const void *G004933[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(ex_syntax ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,31),I(23,00,00,00),B(ex_syntax ,30),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,31),I(23,00,00,00),B(ex_syntax ,28),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,24),B(p_env ,13),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,60),I(08,1b,24,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,27),I(1c,24,00,00),B(boot ,15),I(3c,02,47,00),I(00,73,24,00),B(number ,7),I(3c,02,23,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,27),I(3b,01,1c,24),B(boot ,4),I(3c,02,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,19,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 33 binding: anonymous */
  static const void *G004936[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_syntax ,2),I(3c,02,24,00),B(p_env ,15),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 34 binding: (method-G004666) */
  static const void *G004938[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4942, "bad syntax only syntax", 22);
  /* Byte-vector with size: 16 is_init: 0 index: 36 binding: (method-G004666) */
  static const void *G004940[] = {I(ab,24,00,00),B(i_param ,56),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_syntax ,35),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_syntax ,15),I(47,00,00,24),B(boot ,22),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 87 is_init: 0 index: 38 binding: anonymous */
  static const void *G004943[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(ex_syntax ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,37),I(23,00,00,00),B(ex_syntax ,36),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,37),I(23,00,00,00),B(ex_syntax ,34),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,24),B(p_env ,13),I(3c,01,1b,48),I(00,01,47,00),I(00,73,23,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,33),I(3b,01,1c,24),B(boot ,4),I(3c,02,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,17,00,00)};

  /* Byte-vector with size: 38 is_init: 0 index: 43 binding: top-level */
  static const void *G004946[] = {I(a9,24,00,00),B(aux_table ,3),I(3c,00,1b,89),B(ex_syntax ,10),I(2a,23,00,00),B(ex_syntax ,39),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,38),I(3b,02,24,00),B(ex_syntax ,8),I(3c,02,2a,23),B(ex_syntax ,40),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,32),I(3b,02,24,00),B(ex_syntax ,8),I(3c,02,2a,23),B(ex_syntax ,41),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,26),I(3b,02,24,00),B(ex_syntax ,8),I(3c,02,2a,23),B(ex_syntax ,42),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,19),I(3b,02,24,00),B(ex_syntax ,8),I(3d,02,01,45),I(01,00,00,00)};

  eul_allocate_static_string(str_4954, "external syntax binding ~a not available in module", 50);
  /* Byte-vector with size: 23 is_init: 0 index: 45 binding: import-syntax-binding */
  static const void *G004952[] = {I(ab,1c,1c,24),B(p_env ,19),I(3c,02,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,27),I(1d,24,00,00),B(sx_node ,5),I(3c,01,1b,23),B(ex_syntax ,44),I(1f,05,1f,05),I(24,00,00,00),B(i_notify ,6),I(3c,04,22,01),I(1b,87,1c,26),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,44),I(09,22,02,2a),I(1d,24,00,00),B(ex_syntax ,9),I(3c,01,2a,1b),I(45,04,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 46 binding: anonymous */
  static const void *G004955[] = {I(ab,1b,7a,1b),I(34,00,00,00),I(00,00,00,28),I(1d,41,00,00),B(aux_table ,10),I(22,01,1d,1c),I(24,00,00,00),B(p_env ,15),I(3d,02,04,22),I(01,32,00,00),I(00,00,00,26),I(1c,24,00,00),B(p_env ,15),I(3c,01,2a,1c),I(87,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,44),I(09,22,02,45),I(03,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 47 binding: (method-G004619) */
  static const void *G004957[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4961, "cannot import syntax module ~a", 30);
  /* Byte-vector with size: 22 is_init: 0 index: 50 binding: (method-G004619) */
  static const void *G004959[] = {I(ab,24,00,00),B(i_param ,56),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,44),I(86,23,00,00),B(ex_syntax ,48),I(47,00,00,24),B(mop_gf ,17),I(3c,03,23,00),B(ex_syntax ,49),I(24,00,00,00),B(dynamic ,3),I(3c,01,1c,24),B(i_error ,5),I(23,00,00,00),B(ex_syntax ,15),I(1f,03,24,00),B(boot ,22),I(3d,04,04,22),I(02,45,02,00)};

  eul_allocate_static_string(str_4965, "  Import syntax module ~a ...", 29);
  /* Byte-vector with size: 102 is_init: 0 index: 53 binding: import-syntax-module */
  static const void *G004963[] = {I(aa,46,01,1b),I(48,00,00,23),B(ex_syntax ,51),I(47,00,00,24),B(i_notify ,4),I(3c,02,2a,84),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(ex_syntax ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,52),I(23,00,00,00),B(ex_syntax ,50),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(ex_syntax ,52),I(23,00,00,00),B(ex_syntax ,47),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,24,00),B(sx_obj1 ,7),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,11),I(47,00,00,24),B(p_env ,13),I(3c,01,1b,26),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,60),I(08,23,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,46),I(3b,02,1c,24),B(aux_table ,7),I(3c,02,2a,1c),I(24,00,00,00),B(ex_syntax ,9),I(3c,01,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,17,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 54 binding: expand-syntax-import */
  static const void *G004967[] = {I(aa,24,00,00),B(ex_syntax ,6),I(24,00,00,00),B(ex_syntax ,6),I(3d,02,00,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 55 binding: make-prefix */
  static const void *G004969[] = {I(ab,1c,82,02),I(1c,82,02,1c),I(1c,24,00,00),B(collect ,19),I(3c,02,24,00),B(mop_class ,6),I(24,00,00,00),B(convert ,2),I(3d,02,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 56 binding: anonymous */
  static const void *G004971[] = {I(ab,1c,45,02)};

  /* Byte-vector with size: 3 is_init: 0 index: 57 binding: anonymous */
  static const void *G004973[] = {I(ab,1c,24,00),B(ex_syntax ,3),I(3d,01,02,00)};

  eul_allocate_static_string(str_4977, "no syntax-import expander ~a available", 38);
  eul_allocate_static_string(str_4978, "no syntax-import expander ~a available", 38);
  /* Byte-vector with size: 43 is_init: 0 index: 60 binding: syntax-import-expander */
  static const void *G004975[] = {I(ab,1c,7c,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,57),I(3b,02,32,00),I(00,00,00,84),I(1d,7a,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,56),I(3b,02,32,00),I(00,00,00,5e),I(1f,03,10,7c),I(1b,34,00,00),I(00,00,00,3f),I(1f,04,10,24),B(ex_syntax ,10),I(3c,01,1b,1b),I(34,00,00,00),I(00,00,00,10),I(1b,32,00,00),I(00,00,00,19),I(23,00,00,00),B(ex_syntax ,58),I(1f,07,24,00),B(boot ,22),I(3c,02,22,02),I(32,00,00,00),I(00,00,00,1a),I(23,00,00,00),B(ex_syntax ,59),I(1f,05,24,00),B(boot ,22),I(3c,02,22,01),I(22,01,1f,03),I(1f,03,1d,3d),I(02,04,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 61 binding: expand-old-syntax-imports */
  static const void *G004979[] = {I(aa,24,00,00),B(ex_syntax ,4),I(1c,24,00,00),B(boot ,15),I(3d,02,01,00)};

  eul_allocate_static_string(str_4983, "redefinition of expander ~a", 27);
  /* Byte-vector with size: 17 is_init: 0 index: 63 binding: install-syntax-import-expander */
  static const void *G004981[] = {I(ab,1c,24,00),B(ex_syntax ,10),I(3c,01,1b,34),I(00,00,00,1d),I(86,23,00,00),B(ex_syntax ,62),I(1f,04,24,00),B(i_notify ,5),I(3c,03,32,00),I(00,00,00,07),I(86,2a,24,00),B(ex_syntax ,10),I(24,00,00,00),B(boot1 ,41),I(3c,01,1f,03),I(1f,03,1d,3d),I(02,04,00,00)};

  /* Byte-vector with size: 41 is_init: 0 index: 64 binding: anonymous */
  static const void *G004984[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,44),I(23,00,00,00),B(ex_syntax ,49),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,84),I(24,00,00,00),B(sx_obj1 ,60),I(08,1b,47,00),I(00,24,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,84,1d,24),B(sx_obj1 ,60),I(09,22,05,32),I(00,00,00,5e),I(1c,10,1b,24),B(sx_obj1 ,7),I(3c,01,1b,34),I(00,00,00,29),I(1c,47,00,00),I(50,1b,34,00),I(00,00,00,0e),I(1d,32,00,00),I(00,00,00,10),I(1f,04,11,47),I(00,01,3d,01),I(05,22,01,32),I(00,00,00,28),I(1c,47,00,02),I(50,1b,34,00),I(00,00,00,12),I(1f,04,47,00),I(00,8f,32,00),I(00,00,00,0f),I(1f,04,11,47),I(00,01,3d,01),I(05,22,01,22),I(02,45,02,00)};

  /* Byte-vector with size: 29 is_init: 0 index: 65 binding: register-imported-syntax-module */
  static const void *G004986[] = {I(aa,46,04,1b),I(48,00,00,47),I(00,00,24,00),B(sx_obj1 ,7),I(3c,01,1b,34),I(00,00,00,1d),I(47,00,00,26),I(00,00,00,15),I(24,00,00,00),B(sx_obj1 ,60),I(08,32,00,00),I(00,00,00,0a),I(47,00,00,1b),I(48,00,02,86),I(1b,48,00,01),I(23,00,00,00),B(ex_syntax ,17),I(23,00,00,00),B(ex_syntax ,64),I(3b,01,48,00),I(01,23,00,00),B(ex_syntax ,49),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,84),I(24,00,00,00),B(sx_obj1 ,60),I(08,47,00,01),I(3d,01,05,00)};

  /* Byte-vector with size: 88 is_init: 1 index: 0 binding: initialize-ex-syntax */
  static const void *G004988[] = {I(87,25,00,00),B(ex_syntax ,1),I(24,00,00,00),B(cg_interf ,1),I(3e,0b,24,00),B(cg_interf ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_node ,1),I(3e,0b,24,00),B(sx_node ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(p_env ,1),I(3e,0b,24,00),B(p_env ,0),I(3c,00,21,01),I(24,00,00,00),B(i_modify ,1),I(3e,0b,24,00),B(i_modify ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(86,25,00,00),B(ex_syntax ,10),I(23,00,00,00),B(ex_syntax ,66),I(23,00,00,00),B(ex_syntax ,65),I(3b,01,25,00),B(ex_syntax ,9),I(23,00,00,00),B(ex_syntax ,67),I(23,00,00,00),B(ex_syntax ,63),I(3b,02,25,00),B(ex_syntax ,8),I(23,00,00,00),B(ex_syntax ,68),I(23,00,00,00),B(ex_syntax ,61),I(3b,01,25,00),B(ex_syntax ,7),I(23,00,00,00),B(ex_syntax ,69),I(23,00,00,00),B(ex_syntax ,60),I(3b,02,25,00),B(ex_syntax ,6),I(23,00,00,00),B(ex_syntax ,70),I(23,00,00,00),B(ex_syntax ,55),I(3b,02,25,00),B(ex_syntax ,5),I(23,00,00,00),B(ex_syntax ,71),I(23,00,00,00),B(ex_syntax ,54),I(3b,01,25,00),B(ex_syntax ,4),I(23,00,00,00),B(ex_syntax ,72),I(23,00,00,00),B(ex_syntax ,53),I(3b,01,25,00),B(ex_syntax ,3),I(23,00,00,00),B(ex_syntax ,73),I(23,00,00,00),B(ex_syntax ,45),I(3b,02,25,00),B(ex_syntax ,2),I(23,00,00,00),B(ex_syntax ,74),I(23,00,00,00),B(ex_syntax ,43),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G004901,G004900);
  eul_allocate_bytevector( G004903,G004902);
  eul_allocate_bytevector( G004905,G004904);
  object_class(str_4908) = eul_static_string_class;
  eul_intern_keyword(key_4909,"ct-error-value");
  eul_allocate_bytevector( G004907,G004906);
  eul_intern_symbol(sym_4912,"anonymous");
  eul_intern_symbol(sym_4913,"(method G004743)");
  eul_allocate_bytevector( G004911,G004910);
  eul_allocate_bytevector( G004915,G004914);
  eul_allocate_bytevector( G004917,G004916);
  eul_allocate_bytevector( G004919,G004918);
  object_class(str_4922) = eul_static_string_class;
  eul_allocate_bytevector( G004921,G004920);
  eul_intern_symbol(sym_4925,"(method G004714)");
  eul_allocate_bytevector( G004924,G004923);
  eul_allocate_bytevector( G004927,G004926);
  eul_allocate_bytevector( G004929,G004928);
  object_class(str_4932) = eul_static_string_class;
  eul_allocate_bytevector( G004931,G004930);
  eul_intern_symbol(sym_4935,"(method G004689)");
  eul_allocate_bytevector( G004934,G004933);
  eul_allocate_bytevector( G004937,G004936);
  eul_allocate_bytevector( G004939,G004938);
  object_class(str_4942) = eul_static_string_class;
  eul_allocate_bytevector( G004941,G004940);
  eul_intern_symbol(sym_4945,"(method G004666)");
  eul_allocate_bytevector( G004944,G004943);
  eul_intern_symbol(sym_4948,"only");
  eul_intern_symbol(sym_4949,"except");
  eul_intern_symbol(sym_4950,"rename");
  eul_intern_symbol(sym_4951,"prefix");
  eul_allocate_bytevector( G004947,G004946);
  object_class(str_4954) = eul_static_string_class;
  eul_allocate_bytevector( G004953,G004952);
  eul_allocate_bytevector( G004956,G004955);
  eul_allocate_bytevector( G004958,G004957);
  object_class(str_4961) = eul_static_string_class;
  eul_intern_symbol(sym_4962,"*actual-module*");
  eul_allocate_bytevector( G004960,G004959);
  object_class(str_4965) = eul_static_string_class;
  eul_intern_symbol(sym_4966,"(method G004619)");
  eul_allocate_bytevector( G004964,G004963);
  eul_allocate_bytevector( G004968,G004967);
  eul_allocate_bytevector( G004970,G004969);
  eul_allocate_bytevector( G004972,G004971);
  eul_allocate_bytevector( G004974,G004973);
  object_class(str_4977) = eul_static_string_class;
  object_class(str_4978) = eul_static_string_class;
  eul_allocate_bytevector( G004976,G004975);
  eul_allocate_bytevector( G004980,G004979);
  object_class(str_4983) = eul_static_string_class;
  eul_allocate_bytevector( G004982,G004981);
  eul_allocate_bytevector( G004985,G004984);
  eul_allocate_bytevector( G004987,G004986);
  eul_intern_symbol(sym_4990,"register-imported-syntax-module");
  eul_intern_symbol(sym_4991,"install-syntax-import-expander");
  eul_intern_symbol(sym_4992,"expand-old-syntax-imports");
  eul_intern_symbol(sym_4993,"syntax-import-expander");
  eul_intern_symbol(sym_4994,"make-prefix");
  eul_intern_symbol(sym_4995,"expand-syntax-import");
  eul_intern_symbol(sym_4996,"import-syntax-module");
  eul_intern_symbol(sym_4997,"import-syntax-binding");
  eul_intern_symbol(sym_4998,"top-level");
  eul_allocate_bytevector( G004989,G004988);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      ex_syntax_bindings[i] = eul_nil;
  }

  ex_syntax_bindings[ 11] = G004901;
  ex_syntax_bindings[ 12] = G004903;
  ex_syntax_bindings[ 13] = G004905;
  ex_syntax_bindings[ 14] = str_4908;
  ex_syntax_bindings[ 15] = key_4909;
  ex_syntax_bindings[ 16] = G004907;
  ex_syntax_bindings[ 17] = sym_4912;
  ex_syntax_bindings[ 18] = sym_4913;
  ex_syntax_bindings[ 19] = G004911;
  ex_syntax_bindings[ 20] = G004915;
  ex_syntax_bindings[ 21] = G004917;
  ex_syntax_bindings[ 22] = G004919;
  ex_syntax_bindings[ 23] = str_4922;
  ex_syntax_bindings[ 24] = G004921;
  ex_syntax_bindings[ 25] = sym_4925;
  ex_syntax_bindings[ 26] = G004924;
  ex_syntax_bindings[ 27] = G004927;
  ex_syntax_bindings[ 28] = G004929;
  ex_syntax_bindings[ 29] = str_4932;
  ex_syntax_bindings[ 30] = G004931;
  ex_syntax_bindings[ 31] = sym_4935;
  ex_syntax_bindings[ 32] = G004934;
  ex_syntax_bindings[ 33] = G004937;
  ex_syntax_bindings[ 34] = G004939;
  ex_syntax_bindings[ 35] = str_4942;
  ex_syntax_bindings[ 36] = G004941;
  ex_syntax_bindings[ 37] = sym_4945;
  ex_syntax_bindings[ 38] = G004944;
  ex_syntax_bindings[ 39] = sym_4948;
  ex_syntax_bindings[ 40] = sym_4949;
  ex_syntax_bindings[ 41] = sym_4950;
  ex_syntax_bindings[ 42] = sym_4951;
  ex_syntax_bindings[ 43] = G004947;
  ex_syntax_bindings[ 44] = str_4954;
  ex_syntax_bindings[ 45] = G004953;
  ex_syntax_bindings[ 46] = G004956;
  ex_syntax_bindings[ 47] = G004958;
  ex_syntax_bindings[ 48] = str_4961;
  ex_syntax_bindings[ 49] = sym_4962;
  ex_syntax_bindings[ 50] = G004960;
  ex_syntax_bindings[ 51] = str_4965;
  ex_syntax_bindings[ 52] = sym_4966;
  ex_syntax_bindings[ 53] = G004964;
  ex_syntax_bindings[ 54] = G004968;
  ex_syntax_bindings[ 55] = G004970;
  ex_syntax_bindings[ 56] = G004972;
  ex_syntax_bindings[ 57] = G004974;
  ex_syntax_bindings[ 58] = str_4977;
  ex_syntax_bindings[ 59] = str_4978;
  ex_syntax_bindings[ 60] = G004976;
  ex_syntax_bindings[ 61] = G004980;
  ex_syntax_bindings[ 62] = str_4983;
  ex_syntax_bindings[ 63] = G004982;
  ex_syntax_bindings[ 64] = G004985;
  ex_syntax_bindings[ 65] = G004987;
  ex_syntax_bindings[ 1] = eul_nil;
  ex_syntax_bindings[ 66] = sym_4990;
  ex_syntax_bindings[ 67] = sym_4991;
  ex_syntax_bindings[ 68] = sym_4992;
  ex_syntax_bindings[ 69] = sym_4993;
  ex_syntax_bindings[ 70] = sym_4994;
  ex_syntax_bindings[ 71] = sym_4995;
  ex_syntax_bindings[ 72] = sym_4996;
  ex_syntax_bindings[ 73] = sym_4997;
  ex_syntax_bindings[ 74] = sym_4998;
  eul_allocate_lambda( ex_syntax_bindings[0], "initialize-ex-syntax", 0, G004989);

  }
}


/* eof */
