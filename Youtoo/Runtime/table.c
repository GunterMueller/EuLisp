/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module table
 **  Copyright: See file table.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_copy();
extern void initialize_module_collect();
extern void initialize_module_compare();
extern void initialize_module_list();
extern void initialize_module_fpi();
extern void initialize_module_string();
extern void initialize_module_vector();
extern void initialize_module_table1();
extern LispRef fpi_bindings[];
extern LispRef compare_bindings[];
extern LispRef copy_bindings[];
extern LispRef convert_bindings[];
extern LispRef telos_bindings[];
extern LispRef list_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef string_bindings[];
extern LispRef boot1_bindings[];
extern LispRef vector_bindings[];
extern LispRef boot_bindings[];
extern LispRef condition_bindings[];
extern LispRef collect_bindings[];
extern LispRef table1_bindings[];

/* Module bindings with size 84 */
LispRef table_bindings[84];

/* Foreign functions */
static LispRef ff_stub_eul_table_ref8470 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008583, G008584, res;

  POPVAL1(G008584);
  POPVAL1(G008583);
  FF_RES_CONVERT6(res,eul_table_ref(FF_ARG_CONVERT8(G008583), FF_ARG_CONVERT3(G008584)));
  return res;
}

static LispRef ff_stub_eul_addr_str8471 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008585, res;

  POPVAL1(G008585);
  FF_RES_CONVERT3(res,eul_addr_str(FF_ARG_CONVERT8(G008585)));
  return res;
}

static LispRef ff_stub_eul_table_set8472 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008586, G008587, G008588, res;

  POPVAL1(G008588);
  POPVAL1(G008587);
  POPVAL1(G008586);
  FF_RES_CONVERT6(res,eul_table_set(FF_ARG_CONVERT8(G008586), FF_ARG_CONVERT3(G008587), FF_ARG_CONVERT8(G008588)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module table */
void initialize_module_table()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_copy();
  initialize_module_collect();
  initialize_module_compare();
  initialize_module_list();
  initialize_module_fpi();
  initialize_module_string();
  initialize_module_vector();
  initialize_module_table1();
  eul_fast_table_set(eul_modules,"table",(LispRef) table_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_8582, sym_8581, sym_8580, sym_8579, sym_8578, sym_8577, sym_8576, sym_8575, sym_8574, G008573, G008571, G008569, G008567, G008565, G008563, G008561, G008559, G008557, G008555, G008553, G008551, G008549, G008547, G008545, sym_8543, sym_8541, sym_8539, sym_8538, sym_8537, sym_8536, sym_8535, sym_8534, sym_8533, sym_8532, sym_8531, sym_8529, sym_8527, sym_8526, sym_8525, sym_8524, G008523, G008521, G008519, G008516, G008514, G008512, G008510, G008508, G008506, G008503, G008501, G008499, G008497, G008495, G008492, G008489, G008486, G008483, sym_8481, G008480, G008478, G008476, G008474;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 3 is_init: 0 index: 14 binding: (method-accumulate1) */
  static const void *G008473[] = {I(ab,24,00,00),B(table ,10),I(3d,02,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 15 binding: (method-accumulate) */
  static const void *G008475[] = {I(43,03,24,00),B(table ,4),I(3d,03,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 16 binding: anonymous */
  static const void *G008477[] = {I(aa,1b,7a,1b),I(44,14,1c,10),I(1d,11,47,00),I(00,1d,1d,47),I(00,01,3d,03),I(04,22,02,36),I(02,86,45,02)};

  /* Byte-vector with size: 20 is_init: 0 index: 18 binding: (method-member) */
  static const void *G008479[] = {I(43,fd,46,02),I(1d,48,00,00),I(1b,12,1b,44),I(11,1d,47,00),I(00,24,00,00),B(table ,7),I(3d,02,04,36),I(31,1c,10,1b),I(48,00,01,1f),I(03,8a,03,24),B(table1 ,3),I(08,47,00,00),I(1c,23,00,00),B(table ,17),I(23,00,00,00),B(table ,16),I(3b,01,24,00),B(collect ,11),I(3d,03,06,22),I(02,45,04,00)};

  eul_allocate_static_string(str_8484, "all? on multiple tables not yet implemented", 43);
  /* Byte-vector with size: 13 is_init: 0 index: 20 binding: (method-all?) */
  static const void *G008482[] = {I(43,fd,12,1b),I(44,10,1d,1d),I(24,00,00,00),B(table ,5),I(3d,02,03,36),I(1b,24,00,00),B(condition ,8),I(23,00,00,00),B(table ,19),I(24,00,00,00),B(boot ,13),I(3d,02,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8487, "any? on multiple tables not yet implemented", 43);
  /* Byte-vector with size: 13 is_init: 0 index: 22 binding: (method-any?) */
  static const void *G008485[] = {I(43,fd,12,1b),I(44,10,1d,1d),I(24,00,00,00),B(table ,8),I(3d,02,03,36),I(1b,24,00,00),B(condition ,8),I(23,00,00,00),B(table ,21),I(24,00,00,00),B(boot ,13),I(3d,02,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8490, "map on multiple tables not yet implemented", 42);
  /* Byte-vector with size: 13 is_init: 0 index: 24 binding: (method-map) */
  static const void *G008488[] = {I(43,fd,12,1b),I(44,10,1d,1d),I(24,00,00,00),B(table ,2),I(3d,02,03,36),I(1b,24,00,00),B(condition ,8),I(23,00,00,00),B(table ,23),I(24,00,00,00),B(boot ,13),I(3d,02,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8493, "do on multiple tables not yet implemented", 41);
  /* Byte-vector with size: 13 is_init: 0 index: 26 binding: (method-do) */
  static const void *G008491[] = {I(43,fd,12,1b),I(44,10,1d,1d),I(24,00,00,00),B(table ,9),I(3d,02,03,36),I(1b,24,00,00),B(condition ,8),I(23,00,00,00),B(table ,25),I(24,00,00,00),B(boot ,13),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 27 binding: (method-emptyp) */
  static const void *G008494[] = {I(aa,24,00,00),B(table ,6),I(3d,01,00,00)};

  /* Byte-vector with size: 43 is_init: 0 index: 28 binding: anonymous */
  static const void *G008496[] = {I(aa,47,00,03),I(1c,02,1b,7a),I(1b,44,3f,1c),I(10,1b,47,00),I(01,47,00,04),I(3c,02,1b,44),I(10,1f,03,11),I(1f,04,47,00),I(02,90,2a,1b),I(22,01,36,1e),I(1f,04,2b,1b),I(47,00,06,1a),I(1b,44,0a,1c),I(47,00,05,3d),I(01,07,36,08),I(82,47,00,05),I(3d,01,07,22),I(02,22,02,36),I(5f,47,00,00),I(83,24,00,00),B(table1 ,3),I(08,2b,47,00),I(01,47,00,02),I(0f,47,00,03),I(1f,05,1d,03),I(2a,47,00,00),I(1d,1c,83,1d),I(24,00,00,00),B(table1 ,3),I(09,22,02,2a),I(47,00,00,82),I(24,00,00,00),B(table1 ,3),I(08,1d,1c,1a),I(1b,44,04,86),I(36,0d,47,00),I(00,24,00,00),B(table ,3),I(3c,01,2a,47),I(00,00,84,24),B(table1 ,3),I(08,22,04,45),I(03,00,00,00)};

  /* Byte-vector with size: 43 is_init: 0 index: 29 binding: (method-(setter table-ref)) */
  static const void *G008498[] = {I(43,03,46,0a),I(1d,48,00,00),I(1c,48,00,01),I(1b,48,00,02),I(47,00,00,8a),I(03,24,00,00),B(table1 ,3),I(08,47,00,00),I(83,24,00,00),B(table1 ,13),I(08,47,00,00),I(82,24,00,00),B(table1 ,13),I(08,1d,48,00),I(03,1c,48,00),I(04,47,00,03),I(24,00,00,00),B(vector ,6),I(3c,01,1b,44),I(04,86,36,24),I(8a,10,24,00),B(boot1 ,40),I(3c,01,1b,48),I(00,03,47,00),I(03,47,00,00),I(1c,1c,8a,03),I(1d,24,00,00),B(table1 ,3),I(09,22,04,2a),I(47,00,03,06),I(1b,48,00,06),I(47,00,01,1f),I(03,3c,01,1b),I(47,00,06,18),I(86,1b,48,00),I(05,23,00,00),B(table ,17),I(23,00,00,00),B(table ,28),I(3b,01,48,00),I(05,1c,47,00),I(05,3d,01,0b),I(45,0b,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 30 binding: (method-(setter table-ref)) */
  static const void *G008500[] = {I(43,03,1c,24),B(collect ,8),I(3c,01,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  eul_allocate_static_string(str_8504, "", 1);
  /* Byte-vector with size: 8 is_init: 0 index: 32 binding: (method-(setter table-ref)) */
  static const void *G008502[] = {I(43,03,1c,23),B(table ,31),I(24,00,00,00),B(string ,11),I(3c,02,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 33 binding: (method-(setter table-ref)) */
  static const void *G008505[] = {I(43,03,1c,41),B(table ,12),I(22,01,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 34 binding: (method-(setter element)) */
  static const void *G008507[] = {I(43,03,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1f,03),I(1f,03,1f,03),I(1f,03,3d,03),I(04,45,04,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 35 binding: anonymous */
  static const void *G008509[] = {I(aa,47,00,02),I(1c,02,1b,7a),I(1b,44,35,1c),I(10,1b,47,00),I(01,47,00,03),I(3c,02,1b,44),I(06,1f,03,11),I(36,1e,1f,04),I(2b,1b,47,00),I(05,1a,1b,44),I(0a,1c,47,00),I(04,3d,01,07),I(36,08,82,47),I(00,04,3d,01),I(07,22,02,22),I(02,36,0b,47),I(00,00,84,24),B(table1 ,3),I(08,45,03,00)};

  /* Byte-vector with size: 36 is_init: 0 index: 36 binding: (method-table-ref) */
  static const void *G008511[] = {I(ab,46,07,1c),I(48,00,00,1b),I(48,00,01,47),I(00,00,8a,03),I(24,00,00,00),B(table1 ,3),I(08,47,00,00),I(83,24,00,00),B(table1 ,13),I(08,47,00,00),I(82,24,00,00),B(table1 ,13),I(08,1d,48,00),I(02,1c,48,00),I(03,47,00,02),I(24,00,00,00),B(vector ,6),I(3c,01,1b,44),I(38,47,00,02),I(06,1b,48,00),I(05,47,00,01),I(1f,03,3c,01),I(1b,47,00,05),I(18,86,1b,48),I(00,04,23,00),B(table ,17),I(23,00,00,00),B(table ,35),I(3b,01,48,00),I(04,1c,47,00),I(04,3d,01,0a),I(22,04,36,0e),I(47,00,00,84),I(24,00,00,00),B(table1 ,3),I(08,45,06,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 37 binding: (method-table-ref) */
  static const void *G008513[] = {I(ab,24,00,00),B(collect ,8),I(3c,01,41,00),B(table ,11),I(45,02,00,00)};

  eul_allocate_static_string(str_8517, "", 1);
  /* Byte-vector with size: 7 is_init: 0 index: 39 binding: (method-table-ref) */
  static const void *G008515[] = {I(ab,23,00,00),B(table ,38),I(24,00,00,00),B(string ,11),I(3c,02,41,00),B(table ,11),I(45,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 40 binding: (method-table-ref) */
  static const void *G008518[] = {I(ab,41,00,00),B(table ,12),I(22,01,41,00),B(table ,11),I(45,02,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 41 binding: (method-element) */
  static const void *G008520[] = {I(ab,24,00,00),B(table ,7),I(3d,02,00,00)};

  eul_allocate_static_cons(cons_8530, NULL, NULL);
  eul_allocate_static_cons(cons_8528, NULL, eul_as_static(cons_8530));
  eul_allocate_static_cons(cons_8542, NULL, NULL);
  eul_allocate_static_cons(cons_8540, NULL, eul_as_static(cons_8542));
  /* Byte-vector with size: 619 is_init: 0 index: 60 binding: top-level */
  static const void *G008522[] = {I(a9,84,24,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table ,42),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table ,7),I(2a,24,00,00),B(collect ,20),I(8a,03,02,84),I(24,00,00,00),B(table1 ,3),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,20),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,43),I(23,00,00,00),B(table ,41),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,20),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(2a,24,00,00),B(table ,7),I(8a,03,02,84),I(24,00,00,00),B(table1 ,18),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,40),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(8a,03,02,84),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(string ,13),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,39),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(8a,03,02,84),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(mop_class ,39),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,37),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(8a,03,02,84),I(24,00,00,00),B(table1 ,13),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,36),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,20),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(collect ,20),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(03,02,8a,03),I(24,00,00,00),B(table1 ,3),I(86,86,24,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,20),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(04,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,05),I(1f,05,23,00),B(table ,45),I(23,00,00,00),B(table ,34),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,8a,03),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table ,47),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(table ,7),I(1c,1f,06,3c),I(02,2a,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,2a,24),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(03,02,8a,03),I(24,00,00,00),B(table1 ,18),I(86,86,24,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(04,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,33),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(03,02,8a,03),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(string ,13),I(86,24,00,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(04,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,32),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(03,02,8a,03),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(mop_class ,39),I(86,24,00,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(04,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,30),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(03,02,8a,03),I(24,00,00,00),B(table1 ,13),I(86,86,24,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1b,8a),I(04,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,29),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,3),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,49),I(23,00,00,00),B(table ,27),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,6),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(24,00,00,00),B(table1 ,13),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,6),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,50),I(23,00,00,00),B(table ,26),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,2),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(24,00,00,00),B(table1 ,13),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,2),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,51),I(23,00,00,00),B(table ,24),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,24),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(24,00,00,00),B(table1 ,13),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,24),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,52),I(23,00,00,00),B(table ,22),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,24),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,16),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(24,00,00,00),B(table1 ,13),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,16),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,53),I(23,00,00,00),B(table ,20),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,16),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,11),I(8a,03,02,84),I(86,24,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,11),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,54),I(23,00,00,00),B(table ,18),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,11),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,23),I(8a,03,02,8a),I(03,24,00,00),B(mop_class ,32),I(86,24,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,23),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,55),I(23,00,00,00),B(table ,15),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,23),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,12),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,12),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table ,56),I(23,00,00,00),B(table ,14),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,13),I(24,00,00,00),B(boot1 ,42),I(3c,01,83,24),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table ,59),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(table1 ,3),I(1c,1f,06,3c),I(02,2a,24,00),B(table1 ,3),I(24,00,00,00),B(mop_class ,13),I(3d,01,89,45),I(89,00,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 61 binding: anonymous */
  static const void *G008544[] = {I(ab,1c,47,00),I(03,1a,1b,44),I(39,47,00,02),I(1f,03,02,1f),I(03,2b,1c,7a),I(1b,44,1e,1d),I(10,1f,03,11),I(1c,1c,47,00),I(00,3c,02,1b),I(1f,08,0f,1f),I(05,1c,47,00),I(01,3d,02,0a),I(22,04,36,0a),I(1c,1f,05,47),I(00,01,3d,02),I(06,22,03,36),I(0b,1c,24,00),B(boot ,28),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 21 is_init: 0 index: 62 binding: map1-table */
  static const void *G008546[] = {I(ab,46,04,1c),I(48,00,00,8a),I(03,24,00,00),B(table1 ,3),I(08,1b,48,00),I(02,47,00,02),I(06,1b,48,00),I(03,47,00,02),I(24,00,00,00),B(vector ,6),I(3c,01,1b,44),I(25,86,1b,48),I(00,01,23,00),B(table ,17),I(23,00,00,00),B(table ,61),I(3b,02,48,00),I(01,82,86,47),I(00,01,3d,02),I(05,22,01,36),I(02,86,45,04)};

  /* Byte-vector with size: 16 is_init: 0 index: 63 binding: anonymous */
  static const void *G008548[] = {I(aa,1b,47,00),I(05,1a,1b,44),I(32,47,00,01),I(1d,02,1b,7a),I(1b,44,19,1c),I(10,47,00,02),I(3c,01,1b,47),I(00,06,18,1b),I(1f,04,47,00),I(03,3c,02,22),I(02,36,02,86),I(2a,1f,03,2b),I(47,00,04,3d),I(01,04,22,02),I(36,04,47,00),I(00,45,02,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 64 binding: anonymous */
  static const void *G008550[] = {I(ab,1c,47,00),I(06,1a,1b,44),I(26,47,00,07),I(1f,03,02,1b),I(44,11,1f,03),I(2b,1b,1f,04),I(47,00,03,3d),I(02,05,22,01),I(36,09,47,00),I(07,1f,04,1f),I(04,03,22,01),I(36,09,82,1d),I(47,00,03,3d),I(02,03,45,03)};

  /* Byte-vector with size: 52 is_init: 0 index: 65 binding: table-rehash */
  static const void *G008552[] = {I(aa,46,08,1b),I(48,00,00,47),I(00,00,8a,03),I(24,00,00,00),B(table1 ,3),I(08,47,00,00),I(82,24,00,00),B(table1 ,13),I(08,1c,48,00),I(01,1b,48,00),I(02,47,00,01),I(24,00,00,00),B(vector ,6),I(3c,01,1b,44),I(90,47,00,01),I(06,1b,48,00),I(05,47,00,05),I(84,16,1b,48),I(00,06,47,00),I(06,24,00,00),B(boot1 ,40),I(3c,01,1b,48),I(00,07,47,00),I(00,82,24,00),B(table1 ,3),I(08,84,16,47),I(00,00,47,00),I(07,1c,8a,03),I(1d,24,00,00),B(table1 ,3),I(09,22,02,2a),I(47,00,00,1c),I(1c,82,1d,24),B(table1 ,3),I(09,22,02,2a),I(86,86,1c,48),I(00,03,1b,48),I(00,04,23,00),B(table ,17),I(23,00,00,00),B(table ,64),I(3b,02,48,00),I(03,23,00,00),B(table ,17),I(23,00,00,00),B(table ,63),I(3b,01,48,00),I(04,82,47,00),I(04,3d,01,0a),I(22,06,36,04),I(47,00,00,45),I(04,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 66 binding: accumulate-table */
  static const void *G008554[] = {I(43,03,24,00),B(table1 ,9),I(3c,01,24,00),B(list ,26),I(3d,03,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 67 binding: anonymous */
  static const void *G008556[] = {I(aa,1b,7a,1b),I(44,04,86,36),I(0b,1c,10,1d),I(11,47,00,00),I(3d,02,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 68 binding: all1-table? */
  static const void *G008558[] = {I(ab,46,01,1c),I(48,00,00,8a),I(03,24,00,00),B(table1 ,3),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,67),I(3b,01,1c,24),B(vector ,10),I(3d,02,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 69 binding: table-empty? */
  static const void *G008560[] = {I(aa,83,24,00),B(table1 ,3),I(08,2d,45,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 70 binding: anonymous */
  static const void *G008562[] = {I(aa,1b,7a,1b),I(44,04,86,36),I(0b,1c,10,1d),I(11,47,00,00),I(3d,02,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 71 binding: anyp1-table */
  static const void *G008564[] = {I(ab,46,01,1c),I(48,00,00,8a),I(03,24,00,00),B(table1 ,3),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,70),I(3b,01,1c,24),B(vector ,4),I(3d,02,02,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 72 binding: anonymous */
  static const void *G008566[] = {I(aa,1b,7a,1b),I(44,0d,1c,10),I(1d,11,47,00),I(00,3d,02,02),I(36,02,86,45),I(02,00,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 73 binding: do1-table */
  static const void *G008568[] = {I(ab,46,01,1c),I(48,00,00,8a),I(03,24,00,00),B(table1 ,3),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,72),I(3b,01,1c,24),B(vector ,16),I(3d,02,02,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 74 binding: accumulate1-table */
  static const void *G008570[] = {I(ab,24,00,00),B(table1 ,9),I(3c,01,24,00),B(list ,24),I(3d,02,00,00)};

  /* Byte-vector with size: 113 is_init: 1 index: 0 binding: initialize-table */
  static const void *G008572[] = {I(87,25,00,00),B(table ,1),I(24,00,00,00),B(table1 ,1),I(3e,0b,24,00),B(table1 ,0),I(3c,00,21,01),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(copy ,1),I(3e,0b,24,00),B(copy ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(table ,75),I(23,00,00,00),B(table ,74),I(3b,02,25,00),B(table ,10),I(23,00,00,00),B(table ,76),I(23,00,00,00),B(table ,73),I(3b,02,25,00),B(table ,9),I(23,00,00,00),B(table ,77),I(23,00,00,00),B(table ,71),I(3b,02,25,00),B(table ,8),I(86,25,00,00),B(table ,7),I(23,00,00,00),B(table ,78),I(23,00,00,00),B(table ,69),I(3b,01,25,00),B(table ,6),I(23,00,00,00),B(table ,79),I(23,00,00,00),B(table ,68),I(3b,02,25,00),B(table ,5),I(23,00,00,00),B(table ,80),I(23,00,00,00),B(table ,66),I(3b,03,25,00),B(table ,4),I(23,00,00,00),B(table ,81),I(23,00,00,00),B(table ,65),I(3b,01,25,00),B(table ,3),I(23,00,00,00),B(table ,82),I(23,00,00,00),B(table ,62),I(3b,02,25,00),B(table ,2),I(23,00,00,00),B(table ,83),I(23,00,00,00),B(table ,60),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G008474,G008473);
  eul_allocate_bytevector( G008476,G008475);
  eul_allocate_bytevector( G008478,G008477);
  eul_intern_symbol(sym_8481,"anonymous");
  eul_allocate_bytevector( G008480,G008479);
  object_class(str_8484) = eul_static_string_class;
  eul_allocate_bytevector( G008483,G008482);
  object_class(str_8487) = eul_static_string_class;
  eul_allocate_bytevector( G008486,G008485);
  object_class(str_8490) = eul_static_string_class;
  eul_allocate_bytevector( G008489,G008488);
  object_class(str_8493) = eul_static_string_class;
  eul_allocate_bytevector( G008492,G008491);
  eul_allocate_bytevector( G008495,G008494);
  eul_allocate_bytevector( G008497,G008496);
  eul_allocate_bytevector( G008499,G008498);
  eul_allocate_bytevector( G008501,G008500);
  object_class(str_8504) = eul_static_string_class;
  eul_allocate_bytevector( G008503,G008502);
  eul_allocate_bytevector( G008506,G008505);
  eul_allocate_bytevector( G008508,G008507);
  eul_allocate_bytevector( G008510,G008509);
  eul_allocate_bytevector( G008512,G008511);
  eul_allocate_bytevector( G008514,G008513);
  object_class(str_8517) = eul_static_string_class;
  eul_allocate_bytevector( G008516,G008515);
  eul_allocate_bytevector( G008519,G008518);
  eul_allocate_bytevector( G008521,G008520);
  eul_intern_symbol(sym_8524,"table-ref");
  eul_intern_symbol(sym_8525,"(method element)");
  eul_intern_symbol(sym_8526,"(method table-ref)");
  eul_intern_symbol(sym_8527,"(method (setter element))");
  eul_intern_symbol(sym_8529,"setter");
  object_class(cons_8530) = eul_static_cons_class;
  eul_car(cons_8530) = sym_8524;
  eul_cdr(cons_8530) = eul_nil;
  object_class(cons_8528) = eul_static_cons_class;
  eul_car(cons_8528) = sym_8529;
  eul_intern_symbol(sym_8531,"(method (setter table-ref))");
  eul_intern_symbol(sym_8532,"(method emptyp)");
  eul_intern_symbol(sym_8533,"(method do)");
  eul_intern_symbol(sym_8534,"(method map)");
  eul_intern_symbol(sym_8535,"(method any?)");
  eul_intern_symbol(sym_8536,"(method all?)");
  eul_intern_symbol(sym_8537,"(method member)");
  eul_intern_symbol(sym_8538,"(method accumulate)");
  eul_intern_symbol(sym_8539,"(method accumulate1)");
  eul_intern_symbol(sym_8541,"converter");
  eul_intern_symbol(sym_8543,"<table>");
  object_class(cons_8542) = eul_static_cons_class;
  eul_car(cons_8542) = sym_8543;
  eul_cdr(cons_8542) = eul_nil;
  object_class(cons_8540) = eul_static_cons_class;
  eul_car(cons_8540) = sym_8541;
  eul_allocate_bytevector( G008523,G008522);
  eul_allocate_bytevector( G008545,G008544);
  eul_allocate_bytevector( G008547,G008546);
  eul_allocate_bytevector( G008549,G008548);
  eul_allocate_bytevector( G008551,G008550);
  eul_allocate_bytevector( G008553,G008552);
  eul_allocate_bytevector( G008555,G008554);
  eul_allocate_bytevector( G008557,G008556);
  eul_allocate_bytevector( G008559,G008558);
  eul_allocate_bytevector( G008561,G008560);
  eul_allocate_bytevector( G008563,G008562);
  eul_allocate_bytevector( G008565,G008564);
  eul_allocate_bytevector( G008567,G008566);
  eul_allocate_bytevector( G008569,G008568);
  eul_allocate_bytevector( G008571,G008570);
  eul_intern_symbol(sym_8574,"accumulate1-table");
  eul_intern_symbol(sym_8575,"do1-table");
  eul_intern_symbol(sym_8576,"anyp1-table");
  eul_intern_symbol(sym_8577,"table-empty?");
  eul_intern_symbol(sym_8578,"all1-table?");
  eul_intern_symbol(sym_8579,"accumulate-table");
  eul_intern_symbol(sym_8580,"table-rehash");
  eul_intern_symbol(sym_8581,"map1-table");
  eul_intern_symbol(sym_8582,"top-level");
  eul_allocate_bytevector( G008573,G008572);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      table_bindings[i] = eul_nil;
  }

  table_bindings[ 11] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_table_ref8470;
  table_bindings[ 12] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_addr_str8471;
  table_bindings[ 13] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_table_set8472;
  table_bindings[ 14] = G008474;
  table_bindings[ 15] = G008476;
  table_bindings[ 16] = G008478;
  table_bindings[ 17] = sym_8481;
  table_bindings[ 18] = G008480;
  table_bindings[ 19] = str_8484;
  table_bindings[ 20] = G008483;
  table_bindings[ 21] = str_8487;
  table_bindings[ 22] = G008486;
  table_bindings[ 23] = str_8490;
  table_bindings[ 24] = G008489;
  table_bindings[ 25] = str_8493;
  table_bindings[ 26] = G008492;
  table_bindings[ 27] = G008495;
  table_bindings[ 28] = G008497;
  table_bindings[ 29] = G008499;
  table_bindings[ 30] = G008501;
  table_bindings[ 31] = str_8504;
  table_bindings[ 32] = G008503;
  table_bindings[ 33] = G008506;
  table_bindings[ 34] = G008508;
  table_bindings[ 35] = G008510;
  table_bindings[ 36] = G008512;
  table_bindings[ 37] = G008514;
  table_bindings[ 38] = str_8517;
  table_bindings[ 39] = G008516;
  table_bindings[ 40] = G008519;
  table_bindings[ 41] = G008521;
  table_bindings[ 42] = sym_8524;
  table_bindings[ 43] = sym_8525;
  table_bindings[ 44] = sym_8526;
  table_bindings[ 45] = sym_8527;
  table_bindings[ 46] = sym_8529;
  table_bindings[ 47] = cons_8528;
  table_bindings[ 48] = sym_8531;
  table_bindings[ 49] = sym_8532;
  table_bindings[ 50] = sym_8533;
  table_bindings[ 51] = sym_8534;
  table_bindings[ 52] = sym_8535;
  table_bindings[ 53] = sym_8536;
  table_bindings[ 54] = sym_8537;
  table_bindings[ 55] = sym_8538;
  table_bindings[ 56] = sym_8539;
  table_bindings[ 57] = sym_8541;
  table_bindings[ 58] = sym_8543;
  table_bindings[ 59] = cons_8540;
  table_bindings[ 60] = G008523;
  table_bindings[ 61] = G008545;
  table_bindings[ 62] = G008547;
  table_bindings[ 63] = G008549;
  table_bindings[ 64] = G008551;
  table_bindings[ 65] = G008553;
  table_bindings[ 66] = G008555;
  table_bindings[ 67] = G008557;
  table_bindings[ 68] = G008559;
  table_bindings[ 69] = G008561;
  table_bindings[ 70] = G008563;
  table_bindings[ 71] = G008565;
  table_bindings[ 72] = G008567;
  table_bindings[ 73] = G008569;
  table_bindings[ 74] = G008571;
  table_bindings[ 1] = eul_nil;
  table_bindings[ 75] = sym_8574;
  table_bindings[ 76] = sym_8575;
  table_bindings[ 77] = sym_8576;
  table_bindings[ 78] = sym_8577;
  table_bindings[ 79] = sym_8578;
  table_bindings[ 80] = sym_8579;
  table_bindings[ 81] = sym_8580;
  table_bindings[ 82] = sym_8581;
  table_bindings[ 83] = sym_8582;
  eul_allocate_lambda( table_bindings[0], "initialize-table", 0, G008573);

  }
}


/* eof */
