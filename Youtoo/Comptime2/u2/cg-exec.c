/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module cg-exec
 **  Copyright: See file cg-exec.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_cg_exec_word_length();
extern void initialize_module_i_all();
extern void initialize_module_i_modify();
extern void initialize_module_p_env();
extern void initialize_module_sx_obj();
extern void initialize_module_sx_node();
extern void initialize_module_cg_state();
extern void initialize_module_cg_asm();
extern void initialize_module_cg_interf();
extern void initialize_module_i_ffi();
extern void initialize_module_ex_expr();
extern void initialize_module_cg_dld();
extern LispRef ex_expr_bindings[];
extern LispRef i_ffi_bindings[];
extern LispRef cg_interf_bindings[];
extern LispRef cg_asm_bindings[];
extern LispRef i_modify_bindings[];
extern LispRef i_all_bindings[];
extern LispRef list_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef sx_node_bindings[];
extern LispRef sx_obj_bindings[];
extern LispRef p_env_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef i_error_bindings[];
extern LispRef mop_access_bindings[];
extern LispRef stream2_bindings[];
extern LispRef condition_bindings[];
extern LispRef format_bindings[];
extern LispRef integer_bindings[];
extern LispRef number_bindings[];
extern LispRef cg_exec_word_length_bindings[];
extern LispRef cg_dld_bindings[];
extern LispRef i_param_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef string_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef boot_bindings[];
extern LispRef cg_state_bindings[];
extern LispRef boot1_bindings[];
extern LispRef table_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef table1_bindings[];

/* Module bindings with size 83 */
LispRef cg_exec_bindings[83];

/* Foreign functions */
static LispRef ff_stub_eul_allocate_lambda28390 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008477, G008478, G008479, res;

  POPVAL1(G008479);
  POPVAL1(G008478);
  POPVAL1(G008477);
  FF_RES_CONVERT6(res,eul_allocate_lambda2(FF_ARG_CONVERT8(G008477), FF_ARG_CONVERT8(G008478), FF_ARG_CONVERT8(G008479)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module cg-exec */
void initialize_module_cg_exec()
{
  if (is_initialized) return;
  initialize_module_cg_exec_word_length();
  initialize_module_i_all();
  initialize_module_i_modify();
  initialize_module_p_env();
  initialize_module_sx_obj();
  initialize_module_sx_node();
  initialize_module_cg_state();
  initialize_module_cg_asm();
  initialize_module_cg_interf();
  initialize_module_i_ffi();
  initialize_module_ex_expr();
  initialize_module_cg_dld();
  eul_fast_table_set(eul_modules,"cg_exec",(LispRef) cg_exec_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_8476, sym_8475, sym_8474, sym_8473, sym_8472, sym_8471, sym_8470, sym_8469, sym_8468, sym_8467, sym_8466, sym_8465, sym_8464, sym_8463, sym_8462, sym_8461, G008460, G008458, G008456, sym_8454, G008453, G008451, G008449, G008447, G008445, G008443, G008440, G008438, G008435, G008433, G008429, sym_8427, sym_8425, G008424, key_8422, G008419, G008417, G008413, sym_8411, sym_8410, sym_8409, sym_8408, G008407, sym_8405, sym_8404, sym_8403, G008402, G008400, G008396, G008394, G008392;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 11 is_init: 0 index: 22 binding: top-level */
  static const void *G008391[] = {I(a9,86,89,00),B(cg_exec ,16),I(2a,86,89,00),B(cg_exec ,5),I(2a,24,00,00),B(table1 ,3),I(24,00,00,00),B(mop_gf ,2),I(3c,01,1b,89),B(cg_exec ,15),I(45,01,00,00)};

  /* Byte-vector with size: 9 is_init: 0 index: 23 binding: set-bytevector-pos */
  static const void *G008393[] = {I(ab,24,00,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(cg_exec ,15),I(1f,03,1f,03),I(1f,03,3d,03),I(03,45,03,00)};

  eul_allocate_static_cons(cons_8397, c_int_as_eul_fpi(172), NULL);
  eul_allocate_static_string(str_8398, "initialize-", 11);
  /* Byte-vector with size: 27 is_init: 0 index: 26 binding: run-init-bytevector */
  static const void *G008395[] = {I(aa,1b,82,24),B(cg_state ,39),I(08,1c,83,24),B(cg_state ,39),I(08,23,00,00),B(cg_exec ,24),I(24,00,00,00),B(boot ,8),I(3c,02,1c,83),I(14,86,24,00),B(cg_exec ,12),I(3c,03,24,00),B(cg_exec ,16),I(8a,15,24,00),B(sx_obj1 ,59),I(08,1b,82,02),I(23,00,00,00),B(cg_exec ,25),I(1c,24,00,00),B(string ,11),I(3c,02,41,00),B(boot1 ,56),I(22,01,1b,82),I(1f,05,41,00),B(cg_exec ,21),I(22,03,1b,3d),I(00,07,45,07)};

  /* Byte-vector with size: 7 is_init: 0 index: 27 binding: anonymous */
  static const void *G008399[] = {I(a9,47,00,00),I(24,00,00,00),B(cg_exec ,18),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 48 is_init: 0 index: 31 binding: execute */
  static const void *G008401[] = {I(ab,46,01,1c),I(48,00,00,23),B(cg_exec ,28),I(89,00,00,00),B(i_param ,60),I(2a,47,00,00),I(89,00,00,00),B(cg_exec ,16),I(2a,47,00,00),I(8a,15,24,00),B(sx_obj1 ,59),I(08,41,00,00),B(cg_dld ,14),I(22,01,1b,89),B(cg_exec ,5),I(2a,1c,82,24),B(cg_state ,2),I(08,1d,83,24),B(cg_state ,2),I(08,24,00,00),B(cg_exec ,11),I(3c,00,2a,1c),I(24,00,00,00),B(cg_exec ,17),I(3c,01,2a,23),B(cg_exec ,29),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(cg_exec ,30),I(23,00,00,00),B(cg_exec ,27),I(3b,00,1c,0f),I(23,00,00,00),B(cg_exec ,29),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,1d),I(24,00,00,00),B(cg_exec ,3),I(3c,01,47,00),I(00,24,00,00),B(cg_exec ,18),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1b),I(45,08,00,00)};

  /* Byte-vector with size: 63 is_init: 0 index: 36 binding: compute-bytevector-aux */
  static const void *G008406[] = {I(aa,1b,12,1b),I(44,0c,24,00),B(cg_exec_word_length ,3),I(3d,00,02,36),I(e9,1c,10,1b),I(24,00,00,00),B(number ,24),I(3c,01,1b,44),I(0c,1c,24,00),B(cg_exec_word_length ,6),I(3c,01,36,be),I(1c,7a,1b,44),I(b6,1d,10,1f),I(03,11,1f,04),I(11,1b,10,1f),I(03,1f,03,1d),I(1d,23,00,00),B(cg_exec ,32),I(50,1b,44,0d),I(1c,24,00,00),B(cg_exec ,10),I(3c,01,36,8b),I(1f,03,23,00),B(cg_exec ,33),I(50,1b,44,0d),I(1d,24,00,00),B(cg_exec ,20),I(3c,01,36,71),I(1f,04,23,00),B(cg_exec ,34),I(50,1b,44,17),I(1f,04,11,1b),I(10,1b,1f,06),I(24,00,00,00),B(cg_exec ,8),I(3c,02,22,02),I(36,4d,1f,05),I(24,00,00,00),B(integer ,4),I(3c,01,1b,44),I(0c,1f,0d,24),B(cg_exec ,6),I(3c,01,36,31),I(1f,06,23,00),B(cg_exec ,35),I(50,1b,44,0d),I(1f,05,24,00),B(cg_exec ,9),I(3c,01,36,17),I(1f,07,24,00),B(cg_exec_word_length ,6),I(3c,01,2a,1f),I(05,24,00,00),B(cg_exec_word_length ,6),I(3c,01,22,01),I(22,01,22,01),I(22,01,22,08),I(36,02,86,22),I(01,2a,1f,03),I(11,24,00,00),B(cg_exec ,6),I(3d,01,04,22),I(02,45,02,00)};

  eul_allocate_static_string(str_8414, "statically linked module ~a cannot get new bindings", 51);
  eul_allocate_static_string(str_8415, "module ~a cannot have more than ~a bindings", 43);
  /* Byte-vector with size: 45 is_init: 0 index: 39 binding: next-local-index */
  static const void *G008412[] = {I(a9,24,00,00),B(cg_exec ,16),I(8a,12,24,00),B(sx_obj1 ,59),I(08,1b,44,04),I(86,36,32,24),B(cg_exec ,16),I(8a,15,24,00),B(sx_obj1 ,59),I(08,23,00,00),B(cg_exec ,37),I(1c,24,00,00),B(format ,2),I(3c,02,24,00),B(condition ,8),I(1c,24,00,00),B(boot ,13),I(3c,02,22,02),I(2a,24,00,00),B(cg_exec ,16),I(8a,11,24,00),B(sx_obj1 ,59),I(08,1c,1c,1a),I(1b,44,04,86),I(36,33,24,00),B(cg_exec ,16),I(8a,15,24,00),B(sx_obj1 ,59),I(08,23,00,00),B(cg_exec ,38),I(1c,24,00,00),B(format ,2),I(3c,02,24,00),B(condition ,8),I(1c,1f,06,24),B(boot ,13),I(3c,03,22,02),I(2a,1d,83,14),I(24,00,00,00),B(cg_exec ,16),I(1c,1c,8a,12),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1f,03,45,04)};

  /* Byte-vector with size: 1 is_init: 0 index: 40 binding: (method-G008231) */
  static const void *G008416[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_8420, "compile time error condition: ", 30);
  eul_allocate_static_string(str_8421, "can't compute binding ~a of module ~a", 37);
  /* Byte-vector with size: 30 is_init: 0 index: 44 binding: (method-G008231) */
  static const void *G008418[] = {I(ab,24,00,00),B(stream2 ,9),I(23,00,00,00),B(cg_exec ,41),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,9),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,56),I(44,04,86,36),I(41,24,00,00),B(cg_exec ,16),I(8a,15,24,00),B(sx_obj1 ,59),I(08,23,00,00),B(cg_exec ,42),I(47,00,00,1d),I(24,00,00,00),B(format ,2),I(3c,03,24,00),B(i_error ,5),I(1c,23,00,00),B(cg_exec ,43),I(47,00,00,24),B(boot ,13),I(3d,04,04,22),I(02,45,02,00)};

  eul_allocate_static_string(str_8426, "  compute-binding ~a ~a", 23);
  /* Byte-vector with size: 94 is_init: 0 index: 48 binding: compute-binding */
  static const void *G008423[] = {I(ab,46,01,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(cg_exec ,30),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,8a),I(03,02,84,86),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(cg_exec ,45),I(23,00,00,00),B(cg_exec ,44),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(8a,03,02,84),I(24,00,00,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(cg_exec ,45),I(23,00,00,00),B(cg_exec ,40),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,23),B(cg_exec ,46),I(1f,13,47,00),I(00,24,00,00),B(i_notify ,4),I(3c,03,2a,47),I(00,00,24,00),B(p_env ,7),I(3c,01,1f,13),I(23,00,00,00),B(cg_exec ,47),I(50,1b,44,0d),I(1c,24,00,00),B(sx_obj ,16),I(3c,01,36,03),I(1f,14,41,00),B(cg_dld ,14),I(22,01,1d,83),I(24,00,00,00),B(sx_obj1 ,42),I(08,24,00,00),B(cg_exec_word_length ,2),I(3c,02,83,24),B(dynamic ,6),I(3c,01,2a,1b),I(45,17,00,00)};

  eul_allocate_static_string(str_8430, "body of inlined function contains non exported binding ~a", 57);
  eul_allocate_static_string(str_8431, "bad index ~a of foreign function ~a", 35);
  /* Byte-vector with size: 27 is_init: 0 index: 51 binding: compute-foreign-function-binding */
  static const void *G008428[] = {I(aa,1b,24,00),B(p_env ,7),I(3c,01,1b,44),I(04,1b,36,13),I(85,23,00,00),B(cg_exec ,49),I(1f,03,24,00),B(i_error ,4),I(3c,03,1b,24),B(sx_node ,28),I(3c,01,1c,83),I(24,00,00,00),B(sx_obj1 ,42),I(08,1c,41,00),B(cg_dld ,14),I(22,01,1c,7e),I(1b,44,0f,1c),I(1f,03,24,00),B(cg_exec_word_length ,2),I(3d,02,07,36),I(17,85,23,00),B(cg_exec ,50),I(1f,04,1f,09),I(24,00,00,00),B(i_error ,4),I(3d,04,07,45),I(07,00,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 52 binding: compute-code-vector */
  static const void *G008432[] = {I(aa,24,00,00),B(cg_exec ,19),I(3c,01,24,00),B(cg_exec ,5),I(1c,24,00,00),B(cg_exec_word_length ,2),I(3d,02,01,00)};

  eul_allocate_static_string(str_8436, "  set-up-binding ~a ~a", 22);
  /* Byte-vector with size: 19 is_init: 0 index: 54 binding: anonymous */
  static const void *G008434[] = {I(aa,1b,83,24),B(sx_obj1 ,42),I(08,1b,44,04),I(86,36,15,24),B(cg_exec ,7),I(3c,00,1d,1c),I(1c,83,1d,24),B(sx_obj1 ,42),I(09,22,03,2a),I(1c,8a,06,24),B(sx_obj1 ,42),I(08,1d,83,24),B(sx_obj1 ,42),I(08,23,00,00),B(cg_exec ,53),I(1d,1d,24,00),B(i_notify ,4),I(3d,03,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 55 binding: set-up-bindings */
  static const void *G008437[] = {I(a9,24,00,00),B(cg_exec ,16),I(82,24,00,00),B(sx_obj1 ,59),I(08,24,00,00),B(aux_table ,8),I(3c,01,24,00),B(sx_node ,9),I(1c,24,00,00),B(list ,37),I(3c,02,23,00),B(cg_exec ,30),I(23,00,00,00),B(cg_exec ,54),I(3b,01,1c,24),B(boot ,17),I(3d,02,02,45),I(02,00,00,00)};

  eul_allocate_static_string(str_8441, "  compute-bytevector ~a ~a", 26);
  /* Byte-vector with size: 11 is_init: 0 index: 57 binding: compute-bytevector */
  static const void *G008439[] = {I(43,03,23,00),B(cg_exec ,56),I(1c,1f,04,24),B(i_notify ,4),I(3c,03,2a,1c),I(24,00,00,00),B(cg_exec_word_length ,5),I(3c,01,2a,1d),I(24,00,00,00),B(cg_exec ,6),I(3d,01,03,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 58 binding: register-new-local */
  static const void *G008442[] = {I(a7,1b,44,05),I(1b,10,36,02),I(86,1b,44,04),I(1b,36,08,24),B(cg_exec ,7),I(3c,00,24,00),B(cg_exec ,5),I(1c,1f,05,41),B(cg_dld ,12),I(22,03,2a,1b),I(45,04,00,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 59 binding: anonymous */
  static const void *G008444[] = {I(aa,1b,12,1b),I(44,04,86,36),I(2c,1c,10,24),B(p_env ,14),I(3c,01,1b,83),I(24,00,00,00),B(sx_obj1 ,59),I(08,47,00,01),I(1c,86,6c,1b),I(44,04,1d,36),I(0a,1f,04,11),I(47,00,00,3d),I(01,05,22,03),I(45,02,00,00)};

  /* Byte-vector with size: 21 is_init: 0 index: 60 binding: get-imported-module-or-library */
  static const void *G008446[] = {I(a9,46,02,24),B(cg_exec ,16),I(8a,15,24,00),B(sx_obj1 ,59),I(08,1b,48,00),I(01,47,00,01),I(24,00,00,00),B(p_env ,14),I(3c,01,1b,44),I(04,1b,36,29),I(86,1b,48,00),I(00,23,00,00),B(cg_exec ,30),I(23,00,00,00),B(cg_exec ,59),I(3b,01,48,00),I(00,24,00,00),B(i_param ,14),I(47,00,00,3d),I(01,03,22,01),I(45,02,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 61 binding: anonymous */
  static const void *G008448[] = {I(aa,1b,83,24),B(cg_state ,39),I(08,1c,8a,03),I(24,00,00,00),B(cg_state ,39),I(08,1d,84,24),B(cg_state ,39),I(08,1f,03,82),I(24,00,00,00),B(cg_state ,39),I(08,1f,03,1c),I(1f,03,24,00),B(cg_exec ,12),I(3c,03,1b,24),B(cg_exec ,13),I(3c,01,1f,04),I(1c,24,00,00),B(cg_exec ,2),I(3d,02,07,45),I(07,00,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 62 binding: set-up-bytevectors */
  static const void *G008450[] = {I(aa,23,00,00),B(cg_exec ,30),I(23,00,00,00),B(cg_exec ,61),I(3b,01,1c,24),B(boot ,17),I(3d,02,01,00)};

  /* Byte-vector with size: 36 is_init: 0 index: 64 binding: reset-interactive-module */
  static const void *G008452[] = {I(aa,1b,82,24),B(sx_obj1 ,59),I(08,24,00,00),B(aux_table ,9),I(3c,01,2a,24),B(i_param ,48),I(24,00,00,00),B(aux_table ,9),I(3c,01,2a,1b),I(86,1c,8a,06),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1b,86,1c,8a),I(09,1d,24,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1b,86,1c,8a),I(08,1d,24,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1b,86,1c,8a),I(0d,1d,24,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1b,86,1c,8a),I(0c,1d,24,00),B(sx_obj1 ,59),I(09,22,02,2a),I(86,89,00,00),B(i_param ,56),I(2a,23,00,00),B(cg_exec ,63),I(89,00,00,00),B(i_param ,60),I(45,01,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 65 binding: get-bytevector-pos */
  static const void *G008455[] = {I(aa,24,00,00),B(cg_exec ,15),I(1c,24,00,00),B(table ,7),I(3d,02,01,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 66 binding: compute-static */
  static const void *G008457[] = {I(aa,24,00,00),B(cg_exec ,13),I(3c,01,24,00),B(cg_exec ,5),I(1c,24,00,00),B(cg_exec_word_length ,2),I(3d,02,01,00)};

  /* Byte-vector with size: 170 is_init: 1 index: 0 binding: initialize-cg-exec */
  static const void *G008459[] = {I(87,25,00,00),B(cg_exec ,1),I(24,00,00,00),B(cg_dld ,1),I(3e,0b,24,00),B(cg_dld ,0),I(3c,00,21,01),I(24,00,00,00),B(ex_expr ,1),I(3e,0b,24,00),B(ex_expr ,0),I(3c,00,21,01),I(24,00,00,00),B(i_ffi ,1),I(3e,0b,24,00),B(i_ffi ,0),I(3c,00,21,01),I(24,00,00,00),B(cg_interf ,1),I(3e,0b,24,00),B(cg_interf ,0),I(3c,00,21,01),I(24,00,00,00),B(cg_asm ,1),I(3e,0b,24,00),B(cg_asm ,0),I(3c,00,21,01),I(24,00,00,00),B(cg_state ,1),I(3e,0b,24,00),B(cg_state ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_node ,1),I(3e,0b,24,00),B(sx_node ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(p_env ,1),I(3e,0b,24,00),B(p_env ,0),I(3c,00,21,01),I(24,00,00,00),B(i_modify ,1),I(3e,0b,24,00),B(i_modify ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(24,00,00,00),B(cg_exec_word_length ,1),I(3e,0b,24,00),B(cg_exec_word_length ,0),I(3c,00,21,01),I(23,00,00,00),B(cg_exec ,67),I(23,00,00,00),B(cg_exec ,66),I(3b,01,25,00),B(cg_exec ,20),I(23,00,00,00),B(cg_exec ,68),I(23,00,00,00),B(cg_exec ,65),I(3b,01,25,00),B(cg_exec ,19),I(23,00,00,00),B(cg_exec ,69),I(23,00,00,00),B(cg_exec ,64),I(3b,01,25,00),B(cg_exec ,18),I(23,00,00,00),B(cg_exec ,70),I(23,00,00,00),B(cg_exec ,62),I(3b,01,25,00),B(cg_exec ,17),I(86,25,00,00),B(cg_exec ,16),I(86,25,00,00),B(cg_exec ,15),I(23,00,00,00),B(cg_exec ,71),I(23,00,00,00),B(cg_exec ,60),I(3b,00,25,00),B(cg_exec ,14),I(23,00,00,00),B(cg_exec ,72),I(23,00,00,00),B(cg_exec ,58),I(3b,fe,25,00),B(cg_exec ,13),I(23,00,00,00),B(cg_exec ,73),I(23,00,00,00),B(cg_exec ,57),I(3b,03,25,00),B(cg_exec ,12),I(23,00,00,00),B(cg_exec ,74),I(23,00,00,00),B(cg_exec ,55),I(3b,00,25,00),B(cg_exec ,11),I(23,00,00,00),B(cg_exec ,75),I(23,00,00,00),B(cg_exec ,52),I(3b,01,25,00),B(cg_exec ,10),I(23,00,00,00),B(cg_exec ,76),I(23,00,00,00),B(cg_exec ,51),I(3b,01,25,00),B(cg_exec ,9),I(23,00,00,00),B(cg_exec ,77),I(23,00,00,00),B(cg_exec ,48),I(3b,02,25,00),B(cg_exec ,8),I(23,00,00,00),B(cg_exec ,78),I(23,00,00,00),B(cg_exec ,39),I(3b,00,25,00),B(cg_exec ,7),I(23,00,00,00),B(cg_exec ,79),I(23,00,00,00),B(cg_exec ,36),I(3b,01,25,00),B(cg_exec ,6),I(86,25,00,00),B(cg_exec ,5),I(23,00,00,00),B(cg_exec ,28),I(23,00,00,00),B(cg_exec ,31),I(3b,02,25,00),B(cg_exec ,4),I(23,00,00,00),B(cg_exec ,80),I(23,00,00,00),B(cg_exec ,26),I(3b,01,25,00),B(cg_exec ,3),I(23,00,00,00),B(cg_exec ,81),I(23,00,00,00),B(cg_exec ,23),I(3b,02,25,00),B(cg_exec ,2),I(23,00,00,00),B(cg_exec ,82),I(23,00,00,00),B(cg_exec ,22),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G008392,G008391);
  eul_allocate_bytevector( G008394,G008393);
  object_class(cons_8397) = eul_static_cons_class;
  eul_cdr(cons_8397) = eul_nil;
  object_class(str_8398) = eul_static_string_class;
  eul_allocate_bytevector( G008396,G008395);
  eul_allocate_bytevector( G008400,G008399);
  eul_intern_symbol(sym_8403,"execute");
  eul_intern_symbol(sym_8404,"*clean-ups*");
  eul_intern_symbol(sym_8405,"anonymous");
  eul_allocate_bytevector( G008402,G008401);
  eul_intern_symbol(sym_8408,"CODE-VECTOR");
  eul_intern_symbol(sym_8409,"STATIC");
  eul_intern_symbol(sym_8410,"BINDING");
  eul_intern_symbol(sym_8411,"FF");
  eul_allocate_bytevector( G008407,G008406);
  object_class(str_8414) = eul_static_string_class;
  object_class(str_8415) = eul_static_string_class;
  eul_allocate_bytevector( G008413,G008412);
  eul_allocate_bytevector( G008417,G008416);
  object_class(str_8420) = eul_static_string_class;
  object_class(str_8421) = eul_static_string_class;
  eul_intern_keyword(key_8422,"ct-error-value");
  eul_allocate_bytevector( G008419,G008418);
  eul_intern_symbol(sym_8425,"(method G008231)");
  object_class(str_8426) = eul_static_string_class;
  eul_intern_symbol(sym_8427,"?");
  eul_allocate_bytevector( G008424,G008423);
  object_class(str_8430) = eul_static_string_class;
  object_class(str_8431) = eul_static_string_class;
  eul_allocate_bytevector( G008429,G008428);
  eul_allocate_bytevector( G008433,G008432);
  object_class(str_8436) = eul_static_string_class;
  eul_allocate_bytevector( G008435,G008434);
  eul_allocate_bytevector( G008438,G008437);
  object_class(str_8441) = eul_static_string_class;
  eul_allocate_bytevector( G008440,G008439);
  eul_allocate_bytevector( G008443,G008442);
  eul_allocate_bytevector( G008445,G008444);
  eul_allocate_bytevector( G008447,G008446);
  eul_allocate_bytevector( G008449,G008448);
  eul_allocate_bytevector( G008451,G008450);
  eul_intern_symbol(sym_8454,"idle");
  eul_allocate_bytevector( G008453,G008452);
  eul_allocate_bytevector( G008456,G008455);
  eul_allocate_bytevector( G008458,G008457);
  eul_intern_symbol(sym_8461,"compute-static");
  eul_intern_symbol(sym_8462,"get-bytevector-pos");
  eul_intern_symbol(sym_8463,"reset-interactive-module");
  eul_intern_symbol(sym_8464,"set-up-bytevectors");
  eul_intern_symbol(sym_8465,"get-imported-module-or-library");
  eul_intern_symbol(sym_8466,"register-new-local");
  eul_intern_symbol(sym_8467,"compute-bytevector");
  eul_intern_symbol(sym_8468,"set-up-bindings");
  eul_intern_symbol(sym_8469,"compute-code-vector");
  eul_intern_symbol(sym_8470,"compute-foreign-function-binding");
  eul_intern_symbol(sym_8471,"compute-binding");
  eul_intern_symbol(sym_8472,"next-local-index");
  eul_intern_symbol(sym_8473,"compute-bytevector-aux");
  eul_intern_symbol(sym_8474,"run-init-bytevector");
  eul_intern_symbol(sym_8475,"set-bytevector-pos");
  eul_intern_symbol(sym_8476,"top-level");
  eul_allocate_bytevector( G008460,G008459);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 21; i++)
      cg_exec_bindings[i] = eul_nil;
  }

  cg_exec_bindings[ 21] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_allocate_lambda28390;
  cg_exec_bindings[ 22] = G008392;
  cg_exec_bindings[ 23] = G008394;
  cg_exec_bindings[ 24] = cons_8397;
  cg_exec_bindings[ 25] = str_8398;
  cg_exec_bindings[ 26] = G008396;
  cg_exec_bindings[ 27] = G008400;
  cg_exec_bindings[ 28] = sym_8403;
  cg_exec_bindings[ 29] = sym_8404;
  cg_exec_bindings[ 30] = sym_8405;
  cg_exec_bindings[ 31] = G008402;
  cg_exec_bindings[ 32] = sym_8408;
  cg_exec_bindings[ 33] = sym_8409;
  cg_exec_bindings[ 34] = sym_8410;
  cg_exec_bindings[ 35] = sym_8411;
  cg_exec_bindings[ 36] = G008407;
  cg_exec_bindings[ 37] = str_8414;
  cg_exec_bindings[ 38] = str_8415;
  cg_exec_bindings[ 39] = G008413;
  cg_exec_bindings[ 40] = G008417;
  cg_exec_bindings[ 41] = str_8420;
  cg_exec_bindings[ 42] = str_8421;
  cg_exec_bindings[ 43] = key_8422;
  cg_exec_bindings[ 44] = G008419;
  cg_exec_bindings[ 45] = sym_8425;
  cg_exec_bindings[ 46] = str_8426;
  cg_exec_bindings[ 47] = sym_8427;
  cg_exec_bindings[ 48] = G008424;
  cg_exec_bindings[ 49] = str_8430;
  cg_exec_bindings[ 50] = str_8431;
  cg_exec_bindings[ 51] = G008429;
  cg_exec_bindings[ 52] = G008433;
  cg_exec_bindings[ 53] = str_8436;
  cg_exec_bindings[ 54] = G008435;
  cg_exec_bindings[ 55] = G008438;
  cg_exec_bindings[ 56] = str_8441;
  cg_exec_bindings[ 57] = G008440;
  cg_exec_bindings[ 58] = G008443;
  cg_exec_bindings[ 59] = G008445;
  cg_exec_bindings[ 60] = G008447;
  cg_exec_bindings[ 61] = G008449;
  cg_exec_bindings[ 62] = G008451;
  cg_exec_bindings[ 63] = sym_8454;
  cg_exec_bindings[ 64] = G008453;
  cg_exec_bindings[ 65] = G008456;
  cg_exec_bindings[ 66] = G008458;
  cg_exec_bindings[ 1] = eul_nil;
  cg_exec_bindings[ 67] = sym_8461;
  cg_exec_bindings[ 68] = sym_8462;
  cg_exec_bindings[ 69] = sym_8463;
  cg_exec_bindings[ 70] = sym_8464;
  cg_exec_bindings[ 71] = sym_8465;
  cg_exec_bindings[ 72] = sym_8466;
  cg_exec_bindings[ 73] = sym_8467;
  cg_exec_bindings[ 74] = sym_8468;
  cg_exec_bindings[ 75] = sym_8469;
  cg_exec_bindings[ 76] = sym_8470;
  cg_exec_bindings[ 77] = sym_8471;
  cg_exec_bindings[ 78] = sym_8472;
  cg_exec_bindings[ 79] = sym_8473;
  cg_exec_bindings[ 80] = sym_8474;
  cg_exec_bindings[ 81] = sym_8475;
  cg_exec_bindings[ 82] = sym_8476;
  eul_allocate_lambda( cg_exec_bindings[0], "initialize-cg-exec", 0, G008460);

  }
}


/* eof */
