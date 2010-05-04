/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module ex-body
 **  Copyright: See file ex-body.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_p_env();
extern void initialize_module_ex_expr();
extern void initialize_module_sx_obj();
extern void initialize_module_sx_node();
extern LispRef p_env_bindings[];
extern LispRef i_all_bindings[];
extern LispRef boot_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef sx_node_bindings[];
extern LispRef sx_obj_bindings[];
extern LispRef ex_expr_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef sx_obj2_bindings[];
extern LispRef dynamic_bindings[];

/* Module bindings with size 29 */
LispRef ex_body_bindings[29];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module ex-body */
void initialize_module_ex_body()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_p_env();
  initialize_module_ex_expr();
  initialize_module_sx_obj();
  initialize_module_sx_node();
  eul_fast_table_set(eul_modules,"ex_body",(LispRef) ex_body_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1642, sym_1641, sym_1640, sym_1639, sym_1638, G001637, G001631, sym_1629, key_1627, key_1626, G001625, sym_1623, sym_1622, G001621, G001619, sym_1617, sym_1616, G001615;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 30 is_init: 0 index: 9 binding: complete-variable-node */
  static const void *G001614[] = {I(aa,23,00,00),B(ex_body ,7),I(24,00,00,00),B(dynamic ,3),I(3c,01,1c,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj2 ,11),I(08,1d,83,24),B(sx_obj2 ,11),I(08,1b,86,0f),I(1d,1c,0f,23),B(ex_body ,8),I(1c,0f,1f,05),I(26,00,00,00),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,60),I(08,1c,1c,0f),I(1f,07,1c,1c),I(26,00,00,00),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,60),I(09,22,02,2a),I(1f,08,24,00),B(ex_expr ,2),I(1c,83,1d,24),B(sx_obj2 ,11),I(09,45,0b,00)};

  /* Byte-vector with size: 30 is_init: 0 index: 10 binding: complete-constant-node */
  static const void *G001618[] = {I(aa,23,00,00),B(ex_body ,7),I(24,00,00,00),B(dynamic ,3),I(3c,01,1c,83),I(24,00,00,00),B(sx_obj2 ,21),I(08,1d,82,24),B(sx_obj2 ,29),I(08,87,86,0f),I(1c,1c,0f,1f),I(03,1c,0f,23),B(ex_body ,8),I(1c,0f,1f,06),I(26,00,00,00),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,60),I(08,1c,1c,0f),I(1f,08,1c,1c),I(26,00,00,00),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,60),I(09,22,02,2a),I(1f,09,24,00),B(ex_expr ,2),I(1c,82,1d,24),B(sx_obj2 ,29),I(09,45,0c,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 13 binding: set-up-top-level-lambda */
  static const void *G001620[] = {I(a9,24,00,00),B(sx_obj ,13),I(23,00,00,00),B(ex_body ,11),I(86,86,24,00),B(sx_node ,8),I(3c,04,24,00),B(dynamic ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,23,00),B(ex_body ,12),I(1d,1d,3c,02),I(2a,1c,45,02)};

  eul_allocate_static_string(str_1628, "    Top-level forms: ~a", 23);
  /* Byte-vector with size: 31 is_init: 0 index: 18 binding: complete-top-level-forms */
  static const void *G001624[] = {I(ab,1c,26,00),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,60),I(08,24,00,00),B(sx_obj ,42),I(23,00,00,00),B(ex_body ,14),I(1f,03,23,00),B(ex_body ,15),I(86,24,00,00),B(mop_gf ,2),I(3c,05,23,00),B(ex_body ,16),I(1d,24,00,00),B(i_notify ,4),I(3c,02,2a,23),B(ex_body ,17),I(1d,0f,1f,03),I(1c,1c,84,1d),I(24,00,00,00),B(sx_obj ,18),I(09,22,02,2a),I(1f,03,24,00),B(ex_expr ,27),I(3c,01,2a,1f),I(04,1d,1c,26),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,60),I(09,45,07,00)};

  eul_allocate_static_string(str_1632, "   Complete variable nodes", 26);
  eul_allocate_static_string(str_1633, "   Complete constant nodes", 26);
  eul_allocate_static_string(str_1634, "   Complete defun bodies", 24);
  eul_allocate_static_string(str_1635, "   Expand top-level forms", 25);
  /* Byte-vector with size: 50 is_init: 0 index: 23 binding: expand-bodies */
  static const void *G001630[] = {I(aa,24,00,00),B(ex_body ,4),I(3c,00,23,00),B(ex_body ,19),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,1c),I(26,00,00,00),I(00,00,00,0c),I(24,00,00,00),B(sx_obj1 ,60),I(08,24,00,00),B(ex_body ,2),I(1c,24,00,00),B(boot ,4),I(3c,02,2a,23),B(ex_body ,20),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,1d),I(26,00,00,00),I(00,00,00,0d),I(24,00,00,00),B(sx_obj1 ,60),I(08,24,00,00),B(ex_body ,3),I(1c,24,00,00),B(boot ,4),I(3c,02,2a,23),B(ex_body ,21),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,1f),I(03,26,00,00),I(00,00,00,09),I(24,00,00,00),B(sx_obj1 ,60),I(08,24,00,00),B(ex_expr ,27),I(1c,24,00,00),B(boot ,4),I(3c,02,2a,23),B(ex_body ,22),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,1f),I(04,1f,04,24),B(ex_body ,5),I(3d,02,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 58 is_init: 1 index: 0 binding: initialize-ex-body */
  static const void *G001636[] = {I(87,25,00,00),B(ex_body ,1),I(24,00,00,00),B(sx_node ,1),I(3e,0b,24,00),B(sx_node ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(ex_expr ,1),I(3e,0b,24,00),B(ex_expr ,0),I(3c,00,21,01),I(24,00,00,00),B(p_env ,1),I(3e,0b,24,00),B(p_env ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(ex_body ,24),I(23,00,00,00),B(ex_body ,23),I(3b,01,25,00),B(ex_body ,6),I(23,00,00,00),B(ex_body ,25),I(23,00,00,00),B(ex_body ,18),I(3b,02,25,00),B(ex_body ,5),I(23,00,00,00),B(ex_body ,26),I(23,00,00,00),B(ex_body ,13),I(3b,00,25,00),B(ex_body ,4),I(23,00,00,00),B(ex_body ,27),I(23,00,00,00),B(ex_body ,10),I(3b,01,25,00),B(ex_body ,3),I(23,00,00,00),B(ex_body ,28),I(23,00,00,00),B(ex_body ,9),I(3b,01,25,00),B(ex_body ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_1616,"*actual-module*");
  eul_intern_symbol(sym_1617,"setq");
  eul_allocate_bytevector( G001615,G001614);
  eul_allocate_bytevector( G001619,G001618);
  eul_intern_symbol(sym_1622,"top-level");
  eul_intern_symbol(sym_1623,"*encl-lambda*");
  eul_allocate_bytevector( G001621,G001620);
  eul_intern_keyword(key_1626,"fun");
  eul_intern_keyword(key_1627,"args");
  object_class(str_1628) = eul_static_string_class;
  eul_intern_symbol(sym_1629,"progn");
  eul_allocate_bytevector( G001625,G001624);
  object_class(str_1632) = eul_static_string_class;
  object_class(str_1633) = eul_static_string_class;
  object_class(str_1634) = eul_static_string_class;
  object_class(str_1635) = eul_static_string_class;
  eul_allocate_bytevector( G001631,G001630);
  eul_intern_symbol(sym_1638,"expand-bodies");
  eul_intern_symbol(sym_1639,"complete-top-level-forms");
  eul_intern_symbol(sym_1640,"set-up-top-level-lambda");
  eul_intern_symbol(sym_1641,"complete-constant-node");
  eul_intern_symbol(sym_1642,"complete-variable-node");
  eul_allocate_bytevector( G001637,G001636);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 7; i++)
      ex_body_bindings[i] = eul_nil;
  }

  ex_body_bindings[ 7] = sym_1616;
  ex_body_bindings[ 8] = sym_1617;
  ex_body_bindings[ 9] = G001615;
  ex_body_bindings[ 10] = G001619;
  ex_body_bindings[ 11] = sym_1622;
  ex_body_bindings[ 12] = sym_1623;
  ex_body_bindings[ 13] = G001621;
  ex_body_bindings[ 14] = key_1626;
  ex_body_bindings[ 15] = key_1627;
  ex_body_bindings[ 16] = str_1628;
  ex_body_bindings[ 17] = sym_1629;
  ex_body_bindings[ 18] = G001625;
  ex_body_bindings[ 19] = str_1632;
  ex_body_bindings[ 20] = str_1633;
  ex_body_bindings[ 21] = str_1634;
  ex_body_bindings[ 22] = str_1635;
  ex_body_bindings[ 23] = G001631;
  ex_body_bindings[ 1] = eul_nil;
  ex_body_bindings[ 24] = sym_1638;
  ex_body_bindings[ 25] = sym_1639;
  ex_body_bindings[ 26] = sym_1640;
  ex_body_bindings[ 27] = sym_1641;
  ex_body_bindings[ 28] = sym_1642;
  eul_allocate_lambda( ex_body_bindings[0], "initialize-ex-body", 0, G001637);

  }
}


/* eof */
