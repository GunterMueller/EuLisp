/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module sx-write
 **  Copyright: See file sx-write.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_sx_obj();
extern LispRef i_all_bindings[];
extern LispRef stream_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef mop_inspect_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef stream2_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef mop_defcl_bindings[];
extern LispRef mop_access_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef number_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef boot_bindings[];
extern LispRef sx_obj2_bindings[];
extern LispRef format_bindings[];
extern LispRef sx_obj_bindings[];

/* Module bindings with size 70 */
LispRef sx_write_bindings[70];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module sx-write */
void initialize_module_sx_write()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_sx_obj();
  eul_fast_table_set(eul_modules,"sx_write",(LispRef) sx_write_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_2268, sym_2267, sym_2266, G002265, sym_2259, G002258, G002256, G002253, G002250, G002247, G002243, sym_2241, sym_2240, sym_2239, G002238, G002235, G002231, G002229, G002226, sym_2222, G002221, G002218, G002215, G002212, G002209, G002206, G002203, sym_2197, G002196, G002193, sym_2190, G002188, G002185, G002182, G002179;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_2180, "(if ~a ~a ~a)", 13);
  /* Byte-vector with size: 14 is_init: 0 index: 6 binding: (method-new-generic-print) */
  static const void *G002178[] = {I(ab,1c,84,24),B(sx_obj ,12),I(08,1d,83,24),B(sx_obj ,12),I(08,1f,03,82),I(24,00,00,00),B(sx_obj ,12),I(08,1f,03,23),B(sx_write ,5),I(1f,04,1f,04),I(1f,04,24,00),B(format ,4),I(3d,05,05,45),I(05,00,00,00)};

  eul_allocate_static_string(str_2183, "~a", 2);
  /* Byte-vector with size: 11 is_init: 0 index: 8 binding: (method-new-generic-print) */
  static const void *G002181[] = {I(ab,1c,83,24),B(sx_obj ,39),I(08,1d,82,24),B(sx_obj ,39),I(08,1c,1c,0f),I(1f,03,23,00),B(sx_write ,7),I(1d,24,00,00),B(format ,4),I(3d,03,05,45),I(05,00,00,00)};

  eul_allocate_static_string(str_2186, "(~a ~a)", 7);
  /* Byte-vector with size: 13 is_init: 0 index: 10 binding: anonymous */
  static const void *G002184[] = {I(aa,1b,24,00),B(sx_obj2 ,33),I(3c,01,1b,44),I(0b,1c,83,24),B(sx_obj2 ,11),I(08,36,02,86),I(47,00,00,23),B(sx_write ,9),I(1f,04,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2189, "(let* (", 7);
  eul_allocate_static_string(str_2191, ") ~a)", 5);
  /* Byte-vector with size: 25 is_init: 0 index: 14 binding: (method-new-generic-print) */
  static const void *G002187[] = {I(ab,46,01,1b),I(48,00,00,47),I(00,00,23,00),B(sx_write ,11),I(24,00,00,00),B(format ,4),I(3c,02,2a,1c),I(8a,05,24,00),B(sx_obj ,17),I(08,23,00,00),B(sx_write ,12),I(23,00,00,00),B(sx_write ,10),I(3b,01,1c,24),B(boot ,17),I(3c,02,2a,1d),I(84,24,00,00),B(sx_obj ,17),I(08,47,00,00),I(23,00,00,00),B(sx_write ,13),I(1d,24,00,00),B(format ,4),I(3d,03,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2194, "(opencoded-lambda ~a ~a)", 24);
  /* Byte-vector with size: 12 is_init: 0 index: 16 binding: (method-new-generic-print) */
  static const void *G002192[] = {I(ab,1c,8a,05),I(24,00,00,00),B(sx_obj ,17),I(08,1d,84,24),B(sx_obj ,17),I(08,1d,23,00),B(sx_write ,15),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2198, "~a", 2);
  eul_allocate_static_string(str_2199, "inlined-lambda", 14);
  eul_allocate_static_string(str_2200, "lambda", 6);
  eul_allocate_static_string(str_2201, "(~a ~a ~a)", 10);
  /* Byte-vector with size: 42 is_init: 0 index: 22 binding: (method-new-generic-print) */
  static const void *G002195[] = {I(ab,23,00,00),B(sx_write ,17),I(24,00,00,00),B(dynamic ,3),I(3c,01,24,00),B(number ,24),I(3c,01,1b,44),I(16,23,00,00),B(sx_write ,17),I(24,00,00,00),B(dynamic ,3),I(3c,01,84,1a),I(36,02,86,1b),I(44,22,1f,03),I(8a,07,24,00),B(sx_obj ,17),I(08,1f,03,23),B(sx_write ,18),I(1d,24,00,00),B(format ,4),I(3d,03,05,22),I(01,36,4f,1f),I(03,84,24,00),B(sx_obj ,13),I(08,1b,44,0b),I(23,00,00,00),B(sx_write ,19),I(36,07,23,00),B(sx_write ,20),I(1f,05,8a,05),I(24,00,00,00),B(sx_obj ,17),I(08,1f,06,84),I(24,00,00,00),B(sx_obj ,17),I(08,1f,06,23),B(sx_write ,21),I(1f,04,1f,04),I(1f,04,24,00),B(format ,4),I(3d,05,08,22),I(04,45,04,00)};

  eul_allocate_static_string(str_2204, "~a", 2);
  /* Byte-vector with size: 8 is_init: 0 index: 24 binding: (method-new-generic-print) */
  static const void *G002202[] = {I(ab,1c,82,24),B(sx_obj2 ,29),I(08,1c,23,00),B(sx_write ,23),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2207, "~a", 2);
  /* Byte-vector with size: 8 is_init: 0 index: 26 binding: (method-new-generic-print) */
  static const void *G002205[] = {I(ab,1c,83,24),B(sx_obj2 ,22),I(08,1c,23,00),B(sx_write ,25),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2210, "(setq ~a ~a)", 12);
  /* Byte-vector with size: 11 is_init: 0 index: 28 binding: (method-new-generic-print) */
  static const void *G002208[] = {I(ab,1c,83,24),B(sx_obj2 ,25),I(08,1d,82,24),B(sx_obj2 ,25),I(08,1d,23,00),B(sx_write ,27),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2213, "~a", 2);
  /* Byte-vector with size: 9 is_init: 0 index: 30 binding: (method-new-generic-print) */
  static const void *G002211[] = {I(ab,1c,8a,03),I(24,00,00,00),B(sx_obj2 ,11),I(08,1c,23,00),B(sx_write ,29),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2216, "~a", 2);
  /* Byte-vector with size: 9 is_init: 0 index: 32 binding: (method-new-generic-print) */
  static const void *G002214[] = {I(ab,1c,8a,06),I(24,00,00,00),B(sx_obj1 ,42),I(08,1c,23,00),B(sx_write ,31),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2219, "~a", 2);
  /* Byte-vector with size: 9 is_init: 0 index: 34 binding: (method-new-generic-print) */
  static const void *G002217[] = {I(ab,1c,8a,15),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,23,00),B(sx_write ,33),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2223, "~a", 2);
  eul_allocate_static_string(str_2224, "<unprintable syntax object>", 27);
  /* Byte-vector with size: 23 is_init: 0 index: 38 binding: (method-new-generic-print) */
  static const void *G002220[] = {I(ab,1c,24,00),B(mop_access ,5),I(3c,01,23,00),B(sx_write ,35),I(1c,86,6c,1b),I(44,2e,1f,03),I(23,00,00,00),B(sx_write ,35),I(24,00,00,00),B(mop_defcl ,8),I(3c,02,1f,03),I(23,00,00,00),B(sx_write ,36),I(1d,24,00,00),B(format ,4),I(3d,03,05,22),I(01,36,15,1d),I(23,00,00,00),B(sx_write ,37),I(24,00,00,00),B(format ,4),I(3d,02,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2227, "#<macro-function>", 17);
  /* Byte-vector with size: 5 is_init: 0 index: 40 binding: (method-new-generic-print) */
  static const void *G002225[] = {I(ab,23,00,00),B(sx_write ,39),I(24,00,00,00),B(format ,4),I(3d,02,01,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 41 binding: (method-generic-print) */
  static const void *G002228[] = {I(ab,23,00,00),B(sx_write ,17),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,44),I(0d,1d,1d,24),B(sx_write ,3),I(3d,02,03,36),I(04,38,02,01),I(45,03,00,00)};

  eul_allocate_static_string(str_2232, "#<binding: ~a:~a:~a>", 20);
  eul_allocate_static_string(str_2233, "#<binding: ~a:~a>", 17);
  /* Byte-vector with size: 31 is_init: 0 index: 44 binding: (method-generic-print) */
  static const void *G002230[] = {I(ab,1c,8a,05),I(24,00,00,00),B(sx_obj1 ,42),I(08,1d,8a,06),I(24,00,00,00),B(sx_obj1 ,42),I(08,1f,03,83),I(24,00,00,00),B(sx_obj1 ,42),I(08,1d,24,00),B(sx_obj1 ,45),I(3c,01,1b,44),I(0f,1f,03,8a),I(15,24,00,00),B(sx_obj1 ,59),I(08,36,03,1f),I(03,1d,7e,1b),I(44,1c,1f,06),I(23,00,00,00),B(sx_write ,42),I(1f,03,1f,07),I(1f,07,24,00),B(format ,4),I(3d,05,08,36),I(17,1f,06,23),B(sx_write ,43),I(1f,03,1f,07),I(24,00,00,00),B(format ,4),I(3d,04,08,45),I(08,00,00,00)};

  eul_allocate_static_string(str_2236, "#<module: ~a>", 13);
  /* Byte-vector with size: 9 is_init: 0 index: 46 binding: (method-generic-print) */
  static const void *G002234[] = {I(ab,1c,8a,15),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,23,00),B(sx_write ,45),I(1d,24,00,00),B(format ,4),I(3d,03,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 447 is_init: 0 index: 50 binding: top-level */
  static const void *G002237[] = {I(a9,84,24,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(sx_write ,47),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(sx_write ,3),I(2a,24,00,00),B(stream2 ,31),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,59),I(24,00,00,00),B(stream2 ,20),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,31),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,48),I(23,00,00,00),B(sx_write ,46),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,31),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,31),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,42),I(24,00,00,00),B(stream2 ,20),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,31),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,48),I(23,00,00,00),B(sx_write ,44),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,31),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,31),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,27),I(24,00,00,00),B(stream2 ,20),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,31),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,48),I(23,00,00,00),B(sx_write ,41),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,31),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(2a,24,00,00),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(mop_class ,32),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,40),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,27),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,38),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,59),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,34),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj1 ,42),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,32),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj2 ,11),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,30),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj2 ,25),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,28),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj2 ,22),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,26),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj2 ,32),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,24),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj ,13),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,22),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj ,8),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,16),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj ,2),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,14),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj ,39),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,8),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(sx_write ,3),I(8a,03,02,84),I(24,00,00,00),B(sx_obj ,12),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(sx_write ,3),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(sx_write ,49),I(23,00,00,00),B(sx_write ,6),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(sx_write ,3),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,64,45),I(64,00,00,00)};

  eul_allocate_static_string(str_2244, "#<macro-function>", 17);
  eul_allocate_static_string(str_2245, "~a", 2);
  /* Byte-vector with size: 13 is_init: 0 index: 53 binding: binding-print-string */
  static const void *G002242[] = {I(aa,84,24,00),B(sx_obj1 ,42),I(08,1b,24,00),B(mop_inspect ,8),I(3c,01,1b,44),I(0a,23,00,00),B(sx_write ,51),I(36,12,23,00),B(sx_write ,52),I(1d,24,00,00),B(format ,2),I(3d,02,02,45),I(02,00,00,00)};

  eul_allocate_static_string(str_2248, "\n   (~a . ~a)", 13);
  /* Byte-vector with size: 13 is_init: 0 index: 55 binding: anonymous */
  static const void *G002246[] = {I(ab,1b,8a,06),I(24,00,00,00),B(sx_obj1 ,42),I(08,1c,24,00),B(sx_write ,2),I(3c,01,47,00),I(00,23,00,00),B(sx_write ,54),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2251, "\n   (~a . ~a)", 13);
  /* Byte-vector with size: 13 is_init: 0 index: 57 binding: anonymous */
  static const void *G002249[] = {I(ab,1b,8a,06),I(24,00,00,00),B(sx_obj1 ,42),I(08,1c,24,00),B(sx_write ,2),I(3c,01,47,00),I(00,23,00,00),B(sx_write ,56),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,04,45),I(04,00,00,00)};

  eul_allocate_static_string(str_2254, "\n   (~a . ~a)", 13);
  /* Byte-vector with size: 13 is_init: 0 index: 59 binding: anonymous */
  static const void *G002252[] = {I(ab,1b,8a,06),I(24,00,00,00),B(sx_obj1 ,42),I(08,1c,24,00),B(sx_write ,2),I(3c,01,47,00),I(00,23,00,00),B(sx_write ,58),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 60 binding: anonymous */
  static const void *G002255[] = {I(a9,83,24,00),B(dynamic ,8),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_2260, "\nPretty printed environment of module ~a:", 41);
  eul_allocate_static_string(str_2261, "\n  lexical-env:", 15);
  eul_allocate_static_string(str_2262, "\n  external-env:", 16);
  eul_allocate_static_string(str_2263, "\n  syntax-env:", 14);
  /* Byte-vector with size: 87 is_init: 0 index: 66 binding: pprint-module */
  static const void *G002257[] = {I(a7,46,01,1b),I(44,05,1b,10),I(36,02,86,1b),I(44,04,1b,36),I(02,87,1b,48),I(00,00,23,00),B(sx_write ,17),I(87,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(sx_write ,61),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(sx_write ,12),I(23,00,00,00),B(sx_write ,60),I(3b,00,1c,0f),I(23,00,00,00),B(sx_write ,61),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,1f,06),I(24,00,00,00),B(mop_access ,8),I(3c,02,2a,1f),I(05,8a,15,24),B(sx_obj1 ,59),I(08,47,00,00),I(23,00,00,00),B(sx_write ,62),I(1d,24,00,00),B(format ,4),I(3c,03,2a,47),I(00,00,23,00),B(sx_write ,63),I(24,00,00,00),B(format ,4),I(3c,02,2a,1f),I(06,8a,10,24),B(sx_obj1 ,59),I(08,23,00,00),B(sx_write ,12),I(23,00,00,00),B(sx_write ,59),I(3b,02,1c,24),B(aux_table ,7),I(3c,02,2a,47),I(00,00,23,00),B(sx_write ,64),I(24,00,00,00),B(format ,4),I(3c,02,2a,1f),I(07,8a,0f,24),B(sx_obj1 ,59),I(08,23,00,00),B(sx_write ,12),I(23,00,00,00),B(sx_write ,57),I(3b,02,1c,24),B(aux_table ,7),I(3c,02,2a,47),I(00,00,23,00),B(sx_write ,65),I(24,00,00,00),B(format ,4),I(3c,02,2a,1f),I(08,8a,0e,24),B(sx_obj1 ,59),I(08,23,00,00),B(sx_write ,12),I(23,00,00,00),B(sx_write ,55),I(3b,02,1c,24),B(aux_table ,7),I(3c,02,2a,47),I(00,00,27,0a),I(24,00,00,00),B(stream ,8),I(3c,02,83,24),B(dynamic ,8),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(0a,45,0b,00)};

  /* Byte-vector with size: 32 is_init: 1 index: 0 binding: initialize-sx-write */
  static const void *G002264[] = {I(87,25,00,00),B(sx_write ,1),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(sx_write ,67),I(23,00,00,00),B(sx_write ,66),I(3b,fe,25,00),B(sx_write ,4),I(86,25,00,00),B(sx_write ,3),I(23,00,00,00),B(sx_write ,68),I(23,00,00,00),B(sx_write ,53),I(3b,01,25,00),B(sx_write ,2),I(23,00,00,00),B(sx_write ,69),I(23,00,00,00),B(sx_write ,50),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_2180) = eul_static_string_class;
  eul_allocate_bytevector( G002179,G002178);
  object_class(str_2183) = eul_static_string_class;
  eul_allocate_bytevector( G002182,G002181);
  object_class(str_2186) = eul_static_string_class;
  eul_allocate_bytevector( G002185,G002184);
  object_class(str_2189) = eul_static_string_class;
  eul_intern_symbol(sym_2190,"anonymous");
  object_class(str_2191) = eul_static_string_class;
  eul_allocate_bytevector( G002188,G002187);
  object_class(str_2194) = eul_static_string_class;
  eul_allocate_bytevector( G002193,G002192);
  eul_intern_symbol(sym_2197,"*pprint*");
  object_class(str_2198) = eul_static_string_class;
  object_class(str_2199) = eul_static_string_class;
  object_class(str_2200) = eul_static_string_class;
  object_class(str_2201) = eul_static_string_class;
  eul_allocate_bytevector( G002196,G002195);
  object_class(str_2204) = eul_static_string_class;
  eul_allocate_bytevector( G002203,G002202);
  object_class(str_2207) = eul_static_string_class;
  eul_allocate_bytevector( G002206,G002205);
  object_class(str_2210) = eul_static_string_class;
  eul_allocate_bytevector( G002209,G002208);
  object_class(str_2213) = eul_static_string_class;
  eul_allocate_bytevector( G002212,G002211);
  object_class(str_2216) = eul_static_string_class;
  eul_allocate_bytevector( G002215,G002214);
  object_class(str_2219) = eul_static_string_class;
  eul_allocate_bytevector( G002218,G002217);
  eul_intern_symbol(sym_2222,"binding");
  object_class(str_2223) = eul_static_string_class;
  object_class(str_2224) = eul_static_string_class;
  eul_allocate_bytevector( G002221,G002220);
  object_class(str_2227) = eul_static_string_class;
  eul_allocate_bytevector( G002226,G002225);
  eul_allocate_bytevector( G002229,G002228);
  object_class(str_2232) = eul_static_string_class;
  object_class(str_2233) = eul_static_string_class;
  eul_allocate_bytevector( G002231,G002230);
  object_class(str_2236) = eul_static_string_class;
  eul_allocate_bytevector( G002235,G002234);
  eul_intern_symbol(sym_2239,"new-generic-print");
  eul_intern_symbol(sym_2240,"(method generic-print)");
  eul_intern_symbol(sym_2241,"(method new-generic-print)");
  eul_allocate_bytevector( G002238,G002237);
  object_class(str_2244) = eul_static_string_class;
  object_class(str_2245) = eul_static_string_class;
  eul_allocate_bytevector( G002243,G002242);
  object_class(str_2248) = eul_static_string_class;
  eul_allocate_bytevector( G002247,G002246);
  object_class(str_2251) = eul_static_string_class;
  eul_allocate_bytevector( G002250,G002249);
  object_class(str_2254) = eul_static_string_class;
  eul_allocate_bytevector( G002253,G002252);
  eul_allocate_bytevector( G002256,G002255);
  eul_intern_symbol(sym_2259,"*clean-ups*");
  object_class(str_2260) = eul_static_string_class;
  object_class(str_2261) = eul_static_string_class;
  object_class(str_2262) = eul_static_string_class;
  object_class(str_2263) = eul_static_string_class;
  eul_allocate_bytevector( G002258,G002257);
  eul_intern_symbol(sym_2266,"pprint-module");
  eul_intern_symbol(sym_2267,"binding-print-string");
  eul_intern_symbol(sym_2268,"top-level");
  eul_allocate_bytevector( G002265,G002264);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 5; i++)
      sx_write_bindings[i] = eul_nil;
  }

  sx_write_bindings[ 5] = str_2180;
  sx_write_bindings[ 6] = G002179;
  sx_write_bindings[ 7] = str_2183;
  sx_write_bindings[ 8] = G002182;
  sx_write_bindings[ 9] = str_2186;
  sx_write_bindings[ 10] = G002185;
  sx_write_bindings[ 11] = str_2189;
  sx_write_bindings[ 12] = sym_2190;
  sx_write_bindings[ 13] = str_2191;
  sx_write_bindings[ 14] = G002188;
  sx_write_bindings[ 15] = str_2194;
  sx_write_bindings[ 16] = G002193;
  sx_write_bindings[ 17] = sym_2197;
  sx_write_bindings[ 18] = str_2198;
  sx_write_bindings[ 19] = str_2199;
  sx_write_bindings[ 20] = str_2200;
  sx_write_bindings[ 21] = str_2201;
  sx_write_bindings[ 22] = G002196;
  sx_write_bindings[ 23] = str_2204;
  sx_write_bindings[ 24] = G002203;
  sx_write_bindings[ 25] = str_2207;
  sx_write_bindings[ 26] = G002206;
  sx_write_bindings[ 27] = str_2210;
  sx_write_bindings[ 28] = G002209;
  sx_write_bindings[ 29] = str_2213;
  sx_write_bindings[ 30] = G002212;
  sx_write_bindings[ 31] = str_2216;
  sx_write_bindings[ 32] = G002215;
  sx_write_bindings[ 33] = str_2219;
  sx_write_bindings[ 34] = G002218;
  sx_write_bindings[ 35] = sym_2222;
  sx_write_bindings[ 36] = str_2223;
  sx_write_bindings[ 37] = str_2224;
  sx_write_bindings[ 38] = G002221;
  sx_write_bindings[ 39] = str_2227;
  sx_write_bindings[ 40] = G002226;
  sx_write_bindings[ 41] = G002229;
  sx_write_bindings[ 42] = str_2232;
  sx_write_bindings[ 43] = str_2233;
  sx_write_bindings[ 44] = G002231;
  sx_write_bindings[ 45] = str_2236;
  sx_write_bindings[ 46] = G002235;
  sx_write_bindings[ 47] = sym_2239;
  sx_write_bindings[ 48] = sym_2240;
  sx_write_bindings[ 49] = sym_2241;
  sx_write_bindings[ 50] = G002238;
  sx_write_bindings[ 51] = str_2244;
  sx_write_bindings[ 52] = str_2245;
  sx_write_bindings[ 53] = G002243;
  sx_write_bindings[ 54] = str_2248;
  sx_write_bindings[ 55] = G002247;
  sx_write_bindings[ 56] = str_2251;
  sx_write_bindings[ 57] = G002250;
  sx_write_bindings[ 58] = str_2254;
  sx_write_bindings[ 59] = G002253;
  sx_write_bindings[ 60] = G002256;
  sx_write_bindings[ 61] = sym_2259;
  sx_write_bindings[ 62] = str_2260;
  sx_write_bindings[ 63] = str_2261;
  sx_write_bindings[ 64] = str_2262;
  sx_write_bindings[ 65] = str_2263;
  sx_write_bindings[ 66] = G002258;
  sx_write_bindings[ 1] = eul_nil;
  sx_write_bindings[ 67] = sym_2266;
  sx_write_bindings[ 68] = sym_2267;
  sx_write_bindings[ 69] = sym_2268;
  eul_allocate_lambda( sx_write_bindings[0], "initialize-sx-write", 0, G002265);

  }
}


/* eof */
