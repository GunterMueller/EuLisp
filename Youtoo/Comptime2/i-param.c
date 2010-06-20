/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module i-param
 **  Copyright: See file i-param.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_level1();
extern LispRef i_level1_bindings[];
extern LispRef number_bindings[];
extern LispRef read_bindings[];
extern LispRef stream2_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef compare_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot_bindings[];
extern LispRef boot1_bindings[];

/* Module bindings with size 124 */
LispRef i_param_bindings[124];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module i-param */
void initialize_module_i_param()
{
  if (is_initialized) return;
  initialize_module_i_level1();
  eul_fast_table_set(eul_modules,"i_param",(LispRef) i_param_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_169, sym_168, sym_167, sym_166, sym_165, sym_164, sym_163, G00162, sym_160, G00159, G00155, G00153, sym_151, G00150, G00147, sym_144, sym_143, key_142, G00138, G00136, G00134, sym_131, sym_129, sym_128, sym_127, sym_126, sym_125, sym_124, sym_123, sym_122, sym_121, sym_120, key_119, sym_116, sym_115, sym_114, sym_113, sym_112, sym_111, G00104;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_105, "/", 1);
  eul_allocate_static_string(str_106, "EUL_LOAD_PATH", 13);
  eul_allocate_static_string(str_107, "EUL_DIR", 7);
  eul_allocate_static_string(str_108, "EUL_ARCH", 8);
  eul_allocate_static_string(str_109, "EUL_LIBRARY_LOAD_PATH", 21);
  eul_allocate_static_string(str_110, "EUL_START_MODULE", 16);
  eul_allocate_static_string(str_117, "Lib.~a/eul-appl.o", 17);
  eul_allocate_static_string(str_118, "~a~a~a", 6);
  eul_allocate_static_string(str_130, "", 0);
  eul_allocate_static_string(str_132, "", 0);
  /* Byte-vector with size: 330 is_init: 0 index: 96 binding: top-level */
  static const void *G00103[] = {I(a9,23,00,00),B(i_param ,68),I(89,00,00,00),B(i_param ,23),I(2a,23,00,00),B(i_param ,69),I(24,00,00,00),B(boot1 ,18),I(3c,01,24,00),B(boot ,25),I(3c,01,1b,89),B(i_param ,17),I(2a,23,00,00),B(i_param ,70),I(24,00,00,00),B(boot1 ,18),I(3c,01,1b,89),B(i_param ,2),I(2a,23,00,00),B(i_param ,71),I(24,00,00,00),B(boot1 ,18),I(3c,01,1b,89),B(i_param ,35),I(2a,23,00,00),B(i_param ,72),I(24,00,00,00),B(boot1 ,18),I(3c,01,24,00),B(boot ,25),I(3c,01,1b,89),B(i_param ,9),I(2a,23,00,00),B(i_param ,73),I(24,00,00,00),B(boot1 ,18),I(3c,01,1b,89),B(i_param ,12),I(2a,86,89,00),B(i_param ,37),I(2a,24,00,00),B(i_param ,27),I(3c,00,1b,89),B(i_param ,16),I(2a,23,00,00),B(i_param ,74),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,44),I(2a,23,00,00),B(i_param ,75),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,50),I(2a,86,89,00),B(i_param ,10),I(2a,23,00,00),B(i_param ,76),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,45),I(2a,24,00,00),B(i_param ,45),I(89,00,00,00),B(i_param ,66),I(2a,24,00,00),B(i_param ,67),I(3c,00,1b,89),B(i_param ,55),I(2a,24,00,00),B(i_param ,33),I(3c,00,1b,89),B(i_param ,22),I(2a,86,89,00),B(i_param ,57),I(2a,23,00,00),B(i_param ,77),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,38),I(2a,23,00,00),B(i_param ,78),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,8),I(2a,86,89,00),B(i_param ,61),I(2a,86,89,00),B(i_param ,29),I(2a,86,89,00),B(i_param ,15),I(2a,86,89,00),B(i_param ,13),I(2a,86,89,00),B(i_param ,4),I(2a,87,89,00),B(i_param ,7),I(2a,23,00,00),B(i_param ,79),I(24,00,00,00),B(i_param ,28),I(3c,01,86,23),B(i_param ,80),I(1d,24,00,00),B(mop_gf ,17),I(3c,03,86,23),B(i_param ,81),I(24,00,00,00),B(i_param ,2),I(24,00,00,00),B(i_param ,23),I(1f,04,24,00),B(mop_gf ,17),I(3c,05,1b,89),B(i_param ,54),I(2a,86,89,00),B(i_param ,43),I(2a,86,89,00),B(i_param ,21),I(2a,86,89,00),B(i_param ,52),I(2a,87,89,00),B(i_param ,59),I(2a,87,89,00),B(i_param ,26),I(2a,82,89,00),B(i_param ,41),I(2a,82,89,00),B(i_param ,47),I(2a,86,89,00),B(i_param ,56),I(2a,86,89,00),B(i_param ,46),I(2a,83,89,00),B(i_param ,65),I(2a,87,89,00),B(i_param ,51),I(2a,86,89,00),B(i_param ,58),I(2a,86,89,00),B(i_param ,36),I(2a,86,89,00),B(i_param ,63),I(2a,86,89,00),B(i_param ,14),I(2a,86,89,00),B(i_param ,32),I(2a,86,89,00),B(i_param ,34),I(2a,86,89,00),B(i_param ,62),I(2a,86,89,00),B(i_param ,24),I(2a,86,89,00),B(i_param ,6),I(2a,86,89,00),B(i_param ,53),I(2a,86,89,00),B(i_param ,49),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(i_param ,20),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(i_param ,31),I(2a,23,00,00),B(i_param ,82),I(24,00,00,00),B(compare ,9),I(24,00,00,00),B(aux_table ,4),I(3c,02,1b,89),B(i_param ,11),I(2a,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(i_param ,42),I(2a,86,89,00),B(i_param ,30),I(2a,23,00,00),B(i_param ,83),I(89,00,00,00),B(i_param ,60),I(2a,86,89,00),B(i_param ,25),I(2a,23,00,00),B(i_param ,82),I(24,00,00,00),B(compare ,9),I(24,00,00,00),B(aux_table ,4),I(3c,02,1b,89),B(i_param ,48),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(i_param ,84),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(i_param ,19),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(i_param ,85),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(i_param ,5),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(i_param ,86),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(i_param ,18),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(i_param ,87),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(i_param ,3),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(i_param ,88),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(i_param ,40),I(2a,23,00,00),B(i_param ,89),I(86,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_param ,90),I(86,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_param ,91),I(86,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_param ,92),I(23,00,00,00),B(i_param ,93),I(24,00,00,00),B(dynamic ,2),I(3c,02,2a,23),B(i_param ,94),I(23,00,00,00),B(i_param ,95),I(24,00,00,00),B(dynamic ,2),I(3c,02,2a,24),B(i_param ,19),I(2a,24,00,00),B(i_param ,5),I(2a,24,00,00),B(i_param ,18),I(2a,24,00,00),B(i_param ,3),I(2a,24,00,00),B(i_param ,40),I(45,29,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 97 binding: anonymous */
  static const void *G00133[] = {I(a9,47,00,01),I(24,00,00,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 98 binding: anonymous */
  static const void *G00135[] = {I(a9,47,00,00),I(24,00,00,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_139, "HOME", 4);
  eul_allocate_static_string(str_140, "~a~a.eulrc.~a", 13);
  eul_allocate_static_string(str_141, "~a~a.eulrc.~a", 13);
  eul_allocate_static_string(str_145, "test -f ~a", 10);
  /* Byte-vector with size: 112 is_init: 0 index: 106 binding: load-config-info */
  static const void *G00137[] = {I(a9,46,02,23),B(i_param ,99),I(24,00,00,00),B(boot1 ,18),I(3c,01,86,23),B(i_param ,100),I(1d,24,00,00),B(i_param ,23),I(24,00,00,00),B(i_param ,35),I(24,00,00,00),B(mop_gf ,17),I(3c,05,86,23),B(i_param ,101),I(24,00,00,00),B(i_param ,2),I(24,00,00,00),B(i_param ,23),I(24,00,00,00),B(i_param ,35),I(24,00,00,00),B(mop_gf ,17),I(3c,05,24,00),B(stream2 ,5),I(23,00,00,00),B(i_param ,102),I(1d,24,00,00),B(mop_gf ,2),I(3c,03,86,1c),I(48,00,00,23),B(i_param ,103),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(i_param ,104),I(23,00,00,00),B(i_param ,98),I(3b,00,1c,0f),I(23,00,00,00),B(i_param ,103),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(read ,12),I(3c,01,1b,20),I(04,1f,03,47),I(00,00,24,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(04,22,06,86),I(23,00,00,00),B(i_param ,105),I(1f,04,24,00),B(mop_gf ,17),I(3c,03,1b,24),B(boot1 ,48),I(3c,01,24,00),B(number ,16),I(3c,01,1b,34),I(00,00,00,c1),I(24,00,00,00),B(stream2 ,5),I(23,00,00,00),B(i_param ,102),I(1f,06,24,00),B(mop_gf ,2),I(3c,03,86,1c),I(48,00,01,23),B(i_param ,103),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(i_param ,104),I(23,00,00,00),B(i_param ,97),I(3b,00,1c,0f),I(23,00,00,00),B(i_param ,103),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,01,24,00),B(read ,12),I(3c,01,1b,20),I(04,1f,03,47),I(00,01,24,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(04,22,06,23),B(i_param ,74),I(1c,24,00,00),B(boot ,16),I(3c,02,23,00),B(i_param ,74),I(1f,05,24,00),B(boot ,16),I(3c,02,24,00),B(compare ,9),I(3c,02,1b,34),I(00,00,00,0d),I(1c,32,00,00),I(00,00,00,08),I(1c,22,02,32),I(00,00,00,06),I(1d,45,06,00)};

  eul_allocate_static_string(str_148, "", 0);
  /* Byte-vector with size: 11 is_init: 0 index: 108 binding: get-config-info */
  static const void *G00146[] = {I(aa,24,00,00),B(i_param ,16),I(1c,24,00,00),B(boot ,16),I(3c,02,1b,34),I(00,00,00,0d),I(1b,11,32,00),I(00,00,00,0e),I(23,00,00,00),B(i_param ,107),I(45,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 110 binding: get-cc-libs */
  static const void *G00149[] = {I(a9,23,00,00),B(i_param ,109),I(24,00,00,00),B(i_param ,28),I(3d,01,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 111 binding: stop-after-pass */
  static const void *G00152[] = {I(a8,1b,34,00),I(00,00,00,0e),I(1b,10,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,0f),I(24,00,00,00),B(i_param ,60),I(1b,89,00,00),B(i_param ,25),I(45,03,00,00)};

  eul_allocate_static_string(str_156, "EUL_LOAD_PATH", 13);
  eul_allocate_static_string(str_157, "EUL_LIBRARY_LOAD_PATH", 21);
  /* Byte-vector with size: 122 is_init: 0 index: 114 binding: ct-reset */
  static const void *G00154[] = {I(a9,86,89,00),B(i_param ,10),I(2a,23,00,00),B(i_param ,76),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,45),I(2a,24,00,00),B(i_param ,45),I(89,00,00,00),B(i_param ,66),I(2a,86,89,00),B(i_param ,57),I(2a,24,00,00),B(i_param ,67),I(3c,00,1b,89),B(i_param ,55),I(2a,24,00,00),B(i_param ,33),I(3c,00,1b,89),B(i_param ,22),I(2a,23,00,00),B(i_param ,77),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,38),I(2a,23,00,00),B(i_param ,78),I(24,00,00,00),B(i_param ,28),I(3c,01,1b,89),B(i_param ,8),I(2a,86,89,00),B(i_param ,61),I(2a,86,89,00),B(i_param ,29),I(2a,23,00,00),B(i_param ,112),I(24,00,00,00),B(boot1 ,18),I(3c,01,24,00),B(boot ,25),I(3c,01,1b,89),B(i_param ,17),I(2a,23,00,00),B(i_param ,113),I(24,00,00,00),B(boot1 ,18),I(3c,01,24,00),B(boot ,25),I(3c,01,1b,89),B(i_param ,9),I(2a,86,89,00),B(i_param ,15),I(2a,86,89,00),B(i_param ,13),I(2a,86,89,00),B(i_param ,4),I(2a,87,89,00),B(i_param ,7),I(2a,86,89,00),B(i_param ,43),I(2a,86,89,00),B(i_param ,21),I(2a,86,89,00),B(i_param ,52),I(2a,87,89,00),B(i_param ,59),I(2a,87,89,00),B(i_param ,26),I(2a,82,89,00),B(i_param ,41),I(2a,82,89,00),B(i_param ,47),I(2a,86,89,00),B(i_param ,34),I(2a,86,89,00),B(i_param ,6),I(2a,86,89,00),B(i_param ,53),I(2a,86,89,00),B(i_param ,30),I(2a,23,00,00),B(i_param ,83),I(89,00,00,00),B(i_param ,60),I(2a,86,89,00),B(i_param ,25),I(2a,86,89,00),B(i_param ,46),I(2a,83,89,00),B(i_param ,65),I(2a,87,89,00),B(i_param ,51),I(2a,86,89,00),B(i_param ,58),I(2a,86,89,00),B(i_param ,36),I(2a,86,89,00),B(i_param ,63),I(2a,86,89,00),B(i_param ,37),I(2a,86,89,00),B(i_param ,62),I(2a,86,89,00),B(i_param ,24),I(2a,24,00,00),B(i_param ,48),I(24,00,00,00),B(aux_table ,9),I(3c,01,2a,24),B(i_param ,11),I(24,00,00,00),B(aux_table ,9),I(3c,01,2a,24),B(i_param ,42),I(24,00,00,00),B(aux_table ,9),I(3d,01,07,45),I(07,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 116 binding: get-cc-flags */
  static const void *G00158[] = {I(a9,23,00,00),B(i_param ,115),I(24,00,00,00),B(i_param ,28),I(3d,01,00,00)};

  /* Byte-vector with size: 169 is_init: 1 index: 0 binding: initialize-i-param */
  static const void *G00161[] = {I(87,25,00,00),B(i_param ,1),I(24,00,00,00),B(i_level1 ,1),I(3e,0b,24,00),B(i_level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(i_param ,117),I(23,00,00,00),B(i_param ,116),I(3b,00,25,00),B(i_param ,67),I(86,25,00,00),B(i_param ,66),I(86,25,00,00),B(i_param ,65),I(23,00,00,00),B(i_param ,118),I(23,00,00,00),B(i_param ,114),I(3b,00,25,00),B(i_param ,64),I(86,25,00,00),B(i_param ,63),I(86,25,00,00),B(i_param ,62),I(86,25,00,00),B(i_param ,61),I(86,25,00,00),B(i_param ,60),I(86,25,00,00),B(i_param ,59),I(86,25,00,00),B(i_param ,58),I(86,25,00,00),B(i_param ,57),I(86,25,00,00),B(i_param ,56),I(86,25,00,00),B(i_param ,55),I(86,25,00,00),B(i_param ,54),I(86,25,00,00),B(i_param ,53),I(86,25,00,00),B(i_param ,52),I(86,25,00,00),B(i_param ,51),I(86,25,00,00),B(i_param ,50),I(86,25,00,00),B(i_param ,49),I(86,25,00,00),B(i_param ,48),I(86,25,00,00),B(i_param ,47),I(86,25,00,00),B(i_param ,46),I(86,25,00,00),B(i_param ,45),I(86,25,00,00),B(i_param ,44),I(86,25,00,00),B(i_param ,43),I(86,25,00,00),B(i_param ,42),I(86,25,00,00),B(i_param ,41),I(86,25,00,00),B(i_param ,40),I(23,00,00,00),B(i_param ,119),I(23,00,00,00),B(i_param ,111),I(3b,ff,25,00),B(i_param ,39),I(86,25,00,00),B(i_param ,38),I(86,25,00,00),B(i_param ,37),I(86,25,00,00),B(i_param ,36),I(86,25,00,00),B(i_param ,35),I(86,25,00,00),B(i_param ,34),I(23,00,00,00),B(i_param ,120),I(23,00,00,00),B(i_param ,110),I(3b,00,25,00),B(i_param ,33),I(86,25,00,00),B(i_param ,32),I(86,25,00,00),B(i_param ,31),I(86,25,00,00),B(i_param ,30),I(86,25,00,00),B(i_param ,29),I(23,00,00,00),B(i_param ,121),I(23,00,00,00),B(i_param ,108),I(3b,01,25,00),B(i_param ,28),I(23,00,00,00),B(i_param ,122),I(23,00,00,00),B(i_param ,106),I(3b,00,25,00),B(i_param ,27),I(86,25,00,00),B(i_param ,26),I(86,25,00,00),B(i_param ,25),I(86,25,00,00),B(i_param ,24),I(86,25,00,00),B(i_param ,23),I(86,25,00,00),B(i_param ,22),I(86,25,00,00),B(i_param ,21),I(86,25,00,00),B(i_param ,20),I(86,25,00,00),B(i_param ,19),I(86,25,00,00),B(i_param ,18),I(86,25,00,00),B(i_param ,17),I(86,25,00,00),B(i_param ,16),I(86,25,00,00),B(i_param ,15),I(86,25,00,00),B(i_param ,14),I(86,25,00,00),B(i_param ,13),I(86,25,00,00),B(i_param ,12),I(86,25,00,00),B(i_param ,11),I(86,25,00,00),B(i_param ,10),I(86,25,00,00),B(i_param ,9),I(86,25,00,00),B(i_param ,8),I(86,25,00,00),B(i_param ,7),I(86,25,00,00),B(i_param ,6),I(86,25,00,00),B(i_param ,5),I(86,25,00,00),B(i_param ,4),I(86,25,00,00),B(i_param ,3),I(86,25,00,00),B(i_param ,2),I(23,00,00,00),B(i_param ,123),I(23,00,00,00),B(i_param ,96),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_105) = eul_static_string_class;
  object_class(str_106) = eul_static_string_class;
  object_class(str_107) = eul_static_string_class;
  object_class(str_108) = eul_static_string_class;
  object_class(str_109) = eul_static_string_class;
  object_class(str_110) = eul_static_string_class;
  eul_intern_symbol(sym_111,"VERSION");
  eul_intern_symbol(sym_112,"OSTYPE");
  eul_intern_symbol(sym_113,"CC");
  eul_intern_symbol(sym_114,"AR");
  eul_intern_symbol(sym_115,"RANLIB");
  eul_intern_symbol(sym_116,"ARCH");
  object_class(str_117) = eul_static_string_class;
  object_class(str_118) = eul_static_string_class;
  eul_intern_keyword(key_119,"comparator");
  eul_intern_symbol(sym_120,"start");
  eul_intern_symbol(sym_121,"compile-module");
  eul_intern_symbol(sym_122,"load-syntax-module");
  eul_intern_symbol(sym_123,"parse-module");
  eul_intern_symbol(sym_124,"get-ct-error-condition-class");
  eul_intern_symbol(sym_125,"get-named-encl-lambda");
  eul_intern_symbol(sym_126,"*actual-module*");
  eul_intern_symbol(sym_127,"*encl-lambda*");
  eul_intern_symbol(sym_128,"*pprint*");
  eul_intern_symbol(sym_129,"*indent*");
  object_class(str_130) = eul_static_string_class;
  eul_intern_symbol(sym_131,"*trace-indent*");
  object_class(str_132) = eul_static_string_class;
  eul_allocate_bytevector( G00104,G00103);
  eul_allocate_bytevector( G00134,G00133);
  eul_allocate_bytevector( G00136,G00135);
  object_class(str_139) = eul_static_string_class;
  object_class(str_140) = eul_static_string_class;
  object_class(str_141) = eul_static_string_class;
  eul_intern_keyword(key_142,"file-name");
  eul_intern_symbol(sym_143,"*clean-ups*");
  eul_intern_symbol(sym_144,"anonymous");
  object_class(str_145) = eul_static_string_class;
  eul_allocate_bytevector( G00138,G00137);
  object_class(str_148) = eul_static_string_class;
  eul_allocate_bytevector( G00147,G00146);
  eul_intern_symbol(sym_151,"CLIBS");
  eul_allocate_bytevector( G00150,G00149);
  eul_allocate_bytevector( G00153,G00152);
  object_class(str_156) = eul_static_string_class;
  object_class(str_157) = eul_static_string_class;
  eul_allocate_bytevector( G00155,G00154);
  eul_intern_symbol(sym_160,"CFLAGS");
  eul_allocate_bytevector( G00159,G00158);
  eul_intern_symbol(sym_163,"get-cc-flags");
  eul_intern_symbol(sym_164,"ct-reset");
  eul_intern_symbol(sym_165,"stop-after-pass");
  eul_intern_symbol(sym_166,"get-cc-libs");
  eul_intern_symbol(sym_167,"get-config-info");
  eul_intern_symbol(sym_168,"load-config-info");
  eul_intern_symbol(sym_169,"top-level");
  eul_allocate_bytevector( G00162,G00161);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 68; i++)
      i_param_bindings[i] = eul_nil;
  }

  i_param_bindings[ 68] = str_105;
  i_param_bindings[ 69] = str_106;
  i_param_bindings[ 70] = str_107;
  i_param_bindings[ 71] = str_108;
  i_param_bindings[ 72] = str_109;
  i_param_bindings[ 73] = str_110;
  i_param_bindings[ 74] = sym_111;
  i_param_bindings[ 75] = sym_112;
  i_param_bindings[ 76] = sym_113;
  i_param_bindings[ 77] = sym_114;
  i_param_bindings[ 78] = sym_115;
  i_param_bindings[ 79] = sym_116;
  i_param_bindings[ 80] = str_117;
  i_param_bindings[ 81] = str_118;
  i_param_bindings[ 82] = key_119;
  i_param_bindings[ 83] = sym_120;
  i_param_bindings[ 84] = sym_121;
  i_param_bindings[ 85] = sym_122;
  i_param_bindings[ 86] = sym_123;
  i_param_bindings[ 87] = sym_124;
  i_param_bindings[ 88] = sym_125;
  i_param_bindings[ 89] = sym_126;
  i_param_bindings[ 90] = sym_127;
  i_param_bindings[ 91] = sym_128;
  i_param_bindings[ 92] = sym_129;
  i_param_bindings[ 93] = str_130;
  i_param_bindings[ 94] = sym_131;
  i_param_bindings[ 95] = str_132;
  i_param_bindings[ 96] = G00104;
  i_param_bindings[ 97] = G00134;
  i_param_bindings[ 98] = G00136;
  i_param_bindings[ 99] = str_139;
  i_param_bindings[ 100] = str_140;
  i_param_bindings[ 101] = str_141;
  i_param_bindings[ 102] = key_142;
  i_param_bindings[ 103] = sym_143;
  i_param_bindings[ 104] = sym_144;
  i_param_bindings[ 105] = str_145;
  i_param_bindings[ 106] = G00138;
  i_param_bindings[ 107] = str_148;
  i_param_bindings[ 108] = G00147;
  i_param_bindings[ 109] = sym_151;
  i_param_bindings[ 110] = G00150;
  i_param_bindings[ 111] = G00153;
  i_param_bindings[ 112] = str_156;
  i_param_bindings[ 113] = str_157;
  i_param_bindings[ 114] = G00155;
  i_param_bindings[ 115] = sym_160;
  i_param_bindings[ 116] = G00159;
  i_param_bindings[ 1] = eul_nil;
  i_param_bindings[ 117] = sym_163;
  i_param_bindings[ 118] = sym_164;
  i_param_bindings[ 119] = sym_165;
  i_param_bindings[ 120] = sym_166;
  i_param_bindings[ 121] = sym_167;
  i_param_bindings[ 122] = sym_168;
  i_param_bindings[ 123] = sym_169;
  eul_allocate_lambda( i_param_bindings[0], "initialize-i-param", 0, G00162);

  }
}


/* eof */
