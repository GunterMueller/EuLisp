/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module read
 **  Copyright: See file read.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_convert1();
extern void initialize_module_vector();
extern void initialize_module_string();
extern void initialize_module_lock();
extern void initialize_module_dynamic();
extern void initialize_module_table();
extern void initialize_module_stream();
extern LispRef stream_bindings[];
extern LispRef vector_bindings[];
extern LispRef convert1_bindings[];
extern LispRef telos_bindings[];
extern LispRef mop_inspect_bindings[];
extern LispRef stream1_bindings[];
extern LispRef table_bindings[];
extern LispRef convert_bindings[];
extern LispRef string_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef lock_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef table1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot1_bindings[];
extern LispRef condition_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef stream2_bindings[];

/* Module bindings with size 109 */
LispRef read_bindings[109];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module read */
void initialize_module_read()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_convert1();
  initialize_module_vector();
  initialize_module_string();
  initialize_module_lock();
  initialize_module_dynamic();
  initialize_module_table();
  initialize_module_stream();
  eul_fast_table_set(eul_modules,"read",(LispRef) read_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_2776, sym_2775, sym_2774, sym_2773, sym_2772, sym_2771, sym_2770, sym_2769, sym_2768, sym_2767, sym_2766, G002765, G002763, G002761, sym_2757, sym_2755, sym_2754, G002753, sym_2747, G002746, G002743, G002741, G002739, G002737, G002735, G002733, G002731, G002729, G002727, G002725, G002723, key_2721, sym_2720, G002719, G002717, key_2715, G002714, sym_2712, sym_2711, sym_2710, key_2708, sym_2696, key_2695, key_2694, sym_2693, key_2692, key_2691, key_2690, sym_2689, key_2688, sym_2687, key_2686, key_2685, G002684, G002682, G002680, G002678, G002676;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 1 is_init: 0 index: 32 binding: (method-specialp) */
  static const void *G002675[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 33 binding: (method-specialp) */
  static const void *G002677[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 34 binding: (setter-tag) */
  static const void *G002679[] = {I(ab,1c,82,1d),I(24,00,00,00),B(read ,31),I(09,45,02,00)};

  /* Byte-vector with size: 9 is_init: 0 index: 35 binding: anonymous */
  static const void *G002681[] = {I(43,03,1d,26),I(00,00,00,04),I(24,00,00,00),B(stream2 ,21),I(08,1b,1f,03),I(1f,03,24,00),B(read ,28),I(3d,03,04,45),I(04,00,00,00)};

  eul_allocate_static_cons(cons_2697, NULL, NULL);
  eul_allocate_static_string(str_2698, "(", 1);
  eul_allocate_static_string(str_2699, ")", 1);
  eul_allocate_static_string(str_2700, "#(", 2);
  eul_allocate_static_string(str_2701, "'", 1);
  eul_allocate_static_string(str_2702, "`", 1);
  eul_allocate_static_string(str_2703, ",", 1);
  eul_allocate_static_string(str_2704, ",@", 2);
  eul_allocate_static_string(str_2705, ".", 1);
  eul_allocate_static_string(str_2706, "#;", 2);
  eul_allocate_static_string(str_2707, "<end of stream>", 15);
  eul_allocate_static_string(str_2709, "", 0);
  /* Byte-vector with size: 313 is_init: 0 index: 64 binding: top-level */
  static const void *G002683[] = {I(a9,24,00,00),B(stream2 ,21),I(23,00,00,00),B(read ,36),I(24,00,00,00),B(stream2 ,50),I(23,00,00,00),B(read ,37),I(23,00,00,00),B(read ,38),I(23,00,00,00),B(read ,35),I(3b,03,24,00),B(mop_gf ,2),I(3c,05,1b,89),B(read ,12),I(2a,24,00,00),B(condition ,9),I(24,00,00,00),B(boot1 ,26),I(3c,01,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,71),I(23,00,00,00),B(read ,39),I(23,00,00,00),B(read ,40),I(23,00,00,00),B(read ,41),I(1f,05,23,00),B(read ,42),I(1f,06,23,00),B(read ,43),I(86,24,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(read ,9),I(2a,24,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(read ,39),I(23,00,00,00),B(read ,44),I(23,00,00,00),B(read ,45),I(23,00,00,00),B(read ,46),I(24,00,00,00),B(boot1 ,26),I(3c,04,1b,24),B(boot1 ,26),I(3c,01,24,00),B(mop_class ,71),I(23,00,00,00),B(read ,39),I(23,00,00,00),B(read ,47),I(23,00,00,00),B(read ,41),I(1f,06,23,00),B(read ,42),I(1f,06,23,00),B(read ,43),I(23,00,00,00),B(read ,48),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(read ,31),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,49),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,14),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,50),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,7),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,51),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,18),I(2a,24,00,00),B(read ,7),I(89,00,00,00),B(read ,23),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,52),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,20),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,53),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,21),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,54),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,8),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,55),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,26),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,56),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,19),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,57),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,25),I(2a,24,00,00),B(read ,31),I(23,00,00,00),B(read ,46),I(23,00,00,00),B(read ,58),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,6),I(2a,26,00,00),I(00,00,00,0b),I(24,00,00,00),B(read ,14),I(24,00,00,00),B(read ,7),I(24,00,00,00),B(read ,18),I(24,00,00,00),B(read ,23),I(24,00,00,00),B(read ,20),I(24,00,00,00),B(read ,21),I(24,00,00,00),B(read ,8),I(24,00,00,00),B(read ,26),I(24,00,00,00),B(read ,19),I(24,00,00,00),B(read ,25),I(24,00,00,00),B(read ,6),I(24,00,00,00),B(boot1 ,39),I(3c,0c,1b,89),B(read ,30),I(2a,24,00,00),B(table1 ,4),I(24,00,00,00),B(mop_gf ,2),I(3c,01,1b,89),B(read ,17),I(2a,24,00,00),B(stream2 ,46),I(23,00,00,00),B(read ,59),I(23,00,00,00),B(read ,60),I(24,00,00,00),B(mop_gf ,2),I(3c,03,1b,89),B(read ,2),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(read ,61),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(read ,13),I(2a,24,00,00),B(read ,9),I(2a,24,00,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(read ,22),I(23,00,00,00),B(read ,62),I(23,00,00,00),B(read ,34),I(3b,02,1d,3c),I(02,2a,24,00),B(read ,13),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(read ,13),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(read ,63),I(23,00,00,00),B(read ,33),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(read ,13),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(read ,13),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(read ,31),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(read ,13),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(read ,63),I(23,00,00,00),B(read ,32),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(read ,13),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(read ,13),I(2a,24,00,00),B(read ,31),I(45,26,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 66 binding: read-error */
  static const void *G002713[] = {I(43,fd,24,00),B(mop_gf ,17),I(86,1f,03,1f),I(03,24,00,00),B(boot ,9),I(3c,04,1b,24),B(read ,9),I(23,00,00,00),B(read ,65),I(1f,06,24,00),B(boot ,21),I(3d,04,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 67 binding: anonymous */
  static const void *G002716[] = {I(a9,47,00,00),I(24,00,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 67 is_init: 0 index: 70 binding: sread-s-expression */
  static const void *G002718[] = {I(43,fe,46,02),I(86,86,1d,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,03,1d),I(32,00,00,00),I(00,00,00,18),I(1f,03,10,1b),I(20,04,1f,04),I(11,1b,20,06),I(1f,05,22,02),I(2a,1f,03,12),I(1b,34,00,00),I(00,00,00,13),I(86,20,03,1d),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,04,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,24,00,00),B(read ,2),I(83,24,00,00),B(stream2 ,21),I(08,1b,48,00),I(00,23,00,00),B(read ,68),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(read ,38),I(23,00,00,00),B(read ,67),I(3b,00,1c,0f),I(23,00,00,00),B(read ,68),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(lock ,3),I(3c,01,2a,24),B(read ,2),I(26,00,00,00),I(00,00,00,04),I(24,00,00,00),B(stream2 ,21),I(08,23,00,00),B(read ,69),I(1f,0a,24,00),B(boot1 ,26),I(3c,02,24,00),B(mop_gf ,12),I(3c,02,2a,24),B(read ,2),I(1f,07,1f,07),I(24,00,00,00),B(read ,28),I(3c,03,47,00),I(00,24,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1b),I(45,0a,00,00)};

  /* Byte-vector with size: 40 is_init: 0 index: 71 binding: read-char */
  static const void *G002722[] = {I(a8,86,86,86),I(1f,03,12,1b),I(34,00,00,00),I(00,00,00,1c),I(24,00,00,00),B(stream2 ,50),I(20,04,1f,03),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,05,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,1f,04,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,04,1f),I(03,32,00,00),I(00,00,00,17),I(1f,05,10,1b),I(20,05,1f,06),I(11,1b,20,08),I(1f,07,22,02),I(2a,1f,05,12),I(1b,34,00,00),I(00,00,00,1f),I(24,00,00,00),B(stream2 ,6),I(3c,00,1b,20),I(05,1f,04,22),I(01,32,00,00),I(00,00,00,17),I(1f,06,10,1b),I(20,05,1f,07),I(11,1b,20,09),I(1f,08,22,02),I(2a,1f,05,1f),I(05,1f,05,24),B(stream2 ,44),I(3d,03,07,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 72 binding: anonymous */
  static const void *G002724[] = {I(a9,47,00,00),I(24,00,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 67 is_init: 0 index: 73 binding: sread */
  static const void *G002726[] = {I(43,fe,46,02),I(86,86,1d,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,03,1d),I(32,00,00,00),I(00,00,00,18),I(1f,03,10,1b),I(20,04,1f,04),I(11,1b,20,06),I(1f,05,22,02),I(2a,1f,03,12),I(1b,34,00,00),I(00,00,00,13),I(86,20,03,1d),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,04,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,24,00,00),B(read ,2),I(83,24,00,00),B(stream2 ,21),I(08,1b,48,00),I(00,23,00,00),B(read ,68),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(read ,38),I(23,00,00,00),B(read ,72),I(3b,00,1c,0f),I(23,00,00,00),B(read ,68),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(lock ,3),I(3c,01,2a,24),B(read ,2),I(26,00,00,00),I(00,00,00,04),I(24,00,00,00),B(stream2 ,21),I(08,23,00,00),B(read ,69),I(1f,0a,24,00),B(boot1 ,26),I(3c,02,24,00),B(mop_gf ,12),I(3c,02,2a,24),B(read ,2),I(1f,07,1f,07),I(24,00,00,00),B(read ,24),I(3c,03,47,00),I(00,24,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1b),I(45,0a,00,00)};

  /* Byte-vector with size: 40 is_init: 0 index: 74 binding: read-s-expression */
  static const void *G002728[] = {I(a8,86,86,86),I(1f,03,12,1b),I(34,00,00,00),I(00,00,00,1c),I(24,00,00,00),B(stream2 ,50),I(20,04,1f,03),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,05,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,1f,04,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,04,1f),I(03,32,00,00),I(00,00,00,17),I(1f,05,10,1b),I(20,05,1f,06),I(11,1b,20,08),I(1f,07,22,02),I(2a,1f,05,12),I(1b,34,00,00),I(00,00,00,1f),I(24,00,00,00),B(stream2 ,6),I(3c,00,1b,20),I(05,1f,04,22),I(01,32,00,00),I(00,00,00,17),I(1f,06,10,1b),I(20,05,1f,07),I(11,1b,20,09),I(1f,08,22,02),I(2a,1f,05,1f),I(05,1f,05,24),B(read ,28),I(3d,03,07,00)};

  /* Byte-vector with size: 38 is_init: 0 index: 75 binding: anonymous */
  static const void *G002730[] = {I(ab,1c,27,0a),I(50,1b,34,00),I(00,00,00,2e),I(1d,1d,0f,1b),I(24,00,00,00),B(boot ,23),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3d,02,04,22),I(01,32,00,00),I(00,00,00,66),I(1d,47,00,02),I(50,1b,34,00),I(00,00,00,3a),I(1d,12,1b,34),I(00,00,00,0d),I(47,00,02,32),I(00,00,00,20),I(1f,03,24,00),B(boot ,23),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3d,02,05,22),I(01,32,00,00),I(00,00,00,24),I(47,00,00,47),I(00,01,47,00),I(02,24,00,00),B(stream2 ,44),I(3c,03,1f,04),I(1f,04,0f,47),I(00,03,3d,02),I(04,22,01,45),I(03,00,00,00)};

  /* Byte-vector with size: 52 is_init: 0 index: 76 binding: read-line */
  static const void *G002732[] = {I(a8,46,05,86),I(86,86,1d,48),I(00,00,1c,48),I(00,01,1b,48),I(00,02,1f,03),I(12,1b,34,00),I(00,00,00,1a),I(24,00,00,00),B(stream2 ,50),I(48,00,00,47),I(00,00,32,00),I(00,00,00,17),I(1f,04,10,1b),I(48,00,00,1f),I(05,11,1b,20),I(07,1f,06,22),I(02,2a,1f,04),I(12,1b,34,00),I(00,00,00,12),I(87,48,00,01),I(47,00,01,32),I(00,00,00,16),I(1f,05,10,1b),I(48,00,01,1f),I(06,11,1b,20),I(08,1f,07,22),I(02,2a,1f,05),I(12,1b,34,00),I(00,00,00,1e),I(24,00,00,00),B(stream2 ,6),I(3c,00,1b,48),I(00,02,47,00),I(02,22,01,32),I(00,00,00,16),I(1f,06,10,1b),I(48,00,02,1f),I(07,11,1b,20),I(09,1f,08,22),I(02,2a,86,1b),I(48,00,03,23),B(read ,38),I(23,00,00,00),B(read ,75),I(3b,02,48,00),I(03,47,00,00),I(47,00,01,47),I(00,02,24,00),B(stream2 ,44),I(3c,03,86,47),I(00,03,3d,02),I(08,00,00,00)};

  /* Byte-vector with size: 22 is_init: 0 index: 77 binding: set-dispatch-macro-character */
  static const void *G002734[] = {I(43,03,1d,1d),I(24,00,00,00),B(boot1 ,26),I(3c,02,1b,24),B(string ,13),I(24,00,00,00),B(convert ,2),I(3c,02,24,00),B(mop_class ,6),I(23,00,00,00),B(read ,39),I(1d,24,00,00),B(mop_gf ,2),I(3c,03,24,00),B(table ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(read ,17),I(1d,1f,06,1f),I(03,3d,03,07),I(45,07,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 78 binding: tag */
  static const void *G002736[] = {I(aa,82,24,00),B(read ,31),I(08,45,00,00)};

  /* Byte-vector with size: 40 is_init: 0 index: 79 binding: read */
  static const void *G002738[] = {I(a8,86,86,86),I(1f,03,12,1b),I(34,00,00,00),I(00,00,00,1c),I(24,00,00,00),B(read ,12),I(20,04,1f,03),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,05,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,1f,04,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,04,1f),I(03,32,00,00),I(00,00,00,17),I(1f,05,10,1b),I(20,05,1f,06),I(11,1b,20,08),I(1f,07,22,02),I(2a,1f,05,12),I(1b,34,00,00),I(00,00,00,1f),I(24,00,00,00),B(stream2 ,6),I(3c,00,1b,20),I(05,1f,04,22),I(01,32,00,00),I(00,00,00,17),I(1f,06,10,1b),I(20,05,1f,07),I(11,1b,20,09),I(1f,08,22,02),I(2a,1f,05,1f),I(05,1f,05,24),B(stream2 ,44),I(3d,03,07,00)};

  /* Byte-vector with size: 53 is_init: 0 index: 80 binding: read-token */
  static const void *G002740[] = {I(a8,86,86,86),I(1f,03,12,1b),I(34,00,00,00),I(00,00,00,1c),I(24,00,00,00),B(stream2 ,50),I(20,04,1f,03),I(32,00,00,00),I(00,00,00,18),I(1f,04,10,1b),I(20,05,1f,05),I(11,1b,20,07),I(1f,06,22,02),I(2a,1f,04,12),I(1b,34,00,00),I(00,00,00,13),I(87,20,04,1f),I(03,32,00,00),I(00,00,00,17),I(1f,05,10,1b),I(20,05,1f,06),I(11,1b,20,08),I(1f,07,22,02),I(2a,1f,05,12),I(1b,34,00,00),I(00,00,00,1f),I(24,00,00,00),B(stream2 ,6),I(3c,00,1b,20),I(05,1f,04,22),I(01,32,00,00),I(00,00,00,17),I(1f,06,10,1b),I(20,05,1f,07),I(11,1b,20,09),I(1f,08,22,02),I(2a,1f,05,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,24),B(read ,6),I(50,1b,34,00),I(00,00,00,26),I(1f,06,34,00),I(00,00,00,16),I(1f,07,24,00),B(stream2 ,19),I(3d,01,09,32),I(00,00,00,07),I(1f,05,32,00),I(00,00,00,07),I(1c,45,09,00)};

  eul_allocate_static_string(str_2744, "unexpected end of file during vector ~a", 39);
  /* Byte-vector with size: 48 is_init: 0 index: 82 binding: anonymous */
  static const void *G002742[] = {I(aa,47,01,00),I(24,00,00,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,48),I(00,00,47,00),I(00,24,00,00),B(read ,23),I(50,1b,34,00),I(00,00,00,2a),I(1d,24,00,00),B(boot ,8),I(3c,01,1f,03),I(24,00,00,00),B(boot ,23),I(3c,01,41,00),B(boot1 ,54),I(22,02,32,00),I(00,00,00,73),I(47,00,00,24),B(read ,6),I(50,1b,34,00),I(00,00,00,4a),I(1f,03,24,00),B(boot ,23),I(3c,01,47,01),I(00,23,00,00),B(read ,81),I(1d,24,00,00),B(read ,3),I(3c,03,2a,1f),I(04,24,00,00),B(boot ,8),I(3c,01,1f,05),I(24,00,00,00),B(boot ,23),I(3c,01,41,00),B(boot1 ,54),I(22,03,32,00),I(00,00,00,1d),I(47,00,00,47),I(00,01,47,01),I(03,3c,02,1b),I(1f,05,0f,47),I(00,03,3d,01),I(05,22,01,22),I(01,45,03,00)};

  eul_allocate_static_string(str_2748, "misplaced dot/unquote after ~s", 30);
  eul_allocate_static_string(str_2749, "misplaced dot after ~s", 22);
  eul_allocate_static_string(str_2750, " ... ", 5);
  eul_allocate_static_string(str_2751, "unexpected end of file during list ~a", 37);
  /* Byte-vector with size: 138 is_init: 0 index: 88 binding: anonymous */
  static const void *G002745[] = {I(aa,47,01,00),I(24,00,00,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,48),I(00,00,47,00),I(00,24,00,00),B(read ,7),I(50,1b,34,00),I(00,00,00,16),I(1d,24,00,00),B(boot ,23),I(3d,01,03,32),I(00,00,01,ec),I(47,00,00,24),B(read ,19),I(50,1b,34,00),I(00,00,01,2a),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,24),B(read ,8),I(50,1b,34,00),I(00,00,00,aa),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,47),I(00,01,47,01),I(03,3c,02,47),I(00,01,34,00),I(00,00,00,26),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,24,00),B(read ,7),I(50,32,00,00),I(00,00,00,08),I(86,1b,34,00),I(00,00,00,36),I(1c,86,47,01),I(03,3c,02,1b),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(read ,83),I(1c,0f,1f,0b),I(1c,24,00,00),B(read ,29),I(3d,02,0c,22),I(03,32,00,00),I(00,00,00,28),I(1f,08,24,00),B(boot ,23),I(3c,01,47,01),I(00,23,00,00),B(read ,84),I(1d,24,00,00),B(read ,3),I(3d,03,0a,22),I(01,22,03,32),I(00,00,00,5e),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,24),B(read ,7),I(50,12,1b,34),I(00,00,00,2d),I(1f,07,24,00),B(boot ,23),I(3c,01,47,01),I(00,23,00,00),B(read ,85),I(1d,24,00,00),B(read ,3),I(3d,03,09,22),I(01,32,00,00),I(00,00,00,16),I(1f,07,1f,04),I(24,00,00,00),B(read ,29),I(3d,02,08,22),I(02,22,02,32),I(00,00,00,b6),I(47,00,00,24),B(read ,6),I(50,1b,34,00),I(00,00,00,8e),I(1f,04,24,00),B(boot ,23),I(3c,01,1b,24),B(boot ,8),I(3c,01,1b,26),I(00,00,00,80),I(1a,1b,34,00),I(00,00,00,0e),I(1d,32,00,00),I(00,00,00,3f),I(1d,10,1f,03),I(11,1b,10,1f),I(05,11,1b,10),I(1b,10,1f,0d),I(11,1b,10,1b),I(10,1f,10,11),I(1b,10,1f,12),I(10,1f,0b,1f),I(0a,1f,08,23),B(read ,86),I(1f,07,1f,06),I(1f,06,24,00),B(boot1 ,26),I(3c,07,22,0c),I(47,01,00,23),B(read ,87),I(1d,24,00,00),B(read ,3),I(3c,03,2a,1f),I(08,24,00,00),B(boot ,23),I(3d,01,09,22),I(04,32,00,00),I(00,00,00,1e),I(47,00,00,47),I(00,01,47,01),I(03,3c,02,1b),I(1f,06,0f,47),I(00,02,3d,01),I(06,22,01,22),I(01,22,01,45),I(03,00,00,00)};

  eul_allocate_static_string(str_2756, "misplaced unquote", 17);
  eul_allocate_static_string(str_2758, "misplaced unquote-splicing", 26);
  eul_allocate_static_string(str_2759, "unexpected token ~a", 19);
  /* Byte-vector with size: 197 is_init: 0 index: 95 binding: anonymous */
  static const void *G002752[] = {I(ab,46,04,1c),I(48,00,00,1b),I(48,00,01,47),I(00,00,24,00),B(read ,13),I(3c,01,1b,34),I(00,00,02,c1),I(47,00,00,24),B(read ,14),I(50,1b,34,00),I(00,00,00,2e),I(86,1b,48,00),I(02,23,00,00),B(read ,38),I(23,00,00,00),B(read ,88),I(3b,01,48,00),I(02,86,47,00),I(02,3d,01,05),I(22,01,32,00),I(00,00,02,82),I(47,00,00,24),B(read ,18),I(50,1b,34,00),I(00,00,00,2e),I(86,1b,48,00),I(03,23,00,00),B(read ,38),I(23,00,00,00),B(read ,82),I(3b,01,48,00),I(03,86,47,00),I(03,3d,01,06),I(22,01,32,00),I(00,00,02,48),I(47,00,00,24),B(read ,20),I(50,1b,34,00),I(00,00,00,3e),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,47),I(00,01,47,01),I(03,3c,02,1b),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(read ,89),I(1c,0f,22,03),I(32,00,00,00),I(00,00,02,00),I(47,00,00,24),B(read ,21),I(50,1b,34,00),I(00,00,00,3a),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,87),I(47,01,03,3c),I(02,1b,24,00),B(boot1 ,26),I(3c,01,23,00),B(read ,90),I(1c,0f,22,03),I(32,00,00,00),I(00,00,01,ba),I(47,00,00,24),B(read ,8),I(50,1b,34,00),I(00,00,00,7a),I(47,00,01,34),I(00,00,00,39),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,86),I(47,01,03,3c),I(02,1b,24,00),B(boot1 ,26),I(3c,01,23,00),B(read ,83),I(1c,0f,22,03),I(32,00,00,00),I(00,00,00,3a),I(47,01,00,23),B(read ,91),I(24,00,00,00),B(read ,3),I(3c,02,2a,47),I(01,00,24,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,47),I(00,01,47,01),I(03,3d,02,09),I(22,01,32,00),I(00,00,01,32),I(47,00,00,24),B(read ,26),I(50,1b,34,00),I(00,00,00,7a),I(47,00,01,34),I(00,00,00,39),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,86),I(47,01,03,3c),I(02,1b,24,00),B(boot1 ,26),I(3c,01,23,00),B(read ,92),I(1c,0f,22,03),I(32,00,00,00),I(00,00,00,3a),I(47,01,00,23),B(read ,93),I(24,00,00,00),B(read ,3),I(3c,02,2a,47),I(01,00,24,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,1b,47),I(00,01,47,01),I(03,3d,02,0a),I(22,01,32,00),I(00,00,00,ac),I(47,00,00,24),B(read ,25),I(50,1b,34,00),I(00,00,00,42),I(47,01,00,24),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,86,47),I(01,03,3c,02),I(2a,47,01,00),I(24,00,00,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,86,47),I(01,03,3d,02),I(0a,32,00,00),I(00,00,00,5f),I(47,00,00,24),B(read ,6),I(50,1b,34,00),I(00,00,00,26),I(47,01,01,34),I(00,00,00,15),I(47,01,00,24),B(stream2 ,19),I(3d,01,0b,32),I(00,00,00,08),I(47,01,02,32),I(00,00,00,2b),I(47,00,00,82),I(24,00,00,00),B(read ,31),I(08,47,01,00),I(23,00,00,00),B(read ,94),I(1d,24,00,00),B(read ,3),I(3c,03,2a,86),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,32,00),I(00,00,00,3f),I(24,00,00,00),B(read ,17),I(47,00,00,24),B(table ,3),I(3c,02,1b,24),B(mop_inspect ,6),I(3c,01,1b,34),I(00,00,00,19),I(47,01,00,47),I(00,00,86,1f),I(04,3d,03,05),I(32,00,00,00),I(00,00,00,0b),I(47,00,00,22),I(02,45,03,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 96 binding: parse */
  static const void *G002760[] = {I(43,03,46,04),I(1d,48,00,00),I(1c,48,00,01),I(1b,48,00,02),I(86,1b,48,00),I(03,23,00,00),B(read ,38),I(23,00,00,00),B(read ,95),I(3b,02,48,00),I(03,47,00,00),I(24,00,00,00),B(read ,30),I(41,00,00,00),B(stream1 ,26),I(22,02,86,47),I(00,03,3d,02),I(04,45,04,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 97 binding: reverse-onto */
  static const void *G002762[] = {I(ab,1c,12,1b),I(34,00,00,00),I(00,00,00,10),I(1c,32,00,00),I(00,00,00,1c),I(1d,11,1f,03),I(10,1b,1f,04),I(0f,1d,1c,24),B(read ,29),I(3d,02,06,22),I(03,45,03,00)};

  /* Byte-vector with size: 166 is_init: 1 index: 0 binding: initialize-read */
  static const void *G002764[] = {I(87,25,00,00),B(read ,1),I(24,00,00,00),B(stream ,1),I(3e,0b,24,00),B(stream ,0),I(3c,00,21,01),I(24,00,00,00),B(table ,1),I(3e,0b,24,00),B(table ,0),I(3c,00,21,01),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(lock ,1),I(3e,0b,24,00),B(lock ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(convert1 ,1),I(3e,0b,24,00),B(convert1 ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(read ,31),I(86,25,00,00),B(read ,30),I(23,00,00,00),B(read ,98),I(23,00,00,00),B(read ,97),I(3b,02,25,00),B(read ,29),I(23,00,00,00),B(read ,99),I(23,00,00,00),B(read ,96),I(3b,03,25,00),B(read ,28),I(23,00,00,00),B(read ,100),I(23,00,00,00),B(read ,80),I(3b,ff,25,00),B(read ,27),I(86,25,00,00),B(read ,26),I(86,25,00,00),B(read ,25),I(23,00,00,00),B(read ,101),I(23,00,00,00),B(read ,79),I(3b,ff,25,00),B(read ,24),I(86,25,00,00),B(read ,23),I(23,00,00,00),B(read ,44),I(23,00,00,00),B(read ,78),I(3b,01,25,00),B(read ,22),I(86,25,00,00),B(read ,21),I(86,25,00,00),B(read ,20),I(86,25,00,00),B(read ,19),I(86,25,00,00),B(read ,18),I(86,25,00,00),B(read ,17),I(23,00,00,00),B(read ,102),I(23,00,00,00),B(read ,77),I(3b,03,25,00),B(read ,16),I(23,00,00,00),B(read ,103),I(23,00,00,00),B(read ,76),I(3b,ff,25,00),B(read ,15),I(86,25,00,00),B(read ,14),I(86,25,00,00),B(read ,13),I(86,25,00,00),B(read ,12),I(23,00,00,00),B(read ,104),I(23,00,00,00),B(read ,74),I(3b,ff,25,00),B(read ,11),I(23,00,00,00),B(read ,105),I(23,00,00,00),B(read ,73),I(3b,fe,25,00),B(read ,10),I(86,25,00,00),B(read ,9),I(86,25,00,00),B(read ,8),I(86,25,00,00),B(read ,7),I(86,25,00,00),B(read ,6),I(23,00,00,00),B(read ,106),I(23,00,00,00),B(read ,71),I(3b,ff,25,00),B(read ,5),I(23,00,00,00),B(read ,107),I(23,00,00,00),B(read ,70),I(3b,fe,25,00),B(read ,4),I(23,00,00,00),B(read ,40),I(23,00,00,00),B(read ,66),I(3b,fd,25,00),B(read ,3),I(86,25,00,00),B(read ,2),I(23,00,00,00),B(read ,108),I(23,00,00,00),B(read ,64),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G002676,G002675);
  eul_allocate_bytevector( G002678,G002677);
  eul_allocate_bytevector( G002680,G002679);
  eul_allocate_bytevector( G002682,G002681);
  eul_intern_keyword(key_2685,"source");
  eul_intern_keyword(key_2686,"read-action");
  eul_intern_symbol(sym_2687,"anonymous");
  eul_intern_keyword(key_2688,"name");
  eul_intern_symbol(sym_2689,"read-error");
  eul_intern_keyword(key_2690,"direct-superclasses");
  eul_intern_keyword(key_2691,"direct-slots");
  eul_intern_keyword(key_2692,"direct-keywords");
  eul_intern_symbol(sym_2693,"tag");
  eul_intern_keyword(key_2694,"keyword");
  eul_intern_keyword(key_2695,"tag");
  eul_intern_symbol(sym_2696,"special");
  object_class(cons_2697) = eul_static_cons_class;
  eul_car(cons_2697) = key_2695;
  eul_cdr(cons_2697) = eul_nil;
  object_class(str_2698) = eul_static_string_class;
  object_class(str_2699) = eul_static_string_class;
  object_class(str_2700) = eul_static_string_class;
  object_class(str_2701) = eul_static_string_class;
  object_class(str_2702) = eul_static_string_class;
  object_class(str_2703) = eul_static_string_class;
  object_class(str_2704) = eul_static_string_class;
  object_class(str_2705) = eul_static_string_class;
  object_class(str_2706) = eul_static_string_class;
  object_class(str_2707) = eul_static_string_class;
  eul_intern_keyword(key_2708,"string");
  object_class(str_2709) = eul_static_string_class;
  eul_intern_symbol(sym_2710,"specialp");
  eul_intern_symbol(sym_2711,"(setter tag)");
  eul_intern_symbol(sym_2712,"(method specialp)");
  eul_allocate_bytevector( G002684,G002683);
  eul_intern_keyword(key_2715,"value");
  eul_allocate_bytevector( G002714,G002713);
  eul_allocate_bytevector( G002717,G002716);
  eul_intern_symbol(sym_2720,"*clean-ups*");
  eul_intern_keyword(key_2721,"buffer");
  eul_allocate_bytevector( G002719,G002718);
  eul_allocate_bytevector( G002723,G002722);
  eul_allocate_bytevector( G002725,G002724);
  eul_allocate_bytevector( G002727,G002726);
  eul_allocate_bytevector( G002729,G002728);
  eul_allocate_bytevector( G002731,G002730);
  eul_allocate_bytevector( G002733,G002732);
  eul_allocate_bytevector( G002735,G002734);
  eul_allocate_bytevector( G002737,G002736);
  eul_allocate_bytevector( G002739,G002738);
  eul_allocate_bytevector( G002741,G002740);
  object_class(str_2744) = eul_static_string_class;
  eul_allocate_bytevector( G002743,G002742);
  eul_intern_symbol(sym_2747,"unquote");
  object_class(str_2748) = eul_static_string_class;
  object_class(str_2749) = eul_static_string_class;
  object_class(str_2750) = eul_static_string_class;
  object_class(str_2751) = eul_static_string_class;
  eul_allocate_bytevector( G002746,G002745);
  eul_intern_symbol(sym_2754,"quote");
  eul_intern_symbol(sym_2755,"quasiquote");
  object_class(str_2756) = eul_static_string_class;
  eul_intern_symbol(sym_2757,"unquote-splicing");
  object_class(str_2758) = eul_static_string_class;
  object_class(str_2759) = eul_static_string_class;
  eul_allocate_bytevector( G002753,G002752);
  eul_allocate_bytevector( G002761,G002760);
  eul_allocate_bytevector( G002763,G002762);
  eul_intern_symbol(sym_2766,"reverse-onto");
  eul_intern_symbol(sym_2767,"parse");
  eul_intern_symbol(sym_2768,"read-token");
  eul_intern_symbol(sym_2769,"read");
  eul_intern_symbol(sym_2770,"set-dispatch-macro-character");
  eul_intern_symbol(sym_2771,"read-line");
  eul_intern_symbol(sym_2772,"read-s-expression");
  eul_intern_symbol(sym_2773,"sread");
  eul_intern_symbol(sym_2774,"read-char");
  eul_intern_symbol(sym_2775,"sread-s-expression");
  eul_intern_symbol(sym_2776,"top-level");
  eul_allocate_bytevector( G002765,G002764);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 32; i++)
      read_bindings[i] = eul_nil;
  }

  read_bindings[ 32] = G002676;
  read_bindings[ 33] = G002678;
  read_bindings[ 34] = G002680;
  read_bindings[ 35] = G002682;
  read_bindings[ 36] = key_2685;
  read_bindings[ 37] = key_2686;
  read_bindings[ 38] = sym_2687;
  read_bindings[ 39] = key_2688;
  read_bindings[ 40] = sym_2689;
  read_bindings[ 41] = key_2690;
  read_bindings[ 42] = key_2691;
  read_bindings[ 43] = key_2692;
  read_bindings[ 44] = sym_2693;
  read_bindings[ 45] = key_2694;
  read_bindings[ 46] = key_2695;
  read_bindings[ 47] = sym_2696;
  read_bindings[ 48] = cons_2697;
  read_bindings[ 49] = str_2698;
  read_bindings[ 50] = str_2699;
  read_bindings[ 51] = str_2700;
  read_bindings[ 52] = str_2701;
  read_bindings[ 53] = str_2702;
  read_bindings[ 54] = str_2703;
  read_bindings[ 55] = str_2704;
  read_bindings[ 56] = str_2705;
  read_bindings[ 57] = str_2706;
  read_bindings[ 58] = str_2707;
  read_bindings[ 59] = key_2708;
  read_bindings[ 60] = str_2709;
  read_bindings[ 61] = sym_2710;
  read_bindings[ 62] = sym_2711;
  read_bindings[ 63] = sym_2712;
  read_bindings[ 64] = G002684;
  read_bindings[ 65] = key_2715;
  read_bindings[ 66] = G002714;
  read_bindings[ 67] = G002717;
  read_bindings[ 68] = sym_2720;
  read_bindings[ 69] = key_2721;
  read_bindings[ 70] = G002719;
  read_bindings[ 71] = G002723;
  read_bindings[ 72] = G002725;
  read_bindings[ 73] = G002727;
  read_bindings[ 74] = G002729;
  read_bindings[ 75] = G002731;
  read_bindings[ 76] = G002733;
  read_bindings[ 77] = G002735;
  read_bindings[ 78] = G002737;
  read_bindings[ 79] = G002739;
  read_bindings[ 80] = G002741;
  read_bindings[ 81] = str_2744;
  read_bindings[ 82] = G002743;
  read_bindings[ 83] = sym_2747;
  read_bindings[ 84] = str_2748;
  read_bindings[ 85] = str_2749;
  read_bindings[ 86] = str_2750;
  read_bindings[ 87] = str_2751;
  read_bindings[ 88] = G002746;
  read_bindings[ 89] = sym_2754;
  read_bindings[ 90] = sym_2755;
  read_bindings[ 91] = str_2756;
  read_bindings[ 92] = sym_2757;
  read_bindings[ 93] = str_2758;
  read_bindings[ 94] = str_2759;
  read_bindings[ 95] = G002753;
  read_bindings[ 96] = G002761;
  read_bindings[ 97] = G002763;
  read_bindings[ 1] = eul_nil;
  read_bindings[ 98] = sym_2766;
  read_bindings[ 99] = sym_2767;
  read_bindings[ 100] = sym_2768;
  read_bindings[ 101] = sym_2769;
  read_bindings[ 102] = sym_2770;
  read_bindings[ 103] = sym_2771;
  read_bindings[ 104] = sym_2772;
  read_bindings[ 105] = sym_2773;
  read_bindings[ 106] = sym_2774;
  read_bindings[ 107] = sym_2775;
  read_bindings[ 108] = sym_2776;
  eul_allocate_lambda( read_bindings[0], "initialize-read", 0, G002765);

  }
}


/* eof */