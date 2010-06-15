/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module socket
 **  Copyright: See file socket.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_lock();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_dynamic();
extern void initialize_module_stream1();
extern void initialize_module_stream2();
extern void initialize_module_string();
extern LispRef condition_bindings[];
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef stream2_bindings[];
extern LispRef convert_bindings[];
extern LispRef string_bindings[];
extern LispRef stream1_bindings[];
extern LispRef boot_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef lock_bindings[];

/* Module bindings with size 85 */
LispRef socket_bindings[85];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module socket */
void initialize_module_socket()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_lock();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_dynamic();
  initialize_module_stream1();
  initialize_module_stream2();
  initialize_module_string();
  eul_fast_table_set(eul_modules,"socket",(LispRef) socket_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1061, sym_1060, sym_1059, sym_1058, sym_1057, sym_1056, sym_1055, G001054, G001052, G001050, G001048, G001046, G001044, G001042, sym_1040, sym_1039, sym_1038, sym_1037, sym_1036, sym_1035, sym_1034, sym_1033, sym_1032, sym_1031, sym_1030, sym_1026, key_1022, key_1021, key_1020, sym_1019, key_1018, sym_1017, sym_1016, key_1015, sym_1014, key_1013, key_1012, key_1011, sym_1010, key_1009, G001008, G001006, G001004, G001002, G001000, G00998, G00996, G00994, G00992, G00990, G00988, G00986, G00983, G00981, G00979, G00977, G00975, G00973, sym_971, sym_970, sym_969, key_968, sym_967, key_966, key_965, key_963, key_961, G00960, G00958;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 7 is_init: 0 index: 12 binding: anonymous */
  static const void *G00957[] = {I(a9,47,00,00),I(24,00,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_962, "tcp", 3);
  eul_allocate_static_string(str_964, "~a:~a", 5);
  /* Byte-vector with size: 153 is_init: 0 index: 24 binding: (method-initialize) */
  static const void *G00959[] = {I(ab,46,04,1c),I(1c,37,02,2a),I(1b,23,00,00),B(socket ,13),I(24,00,00,00),B(boot ,28),I(3c,02,86,86),I(86,1f,03,24),B(socket ,9),I(3c,01,1b,34),I(00,00,00,65),I(1f,04,84,24),B(socket ,7),I(08,1b,20,05),I(1f,05,26,00),I(00,00,00,03),I(24,00,00,00),B(socket ,7),I(08,1b,20,05),I(1f,08,1f,06),I(1c,83,1d,24),B(socket ,2),I(09,22,02,2a),I(1f,08,1f,05),I(1c,82,1d,24),B(socket ,2),I(09,22,02,2a),I(1f,06,83,24),B(socket ,7),I(08,41,00,00),B(stream1 ,22),I(22,01,1b,20),I(05,1f,04,22),I(03,32,00,00),I(00,00,00,4c),I(1f,06,83,24),B(socket ,2),I(08,1b,20,05),I(1f,07,82,24),B(socket ,2),I(08,1b,20,05),I(1f,04,24,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3c,02,1f,06),I(1c,23,00,00),B(socket ,14),I(41,00,00,00),B(stream1 ,23),I(22,03,1b,20),I(06,1f,05,22),I(04,2a,1c,85),I(19,1b,34,00),I(00,00,00,32),I(24,00,00,00),B(stream1 ,6),I(3c,00,1b,24),B(stream2 ,38),I(23,00,00,00),B(socket ,15),I(1f,0b,24,00),B(boot ,22),I(3c,04,22,01),I(32,00,00,00),I(00,00,00,43),I(1d,85,1a,1b),I(34,00,00,00),I(00,00,00,34),I(1f,03,41,00),B(stream1 ,24),I(22,01,1b,24),B(stream2 ,38),I(23,00,00,00),B(socket ,15),I(1f,0c,24,00),B(boot ,22),I(3c,04,22,01),I(32,00,00,00),I(00,00,00,09),I(86,22,01,2a),I(86,23,00,00),B(socket ,16),I(1f,06,1f,06),I(24,00,00,00),B(mop_gf ,17),I(3c,04,24,00),B(stream2 ,4),I(23,00,00,00),B(socket ,17),I(1d,23,00,00),B(socket ,18),I(23,00,00,00),B(socket ,19),I(23,00,00,00),B(socket ,20),I(1f,09,24,00),B(mop_gf ,2),I(3c,07,24,00),B(stream2 ,4),I(23,00,00,00),B(socket ,17),I(1f,03,23,00),B(socket ,18),I(23,00,00,00),B(socket ,21),I(23,00,00,00),B(socket ,20),I(1f,0a,24,00),B(mop_gf ,2),I(3c,07,1f,0a),I(1d,1c,26,00),I(00,00,00,04),I(1d,24,00,00),B(stream2 ,21),I(09,22,02,2a),I(1f,0a,1c,1c),I(26,00,00,00),I(00,00,00,05),I(1d,24,00,00),B(stream2 ,21),I(09,22,02,2a),I(24,00,00,00),B(stream2 ,12),I(1b,48,00,00),I(23,00,00,00),B(socket ,22),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(socket ,23),I(23,00,00,00),B(socket ,12),I(3b,00,1c,0f),I(23,00,00,00),B(socket ,22),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(lock ,3),I(3c,01,2a,1f),I(0d,24,00,00),B(stream2 ,37),I(0f,1b,89,00),B(stream2 ,37),I(47,00,00,24),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(0f,45,10,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 25 binding: (method-connectionp) */
  static const void *G00972[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 26 binding: (method-connectionp) */
  static const void *G00974[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 27 binding: (setter-connection-host) */
  static const void *G00976[] = {I(ab,1c,83,1d),I(24,00,00,00),B(socket ,2),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 28 binding: (setter-connection-port) */
  static const void *G00978[] = {I(ab,1c,82,1d),I(24,00,00,00),B(socket ,2),I(09,45,02,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 29 binding: anonymous */
  static const void *G00980[] = {I(a9,47,00,00),I(24,00,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_984, "tcp", 3);
  /* Byte-vector with size: 81 is_init: 0 index: 31 binding: (method-initialize) */
  static const void *G00982[] = {I(ab,46,02,1c),I(1c,37,02,2a),I(1c,26,00,00),I(00,00,00,03),I(24,00,00,00),B(socket ,7),I(08,24,00,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3c,02,1d,82),I(24,00,00,00),B(socket ,7),I(08,1c,23,00),B(socket ,30),I(1d,41,00,00),B(stream1 ,21),I(22,03,1b,85),I(19,1b,34,00),I(00,00,00,32),I(24,00,00,00),B(stream1 ,6),I(3c,00,1b,24),B(stream2 ,38),I(23,00,00,00),B(socket ,15),I(1f,09,24,00),B(boot ,22),I(3d,04,07,22),I(01,32,00,00),I(00,00,00,cc),I(1c,85,1a,1b),I(34,00,00,00),I(00,00,00,34),I(1d,41,00,00),B(stream1 ,24),I(22,01,1b,24),B(stream2 ,38),I(23,00,00,00),B(socket ,15),I(1f,0a,24,00),B(boot ,22),I(3d,04,08,22),I(01,32,00,00),I(00,00,00,92),I(1f,06,1f,03),I(1c,83,1d,24),B(socket ,7),I(09,22,02,2a),I(24,00,00,00),B(stream2 ,12),I(1b,48,00,00),I(23,00,00,00),B(socket ,22),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(socket ,23),I(23,00,00,00),B(socket ,29),I(3b,00,1c,0f),I(23,00,00,00),B(socket ,22),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(lock ,3),I(3c,01,2a,1f),I(09,24,00,00),B(stream2 ,37),I(0f,1b,89,00),B(stream2 ,37),I(47,00,00,24),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(0b,22,05,22),I(01,45,06,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 32 binding: (method-socket?) */
  static const void *G00985[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 33 binding: (method-socket?) */
  static const void *G00987[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 5 is_init: 0 index: 34 binding: (setter-socket-port) */
  static const void *G00989[] = {I(ab,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(socket ,7),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 35 binding: (setter-socket-host) */
  static const void *G00991[] = {I(ab,1c,84,1d),I(24,00,00,00),B(socket ,7),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 36 binding: (setter-socket-descriptor) */
  static const void *G00993[] = {I(ab,1c,83,1d),I(24,00,00,00),B(socket ,7),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 37 binding: (setter-socket-queue-size) */
  static const void *G00995[] = {I(ab,1c,82,1d),I(24,00,00,00),B(socket ,7),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 38 binding: anonymous */
  static const void *G00997[] = {I(a9,26,00,00),I(00,00,12,67),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 39 binding: anonymous */
  static const void *G00999[] = {I(a9,24,00,00),B(stream1 ,3),I(3d,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 40 binding: anonymous */
  static const void *G001001[] = {I(a9,26,00,00),I(00,00,00,05),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 41 binding: anonymous */
  static const void *G001003[] = {I(a9,24,00,00),B(stream1 ,3),I(3d,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 42 binding: anonymous */
  static const void *G001005[] = {I(a9,26,00,00),I(00,00,12,67),I(45,00,00,00)};

  eul_allocate_static_cons(cons_1025, NULL, NULL);
  eul_allocate_static_cons(cons_1024, NULL, eul_as_static(cons_1025));
  eul_allocate_static_cons(cons_1023, NULL, eul_as_static(cons_1024));
  eul_allocate_static_cons(cons_1029, NULL, NULL);
  eul_allocate_static_cons(cons_1028, NULL, eul_as_static(cons_1029));
  eul_allocate_static_cons(cons_1027, NULL, eul_as_static(cons_1028));
  /* Byte-vector with size: 419 is_init: 0 index: 71 binding: top-level */
  static const void *G001007[] = {I(a9,24,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,24),I(3c,01,23,00),B(socket ,43),I(23,00,00,00),B(socket ,44),I(23,00,00,00),B(socket ,45),I(23,00,00,00),B(socket ,23),I(23,00,00,00),B(socket ,42),I(3b,00,23,00),B(socket ,46),I(23,00,00,00),B(socket ,47),I(24,00,00,00),B(boot1 ,24),I(3c,06,23,00),B(socket ,43),I(23,00,00,00),B(socket ,48),I(23,00,00,00),B(socket ,45),I(23,00,00,00),B(socket ,23),I(23,00,00,00),B(socket ,41),I(3b,00,23,00),B(socket ,46),I(23,00,00,00),B(socket ,49),I(24,00,00,00),B(boot1 ,24),I(3c,06,23,00),B(socket ,43),I(23,00,00,00),B(socket ,50),I(24,00,00,00),B(boot1 ,24),I(3c,02,23,00),B(socket ,43),I(23,00,00,00),B(socket ,51),I(23,00,00,00),B(socket ,45),I(23,00,00,00),B(socket ,23),I(23,00,00,00),B(socket ,40),I(3b,00,23,00),B(socket ,46),I(23,00,00,00),B(socket ,52),I(24,00,00,00),B(boot1 ,24),I(3c,06,1f,03),I(1f,03,1f,03),I(1f,03,24,00),B(boot1 ,24),I(3c,04,24,00),B(mop_class ,70),I(23,00,00,00),B(socket ,43),I(23,00,00,00),B(socket ,53),I(23,00,00,00),B(socket ,54),I(1f,09,23,00),B(socket ,55),I(1f,06,23,00),B(socket ,56),I(23,00,00,00),B(socket ,57),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(socket ,7),I(2a,24,00,00),B(stream2 ,5),I(24,00,00,00),B(boot1 ,24),I(3c,01,23,00),B(socket ,43),I(23,00,00,00),B(socket ,48),I(23,00,00,00),B(socket ,45),I(23,00,00,00),B(socket ,23),I(23,00,00,00),B(socket ,39),I(3b,00,23,00),B(socket ,46),I(23,00,00,00),B(socket ,49),I(24,00,00,00),B(boot1 ,24),I(3c,06,23,00),B(socket ,43),I(23,00,00,00),B(socket ,44),I(23,00,00,00),B(socket ,45),I(23,00,00,00),B(socket ,23),I(23,00,00,00),B(socket ,38),I(3b,00,23,00),B(socket ,46),I(23,00,00,00),B(socket ,47),I(24,00,00,00),B(boot1 ,24),I(3c,06,1c,1c),I(24,00,00,00),B(boot1 ,24),I(3c,02,24,00),B(mop_class ,70),I(23,00,00,00),B(socket ,43),I(23,00,00,00),B(socket ,58),I(23,00,00,00),B(socket ,54),I(1f,07,23,00),B(socket ,55),I(1f,06,23,00),B(socket ,56),I(23,00,00,00),B(socket ,59),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(socket ,2),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(socket ,60),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(socket ,9),I(2a,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(boot1 ,24),I(3c,00,23,00),B(socket ,61),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(socket ,10),I(2a,24,00,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,5),I(23,00,00,00),B(socket ,62),I(23,00,00,00),B(socket ,37),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,3),I(23,00,00,00),B(socket ,63),I(23,00,00,00),B(socket ,36),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,11),I(23,00,00,00),B(socket ,64),I(23,00,00,00),B(socket ,35),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,8),I(23,00,00,00),B(socket ,65),I(23,00,00,00),B(socket ,34),I(3b,02,1d,3c),I(02,2a,24,00),B(socket ,9),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(socket ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,66),I(23,00,00,00),B(socket ,33),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,9),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(socket ,7),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(socket ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,66),I(23,00,00,00),B(socket ,32),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,9),I(2a,24,00,00),B(socket ,7),I(2a,24,00,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(socket ,7),I(86,24,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,24),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,67),I(23,00,00,00),B(socket ,31),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,6),I(23,00,00,00),B(socket ,68),I(23,00,00,00),B(socket ,28),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(socket ,4),I(23,00,00,00),B(socket ,69),I(23,00,00,00),B(socket ,27),I(3b,02,1d,3c),I(02,2a,24,00),B(socket ,10),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(socket ,10),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,70),I(23,00,00,00),B(socket ,26),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,10),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(socket ,2),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,24),I(3c,00,24,00),B(socket ,10),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,70),I(23,00,00,00),B(socket ,25),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,10),I(2a,24,00,00),B(socket ,2),I(2a,24,00,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(socket ,2),I(86,24,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,24),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,67),I(23,00,00,00),B(socket ,24),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,3e,45),I(3e,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 72 binding: socket-descriptor */
  static const void *G001041[] = {I(aa,83,24,00),B(socket ,7),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 73 binding: connection-host */
  static const void *G001043[] = {I(aa,83,24,00),B(socket ,2),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 74 binding: socket-queue-size */
  static const void *G001045[] = {I(aa,82,24,00),B(socket ,7),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 75 binding: connection-port */
  static const void *G001047[] = {I(aa,82,24,00),B(socket ,2),I(08,45,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 76 binding: socket-port */
  static const void *G001049[] = {I(aa,26,00,00),I(00,00,00,03),I(24,00,00,00),B(socket ,7),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 77 binding: socket-host */
  static const void *G001051[] = {I(aa,84,24,00),B(socket ,7),I(08,45,00,00)};

  /* Byte-vector with size: 92 is_init: 1 index: 0 binding: initialize-socket */
  static const void *G001053[] = {I(87,25,00,00),B(socket ,1),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(stream2 ,1),I(3e,0b,24,00),B(stream2 ,0),I(3c,00,21,01),I(24,00,00,00),B(stream1 ,1),I(3e,0b,24,00),B(stream1 ,0),I(3c,00,21,01),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(lock ,1),I(3e,0b,24,00),B(lock ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(socket ,78),I(23,00,00,00),B(socket ,77),I(3b,01,25,00),B(socket ,11),I(86,25,00,00),B(socket ,10),I(86,25,00,00),B(socket ,9),I(23,00,00,00),B(socket ,79),I(23,00,00,00),B(socket ,76),I(3b,01,25,00),B(socket ,8),I(86,25,00,00),B(socket ,7),I(23,00,00,00),B(socket ,80),I(23,00,00,00),B(socket ,75),I(3b,01,25,00),B(socket ,6),I(23,00,00,00),B(socket ,81),I(23,00,00,00),B(socket ,74),I(3b,01,25,00),B(socket ,5),I(23,00,00,00),B(socket ,82),I(23,00,00,00),B(socket ,73),I(3b,01,25,00),B(socket ,4),I(23,00,00,00),B(socket ,83),I(23,00,00,00),B(socket ,72),I(3b,01,25,00),B(socket ,3),I(86,25,00,00),B(socket ,2),I(23,00,00,00),B(socket ,84),I(23,00,00,00),B(socket ,71),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00958,G00957);
  eul_intern_keyword(key_961,"socket");
  object_class(str_962) = eul_static_string_class;
  eul_intern_keyword(key_963,"value");
  object_class(str_964) = eul_static_string_class;
  eul_intern_keyword(key_965,"file-name");
  eul_intern_keyword(key_966,"mode");
  eul_intern_symbol(sym_967,"r");
  eul_intern_keyword(key_968,"descriptor");
  eul_intern_symbol(sym_969,"w");
  eul_intern_symbol(sym_970,"*clean-ups*");
  eul_intern_symbol(sym_971,"anonymous");
  eul_allocate_bytevector( G00960,G00959);
  eul_allocate_bytevector( G00973,G00972);
  eul_allocate_bytevector( G00975,G00974);
  eul_allocate_bytevector( G00977,G00976);
  eul_allocate_bytevector( G00979,G00978);
  eul_allocate_bytevector( G00981,G00980);
  object_class(str_984) = eul_static_string_class;
  eul_allocate_bytevector( G00983,G00982);
  eul_allocate_bytevector( G00986,G00985);
  eul_allocate_bytevector( G00988,G00987);
  eul_allocate_bytevector( G00990,G00989);
  eul_allocate_bytevector( G00992,G00991);
  eul_allocate_bytevector( G00994,G00993);
  eul_allocate_bytevector( G00996,G00995);
  eul_allocate_bytevector( G00998,G00997);
  eul_allocate_bytevector( G001000,G00999);
  eul_allocate_bytevector( G001002,G001001);
  eul_allocate_bytevector( G001004,G001003);
  eul_allocate_bytevector( G001006,G001005);
  eul_intern_keyword(key_1009,"name");
  eul_intern_symbol(sym_1010,"port");
  eul_intern_keyword(key_1011,"default");
  eul_intern_keyword(key_1012,"keyword");
  eul_intern_keyword(key_1013,"port");
  eul_intern_symbol(sym_1014,"host");
  eul_intern_keyword(key_1015,"host");
  eul_intern_symbol(sym_1016,"descriptor");
  eul_intern_symbol(sym_1017,"queue-size");
  eul_intern_keyword(key_1018,"queue-size");
  eul_intern_symbol(sym_1019,"socket");
  eul_intern_keyword(key_1020,"direct-superclasses");
  eul_intern_keyword(key_1021,"direct-slots");
  eul_intern_keyword(key_1022,"direct-keywords");
  object_class(cons_1025) = eul_static_cons_class;
  eul_car(cons_1025) = key_1013;
  eul_cdr(cons_1025) = eul_nil;
  object_class(cons_1024) = eul_static_cons_class;
  eul_car(cons_1024) = key_1015;
  object_class(cons_1023) = eul_static_cons_class;
  eul_car(cons_1023) = key_1018;
  eul_intern_symbol(sym_1026,"connection");
  object_class(cons_1029) = eul_static_cons_class;
  eul_car(cons_1029) = key_1015;
  eul_cdr(cons_1029) = eul_nil;
  object_class(cons_1028) = eul_static_cons_class;
  eul_car(cons_1028) = key_1013;
  object_class(cons_1027) = eul_static_cons_class;
  eul_car(cons_1027) = key_961;
  eul_intern_symbol(sym_1030,"socket?");
  eul_intern_symbol(sym_1031,"connectionp");
  eul_intern_symbol(sym_1032,"(setter socket-queue-size)");
  eul_intern_symbol(sym_1033,"(setter socket-descriptor)");
  eul_intern_symbol(sym_1034,"(setter socket-host)");
  eul_intern_symbol(sym_1035,"(setter socket-port)");
  eul_intern_symbol(sym_1036,"(method socket?)");
  eul_intern_symbol(sym_1037,"(method initialize)");
  eul_intern_symbol(sym_1038,"(setter connection-port)");
  eul_intern_symbol(sym_1039,"(setter connection-host)");
  eul_intern_symbol(sym_1040,"(method connectionp)");
  eul_allocate_bytevector( G001008,G001007);
  eul_allocate_bytevector( G001042,G001041);
  eul_allocate_bytevector( G001044,G001043);
  eul_allocate_bytevector( G001046,G001045);
  eul_allocate_bytevector( G001048,G001047);
  eul_allocate_bytevector( G001050,G001049);
  eul_allocate_bytevector( G001052,G001051);
  eul_intern_symbol(sym_1055,"socket-host");
  eul_intern_symbol(sym_1056,"socket-port");
  eul_intern_symbol(sym_1057,"connection-port");
  eul_intern_symbol(sym_1058,"socket-queue-size");
  eul_intern_symbol(sym_1059,"connection-host");
  eul_intern_symbol(sym_1060,"socket-descriptor");
  eul_intern_symbol(sym_1061,"top-level");
  eul_allocate_bytevector( G001054,G001053);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 12; i++)
      socket_bindings[i] = eul_nil;
  }

  socket_bindings[ 12] = G00958;
  socket_bindings[ 13] = key_961;
  socket_bindings[ 14] = str_962;
  socket_bindings[ 15] = key_963;
  socket_bindings[ 16] = str_964;
  socket_bindings[ 17] = key_965;
  socket_bindings[ 18] = key_966;
  socket_bindings[ 19] = sym_967;
  socket_bindings[ 20] = key_968;
  socket_bindings[ 21] = sym_969;
  socket_bindings[ 22] = sym_970;
  socket_bindings[ 23] = sym_971;
  socket_bindings[ 24] = G00960;
  socket_bindings[ 25] = G00973;
  socket_bindings[ 26] = G00975;
  socket_bindings[ 27] = G00977;
  socket_bindings[ 28] = G00979;
  socket_bindings[ 29] = G00981;
  socket_bindings[ 30] = str_984;
  socket_bindings[ 31] = G00983;
  socket_bindings[ 32] = G00986;
  socket_bindings[ 33] = G00988;
  socket_bindings[ 34] = G00990;
  socket_bindings[ 35] = G00992;
  socket_bindings[ 36] = G00994;
  socket_bindings[ 37] = G00996;
  socket_bindings[ 38] = G00998;
  socket_bindings[ 39] = G001000;
  socket_bindings[ 40] = G001002;
  socket_bindings[ 41] = G001004;
  socket_bindings[ 42] = G001006;
  socket_bindings[ 43] = key_1009;
  socket_bindings[ 44] = sym_1010;
  socket_bindings[ 45] = key_1011;
  socket_bindings[ 46] = key_1012;
  socket_bindings[ 47] = key_1013;
  socket_bindings[ 48] = sym_1014;
  socket_bindings[ 49] = key_1015;
  socket_bindings[ 50] = sym_1016;
  socket_bindings[ 51] = sym_1017;
  socket_bindings[ 52] = key_1018;
  socket_bindings[ 53] = sym_1019;
  socket_bindings[ 54] = key_1020;
  socket_bindings[ 55] = key_1021;
  socket_bindings[ 56] = key_1022;
  socket_bindings[ 57] = cons_1023;
  socket_bindings[ 58] = sym_1026;
  socket_bindings[ 59] = cons_1027;
  socket_bindings[ 60] = sym_1030;
  socket_bindings[ 61] = sym_1031;
  socket_bindings[ 62] = sym_1032;
  socket_bindings[ 63] = sym_1033;
  socket_bindings[ 64] = sym_1034;
  socket_bindings[ 65] = sym_1035;
  socket_bindings[ 66] = sym_1036;
  socket_bindings[ 67] = sym_1037;
  socket_bindings[ 68] = sym_1038;
  socket_bindings[ 69] = sym_1039;
  socket_bindings[ 70] = sym_1040;
  socket_bindings[ 71] = G001008;
  socket_bindings[ 72] = G001042;
  socket_bindings[ 73] = G001044;
  socket_bindings[ 74] = G001046;
  socket_bindings[ 75] = G001048;
  socket_bindings[ 76] = G001050;
  socket_bindings[ 77] = G001052;
  socket_bindings[ 1] = eul_nil;
  socket_bindings[ 78] = sym_1055;
  socket_bindings[ 79] = sym_1056;
  socket_bindings[ 80] = sym_1057;
  socket_bindings[ 81] = sym_1058;
  socket_bindings[ 82] = sym_1059;
  socket_bindings[ 83] = sym_1060;
  socket_bindings[ 84] = sym_1061;
  eul_allocate_lambda( socket_bindings[0], "initialize-socket", 0, G001054);

  }
}


/* eof */
