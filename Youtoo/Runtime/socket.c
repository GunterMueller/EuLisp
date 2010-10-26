/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module socket
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
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot_bindings[];
extern LispRef stream2_bindings[];
extern LispRef stream1_bindings[];
extern LispRef convert_bindings[];
extern LispRef string_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef lock_bindings[];

/* Module bindings with size 76 */
LispRef socket_bindings[76];

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
  LispRef sym_5942, sym_5941, sym_5940, sym_5939, sym_5938, sym_5937, sym_5936, G005935, G005933, G005931, G005929, G005927, G005925, G005923, sym_5921, sym_5920, sym_5919, sym_5918, sym_5917, sym_5916, sym_5915, sym_5914, sym_5913, sym_5912, sym_5911, key_5908, sym_5906, key_5902, key_5901, key_5900, sym_5899, key_5898, sym_5897, sym_5896, key_5895, sym_5894, key_5893, key_5892, key_5891, sym_5890, key_5889, G005888, G005886, G005884, G005882, G005880, G005878, G005876, G005874, G005872, G005870, G005868, G005866, sym_5864, sym_5863, key_5862, G005860, G005858, G005856, G005854, G005852, G005850;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 1 is_init: 0 index: 12 binding: (method-connection?) */
  static const void *G005849[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-connection?) */
  static const void *G005851[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 14 binding: (setter-connection-host) */
  static const void *G005853[] = {I(ab,1c,83,1d),I(24,00,00,00),B(socket ,2),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 15 binding: (setter-connection-port) */
  static const void *G005855[] = {I(ab,1c,82,1d),I(24,00,00,00),B(socket ,2),I(09,45,02,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 16 binding: anonymous */
  static const void *G005857[] = {I(a9,47,00,00),I(24,00,00,00),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_5861, "tcp", 3);
  /* Byte-vector with size: 76 is_init: 0 index: 21 binding: (method-initialize) */
  static const void *G005859[] = {I(ab,46,02,1c),I(1c,37,02,2a),I(1c,8a,03,24),B(socket ,8),I(08,24,00,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3c,02,1d,82),I(24,00,00,00),B(socket ,8),I(08,1c,23,00),B(socket ,17),I(1d,41,00,00),B(stream1 ,21),I(22,03,1b,85),I(19,1b,44,28),I(24,00,00,00),B(stream1 ,6),I(3c,00,24,00),B(stream2 ,38),I(1c,23,00,00),B(socket ,18),I(1f,09,24,00),B(boot ,13),I(3d,04,07,22),I(01,36,c3,1c),I(85,1a,1b,44),I(2f,1d,41,00),B(stream1 ,24),I(22,01,24,00),B(stream2 ,38),I(1c,24,00,00),B(stream2 ,38),I(23,00,00,00),B(socket ,18),I(1f,0b,24,00),B(boot ,13),I(3d,05,08,22),I(01,36,8d,1f),I(06,1f,03,1c),I(83,1d,24,00),B(socket ,8),I(09,22,02,2a),I(24,00,00,00),B(stream2 ,11),I(1b,48,00,00),I(23,00,00,00),B(socket ,19),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(socket ,20),I(23,00,00,00),B(socket ,16),I(3b,00,1c,0f),I(23,00,00,00),B(socket ,19),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,47),I(00,00,24,00),B(lock ,3),I(3c,01,2a,1f),I(09,24,00,00),B(stream2 ,36),I(0f,1b,89,00),B(stream2 ,36),I(47,00,00,24),B(lock ,4),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(0b,22,05,22),I(01,45,06,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 22 binding: (method-socket?) */
  static const void *G005865[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 23 binding: (method-socket?) */
  static const void *G005867[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 24 binding: (setter-socket-port) */
  static const void *G005869[] = {I(ab,1c,8a,03),I(1d,24,00,00),B(socket ,8),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 25 binding: (setter-socket-host) */
  static const void *G005871[] = {I(ab,1c,84,1d),I(24,00,00,00),B(socket ,8),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 26 binding: (setter-socket-descriptor) */
  static const void *G005873[] = {I(ab,1c,83,1d),I(24,00,00,00),B(socket ,8),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 27 binding: (setter-socket-queue-size) */
  static const void *G005875[] = {I(ab,1c,82,1d),I(24,00,00,00),B(socket ,8),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 28 binding: anonymous */
  static const void *G005877[] = {I(a9,26,00,00),I(00,00,12,67),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 29 binding: anonymous */
  static const void *G005879[] = {I(a9,24,00,00),B(stream1 ,3),I(3d,00,00,00)};

  /* Byte-vector with size: 2 is_init: 0 index: 30 binding: anonymous */
  static const void *G005881[] = {I(a9,8a,05,45),I(00,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 31 binding: anonymous */
  static const void *G005883[] = {I(a9,24,00,00),B(stream1 ,3),I(3d,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 32 binding: anonymous */
  static const void *G005885[] = {I(a9,26,00,00),I(00,00,12,67),I(45,00,00,00)};

  eul_allocate_static_cons(cons_5905, NULL, NULL);
  eul_allocate_static_cons(cons_5904, NULL, eul_as_static(cons_5905));
  eul_allocate_static_cons(cons_5903, NULL, eul_as_static(cons_5904));
  eul_allocate_static_cons(cons_5910, NULL, NULL);
  eul_allocate_static_cons(cons_5909, NULL, eul_as_static(cons_5910));
  eul_allocate_static_cons(cons_5907, NULL, eul_as_static(cons_5909));
  /* Byte-vector with size: 380 is_init: 0 index: 62 binding: top-level */
  static const void *G005887[] = {I(a9,24,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(socket ,33),I(23,00,00,00),B(socket ,34),I(23,00,00,00),B(socket ,35),I(23,00,00,00),B(socket ,20),I(23,00,00,00),B(socket ,32),I(3b,00,23,00),B(socket ,36),I(23,00,00,00),B(socket ,37),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(socket ,33),I(23,00,00,00),B(socket ,38),I(23,00,00,00),B(socket ,35),I(23,00,00,00),B(socket ,20),I(23,00,00,00),B(socket ,31),I(3b,00,23,00),B(socket ,36),I(23,00,00,00),B(socket ,39),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(socket ,33),I(23,00,00,00),B(socket ,40),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(socket ,33),I(23,00,00,00),B(socket ,41),I(23,00,00,00),B(socket ,35),I(23,00,00,00),B(socket ,20),I(23,00,00,00),B(socket ,30),I(3b,00,23,00),B(socket ,36),I(23,00,00,00),B(socket ,42),I(24,00,00,00),B(boot1 ,26),I(3c,06,1f,03),I(1f,03,1f,03),I(1f,03,24,00),B(boot1 ,26),I(3c,04,24,00),B(mop_class ,81),I(23,00,00,00),B(socket ,33),I(23,00,00,00),B(socket ,43),I(23,00,00,00),B(socket ,44),I(1f,09,23,00),B(socket ,45),I(1f,06,23,00),B(socket ,46),I(23,00,00,00),B(socket ,47),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(socket ,8),I(2a,24,00,00),B(stream2 ,4),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(socket ,33),I(23,00,00,00),B(socket ,38),I(23,00,00,00),B(socket ,35),I(23,00,00,00),B(socket ,20),I(23,00,00,00),B(socket ,29),I(3b,00,23,00),B(socket ,36),I(23,00,00,00),B(socket ,39),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(socket ,33),I(23,00,00,00),B(socket ,34),I(23,00,00,00),B(socket ,35),I(23,00,00,00),B(socket ,20),I(23,00,00,00),B(socket ,28),I(3b,00,23,00),B(socket ,36),I(23,00,00,00),B(socket ,37),I(24,00,00,00),B(boot1 ,26),I(3c,06,1c,1c),I(24,00,00,00),B(boot1 ,26),I(3c,02,24,00),B(mop_class ,81),I(23,00,00,00),B(socket ,33),I(23,00,00,00),B(socket ,48),I(23,00,00,00),B(socket ,44),I(1f,07,23,00),B(socket ,45),I(1f,06,23,00),B(socket ,46),I(23,00,00,00),B(socket ,50),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(socket ,2),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(socket ,51),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(socket ,10),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(socket ,52),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(socket ,4),I(2a,24,00,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,6),I(23,00,00,00),B(socket ,53),I(23,00,00,00),B(socket ,27),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,3),I(23,00,00,00),B(socket ,54),I(23,00,00,00),B(socket ,26),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,11),I(23,00,00,00),B(socket ,55),I(23,00,00,00),B(socket ,25),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,9),I(23,00,00,00),B(socket ,56),I(23,00,00,00),B(socket ,24),I(3b,02,1d,3c),I(02,2a,24,00),B(socket ,10),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(socket ,10),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,57),I(23,00,00,00),B(socket ,23),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,10),I(8a,03,02,83),I(24,00,00,00),B(socket ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(socket ,10),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,57),I(23,00,00,00),B(socket ,22),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,10),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,10),I(2a,24,00,00),B(socket ,8),I(2a,24,00,00),B(mop_gf ,12),I(8a,03,02,84),I(24,00,00,00),B(socket ,8),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,58),I(23,00,00,00),B(socket ,21),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,7),I(23,00,00,00),B(socket ,59),I(23,00,00,00),B(socket ,15),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(socket ,5),I(23,00,00,00),B(socket ,60),I(23,00,00,00),B(socket ,14),I(3b,02,1d,3c),I(02,2a,24,00),B(socket ,4),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(socket ,4),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,61),I(23,00,00,00),B(socket ,13),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,4),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,4),I(8a,03,02,83),I(24,00,00,00),B(socket ,2),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(socket ,4),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(socket ,61),I(23,00,00,00),B(socket ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(socket ,4),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(socket ,4),I(2a,24,00,00),B(socket ,2),I(45,38,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 63 binding: socket-descriptor */
  static const void *G005922[] = {I(aa,83,24,00),B(socket ,8),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 64 binding: connection-host */
  static const void *G005924[] = {I(aa,83,24,00),B(socket ,2),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 65 binding: socket-queue-size */
  static const void *G005926[] = {I(aa,82,24,00),B(socket ,8),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 66 binding: connection-port */
  static const void *G005928[] = {I(aa,82,24,00),B(socket ,2),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 67 binding: socket-port */
  static const void *G005930[] = {I(aa,8a,03,24),B(socket ,8),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 68 binding: socket-host */
  static const void *G005932[] = {I(aa,84,24,00),B(socket ,8),I(08,45,00,00)};

  /* Byte-vector with size: 92 is_init: 1 index: 0 binding: initialize-socket */
  static const void *G005934[] = {I(87,25,00,00),B(socket ,1),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(stream2 ,1),I(3e,0b,24,00),B(stream2 ,0),I(3c,00,21,01),I(24,00,00,00),B(stream1 ,1),I(3e,0b,24,00),B(stream1 ,0),I(3c,00,21,01),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(lock ,1),I(3e,0b,24,00),B(lock ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(socket ,69),I(23,00,00,00),B(socket ,68),I(3b,01,25,00),B(socket ,11),I(86,25,00,00),B(socket ,10),I(23,00,00,00),B(socket ,70),I(23,00,00,00),B(socket ,67),I(3b,01,25,00),B(socket ,9),I(86,25,00,00),B(socket ,8),I(23,00,00,00),B(socket ,71),I(23,00,00,00),B(socket ,66),I(3b,01,25,00),B(socket ,7),I(23,00,00,00),B(socket ,72),I(23,00,00,00),B(socket ,65),I(3b,01,25,00),B(socket ,6),I(23,00,00,00),B(socket ,73),I(23,00,00,00),B(socket ,64),I(3b,01,25,00),B(socket ,5),I(86,25,00,00),B(socket ,4),I(23,00,00,00),B(socket ,74),I(23,00,00,00),B(socket ,63),I(3b,01,25,00),B(socket ,3),I(86,25,00,00),B(socket ,2),I(23,00,00,00),B(socket ,75),I(23,00,00,00),B(socket ,62),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G005850,G005849);
  eul_allocate_bytevector( G005852,G005851);
  eul_allocate_bytevector( G005854,G005853);
  eul_allocate_bytevector( G005856,G005855);
  eul_allocate_bytevector( G005858,G005857);
  object_class(str_5861) = eul_static_string_class;
  eul_intern_keyword(key_5862,"value");
  eul_intern_symbol(sym_5863,"*clean-ups*");
  eul_intern_symbol(sym_5864,"anonymous");
  eul_allocate_bytevector( G005860,G005859);
  eul_allocate_bytevector( G005866,G005865);
  eul_allocate_bytevector( G005868,G005867);
  eul_allocate_bytevector( G005870,G005869);
  eul_allocate_bytevector( G005872,G005871);
  eul_allocate_bytevector( G005874,G005873);
  eul_allocate_bytevector( G005876,G005875);
  eul_allocate_bytevector( G005878,G005877);
  eul_allocate_bytevector( G005880,G005879);
  eul_allocate_bytevector( G005882,G005881);
  eul_allocate_bytevector( G005884,G005883);
  eul_allocate_bytevector( G005886,G005885);
  eul_intern_keyword(key_5889,"name");
  eul_intern_symbol(sym_5890,"port");
  eul_intern_keyword(key_5891,"default");
  eul_intern_keyword(key_5892,"keyword");
  eul_intern_keyword(key_5893,"port");
  eul_intern_symbol(sym_5894,"host");
  eul_intern_keyword(key_5895,"host");
  eul_intern_symbol(sym_5896,"descriptor");
  eul_intern_symbol(sym_5897,"queue-size");
  eul_intern_keyword(key_5898,"queue-size");
  eul_intern_symbol(sym_5899,"socket");
  eul_intern_keyword(key_5900,"direct-superclasses");
  eul_intern_keyword(key_5901,"direct-slots");
  eul_intern_keyword(key_5902,"direct-keywords");
  object_class(cons_5905) = eul_static_cons_class;
  eul_car(cons_5905) = key_5893;
  eul_cdr(cons_5905) = eul_nil;
  object_class(cons_5904) = eul_static_cons_class;
  eul_car(cons_5904) = key_5895;
  object_class(cons_5903) = eul_static_cons_class;
  eul_car(cons_5903) = key_5898;
  eul_intern_symbol(sym_5906,"connection");
  eul_intern_keyword(key_5908,"socket");
  object_class(cons_5910) = eul_static_cons_class;
  eul_car(cons_5910) = key_5895;
  eul_cdr(cons_5910) = eul_nil;
  object_class(cons_5909) = eul_static_cons_class;
  eul_car(cons_5909) = key_5893;
  object_class(cons_5907) = eul_static_cons_class;
  eul_car(cons_5907) = key_5908;
  eul_intern_symbol(sym_5911,"socket?");
  eul_intern_symbol(sym_5912,"connection?");
  eul_intern_symbol(sym_5913,"(setter socket-queue-size)");
  eul_intern_symbol(sym_5914,"(setter socket-descriptor)");
  eul_intern_symbol(sym_5915,"(setter socket-host)");
  eul_intern_symbol(sym_5916,"(setter socket-port)");
  eul_intern_symbol(sym_5917,"(method socket?)");
  eul_intern_symbol(sym_5918,"(method initialize)");
  eul_intern_symbol(sym_5919,"(setter connection-port)");
  eul_intern_symbol(sym_5920,"(setter connection-host)");
  eul_intern_symbol(sym_5921,"(method connection?)");
  eul_allocate_bytevector( G005888,G005887);
  eul_allocate_bytevector( G005923,G005922);
  eul_allocate_bytevector( G005925,G005924);
  eul_allocate_bytevector( G005927,G005926);
  eul_allocate_bytevector( G005929,G005928);
  eul_allocate_bytevector( G005931,G005930);
  eul_allocate_bytevector( G005933,G005932);
  eul_intern_symbol(sym_5936,"socket-host");
  eul_intern_symbol(sym_5937,"socket-port");
  eul_intern_symbol(sym_5938,"connection-port");
  eul_intern_symbol(sym_5939,"socket-queue-size");
  eul_intern_symbol(sym_5940,"connection-host");
  eul_intern_symbol(sym_5941,"socket-descriptor");
  eul_intern_symbol(sym_5942,"top-level");
  eul_allocate_bytevector( G005935,G005934);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 12; i++)
      socket_bindings[i] = eul_nil;
  }

  socket_bindings[ 12] = G005850;
  socket_bindings[ 13] = G005852;
  socket_bindings[ 14] = G005854;
  socket_bindings[ 15] = G005856;
  socket_bindings[ 16] = G005858;
  socket_bindings[ 17] = str_5861;
  socket_bindings[ 18] = key_5862;
  socket_bindings[ 19] = sym_5863;
  socket_bindings[ 20] = sym_5864;
  socket_bindings[ 21] = G005860;
  socket_bindings[ 22] = G005866;
  socket_bindings[ 23] = G005868;
  socket_bindings[ 24] = G005870;
  socket_bindings[ 25] = G005872;
  socket_bindings[ 26] = G005874;
  socket_bindings[ 27] = G005876;
  socket_bindings[ 28] = G005878;
  socket_bindings[ 29] = G005880;
  socket_bindings[ 30] = G005882;
  socket_bindings[ 31] = G005884;
  socket_bindings[ 32] = G005886;
  socket_bindings[ 33] = key_5889;
  socket_bindings[ 34] = sym_5890;
  socket_bindings[ 35] = key_5891;
  socket_bindings[ 36] = key_5892;
  socket_bindings[ 37] = key_5893;
  socket_bindings[ 38] = sym_5894;
  socket_bindings[ 39] = key_5895;
  socket_bindings[ 40] = sym_5896;
  socket_bindings[ 41] = sym_5897;
  socket_bindings[ 42] = key_5898;
  socket_bindings[ 43] = sym_5899;
  socket_bindings[ 44] = key_5900;
  socket_bindings[ 45] = key_5901;
  socket_bindings[ 46] = key_5902;
  socket_bindings[ 47] = cons_5903;
  socket_bindings[ 48] = sym_5906;
  socket_bindings[ 49] = key_5908;
  socket_bindings[ 50] = cons_5907;
  socket_bindings[ 51] = sym_5911;
  socket_bindings[ 52] = sym_5912;
  socket_bindings[ 53] = sym_5913;
  socket_bindings[ 54] = sym_5914;
  socket_bindings[ 55] = sym_5915;
  socket_bindings[ 56] = sym_5916;
  socket_bindings[ 57] = sym_5917;
  socket_bindings[ 58] = sym_5918;
  socket_bindings[ 59] = sym_5919;
  socket_bindings[ 60] = sym_5920;
  socket_bindings[ 61] = sym_5921;
  socket_bindings[ 62] = G005888;
  socket_bindings[ 63] = G005923;
  socket_bindings[ 64] = G005925;
  socket_bindings[ 65] = G005927;
  socket_bindings[ 66] = G005929;
  socket_bindings[ 67] = G005931;
  socket_bindings[ 68] = G005933;
  socket_bindings[ 1] = eul_nil;
  socket_bindings[ 69] = sym_5936;
  socket_bindings[ 70] = sym_5937;
  socket_bindings[ 71] = sym_5938;
  socket_bindings[ 72] = sym_5939;
  socket_bindings[ 73] = sym_5940;
  socket_bindings[ 74] = sym_5941;
  socket_bindings[ 75] = sym_5942;
  eul_allocate_lambda( socket_bindings[0], "initialize-socket", 0, G005935);

  }
}


/* eof */
