/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module i-args
 **  Copyright: See file i-args.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern LispRef i_all_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef string_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot_bindings[];
extern LispRef i_error_bindings[];
extern LispRef mop_access_bindings[];
extern LispRef format_bindings[];
extern LispRef stream2_bindings[];
extern LispRef i_param_bindings[];
extern LispRef stream_bindings[];

/* Module bindings with size 135 */
LispRef i_args_bindings[135];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module i-args */
void initialize_module_i_args()
{
  if (is_initialized) return;
  initialize_module_i_all();
  eul_fast_table_set(eul_modules,"i_args",(LispRef) i_args_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_576, sym_575, sym_574, sym_573, sym_572, G00571, G00568, G00560, G00557, key_555, sym_506, sym_505, G00504, key_502, G00499, G00497, G00476, G00440;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_441, "Usage: youtoo [<options>] <source-file(s)> [<options>]", 54);
  eul_allocate_static_string(str_442, "  -help                 --  show usage", 38);
  eul_allocate_static_string(str_443, "  -version              --  show current release", 48);
  eul_allocate_static_string(str_444, "  -params               --  show current parameter setting", 58);
  eul_allocate_static_string(str_445, "  -load_path <dir>      --  add <dir> to load path", 50);
  eul_allocate_static_string(str_446, "  -c                    --  create C linkable module file only", 62);
  eul_allocate_static_string(str_447, "  -ar                   --  create C linkable library file", 58);
  eul_allocate_static_string(str_448, "  -l <lib>              --  specify C linkable library", 54);
  eul_allocate_static_string(str_449, "  -L <dir>              --  extent C linkable library load path", 63);
  eul_allocate_static_string(str_450, "  -fff <file>           --  specify C foreign function file", 59);
  eul_allocate_static_string(str_451, "  -ffl <lib>            --  specify C foreign function library", 62);
  eul_allocate_static_string(str_452, "  -o <file>             --  destination file", 44);
  eul_allocate_static_string(str_453, "  -od <dir>             --  destination directory for object files", 66);
  eul_allocate_static_string(str_454, "  --script <file>       --  script mode", 39);
  eul_allocate_static_string(str_455, "  -silent               --  silent mode", 39);
  eul_allocate_static_string(str_456, "  -verbose              --  verbose mode", 40);
  eul_allocate_static_string(str_457, "  -no_warnings          --  no warning messages", 47);
  eul_allocate_static_string(str_458, "  -no_errors            --  no error messages", 45);
  eul_allocate_static_string(str_459, "  -no_else              --  omit warning for if without else", 60);
  eul_allocate_static_string(str_460, "  -redefine             --  redefine imported bindings", 54);
  eul_allocate_static_string(str_461, "  -no_inline            --  ignore inline declarations", 54);
  eul_allocate_static_string(str_462, "  -stop_after <phase>   --  stop after compilation phase (e.g. read)", 68);
  eul_allocate_static_string(str_463, "  -recompile            --  recompile imported modules", 54);
  eul_allocate_static_string(str_464, "  -no_recompile         --  no automatic recompilation of imports", 65);
  eul_allocate_static_string(str_465, "  -no_gc                --  garbage collection library not linked", 65);
  eul_allocate_static_string(str_466, "  -cc                   --  used C compiler", 43);
  eul_allocate_static_string(str_467, "  -ld                   --  used C linker", 41);
  eul_allocate_static_string(str_468, "  -ar_cmd               --  used C ar command", 45);
  eul_allocate_static_string(str_469, "  -ranlib_cmd           --  used C ranlib command", 49);
  eul_allocate_static_string(str_470, "  -cflags               --  additional C flag", 45);
  eul_allocate_static_string(str_471, "  -static               --  no shared libraries used", 52);
  eul_allocate_static_string(str_472, "  -g                    --  C debug info", 40);
  eul_allocate_static_string(str_473, "  -i                    --  force interpretation mode", 53);
  eul_allocate_static_string(str_474, "  -no_ct_handlers       --  no compile-time error handling", 58);
  /* Byte-vector with size: 139 is_init: 0 index: 41 binding: print-help */
  static const void *G00439[] = {I(a9,23,00,00),B(i_args ,7),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,8),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,9),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,10),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,11),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,12),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,13),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,14),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,15),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,16),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,17),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,18),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,19),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,20),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,21),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,22),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,23),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,24),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,25),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,26),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,27),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,28),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,29),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,30),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,31),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,32),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,33),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,34),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,35),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,36),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,37),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,38),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,39),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,40),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,24),B(stream ,2),I(3d,00,00,00)};

  eul_allocate_static_string(str_477, "PARAMETER SETTINGS:", 19);
  eul_allocate_static_string(str_478, "*silent*", 8);
  eul_allocate_static_string(str_479, "*verbose*", 9);
  eul_allocate_static_string(str_480, "*warnings*", 10);
  eul_allocate_static_string(str_481, "*errors*", 8);
  eul_allocate_static_string(str_482, "*load-path*", 11);
  eul_allocate_static_string(str_483, "*C-library-load-path*", 21);
  eul_allocate_static_string(str_484, "*eulysses-dir*", 14);
  eul_allocate_static_string(str_485, "*create-C-module*", 17);
  eul_allocate_static_string(str_486, "*create-C-library*", 18);
  eul_allocate_static_string(str_487, "*stand-alone*", 13);
  eul_allocate_static_string(str_488, "*C-cc*", 6);
  eul_allocate_static_string(str_489, "*C-ld*", 6);
  eul_allocate_static_string(str_490, "*C-cc-flags*", 12);
  eul_allocate_static_string(str_491, "*C-ar*", 6);
  eul_allocate_static_string(str_492, "*C-ranlib*", 10);
  eul_allocate_static_string(str_493, "*no-ct-handlers*", 16);
  eul_allocate_static_string(str_494, "*inline-level*", 14);
  eul_allocate_static_string(str_495, "*recompile*", 11);
  /* Byte-vector with size: 117 is_init: 0 index: 61 binding: print-params */
  static const void *G00475[] = {I(a9,27,0a,24),B(stream ,13),I(3c,01,2a,23),B(i_args ,42),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,43),I(24,00,00,00),B(i_param ,20),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,44),I(24,00,00,00),B(i_param ,51),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,45),I(24,00,00,00),B(i_param ,58),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,46),I(24,00,00,00),B(i_param ,25),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,47),I(24,00,00,00),B(i_param ,16),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,48),I(24,00,00,00),B(i_param ,9),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,49),I(24,00,00,00),B(i_param ,2),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,50),I(24,00,00,00),B(i_param ,60),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,51),I(24,00,00,00),B(i_param ,28),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,52),I(24,00,00,00),B(i_param ,7),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,53),I(24,00,00,00),B(i_param ,44),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,54),I(24,00,00,00),B(i_param ,65),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,55),I(24,00,00,00),B(i_param ,54),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,56),I(24,00,00,00),B(i_param ,37),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,57),I(24,00,00,00),B(i_param ,8),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,58),I(24,00,00,00),B(i_param ,55),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,59),I(24,00,00,00),B(i_param ,64),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,23),B(i_args ,60),I(24,00,00,00),B(i_param ,57),I(24,00,00,00),B(i_args ,6),I(3c,02,2a,24),B(stream ,2),I(3d,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 62 binding: (method-G003) */
  static const void *G00496[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_500, "compile time error condition: ", 30);
  eul_allocate_static_string(str_501, "bad parameter ~a", 16);
  /* Byte-vector with size: 25 is_init: 0 index: 66 binding: (method-G003) */
  static const void *G00498[] = {I(ab,24,00,00),B(stream2 ,9),I(23,00,00,00),B(i_args ,63),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,9),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(44,04,86,36),I(2d,23,00,00),B(i_args ,64),I(47,00,00,24),B(format ,2),I(3c,02,24,00),B(i_error ,5),I(1c,23,00,00),B(i_args ,65),I(47,00,00,24),B(boot ,13),I(3d,04,03,22),I(01,45,02,00)};

  eul_allocate_static_string(str_507, "-help", 5);
  eul_allocate_static_string(str_508, "-version", 8);
  eul_allocate_static_string(str_509, "-params", 7);
  eul_allocate_static_string(str_510, "-load_path", 10);
  eul_allocate_static_string(str_511, "-o", 2);
  eul_allocate_static_string(str_512, "-od", 3);
  eul_allocate_static_string(str_513, "--script", 8);
  eul_allocate_static_string(str_514, "-silent", 7);
  eul_allocate_static_string(str_515, " -w", 3);
  eul_allocate_static_string(str_516, "-verbose", 8);
  eul_allocate_static_string(str_517, " -v", 3);
  eul_allocate_static_string(str_518, "-no_warnings", 12);
  eul_allocate_static_string(str_519, "-no_inline", 10);
  eul_allocate_static_string(str_520, "-no_peephole", 12);
  eul_allocate_static_string(str_521, "-no_errors", 10);
  eul_allocate_static_string(str_522, "-i", 2);
  eul_allocate_static_string(str_523, "-cc", 3);
  eul_allocate_static_string(str_524, "-ld", 3);
  eul_allocate_static_string(str_525, "-cflags", 7);
  eul_allocate_static_string(str_526, " ", 1);
  eul_allocate_static_string(str_527, " ", 1);
  eul_allocate_static_string(str_528, "-static", 7);
  eul_allocate_static_string(str_529, "gcc", 3);
  eul_allocate_static_string(str_530, " -static", 8);
  eul_allocate_static_string(str_531, "SUNOS5", 6);
  eul_allocate_static_string(str_532, " -dn", 4);
  eul_allocate_static_string(str_533, "IRIX", 4);
  eul_allocate_static_string(str_534, " -non_shared", 12);
  eul_allocate_static_string(str_535, " -static", 8);
  eul_allocate_static_string(str_536, "-no_else", 8);
  eul_allocate_static_string(str_537, "-redefine", 9);
  eul_allocate_static_string(str_538, "-g", 2);
  eul_allocate_static_string(str_539, " -g", 3);
  eul_allocate_static_string(str_540, "-ar_cmd", 7);
  eul_allocate_static_string(str_541, "-ranlib_cmd", 11);
  eul_allocate_static_string(str_542, "-l", 2);
  eul_allocate_static_string(str_543, "-L", 2);
  eul_allocate_static_string(str_544, "-fff", 4);
  eul_allocate_static_string(str_545, "-ffl", 4);
  eul_allocate_static_string(str_546, "-stop_after", 11);
  eul_allocate_static_string(str_547, "-c", 2);
  eul_allocate_static_string(str_548, "-stand_alone", 12);
  eul_allocate_static_string(str_549, "-ar", 3);
  eul_allocate_static_string(str_550, "-recompile", 10);
  eul_allocate_static_string(str_551, "-no_recompile", 13);
  eul_allocate_static_string(str_552, "-no_gc", 6);
  eul_allocate_static_string(str_553, "-no_ct_handlers", 15);
  eul_allocate_static_string(str_554, "bad parameter ~a", 16);
  /* Byte-vector with size: 801 is_init: 0 index: 118 binding: anonymous */
  static const void *G00503[] = {I(aa,84,24,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(i_args ,67),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,8a),I(03,02,84,86),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(i_args ,68),I(23,00,00,00),B(i_args ,66),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(8a,03,02,84),I(24,00,00,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(i_args ,68),I(23,00,00,00),B(i_args ,62),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,1f),I(12,47,00,02),I(19,1b,34,00),I(00,00,00,0e),I(87,32,00,00),I(00,00,0b,5b),I(47,00,00,1f),I(14,02,1b,23),B(i_args ,69),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(24,00,00,00),B(i_args ,2),I(3c,00,2a,24),B(i_error ,2),I(3c,00,32,00),I(00,00,0b,20),I(1c,23,00,00),B(i_args ,70),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(24,00,00,00),B(i_args ,5),I(3c,00,2a,24),B(i_error ,2),I(3c,00,32,00),I(00,00,0a,ea),I(1d,23,00,00),B(i_args ,71),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(24,00,00,00),B(i_args ,3),I(3c,00,2a,24),B(i_error ,2),I(3c,00,32,00),I(00,00,0a,b4),I(1f,03,23,00),B(i_args ,72),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,34),I(1f,18,83,14),I(47,00,00,1c),I(02,24,00,00),B(i_param ,16),I(0f,1b,89,00),B(i_param ,16),I(2a,1f,1a,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,0a,6c),I(1f,04,23,00),B(i_args ,73),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,34),I(1f,19,83,14),I(47,00,00,1c),I(02,41,00,00),B(boot1 ,56),I(22,01,1b,89),B(i_param ,6),I(2a,1f,1b,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,0a,22),I(1f,05,23,00),B(i_args ,74),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,34),I(1f,1a,83,14),I(47,00,00,1c),I(02,41,00,00),B(boot1 ,56),I(22,01,1b,89),B(i_param ,52),I(2a,1f,1c,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,09,d8),I(1f,06,23,00),B(i_args ,75),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,4c),I(1f,1b,83,14),I(47,00,00,1c),I(02,1b,89,00),B(i_param ,48),I(2a,87,89,00),B(i_param ,42),I(2a,86,89,00),B(i_param ,51),I(2a,87,89,00),B(i_param ,20),I(2a,87,89,00),B(i_param ,10),I(2a,1f,1d,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,09,76),I(1f,07,23,00),B(i_args ,76),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,4c),I(87,89,00,00),B(i_param ,20),I(2a,86,89,00),B(i_param ,51),I(2a,24,00,00),B(i_param ,54),I(23,00,00,00),B(i_args ,77),I(24,00,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(2a,1f,1d,83),I(14,47,00,01),I(3c,01,22,01),I(32,00,00,00),I(00,00,09,14),I(1f,08,23,00),B(i_args ,78),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,4c),I(86,89,00,00),B(i_param ,20),I(2a,87,89,00),B(i_param ,51),I(2a,24,00,00),B(i_param ,54),I(23,00,00,00),B(i_args ,79),I(24,00,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(2a,1f,1e,83),I(14,47,00,01),I(3c,01,22,01),I(32,00,00,00),I(00,00,08,b2),I(1f,09,23,00),B(i_args ,80),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(86,89,00,00),B(i_param ,58),I(2a,1f,1e,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,08,7a),I(1f,0a,23,00),B(i_args ,81),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(82,89,00,00),B(i_param ,64),I(2a,1f,1f,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,08,44),I(1f,0b,23,00),B(i_args ,82),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(86,89,00,00),B(i_param ,50),I(2a,1f,20,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,08,0e),I(1f,0c,23,00),B(i_args ,83),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(86,89,00,00),B(i_param ,25),I(2a,1f,21,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,07,d8),I(1f,0d,23,00),B(i_args ,84),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,30),I(87,89,00,00),B(i_param ,10),I(2a,87,89,00),B(i_param ,20),I(2a,86,89,00),B(i_param ,51),I(2a,1f,22,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,07,92),I(1f,0e,23,00),B(i_args ,85),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,2c),I(1f,23,83,14),I(47,00,00,1c),I(02,1b,89,00),B(i_param ,44),I(2a,1f,25,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,07,52),I(1f,0f,23,00),B(i_args ,86),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,2c),I(1f,24,83,14),I(47,00,00,1c),I(02,1b,89,00),B(i_param ,65),I(2a,1f,26,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,07,10),I(1f,10,23,00),B(i_args ,87),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,a4),I(24,00,00,00),B(i_param ,54),I(23,00,00,00),B(i_args ,88),I(24,00,00,00),B(string ,11),I(3c,02,1f,26),I(83,14,47,00),I(00,1c,02,1d),I(1c,24,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(2a,1f,29,84),I(14,47,00,00),I(1c,02,1b,82),I(0b,27,2d,50),I(1b,34,00,00),I(00,00,00,0f),I(86,32,00,00),I(00,00,00,3d),I(24,00,00,00),B(i_param ,54),I(23,00,00,00),B(i_args ,89),I(24,00,00,00),B(string ,11),I(3c,02,1f,2d),I(84,14,47,00),I(00,1c,02,1d),I(1c,24,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(22,04,2a,1f),I(2c,84,14,47),I(00,01,3c,01),I(22,07,32,00),I(00,00,06,54),I(1f,11,23,00),B(i_args ,90),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,dc),I(24,00,00,00),B(i_param ,44),I(23,00,00,00),B(i_args ,91),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,18),I(23,00,00,00),B(i_args ,92),I(32,00,00,00),I(00,00,00,7c),I(24,00,00,00),B(i_param ,49),I(23,00,00,00),B(i_args ,93),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,18),I(23,00,00,00),B(i_args ,94),I(32,00,00,00),I(00,00,00,46),I(24,00,00,00),B(i_param ,49),I(23,00,00,00),B(i_args ,95),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,18),I(23,00,00,00),B(i_args ,96),I(32,00,00,00),I(00,00,00,10),I(23,00,00,00),B(i_args ,97),I(22,01,22,01),I(24,00,00,00),B(i_param ,54),I(1c,24,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(2a,1f,29,83),I(14,47,00,01),I(3c,01,22,03),I(32,00,00,00),I(00,00,05,64),I(1f,12,23,00),B(i_args ,98),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(87,89,00,00),B(i_param ,13),I(2a,1f,27,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,05,2c),I(1f,13,23,00),B(i_args ,99),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(87,89,00,00),B(i_param ,31),I(2a,1f,28,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,04,f6),I(1f,14,23,00),B(i_args ,100),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,4c),I(24,00,00,00),B(i_param ,54),I(23,00,00,00),B(i_args ,101),I(24,00,00,00),B(string ,11),I(3c,02,1b,89),B(i_param ,54),I(2a,86,89,00),B(i_param ,56),I(2a,87,89,00),B(i_param ,45),I(2a,1f,2a,83),I(14,47,00,01),I(3c,01,22,01),I(32,00,00,00),I(00,00,04,96),I(1f,15,23,00),B(i_args ,102),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,2c),I(1f,2a,83,14),I(47,00,00,1c),I(02,1b,89,00),B(i_param ,37),I(2a,1f,2c,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,04,54),I(1f,16,23,00),B(i_args ,103),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,2c),I(1f,2b,83,14),I(47,00,00,1c),I(02,1b,89,00),B(i_param ,8),I(2a,1f,2d,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,04,12),I(1f,17,23,00),B(i_args ,104),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,3c),I(1f,2c,83,14),I(47,00,00,1c),I(02,1b,41,00),B(boot1 ,56),I(22,01,24,00),B(i_param ,14),I(0f,1b,89,00),B(i_param ,14),I(2a,1f,2f,84),I(14,47,00,01),I(3c,01,22,03),I(32,00,00,00),I(00,00,03,c0),I(1f,18,23,00),B(i_args ,105),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,34),I(1f,2d,83,14),I(47,00,00,1c),I(02,24,00,00),B(i_param ,9),I(0f,1b,89,00),B(i_param ,9),I(2a,1f,2f,84),I(14,47,00,01),I(3c,01,22,02),I(32,00,00,00),I(00,00,03,76),I(1f,19,23,00),B(i_args ,106),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,3c),I(1f,2e,83,14),I(47,00,00,1c),I(02,1b,41,00),B(boot1 ,56),I(22,01,24,00),B(i_param ,12),I(0f,1b,89,00),B(i_param ,12),I(2a,1f,31,84),I(14,47,00,01),I(3c,01,22,03),I(32,00,00,00),I(00,00,03,24),I(1f,1a,23,00),B(i_args ,107),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,3c),I(1f,2f,83,14),I(47,00,00,1c),I(02,1b,41,00),B(boot1 ,56),I(22,01,24,00),B(i_param ,3),I(0f,1b,89,00),B(i_param ,3),I(2a,1f,32,84),I(14,47,00,01),I(3c,01,22,03),I(32,00,00,00),I(00,00,02,d2),I(1f,1b,23,00),B(i_args ,108),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,34),I(1f,30,83,14),I(47,00,00,1c),I(02,1b,41,00),B(boot1 ,56),I(22,01,24,00),B(i_param ,38),I(3c,01,2a,1f),I(32,84,14,47),I(00,01,3c,01),I(22,02,32,00),I(00,00,02,86),I(1f,1c,23,00),B(i_args ,109),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,28),I(87,89,00,00),B(i_param ,60),I(2a,86,89,00),B(i_param ,7),I(2a,1f,31,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,02,48),I(1f,1d,23,00),B(i_args ,110),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(87,89,00,00),B(i_param ,7),I(2a,1f,32,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,02,12),I(1f,1e,23,00),B(i_args ,111),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,28),I(87,89,00,00),B(i_param ,28),I(2a,86,89,00),B(i_param ,7),I(2a,1f,33,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,01,d4),I(1f,1f,23,00),B(i_args ,112),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,28),I(87,89,00,00),B(i_param ,57),I(2a,86,89,00),B(i_param ,35),I(2a,1f,34,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,01,96),I(1f,20,23,00),B(i_args ,113),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,28),I(87,89,00,00),B(i_param ,35),I(2a,86,89,00),B(i_param ,57),I(2a,1f,35,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,01,58),I(1f,21,23,00),B(i_args ,114),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(87,89,00,00),B(i_param ,61),I(2a,1f,36,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,01,22),I(1f,22,23,00),B(i_args ,115),I(41,00,00,00),B(string ,17),I(22,02,2d,1b),I(34,00,00,00),I(00,00,00,20),I(87,89,00,00),B(i_param ,55),I(2a,1f,37,83),I(14,47,00,01),I(3c,01,32,00),I(00,00,00,ec),I(1f,23,82,0b),I(27,2d,50,1b),I(34,00,00,00),I(00,00,00,2c),I(86,23,00,00),B(i_args ,116),I(1f,26,24,00),B(i_notify ,6),I(3c,03,2a,1f),I(38,83,14,47),I(00,01,3c,01),I(32,00,00,00),I(00,00,00,b8),I(1f,24,06,1b),I(8a,03,15,1f),I(26,1c,0b,27),I(2e,51,1b,34),I(00,00,00,31),I(1d,84,15,1f),I(28,1c,0b,27),I(65,51,1b,34),I(00,00,00,19),I(1f,04,83,15),I(1f,2a,1c,0b),I(27,6d,51,22),I(01,32,00,00),I(00,00,00,08),I(86,22,02,32),I(00,00,00,06),I(86,1b,34,00),I(00,00,00,3a),I(1f,03,8a,03),I(15,1f,29,82),I(1d,24,00,00),B(string ,6),I(3c,03,24,00),B(mop_class ,5),I(23,00,00,00),B(i_args ,117),I(1d,24,00,00),B(mop_gf ,2),I(3c,03,22,02),I(32,00,00,00),I(00,00,00,22),I(24,00,00,00),B(mop_class ,5),I(23,00,00,00),B(i_args ,117),I(1f,2a,24,00),B(mop_gf ,2),I(3c,03,24,00),B(i_param ,33),I(0f,1b,89,00),B(i_param ,33),I(2a,1f,3d,83),I(14,47,00,01),I(3c,01,22,05),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,01),I(22,01,22,02),I(83,24,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,15,00,00)};

  eul_allocate_static_string(str_558, "Parse arguments ...", 19);
  /* Byte-vector with size: 26 is_init: 0 index: 120 binding: parse-args */
  static const void *G00556[] = {I(aa,46,04,1b),I(48,00,00,47),I(00,00,06,1b),I(48,00,02,86),I(1b,48,00,01),I(23,00,00,00),B(i_args ,67),I(23,00,00,00),B(i_args ,118),I(3b,01,48,00),I(01,23,00,00),B(i_args ,119),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,47),I(00,02,83,19),I(1b,44,1c,87),I(89,00,00,00),B(i_param ,10),I(2a,87,89,00),B(i_param ,20),I(2a,86,89,00),B(i_param ,51),I(36,08,83,47),I(00,01,3d,01),I(04,45,04,00)};

  eul_allocate_static_string(str_561, "EuLisp System 'youtoo'", 22);
  eul_allocate_static_string(str_562, "Version ~a updated\n", 19);
  eul_allocate_static_string(str_563, "Copyright 1996, 1997 A. Kind & University of Bath", 49);
  eul_allocate_static_string(str_564, "Copyright 2010 Henry G. Weller", 30);
  eul_allocate_static_string(str_565, "This is free software; see the source and the file COPYING for copying conditions.", 82);
  eul_allocate_static_string(str_566, "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.", 87);
  /* Byte-vector with size: 32 is_init: 0 index: 127 binding: print-version */
  static const void *G00559[] = {I(a9,23,00,00),B(i_args ,121),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,122),I(24,00,00,00),B(i_param ,43),I(24,00,00,00),B(format ,5),I(3c,02,2a,23),B(i_args ,123),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,124),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,125),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,23),B(i_args ,126),I(27,0a,24,00),B(stream ,13),I(3c,02,2a,27),I(0a,24,00,00),B(stream ,13),I(3c,01,2a,24),B(stream ,2),I(3d,00,00,00)};

  eul_allocate_static_string(str_569, "  ~a = ~a\n", 10);
  /* Byte-vector with size: 5 is_init: 0 index: 129 binding: print-param */
  static const void *G00567[] = {I(ab,23,00,00),B(i_args ,128),I(1d,1d,24,00),B(format ,5),I(3d,03,02,00)};

  /* Byte-vector with size: 38 is_init: 1 index: 0 binding: initialize-i-args */
  static const void *G00570[] = {I(87,25,00,00),B(i_args ,1),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(i_args ,130),I(23,00,00,00),B(i_args ,129),I(3b,02,25,00),B(i_args ,6),I(23,00,00,00),B(i_args ,131),I(23,00,00,00),B(i_args ,127),I(3b,00,25,00),B(i_args ,5),I(23,00,00,00),B(i_args ,132),I(23,00,00,00),B(i_args ,120),I(3b,01,25,00),B(i_args ,4),I(23,00,00,00),B(i_args ,133),I(23,00,00,00),B(i_args ,61),I(3b,00,25,00),B(i_args ,3),I(23,00,00,00),B(i_args ,134),I(23,00,00,00),B(i_args ,41),I(3b,00,25,00),B(i_args ,2),I(86,ac,00,00)};


  /* Initializations */
  object_class(str_441) = eul_static_string_class;
  object_class(str_442) = eul_static_string_class;
  object_class(str_443) = eul_static_string_class;
  object_class(str_444) = eul_static_string_class;
  object_class(str_445) = eul_static_string_class;
  object_class(str_446) = eul_static_string_class;
  object_class(str_447) = eul_static_string_class;
  object_class(str_448) = eul_static_string_class;
  object_class(str_449) = eul_static_string_class;
  object_class(str_450) = eul_static_string_class;
  object_class(str_451) = eul_static_string_class;
  object_class(str_452) = eul_static_string_class;
  object_class(str_453) = eul_static_string_class;
  object_class(str_454) = eul_static_string_class;
  object_class(str_455) = eul_static_string_class;
  object_class(str_456) = eul_static_string_class;
  object_class(str_457) = eul_static_string_class;
  object_class(str_458) = eul_static_string_class;
  object_class(str_459) = eul_static_string_class;
  object_class(str_460) = eul_static_string_class;
  object_class(str_461) = eul_static_string_class;
  object_class(str_462) = eul_static_string_class;
  object_class(str_463) = eul_static_string_class;
  object_class(str_464) = eul_static_string_class;
  object_class(str_465) = eul_static_string_class;
  object_class(str_466) = eul_static_string_class;
  object_class(str_467) = eul_static_string_class;
  object_class(str_468) = eul_static_string_class;
  object_class(str_469) = eul_static_string_class;
  object_class(str_470) = eul_static_string_class;
  object_class(str_471) = eul_static_string_class;
  object_class(str_472) = eul_static_string_class;
  object_class(str_473) = eul_static_string_class;
  object_class(str_474) = eul_static_string_class;
  eul_allocate_bytevector( G00440,G00439);
  object_class(str_477) = eul_static_string_class;
  object_class(str_478) = eul_static_string_class;
  object_class(str_479) = eul_static_string_class;
  object_class(str_480) = eul_static_string_class;
  object_class(str_481) = eul_static_string_class;
  object_class(str_482) = eul_static_string_class;
  object_class(str_483) = eul_static_string_class;
  object_class(str_484) = eul_static_string_class;
  object_class(str_485) = eul_static_string_class;
  object_class(str_486) = eul_static_string_class;
  object_class(str_487) = eul_static_string_class;
  object_class(str_488) = eul_static_string_class;
  object_class(str_489) = eul_static_string_class;
  object_class(str_490) = eul_static_string_class;
  object_class(str_491) = eul_static_string_class;
  object_class(str_492) = eul_static_string_class;
  object_class(str_493) = eul_static_string_class;
  object_class(str_494) = eul_static_string_class;
  object_class(str_495) = eul_static_string_class;
  eul_allocate_bytevector( G00476,G00475);
  eul_allocate_bytevector( G00497,G00496);
  object_class(str_500) = eul_static_string_class;
  object_class(str_501) = eul_static_string_class;
  eul_intern_keyword(key_502,"ct-error-value");
  eul_allocate_bytevector( G00499,G00498);
  eul_intern_symbol(sym_505,"anonymous");
  eul_intern_symbol(sym_506,"(method G003)");
  object_class(str_507) = eul_static_string_class;
  object_class(str_508) = eul_static_string_class;
  object_class(str_509) = eul_static_string_class;
  object_class(str_510) = eul_static_string_class;
  object_class(str_511) = eul_static_string_class;
  object_class(str_512) = eul_static_string_class;
  object_class(str_513) = eul_static_string_class;
  object_class(str_514) = eul_static_string_class;
  object_class(str_515) = eul_static_string_class;
  object_class(str_516) = eul_static_string_class;
  object_class(str_517) = eul_static_string_class;
  object_class(str_518) = eul_static_string_class;
  object_class(str_519) = eul_static_string_class;
  object_class(str_520) = eul_static_string_class;
  object_class(str_521) = eul_static_string_class;
  object_class(str_522) = eul_static_string_class;
  object_class(str_523) = eul_static_string_class;
  object_class(str_524) = eul_static_string_class;
  object_class(str_525) = eul_static_string_class;
  object_class(str_526) = eul_static_string_class;
  object_class(str_527) = eul_static_string_class;
  object_class(str_528) = eul_static_string_class;
  object_class(str_529) = eul_static_string_class;
  object_class(str_530) = eul_static_string_class;
  object_class(str_531) = eul_static_string_class;
  object_class(str_532) = eul_static_string_class;
  object_class(str_533) = eul_static_string_class;
  object_class(str_534) = eul_static_string_class;
  object_class(str_535) = eul_static_string_class;
  object_class(str_536) = eul_static_string_class;
  object_class(str_537) = eul_static_string_class;
  object_class(str_538) = eul_static_string_class;
  object_class(str_539) = eul_static_string_class;
  object_class(str_540) = eul_static_string_class;
  object_class(str_541) = eul_static_string_class;
  object_class(str_542) = eul_static_string_class;
  object_class(str_543) = eul_static_string_class;
  object_class(str_544) = eul_static_string_class;
  object_class(str_545) = eul_static_string_class;
  object_class(str_546) = eul_static_string_class;
  object_class(str_547) = eul_static_string_class;
  object_class(str_548) = eul_static_string_class;
  object_class(str_549) = eul_static_string_class;
  object_class(str_550) = eul_static_string_class;
  object_class(str_551) = eul_static_string_class;
  object_class(str_552) = eul_static_string_class;
  object_class(str_553) = eul_static_string_class;
  object_class(str_554) = eul_static_string_class;
  eul_intern_keyword(key_555,"name");
  eul_allocate_bytevector( G00504,G00503);
  object_class(str_558) = eul_static_string_class;
  eul_allocate_bytevector( G00557,G00556);
  object_class(str_561) = eul_static_string_class;
  object_class(str_562) = eul_static_string_class;
  object_class(str_563) = eul_static_string_class;
  object_class(str_564) = eul_static_string_class;
  object_class(str_565) = eul_static_string_class;
  object_class(str_566) = eul_static_string_class;
  eul_allocate_bytevector( G00560,G00559);
  object_class(str_569) = eul_static_string_class;
  eul_allocate_bytevector( G00568,G00567);
  eul_intern_symbol(sym_572,"print-param");
  eul_intern_symbol(sym_573,"print-version");
  eul_intern_symbol(sym_574,"parse-args");
  eul_intern_symbol(sym_575,"print-params");
  eul_intern_symbol(sym_576,"print-help");
  eul_allocate_bytevector( G00571,G00570);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 7; i++)
      i_args_bindings[i] = eul_nil;
  }

  i_args_bindings[ 7] = str_441;
  i_args_bindings[ 8] = str_442;
  i_args_bindings[ 9] = str_443;
  i_args_bindings[ 10] = str_444;
  i_args_bindings[ 11] = str_445;
  i_args_bindings[ 12] = str_446;
  i_args_bindings[ 13] = str_447;
  i_args_bindings[ 14] = str_448;
  i_args_bindings[ 15] = str_449;
  i_args_bindings[ 16] = str_450;
  i_args_bindings[ 17] = str_451;
  i_args_bindings[ 18] = str_452;
  i_args_bindings[ 19] = str_453;
  i_args_bindings[ 20] = str_454;
  i_args_bindings[ 21] = str_455;
  i_args_bindings[ 22] = str_456;
  i_args_bindings[ 23] = str_457;
  i_args_bindings[ 24] = str_458;
  i_args_bindings[ 25] = str_459;
  i_args_bindings[ 26] = str_460;
  i_args_bindings[ 27] = str_461;
  i_args_bindings[ 28] = str_462;
  i_args_bindings[ 29] = str_463;
  i_args_bindings[ 30] = str_464;
  i_args_bindings[ 31] = str_465;
  i_args_bindings[ 32] = str_466;
  i_args_bindings[ 33] = str_467;
  i_args_bindings[ 34] = str_468;
  i_args_bindings[ 35] = str_469;
  i_args_bindings[ 36] = str_470;
  i_args_bindings[ 37] = str_471;
  i_args_bindings[ 38] = str_472;
  i_args_bindings[ 39] = str_473;
  i_args_bindings[ 40] = str_474;
  i_args_bindings[ 41] = G00440;
  i_args_bindings[ 42] = str_477;
  i_args_bindings[ 43] = str_478;
  i_args_bindings[ 44] = str_479;
  i_args_bindings[ 45] = str_480;
  i_args_bindings[ 46] = str_481;
  i_args_bindings[ 47] = str_482;
  i_args_bindings[ 48] = str_483;
  i_args_bindings[ 49] = str_484;
  i_args_bindings[ 50] = str_485;
  i_args_bindings[ 51] = str_486;
  i_args_bindings[ 52] = str_487;
  i_args_bindings[ 53] = str_488;
  i_args_bindings[ 54] = str_489;
  i_args_bindings[ 55] = str_490;
  i_args_bindings[ 56] = str_491;
  i_args_bindings[ 57] = str_492;
  i_args_bindings[ 58] = str_493;
  i_args_bindings[ 59] = str_494;
  i_args_bindings[ 60] = str_495;
  i_args_bindings[ 61] = G00476;
  i_args_bindings[ 62] = G00497;
  i_args_bindings[ 63] = str_500;
  i_args_bindings[ 64] = str_501;
  i_args_bindings[ 65] = key_502;
  i_args_bindings[ 66] = G00499;
  i_args_bindings[ 67] = sym_505;
  i_args_bindings[ 68] = sym_506;
  i_args_bindings[ 69] = str_507;
  i_args_bindings[ 70] = str_508;
  i_args_bindings[ 71] = str_509;
  i_args_bindings[ 72] = str_510;
  i_args_bindings[ 73] = str_511;
  i_args_bindings[ 74] = str_512;
  i_args_bindings[ 75] = str_513;
  i_args_bindings[ 76] = str_514;
  i_args_bindings[ 77] = str_515;
  i_args_bindings[ 78] = str_516;
  i_args_bindings[ 79] = str_517;
  i_args_bindings[ 80] = str_518;
  i_args_bindings[ 81] = str_519;
  i_args_bindings[ 82] = str_520;
  i_args_bindings[ 83] = str_521;
  i_args_bindings[ 84] = str_522;
  i_args_bindings[ 85] = str_523;
  i_args_bindings[ 86] = str_524;
  i_args_bindings[ 87] = str_525;
  i_args_bindings[ 88] = str_526;
  i_args_bindings[ 89] = str_527;
  i_args_bindings[ 90] = str_528;
  i_args_bindings[ 91] = str_529;
  i_args_bindings[ 92] = str_530;
  i_args_bindings[ 93] = str_531;
  i_args_bindings[ 94] = str_532;
  i_args_bindings[ 95] = str_533;
  i_args_bindings[ 96] = str_534;
  i_args_bindings[ 97] = str_535;
  i_args_bindings[ 98] = str_536;
  i_args_bindings[ 99] = str_537;
  i_args_bindings[ 100] = str_538;
  i_args_bindings[ 101] = str_539;
  i_args_bindings[ 102] = str_540;
  i_args_bindings[ 103] = str_541;
  i_args_bindings[ 104] = str_542;
  i_args_bindings[ 105] = str_543;
  i_args_bindings[ 106] = str_544;
  i_args_bindings[ 107] = str_545;
  i_args_bindings[ 108] = str_546;
  i_args_bindings[ 109] = str_547;
  i_args_bindings[ 110] = str_548;
  i_args_bindings[ 111] = str_549;
  i_args_bindings[ 112] = str_550;
  i_args_bindings[ 113] = str_551;
  i_args_bindings[ 114] = str_552;
  i_args_bindings[ 115] = str_553;
  i_args_bindings[ 116] = str_554;
  i_args_bindings[ 117] = key_555;
  i_args_bindings[ 118] = G00504;
  i_args_bindings[ 119] = str_558;
  i_args_bindings[ 120] = G00557;
  i_args_bindings[ 121] = str_561;
  i_args_bindings[ 122] = str_562;
  i_args_bindings[ 123] = str_563;
  i_args_bindings[ 124] = str_564;
  i_args_bindings[ 125] = str_565;
  i_args_bindings[ 126] = str_566;
  i_args_bindings[ 127] = G00560;
  i_args_bindings[ 128] = str_569;
  i_args_bindings[ 129] = G00568;
  i_args_bindings[ 1] = eul_nil;
  i_args_bindings[ 130] = sym_572;
  i_args_bindings[ 131] = sym_573;
  i_args_bindings[ 132] = sym_574;
  i_args_bindings[ 133] = sym_575;
  i_args_bindings[ 134] = sym_576;
  eul_allocate_lambda( i_args_bindings[0], "initialize-i-args", 0, G00571);

  }
}


/* eof */
