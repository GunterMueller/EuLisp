/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module symbol
 **  Copyright: See file symbol.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_convert();
extern void initialize_module_compare();
extern void initialize_module_collect();
extern void initialize_module_character();
extern void initialize_module_string();
extern void initialize_module_table();
extern void initialize_module_fpi();
extern LispRef character_bindings[];
extern LispRef telos_bindings[];
extern LispRef fpi_bindings[];
extern LispRef table_bindings[];
extern LispRef compare_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef boot1_bindings[];
extern LispRef collect_bindings[];
extern LispRef convert_bindings[];
extern LispRef string_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_class_bindings[];

/* Module bindings with size 37 */
LispRef symbol_bindings[37];

/* Foreign functions */
static LispRef ff_stub_eul_init_symbol10174 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G0010219, res;

  POPVAL1(G0010219);
  FF_RES_CONVERT6(res,eul_init_symbol(FF_ARG_CONVERT8(G0010219)));
  return res;
}

static LispRef ff_stub_eul_init_keyword10175 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G0010220, res;

  POPVAL1(G0010220);
  FF_RES_CONVERT6(res,eul_init_keyword(FF_ARG_CONVERT8(G0010220)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module symbol */
void initialize_module_symbol()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_convert();
  initialize_module_compare();
  initialize_module_collect();
  initialize_module_character();
  initialize_module_string();
  initialize_module_table();
  initialize_module_fpi();
  eul_fast_table_set(eul_modules,"symbol",(LispRef) symbol_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_10218, sym_10217, sym_10216, sym_10215, G0010214, G0010211, G0010209, G0010207, sym_10205, sym_10204, sym_10202, sym_10200, sym_10199, sym_10198, sym_10197, sym_10196, G0010195, G0010193, G0010191, G0010189, G0010187, G0010185, sym_10183, G0010182, G0010180, key_10178, G0010177;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 7 is_init: 0 index: 10 binding: (method-(converter <symbol>)) */
  static const void *G0010176[] = {I(aa,24,00,00),B(mop_class ,6),I(23,00,00,00),B(symbol ,9),I(1d,24,00,00),B(mop_gf ,2),I(3d,03,01,00)};

  /* Byte-vector with size: 23 is_init: 0 index: 11 binding: anonymous */
  static const void *G0010179[] = {I(ab,1c,12,1b),I(34,00,00,00),I(00,00,00,28),I(47,00,00,04),I(1b,23,00,00),B(symbol ,9),I(1f,04,24,00),B(mop_gf ,2),I(3d,03,04,22),I(01,32,00,00),I(00,00,00,34),I(1d,11,1f,03),I(10,1b,24,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3c,02,1f,04),I(1c,24,00,00),B(string ,11),I(3c,02,1f,03),I(1c,47,00,01),I(3d,02,07,22),I(04,45,03,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 13 binding: (method-concatenate) */
  static const void *G0010181[] = {I(43,fe,46,02),I(1c,48,00,00),I(86,1b,48,00),I(01,23,00,00),B(symbol ,12),I(23,00,00,00),B(symbol ,11),I(3b,02,48,00),I(01,47,00,00),I(24,00,00,00),B(collect ,8),I(3c,01,1d,1c),I(47,00,01,3d),I(02,04,45,04)};

  /* Byte-vector with size: 6 is_init: 0 index: 14 binding: (method-binary<) */
  static const void *G0010184[] = {I(ab,1c,82,02),I(1c,82,02,1c),I(1c,41,00,00),B(string ,17),I(22,02,82,1a),I(45,04,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 15 binding: (method-namep) */
  static const void *G0010186[] = {I(aa,87,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 16 binding: (method-namep) */
  static const void *G0010188[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 17 binding: (method-initialize) */
  static const void *G0010190[] = {I(ab,1c,1c,37),I(02,2a,1c,41),B(symbol ,8),I(45,03,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 18 binding: (method-initialize) */
  static const void *G0010192[] = {I(ab,1c,1c,37),I(02,2a,1c,41),B(symbol ,7),I(45,03,00,00)};

  eul_allocate_static_cons(cons_10203, NULL, NULL);
  eul_allocate_static_cons(cons_10201, NULL, eul_as_static(cons_10203));
  /* Byte-vector with size: 256 is_init: 0 index: 28 binding: top-level */
  static const void *G0010194[] = {I(a9,82,89,00),B(symbol ,4),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(symbol ,19),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(symbol ,5),I(2a,24,00,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,6),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,20),I(23,00,00,00),B(symbol ,18),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,81),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,20),I(23,00,00,00),B(symbol ,17),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(symbol ,5),I(26,00,00,00),I(00,00,00,03),I(02,83,86,24),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(symbol ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,21),I(23,00,00,00),B(symbol ,16),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(symbol ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(symbol ,5),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,34),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(symbol ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,21),I(23,00,00,00),B(symbol ,15),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(symbol ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(symbol ,5),I(2a,24,00,00),B(compare ,6),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,6),I(24,00,00,00),B(mop_class ,6),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,22),I(23,00,00,00),B(symbol ,14),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,19),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,34),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(symbol ,23),I(23,00,00,00),B(symbol ,13),I(3b,fe,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,13),I(24,00,00,00),B(boot1 ,41),I(3c,01,83,24),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(symbol ,26),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(mop_class ,6),I(1c,1f,06,3c),I(02,2a,24,00),B(mop_class ,6),I(24,00,00,00),B(mop_class ,13),I(3c,01,2a,24),B(mop_class ,6),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(mop_class ,6),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(string ,13),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,6),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(symbol ,27),I(23,00,00,00),B(symbol ,10),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,36,45),I(36,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 29 binding: symbol-exists-p */
  static const void *G0010206[] = {I(aa,28,11,1b),I(1d,24,00,00),B(table ,3),I(3d,02,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 30 binding: keywordp */
  static const void *G0010208[] = {I(aa,04,24,00),B(mop_class ,81),I(50,45,00,00)};

  eul_allocate_static_string(str_10212, "G00", 3);
  /* Byte-vector with size: 20 is_init: 0 index: 32 binding: gensym */
  static const void *G0010210[] = {I(a8,1b,34,00),I(00,00,00,0e),I(1b,10,32,00),I(00,00,00,0e),I(23,00,00,00),B(symbol ,31),I(24,00,00,00),B(symbol ,4),I(41,00,00,00),B(fpi ,8),I(22,01,1c,1c),I(24,00,00,00),B(string ,11),I(3c,02,41,00),B(boot1 ,52),I(22,01,24,00),B(symbol ,4),I(2b,1b,89,00),B(symbol ,4),I(2a,1c,45,05)};

  /* Byte-vector with size: 70 is_init: 1 index: 0 binding: initialize-symbol */
  static const void *G0010213[] = {I(87,25,00,00),B(symbol ,1),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(table ,1),I(3e,0b,24,00),B(table ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(character ,1),I(3e,0b,24,00),B(character ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(symbol ,33),I(23,00,00,00),B(symbol ,32),I(3b,ff,25,00),B(symbol ,6),I(86,25,00,00),B(symbol ,5),I(86,25,00,00),B(symbol ,4),I(23,00,00,00),B(symbol ,34),I(23,00,00,00),B(symbol ,30),I(3b,01,25,00),B(symbol ,3),I(23,00,00,00),B(symbol ,35),I(23,00,00,00),B(symbol ,29),I(3b,01,25,00),B(symbol ,2),I(23,00,00,00),B(symbol ,36),I(23,00,00,00),B(symbol ,28),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_keyword(key_10178,"name");
  eul_allocate_bytevector( G0010177,G0010176);
  eul_allocate_bytevector( G0010180,G0010179);
  eul_intern_symbol(sym_10183,"anonymous");
  eul_allocate_bytevector( G0010182,G0010181);
  eul_allocate_bytevector( G0010185,G0010184);
  eul_allocate_bytevector( G0010187,G0010186);
  eul_allocate_bytevector( G0010189,G0010188);
  eul_allocate_bytevector( G0010191,G0010190);
  eul_allocate_bytevector( G0010193,G0010192);
  eul_intern_symbol(sym_10196,"namep");
  eul_intern_symbol(sym_10197,"(method initialize)");
  eul_intern_symbol(sym_10198,"(method namep)");
  eul_intern_symbol(sym_10199,"(method binary<)");
  eul_intern_symbol(sym_10200,"(method concatenate)");
  eul_intern_symbol(sym_10202,"converter");
  eul_intern_symbol(sym_10204,"<symbol>");
  object_class(cons_10203) = eul_static_cons_class;
  eul_car(cons_10203) = sym_10204;
  eul_cdr(cons_10203) = eul_nil;
  object_class(cons_10201) = eul_static_cons_class;
  eul_car(cons_10201) = sym_10202;
  eul_intern_symbol(sym_10205,"(method (converter <symbol>))");
  eul_allocate_bytevector( G0010195,G0010194);
  eul_allocate_bytevector( G0010207,G0010206);
  eul_allocate_bytevector( G0010209,G0010208);
  object_class(str_10212) = eul_static_string_class;
  eul_allocate_bytevector( G0010211,G0010210);
  eul_intern_symbol(sym_10215,"gensym");
  eul_intern_symbol(sym_10216,"keywordp");
  eul_intern_symbol(sym_10217,"symbol-exists-p");
  eul_intern_symbol(sym_10218,"top-level");
  eul_allocate_bytevector( G0010214,G0010213);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 7; i++)
      symbol_bindings[i] = eul_nil;
  }

  symbol_bindings[ 7] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_init_symbol10174;
  symbol_bindings[ 8] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_init_keyword10175;
  symbol_bindings[ 9] = key_10178;
  symbol_bindings[ 10] = G0010177;
  symbol_bindings[ 11] = G0010180;
  symbol_bindings[ 12] = sym_10183;
  symbol_bindings[ 13] = G0010182;
  symbol_bindings[ 14] = G0010185;
  symbol_bindings[ 15] = G0010187;
  symbol_bindings[ 16] = G0010189;
  symbol_bindings[ 17] = G0010191;
  symbol_bindings[ 18] = G0010193;
  symbol_bindings[ 19] = sym_10196;
  symbol_bindings[ 20] = sym_10197;
  symbol_bindings[ 21] = sym_10198;
  symbol_bindings[ 22] = sym_10199;
  symbol_bindings[ 23] = sym_10200;
  symbol_bindings[ 24] = sym_10202;
  symbol_bindings[ 25] = sym_10204;
  symbol_bindings[ 26] = cons_10201;
  symbol_bindings[ 27] = sym_10205;
  symbol_bindings[ 28] = G0010195;
  symbol_bindings[ 29] = G0010207;
  symbol_bindings[ 30] = G0010209;
  symbol_bindings[ 31] = str_10212;
  symbol_bindings[ 32] = G0010211;
  symbol_bindings[ 1] = eul_nil;
  symbol_bindings[ 33] = sym_10215;
  symbol_bindings[ 34] = sym_10216;
  symbol_bindings[ 35] = sym_10217;
  symbol_bindings[ 36] = sym_10218;
  eul_allocate_lambda( symbol_bindings[0], "initialize-symbol", 0, G0010214);

  }
}


/* eof */
