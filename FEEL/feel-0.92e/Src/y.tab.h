typedef union{
    LispObject lspobj;
      } YYSTYPE;
#define	IDENTIFIER	258
#define	TINTEGER	259
#define	TRATIONAL	260
#define	TFLOAT	261
#define	TCHAR	262
#define	TSTRING	263
#define	QUOTATION	264
#define	UNQUOTATION	265
#define	UNQUOTE_SPLICE	266
#define	ANTI_QUOTATION	267
#define	EXTENSION	268
#define	PAIR_BEGIN	269
#define	PAIR_END	270
#define	DOT	271
#define	END_OF_STREAM	272


extern YYSTYPE yylval;
