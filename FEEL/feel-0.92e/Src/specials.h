extern LispObject my_make_special(LispObject*,char *,LispObject (*)());

extern EUDECL( Sf_lambda);
extern EUDECL( Sf_mlambda);
extern EUDECL( Sf_setq);
extern EUDECL( Sf_progn);
extern EUDECL( Sf_if);

extern EUDECL( Sf_letcc);
extern EUDECL( Sf_unwind_protect);
extern EUDECL( Sf_with_handler);

extern LispObject special_macro_lambda;
extern LispObject special_setq, special_lambda, special_progn, special_if;
extern LispObject special_letcc,special_unwind_protect,special_with_handler;

extern void call_continuation(LispObject*,LispObject,LispObject);

extern void initialise_specials(LispObject*);

extern LispObject returned_continue_value;
extern LispObject last_continue;

extern EUDECL( Sf_dynamic_let);
extern EUDECL( Sf_dynamic_setq);
extern EUDECL( Sf_dynamic_set);

extern LispObject special_dynamic,special_dynamic_let,special_dynamic_setq;
extern LispObject special_dynamic_set;

extern EUDECL(Sf_quote);

extern LispObject special_quote;

extern LispObject special_evalcm;

extern LispObject special_tagbody;

extern EUDECL( Fn_special_form_p);
extern EUDECL( Fn_dynamic);
extern EUDECL( Fn_dynamic_setq);

extern LispObject special_table;
