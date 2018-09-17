
/* A table: 
   table_population(t)==int
   table_threshold(t)==int
   table_values(t)=vector
   
   The hashing technique is an implementation of CACM 6/90, Pearson pp677-681

*/
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"
#include "modboot.h"

#include "calls.h"
#include "table.h"
#include "t2.h"


static LispObject Cb_std_tab_functions;

static void table_rehash(LispObject *, LispObject );

/* Hash table --- need 16 bit hash values */

unsigned char hash_table[]=
{ /* Bunch of numbers from CACM 6/90 -- extra 1 to avoid arith in hash fn */
  1, 87, 49, 12, 176, 178, 102, 166, 121, 193, 6, 84, 249, 230, 44, 163,
  14, 197, 213, 181, 161, 85, 218, 80, 64, 239, 24, 226, 236, 142, 38, 200,
  110, 177, 104, 103, 141, 253, 255, 50, 77, 101, 81, 18, 45, 96, 31, 222,
  25, 107, 190, 70, 86, 237, 240,  34, 72, 242, 20, 214, 244, 227, 149, 235,
  97, 234, 57, 22, 60, 250, 82, 175, 208, 5, 127, 199, 111, 62, 135, 248,
  174, 169, 211, 58, 66, 154, 106, 195, 245, 171, 17, 187, 182, 179, 0, 243,
  132, 56, 148, 75, 128, 133, 158, 100, 130, 126, 91, 13, 153, 246, 216, 219,
  119, 68, 223, 78, 83, 88, 201, 99, 122, 11, 92, 32, 136, 114, 52, 10,
  138, 30, 48, 183, 156, 35, 61, 26, 143, 74, 251, 94, 129, 162, 63, 152,
  170, 7, 115, 167, 241, 206, 3, 150, 55, 59, 151, 220, 90, 53, 23, 131,
  125, 173, 15, 238, 79, 95, 89, 16, 105, 137, 225, 224, 217, 160, 37, 123,
  118, 73, 2, 157, 46, 116, 9, 145, 134, 228, 207, 212, 202, 215, 69, 229,
  27, 188, 67, 124, 168, 252, 42, 4, 29, 108, 21, 247, 19, 205, 39, 203,
  233, 40, 186, 147, 198, 192, 155, 33, 164, 191, 98, 204, 165, 180, 117, 76,
  140, 36, 210, 172, 41, 54, 159, 8, 185, 232, 113, 196, 231, 47, 146, 120,
  51, 65, 28, 144, 254, 221, 93, 189, 194, 139, 112, 43, 71, 109, 184, 209,
  /* repeat to avoid taking mods. */
  1, 87
  };

/* Hash should be good enough that last dn bits are a hash value */
int hash(char *ptr)
{
  unsigned char h1=0,h2=0,h3=0;

  while (*ptr!='\0')
    { 
      h1 = hash_table[h1 ^ (*ptr)];
      h2 = hash_table[h2 ^ (*ptr+1)];
      h3 = hash_table[h3 ^ (*ptr+2)];
      ptr++;
    }
  return (((int) h1<<16) | ((int)h2<<8) | (int) h3);
}

#define total_hash(x) 	(is_symbol(x) ? ((x)->SYMBOL.hash) \
			 : is_fixnum(x) ? abs(intval(x)) \
			 : is_c_function(x) ? ((x)->C_FUNCTION.name->SYMBOL.hash) \
			 : general_hash(stacktop,x))
#define total_rehash(ohash) (is_symbol(x) ? (rehash_string(x)) : hash((char *) &ohash));

#define TBUG(x) 

EUFUN_1( Fn_tablep, x)
{
  if (is_table(x)) 
    return lisptrue;
  
  return nil;
}
EUFUN_CLOSE

EUFUN_1(Fn_symbol_hash, sym)
{
  return allocate_integer(stacktop,sym->SYMBOL.hash);
}
EUFUN_CLOSE

EUFUN_1(make_table, list)
{
  LispObject Fn_comparator_function(LispObject *);
  LispObject Fn_hash_function(LispObject *);

  LispObject table;

  if (list==NULL)
    list=nil;

  TBUG(fprintf(stderr,"Make table..\n"));
  table=allocate_instance(stacktop,Table);
  table_population(table)=allocate_integer(stacktop,0);
  table_threshold(table)=allocate_integer(stacktop,MIN_TABLE_SIZE-TABLE_FILL_FACTOR);
  table_fill(table)=nil;
/*
JAP/931116 if this slot is left unbound, operations on empty tables break.
nil is a cheap hack since the type test is for (not vectorp) in
(setter table-ref).
*/
  table_values(table)=nil;
  
  /* Check for fast table --- no delete, eq table, etc */
  /* All Internal tables are "fast" */
  /* NB: this is no longer true, since use of external 'fast' tables
     means we have to allow for delete */
  if (list!=nil && CAR(list)!=EUCALL_0(Fn_comparator_function))
    {
      table_comparator(table)=CAR(list);
      table_hash_fn(table)=EUCALL_0(Fn_hash_function);
    }
  else
    {
      table_comparator(table)=nil;
      table_hash_fn(table)=nil;
    }
  return table;
}
EUFUN_CLOSE

EUFUN_2(Fn_table_ref,tab,key)
{
  int hash,index,limit;
  LispObject vect,*ptr,*last_elt,hash_fn,tmp_hash;
  char wrapped;

  TBUG(fprintf(stderr,"In table-ref table: 0x%x key: 0x%x\n", table, key));

  vect=table_values(tab);
  if (!is_vector(vect))
    return table_fill(tab);

  limit=vector_length(table_values(tab));
  wrapped=0;

  if (table_comparator(tab)==nil) /* Comparator ? */
  {
      hash=total_hash(key);
      tab=ARG_0(stackbase);
      key=ARG_1(stackbase);
      vect=table_values(tab);
      last_elt=&vref(vect,limit);
  
      ptr = &vref(vect,hash%limit); /* only while loop is GC safe */
      
      while (1)
      {
	  if (ELTP(*ptr) && KEYOF(*ptr)==key)
	  {
	      return VALOF(*ptr);
	  }

	  if (wrapped && (*ptr==nil))
	  {
	      return table_fill(tab);
	  }
	  
	  ptr++;

	  if (ptr==last_elt)
	  {
	      ptr=&vref(vect,0);
	      wrapped=1;
	  }
      }
  }
  else
  {
      hash_fn=table_hash_fn(tab);
      if (hash_fn==nil)
      {
	  hash=total_hash(key)%limit;
      }
      else
      {
	  tmp_hash=EUCALL_2(apply1,hash_fn,key);
	  hash=intval(tmp_hash)%limit;
      }
      vect=table_values(ARG_0(stackbase));

      while (1)
	{
	  LispObject tmp;

	  if (hash==limit)
	  {
	      hash=0;
	      wrapped=1;
	  }

	  if (wrapped && (vref(vect,hash)==nil))
	  { 
	    return table_fill(ARG_0(stackbase));
	  }

	  STACK_TMP(vect); /* stack here as EUCALL may cause GC */
	  if (ELTP(vref(vect,hash))
	      && (tmp=EUCALL_3(apply2,
			       table_comparator(ARG_0(stackbase)),
			       ARG_1(stackbase),
			       KEYOF(vref(vect,hash))),
		  tmp==lisptrue))
	  {	
	      UNSTACK_TMP(vect); /* unstack before return */
	      return VALOF(vref(vect,hash));
	  }
	  UNSTACK_TMP(vect); /* or unstack before going around again */
	  hash++;
      }
  }
  CallError(stacktop,"Impossible to get here",nil,NONCONTINUABLE);
  return nil;
}
EUFUN_CLOSE

EUFUN_3(Fn_table_ref_setter,tab,key,value)
{
  LispObject hash_fn,tmp_hash,vect;
  int hash,limit;

  if (!is_vector(table_values(tab)))
    {
      vect=allocate_vector(stacktop,MIN_TABLE_SIZE);
      tab=ARG_0(stackbase);
      table_values(tab)=vect;
    }
  
  limit=vector_length(table_values(tab));

  if (table_comparator(tab)==nil)
    {
      hash=total_hash(key)%limit;
      tab=ARG_0(stackbase);
      key=ARG_1(stackbase);
      value=ARG_2(stackbase);
      vect=table_values(tab);

      while (1)
	{
	  if (!ELTP(vref(vect,hash)))
	    {			/* XXX: GC proof */
	      LispObject new;
	    
	      STACK_TMP(vect);
	      new=EUCALL_2(Fn_cons,key,value);
	      UNSTACK_TMP(vect);
	      vref(vect,hash)=new;
	  
	      new=allocate_integer(stacktop,intval(table_population(ARG_0(stackbase)))+1);
	      table_population(ARG_0(stackbase))=new;

	      if (intval(table_population(ARG_0(stackbase)))==intval(table_threshold(ARG_0(stackbase))))
		table_rehash(stacktop,ARG_0(stackbase));

	      return table_fill(ARG_0(stackbase));
	    }
      
	  if (KEYOF(vref(vect,hash))==key) /* XXX: Insert comparator here */
	    {
	      LispObject old;
      
	      old=VALOF(vref(vect,hash));
	      VALOF(vref(vect,hash))=value;

	      return old;
	    }	
	  hash++;
	  if (hash==limit)
	    hash=0;	
	}
    }
  else
    {
      hash_fn=table_hash_fn(tab);
      if (hash_fn==nil)
      {
	  hash=total_hash(key)%limit;
      }
      else
      {
	  tmp_hash=EUCALL_2(apply1,hash_fn,key);
	  hash=intval(tmp_hash)%limit;
      }
      tab=ARG_0(stackbase);
      vect=table_values(tab);

      while (1)
	{	
	  LispObject tmp;

	  if (!ELTP(vref(vect,hash)))
	    {			/* XXX: GC proof */
	      LispObject new;
	    
	      STACK_TMP(vect);
	      new=EUCALL_2(Fn_cons,key,value);
	      UNSTACK_TMP(vect);
	      vref(vect,hash)=new;
	      
	      new=allocate_integer(stacktop,intval(table_population(ARG_0(stackbase)))+1);
	      tab=ARG_0(stackbase);
	      table_population(tab)=new;

	      if (intval(table_population(ARG_0(stackbase)))==intval(table_threshold(ARG_0(stackbase))))
		table_rehash(stacktop,ARG_0(stackbase));

	      return table_fill(ARG_0(stackbase));
	    }

	  STACK_TMP(vect);
	  tmp=EUCALL_3(apply2,table_comparator(ARG_0(stackbase)),KEYOF(vref(vect,hash)),ARG_1(stackbase));
	  if (tmp!=nil)
	    {
		LispObject old;

		UNSTACK_TMP(vect);
		old=VALOF(vref(vect,hash));
		VALOF(vref(vect,hash))=ARG_2(stackbase);

		return old;
	      }	
	  UNSTACK_TMP(vect);
	  hash++;
	  if (hash==limit)
	    hash=0;	
	}
    }
}
EUFUN_CLOSE

EUFUN_2(Fn_table_delete,tab,key)
{
    LispObject hash_fn,tmp_hash,vect,*ptr,*last_elt;
    int hash,limit;
    char wrapped;

    if (!is_vector(table_values(tab)))
    {
	return nil;
    }
    
    limit=vector_length(table_values(tab));
    wrapped=0;
    
    if (table_comparator(tab)==nil) /* Comparator ? */
    {
	hash=total_hash(key);
	tab=ARG_0(stackbase);
	key=ARG_1(stackbase);
	vect=table_values(tab);
	last_elt=&vref(vect,limit);

	ptr = &vref(vect,hash%limit); /* only while loop is GC safe */
	
	while (1)
	{
	    if (ELTP(*ptr) && KEYOF(*ptr)==key)
	    {
		LispObject old,new;
		    
		old=VALOF(*ptr);
		*ptr=nil;

		STACK_TMP(old);
		table_population(ARG_0(stackbase))=allocate_integer(stacktop,intval(table_population(ARG_0(stackbase)))-1);
		UNSTACK_TMP(old);
		
		return old;
	    }

	    if (wrapped && (*ptr==nil))
	    {
		return nil;
	    }
	    
	    ptr++;

	    if (ptr==last_elt)
	    {
		ptr=&vref(vect,0);
		wrapped=1;
	    }
	}
    }
    else
    {
	hash_fn=table_hash_fn(tab);
	if (hash_fn==nil)
	{
	    hash=total_hash(key)%limit;
	}
	else
	{
	    tmp_hash=EUCALL_2(apply1,hash_fn,key);
	    hash=intval(tmp_hash)%limit;
	}
	vect=table_values(ARG_0(stackbase));
	
	while (1)
	{	
	    LispObject tmp;

	    if (hash==limit)
	    {
		hash=0;
		wrapped=1;
	    }

	    if (wrapped && (vref(vect,hash)==nil))
	    {
		return nil;
	    }

	    STACK_TMP(vect);
	    if (ELTP(vref(vect,hash))
		&& (tmp=EUCALL_3(apply2,
				 table_comparator(ARG_0(stackbase)),
				 ARG_1(stackbase),
				 KEYOF(vref(vect,hash))),
		    tmp==lisptrue))
	    {
		LispObject old,new;

		UNSTACK_TMP(vect);
		old=VALOF(vref(vect,hash));
		vref(vect,hash)=nil;
		    
		new=allocate_integer(stacktop,intval(table_population(ARG_0(stackbase)))-1);
		table_population(ARG_0(stackbase))=new;
		return old;
	    }
	    UNSTACK_TMP(vect);
	    hash++;
	}
    }
}
EUFUN_CLOSE

static void table_rehash(LispObject *stacktop, LispObject tab)
{
  LispObject *stackbase=stacktop;
  LispObject oldvect, newvect,hash_fn;
  int newsize;
  int hashval;
  int i;
  
  stacktop++;
  newsize=vector_length(table_values(tab))*2;
  STACK_TMP(tab);
  table_threshold(tab)=allocate_integer(stacktop,intval(table_threshold(tab))*2);
  UNSTACK_TMP(tab) ; STACK_TMP(tab);
  newvect=allocate_vector(stacktop,newsize);
  UNSTACK_TMP(tab);
  oldvect=table_values(tab);

  hash_fn=table_hash_fn(tab);
  if (hash_fn==nil)
  {
      hash_fn=NULL;
  }      
  ARG_0(stackbase)=hash_fn;
  STACK_TMP(tab);
  for (i=0 ; i<vector_length(oldvect) ; i++)
    {
      if (ELTP(vref(oldvect,i)))
	{
	  LispObject new;
	  
	  STACK_TMP(oldvect);
	  STACK_TMP(newvect);

	  if (ARG_0(stackbase))
	    hashval=intval(EUCALL_2(apply1,ARG_0(stackbase),KEYOF(vref(oldvect,i))));
	  else
	    hashval=total_hash(KEYOF(vref(oldvect,i)));
	  UNSTACK_TMP(newvect);
	  UNSTACK_TMP(oldvect);
	  
	  new=vref(oldvect,i);

	  while (vref(newvect,hashval%newsize)!=nil)
	    hashval++;
	  
	  vref(newvect,hashval%newsize) = new;
	}
    }
  UNSTACK_TMP(tab);
  table_values(tab)=newvect;
  
  
}

/* Other minor functions */
/* NB: map-table and copy-table implemented by table.em */
/* This would be *so* nice with generators/iterators */
EUFUN_1(Fn_table_parameters,table)
{
  LispObject plist;
  int tlen,i;
  
  if (!is_vector(table_values(table)))
    return nil;

  tlen=vector_length(table_values(table));
  plist=nil;
  for (i=0 ; i<tlen ; i++)
    {
      LispObject elt;

      elt=vref(table_values(ARG_0(stackbase)),i);
      
      if (elt!=nil)
	plist=EUCALL_2(Fn_cons,elt,plist);
    }
  return plist;
}
EUFUN_CLOSE

EUFUN_1(Fn_table_values,table)
{
  return table_values(table);
}
EUFUN_CLOSE

EUFUN_0(Fn_hash_function)
{
  return CAR(Cb_std_tab_functions);
}
EUFUN_CLOSE

EUFUN_0(Fn_comparator_function)
{
  return CDR(Cb_std_tab_functions);
}
EUFUN_CLOSE

EUFUN_2(Fn_set_std_tab_functions,hash,comp)
{
  CAR(Cb_std_tab_functions)=hash;
  CDR(Cb_std_tab_functions)=comp;
  return lisptrue;
}
EUFUN_CLOSE

EUFUN_1(Fn_table_comparator,tab)
{
  if (table_comparator(tab)==nil)
    return CDR(Cb_std_tab_functions);
  else
    return table_comparator(tab);
}
EUFUN_CLOSE

EUFUN_1(Fn_table_hash_function,tab)
{
  if (table_hash_fn(tab)==nil)
    return CAR(Cb_std_tab_functions);
  else
    return table_hash_fn(tab);

}
EUFUN_CLOSE
int general_hash(LispObject *stacktop,LispObject x)
{

  x=EUCALL_2(apply1,CAR(Cb_std_tab_functions),x);
  
  return intval(x);
}

/* Initialisation */

#define TABLES_ENTRIES 8
MODULE Module_tables;
LispObject Module_tables_values[TABLES_ENTRIES];

void initialise_tables(LispObject *stacktop)
{
  LispObject fun, upd;

  Cb_std_tab_functions=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_std_tab_functions);
  open_module(stacktop,	
	      &Module_tables,
	      Module_tables_values,
	      "tables",
	      TABLES_ENTRIES);

  (void) make_module_function(stacktop,"symbol-hash",Fn_symbol_hash,1);
  (void) make_module_function(stacktop,"make-table",make_table,-1);
  (void) make_module_function(stacktop,"table-parameters",Fn_table_parameters,1);
  fun = make_module_function(stacktop,"sys-table-ref",Fn_table_ref,2);
  STACK_TMP(fun);
  upd = make_unexported_module_function(stacktop,"sys-table-ref-setter", Fn_table_ref_setter, 3);
  UNSTACK_TMP(fun);
  set_anon_associate(stacktop,fun, upd);
  (void) make_module_function(stacktop,"standard-hash-function",Fn_hash_function,0);
  (void) make_module_function(stacktop,"set-standard-tab-functions",Fn_set_std_tab_functions,2);
  (void) make_module_function(stacktop,"table-delete",Fn_table_delete,2);

  close_module();
}

