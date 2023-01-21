#include "config.h"
#include "orbit-idl-c-backend.h"

#include <string.h>
#include <ctype.h>

/* #define DO_CPP */

/* ch = C header */
static void ch_output_types(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_poa(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_protos(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);

void
orbit_idl_output_c_headers(OIDL_Output_Tree *tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  fprintf(ci->fh, "/*\n"
	  " * This file was generated by orbit-idl - DO NOT EDIT!\n"
	  " */\n\n");
  fprintf(ci->fh, "#include <glib.h>\n");
  fprintf(ci->fh, "#define ORBIT_IDL_SERIAL %d\n", ORBIT_SERIAL);
  fprintf(ci->fh, "#include <orb/orbit.h>\n\n");

  fprintf(ci->fh, "#ifndef %s_H\n", ci->c_base_name);
  fprintf(ci->fh, "#define %s_H 1\n", ci->c_base_name);

  fprintf(ci->fh, "#ifdef __cplusplus\n");
  fprintf(ci->fh, "extern \"C\" {\n");
  fprintf(ci->fh, "#endif /* __cplusplus */\n\n");

  /* Do all the typedefs, etc. */
  fprintf(ci->fh, "\n/** typedefs **/\n");
  ch_output_types(tree->tree, rinfo, ci);
  /* Do all the POA structures, etc. */
  fprintf(ci->fh, "\n/** POA structures **/\n");
  ch_output_poa(tree->tree, rinfo, ci);
  /* Do all the stub prototypes */
  fprintf(ci->fh, "\n/** prototypes **/\n");
  ch_output_protos(tree->tree, rinfo, ci);

  fprintf(ci->fh, "#ifdef __cplusplus\n");
  fprintf(ci->fh, "}\n");
  fprintf(ci->fh, "#endif /* __cplusplus */\n\n");

  fprintf(ci->fh, "#endif\n");

  fprintf(ci->fh, "#undef ORBIT_IDL_SERIAL\n");
}

static void
ch_output_var(IDL_tree val, IDL_tree name, OIDL_C_Info *ci)
{
  orbit_cbe_write_typespec(ci->fh, val);

  fprintf(ci->fh, " ");
  switch(IDL_NODE_TYPE(name)) {
  case IDLN_IDENT:
    fprintf(ci->fh, "%s", IDL_IDENT(name).str);
    break;
  case IDLN_TYPE_ARRAY:
    {
      IDL_tree curitem;

      fprintf(ci->fh, "%s", IDL_IDENT(IDL_TYPE_ARRAY(name).ident).str);
      for(curitem = IDL_TYPE_ARRAY(name).size_list; curitem; curitem = IDL_LIST(curitem).next) {
	fprintf(ci->fh, "[%" IDL_LL "d]", IDL_INTEGER(IDL_LIST(curitem).data).value);
      }
    }
    break;
  default:
    g_error("Weird varname - %s", IDL_tree_type_names[IDL_NODE_TYPE(name)]);
    break;
  }
  fprintf(ci->fh, ";\n");
}

static void ch_output_type_struct(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_type_enum(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_type_dcl(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_type_union(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_codefrag(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_output_const_dcl(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);
static void ch_prep(IDL_tree tree, OIDL_C_Info *ci);
static void ch_type_alloc_and_tc(IDL_tree tree, OIDL_C_Info *ci, gboolean do_alloc);

static void
ch_output_types(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *fullname;

  if(!tree) return;

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_EXCEPT_DCL:
    {
      char *id;
      id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_EXCEPT_DCL(tree).ident), "_", 0);
      fprintf(ci->fh, "#define ex_%s \"%s\"\n", id, IDL_IDENT(IDL_EXCEPT_DCL(tree).ident).repo_id);
      fprintf(ci->fh, "void _ORBIT_%s_demarshal(GIOPRecvBuffer *_ORBIT_recv_buffer, CORBA_Environment *ev);\n", id);
      fprintf(ci->fh, "void _ORBIT_%s_marshal(GIOPSendBuffer *_ORBIT_send_buffer, CORBA_Environment *ev);\n", id);
      g_free(id);
      ch_output_type_struct(tree, rinfo, ci);
    }
    break;
  case IDLN_FORWARD_DCL:
  case IDLN_INTERFACE:
    fullname = orbit_cbe_get_typename(tree);
    fprintf(ci->fh, "#if !defined(ORBIT_DECL_%s) && !defined(_%s_defined)\n#define ORBIT_DECL_%s 1\n#define _%s_defined 1\n", fullname, fullname, fullname, fullname);

    fprintf(ci->fh, "#define %s__free CORBA_Object__free\n", fullname);
    fprintf(ci->fh, "typedef CORBA_Object %s;\n", fullname);
    fprintf(ci->fh, "extern CORBA_unsigned_long %s__classid;\n", fullname);
    ch_type_alloc_and_tc(tree, ci, FALSE);

    fprintf(ci->fh, "#endif\n");
    g_free(fullname);
    break;
  case IDLN_TYPE_STRUCT:
    ch_output_type_struct(tree, rinfo, ci);
    break;
  case IDLN_TYPE_ENUM:
    ch_output_type_enum(tree, rinfo, ci);
    break;
  case IDLN_TYPE_DCL:
    ch_output_type_dcl(tree, rinfo, ci);
    break;
  case IDLN_TYPE_UNION:
    ch_output_type_union(tree, rinfo, ci);
    break;
  case IDLN_CODEFRAG:
    ch_output_codefrag(tree, rinfo, ci);
    break;
  case IDLN_CONST_DCL:
    ch_output_const_dcl(tree, rinfo, ci);
    break;
  default:
    break;
  }

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MODULE:
    ch_output_types(IDL_MODULE(tree).definition_list, rinfo, ci);
    return;
    break;
  case IDLN_LIST:
    {
      IDL_tree sub;

      for(sub = tree; sub; sub = IDL_LIST(sub).next) {
	ch_output_types(IDL_LIST(sub).data, rinfo, ci);
      }
    }
    return;
    break;
  case IDLN_INTERFACE:
    ch_output_types(IDL_INTERFACE(tree).body, rinfo, ci);
    break;
  default:
    break;
  }
}

static void
ch_output_type_enum(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  IDL_tree curitem;
  char *id, *enumid;

  /* CORBA spec says to do
	   typedef unsigned int enum_name;
     and then #defines for each enumerator.
     This works just as well and seems cleaner.
  */

  enumid = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_TYPE_ENUM(tree).ident), "_", 0);
  fprintf(ci->fh, "#if !defined(_%s_defined)\n#define _%s_defined 1\n", enumid, enumid);
  fprintf(ci->fh, "typedef enum {\n");

  for(curitem = IDL_TYPE_ENUM(tree).enumerator_list;
      curitem;
      curitem = IDL_LIST(curitem).next) {
    id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_LIST(curitem).data), "_", 0);

    fprintf(ci->fh, "  %s%s\n",
	    id,
	    IDL_LIST(curitem).next?",":"");

    g_free(id);
  }

  fprintf(ci->fh, "} %s;\n", enumid);

  ch_type_alloc_and_tc(tree, ci, FALSE);

  fprintf(ci->fh, "#endif\n");

  g_free(enumid);
}


static void
ch_output_type_struct(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *id;
  IDL_tree cur, curmem;

  /* First, scan for any "weird sequences" */
  for(cur = IDL_TYPE_STRUCT(tree).member_list; cur; cur = IDL_LIST(cur).next) {
    IDL_tree ts;

    ts = IDL_MEMBER(IDL_LIST(cur).data).type_spec;

    ch_prep(ts, ci);
  }

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_TYPE_STRUCT(tree).ident), "_", 0);
  fprintf(ci->fh, "#if !defined(_%s_defined)\n#define _%s_defined 1\n", id, id);
  fprintf(ci->fh, "typedef struct {\n");

  for(cur = IDL_TYPE_STRUCT(tree).member_list; cur; cur = IDL_LIST(cur).next) {
    for(curmem = IDL_MEMBER(IDL_LIST(cur).data).dcls; curmem; curmem = IDL_LIST(curmem).next) {
      ch_output_var(IDL_MEMBER(IDL_LIST(cur).data).type_spec, IDL_LIST(curmem).data, ci);
    }
  }
  if(!IDL_TYPE_STRUCT(tree).member_list)
    fprintf(ci->fh, "int dummy;\n");
  fprintf(ci->fh, "} %s;\n\n", id);

  ch_type_alloc_and_tc(tree, ci, TRUE);

  fprintf(ci->fh, "#endif\n");

  g_free(id);
}

static void
ch_output_type_dcl(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *ctmp;
  IDL_tree curitem, ent, sub;

  ch_prep(IDL_TYPE_DCL(tree).type_spec, ci);

  for(curitem = IDL_TYPE_DCL(tree).dcls; curitem; curitem = IDL_LIST(curitem).next) {
    ent = IDL_LIST(curitem).data;

    switch(IDL_NODE_TYPE(ent)) {
    case IDLN_IDENT:
      ctmp = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(ent), "_", 0);
      break;
    case IDLN_TYPE_ARRAY:
      ctmp = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_TYPE_ARRAY(ent).ident), "_", 0);
      break;
    default:
      g_error("Huh?");
      break;
    }
    fprintf(ci->fh, "#if !defined(_%s_defined)\n#define _%s_defined 1\n", ctmp, ctmp);
    fprintf(ci->fh, "typedef ");
    orbit_cbe_write_typespec(ci->fh, IDL_TYPE_DCL(tree).type_spec);

    switch(IDL_NODE_TYPE(ent)) {
    case IDLN_IDENT:
      {
	fprintf(ci->fh, " %s;\n", ctmp);
      }
      break;
    case IDLN_TYPE_ARRAY:
      {
	fprintf(ci->fh, " %s", ctmp);
	for(sub = IDL_TYPE_ARRAY(ent).size_list; sub; sub = IDL_LIST(sub).next)
	  fprintf(ci->fh, "[%" IDL_LL "d]", IDL_INTEGER(IDL_LIST(sub).data).value);
	fprintf(ci->fh, ";\n");
	fprintf(ci->fh, "typedef ");
	orbit_cbe_write_typespec(ci->fh, IDL_TYPE_DCL(tree).type_spec);
	fprintf(ci->fh, " %s_slice", ctmp);
	for(sub = IDL_LIST(IDL_TYPE_ARRAY(ent).size_list).next; sub; sub = IDL_LIST(sub).next)
	  fprintf(ci->fh, "[%" IDL_LL "d]", IDL_INTEGER(IDL_LIST(sub).data).value);
	fprintf(ci->fh, ";\n");
      }
      break;
    default:
      break;
    }

    ch_type_alloc_and_tc(ent, ci, TRUE);
    fprintf(ci->fh, "#endif\n");
    g_free(ctmp);
  }
}

static void
ch_output_type_union(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *id;
  IDL_tree curitem;

  for(curitem = IDL_TYPE_UNION(tree).switch_body; curitem; curitem = IDL_LIST(curitem).next) {
    IDL_tree member;

    member = IDL_CASE_STMT(IDL_LIST(curitem).data).element_spec;

    ch_prep(IDL_MEMBER(member).type_spec, ci);
  }

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_TYPE_UNION(tree).ident), "_", 0);
  fprintf(ci->fh, "#if !defined(_%s_defined)\n#define _%s_defined 1\n", id, id);
  fprintf(ci->fh, "typedef struct {\n");
  orbit_cbe_write_typespec(ci->fh, IDL_TYPE_UNION(tree).switch_type_spec);
  fprintf(ci->fh, " _d;\nunion {\n");

  for(curitem = IDL_TYPE_UNION(tree).switch_body; curitem; curitem = IDL_LIST(curitem).next) {
    IDL_tree member;

    member = IDL_CASE_STMT(IDL_LIST(curitem).data).element_spec;
    ch_output_var(IDL_MEMBER(member).type_spec,
		  IDL_LIST(IDL_MEMBER(member).dcls).data,
		  ci);
  }

  fprintf(ci->fh, "} _u;\n} %s;\n", id);

  ch_type_alloc_and_tc(tree, ci, TRUE);

  fprintf(ci->fh, "#endif\n");

  g_free(id);
}

static const char token_pragma[]="pragma";
static const char token_include_defs[]="include_defs";

static void ch_output_codefrag(IDL_tree tree, OIDL_Run_Info * rinfo, OIDL_C_Info * ci)
{
  GSList *list;

  for (list = IDL_CODEFRAG(tree).lines; list; list = g_slist_next(list)) {
    int pragmatic = 0;

    if (*(char *) list->data == '#') {
      char *ctmp;
      ctmp = (char *) list->data + 1;
      /* eat blanks between '#' and 'pragma' */
      while (isspace(*ctmp)) ctmp++;
      if (!strncmp(ctmp, token_pragma, sizeof(token_pragma) - 1)) {
        ctmp += sizeof(token_pragma) - 1;
        /* eat blanks between 'pragma' and next token */
        while (isspace(*ctmp)) ctmp++;
        if (!strncmp(ctmp, token_include_defs, sizeof(token_include_defs) - 1)) {
          char *cte;
          ctmp += sizeof(token_include_defs);
          while (*ctmp && (isspace(*ctmp) || *ctmp == '"')) ctmp++;
          cte = ctmp;
          while (*cte && !isspace(*cte) && *cte != '"') cte++;
          *cte = '\0';
          fprintf(ci->fh, "#include <%s>\n", ctmp);
          pragmatic = 1;
        }
      }
    }
    if (!pragmatic)
      fprintf(ci->fh, "%s\n", (char *) list->data);
  }
}

static void
ch_output_const_dcl(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *id;

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_CONST_DCL(tree).ident), "_", 0);
  fprintf(ci->fh, "#ifndef %s\n", id);
  fprintf(ci->fh, "#define %s ", id);

  orbit_cbe_write_const(ci->fh,
			IDL_CONST_DCL(tree).const_exp);
  fprintf(ci->fh, "\n");
  fprintf(ci->fh, "#endif /* !%s */\n\n", id);

  g_free(id);
}

static void ch_prep_sequence(IDL_tree tree, OIDL_C_Info *ci);
static void ch_prep_fixed(IDL_tree tree, OIDL_C_Info *ci);

static
void ch_prep(IDL_tree tree, OIDL_C_Info *ci)
{
  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_TYPE_SEQUENCE:
    ch_prep_sequence(tree, ci);
    break;
  case IDLN_TYPE_FIXED:
    ch_prep_fixed(tree, ci);
    break;
  default:
    break;
  }
}

static void
ch_prep_fixed(IDL_tree tree, OIDL_C_Info *ci)
{
  char *ctmp;

  ctmp = orbit_cbe_get_typename(tree);
  fprintf(ci->fh,
	  "typedef struct { CORBA_unsigned_short _digits; CORBA_short _scale; CORBA_char _value[%d]; } %s;\n",
	  (int) (IDL_INTEGER(IDL_TYPE_FIXED(tree).positive_int_const).value + 2)/2,
	  ctmp);
  g_free(ctmp);

  ch_type_alloc_and_tc(tree, ci, TRUE);
}

static void
ch_prep_sequence(IDL_tree tree, OIDL_C_Info *ci)
{
  char *ctmp, *fullname;

  fullname = orbit_cbe_get_typename(tree);

  fprintf(ci->fh, "#if !defined(ORBIT_DECL_%s) && !defined(_%s_defined)\n#define ORBIT_DECL_%s 1\n#define _%s_defined 1\n",
	  fullname, fullname, fullname, fullname);
  orbit_cbe_id_define_hack(ci->fh, "ORBIT_IMPL", fullname, ci->c_base_name);

  if(IDL_NODE_TYPE(IDL_TYPE_SEQUENCE(tree).simple_type_spec) == IDLN_TYPE_SEQUENCE)
    ch_prep_sequence(IDL_TYPE_SEQUENCE(tree).simple_type_spec, ci);

  fprintf(ci->fh, "typedef struct { CORBA_unsigned_long _maximum, _length; ");
  orbit_cbe_write_typespec(ci->fh, IDL_TYPE_SEQUENCE(tree).simple_type_spec);
  fprintf(ci->fh, "* _buffer; CORBA_boolean _release; } ");
  orbit_cbe_write_typespec(ci->fh, tree);
  fprintf(ci->fh, ";\n");


  ctmp = orbit_cbe_get_typename(IDL_TYPE_SEQUENCE(tree).simple_type_spec);
  orbit_cbe_write_typespec(ci->fh, IDL_TYPE_SEQUENCE(tree).simple_type_spec);
  fprintf(ci->fh, " *CORBA_sequence_%s_allocbuf(CORBA_unsigned_long len);\n",
	  orbit_cbe_type_is_builtin(IDL_TYPE_SEQUENCE(tree).simple_type_spec)?(ctmp+strlen("CORBA_")):ctmp);

  fprintf(ci->fh, "#endif\n");

  ch_type_alloc_and_tc(tree, ci, TRUE);

  g_free(ctmp);

  g_free(fullname);
}

static void
ch_type_alloc_and_tc(IDL_tree tree, OIDL_C_Info *ci, gboolean do_alloc)
{
  char *ctmp;
  IDL_tree tts;

  ctmp = orbit_cbe_get_typename(tree);

  fprintf(ci->fh, "#if !defined(TC_IMPL_TC_%s_0)\n", ctmp);
  orbit_cbe_id_define_hack(ci->fh, "TC_IMPL_TC", ctmp, ci->c_base_name);
  fprintf(ci->fh, "extern const struct CORBA_TypeCode_struct TC_%s_struct;\n", ctmp);
  fprintf(ci->fh, "#define TC_%s ((CORBA_TypeCode)&TC_%s_struct)\n", ctmp, ctmp);
  fprintf(ci->fh, "#endif\n");

  if(do_alloc) {
    tts = orbit_cbe_get_typespec(tree);

    if((IDL_NODE_TYPE(tts) != IDLN_TYPE_FLOAT)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_INTEGER)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_BOOLEAN)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_CHAR)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_WIDE_CHAR)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_OCTET)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_ENUM)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_STRING)
       && (IDL_NODE_TYPE(tts) != IDLN_TYPE_WIDE_STRING)) {
      if((IDL_NODE_TYPE(tts) == IDLN_EXCEPT_DCL
	 || IDL_NODE_TYPE(tts) == IDLN_TYPE_STRUCT)
	 && !IDL_TYPE_STRUCT(tts).member_list) {
	fprintf(ci->fh, "#define %s__alloc() NULL\n", ctmp);
      } else {
	fprintf(ci->fh, "extern %s%s* %s__alloc(void);\n", ctmp,
		(IDL_NODE_TYPE(tree) == IDLN_TYPE_ARRAY)?"_slice":"",
		ctmp);
      }
      fprintf(ci->fh,
	      "extern gpointer %s__free(gpointer mem, gpointer dat, CORBA_boolean free_strings); /* ORBit internal use */\n",
	      ctmp);
    }
    if((IDL_NODE_TYPE(tts) == IDLN_TYPE_STRING)
       || (IDL_NODE_TYPE(tts) == IDLN_TYPE_WIDE_STRING))
      fprintf(ci->fh, "#define %s__free CORBA_string__free\n", ctmp);
  }

  g_free(ctmp);
}

/************************/
static void
cbe_header_interface_print_vepv(IDL_tree node, FILE *of)
{
  char *id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(node).ident),
				     "_", 0);
  fprintf(of, "  POA_%s__epv *%s_epv;\n", id, id);
  g_free(id);

}

static void
ch_output_poa(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  if(!tree) return;

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MODULE:
    ch_output_poa(IDL_MODULE(tree).definition_list, rinfo, ci);
    break;
  case IDLN_LIST:
    {
      IDL_tree sub;

      for(sub = tree; sub; sub = IDL_LIST(sub).next) {
	ch_output_poa(IDL_LIST(sub).data, rinfo, ci);
      }
    }
    break;
  case IDLN_INTERFACE:
    {
      IDL_tree sub;
      char *id;

      id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(tree).ident), "_", 0);

      /* First, do epv for this interface, then vepv, then servant */
      fprintf(ci->fh, "typedef struct {\n");
      fprintf(ci->fh, "  void *_private;\n");
      for(sub = IDL_INTERFACE(tree).body; sub; sub = IDL_LIST(sub).next) {
	IDL_tree cur;

	cur = IDL_LIST(sub).data;

	switch(IDL_NODE_TYPE(cur)) {
	case IDLN_OP_DCL:
	  orbit_cbe_op_write_proto(ci->fh, cur, "", TRUE);
	  fprintf(ci->fh, ";\n");
	  break;
	case IDLN_ATTR_DCL:
	  {
	    OIDL_Attr_Info *ai;
	    IDL_tree curitem;

	    for(curitem = IDL_ATTR_DCL(cur).simple_declarations; curitem; curitem = IDL_LIST(curitem).next) {
	      ai = IDL_LIST(curitem).data->data;
	      
	      orbit_cbe_op_write_proto(ci->fh, ai->op1, "", TRUE);
	      fprintf(ci->fh, ";\n");
	      
	      if(ai->op2) {
		orbit_cbe_op_write_proto(ci->fh, ai->op2, "", TRUE);
		fprintf(ci->fh, ";\n");
	      }
	    }
	  }
	  break;
	default:
	  break;
	}
      }

      fprintf(ci->fh, "} POA_%s__epv;\n", id);

      fprintf(ci->fh, "typedef struct {\n");
      fprintf(ci->fh, "  PortableServer_ServantBase__epv *_base_epv;\n");
      IDL_tree_traverse_parents(tree, (GFunc)cbe_header_interface_print_vepv, ci->fh);
      fprintf(ci->fh, "} POA_%s__vepv;\n", id);

      fprintf(ci->fh, "typedef struct {\n");
      fprintf(ci->fh, "  void *_private;\n");
      fprintf(ci->fh, "  POA_%s__vepv *vepv;\n", id);
      fprintf(ci->fh, "} POA_%s;\n", id);

      fprintf(ci->fh,
	      "extern void POA_%s__init(PortableServer_Servant servant, CORBA_Environment *ev);\n", id);
      fprintf(ci->fh,
	      "extern void POA_%s__fini(PortableServer_Servant servant, CORBA_Environment *ev);\n", id);

      g_free(id);
    }
    break;
  default:
    break;
  }
}

/************************/
typedef struct {
  FILE *of;
  IDL_tree realif;
} InheritedOutputInfo;
static void ch_output_inherited_protos(IDL_tree curif, InheritedOutputInfo *ioi);

static void
ch_output_stub_protos(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  if(!tree) return;

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MODULE:
    ch_output_stub_protos(IDL_MODULE(tree).definition_list, rinfo, ci);
    break;
  case IDLN_LIST:
    {
      IDL_tree sub;

      for(sub = tree; sub; sub = IDL_LIST(sub).next) {
	ch_output_stub_protos(IDL_LIST(sub).data, rinfo, ci);
      }
    }
    break;
  case IDLN_INTERFACE:
    {
      IDL_tree sub;

      if(IDL_INTERFACE(tree).inheritance_spec) {
	InheritedOutputInfo ioi;
	ioi.of = ci->fh;
	ioi.realif = tree;
	IDL_tree_traverse_parents(IDL_INTERFACE(tree).inheritance_spec, (GFunc)ch_output_inherited_protos, &ioi);
      }

      for(sub = IDL_INTERFACE(tree).body; sub; sub = IDL_LIST(sub).next) {
	IDL_tree cur;

	cur = IDL_LIST(sub).data;

	switch(IDL_NODE_TYPE(cur)) {
	case IDLN_OP_DCL:
	  orbit_cbe_op_write_proto(ci->fh, cur, "", FALSE);
	  fprintf(ci->fh, ";\n");
	  break;
	case IDLN_ATTR_DCL:
	  {
	    OIDL_Attr_Info *ai;
	    IDL_tree curitem;

	    for(curitem = IDL_ATTR_DCL(cur).simple_declarations; curitem; curitem = IDL_LIST(curitem).next) {
	      ai = IDL_LIST(curitem).data->data;
	      
	      orbit_cbe_op_write_proto(ci->fh, ai->op1, "", FALSE);
	      fprintf(ci->fh, ";\n");
	      
	      if(ai->op2) {
		orbit_cbe_op_write_proto(ci->fh, ai->op2, "", FALSE);
		fprintf(ci->fh, ";\n");
	      }
	    }
	  }
	  break;
	default:
	  break;
	}
      }
    }
    break;
  default:
    break;
  }
}

static void
ch_output_inherited_protos(IDL_tree curif, InheritedOutputInfo *ioi)
{
  char *id, *realid;
  IDL_tree curitem;

  if(curif == ioi->realif)
    return;

  realid = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(ioi->realif).ident), "_", 0);
  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(curif).ident), "_", 0);

  for(curitem = IDL_INTERFACE(curif).body; curitem; curitem = IDL_LIST(curitem).next) {
    IDL_tree curop = IDL_LIST(curitem).data;

    switch(IDL_NODE_TYPE(curop)) {
    case IDLN_OP_DCL:
      fprintf(ioi->of, "#define %s_%s %s_%s\n",
	      realid, IDL_IDENT(IDL_OP_DCL(curop).ident).str,
	      id, IDL_IDENT(IDL_OP_DCL(curop).ident).str);
      break;
    case IDLN_ATTR_DCL:
      {
	IDL_tree curitem;

	/* We don't use OIDL_Attr_Info here because inherited ops may go back into trees that are output-inhibited
	   and therefore don't have the OIDL_Attr_Info generated on them */

	for(curitem = IDL_ATTR_DCL(curop).simple_declarations; curitem; curitem = IDL_LIST(curitem).next) {
	  IDL_tree ident;

	  ident = IDL_LIST(curitem).data;
	  
	  fprintf(ioi->of, "#define %s__get_%s %s__get_%s\n",
		  realid, IDL_IDENT(ident).str,
		  id, IDL_IDENT(ident).str);

	  if(!IDL_ATTR_DCL(curop).f_readonly)
	    fprintf(ioi->of, "#define %s__set_%s %s__set_%s\n",
		    realid, IDL_IDENT(ident).str,
		    id, IDL_IDENT(ident).str);
	}
      }
      break;
	default:
	  break;
    }
  }

  g_free(id);
  g_free(realid);
}

static void
doskel(IDL_tree cur, char *ifid, OIDL_C_Info *ci)
{
  char *id;

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_OP_DCL(cur).ident), "_", 0);
  
  fprintf(ci->fh,
	  "void _ORBIT_skel_%s(POA_%s *_ORBIT_servant, GIOPRecvBuffer *_ORBIT_recv_buffer, CORBA_Environment *ev, ",
	  id, ifid);
  orbit_cbe_op_write_proto(ci->fh, cur, "_impl_", TRUE);
  fprintf(ci->fh, ");\n");
  g_free(id);
}

static void
ch_output_skel_protos(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  if(!tree) return;

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MODULE:
    ch_output_skel_protos(IDL_MODULE(tree).definition_list, rinfo, ci);
    break;
  case IDLN_LIST:
    {
      IDL_tree sub;

      for(sub = tree; sub; sub = IDL_LIST(sub).next) {
	ch_output_skel_protos(IDL_LIST(sub).data, rinfo, ci);
      }
    }
    break;
  case IDLN_INTERFACE:
    {
      IDL_tree sub;
      char *ifid;

      ifid = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(tree).ident), "_", 0);

      for(sub = IDL_INTERFACE(tree).body; sub; sub = IDL_LIST(sub).next) {
	IDL_tree cur;

	cur = IDL_LIST(sub).data;

	switch(IDL_NODE_TYPE(cur)) {
	case IDLN_OP_DCL:
	  doskel(cur, ifid, ci);
	  break;
	case IDLN_ATTR_DCL:
	  {
	    OIDL_Attr_Info *ai = cur->data;
	    IDL_tree curitem;

	    for(curitem = IDL_ATTR_DCL(cur).simple_declarations; curitem; curitem = IDL_LIST(curitem).next) {
	      ai = IDL_LIST(curitem).data->data;
	      
	      doskel(ai->op1, ifid, ci);
	      if(ai->op2)
		doskel(ai->op2, ifid, ci);
	    }
	  }
	  break;
	default:
	  break;
	}
      }
      g_free(ifid);
    }
    break;
  default:
    break;
  }
}

static void
ch_output_protos(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  if(!tree) return;

  ch_output_stub_protos(tree, rinfo, ci);
  fprintf(ci->fh, "\n");
  ch_output_skel_protos(tree, rinfo, ci);
}

