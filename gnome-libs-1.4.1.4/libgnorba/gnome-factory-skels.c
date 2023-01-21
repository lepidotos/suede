/*
 * This file was generated by orbit-idl - DO NOT EDIT!
 */

#include <string.h>
#include "gnome-factory.h"

void
_ORBIT_GNOME_GenericFactory_CannotActivate_marshal(GIOPSendBuffer *
						   _ORBIT_send_buffer,
						   CORBA_Environment * ev)
{
}

void
_ORBIT_skel_GNOME_GenericFactory_supports(POA_GNOME_GenericFactory *
					  _ORBIT_servant,
					  GIOPRecvBuffer * _ORBIT_recv_buffer,
					  CORBA_Environment * ev,
					  CORBA_boolean(*_impl_supports)
					  (PortableServer_Servant _servant,
					   const CORBA_char * obj_goad_id,
					   CORBA_Environment * ev))
{
   CORBA_boolean _ORBIT_retval;
   CORBA_char *obj_goad_id;

   {				/* demarshalling */
      guchar *_ORBIT_curptr;
      register CORBA_unsigned_long _ORBIT_tmpvar_2;
      CORBA_unsigned_long _ORBIT_tmpvar_3;

      _ORBIT_curptr = GIOP_RECV_BUFFER(_ORBIT_recv_buffer)->cur;
      if (giop_msg_conversion_needed(GIOP_MESSAGE_BUFFER(_ORBIT_recv_buffer))) {
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 (*((guint32 *) & (_ORBIT_tmpvar_3))) =
	    GUINT32_SWAP_LE_BE(*((guint32 *) _ORBIT_curptr));
	 _ORBIT_curptr += 4;
	 obj_goad_id = (void *) _ORBIT_curptr;
	 _ORBIT_curptr +=
	    sizeof(obj_goad_id[_ORBIT_tmpvar_2]) * _ORBIT_tmpvar_3;
      } else {
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 _ORBIT_tmpvar_3 = *((CORBA_unsigned_long *) _ORBIT_curptr);
	 _ORBIT_curptr += 4;
	 obj_goad_id = (void *) _ORBIT_curptr;
	 _ORBIT_curptr +=
	    sizeof(obj_goad_id[_ORBIT_tmpvar_2]) * _ORBIT_tmpvar_3;
      }
   }
   _ORBIT_retval = _impl_supports(_ORBIT_servant, obj_goad_id, ev);
   {				/* marshalling */
      register GIOPSendBuffer *_ORBIT_send_buffer;

      _ORBIT_send_buffer =
	 giop_send_reply_buffer_use(GIOP_MESSAGE_BUFFER(_ORBIT_recv_buffer)->
				    connection, NULL,
				    _ORBIT_recv_buffer->message.u.request.
				    request_id, ev->_major);
      if (_ORBIT_send_buffer) {
	 if (ev->_major == CORBA_NO_EXCEPTION) {
	    {
	       guchar *_ORBIT_t;

	       _ORBIT_t = alloca(sizeof(_ORBIT_retval));
	       memcpy(_ORBIT_t, &(_ORBIT_retval), sizeof(_ORBIT_retval));
	       giop_message_buffer_append_mem(GIOP_MESSAGE_BUFFER
					      (_ORBIT_send_buffer),
					      (_ORBIT_t),
					      sizeof(_ORBIT_retval));
	    }
	 } else
	    ORBit_send_system_exception(_ORBIT_send_buffer, ev);
	 giop_send_buffer_write(_ORBIT_send_buffer);
	 giop_send_buffer_unuse(_ORBIT_send_buffer);
      }
   }
}
void
_ORBIT_skel_GNOME_GenericFactory_create_object(POA_GNOME_GenericFactory *
					       _ORBIT_servant,
					       GIOPRecvBuffer *
					       _ORBIT_recv_buffer,
					       CORBA_Environment * ev,
					       CORBA_Object
					       (*_impl_create_object)
					       (PortableServer_Servant
						_servant,
						const CORBA_char * goad_id,
						const GNOME_stringlist *
						params,
						CORBA_Environment * ev))
{
   CORBA_Object _ORBIT_retval;
   CORBA_char *goad_id;
   GNOME_stringlist params = { 0, 0, NULL, CORBA_FALSE };

   {				/* demarshalling */
      guchar *_ORBIT_curptr;
      register CORBA_unsigned_long _ORBIT_tmpvar_5;
      CORBA_unsigned_long _ORBIT_tmpvar_6;
      register CORBA_unsigned_long _ORBIT_tmpvar_7;
      register CORBA_unsigned_long _ORBIT_tmpvar_8;
      CORBA_unsigned_long _ORBIT_tmpvar_9;

      _ORBIT_curptr = GIOP_RECV_BUFFER(_ORBIT_recv_buffer)->cur;
      if (giop_msg_conversion_needed(GIOP_MESSAGE_BUFFER(_ORBIT_recv_buffer))) {
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 (*((guint32 *) & (_ORBIT_tmpvar_6))) =
	    GUINT32_SWAP_LE_BE(*((guint32 *) _ORBIT_curptr));
	 _ORBIT_curptr += 4;
	 goad_id = (void *) _ORBIT_curptr;
	 _ORBIT_curptr += sizeof(goad_id[_ORBIT_tmpvar_5]) * _ORBIT_tmpvar_6;
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 (*((guint32 *) & (params._length))) =
	    GUINT32_SWAP_LE_BE(*((guint32 *) _ORBIT_curptr));
	 _ORBIT_curptr += 4;
	 params._buffer =
	    alloca(sizeof(params._buffer[_ORBIT_tmpvar_7]) * params._length);
	 params._release = CORBA_FALSE;
	 for (_ORBIT_tmpvar_7 = 0; _ORBIT_tmpvar_7 < params._length;
	      _ORBIT_tmpvar_7++) {
	    _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	    (*((guint32 *) & (_ORBIT_tmpvar_9))) =
	       GUINT32_SWAP_LE_BE(*((guint32 *) _ORBIT_curptr));
	    _ORBIT_curptr += 4;
	    params._buffer[_ORBIT_tmpvar_7] = (void *) _ORBIT_curptr;
	    _ORBIT_curptr +=
	       sizeof(params._buffer[_ORBIT_tmpvar_7][_ORBIT_tmpvar_8]) *
	       _ORBIT_tmpvar_9;
	 }

      } else {
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 _ORBIT_tmpvar_6 = *((CORBA_unsigned_long *) _ORBIT_curptr);
	 _ORBIT_curptr += 4;
	 goad_id = (void *) _ORBIT_curptr;
	 _ORBIT_curptr += sizeof(goad_id[_ORBIT_tmpvar_5]) * _ORBIT_tmpvar_6;
	 _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	 params._length = *((CORBA_unsigned_long *) _ORBIT_curptr);
	 _ORBIT_curptr += 4;
	 params._buffer =
	    alloca(sizeof(params._buffer[_ORBIT_tmpvar_7]) * params._length);
	 params._release = CORBA_FALSE;
	 for (_ORBIT_tmpvar_7 = 0; _ORBIT_tmpvar_7 < params._length;
	      _ORBIT_tmpvar_7++) {
	    _ORBIT_curptr = ALIGN_ADDRESS(_ORBIT_curptr, 4);
	    _ORBIT_tmpvar_9 = *((CORBA_unsigned_long *) _ORBIT_curptr);
	    _ORBIT_curptr += 4;
	    params._buffer[_ORBIT_tmpvar_7] = (void *) _ORBIT_curptr;
	    _ORBIT_curptr +=
	       sizeof(params._buffer[_ORBIT_tmpvar_7][_ORBIT_tmpvar_8]) *
	       _ORBIT_tmpvar_9;
	 }

      }
   }
   _ORBIT_retval =
      _impl_create_object(_ORBIT_servant, goad_id, &(params), ev);
   {				/* marshalling */
      register GIOPSendBuffer *_ORBIT_send_buffer;

      _ORBIT_send_buffer =
	 giop_send_reply_buffer_use(GIOP_MESSAGE_BUFFER(_ORBIT_recv_buffer)->
				    connection, NULL,
				    _ORBIT_recv_buffer->message.u.request.
				    request_id, ev->_major);
      if (_ORBIT_send_buffer) {
	 if (ev->_major == CORBA_NO_EXCEPTION) {
	    ORBit_marshal_object(_ORBIT_send_buffer, _ORBIT_retval);
	 } else if (ev->_major == CORBA_USER_EXCEPTION) {
	    static const ORBit_exception_marshal_info _ORBIT_user_exceptions[]
	       =
	       { {(const CORBA_TypeCode)
		  &TC_GNOME_GenericFactory_CannotActivate_struct,
		  (gpointer)
		  _ORBIT_GNOME_GenericFactory_CannotActivate_marshal},
	       {CORBA_OBJECT_NIL, NULL} };
	    ORBit_send_user_exception(_ORBIT_send_buffer, ev,
				      _ORBIT_user_exceptions);
	 } else
	    ORBit_send_system_exception(_ORBIT_send_buffer, ev);
	 giop_send_buffer_write(_ORBIT_send_buffer);
	 giop_send_buffer_unuse(_ORBIT_send_buffer);
      }
      if (ev->_major == CORBA_NO_EXCEPTION)
	 CORBA_Object_release(_ORBIT_retval, ev);
   }
}
static ORBitSkeleton
get_skel_GNOME_GenericFactory(POA_GNOME_GenericFactory * servant,
			      GIOPRecvBuffer * _ORBIT_recv_buffer,
			      gpointer * impl)
{
   gchar *opname = _ORBIT_recv_buffer->message.u.request.operation;

   switch (opname[0]) {
     case 'c':
	if (strcmp((opname + 1), "reate_object"))
	   break;
	*impl =
	   (gpointer) servant->vepv->GNOME_GenericFactory_epv->create_object;
	return (ORBitSkeleton) _ORBIT_skel_GNOME_GenericFactory_create_object;
	break;
     case 's':
	if (strcmp((opname + 1), "upports"))
	   break;
	*impl = (gpointer) servant->vepv->GNOME_GenericFactory_epv->supports;
	return (ORBitSkeleton) _ORBIT_skel_GNOME_GenericFactory_supports;
	break;
     default:
	break;
   }
   return NULL;
}

static void
init_local_objref_GNOME_GenericFactory(CORBA_Object obj,
				       POA_GNOME_GenericFactory * servant)
{
   obj->vepv[GNOME_GenericFactory__classid] =
      servant->vepv->GNOME_GenericFactory_epv;
}

void
POA_GNOME_GenericFactory__init(PortableServer_Servant servant,
			       CORBA_Environment * env)
{
   static const PortableServer_ClassInfo class_info =
      { (ORBit_impl_finder) & get_skel_GNOME_GenericFactory,
"IDL:GNOME/GenericFactory:1.0", (ORBit_local_objref_init) & init_local_objref_GNOME_GenericFactory };
   PortableServer_ServantBase__init(((PortableServer_ServantBase *) servant),
				    env);
   ORBIT_OBJECT_KEY(((PortableServer_ServantBase *) servant)->_private)->
      class_info = (PortableServer_ClassInfo *) & class_info;
   if (!GNOME_GenericFactory__classid)
      GNOME_GenericFactory__classid = ORBit_register_class(&class_info);
}

void
POA_GNOME_GenericFactory__fini(PortableServer_Servant servant,
			       CORBA_Environment * env)
{
   PortableServer_ServantBase__fini(servant, env);
}