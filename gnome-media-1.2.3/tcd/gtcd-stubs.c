/*
 * This file was generated by orbit-idl - DO NOT EDIT!
 */

#include <string.h>
#include "gtcd.h"

void
GNOME_GTcd_open_new_cd(GNOME_GTcd _obj, CORBA_Environment * ev)
{
   register GIOP_unsigned_long _ORBIT_request_id,
    _ORBIT_system_exception_minor;
   register CORBA_completion_status _ORBIT_completion_status;
   register GIOPSendBuffer *_ORBIT_send_buffer;
   register GIOPRecvBuffer *_ORBIT_recv_buffer;
   register GIOPConnection *_cnx;

   if (_obj->servant && _obj->vepv && GNOME_GTcd__classid) {
      ((POA_GNOME_GTcd__epv *) _obj->vepv[GNOME_GTcd__classid])->
	 open_new_cd(_obj->servant, ev);
      return;
   }
   _cnx = ORBit_object_get_connection(_obj);
 _ORBIT_retry_request:
   _ORBIT_send_buffer = NULL;
   _ORBIT_recv_buffer = NULL;
   _ORBIT_completion_status = CORBA_COMPLETED_NO;
   _ORBIT_request_id = GPOINTER_TO_UINT(alloca(0));
   {				/* marshalling */
      static const struct
      {
	 CORBA_unsigned_long len;
	 char opname[12];
      }
      _ORBIT_operation_name_data =
      {
      12, "open_new_cd"};
      static const struct iovec _ORBIT_operation_vec =
	 { (gpointer) & _ORBIT_operation_name_data, 16 };
      _ORBIT_send_buffer =
	 giop_send_request_buffer_use(_cnx, NULL, _ORBIT_request_id,
				      CORBA_TRUE,
				      &(_obj->active_profile->object_key_vec),
				      &_ORBIT_operation_vec,
				      &ORBit_default_principal_iovec);

      _ORBIT_system_exception_minor = ex_CORBA_COMM_FAILURE;
      if (!_ORBIT_send_buffer)
	 goto _ORBIT_system_exception;
      giop_send_buffer_write(_ORBIT_send_buffer);
      _ORBIT_completion_status = CORBA_COMPLETED_MAYBE;
      giop_send_buffer_unuse(_ORBIT_send_buffer);
      _ORBIT_send_buffer = NULL;
   }
   {				/* demarshalling */
      register guchar *_ORBIT_curptr;

      _ORBIT_recv_buffer =
	 giop_recv_reply_buffer_use_2(_cnx, _ORBIT_request_id, TRUE);
      if (!_ORBIT_recv_buffer)
	 goto _ORBIT_system_exception;
      _ORBIT_completion_status = CORBA_COMPLETED_YES;
      if (_ORBIT_recv_buffer->message.u.reply.reply_status !=
	  GIOP_NO_EXCEPTION)
	 goto _ORBIT_msg_exception;
      _ORBIT_curptr = GIOP_RECV_BUFFER(_ORBIT_recv_buffer)->cur;
      if (giop_msg_conversion_needed(GIOP_MESSAGE_BUFFER(_ORBIT_recv_buffer))) {
      } else {
      }
      giop_recv_buffer_unuse(_ORBIT_recv_buffer);
      return;
    _ORBIT_system_exception:
      CORBA_exception_set_system(ev, _ORBIT_system_exception_minor,
				 _ORBIT_completion_status);
      giop_recv_buffer_unuse(_ORBIT_recv_buffer);
      giop_send_buffer_unuse(_ORBIT_send_buffer);
      return;
    _ORBIT_msg_exception:
      if (_ORBIT_recv_buffer->message.u.reply.reply_status ==
	  GIOP_LOCATION_FORWARD) {
	 if (_obj->forward_locations != NULL)
	    ORBit_delete_profiles(_obj->forward_locations);
	 _obj->forward_locations = ORBit_demarshal_IOR(_ORBIT_recv_buffer);
	 _cnx = ORBit_object_get_forwarded_connection(_obj);
	 giop_recv_buffer_unuse(_ORBIT_recv_buffer);

	 goto _ORBIT_retry_request;
      } else {
	 ORBit_handle_exception(_ORBIT_recv_buffer, ev, NULL, _obj->orb);
	 giop_recv_buffer_unuse(_ORBIT_recv_buffer);
	 return;
      }
   }
}
