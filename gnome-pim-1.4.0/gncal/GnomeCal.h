/*
 * This file was generated by orbit-idl - DO NOT EDIT!
 */

#include <glib.h>
#define ORBIT_IDL_SERIAL 9
#include <orb/orbit.h>

#ifndef GnomeCal_H
#define GnomeCal_H 1
#ifdef __cplusplus
extern "C"
{
#endif				/* __cplusplus */

/** typedefs **/
#include <libgnorba/gnome-factory.h>
#if !defined(ORBIT_DECL_GNOME_Calendar_Repository) && !defined(_GNOME_Calendar_Repository_defined)
#define ORBIT_DECL_GNOME_Calendar_Repository 1
#define _GNOME_Calendar_Repository_defined 1
#define GNOME_Calendar_Repository__free CORBA_Object__free
   typedef CORBA_Object GNOME_Calendar_Repository;
   extern CORBA_unsigned_long GNOME_Calendar_Repository__classid;
#if !defined(TC_IMPL_TC_GNOME_Calendar_Repository_0)
#define TC_IMPL_TC_GNOME_Calendar_Repository_0 'G'
#define TC_IMPL_TC_GNOME_Calendar_Repository_1 'n'
#define TC_IMPL_TC_GNOME_Calendar_Repository_2 'o'
#define TC_IMPL_TC_GNOME_Calendar_Repository_3 'm'
#define TC_IMPL_TC_GNOME_Calendar_Repository_4 'e'
#define TC_IMPL_TC_GNOME_Calendar_Repository_5 'C'
#define TC_IMPL_TC_GNOME_Calendar_Repository_6 'a'
#define TC_IMPL_TC_GNOME_Calendar_Repository_7 'l'
   extern const struct CORBA_TypeCode_struct
      TC_GNOME_Calendar_Repository_struct;
#define TC_GNOME_Calendar_Repository ((CORBA_TypeCode)&TC_GNOME_Calendar_Repository_struct)
#endif
#endif
#define ex_GNOME_Calendar_Repository_NotFound "IDL:GNOME/Calendar/Repository/NotFound:1.0"
   void _ORBIT_GNOME_Calendar_Repository_NotFound_demarshal(GIOPRecvBuffer *
							    _ORBIT_recv_buffer,
							    CORBA_Environment
							    * ev);
   void _ORBIT_GNOME_Calendar_Repository_NotFound_marshal(GIOPSendBuffer *
							  _ORBIT_send_buffer,
							  CORBA_Environment *
							  ev);
#if !defined(_GNOME_Calendar_Repository_NotFound_defined)
#define _GNOME_Calendar_Repository_NotFound_defined 1
   typedef struct
   {
      int dummy;
   }
   GNOME_Calendar_Repository_NotFound;

#if !defined(TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_0)
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_0 'G'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_1 'n'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_2 'o'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_3 'm'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_4 'e'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_5 'C'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_6 'a'
#define TC_IMPL_TC_GNOME_Calendar_Repository_NotFound_7 'l'
   extern const struct CORBA_TypeCode_struct
      TC_GNOME_Calendar_Repository_NotFound_struct;
#define TC_GNOME_Calendar_Repository_NotFound ((CORBA_TypeCode)&TC_GNOME_Calendar_Repository_NotFound_struct)
#endif
#define GNOME_Calendar_Repository_NotFound__alloc() NULL
   extern gpointer GNOME_Calendar_Repository_NotFound__free(gpointer mem,
							    gpointer dat,
							    CORBA_boolean free_strings);	/* ORBit internal use */
#endif
#if !defined(ORBIT_DECL_CORBA_sequence_CORBA_string) && !defined(_CORBA_sequence_CORBA_string_defined)
#define ORBIT_DECL_CORBA_sequence_CORBA_string 1
#define _CORBA_sequence_CORBA_string_defined 1
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_0 'G'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_1 'n'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_2 'o'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_3 'm'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_4 'e'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_5 'C'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_6 'a'
#define ORBIT_IMPL_CORBA_sequence_CORBA_string_7 'l'
   typedef struct
   {
      CORBA_unsigned_long _maximum,
       _length;
      CORBA_char **_buffer;
      CORBA_boolean _release;
   }
   CORBA_sequence_CORBA_string;
#if !defined(TC_IMPL_TC_CORBA_sequence_CORBA_string_0)
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_0 'G'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_1 'n'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_2 'o'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_3 'm'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_4 'e'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_5 'C'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_6 'a'
#define TC_IMPL_TC_CORBA_sequence_CORBA_string_7 'l'
   extern const struct CORBA_TypeCode_struct
      TC_CORBA_sequence_CORBA_string_struct;
#define TC_CORBA_sequence_CORBA_string ((CORBA_TypeCode)&TC_CORBA_sequence_CORBA_string_struct)
#endif
   extern CORBA_sequence_CORBA_string
      *CORBA_sequence_CORBA_string__alloc(void);
   extern gpointer CORBA_sequence_CORBA_string__free(gpointer mem,
						     gpointer dat,
						     CORBA_boolean free_strings);	/* ORBit internal use */
   CORBA_char **CORBA_sequence_CORBA_string_allocbuf(CORBA_unsigned_long len);
#endif
#if !defined(_GNOME_Calendar_Repository_String_Sequence_defined)
#define _GNOME_Calendar_Repository_String_Sequence_defined 1
   typedef CORBA_sequence_CORBA_string
      GNOME_Calendar_Repository_String_Sequence;
#if !defined(TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_0)
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_0 'G'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_1 'n'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_2 'o'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_3 'm'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_4 'e'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_5 'C'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_6 'a'
#define TC_IMPL_TC_GNOME_Calendar_Repository_String_Sequence_7 'l'
   extern const struct CORBA_TypeCode_struct
      TC_GNOME_Calendar_Repository_String_Sequence_struct;
#define TC_GNOME_Calendar_Repository_String_Sequence ((CORBA_TypeCode)&TC_GNOME_Calendar_Repository_String_Sequence_struct)
#endif
   extern GNOME_Calendar_Repository_String_Sequence
      *GNOME_Calendar_Repository_String_Sequence__alloc(void);
   extern gpointer GNOME_Calendar_Repository_String_Sequence__free(gpointer
								   mem,
								   gpointer
								   dat,
								   CORBA_boolean free_strings);	/* ORBit internal use */
#endif
#if !defined(_GNOME_Calendar_Repository_RecordStatus_defined)
#define _GNOME_Calendar_Repository_RecordStatus_defined 1
   typedef enum
   {
      GNOME_Calendar_Repository_ANY,
      GNOME_Calendar_Repository_NEW,
      GNOME_Calendar_Repository_MODIFIED,
      GNOME_Calendar_Repository_DELETED
   }
   GNOME_Calendar_Repository_RecordStatus;
#if !defined(TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_0)
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_0 'G'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_1 'n'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_2 'o'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_3 'm'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_4 'e'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_5 'C'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_6 'a'
#define TC_IMPL_TC_GNOME_Calendar_Repository_RecordStatus_7 'l'
   extern const struct CORBA_TypeCode_struct
      TC_GNOME_Calendar_Repository_RecordStatus_struct;
#define TC_GNOME_Calendar_Repository_RecordStatus ((CORBA_TypeCode)&TC_GNOME_Calendar_Repository_RecordStatus_struct)
#endif
#endif

/** POA structures **/
   typedef struct
   {
      void *_private;
      CORBA_char *(*get_object) (PortableServer_Servant _servant,
				 const CORBA_char * uid,
				 CORBA_Environment * ev);
      CORBA_char *(*get_objects_by_id_list) (PortableServer_Servant _servant,
					     const
					     GNOME_Calendar_Repository_String_Sequence
					     * uid, CORBA_Environment * ev);
      CORBA_char *(*get_object_by_pilot_id) (PortableServer_Servant _servant,
					     const CORBA_long pilot_id,
					     CORBA_Environment * ev);
      CORBA_char *(*get_id_from_pilot_id) (PortableServer_Servant _servant,
					   const CORBA_long pilot_id,
					   CORBA_Environment * ev);
      void (*delete_object) (PortableServer_Servant _servant,
			     const CORBA_char * uid, CORBA_Environment * ev);
      void (*update_object) (PortableServer_Servant _servant,
			     const CORBA_char * uid,
			     const CORBA_char * object,
			     CORBA_Environment * ev);
      void (*update_pilot_id) (PortableServer_Servant _servant,
			       const CORBA_char * uid,
			       const CORBA_long pilot_id,
			       const CORBA_long pilot_status,
			       CORBA_Environment * ev);
      CORBA_char *(*get_objects) (PortableServer_Servant _servant,
				  CORBA_Environment * ev);
      GNOME_Calendar_Repository_String_Sequence
	 *(*get_object_id_list) (PortableServer_Servant _servant,
				 CORBA_Environment * ev);
      CORBA_char *(*get_updated_objects) (PortableServer_Servant _servant,
					  CORBA_Environment * ev);
       CORBA_long(*get_number_of_objects) (PortableServer_Servant _servant,
					   const
					   GNOME_Calendar_Repository_RecordStatus
					   record_status,
					   CORBA_Environment * ev);
      void (*done) (PortableServer_Servant _servant, CORBA_Environment * ev);
   }
   POA_GNOME_Calendar_Repository__epv;
   typedef struct
   {
      PortableServer_ServantBase__epv *_base_epv;
      POA_GNOME_Calendar_Repository__epv *GNOME_Calendar_Repository_epv;
   }
   POA_GNOME_Calendar_Repository__vepv;
   typedef struct
   {
      void *_private;
      POA_GNOME_Calendar_Repository__vepv *vepv;
   }
   POA_GNOME_Calendar_Repository;
   extern void POA_GNOME_Calendar_Repository__init(PortableServer_Servant
						   servant,
						   CORBA_Environment * ev);
   extern void POA_GNOME_Calendar_Repository__fini(PortableServer_Servant
						   servant,
						   CORBA_Environment * ev);

/** prototypes **/
   CORBA_char *GNOME_Calendar_Repository_get_object(GNOME_Calendar_Repository
						    _obj,
						    const CORBA_char * uid,
						    CORBA_Environment * ev);
   CORBA_char
      *GNOME_Calendar_Repository_get_objects_by_id_list
      (GNOME_Calendar_Repository _obj,
       const GNOME_Calendar_Repository_String_Sequence * uid,
       CORBA_Environment * ev);
   CORBA_char
      *GNOME_Calendar_Repository_get_object_by_pilot_id
      (GNOME_Calendar_Repository _obj, const CORBA_long pilot_id,
       CORBA_Environment * ev);
   CORBA_char
      *GNOME_Calendar_Repository_get_id_from_pilot_id
      (GNOME_Calendar_Repository _obj, const CORBA_long pilot_id,
       CORBA_Environment * ev);
   void GNOME_Calendar_Repository_delete_object(GNOME_Calendar_Repository
						_obj, const CORBA_char * uid,
						CORBA_Environment * ev);
   void GNOME_Calendar_Repository_update_object(GNOME_Calendar_Repository
						_obj, const CORBA_char * uid,
						const CORBA_char * object,
						CORBA_Environment * ev);
   void GNOME_Calendar_Repository_update_pilot_id(GNOME_Calendar_Repository
						  _obj,
						  const CORBA_char * uid,
						  const CORBA_long pilot_id,
						  const CORBA_long
						  pilot_status,
						  CORBA_Environment * ev);
   CORBA_char *GNOME_Calendar_Repository_get_objects(GNOME_Calendar_Repository
						     _obj,
						     CORBA_Environment * ev);
   GNOME_Calendar_Repository_String_Sequence
      *GNOME_Calendar_Repository_get_object_id_list(GNOME_Calendar_Repository
						    _obj,
						    CORBA_Environment * ev);
   CORBA_char
      *GNOME_Calendar_Repository_get_updated_objects(GNOME_Calendar_Repository
						     _obj,
						     CORBA_Environment * ev);
   CORBA_long
      GNOME_Calendar_Repository_get_number_of_objects
      (GNOME_Calendar_Repository _obj,
       const GNOME_Calendar_Repository_RecordStatus record_status,
       CORBA_Environment * ev);
   void GNOME_Calendar_Repository_done(GNOME_Calendar_Repository _obj,
				       CORBA_Environment * ev);

   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_object
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char * (*_impl_get_object) (PortableServer_Servant _servant,
					 const CORBA_char * uid,
					 CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_objects_by_id_list
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char *
       (*_impl_get_objects_by_id_list) (PortableServer_Servant _servant,
					const
					GNOME_Calendar_Repository_String_Sequence
					* uid, CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_object_by_pilot_id
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char *
       (*_impl_get_object_by_pilot_id) (PortableServer_Servant _servant,
					const CORBA_long pilot_id,
					CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_id_from_pilot_id
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char *
       (*_impl_get_id_from_pilot_id) (PortableServer_Servant _servant,
				      const CORBA_long pilot_id,
				      CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_delete_object
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       void (*_impl_delete_object) (PortableServer_Servant _servant,
				    const CORBA_char * uid,
				    CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_update_object
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       void (*_impl_update_object) (PortableServer_Servant _servant,
				    const CORBA_char * uid,
				    const CORBA_char * object,
				    CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_update_pilot_id
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       void (*_impl_update_pilot_id) (PortableServer_Servant _servant,
				      const CORBA_char * uid,
				      const CORBA_long pilot_id,
				      const CORBA_long pilot_status,
				      CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_objects
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char * (*_impl_get_objects) (PortableServer_Servant _servant,
					  CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_object_id_list
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       GNOME_Calendar_Repository_String_Sequence *
       (*_impl_get_object_id_list) (PortableServer_Servant _servant,
				    CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_updated_objects
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_char *
       (*_impl_get_updated_objects) (PortableServer_Servant _servant,
				     CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_get_number_of_objects
      (POA_GNOME_Calendar_Repository * _ORBIT_servant,
       GIOPRecvBuffer * _ORBIT_recv_buffer, CORBA_Environment * ev,
       CORBA_long(*_impl_get_number_of_objects) (PortableServer_Servant
						 _servant,
						 const
						 GNOME_Calendar_Repository_RecordStatus
						 record_status,
						 CORBA_Environment * ev));
   void
      _ORBIT_skel_GNOME_Calendar_Repository_done(POA_GNOME_Calendar_Repository
						 * _ORBIT_servant,
						 GIOPRecvBuffer *
						 _ORBIT_recv_buffer,
						 CORBA_Environment * ev,
						 void (*_impl_done)
						 (PortableServer_Servant
						  _servant,
						  CORBA_Environment * ev));
#ifdef __cplusplus
}
#endif				/* __cplusplus */

#endif
#undef ORBIT_IDL_SERIAL
