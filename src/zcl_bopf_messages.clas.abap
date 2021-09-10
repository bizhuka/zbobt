class ZCL_BOPF_MESSAGES definition
  public
  final
  create public .

public section.

  data MT_LOGS type ZCL_EUI_LOGGER=>TT_MSG read-only .

  methods CONSTRUCTOR
    importing
      !IV_SEVERITY type CSEQUENCE optional .
  methods ADD_FROM_TABLE
    importing
      !IT_LOG type ZCL_EUI_LOGGER=>TT_MSG
    returning
      value(RO_ME) type ref to ZCL_BOPF_MESSAGES .
  methods ADD_FROM_MESSAGE
    importing
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
    returning
      value(RO_ME) type ref to ZCL_BOPF_MESSAGES .
  methods ADD_FROM_MANAGER
    importing
      !IO_MANAGER type ref to ZCL_BOPF_MANAGER
      !CV_HAS_CHANGE type ref to ABAP_BOOL optional
    returning
      value(RO_ME) type ref to ZCL_BOPF_MESSAGES .
  methods SHOW
    returning
      value(RV_HAS_LOGS) type ABAP_BOOL .
  class-methods GET_MSG_TEXTID
    importing
      !IV_MESSAGE type CSEQUENCE optional
      !IO_ERROR type ref to CX_ROOT optional
    returning
      value(RS_T100KEY) type SCX_T100KEY .
  class-methods RAISE_ERROR
    importing
      !IV_MESSAGE type CSEQUENCE optional
      !IO_ERROR type ref to CX_ROOT optional
    raising
      /BOBF/CX_FRW .
protected section.
private section.

  data MV_SEVERITY type STRING .
ENDCLASS.



CLASS ZCL_BOPF_MESSAGES IMPLEMENTATION.


METHOD add_from_manager.
  ro_me = me.

  " Use 2 params instead?
  DATA(lo_service)     = io_manager->mo_service.
  DATA(lo_transaction) = io_manager->mo_transaction.

  DATA(lo_changes) = lo_transaction->get_transactional_changes( ).
  DATA(lt_changes) = lo_changes->get_changes( ).

  DATA(lt_logs) = VALUE zcl_eui_logger=>tt_msg( ).
  LOOP AT lt_changes ASSIGNING FIELD-SYMBOL(<ls_changes>) WHERE bo_key = io_manager->mo_metadata->mv_bo_key.
    <ls_changes>-change_object->get_changes(
     IMPORTING et_change = DATA(lt_change) ).

    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>) WHERE
          change_mode = /bobf/if_frw_c=>sc_modify_create
       OR change_mode = /bobf/if_frw_c=>sc_modify_update
       OR change_mode = /bobf/if_frw_c=>sc_modify_delete.

      IF cv_has_change IS NOT INITIAL.
        cv_has_change->* = abap_true.
      ENDIF.

      " execute check_and_determine
      lo_service->check_consistency(
        EXPORTING iv_node_key    = <ls_change>-node_key
                  it_key         = VALUE #( ( key = <ls_change>-key ) )
                  iv_check_scope = /bobf/if_frw_c=>sc_scope_substructure
        IMPORTING eo_message     = DATA(lo_message) ).

      add_from_message( lo_message ).
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD add_from_message.
  ro_me = me.
  CHECK io_message IS NOT INITIAL.

  io_message->get_messages( IMPORTING et_message = DATA(lt_message) ).
  CHECK lt_message IS NOT INITIAL.

  DATA(lt_logs) = VALUE zcl_eui_logger=>tt_msg( ).
  LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<ls_message>) WHERE message IS NOT INITIAL.
    DATA(ls_t100key) = get_msg_textid( io_error = <ls_message>-message ).
    APPEND VALUE #( msgid = ls_t100key-msgid
                    msgno = ls_t100key-msgno
                    msgty = <ls_message>-severity
                    msgv1 = ls_t100key-attr1
                    msgv2 = ls_t100key-attr2
                    msgv3 = ls_t100key-attr3
                    msgv4 = ls_t100key-attr4 ) TO lt_logs.
  ENDLOOP.
  add_from_table( lt_logs ).
ENDMETHOD.


METHOD add_from_table.
  ro_me = me.

  IF mv_severity IS INITIAL.
    APPEND LINES OF it_log[] TO mt_logs.
    RETURN.
  ENDIF.

  DATA(lt_log) = VALUE zcl_eui_logger=>tt_msg(
                 FOR <ls_log> IN it_log WHERE ( msgty CA mv_severity )
                 ( <ls_log> ) ).
  APPEND LINES OF lt_log[] TO mt_logs.
ENDMETHOD.


METHOD constructor.
  mv_severity = iv_severity.
ENDMETHOD.


METHOD get_msg_textid.
  DATA: BEGIN OF l_string,
          attr1 TYPE symsgv,
          attr2 TYPE symsgv,
          attr3 TYPE symsgv,
          attr4 TYPE symsgv,
        END OF l_string.

  ASSERT io_error   IS NOT INITIAL
      OR iv_message IS NOT INITIAL.

  IF iv_message IS NOT INITIAL.
    l_string = iv_message.
  ELSEIF io_error IS NOT INITIAL.
    TRY.
        DATA(lo_message)  = CAST if_t100_message( io_error ).
        rs_t100key = lo_message->t100key.
      CATCH cx_sy_move_cast_error.
        CLEAR rs_t100key.
    ENDTRY.

    IF rs_t100key IS INITIAL.
      l_string = io_error->get_text( ).
    ENDIF.
  ENDIF.

  IF l_string IS NOT INITIAL.
    MOVE-CORRESPONDING l_string TO rs_t100key.
    rs_t100key-msgid = 'BL'.
    rs_t100key-msgno = '001'.
  ENDIF.

  CHECK io_error IS NOT INITIAL.
  DO 4 TIMES.
    DATA(lv_name) = |rs_t100key-attr{ CONV num1( sy-index ) }|.
    ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_dest>).
    CHECK sy-subrc = 0.

    " Public reference to field name ?
    lv_name = |io_error->{ <lv_dest> }|.
    ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_src>).
    CHECK sy-subrc = 0.
    <lv_dest> = <lv_src>.
  ENDDO.
ENDMETHOD.


METHOD raise_error.
  DATA(ls_textid) = get_msg_textid( io_error   = io_error
                                    iv_message = iv_message ).
  RAISE EXCEPTION TYPE /bobf/cx_dac
    EXPORTING
      textid = ls_textid.
ENDMETHOD.


METHOD show.
  CHECK mt_logs[] IS NOT INITIAL.
  rv_has_logs = abap_true.

  DATA(lo_logger) = NEW zcl_eui_logger( ).
  lo_logger->add_batch( mt_logs ).
  lo_logger->show( iv_profile = zcl_eui_logger=>mc_profile-popup ).
ENDMETHOD.
ENDCLASS.
