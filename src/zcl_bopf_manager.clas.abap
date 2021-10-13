class ZCL_BOPF_MANAGER definition
  public
  create private .

public section.

  types:
    BEGIN OF ts_cache,
     name TYPE STRING,
     manager TYPE REF TO ZCL_BOPF_MANAGER,
   END OF ts_cache .
  types:
    tt_cache TYPE SORTED TABLE OF ts_cache WITH UNIQUE KEY name .

  data MO_METADATA type ref to ZCL_BOPF_METADATA read-only .
  data MO_SERVICE type ref to /BOBF/IF_TRA_SERVICE_MANAGER read-only .
  data MO_TRANSACTION type ref to /BOBF/IF_TRA_TRANSACTION_MGR read-only .
  class-data MT_ALL_MANAGERS type TT_CACHE read-only .

  methods CONSTRUCTOR
    importing
      !IV_LOG type ABAP_BOOL .
  class-methods CREATE
    importing
      !IV_BOPF_NAME type CSEQUENCE
      !IV_LOG type ABAP_BOOL optional
    returning
      value(RO_MANAGER) type ref to ZCL_BOPF_MANAGER
    raising
      /BOBF/CX_FRW .
  methods QUERY_ROW
    importing
      !IV_QUERY type /BOBF/OBM_QUERY_KEY optional
      !IT_SEL_PARAM type /BOBF/T_FRW_QUERY_SELPARAM optional
      !IS_QUERY_OPTIONS type /BOBF/S_FRW_QUERY_OPTIONS optional
    returning
      value(RV_KEY) type /BOBF/CONF_KEY
    raising
      /BOBF/CX_FRW .
  methods QUERY_TAB
    importing
      value(IV_QUERY) type /BOBF/OBM_QUERY_KEY optional
      !IT_SEL_PARAM type /BOBF/T_FRW_QUERY_SELPARAM optional
      !IS_QUERY_OPTIONS type /BOBF/S_FRW_QUERY_OPTIONS optional
    returning
      value(RT_KEY) type /BOBF/T_FRW_KEY
    raising
      /BOBF/CX_FRW .
  methods RETRIEVE_ROW
    importing
      !IV_NODE_TYPE type /BOBF/OBM_NODE_KEY optional
      !IV_KEY type /BOBF/CONF_KEY
      !IV_EDIT_MODE type /BOBF/CONF_EDIT_MODE default /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
    returning
      value(RR_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods RETRIEVE_TAB
    importing
      value(IV_NODE_TYPE) type /BOBF/OBM_NODE_KEY optional
      !IT_KEY type /BOBF/T_FRW_KEY
      !IV_EDIT_MODE type /BOBF/CONF_EDIT_MODE default                                   /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
      !IV_FILL_DATA type ABAP_BOOL default ABAP_TRUE
    returning
      value(RR_TABLE) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods RETRIEVE_BY_ASSOC_ROW
    importing
      !IV_PARENT_TYPE type /BOBF/OBM_NODE_KEY optional
      !IV_KEY type /BOBF/CONF_KEY
      !IV_ASSOC_TYPE type /BOBF/OBM_ASSOC_KEY
      !IV_EDIT_MODE type /BOBF/CONF_EDIT_MODE default /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
      !IS_PARAMETERS type ref to DATA optional
    returning
      value(RR_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods RETRIEVE_BY_ASSOC_TAB
    importing
      value(IV_PARENT_TYPE) type /BOBF/OBM_NODE_KEY optional
      !IV_KEY type /BOBF/CONF_KEY
      !IV_ASSOC_TYPE type /BOBF/OBM_ASSOC_KEY
      !IV_EDIT_MODE type /BOBF/CONF_EDIT_MODE default                                   /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
      !IS_PARAMETERS type ref to DATA optional
    returning
      value(RR_TABLE) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods MODIFY
    importing
      !IT_MODIFICATION type /BOBF/T_FRW_MODIFICATION
    exporting
      !EO_CHANGE type ref to /BOBF/IF_TRA_CHANGE
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !EV_OK type ABAP_BOOL
    raising
      /BOBF/CX_FRW .
  methods DO_ACTION
    importing
      !IV_ACT_KEY type /BOBF/ACT_KEY
      !IT_KEY type /BOBF/T_FRW_KEY optional
      !IS_PARAMETERS type ref to DATA optional
    exporting
      !EO_CHANGE type ref to /BOBF/IF_TRA_CHANGE
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_FAILED_KEY type /BOBF/T_FRW_KEY
      !ET_FAILED_ACTION_KEY type /BOBF/T_FRW_KEY
      !ET_DATA type INDEX TABLE .
  methods SAVE
    exporting
      !EV_OK type ABAP_BOOL
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
    raising
      /BOBF/CX_FRW .
protected section.
private section.

  data MO_LOGGER type ref to LCL_LOGGER .

  methods _CHECK_HAS_ERRORS
    importing
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
    raising
      /BOBF/CX_FRW .
  methods _READ_BY_INDEX
    importing
      !IR_TABLE type ref to DATA
      !IV_INDEX type SYTABIX
    returning
      value(RR_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
ENDCLASS.



CLASS ZCL_BOPF_MANAGER IMPLEMENTATION.


METHOD constructor.
  IF iv_log = abap_true.
    mo_logger = NEW #( me ).
  ENDIF.
ENDMETHOD.


METHOD create.
  ASSIGN mt_all_managers[ name = iv_bopf_name ] TO FIELD-SYMBOL(<ls_cache>).
  IF sy-subrc = 0.
    ro_manager = <ls_cache>-manager.
    RETURN.
  ENDIF.

  " 1 isntance only
  INSERT VALUE #( name    = iv_bopf_name
                  manager = NEW #( iv_log ) ) INTO TABLE mt_all_managers[] ASSIGNING <ls_cache>.

  ro_manager  = <ls_cache>-manager.
  DATA(lo_metadata) = NEW zcl_bopf_metadata( iv_bopf_name ).
  ro_manager->mo_metadata    = lo_metadata.
  ro_manager->mo_transaction = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

  CHECK lo_metadata->mv_select_all IS NOT INITIAL.
  ro_manager->mo_service = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( lo_metadata->mv_bo_key ).
ENDMETHOD.


METHOD do_action.
  mo_service->do_action(
    EXPORTING iv_act_key           = iv_act_key
              it_key               = it_key
              is_parameters        = is_parameters
    IMPORTING eo_change            = eo_change
              eo_message           = eo_message
              et_failed_key        = et_failed_key
              et_failed_action_key = et_failed_action_key
              et_data              = et_data ).

  CHECK mo_logger IS NOT INITIAL.
  mo_logger->add( io_message = eo_message
                  iv_ok      = xsdbool( et_failed_key[] IS INITIAL AND et_failed_action_key[] IS INITIAL )
                  is_action  = VALUE #(
                   action_node = iv_act_key
                   t_key       = it_key
                   r_params    = is_parameters
                  ) ).
ENDMETHOD.


METHOD modify.
  ev_ok = abap_true.

  mo_service->modify(
   EXPORTING it_modification = it_modification
   IMPORTING eo_change       = eo_change
             eo_message      = eo_message ).

  IF eo_change IS NOT INITIAL AND eo_change->has_failed_changes( ).
    ev_ok = abap_false.
  ENDIF.

  IF eo_message IS NOT INITIAL AND eo_message->check( ).
    ev_ok = abap_false.
  ENDIF.

  IF mo_logger IS NOT INITIAL.
    mo_logger->add(
       it_modification = it_modification
       iv_ok           = ev_ok
       io_message      = eo_message ).
  ENDIF.

  CHECK eo_message IS NOT REQUESTED
    AND mo_logger IS INITIAL.  " No errors if show logs
  _check_has_errors( eo_message ).
ENDMETHOD.


METHOD query_row.
  DATA(lt_table) = query_tab( iv_query         = iv_query
                              is_query_options = is_query_options
                              it_sel_param     = it_sel_param ).

  DATA(lr_key) = _read_by_index( ir_table = REF #( lt_table )
                                 iv_index = 1 ).

  ASSIGN lr_key->* TO FIELD-SYMBOL(<ls_key>) CASTING TYPE /bobf/conf_key.
  rv_key = <ls_key>.
ENDMETHOD.


METHOD query_tab.
  iv_query = COND #( WHEN iv_query IS NOT INITIAL THEN iv_query
                     ELSE mo_metadata->mv_select_by_elements ).
  mo_service->query(
    EXPORTING iv_query_key            = iv_query
              it_selection_parameters = it_sel_param
              is_query_options        = is_query_options
    IMPORTING eo_message              = DATA(lo_message)
              et_key                  = rt_key ).

  _check_has_errors( lo_message ).
ENDMETHOD.


METHOD retrieve_by_assoc_row.
  DATA(lr_table) = retrieve_by_assoc_tab(
      iv_parent_type = iv_parent_type
      iv_key         = iv_key
      iv_assoc_type  = iv_assoc_type
      iv_edit_mode   = iv_edit_mode
      is_parameters  = is_parameters ).

  rr_data = _read_by_index( ir_table = lr_table
                            iv_index = 1 ).
ENDMETHOD.


METHOD retrieve_by_assoc_tab.
  " ROOT node by default
  iv_parent_type = COND #( WHEN iv_parent_type IS NOT INITIAL THEN iv_parent_type
                           ELSE mo_metadata->mv_root_node ).
  mo_metadata->get_node_assoc( EXPORTING iv_parent_type = iv_parent_type
                                         iv_assoc_type  = iv_assoc_type
                               IMPORTING er_data_table  = rr_table ).
  FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
  ASSIGN rr_table->* TO <lt_table>.

  mo_service->retrieve_by_association(
    EXPORTING iv_node_key    = iv_parent_type
              it_key         = VALUE #( ( key = iv_key ) )
              iv_association = iv_assoc_type
              iv_fill_data   = abap_true
              is_parameters  = is_parameters
              iv_edit_mode   = iv_edit_mode
    IMPORTING eo_message     = DATA(lo_message)
              et_data        = <lt_table> ).

  _check_has_errors( lo_message ).
ENDMETHOD.


METHOD retrieve_row.
  DATA(lr_table) = retrieve_tab(
      iv_node_type      = iv_node_type
      it_key            = VALUE #( ( key = iv_key ) )
      iv_edit_mode      = iv_edit_mode ).

  rr_data = _read_by_index( ir_table = lr_table
                            iv_index = 1 ).
ENDMETHOD.


METHOD retrieve_tab.
  " ROOT node by default
  iv_node_type = COND #( WHEN iv_node_type IS NOT INITIAL THEN iv_node_type
                         ELSE mo_metadata->mv_root_node ).
  mo_metadata->get_node( EXPORTING iv_node_type  = iv_node_type
                         IMPORTING er_data_table = rr_table ).

  FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
  ASSIGN rr_table->* TO <lt_table>.

  mo_service->retrieve(
    EXPORTING iv_node_key  = iv_node_type
              it_key       = it_key
              iv_edit_mode = iv_edit_mode
              iv_fill_data = iv_fill_data
    IMPORTING eo_message   = DATA(lo_message)
              et_data      = <lt_table> ).

  _check_has_errors( lo_message ).
ENDMETHOD.


METHOD save.
  ev_ok = abap_true.

  mo_transaction->save(
  IMPORTING ev_rejected = DATA(lv_rejected)
            eo_message  = eo_message ).

  ev_ok = xsdbool( lv_rejected <> abap_true ).
  IF eo_message IS NOT INITIAL AND eo_message->check( ).
    ev_ok = abap_false.
  ENDIF.

  CHECK eo_message IS NOT REQUESTED.
  _check_has_errors( eo_message ).
ENDMETHOD.


METHOD _check_has_errors.
  CHECK io_message IS NOT INITIAL
    AND io_message->check( ) = abap_true.

  io_message->get_messages(
    IMPORTING
      et_message = DATA(lt_message) ).

  " 1st error
  LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<ls_message>) WHERE severity CA 'AXE'
                                                            AND message IS NOT INITIAL.
    DATA(lo_previous) = <ls_message>-message.
    DATA(ls_textid)   = zcl_bopf_messages=>get_msg_textid( io_error = lo_previous ).
    EXIT.
  ENDLOOP.

  RAISE EXCEPTION TYPE /bobf/cx_dac
    EXPORTING
      textid     = ls_textid
      previous   = lo_previous
      mo_message = io_message.
ENDMETHOD.


METHOD _read_by_index.
  IF ir_table IS NOT BOUND.
    zcl_bopf_messages=>raise_error( iv_message = 'Table read operation is failed'(trf) ).
  ENDIF.

  FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
  ASSIGN ir_table->* TO <lt_table>.

  READ TABLE <lt_table> REFERENCE INTO rr_data INDEX iv_index.
  CHECK sy-subrc <> 0.

  zcl_bopf_messages=>raise_error( iv_message = |Index { iv_index } out of range| ).
ENDMETHOD.
ENDCLASS.
