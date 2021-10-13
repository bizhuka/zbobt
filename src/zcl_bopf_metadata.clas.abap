class ZCL_BOPF_METADATA definition
  public
  final
  create private

  global friends ZCL_BOPF_MANAGER .

public section.

  data MO_CONFIG type ref to /BOBF/IF_FRW_CONFIGURATION read-only .
  data MV_ROOT_NODE type /BOBF/OBM_NODE_KEY read-only .
  data MV_BO_KEY type /BOBF/OBM_BO_KEY read-only .
  data MV_BOPF_NAME type /BOBF/OBM_OBJ-NAME read-only .

  methods CONSTRUCTOR
    importing
      !IV_BOPF_NAME type CSEQUENCE
    raising
      /BOBF/CX_FRW .
  methods GET_NODE
    importing
      !IV_NODE_TYPE type /BOBF/OBM_NODE_KEY
    exporting
      !ES_NODE type /BOBF/S_CONFRO_NODE
      !ER_DATA type ref to DATA
      !ER_DATA_TABLE type ref to DATA .
  methods GET_NODE_ASSOC
    importing
      !IV_PARENT_TYPE type /BOBF/OBM_NODE_KEY
      !IV_ASSOC_TYPE type /BOBF/OBM_NODE_KEY
    exporting
      !ES_ASSOC type /BOBF/S_CONFRO_ASSOC
      !ER_DATA type ref to DATA
      !ER_DATA_TABLE type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods GET_INTF_NODE_NAME
    importing
      !IV_NODE type /BOBF/OBM_NODE_KEY
    exporting
      !EV_NAME type STRING
      !EV_FULL_NAME type STRING .
  methods GET_INTF_NODE_NAME_NESTED
    importing
      !IV_NODE type /BOBF/OBM_NODE_KEY
      !IV_STRUC type CSEQUENCE
    exporting
      !EV_NAME type STRING
      !EV_FULL_NAME type STRING .
protected section.
private section.

  data MV_INTERFACE type SEOCLASS-CLSNAME .
  data MV_SELECT_ALL type /BOBF/OBM_QUERY_KEY .
  data MV_SELECT_BY_ELEMENTS type /BOBF/OBM_QUERY_KEY .

  methods _GET_QUERY
    importing
      !IV_QUERY type CSEQUENCE
      !IV_ROOT_NAME type CSEQUENCE
    returning
      value(RV_QUERY_KEY) type /BOBF/OBM_QUERY_KEY
    raising
      /BOBF/CX_FRW .
ENDCLASS.



CLASS ZCL_BOPF_METADATA IMPLEMENTATION.


METHOD constructor.
  mv_bopf_name = iv_bopf_name.

  SELECT SINGLE bo_key,
                root_node_key,
                objcat,
                const_interface INTO @DATA(ls_obj)
  FROM /bobf/obm_obj
  WHERE name = @mv_bopf_name.
  IF sy-subrc <> 0.
    zcl_bopf_messages=>raise_error( iv_message = |'{ mv_bopf_name }' BOPF is not found | ).
  ENDIF.

  mv_bo_key    = ls_obj-bo_key.
  mv_interface = ls_obj-const_interface.
  mo_config    = /bobf/cl_frw_factory=>get_configuration( mv_bo_key ).

  mv_root_node = ls_obj-root_node_key.
  get_intf_node_name( EXPORTING iv_node = mv_root_node
                      IMPORTING ev_name = DATA(lv_root_name) ).
  IF lv_root_name IS INITIAL.
    zcl_bopf_messages=>raise_error( iv_message = |ROOT node in '{ mv_bopf_name }' is not found| ).
  ENDIF.

  IF ls_obj-objcat = /bobf/if_conf_c=>sc_objcat_bo OR ls_obj-objcat = /bobf/if_conf_c=>sc_objcat_mdo.
    mv_select_all = _get_query( iv_query     = 'SELECT_ALL'
                                iv_root_name = lv_root_name ).
    mv_select_by_elements = _get_query( iv_query     = 'SELECT_BY_ELEMENTS'
                                        iv_root_name = lv_root_name ).
  ENDIF.

  " Check 'ROOT' node
  DATA(lv_name) = |{ mv_interface }=>SC_NODE-{ lv_root_name }|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_root_node>) CASTING TYPE /bobf/obm_node_key.
  IF mv_root_node <> <lv_root_node>.
    zcl_bopf_messages=>raise_error( iv_message = |Wrong version of '{ mv_bopf_name }' was loaded| ).
  ENDIF.

  " Check BO key
  lv_name = |{ mv_interface }=>SC_BO_KEY|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_bo_key>) CASTING TYPE /bobf/obm_bo_key.
  IF sy-subrc <> 0 OR ls_obj-bo_key <> ls_obj-bo_key.
    zcl_bopf_messages=>raise_error( iv_message = |Wrong version of '{ mv_bopf_name }' was loaded| ).
  ENDIF.
ENDMETHOD.


METHOD get_intf_node_name.
  CLEAR: ev_name,
         ev_full_name.

  " All nodes
  DATA(lv_name) = |{ mv_interface }=>SC_NODE|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<ls_sc_node>).
  DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <ls_sc_node> ) ).

  LOOP AT lo_struc->components ASSIGNING FIELD-SYMBOL(<ls_comp>).
    DATA(lv_full_name) = |{ mv_interface }=>SC_NODE-{ <ls_comp>-name }|.
    ASSIGN (lv_full_name) TO FIELD-SYMBOL(<lv_node_key>).
    CHECK <lv_node_key> = iv_node.

    ev_name      = <ls_comp>-name.
    ev_full_name = lv_full_name.
    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD get_intf_node_name_nested.
  CLEAR: ev_name,
         ev_full_name.

  " All nodes
  DATA(lv_name) = |{ mv_interface }=>{ iv_struc }|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<ls_sc_node>).
  DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <ls_sc_node> ) ).

  LOOP AT lo_struc->components ASSIGNING FIELD-SYMBOL(<ls_comp>).
    DATA(lv_assoc_name) = |{ mv_interface }=>{ iv_struc }-{ <ls_comp>-name }|.
    ASSIGN (lv_assoc_name) TO FIELD-SYMBOL(<lv_node_stuc>).

    " Structure in structure %)
    DATA(lo_struc2) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <lv_node_stuc> ) ).
    LOOP AT lo_struc2->components ASSIGNING FIELD-SYMBOL(<ls_comp2>).
      DATA(lv_full_name) = |{ mv_interface }=>{ iv_struc }-{ <ls_comp>-name }-{ <ls_comp2>-name }|.
      ASSIGN (lv_full_name) TO FIELD-SYMBOL(<lv_node_key>).
      CHECK <lv_node_key> = iv_node.

      ev_name      = <ls_comp>-name.
      ev_full_name = lv_full_name.
      RETURN.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD get_node.
  CLEAR: es_node,
         er_data,
         "er_data_data,
         er_data_table.

  mo_config->get_node(
    EXPORTING iv_node_key = iv_node_type
    IMPORTING es_node     = es_node ).

  IF er_data IS REQUESTED.
    CREATE DATA er_data TYPE (es_node-data_type).
  ENDIF.

*  IF er_data_data IS REQUESTED.
*    CREATE DATA er_data_data TYPE (es_node-data_data_type).
*  ENDIF.

  IF er_data_table IS REQUESTED.
    CREATE DATA er_data_table TYPE (es_node-data_table_type).
  ENDIF.
ENDMETHOD.


METHOD get_node_assoc.
  CLEAR: es_assoc,
         er_data,
         "er_data_data,
         er_data_table.

  mo_config->get_assoc(
    EXPORTING iv_assoc_key = iv_assoc_type
              iv_node_key  = iv_parent_type
    IMPORTING es_assoc     = es_assoc ).

  IF es_assoc-target_node IS NOT BOUND.
    RAISE EXCEPTION TYPE /bobf/cx_conf
      EXPORTING
        textid       = /bobf/cx_conf=>type_not_existing
        mv_type_name = es_assoc-assoc_name.
  ENDIF.

  " Business objects
  IF es_assoc-target_node->node_type = /bobf/if_conf_c=>sc_node_type_bo AND
   ( es_assoc-target_node->data_type IS INITIAL OR es_assoc-target_node->data_table_type IS INITIAL ).
    DATA(lo_manager) = zcl_bopf_manager=>create( es_assoc-target_node->node_esr_name ).
    lo_manager->mo_metadata->get_node(
      EXPORTING iv_node_type  = lo_manager->mo_metadata->mv_root_node
      IMPORTING er_data       = er_data
                er_data_table = er_data_table
                es_node       = DATA(ls_node) ).
    RETURN.
  ENDIF.

  IF er_data IS REQUESTED.
    CREATE DATA er_data TYPE (es_assoc-target_node->data_type).
  ENDIF.

*  IF er_data_data IS REQUESTED.
*    CREATE DATA er_data_data TYPE (es_assoc-target_node->data_data_type).
*  ENDIF.

  IF er_data_table IS REQUESTED.
    CREATE DATA er_data_table TYPE (es_assoc-target_node->data_table_type).
  ENDIF.
ENDMETHOD.


METHOD _get_query.
  DATA(lv_name) = |{ mv_interface }=>SC_QUERY-{ iv_root_name }-{ iv_query }|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_query_key>) CASTING TYPE /bobf/obm_query_key.
  IF sy-subrc <> 0.
    zcl_bopf_messages=>raise_error( iv_message = |{ iv_query } node in '{ mv_bopf_name }' is not found| ).
  ENDIF.

  rv_query_key = <lv_query_key>.
ENDMETHOD.
ENDCLASS.
