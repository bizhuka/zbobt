*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section


CLASS lcl_dynamic_screen DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_ddic_based
        IMPORTING iv_struct        TYPE csequence
        RETURNING VALUE(ro_screen) TYPE REF TO lcl_dynamic_screen,

      create_node_based
        IMPORTING io_owner         TYPE REF TO zcl_bopf_ui_node
        RETURNING VALUE(ro_screen) TYPE REF TO lcl_dynamic_screen
        RAISING   /bobf/cx_frw,

      get_default_edit_mode
        RETURNING VALUE(rv_edit_mode) TYPE /bobf/conf_edit_mode,

      set_default_edit_mode
        IMPORTING iv_edit_mode TYPE /bobf/conf_edit_mode.

    TYPES:
      BEGIN OF ts_screen_base,
        v_max_row   TYPE sytabix,
        v_edit_mode TYPE /bobf/conf_edit_mode,
      END OF ts_screen_base.

    DATA:
      mv_prog_seed TYPE string,
      mr_context   TYPE REF TO data.

    METHODS:
      show
        IMPORTING it_pbo     TYPE zcl_eui_screen=>tt_customize OPTIONAL
        EXPORTING es_context TYPE REF TO data
                  ev_cancel  TYPE abap_bool,

      get_node_screen_info
        IMPORTING io_owner     TYPE REF TO zcl_bopf_ui_node
        EXPORTING ev_max_row   TYPE sytabix
                  ev_edit_mode TYPE /bobf/conf_edit_mode
                  et_sel_param TYPE /bobf/t_frw_query_selparam.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_assoc DEFINITION DEFERRED.
CLASS lcl_tab_info DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_component_path,
        fieldname          TYPE fieldname,
        path               TYPE fieldname,
        type               TYPE REF TO cl_abap_typedescr,
        ref_curr_fieldname TYPE fieldname,
      END OF ts_component_path .
    TYPES:
      tt_component_path TYPE HASHED TABLE OF ts_component_path WITH UNIQUE KEY fieldname
                                                               WITH UNIQUE HASHED KEY path COMPONENTS path.
    DATA:
      mr_table          TYPE REF TO data,
      mt_component_path TYPE tt_component_path,
      mo_owner          TYPE REF TO zcl_bopf_ui_node.

    CLASS-METHODS:
      create_by_comp
        IMPORTING ir_add_flds TYPE REF TO data
                  it_comp     TYPE cl_abap_structdescr=>component_table
        EXPORTING er_line     TYPE REF TO data
                  er_table    TYPE REF TO data
        RAISING   /bobf/cx_frw,

      create_ext_fieds
        IMPORTING io_message    TYPE REF TO /bobf/if_frw_message
        RETURNING VALUE(rs_ext) TYPE zcl_bopf_ui_node=>ts_table_ext,

      refresh_grid
        IMPORTING io_grid    TYPE REF TO cl_gui_alv_grid
                  iv_refresh TYPE abap_bool DEFAULT abap_true
                  iv_message TYPE csequence
                  iv_msgty   TYPE symsgty DEFAULT 'S'.
    METHODS:
      constructor
        IMPORTING
                  io_owner TYPE REF TO zcl_bopf_ui_node
        RAISING   /bobf/cx_frw,

      make_table
        IMPORTING io_assoc TYPE REF TO lcl_assoc
        RAISING   /bobf/cx_frw,

      is_editable
        RETURNING VALUE(rv_editable) TYPE abap_bool,

      _get_flat_comps
        IMPORTING ir_line            TYPE REF TO data
        RETURNING VALUE(rt_comp_src) TYPE cl_abap_structdescr=>component_table,


      _get_fake_names_comps
        IMPORTING it_comp_src         TYPE cl_abap_structdescr=>component_table
                  it_catalog          TYPE lvc_t_fcat
        RETURNING VALUE(rt_comp_dest) TYPE cl_abap_structdescr=>component_table,

      bopf_to_ui
        IMPORTING
                  is_src         TYPE any
        RETURNING VALUE(rr_dest) TYPE REF TO data,

      ui_to_bopf
        IMPORTING
                  ir_src         TYPE REF TO data
                  it_good_cells  TYPE lvc_t_modi         OPTIONAL
                  ir_refresh     TYPE REF TO   abap_bool OPTIONAL
        RETURNING VALUE(rr_dest) TYPE REF TO data.
ENDCLASS.
**********************************************************************
**********************************************************************

CLASS lcl_action DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_action,
        act_name        TYPE /bobf/act_list-act_name,
        act_key         TYPE /bobf/act_list-act_key,
        act_class       TYPE /bobf/act_list-act_class,
        act_cardinality TYPE /bobf/act_list-act_cardinality,
        param_data_type TYPE /bobf/act_list-param_data_type,
        fcode           TYPE syucomm,
        desc            TYPE string,
      END OF ts_action,
      tt_action TYPE HASHED TABLE OF ts_action WITH UNIQUE KEY act_name.

    DATA:
      mt_action TYPE tt_action,
      mo_owner  TYPE REF TO zcl_bopf_ui_node.

    METHODS:
      constructor
        IMPORTING
          io_owner TYPE REF TO zcl_bopf_ui_node,

      add_functions
        IMPORTING
          iv_bopf_name TYPE csequence
          iv_bo_key    TYPE xsequence
          io_grid      TYPE REF TO cl_gui_alv_grid
          iv_disabled  TYPE abap_bool
        CHANGING
          ct_toolbar   TYPE ttb_button,

      _check_action IMPORTING is_action   TYPE ts_action
                              io_grid     TYPE REF TO cl_gui_alv_grid
                    CHANGING  cv_disabled TYPE abap_bool,

      is_action
        IMPORTING
                  io_grid             TYPE REF TO cl_gui_alv_grid
                  iv_ucomm            TYPE syucomm
        RETURNING VALUE(rv_is_action) TYPE abap_bool.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_assoc DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_assoc TYPE STANDARD TABLE OF zcl_bopf_ui_node=>ts_assoc WITH DEFAULT KEY.

    DATA:
      mt_assoc       TYPE tt_assoc,
      mo_owner       TYPE REF TO zcl_bopf_ui_node,
      mv_key         TYPE /bobf/conf_key,
      mo_parent_grid TYPE REF TO cl_gui_alv_grid.

    METHODS:
      constructor
        IMPORTING
          io_owner TYPE REF TO zcl_bopf_ui_node,

      show_all_assoc
        IMPORTING
          is_line TYPE any
          io_grid TYPE REF TO cl_gui_alv_grid,

      get_bo_info
        IMPORTING
                  is_assoc          TYPE zcl_bopf_ui_node=>ts_assoc
        RETURNING VALUE(rs_bo_info) TYPE zcl_bopf_ui_node=>ts_bo_info,

      _read_by_assoc
        IMPORTING is_assoc       TYPE zcl_bopf_ui_node=>ts_assoc
                  ir_parameters  TYPE REF TO data OPTIONAL
        EXPORTING es_create_info TYPE zcl_bopf_ui_node=>ts_create_info
                  er_table       TYPE REF TO data,

      _get_copy_map
        IMPORTING is_catalog         TYPE lvc_s_fcat
                  it_catalog         TYPE lvc_t_fcat
        RETURNING VALUE(rt_copy_map) TYPE zcl_bopf_ui_node=>ts_bo_info-copy_map,

      is_name_match
        IMPORTING iv_name1     TYPE csequence
                  iv_name2     TYPE csequence
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      _on_hotspot_click  FOR EVENT hotspot_click OF cl_gui_alv_grid ##RELAX
        IMPORTING
          e_row_id.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS zcl_bopf_ui_node DEFINITION LOCAL FRIENDS lcl_action.
CLASS zcl_bopf_ui_node DEFINITION LOCAL FRIENDS lcl_assoc.
CLASS zcl_bopf_ui_node DEFINITION LOCAL FRIENDS lcl_tab_info.
CLASS zcl_bopf_ui_node DEFINITION LOCAL FRIENDS lcl_dynamic_screen.
