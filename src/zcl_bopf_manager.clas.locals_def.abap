*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_logger DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_find_key TYPE STANDARD TABLE OF /bobf/s_frw_modification-key WITH DEFAULT KEY, " TYPE /bobf/t_frw_key2,

      BEGIN OF ts_find_src,
        key  TYPE /bobf/s_frw_modification-key,
        node TYPE /bobf/s_frw_modification-node,
      END OF ts_find_src,

      BEGIN OF ts_find_res,
        key_name  TYPE string,
        node_name TYPE string,
      END OF ts_find_res,

      BEGIN OF ts_action,
        r_params    TYPE REF TO data,
        action_node TYPE /bobf/act_key,
        t_key       TYPE /bobf/t_frw_key,
      END OF ts_action,

      BEGIN OF ts_log,
        icon      TYPE icon_d,
        time      TYPE syuzeit,
        t_modif   TYPE /bobf/t_frw_modification,
        v_params  TYPE strukname,
        s_action  TYPE ts_action,
        o_message TYPE REF TO /bobf/if_frw_message,
        count     TYPE i,
        code      TYPE icon_d,
      END OF ts_log,
      tt_log TYPE STANDARD TABLE OF ts_log WITH DEFAULT KEY,

      BEGIN OF ts_field,
        field TYPE fieldname,
        value TYPE string,
      END OF ts_field,
      tt_field TYPE STANDARD TABLE OF ts_field WITH DEFAULT KEY.

    DATA:
      mo_owner    TYPE REF TO zcl_bopf_manager,
      mo_menu     TYPE REF TO zcl_eui_menu,

      mt_log      TYPE tt_log,
      mr_log      TYPE REF TO ts_log,

      mt_all_code TYPE tttext255.

    METHODS:
      constructor
        IMPORTING
          io_owner TYPE REF TO zcl_bopf_manager,

      add
        IMPORTING
          io_message      TYPE REF TO /bobf/if_frw_message
          iv_ok           TYPE abap_bool
          it_modification TYPE /bobf/t_frw_modification OPTIONAL
          is_action       TYPE ts_action                OPTIONAL,

      _on_button_pressed FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING fcode,

      _on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id,

      _on_memo_pai FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          iv_command,

      _on_data_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id,

      _create_modif
        IMPORTING
                  it_modif       TYPE /bobf/t_frw_modification
        RETURNING VALUE(rt_code) TYPE stringtab,

      _insert_fields
        IMPORTING
          it_fields TYPE /bobf/s_frw_modification-changed_fields
        CHANGING
          ct_code   TYPE stringtab,

      _create_action
        IMPORTING
                  is_action      TYPE ts_action
        RETURNING VALUE(rt_code) TYPE stringtab,

      _show_abap_code
        IMPORTING
          is_log TYPE ts_log,

      _show_structure
        IMPORTING
          ir_data TYPE REF TO data,

      _get_data_decl
        IMPORTING
                  ir_data        TYPE REF TO data
                  iv_key_val     TYPE string OPTIONAL
                  iv_key_name    TYPE string OPTIONAL
        RETURNING VALUE(rv_code) TYPE string,

      _get_data_info
        IMPORTING
          ir_data  TYPE REF TO data
        EXPORTING
          ev_struc TYPE strukname
          et_field TYPE tt_field,

      _find_with
        IMPORTING
                  is_find_src        TYPE ts_find_src
                  iv_change_mode     TYPE /bobf/s_frw_modification-change_mode OPTIONAL
                  ct_find_key        TYPE REF TO tt_find_key
                  ct_decl            TYPE REF TO stringtab
        RETURNING VALUE(rs_find_res) TYPE ts_find_res.
ENDCLASS.

CLASS zcl_bopf_manager DEFINITION LOCAL FRIENDS lcl_logger.
