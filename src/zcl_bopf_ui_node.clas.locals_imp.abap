*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

**********************************************************************
**********************************************************************
CLASS lcl_dynamic_screen IMPLEMENTATION.
  METHOD create_ddic_based.
    ro_screen = NEW #( ).

    CHECK iv_struct IS NOT INITIAL.
    ro_screen->mv_prog_seed = iv_struct.
    CREATE DATA ro_screen->mr_context TYPE (iv_struct).
  ENDMETHOD.

  METHOD show.
    CLEAR ev_cancel.
    es_context = mr_context.
    CHECK mv_prog_seed IS NOT INITIAL.

    " cache prog name with '%' symbol in it
    DATA(lv_prog) = CONV sycprog( |Z{ mv_prog_seed }| ).
    REPLACE ALL OCCURRENCES OF `/` IN lv_prog WITH ``.

    TRY.
        DATA(lo_screen) = NEW zcl_eui_screen( iv_dynnr   = zcl_eui_screen=>mc_dynnr-dynamic
                                              ir_context = es_context
                                              iv_cprog   = lv_prog ).

      CATCH zcx_eui_exception INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lo_screen->get_dimension( IMPORTING ev_col_end = DATA(lv_col_end) ).
    CHECK lo_screen->customize( it_ = it_pbo
                  )->popup( iv_col_end = lv_col_end
                  )->show( ) <> 'OK'.
    ev_cancel = abap_true.
  ENDMETHOD.

  METHOD create_node_based.
    ro_screen = NEW #( ).
    ro_screen->mv_prog_seed = io_owner->mo_metadata->mv_bopf_name.

    io_owner->_get_node_info( IMPORTING et_catalog = DATA(lt_catalog) ).

    DATA(lt_comp) = VALUE cl_abap_structdescr=>component_table( ).
    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<ls_catalog>) WHERE inttype <> cl_abap_typedescr=>typekind_string.
      IF io_owner->mv_tech <> abap_true.
        CHECK <ls_catalog>-tech <> abap_true.
      ENDIF.

      " Only by KEY field
      IF <ls_catalog>-coltext(1) = '*'.
        CHECK <ls_catalog>-coltext = '*KEY'.
      ENDIF.

      TRY.
          DATA(lr_type) = zcl_eui_type=>create_type_descr(
               "ir_type       = REF #( <lv_field> )
               is_field_desc = VALUE #( name     = <ls_catalog>-fieldname
                                        ui_type  = zcl_eui_type=>mc_ui_type-range
                                        rollname = <ls_catalog>-rollname
                                        sys_type = <ls_catalog>-inttype
                                        decimals = <ls_catalog>-decimals
                                        length   = <ls_catalog>-intlen ) ).
        CATCH zcx_eui_exception INTO DATA(lo_error).
          zcl_bopf_messages=>raise_error( io_error = lo_error ).
      ENDTRY.
      INSERT VALUE #( name = <ls_catalog>-fieldname
                      type = lr_type ) INTO TABLE lt_comp[].
    ENDLOOP.

    lcl_tab_info=>create_by_comp(
     EXPORTING it_comp      = lt_comp
               ir_add_flds  = NEW ts_screen_base(
                               v_max_row   = 250
                               v_edit_mode = get_default_edit_mode( ) )
     IMPORTING er_line      = ro_screen->mr_context ).
  ENDMETHOD.

  METHOD get_default_edit_mode.
    GET PARAMETER ID 'ZBOPF_DEF_EDIT_MODE' FIELD DATA(lv_edit_mode).
    IF sy-subrc <> 0.
      lv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive.
    ENDIF.
    rv_edit_mode = lv_edit_mode.
  ENDMETHOD.

  METHOD set_default_edit_mode.
    DATA(lv_edit_mode) = CONV char1( iv_edit_mode ).
    SET PARAMETER ID 'ZBOPF_DEF_EDIT_MODE' FIELD lv_edit_mode.
  ENDMETHOD.

  METHOD get_node_screen_info.
    CLEAR: ev_max_row,
           ev_edit_mode,
           et_sel_param.
    ASSIGN mr_context->* TO FIELD-SYMBOL(<ls_screen>).

    DATA(lr_base) = CORRESPONDING ts_screen_base( <ls_screen> ).
    ev_edit_mode = lr_base-v_edit_mode.
    ev_max_row   = lr_base-v_max_row.

    io_owner->_get_node_info( IMPORTING et_catalog = DATA(lt_catalog) ).
    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<ls_catalog>).
      " Is SELECT-OPTION is initial ?
      FIELD-SYMBOLS <lt_range> TYPE ANY TABLE.
      ASSIGN COMPONENT <ls_catalog>-fieldname OF STRUCTURE <ls_screen> TO <lt_range>.
      CHECK sy-subrc = 0
        AND <lt_range>[] IS NOT INITIAL.

      LOOP AT <lt_range> ASSIGNING FIELD-SYMBOL(<ls_range>).
        APPEND INITIAL LINE TO et_sel_param ASSIGNING FIELD-SYMBOL(<ls_param>).
        MOVE-CORRESPONDING <ls_range> TO <ls_param>.
        <ls_param>-attribute_name = <ls_catalog>-parameter0.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_tab_info IMPLEMENTATION.
  METHOD constructor.
    mo_owner = io_owner.
  ENDMETHOD.

  METHOD make_table.
    CLEAR: mr_table,
           mt_component_path.

    mo_owner->_get_node_info(
     IMPORTING et_catalog = DATA(lt_catalog)
               er_data    = DATA(lr_line) ).

    " with no .INCLUDEs in it
    DATA(lt_comp_src)  = _get_flat_comps( lr_line ).
    " Names as 'C*'
    DATA(lt_comp_dest) = _get_fake_names_comps( it_comp_src = lt_comp_src
                                                it_catalog  = lt_catalog ).
    create_by_comp(
      EXPORTING ir_add_flds = NEW zcl_bopf_ui_node=>ts_table_ext( )
                it_comp     = lt_comp_dest
      IMPORTING er_table    = mr_table ).

    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<ls_catalog>).
      ASSIGN lt_comp_dest[ name = <ls_catalog>-fieldname ] TO FIELD-SYMBOL(<ls_comp>).
      CHECK sy-subrc = 0.

      INSERT VALUE #( fieldname          = <ls_catalog>-fieldname
                      path               = <ls_catalog>-parameter0
                      type               = <ls_comp>-type
                      " ref to WAERS
                      ref_curr_fieldname = <ls_catalog>-cfieldname ) INTO TABLE mt_component_path.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_by_comp.
    CLEAR: er_line,
           er_table.
    DATA(lt_comp_dest) = it_comp[].

    " At the end of dynamic context
    DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( ir_add_flds ) ).
    INSERT LINES OF lo_struct->get_components( ) INTO TABLE lt_comp_dest[].

    lo_struct = cl_abap_structdescr=>create( lt_comp_dest ).
    IF er_line IS REQUESTED.
      CREATE DATA er_line TYPE HANDLE lo_struct.
      ASSIGN: ir_add_flds->* TO FIELD-SYMBOL(<ls_src>),
              er_line->*     TO FIELD-SYMBOL(<ls_dest>).
      MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
    ENDIF.

    IF er_table IS REQUESTED.
      DATA(lr_table_type) = cl_abap_tabledescr=>create( lo_struct ).
      CREATE DATA er_table TYPE HANDLE lr_table_type.
    ENDIF.
  ENDMETHOD.

  METHOD create_ext_fieds.
    IF io_message IS NOT INITIAL.
      DATA(lt_logs) = NEW zcl_bopf_messages( )->add_from_message( io_message  )->mt_logs[].
    ENDIF.
    rs_ext = VALUE zcl_bopf_ui_node=>ts_table_ext(
      t_logs        = lt_logs
      v_log_icon   = COND #( WHEN lt_logs[] IS NOT INITIAL THEN icon_protocol )
      v_assoc_icon = icon_oo_connection ).
  ENDMETHOD.

  METHOD is_editable.
    rv_editable = COND #( WHEN mo_owner->mv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive THEN abap_true ).
  ENDMETHOD.

  METHOD refresh_grid.
    IF iv_refresh = abap_true.
      DATA(lv_message) = |{ iv_message }.{ COND #( WHEN iv_msgty <> 'E' THEN ` Press SAVE to make change permanent` ) }|.
      MESSAGE lv_message TYPE 'S' DISPLAY LIKE iv_msgty.
      io_grid->refresh_table_display( is_stable = VALUE #( row = 'X' ) ).
    ENDIF.

    cl_gui_cfw=>set_new_ok_code( new_code = 'JUST_4_PBO' ).
    cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD _get_flat_comps.
    DATA(lo_struct)   = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( ir_line ) ).
    rt_comp_src = lo_struct->get_components( ).
    DO.
      DATA(lt_comp_new) = VALUE cl_abap_structdescr=>component_table( ).
      LOOP AT rt_comp_src ASSIGNING FIELD-SYMBOL(<ls_comp>) WHERE as_include = abap_true.
        lo_struct ?= <ls_comp>-type.
        INSERT LINES OF lo_struct->get_components( ) INTO TABLE lt_comp_new[].
      ENDLOOP.

      IF lt_comp_new[] IS INITIAL.
        EXIT.
      ELSE.
        DELETE rt_comp_src WHERE as_include = abap_true.
        INSERT LINES OF lt_comp_new[] INTO TABLE rt_comp_src[].
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD _get_fake_names_comps.
    LOOP AT it_catalog ASSIGNING FIELD-SYMBOL(<ls_catalog>).
      ASSIGN it_comp_src[ name = <ls_catalog>-parameter0 ] TO FIELD-SYMBOL(<ls_comp>).
      DATA(lr_type) = <ls_comp>-type.
      UNASSIGN <ls_comp>.

      INSERT VALUE #( name = <ls_catalog>-fieldname
                      type = lr_type ) INTO TABLE rt_comp_dest[].
    ENDLOOP.
  ENDMETHOD.

  METHOD bopf_to_ui.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    ASSIGN mr_table->* TO <lt_table>.

    FIELD-SYMBOLS <ls_dest> TYPE any.
    CREATE DATA rr_dest LIKE LINE OF <lt_table>.
    ASSIGN rr_dest->* TO <ls_dest>.

    LOOP AT mt_component_path ASSIGNING FIELD-SYMBOL(<ls_path>).
      ASSIGN COMPONENT: <ls_path>-path      OF STRUCTURE is_src    TO FIELD-SYMBOL(<lv_src>),
                        <ls_path>-fieldname OF STRUCTURE <ls_dest> TO FIELD-SYMBOL(<lv_dest>).
      <lv_dest> = <lv_src>.

      UNASSIGN: <lv_src>,
                <lv_dest>.
    ENDLOOP.
  ENDMETHOD.

  METHOD ui_to_bopf.
    IF mt_component_path IS INITIAL.
      rr_dest = ir_src.
      RETURN.
    ENDIF.

    mo_owner->mo_metadata->get_node(
       EXPORTING iv_node_type = mo_owner->mv_node
       IMPORTING er_data      = rr_dest ).
    ASSIGN: rr_dest->* TO FIELD-SYMBOL(<ls_dest>),
            ir_src->*  TO FIELD-SYMBOL(<ls_src>).

    LOOP AT mt_component_path INTO DATA(ls_component_path).
      ASSIGN COMPONENT: ls_component_path-path      OF STRUCTURE <ls_dest> TO FIELD-SYMBOL(<lv_dest>),
                        ls_component_path-fieldname OF STRUCTURE <ls_src>  TO FIELD-SYMBOL(<lv_src>).
      DATA(lv_std) = abap_true.

      " currency formatting ?
      DO 1 TIMES.
        ASSIGN COMPONENT ls_component_path-ref_curr_fieldname OF STRUCTURE <ls_src> TO FIELD-SYMBOL(<lv_ref_curr>).
        CHECK sy-subrc = 0.

        " get decimals of the refered currency
        SELECT SINGLE currdec FROM tcurx INTO @DATA(lv_currdec) WHERE currkey = @<lv_ref_curr>.
        DATA(lv_curr_decimals) = COND #( WHEN sy-subrc = 0 THEN lv_currdec ELSE '2' ).

        " get decimals of the datatype and shift
        DATA(lv_type_decimals) = ls_component_path-type->decimals.
        CHECK lv_curr_decimals = 2 AND lv_type_decimals > 0.

        " recalculate only if the field has been changed (and not if e.g. only the cuky is changed)
        " (et_good_cells contains always only one modification!)
        READ TABLE it_good_cells WITH KEY fieldname = ls_component_path-fieldname TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
        <lv_dest> = <lv_src> * ipow( base = 10 exp = -1 * ( lv_type_decimals - lv_curr_decimals ) ).

        " TODO !!!!!!!!!!!!!!!!!!!!!!!
        <lv_src> = <lv_dest>.
        IF ir_refresh IS NOT INITIAL.
          ir_refresh->* = abap_true.
        ENDIF.

        lv_std = abap_false.
      ENDDO.

      CHECK lv_std = abap_true.
      <lv_dest> = <lv_src>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_action IMPLEMENTATION.
  METHOD constructor.
    mo_owner = io_owner.
  ENDMETHOD.

  METHOD add_functions.
    SELECT d~act_name, d~act_key, d~act_class, d~act_cardinality, d~param_data_type,
           '_ACT_' && d~act_name AS fcode,
           coalesce( t~description, d~act_name ) AS desc  INTO CORRESPONDING FIELDS OF TABLE @mt_action
    FROM /bobf/act_list AS d
       LEFT OUTER JOIN /bobf/act_listt AS t ON t~langu     = @sy-langu
                                           AND t~name      = d~name
                                           AND t~extension = d~extension
                                           AND t~version   = d~version
                                           AND t~act_key   = d~act_key
    WHERE d~name     = @iv_bopf_name
      AND d~bo_key   = @iv_bo_key
      AND d~node_key = @mo_owner->mv_node
      AND d~act_cat  = @/bobf/if_conf_c=>sc_action_standard.

    IF mt_action[] IS NOT INITIAL.
      INSERT VALUE #( butn_type = cntb_btype_sep ) INTO TABLE ct_toolbar.
    ENDIF.

    LOOP AT mt_action ASSIGNING FIELD-SYMBOL(<ls_action>).
      DATA(lv_disabled) = iv_disabled.
      IF lv_disabled <> abap_true AND <ls_action>-param_data_type IS INITIAL.
        _check_action( EXPORTING is_action   = <ls_action>
                                 io_grid     = io_grid
                       CHANGING  cv_disabled = lv_disabled ).
      ENDIF.

      INSERT VALUE #(
         function = <ls_action>-fcode
         icon     = icon_oo_overwrite
         disabled = lv_disabled
         text     = <ls_action>-desc ) INTO TABLE ct_toolbar.
    ENDLOOP.
  ENDMETHOD.

  METHOD _check_action.
    FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
    DATA(lr_alv_table) = zcl_eui_conv=>get_grid_table( io_grid ).
    ASSIGN lr_alv_table->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).
      ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_key>) CASTING TYPE /bobf/conf_key.
      mo_owner->mo_manager->mo_service->check_action(
        EXPORTING iv_act_key    = is_action-act_key
                  it_key        = VALUE #( ( key = <lv_key> ) )
        IMPORTING eo_message    = DATA(lo_message) ).

      CHECK NEW zcl_bopf_messages( iv_severity = 'AXE'
          )->add_from_message( lo_message
          )->mt_logs[] IS NOT INITIAL.
      cv_disabled = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_action.
    CHECK iv_ucomm CP '_ACT_*'.
    rv_is_action = abap_true.

    io_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_row) ).
    IF lt_row[] IS INITIAL.
      MESSAGE 'Please select items to perform the action'(sia) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    ASSIGN mt_action[ act_name = CONV #( iv_ucomm+5 ) ] TO FIELD-SYMBOL(<ls_action>).
    CHECK sy-subrc = 0.

    lcl_dynamic_screen=>create_ddic_based( iv_struct = <ls_action>-param_data_type
    )->show( IMPORTING es_context = DATA(ls_parameters)
                       ev_cancel  = DATA(lv_cancel) ).
    CHECK lv_cancel <> abap_true.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    DATA(lr_table) = zcl_eui_conv=>get_grid_table( io_grid ).
    ASSIGN lr_table->* TO <lt_table>.

    LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<ls_row>).
      ASSIGN <lt_table>[ <ls_row>-index ] TO FIELD-SYMBOL(<ls_line>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_key>) CASTING TYPE /bobf/conf_key.
      mo_owner->mo_manager->mo_service->do_action(
        EXPORTING iv_act_key    = <ls_action>-act_key
                  it_key        = VALUE #( ( key = <lv_key> ) )
                  is_parameters = ls_parameters
        IMPORTING eo_message    = DATA(lo_message) ).

      mo_owner->_refresh_line( EXPORTING iv_key     = <lv_key>
                                         io_message = lo_message
                               CHANGING  cs_line    = <ls_line> ).
    ENDLOOP.

    lcl_tab_info=>refresh_grid( io_grid    = io_grid
                                iv_message = |Action '{ <ls_action>-desc }' is performed| ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_assoc IMPLEMENTATION.
  METHOD constructor.
    mo_owner = io_owner.

    SELECT d~name                  AS _name,
           d~assoc_key             AS _assoc_key,
           d~assoc_type            AS _assoc_type,
           d~assoc_cat             AS _assoc_cat,
           d~assoc_name            AS _assoc_name,
           d~assoc_class           AS _assoc_class,
           d~attr_name_source      AS _attr_name_source,
           d~assoc_resolve         AS _assoc_resolve,
           d~target_node_key       AS _target_node_key,
           d~param_data_type       AS _param_data_type,
           d~cardinality           AS _cardinality,
           d~seckeyname            AS _seckeyname,
           d~change_resolve        AS _change_resolve,
           " Visiblie fields
           coalesce( t~description, d~assoc_name ) AS desc,
           0                                       AS count INTO CORRESPONDING FIELDS OF TABLE @mt_assoc
    FROM /bobf/obm_assoc AS d
       LEFT OUTER JOIN /bobf/obm_assoct AS t ON t~langu     = @sy-langu
                                            AND t~name      = d~name
                                            AND t~extension = d~extension
                                            AND t~version   = d~version
                                            AND t~assoc_key   = d~assoc_key
    WHERE d~name            EQ @mo_owner->mo_metadata->mv_bopf_name
      AND d~bo_key          EQ @mo_owner->mo_metadata->mv_bo_key
      AND d~source_node_key EQ @mo_owner->mv_node
      AND d~change_resolve  NE @space
    ORDER BY assoc_type DESCENDING,
             assoc_cat  ASCENDING.
  ENDMETHOD.

  METHOD get_bo_info.
    CHECK is_assoc-_assoc_cat      = /bobf/if_conf_c=>sc_assoccat_xbo
      AND is_assoc-_assoc_type     = /bobf/if_conf_c=>sc_assoctype_std
      AND is_assoc-_assoc_resolve  = /bobf/if_conf_c=>sc_assoc_resolve_source.
    CHECK zcl_bopf_ui_node=>_one_item_cardinality( is_assoc ) = abap_true.

    SPLIT is_assoc-_assoc_name AT '_' INTO TABLE DATA(lt_bo_name).
    rs_bo_info-bo_name = lt_bo_name[ 1 ].

    mo_owner->_get_node_info( IMPORTING et_catalog = DATA(lt_catalog) ).
    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<ls_catalog>) WHERE f4availabl = abap_true
                                                              AND ref_field IS NOT INITIAL
                                                              AND parameter0 CP |{ rs_bo_info-bo_name }*|
                                                              AND domname <> '/BOBF/CONF_KEY'.
      rs_bo_info-copy_map = _get_copy_map( is_catalog  = <ls_catalog>
                                           it_catalog  = lt_catalog ).
      EXIT.
    ENDLOOP.

    SELECT SINGLE assocb_key,
                  attribute INTO @DATA(ls_binding)
    FROM /bobf/obm_assocb
    WHERE name             EQ @mo_owner->mo_metadata->mv_bopf_name
      AND assoc_key        EQ @is_assoc-_assoc_key
      AND bo_key           EQ @mo_owner->mo_metadata->mv_bo_key
      AND attribute_cat    EQ @/bobf/if_conf_c=>sc_assocbcat_source
      AND from_binding_cat EQ @/bobf/if_conf_c=>sc_assocbcat_xbo.
    rs_bo_info-bind_attribute = COND #( WHEN sy-subrc = 0
                                        THEN ls_binding-attribute
                                        ELSE |{ rs_bo_info-bo_name }_ID_INT| ).
  ENDMETHOD.

  METHOD _get_copy_map.
    DATA(ls_sh_desc) = VALUE shlp_descr( ).
    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname   = is_catalog-ref_table
        fieldname = is_catalog-ref_field
      IMPORTING
        shlp      = ls_sh_desc
      EXCEPTIONS
        OTHERS    = 1.
    CHECK sy-subrc = 0.

    LOOP AT ls_sh_desc-fieldprop ASSIGNING FIELD-SYMBOL(<ls_prop>) WHERE shlpoutput = abap_true.
      ASSIGN ls_sh_desc-fielddescr[ fieldname = <ls_prop>-fieldname ] TO FIELD-SYMBOL(<ls_descr>).
      CHECK sy-subrc = 0 AND <ls_descr>-domname <> '/BOBF/CONF_KEY'.

      ASSIGN it_catalog[ rollname = <ls_descr>-rollname ] TO FIELD-SYMBOL(<ls_catalog>).
      CHECK sy-subrc = 0
        AND is_name_match( iv_name1 = <ls_descr>-fieldname
                           iv_name2 = <ls_catalog>-parameter0 ).
      APPEND VALUE #( src  = <ls_descr>-fieldname
                      dest = <ls_catalog>-parameter0 ) TO rt_copy_map.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_name_match.
    SPLIT: iv_name1 AT `_` INTO TABLE DATA(lt_name1),
           iv_name2 AT `_` INTO TABLE DATA(lt_name2).
    LOOP AT lt_name1 INTO DATA(lv_name1).
      CHECK strlen( lv_name1 ) >= 4.
      LOOP AT lt_name2 INTO DATA(lv_name2) WHERE table_line = lv_name1.
        CHECK strlen( lv_name1 ) >= 4.
        rv_ok = abap_true.
        RETURN.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_all_assoc.
    mo_parent_grid = io_grid.

    ASSIGN COMPONENT 'KEY' OF STRUCTURE is_line TO FIELD-SYMBOL(<lv_key>) CASTING TYPE /bobf/conf_key.
    CHECK sy-subrc = 0.
    mv_key = <lv_key>.

    LOOP AT mt_assoc ASSIGNING FIELD-SYMBOL(<ls_assoc>).
      FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
      _read_by_assoc( EXPORTING is_assoc = <ls_assoc>
                      IMPORTING er_table = DATA(lr_table) ).
      ASSIGN lr_table->* TO <lt_table>.

      <ls_assoc>-count = lines( <lt_table>  ).
    ENDLOOP.

    NEW zcl_eui_alv( ir_table  = REF #( mt_assoc )
                     is_layout = VALUE #( no_rowmark = 'X'
                                          no_toolbar = 'X' )
                     it_mod_catalog = VALUE #( ( fieldname = '_*' tech    = 'X' )
                                               ( fieldname = '*'  hotspot = 'X' ) )
    )->popup( iv_col_beg = 25
              iv_col_end = 65
              iv_row_beg = 3
              iv_row_end = 9
    )->show( io_handler = me ).
  ENDMETHOD.

  METHOD _read_by_assoc.
    es_create_info = VALUE #( source_key = mv_key ).
    TRY.
        IF mo_owner->mo_manager->mo_service IS NOT INITIAL.
          DATA(lo_manager) = mo_owner->mo_manager.
          es_create_info-source_node = mo_owner->mv_node.
          es_create_info-assoc_key   = is_assoc-_assoc_key.
          es_create_info-target_node = is_assoc-_target_node_key.
        ELSE.
          lo_manager = mo_owner->mo_parent->mo_manager.

          DATA(lo_conf) = mo_owner->mo_parent->mo_metadata->mo_config.
          lo_conf->get_assoc_tab( IMPORTING et_assoc = DATA(lt_assoc) ).
          DELETE lt_assoc WHERE source_node_key NE mo_owner->ms_assoc-_target_node_key "#EC CI_SORTSEQ
                             OR change_resolve  EQ space.

          IF lines( mt_assoc ) <> 1 OR lines( lt_assoc ) <> 1.
            zcx_eui_no_check=>raise_sys_error( iv_message = 'Unknown case' ).
          ENDIF.

          DATA(ls_assoc) = lt_assoc[ 1 ].
          es_create_info-source_node = ls_assoc-source_node_key.
          es_create_info-assoc_key   = ls_assoc-assoc_key.
          es_create_info-target_node = ls_assoc-target_node_key.
        ENDIF.

        er_table = lo_manager->retrieve_by_assoc_tab(
          iv_parent_type = es_create_info-source_node
          iv_key         = es_create_info-source_key
          iv_assoc_type  = es_create_info-assoc_key
          iv_edit_mode   = mo_owner->mv_edit_mode
          is_parameters  = ir_parameters ).

      CATCH /bobf/cx_frw INTO DATA(lo_error).
        zcx_eui_no_check=>raise_sys_error( io_error = lo_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD _on_hotspot_click.
    ASSIGN mt_assoc[ e_row_id-index ] TO FIELD-SYMBOL(<ls_assoc>).
    CHECK sy-subrc = 0.

    lcl_dynamic_screen=>create_ddic_based( iv_struct = <ls_assoc>-_param_data_type
    )->show( IMPORTING es_context = DATA(ls_parameters)
                       ev_cancel  = DATA(lv_cancel) ).
    CHECK lv_cancel <> abap_true.

    TRY.
        mo_owner->mo_metadata->get_node_assoc(
         EXPORTING iv_parent_type = mo_owner->mv_node
                   iv_assoc_type  = <ls_assoc>-_assoc_key
         IMPORTING es_assoc       = DATA(ls_assoc) ).

        _read_by_assoc( EXPORTING is_assoc       = <ls_assoc>
                                  ir_parameters  = ls_parameters
                        IMPORTING er_table       = DATA(lr_table)
                                  es_create_info = DATA(ls_create_info) ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF <ls_assoc>-_assoc_cat = /bobf/if_conf_c=>sc_assoccat_xbo OR
       <ls_assoc>-_assoc_cat = /bobf/if_conf_c=>sc_assoccat_object.
      SELECT name, root_node_key AS target_node INTO TABLE @DATA(lt_bo_obj)
      FROM /bobf/obm_obj
      WHERE bo_key = @ls_assoc-target_node->ref_bo_key.
      IF lines( lt_bo_obj ) <> 1.
        zcx_eui_no_check=>raise_sys_error( iv_message = 'Wrong select statement' ).
      ENDIF.

      DATA(ls_bo_obj) = lt_bo_obj[ 1 ].
      IF     ls_assoc-target_node->node_type = /bobf/if_conf_c=>sc_node_type_bo
         AND ls_bo_obj-name <> ls_assoc-target_node->node_esr_name.
        zcx_eui_no_check=>raise_sys_error( iv_message = |Wrong ref { ls_bo_obj-name } <> { ls_assoc-target_node->node_esr_name } | ).
      ENDIF.
    ELSE.
      ls_bo_obj = VALUE #( name        = mo_owner->mo_metadata->mv_bopf_name
                           target_node = <ls_assoc>-_target_node_key ).
    ENDIF.

    TRY.
        <ls_assoc>-_bo = get_bo_info( <ls_assoc> ).
        DATA(lo_sub_node) = NEW zcl_bopf_ui_node(
         iv_bopf_name  = ls_bo_obj-name
         iv_node       = ls_bo_obj-target_node
         iv_tech       = mo_owner->mv_tech ).

        lo_sub_node->mo_parent      = mo_owner.
        lo_sub_node->mv_edit_mode   = mo_owner->mv_edit_mode.
        lo_sub_node->ms_assoc       = <ls_assoc>.
        lo_sub_node->ms_create_info = ls_create_info.

        DATA(lv_cmd) = lo_sub_node->show_alv( lr_table ).
      CATCH /bobf/cx_frw INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF lv_cmd = zcl_bopf_ui_node=>mc_cmd-pick_one.
      mo_owner->_refresh_line( iv_key = mv_key ).
      lcl_tab_info=>refresh_grid( io_grid    = mo_parent_grid
                                  iv_message = 'The association updated'(asu) ).
    ENDIF.

    " Just close previous
    zcl_eui_screen=>top_pai( zcl_eui_screen=>mc_cmd-cancel ).
  ENDMETHOD.
ENDCLASS.
