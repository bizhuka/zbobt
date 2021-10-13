*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_logger IMPLEMENTATION.
  METHOD constructor.
    mo_owner = io_owner.
  ENDMETHOD.

  METHOD add.
    APPEND VALUE #(
     t_modif   = it_modification
     s_action  = is_action
     o_message = io_message
     time      = sy-uzeit
     icon      = COND #( WHEN iv_ok <> abap_true THEN icon_protocol )
     count     = lines( it_modification )
     code      = icon_abap ) TO mt_log[] ASSIGNING FIELD-SYMBOL(<ls_log>).

    IF <ls_log>-s_action IS NOT INITIAL.
      _get_data_info( EXPORTING ir_data  = <ls_log>-s_action-r_params
                      IMPORTING ev_struc = <ls_log>-v_params ).
    ENDIF.

    CHECK mo_menu IS INITIAL.
    mo_menu = NEW #( io_handler = me ).
    mo_menu->create_toolbar( it_menu        = VALUE #( ( icon      = icon_protocol
                                                         function  = 'SHOW_LOGS'
                                                         quickinfo = |Logs of { mo_owner->mo_metadata->mv_bopf_name }| ) )
                             iv_check_tcode = abap_false ).
  ENDMETHOD.

  METHOD _on_button_pressed.
    CHECK fcode = 'SHOW_LOGS'.

    DATA(lv_title) = |Logs of { mo_owner->mo_metadata->mv_bopf_name }|.
    NEW zcl_eui_alv( ir_table  = REF #( mt_log )
                     is_layout = VALUE #( grid_title = lv_title
                                          no_rowmark = 'X'
                                          no_toolbar = 'X' )
                     it_mod_catalog = VALUE #( hotspot = 'X' ( fieldname = 'ICON'     coltext = 'Logs'(log) )
                                                             ( fieldname = 'COUNT'    coltext = 'Modifications count'(mod) )
                                                             ( fieldname = 'CODE'     coltext = 'ABAP code'(abp) )
                                                             ( fieldname = 'V_PARAMS' coltext = 'Action parameters'(par) ) )
    )->popup( iv_col_end = 120
              iv_row_end = 15
    )->set_status( VALUE #( title = lv_title )
    )->show( io_handler = me ).
  ENDMETHOD.

  METHOD _on_hotspot_click.
    ASSIGN mt_log[ e_row_id-index ] TO FIELD-SYMBOL(<ls_log>).
    CHECK sy-subrc = 0.

    CASE e_column_id-fieldname.
      WHEN 'ICON'.
        NEW zcl_bopf_messages(
        )->add_from_message( <ls_log>-o_message
        )->show( ).

      WHEN 'COUNT'.
        mr_log = REF #( <ls_log> ).
        DATA(lv_title) = |Modifications of { mo_owner->mo_metadata->mv_bopf_name } at { <ls_log>-time TIME = USER }|.
        NEW zcl_eui_alv( ir_table  = REF #( <ls_log>-t_modif )
                         is_layout = VALUE #( grid_title = lv_title
                                              no_rowmark = 'X'
                                              no_toolbar = 'X'
                                              sel_mode   = 'B' )
                         it_mod_catalog = VALUE #( tech = 'X' ( fieldname = 'DATA'  )
                                                              ( fieldname = 'CHANGED_FIELDS' )
                                                   tech = ' ' ( fieldname = 'NODE'        coltext   = 'NODE' )
                                                              ( fieldname = 'CHANGE_MODE' outputlen = 10 )
                                                              ( fieldname = 'NODE_CAT'    coltext   = 'NODE_CAT' )
                                                              ( fieldname = 'KEY'         coltext   = 'KEY' hotspot = 'X' )
                                                              ( fieldname = 'ASSOCIATION' coltext   = 'ASSOCIATION' )
                                                              ( fieldname = 'SOURCE_NODE' coltext   = 'SOURCE_NODE' )
                                                              ( fieldname = 'SOURCE_KEY'  coltext   = 'SOURCE_KEY' )
                                                              ( fieldname = 'ROOT_KEY'    coltext   = 'ROOT_KEY' ) )
        )->popup( iv_col_end     = 170
                  iv_row_end     = 20
        )->set_status( VALUE #( title = lv_title )
        )->show( io_handler      = me
                 iv_handlers_map = |_ON_DATA_HOTSPOT_CLICK| ).

      WHEN 'V_PARAMS'.
        _show_structure( <ls_log>-s_action-r_params ).

      WHEN 'CODE'.
        DATA(lv_code) = _create_code( COND #( WHEN <ls_log>-t_modif[] IS NOT INITIAL
                                              THEN _create_modif( <ls_log>-t_modif )
                                              ELSE _create_action( <ls_log>-s_action ) ) ).
        NEW zcl_eui_memo( ir_text     = REF #( lv_code )
                          iv_editable = abap_false
        )->popup(
        )->show( ).

    ENDCASE.
  ENDMETHOD.

  METHOD _create_modif.
    APPEND |  DATA(lt_mod) = VALUE /bobf/t_frw_modification(| TO rt_code.
    DATA(lt_decl) = VALUE stringtab( ).

    DATA lt_key TYPE STANDARD TABLE OF /bobf/s_frw_modification-key. " VALUE /bobf/t_frw_key2( ).
    LOOP AT it_modif ASSIGNING FIELD-SYMBOL(<ls_modif>).
      IF sy-tabix <> 1.
        APPEND INITIAL LINE TO rt_code.
      ENDIF.

      DATA(lv_key_name)         = ||.
      DATA(lv_node_name)        = ||.
      DATA(lv_source_key_name)  = ||.
      DATA(lv_source_node_node) = ||.

      DO 2 TIMES.
        CASE sy-index.
          WHEN 1.
            DATA(lv_key)      = <ls_modif>-key.
            DATA(lv_node)     = <ls_modif>-node.
            DATA(lr_key_name) = REF #( lv_key_name ).
            DATA(lr_node)     = REF #( lv_node_name ).
          WHEN 2.
            lv_key      = <ls_modif>-source_key.
            lv_node     = <ls_modif>-source_node.
            lr_key_name = REF #( lv_source_key_name ).
            lr_node     = REF #( lv_source_node_node ).
        ENDCASE.
        CHECK lv_key IS NOT INITIAL.

        DATA(lv_new_decl) = abap_false.
        ASSIGN lt_key[ table_line = lv_key ] TO FIELD-SYMBOL(<lv_key>).
        IF sy-subrc <> 0.
          lv_new_decl = abap_true.
          APPEND lv_key TO lt_key.
        ENDIF.

        lr_key_name->* = |lv_key{ sy-tabix }|.
        mo_owner->mo_metadata->get_intf_node_name(
         EXPORTING iv_node      = lv_node
         IMPORTING ev_full_name = lr_node->* ).

        CHECK lv_new_decl = abap_true.
        IF <ls_modif>-change_mode = /bobf/if_frw_c=>sc_modify_create.
          APPEND |  DATA({ lr_key_name->* }) = /bobf/cl_frw_factory=>get_new_key( ).| TO lt_decl.
        ELSE.
          APPEND |  DATA({ lr_key_name->* }) = `{ lv_key }`.| TO lt_decl.
        ENDIF.
      ENDDO.

      " Only 3 change modes
      DATA(lv_change_mode) = SWITCH #( <ls_modif>-change_mode
           WHEN /bobf/if_frw_c=>sc_modify_create THEN |/BOBF/IF_FRW_C=>SC_MODIFY_CREATE|
           WHEN /bobf/if_frw_c=>sc_modify_delete THEN |/BOBF/IF_FRW_C=>SC_MODIFY_DELETE|
           WHEN /bobf/if_frw_c=>sc_modify_update THEN |/BOBF/IF_FRW_C=>SC_MODIFY_UPDATE| ).
      DATA(lv_data) = _get_data_decl( ir_data     = <ls_modif>-data
                                      iv_key_val  = CONV #( <ls_modif>-key )
                                      iv_key_name = lv_key_name ).

      APPEND | ( key         = { lv_key_name }|      TO rt_code.
      APPEND |    data        = { lv_data }|         TO rt_code.
      APPEND |    change_mode = { lv_change_mode }|  TO rt_code.
      APPEND |    node        = { lv_node_name }|    TO rt_code.

      IF lv_source_key_name IS NOT INITIAL.
        mo_owner->mo_metadata->get_intf_node_name_nested(
          EXPORTING iv_node      = <ls_modif>-association
                    iv_struc     = |SC_ASSOCIATION|
          IMPORTING ev_full_name = DATA(lv_assoc_node) ).
        APPEND |    source_key  = { lv_source_key_name  }|  TO rt_code.
        APPEND |    source_node = { lv_source_node_node }|  TO rt_code.
        APPEND |    association = { lv_assoc_node }|        TO rt_code.
      ENDIF.

      ASSIGN rt_code[ lines( rt_code ) ] TO FIELD-SYMBOL(<lv_last_line>).
      <lv_last_line> = |{ <lv_last_line> } )|.
    ENDLOOP.

    ASSIGN rt_code[ lines( rt_code ) ] TO <lv_last_line>.
    <lv_last_line> = |{ <lv_last_line> } ).|.

    INSERT LINES OF lt_decl INTO rt_code INDEX 3.

    APPEND |  lo_manager->modify( EXPORTING it_modification = lt_mod |             TO rt_code.
    APPEND |                      IMPORTING eo_message      = DATA(lo_message)|    TO rt_code.
    APPEND |                                ev_ok           = DATA(lv_ok) ).|      TO rt_code.
    APPEND |  CHECK lv_ok <> abap_true.|                                           TO rt_code.
  ENDMETHOD.

  METHOD _create_action.
    mo_owner->mo_metadata->get_intf_node_name_nested(
      EXPORTING iv_node      = is_action-action_node
                iv_struc     = |SC_ACTION|
      IMPORTING ev_full_name = DATA(lv_act_key) ).

    DATA(lv_data) = _get_data_decl( is_action-r_params ).
    DATA(lv_keys) = concat_lines_of(
      table = VALUE stringtab( FOR ls_key IN is_action-t_key ( |( key = `{ ls_key-key }` )| ) )
      sep = ` ` ).

    APPEND |  lo_manager->do_action( EXPORTING iv_act_key    = { lv_act_key } |        TO rt_code.
    APPEND |                                   it_key        = VALUE #( { lv_keys } )| TO rt_code.
    APPEND |                                   is_parameters = { lv_data }|            TO rt_code.
    APPEND |                         IMPORTING eo_message    = DATA(lo_message)|       TO rt_code.
    APPEND |                                   et_failed_key = DATA(lt_failed_key) ).| TO rt_code.
    APPEND |  CHECK lt_failed_key[] IS NOT INITIAL.|                                   TO rt_code.
  ENDMETHOD.

  METHOD _create_code.
    DATA(lt_code) = VALUE stringtab( ( |  DATA(lo_manager) = zcl_bopf_manager=>create( '{ mo_owner->mo_metadata->mv_bopf_name }' ).| )
                                     ( || ) ).
    APPEND LINES OF it_code TO lt_code.

    " Handle erros
    APPEND |  NEW zcl_bopf_messages( )->add_from_message( lo_message )->show( ).|  TO lt_code.

    rv_code = concat_lines_of( table = lt_code sep = cl_abap_char_utilities=>cr_lf ).
  ENDMETHOD.

  METHOD _get_data_decl.
    _get_data_info( EXPORTING ir_data  = ir_data
                    IMPORTING et_field = DATA(lt_field)
                              ev_struc = DATA(lv_struc) ).
    DATA(lt_decl) = VALUE stringtab( ( | NEW { lv_struc }(| ) ).

    LOOP AT lt_field ASSIGNING FIELD-SYMBOL(<ls_field>).
      APPEND |                         { <ls_field>-field } = {
                    COND #( WHEN <ls_field>-field = 'KEY' AND iv_key_val = <ls_field>-value
                    THEN iv_key_name
                    ELSE |'{ <ls_field>-value }'| ) }| TO lt_decl.
    ENDLOOP.
    ASSIGN lt_decl[ lines( lt_decl ) ] TO FIELD-SYMBOL(<lv_last_line>).
    <lv_last_line> = |{ <lv_last_line> } )|.

    rv_code = concat_lines_of( table = lt_decl sep = cl_abap_char_utilities=>cr_lf ).
  ENDMETHOD.

  METHOD _show_structure.
    CHECK ir_data IS NOT INITIAL.

    _get_data_info( EXPORTING ir_data  = ir_data
                    IMPORTING et_field = DATA(lt_field)
                              ev_struc = DATA(lv_struc) ).
    DATA(lv_title) = |DDIC type `{ lv_struc }`|.
    NEW zcl_eui_alv( ir_table  = REF #( lt_field )
                     is_layout = VALUE #( no_rowmark = 'X'
                                          no_toolbar = 'X'
                                          grid_title = lv_title )
                     it_mod_catalog = VALUE #( ( fieldname = 'VALUE' coltext = 'Value'(val) ) )
    )->popup( iv_col_end     = 80
              iv_row_end     = 15
    )->set_status( VALUE #( title = lv_title )
    )->show( ).
  ENDMETHOD.

  METHOD _on_data_hotspot_click.
    CHECK e_column_id-fieldname = 'KEY'
      AND mr_log IS NOT INITIAL.

    ASSIGN mr_log->t_modif[ e_row_id-index ] TO FIELD-SYMBOL(<ls_modif>).
    CHECK <ls_modif> IS ASSIGNED.

    _show_structure( <ls_modif>-data ).
  ENDMETHOD.

  METHOD _get_data_info.
    CLEAR: ev_struc,
           et_field.

    TRY.
        DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( ir_data ) ).
        ev_struc = lo_struc->get_relative_name( ).
      CATCH cx_sy_move_cast_error INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    ASSIGN ir_data->* TO FIELD-SYMBOL(<ls_data>).
    LOOP AT lo_struc->components[] ASSIGNING FIELD-SYMBOL(<ls_comp>).
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_value>).
      CHECK <lv_value> IS NOT INITIAL.
      APPEND VALUE #( field = <ls_comp>-name
                      value = <lv_value> ) TO et_field.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
