*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.
*  METHOD initialization.
*  ENDMETHOD.

  METHOD start_of_selection.
    TRY.
        DATA(lo_ui_node) = NEW zcl_bopf_ui_node(
          iv_bopf_name = _bo_name
        ).
        DATA(lr_table) = lo_ui_node->show_selection_screen( ).
        CHECK lr_table IS NOT INITIAL.

        lo_ui_node->show_alv( lr_table ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        DATA(lv_message) = lo_error->get_text( ).

        "IF lo_error IS INSTANCE OF if_t100_message. "7.40 compatible
        TRY.
            DATA(lo_message) = CAST if_t100_message( lo_error ).
            IF lo_message->t100key-msgid = '/BOBF/FRW_COMMON' AND lo_message->t100key-msgno = 101.
              lv_message = |{ lv_message }. { 'Change edit mode to ''0'' - Only Read Mode'(chm) }|.
            ENDIF.
          CATCH cx_sy_move_cast_error.
            CLEAR lo_message.
        ENDTRY.

        MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
