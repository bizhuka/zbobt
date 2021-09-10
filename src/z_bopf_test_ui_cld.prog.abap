*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS:
      start_of_selection.

*      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING
*          er_data_changed
*          e_onf4
*          e_onf4_before
*          e_onf4_after
*          e_ucomm,
*
*      on_context_menue_request FOR EVENT context_menu_request OF cl_gui_alv_grid,
*
*      on_delayed_changed_sel FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid,
*
*      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
*        IMPORTING
*          es_row_no,
*
*      on_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
*        IMPORTING
*          e_object
*          e_ucomm,
*
*      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
*        IMPORTING
*          e_fieldname
*          es_row_no
*          er_event_data,
*
*      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
*        IMPORTING
*          e_modified
*          et_good_cells.

ENDCLASS.
