*&---------------------------------------------------------------------*
*& Program Name: ZOOPBOOK_BOPF_DEMO                                    *
*& Author:       James Wood (jwood@bowdark.com)                        *
*& Date Written: 10/27/2014                                            *
*& Description:  This program demonstrates how to work with the BOPF   *
*&               API using sample BOs provided by SAP.                 *
*&---------------------------------------------------------------------*
REPORT z_bopf_test.

*&--------------------------------------------------------------------*
*& Selection Screen Definition                                        *
*&--------------------------------------------------------------------*

PARAMETERS: p_order TYPE /bobf/demo_sales_order_id,
            p_log   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN:
  BEGIN OF BLOCK blk_toolbar,
    BEGIN OF LINE,
      PUSHBUTTON 2(32) btn_new USER-COMMAND create VISIBLE LENGTH 16,
      PUSHBUTTON 20(32) btn_chg USER-COMMAND change VISIBLE LENGTH 16,
      PUSHBUTTON 38(35) btn_disp USER-COMMAND display VISIBLE LENGTH 16,
      PUSHBUTTON 56(45) btn_delv USER-COMMAND deliver VISIBLE LENGTH 20,
    END OF LINE,
  END OF BLOCK blk_toolbar.

*&--------------------------------------------------------------------*
*& Local Class Definitions                                            *
*&--------------------------------------------------------------------*
CLASS lcl_order_processor DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF co_event,
                 create  TYPE sy-ucomm VALUE 'CREATE',
                 change  TYPE sy-ucomm VALUE 'CHANGE',
                 display TYPE sy-ucomm VALUE 'DISPLAY',
                 deliver TYPE sy-ucomm VALUE 'DELIVER',
               END OF co_event.

    CLASS-METHODS:
      class_constructor,

      process_request IMPORTING iv_event_id TYPE sy-ucomm
                                iv_order_no TYPE /bobf/demo_sales_order_id.

  PRIVATE SECTION.
    CLASS-DATA so_instance TYPE REF TO lcl_order_processor.
    DATA mo_id_range   TYPE REF TO cl_abap_random_int.
    DATA mo_item_range TYPE REF TO cl_abap_random_int.

    DATA mo_manager TYPE REF TO zcl_bopf_manager.

    METHODS:
      constructor,

      init RAISING /bobf/cx_frw,

      create_customer EXPORTING ev_key TYPE /bobf/conf_key
                                ev_id  TYPE /bobf/demo_customer_id
                      RAISING   /bobf/cx_frw,

      create_product EXPORTING ev_key TYPE /bobf/conf_key
                               ev_id  TYPE /bobf/demo_product_id
                     RAISING   /bobf/cx_frw,

      create_order RETURNING VALUE(rv_order_no) TYPE /bobf/demo_sales_order_id
                   RAISING   /bobf/cx_frw,

      change_order IMPORTING iv_order_no TYPE /bobf/demo_sales_order_id
                   RAISING   /bobf/cx_frw,

      display_order IMPORTING iv_order_no TYPE /bobf/demo_sales_order_id
                    RAISING   /bobf/cx_frw,

      process_delivery IMPORTING iv_order_no TYPE /bobf/demo_sales_order_id
                       RAISING   /bobf/cx_frw,

      get_order_by_id IMPORTING iv_order_no         TYPE /bobf/demo_sales_order_id
                      RETURNING VALUE(rv_order_key) TYPE /bobf/conf_key
                      RAISING   /bobf/cx_frw,

      _get_id
        IMPORTING
                  io_random    TYPE REF TO cl_abap_random_int
        RETURNING VALUE(rv_id) TYPE string,

      commit
        IMPORTING
                  io_manager TYPE REF TO zcl_bopf_manager
        RAISING   /bobf/cx_frw,

      report_messages IMPORTING io_message TYPE REF TO /bobf/if_frw_message.
ENDCLASS.

CLASS lcl_order_processor IMPLEMENTATION.
  METHOD process_request.
    "Method-Local Data Declarations:
    DATA lx_bopf_ex TYPE REF TO /bobf/cx_frw.

    "Process the incoming request:
    TRY.
        so_instance->init( ).

        CASE iv_event_id.
          WHEN co_event-create.
            p_order = so_instance->create_order( ).

          WHEN co_event-change.
            so_instance->change_order( iv_order_no ).

          WHEN co_event-display.
            so_instance->display_order( iv_order_no ).

          WHEN co_event-deliver.
            so_instance->process_delivery( iv_order_no ).

        ENDCASE.
      CATCH /bobf/cx_frw INTO lx_bopf_ex.
        MESSAGE lx_bopf_ex TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _get_id.
    rv_id = |{ io_random->get_next( ) ALIGN = LEFT }|.
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT so_instance.
  ENDMETHOD.                 " METHOD class_constructor

  METHOD constructor.
    DATA(lv_seed) = sy-datum + sy-uzeit.

    mo_id_range   = cl_abap_random_int=>create( seed = lv_seed min = 1 max = 999999 ).
    mo_item_range = cl_abap_random_int=>create( seed = lv_seed min = 1 max = 999 ).
  ENDMETHOD.                 " METHOD constructor

  METHOD init.
    mo_manager = zcl_bopf_manager=>create( iv_bopf_name = '/BOBF/DEMO_SALES_ORDER'
                                           iv_log       = p_log ).
  ENDMETHOD.

  METHOD create_order.
    "Create the order ROOT node:
    DATA(lr_s_root) = NEW /bobf/s_demo_sales_order_hdr_k(
      key         = /bobf/cl_frw_factory=>get_new_key( )
      order_id    = _get_id( mo_id_range )
      sales_org   = 'AMER'
      amount      = '250.00'
      amount_curr = 'USD' ).

    create_customer( IMPORTING ev_key = lr_s_root->customer_id_int
                               ev_id  = lr_s_root->customer_id ).

    "Create the order description:
    DATA(lr_s_root_text) = NEW /bobf/s_demo_short_text_k(
        key      = /bobf/cl_frw_factory=>get_new_key( )
        language = sy-langu
        text     = |Order # { lr_s_root->order_id }| ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node        = /bobf/if_demo_sales_order_c=>sc_node-root
       change_mode = /bobf/if_frw_c=>sc_modify_create
       key         = lr_s_root->key
       data        = lr_s_root )

     ( node        = /bobf/if_demo_sales_order_c=>sc_node-root_text
       change_mode = /bobf/if_frw_c=>sc_modify_create
       source_node = /bobf/if_demo_sales_order_c=>sc_node-root
       association = /bobf/if_demo_sales_order_c=>sc_association-root-root_text
       source_key  = lr_s_root->key
       key         = lr_s_root_text->key
       data        = lr_s_root_text ) ).

    mo_manager->modify( EXPORTING it_modification = lt_mod
                        IMPORTING ev_ok           = DATA(lv_changed) ).
    CHECK lv_changed = abap_true.

    mo_manager->save( ).
    rv_order_no = lr_s_root->order_id.
    MESSAGE |Order { rv_order_no } was created.| TYPE 'S'.
  ENDMETHOD.                 " METHOD create_order

  METHOD change_order.
    "Create a new line item:
    DATA(lr_s_item) = NEW /bobf/s_demo_sales_order_itm_k(
            key         = /bobf/cl_frw_factory=>get_new_key( )
            parent_key  = get_order_by_id( iv_order_no )
            item_no     = _get_id( mo_item_range )
            amount      = '10.00'
            amount_curr = 'USD' ).
    create_product( IMPORTING ev_key = lr_s_item->product_id_int
                              ev_id  = lr_s_item->product_id ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
    ( node        = /bobf/if_demo_sales_order_c=>sc_node-item
      change_mode = /bobf/if_frw_c=>sc_modify_create
      source_node = /bobf/if_demo_sales_order_c=>sc_node-root
      association = /bobf/if_demo_sales_order_c=>sc_association-root-item
      source_key  = lr_s_item->parent_key
      key         = lr_s_item->key
      data        = lr_s_item ) ).

    mo_manager->modify( EXPORTING it_modification = lt_mod
                        IMPORTING ev_ok           = DATA(lv_changed) ).
    CHECK lv_changed = abap_true.

    mo_manager->save( ).
    MESSAGE |Added item { lr_s_item->item_no }.| TYPE 'S'.
  ENDMETHOD.                 " METHOD change_order

  METHOD display_order.
    "Method-Local Data Declarations:
    DATA: BEGIN OF ls_order_info.
            INCLUDE TYPE /bobf/s_demo_sales_order_hdr_k.
            DATA:   text TYPE /bobf/demo_description,
          END OF ls_order_info.

    DATA(lv_key) = get_order_by_id( iv_order_no ).

    "Lookup the order header details:
    DATA(lr_sales_order) = CAST /bobf/s_demo_sales_order_hdr_k(
        mo_manager->retrieve_row( iv_key = lv_key  ) ).
    ls_order_info = CORRESPONDING #( lr_sales_order->* ).

    "Lookup the order short text description:
    DATA(lt_text) = CAST /bobf/t_demo_short_text_k(
      mo_manager->retrieve_by_assoc_tab( iv_key        = lv_key
                                         iv_assoc_type = /bobf/if_demo_sales_order_c=>sc_association-root-root_text_in_logon_lang ) ).
    IF lt_text->* IS NOT INITIAL.
      ls_order_info-text = lt_text->*[ 1 ]-text.
    ENDIF.

    cl_demo_output=>display_data( ls_order_info ).
  ENDMETHOD.                 " METHOD display_order

  METHOD process_delivery.
    "Determine the order key:
    DATA(lv_key) = get_order_by_id( iv_order_no ).
    IF lv_key IS INITIAL.
      MESSAGE |Order { iv_order_no } doesn't exist.| TYPE 'E'.
      RETURN.
    ENDIF.

    "Retrieve the current set of line items:
    DATA(lt_items) = CAST /bobf/t_demo_sales_order_itm_k(
      mo_manager->retrieve_by_assoc_tab( iv_key        = lv_key
                                         iv_assoc_type = /bobf/if_demo_sales_order_c=>sc_association-root-item ) ).

    "Process the delivery action for each of the found line items:
    LOOP AT lt_items->* ASSIGNING FIELD-SYMBOL(<ls_item>).
      "Call the BOPF action to process the delivery:
      DATA(lr_s_parameters) = NEW /bobf/s_demo_sales_order_hdr_d( item_no = <ls_item>-item_no ).

      mo_manager->do_action(
        EXPORTING
          iv_act_key    = /bobf/if_demo_sales_order_c=>sc_action-root-deliver
          it_key        = VALUE #( ( key = lv_key ) )
          is_parameters = lr_s_parameters
        IMPORTING
          eo_message    = DATA(lo_message)
          et_failed_key = DATA(lt_failed_key) ).

      "Check the results:
      IF lines( lt_failed_key ) EQ 0.
        commit( mo_manager ).
        MESSAGE `Order was delivered successfully.` TYPE 'S'.
      ELSE.
        report_messages( lo_message ).
        RETURN.
      ENDIF.
    ENDLOOP.

    "If there are no line items, perform the delivery against the header only:
    CHECK lt_items->*[] IS INITIAL.
    mo_manager->do_action(
      EXPORTING
        iv_act_key    = /bobf/if_demo_sales_order_c=>sc_action-root-deliver
        it_key        = VALUE #( ( key = lv_key ) )
        is_parameters = NEW /bobf/s_demo_sales_order_hdr_d( )
      IMPORTING
        eo_message    = lo_message
        et_failed_key = lt_failed_key ).

    "Check the results:
    IF lines( lt_failed_key ) EQ 0.
      commit( mo_manager ).
      MESSAGE `Order was delivered successfully.` TYPE 'S'.
    ELSE.
      report_messages( lo_message ).
    ENDIF.
  ENDMETHOD.                 " METHOD process_delivery

  METHOD get_order_by_id.
    "Define the selection criteria:
    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam(
     ( attribute_name = 'ORDER_ID'
       sign           = 'I'
       option         = 'EQ'
       low            = iv_order_no ) ).

    rv_order_key = mo_manager->query_row( it_sel_param = lt_params ).
  ENDMETHOD.

  METHOD create_customer.
    "Create the customer ROOT node:
    DATA(lr_s_root) = NEW /bobf/s_demo_customer_hdr_k(
      key            = /bobf/cl_frw_factory=>get_new_key( )
      customer_id    = _get_id( mo_id_range )
      sales_org      = 'AMER'
      cust_curr      = 'USD'
      address_contry = 'US'
      address        = '1234 Any Street' ).

    "Create the customer ROOT_TEXT node:
    DATA(lr_s_text) = NEW /bobf/s_demo_short_text_k(
      key      = /bobf/cl_frw_factory=>get_new_key( )
      text     = |Customer # { lr_s_root->customer_id }|
      language = sy-langu ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node        = /bobf/if_demo_customer_c=>sc_node-root
       change_mode = /bobf/if_frw_c=>sc_modify_create
       key         = lr_s_root->key
       data        = lr_s_root )

     ( node        = /bobf/if_demo_customer_c=>sc_node-root_text
       change_mode = /bobf/if_frw_c=>sc_modify_create
       source_node = /bobf/if_demo_customer_c=>sc_node-root
       association = /bobf/if_demo_customer_c=>sc_association-root-root_text
       source_key  = lr_s_root->key
       key         = lr_s_text->key
       data        = lr_s_text ) ).

    "Apply the changes:
    DATA(lo_manager) = zcl_bopf_manager=>create( iv_bopf_name = '/BOBF/DEMO_CUSTOMER'
                                                 iv_log       = p_log ).
    lo_manager->modify( EXPORTING it_modification = lt_mod
                        IMPORTING eo_message      = DATA(lo_message) ).

    IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
      report_messages( lo_message ).
    ELSE.
      ev_key = lr_s_root->key.
      ev_id  = lr_s_root->customer_id.
    ENDIF.
  ENDMETHOD.                 " METHOD create_customer

  METHOD create_product.
    "Create the product ROOT node:
    DATA(lr_s_root) = NEW /bobf/s_demo_product_hdr_k(
      key             = /bobf/cl_frw_factory=>get_new_key( )
      product_id      = _get_id( mo_id_range )
      product_type    = 'FIN_PROD'
      base_uom        = 'EA'
      sell_price      = '10.00'
      sell_price_curr = 'USD'
      buy_price       = '5.00'
      buy_price_curr  = 'USD' ).

    "Create the short text description:
    DATA(lr_s_text) = NEW /bobf/s_demo_short_text_k(
     key      = /bobf/cl_frw_factory=>get_new_key( )
     language = sy-langu
     text     = |Product # { lr_s_root->product_id }| ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node        = /bobf/if_demo_product_c=>sc_node-root
       change_mode = /bobf/if_frw_c=>sc_modify_create
       key         = lr_s_root->key
       data        = lr_s_root )

     ( node        = /bobf/if_demo_product_c=>sc_node-root_text
       change_mode = /bobf/if_frw_c=>sc_modify_create
       source_node = /bobf/if_demo_product_c=>sc_node-root
       association = /bobf/if_demo_product_c=>sc_association-root-root_text
       source_key  = lr_s_root->key
       key         = lr_s_text->key
       data        = lr_s_text ) ).

    "Apply the changes:
    DATA(lo_manager) = zcl_bopf_manager=>create( iv_bopf_name = '/BOBF/DEMO_PRODUCT'
                                                 iv_log       = p_log ).
    lo_manager->modify( EXPORTING it_modification = lt_mod
                        IMPORTING eo_message      = DATA(lo_message) ).

    IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
      report_messages( lo_message ).
    ELSE.
      ev_key = lr_s_root->key.
      ev_id  = lr_s_root->product_id.
    ENDIF.
  ENDMETHOD.                 " METHOD create_product

  METHOD commit.
    io_manager->save(
      IMPORTING eo_message = DATA(lo_message)
                ev_ok      = DATA(lv_ok) ).

    CHECK lv_ok <> abap_true.
    report_messages( lo_message ).
  ENDMETHOD.

  METHOD report_messages.
    CHECK NEW zcl_bopf_messages( " iv_severity = 'AXE'
            )->add_from_message( io_message
            )->show( ).
  ENDMETHOD.
ENDCLASS.

*&--------------------------------------------------------------------*
*& INITIALIZATION Event Module                                        *
*&--------------------------------------------------------------------*
INITIALIZATION.
  %_p_order_%_app_%-text = 'Sales Order'.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_create
      text   = 'Create'
      info   = 'Create Sales Order'
    IMPORTING
      result = btn_new.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_change
      text   = 'Change'
      info   = 'Change Sales Order'
    IMPORTING
      result = btn_chg.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_display
      text   = 'Display'
      info   = 'Display Sales Order'
    IMPORTING
      result = btn_disp.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_delivery_proposal
      text   = 'Process Delivery'
      info   = 'Process Delivery'
    IMPORTING
      result = btn_delv.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event Module                                   *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  lcl_order_processor=>process_request(
    EXPORTING
      iv_event_id = sy-ucomm
      iv_order_no = p_order ).
