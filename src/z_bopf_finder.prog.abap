*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT z_bopf_finder.

PARAMETERS:
 p_key TYPE /bobf/obm_bo_key OBLIGATORY DEFAULT ''.

START-OF-SELECTION.
  PERFORM start_of_selection.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM start_of_selection .
  SELECT tabname INTO TABLE @DATA(lt_db)
  FROM dd02l
  WHERE tabname  LIKE '/BOBF/OBM%'
    AND as4local EQ   'A'
    AND as4vers  EQ   0000
    AND ( tabclass = 'TRANSP' OR tabclass = 'CLUSTER' ).

  LOOP AT lt_db ASSIGNING FIELD-SYMBOL(<ls_db>).
    DATA(lt_comp) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( <ls_db>-tabname ) )->components[].

    DATA(lt_catalog)   = VALUE lvc_t_fcat( ).
    DATA(lt_all_where) = VALUE stringtab( ).
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>)  WHERE ( type_kind = 'X' ). " AND length = 16 ).
      APPEND VALUE #( fieldname = <ls_comp>-name
                      coltext   = |({ <ls_comp>-name })|
                      emphasize = 'C001' ) TO lt_catalog.
      APPEND |{ <ls_comp>-name } = '{ p_key }'| TO lt_all_where.
    ENDLOOP.
    CHECK lt_all_where[] IS NOT INITIAL.
    DATA(lv_where) = concat_lines_of( table = lt_all_where sep = ` OR ` ).

    DATA lr_table TYPE REF TO data.
    CREATE DATA lr_table TYPE STANDARD TABLE OF (<ls_db>-tabname).

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN lr_table->* TO <lt_table>.

    TRY.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        FROM (<ls_db>-tabname)
        WHERE (lv_where).
      CATCH cx_sy_dynamic_osql_semantics INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
    CHECK <lt_table>[] IS NOT INITIAL.

    NEW zcl_eui_alv( ir_table       = lr_table
                     it_mod_catalog = lt_catalog
                     is_layout      = VALUE #( grid_title = <ls_db>-tabname
                                               smalltitle = 'X' )
    )->show( ).
  ENDLOOP.

  MESSAGE |{ lines( lt_db ) } tables were scanned| TYPE 'S'.
ENDFORM.
