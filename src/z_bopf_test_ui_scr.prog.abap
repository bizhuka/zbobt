*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME TITLE TEXT-tit.
PARAMETERS:
  _bo_name TYPE /bobf/s_conf_model_api_bo-bo_name MEMORY ID /bobf/conf_ui_bo OBLIGATORY,
  _tech    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl_main.
