*&---------------------------------------------------------------------*
*& Include ZCOCKPIT_MC_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'STANDARD_STATUS'.
  SET TITLEBAR 'ZCOCKPIT_MC'.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'REQCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE reqcontrol_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_eban LINES reqcontrol-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'REQCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE reqcontrol_get_lines OUTPUT.
  g_reqcontrol_lines = sy-loopc.

  LOOP AT SCREEN.
    CASE sy-ucomm.
      WHEN 'GO'.
        IF ( screen-name = 'GS_EBAN-BEDNR'
          OR screen-name = 'GS_EBAN-MENGE'
          OR screen-name = 'GS_EBAN-MEINS'
          OR screen-name = 'GS_EBAN-LFDAT').
          screen-invisible = '1'. " screen-input = '1'
          MODIFY SCREEN.
        ENDIF.
      WHEN 'D1'.
        IF ( screen-name = 'GS_EBAN-BEDNR'
          OR screen-name = 'GS_EBAN-MENGE'
          OR screen-name = 'GS_EBAN-MEINS'
          OR screen-name = 'GS_EBAN-LFDAT').
          screen-invisible = '0'. " screen-input = '1'
          MODIFY SCREEN.
        ENDIF.
      WHEN 'D2'.
        IF ( screen-name = 'GS_EBAN-BEDNR'
          OR screen-name = 'GS_EBAN-MENGE'
          OR screen-name = 'GS_EBAN-MEINS'
          OR screen-name = 'GS_EBAN-LFDAT').
          screen-invisible = '1'. " screen-input = '1'
          MODIFY SCREEN.
        ENDIF.

    ENDCASE.
  ENDLOOP.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'PEDCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE pedcontrol_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_pedidos LINES pedcontrol-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'PEDCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE pedcontrol_get_lines OUTPUT.
  g_pedcontrol_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'PROCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE procontrol_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_processo LINES procontrol-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'PROCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE procontrol_get_lines OUTPUT.
  g_procontrol_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0012 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0012 OUTPUT.

* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  TYPE-POOLS: gfw.

  DATA: y_values TYPE TABLE OF gprval WITH HEADER LINE,
        x_texts  TYPE TABLE OF gprtxt WITH HEADER LINE,
        lv_count TYPE i VALUE 1,
        lv_field TYPE string.

  FIELD-SYMBOLS: <fs_cols> TYPE any.

  REFRESH y_values.
  REFRESH x_texts.

  y_values-rowtxt = 'Preço Líquido'.
  x_texts-coltxt = 'Nº de Acompanhamento'.
*  APPEND x_texts.

  SORT gt_resumo DESCENDING BY netpr.

  LOOP AT gt_resumo INTO gs_resumo.

    IF lv_count <= 32.
      DATA: c TYPE string.
      c = lv_count.
      CONCATENATE 'Y_VALUES-VAL' c INTO lv_field.

      ASSIGN (lv_field) TO <fs_cols>.
      <fs_cols> = gs_resumo-netpr.

      x_texts-coltxt = gs_resumo-ebeln. " gs_resumo-bednr '1'
      APPEND x_texts.

      ADD 1 TO lv_count.
    ENDIF.

  ENDLOOP.
  APPEND y_values.

*  FIELD-SYMBOLS: <wa_display> TYPE any.
*
*  LOOP AT gt_resumo INTO gs_resumo.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE gs_resumo TO <wa_display>.
*      IF sy-subrc <> 0.
*        SKIP.
*        EXIT.
*      ENDIF.
*      IF <wa_display> = gs_resumo-ebeln.
*        APPEND y_values.
*      ENDIF.
*      WRITE <wa_display>.
*    ENDDO.
*  ENDLOOP.

* https://blogs.sap.com/2013/09/18/drawing-graphical-charts-with-abap/

  CALL FUNCTION 'GFW_PRES_SHOW'
    EXPORTING
      header            = 'Relação entre o número de pedidos e o preço líquido'
      container         = 'CONTAINER'
      presentation_type = gfw_prestype_horizontal_bars
*     x_axis_title      = 'Preço Líquido'
      y_axis_title      = 'Número de Pedido'
    TABLES
      values            = y_values
      column_texts      = x_texts
    EXCEPTIONS
      error_occurred    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TESTE_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE GT_PEDIDOS LINES TESTE-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TESTE_GET_LINES OUTPUT.
  G_TESTE_LINES = SY-LOOPC.
ENDMODULE.
