*&---------------------------------------------------------------------*
*& Include          ZCOCKPIT_MC_F01
*&---------------------------------------------------------------------*
FORM requisition_release.
*http://sap.technique.free.fr/article.php?sid=74
*FRGGR é chave primária da tabela t16fc e chave estrangeira da tabela eban.

*  SELECT SINGLE frggr FROM eban INTO lv_frggr
*  WHERE banfn = lv_reqnum
*  AND bnfpo = lv_it1.
*
*  SELECT SINGLE frgco FROM t16fc INTO lv_frgco
*  WHERE frggr = lv_frggr.

*  SELECT SINGLE frgst, frggr FROM eban INTO (@lv_frgst, @lv_frggr)
*  WHERE banfn = @lv_reqnum
*  AND bnfpo = @lv_it1.
*
*  SELECT SINGLE frgc1 FROM t16fs INTO @lv_frgab
*  WHERE frggr = @lv_frggr " grupo de liberação
*  AND frgsx = @lv_frgst. " estratégica de liberação

*  LOOP AT gt_eban INTO gs_eban.
*    SELECT a~banfn, a~bnfpo, a~frggr, a~frgst, b~frgsx, b~frgc1
*      FROM eban AS a
*      INNER JOIN t16fs AS b ON a~frggr EQ b~frggr
*      WHERE banfn = @gs_eban-banfn
*      AND bnfpo = @gs_eban-bnfpo
*      APPENDING CORRESPONDING FIELDS
*      OF TABLE @gt_inner.
*  ENDLOOP.

*  DATA:
*    bp_number    TYPE bapi2009ob-preq_no,
*    bp_real_code TYPE bapimmpara-rel_code,
*    bp_item      TYPE bapi2009ob-preq_item.

  LOOP AT gt_eban INTO gs_eban.

    lv_frggr = gs_eban-frggr.
    lv_frgsx = gs_eban-frgst.

    index = 0.

    WHILE index < 3.
      frgc = 'frgc'.
      index = index + 1.
      CONCATENATE frgc index INTO frgci.

      IF frgci = 'frgc1'.

        SELECT frgc1 FROM t16fs
          WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc2'.

        SELECT frgc2 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc3'.

        SELECT frgc3 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ENDIF.

    ENDWHILE.

    LOOP AT gt_frgci INTO gs_frgci.

      IF gs_frgci-frgco IS INITIAL.
        SKIP.
      ELSE.

*  LOOP AT gt_inner INTO gs_inner.


*      number                 = '0010000053'
*      rel_code               = '01'
*      item                   = '00010'

*CURRENT ERROR: Falta indicação da responsabilidade de liberação!

        CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
          EXPORTING
            number                 = gs_eban-banfn
            rel_code               = gs_frgci-frgco
            item                   = gs_eban-bnfpo
            use_exceptions         = 'X'
*           NO_COMMIT_WORK         = ' '
* IMPORTING
*           REL_STATUS_NEW         =
*           REL_INDICATOR_NEW      =
          TABLES
            return                 = lt_return
          EXCEPTIONS
            authority_check_fail   = 1
            requisition_not_found  = 2
            enqueue_fail           = 3
            prerequisite_fail      = 4
            release_already_posted = 5
            responsibility_fail    = 6
            OTHERS                 = 7.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          LOOP AT lt_return INTO ls_return.
            MESSAGE e001(bapi) WITH ls_return-message.
            CONTINUE.
          ENDLOOP.
        ELSE.
          MESSAGE 'Requisição(ões) liberada(s) com sucesso.!' TYPE 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR gt_frgci. " importante !!!

  ENDLOOP.

ENDFORM.
FORM requisition_unrelease.

*  LOOP AT gt_eban INTO gs_eban.
*    SELECT a~banfn, a~bnfpo, a~frggr, a~frgst, b~frgsx, b~frgc1
*      FROM eban AS a
*      INNER JOIN t16fs AS b ON a~frggr EQ b~frggr
*      WHERE banfn = @gs_eban-banfn
*      AND bnfpo = @gs_eban-bnfpo
*      APPENDING CORRESPONDING FIELDS
*      OF TABLE @gt_inner.
*  ENDLOOP.

*  DATA:
*    bp_number    TYPE bapi2009ob-preq_no,
*    bp_real_code TYPE bapimmpara-rel_code,
*    bp_item      TYPE bapi2009ob-preq_item.

  LOOP AT gt_eban INTO gs_eban.

    lv_frggr = gs_eban-frggr.
    lv_frgsx = gs_eban-frgst.

    index = 0.

    WHILE index < 3.
      frgc = 'frgc'.
      index = index + 1.
      CONCATENATE frgc index INTO frgci.

      IF frgci = 'frgc1'.

        SELECT frgc1 FROM t16fs
          WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc2'.

        SELECT frgc2 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc3'.

        SELECT frgc3 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ENDIF.

    ENDWHILE.

    LOOP AT gt_frgci INTO gs_frgci.

      IF gs_frgci-frgco IS INITIAL.
        SKIP.
      ELSE.

*  LOOP AT gt_inner INTO gs_inner.

        CALL FUNCTION 'BAPI_REQUISITION_RESET_RELEASE'
          EXPORTING
            number                   = gs_eban-banfn
            item                     = gs_eban-bnfpo
            rel_code                 = gs_frgci-frgco
            use_exceptions           = 'X'
*           NO_COMMIT_WORK           = ' '
*   IMPORTING
*           REL_STATUS_NEW           =
*           REL_INDICATOR_NEW        =
          TABLES
            return                   = lt_return
          EXCEPTIONS
            authority_check_fail     = 1
            requisition_not_found    = 2
            enqueue_fail             = 3
            prerequisite_fail        = 4
            release_already_posted   = 5
            responsibility_fail      = 6
            no_release_already       = 7
            no_new_release_indicator = 8
            OTHERS                   = 9.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          LOOP AT lt_return INTO ls_return.
            MESSAGE e001(bapi) WITH ls_return-message.
            CONTINUE.
          ENDLOOP.
        ELSE.
          MESSAGE 'Liberação(s) de Requisição(ões) cancelados com sucesso.!' TYPE 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR gt_frgci. " importante !!!

  ENDLOOP.
ENDFORM.
FORM request_order.

*  SELECT SINGLE frggr FROM ekko INTO lv_frggr
*    WHERE ebeln = lv_numped.
*
*  SELECT SINGLE frgco FROM t16fc INTO lv_frgco
*  WHERE frggr = lv_frggr.

*  SELECT SINGLE frggr FROM ekko INTO lv_frggr
*  WHERE ebeln = @lv_reqnum.
*
*  SELECT SINGLE frgc1 FROM t16fs INTO @lv_frgab
*  WHERE frggr = @lv_frggr.

*  SELECT a~ebeln, a~frggr, a~frgsx, b~frgc1
*    FROM ekko AS a
*    INNER JOIN t16fs AS b ON ( a~frggr EQ b~frggr AND a~frgsx EQ b~frgsx )
*    WHERE ebeln = @lv_numped
*    APPENDING CORRESPONDING FIELDS
*    OF TABLE @gt_po.

*  DATA:
*    bp_purchaseorder    TYPE BAPIMMPARA-PO_NUMBER,
*    bp_po_rel_code TYPE BAPIMMPARA-PO_REL_COD.

*  LOOP AT gt_po INTO gs_po.
  LOOP AT gt_pedidos INTO gs_pedidos.

    lv_frggr = gs_pedidos-frggr.
    lv_frgsx = gs_pedidos-frgsx.

    index = 0.

    WHILE index < 3.
      frgc = 'frgc'.
      index = index + 1.
      CONCATENATE frgc index INTO frgci.

      IF frgci = 'frgc1'.

        SELECT frgc1 FROM t16fs
          WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc2'.

        SELECT frgc2 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc3'.

        SELECT frgc3 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ENDIF.

    ENDWHILE.


    LOOP AT gt_frgci INTO gs_frgci.


      IF gs_frgci-frgco IS INITIAL.
        SKIP.
      ELSE.

        CALL FUNCTION 'BAPI_PO_RELEASE'
          EXPORTING
            purchaseorder          = gs_pedidos-ebeln " ekko-ebeln
            po_rel_code            = gs_frgci-frgco " t16fc-frgco
            use_exceptions         = 'X'
*           NO_COMMIT              = ' '
*   IMPORTING
*           REL_STATUS_NEW         =
*           REL_INDICATOR_NEW      =
*           RET_CODE               =
          TABLES
            return                 = lt_return
          EXCEPTIONS
            authority_check_fail   = 1
            document_not_found     = 2
            enqueue_fail           = 3
            prerequisite_fail      = 4
            release_already_posted = 5
            responsibility_fail    = 6
            OTHERS                 = 7.
        IF sy-subrc <> 0.
* Implement suitable error handling here
*          LOOP AT lt_return INTO ls_return.
*            MESSAGE e002(bapi) WITH ls_return-message. " ERROR NUMBER 5
*            CONTINUE.
*          ENDLOOP.

        ELSE.
          MESSAGE 'Pedido(s) liberado(s) com sucesso.' TYPE 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CLEAR gt_frgci.

  ENDLOOP.

ENDFORM.
FORM cancel_order.

*  LOOP AT gt_pedidos INTO gs_pedidos.
*
*    SELECT a~ebeln, a~frggr, a~frgsx, b~frgc1
*     FROM ekko AS a
*     INNER JOIN t16fs AS b ON ( a~frggr EQ b~frggr AND a~frgsx EQ b~frgsx )
*     WHERE ebeln = @gs_pedidos-ebeln
*     APPENDING CORRESPONDING FIELDS
*     OF TABLE @gt_po.

*  DATA:
*    bp_purchaseorder    TYPE BAPIMMPARA-PO_NUMBER,
*    bp_po_rel_code TYPE BAPIMMPARA-PO_REL_COD.

*    LOOP AT gt_po INTO gs_po.
  LOOP AT gt_pedidos INTO gs_pedidos.

    lv_frggr = gs_pedidos-frggr.
    lv_frgsx = gs_pedidos-frgsx.

    index = 0.

    WHILE index < 3.
      frgc = 'frgc'.
      index = index + 1.
      CONCATENATE frgc index INTO frgci.

      IF frgci = 'frgc1'.

        SELECT frgc1 FROM t16fs
          WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc2'.

        SELECT frgc2 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ELSEIF frgci = 'frgc3'.

        SELECT frgc3 FROM t16fs
      WHERE ( frggr = @lv_frggr AND frgsx = @lv_frgsx )
          APPENDING TABLE @gt_frgci.

      ENDIF.

    ENDWHILE.

    LOOP AT gt_frgci INTO gs_frgci.

      IF gs_frgci-frgco IS INITIAL.
        SKIP.
      ELSE.

        CALL FUNCTION 'BAPI_PO_RESET_RELEASE'
          EXPORTING
            purchaseorder            = gs_pedidos-ebeln
            po_rel_code              = gs_frgci-frgco
            use_exceptions           = 'X'
* IMPORTING
*           REL_STATUS_NEW           =
*           REL_INDICATOR_NEW        =
          TABLES
            return                   = lt_return
          EXCEPTIONS
            authority_check_fail     = 1
            document_not_found       = 2
            enqueue_fail             = 3
            prerequisite_fail        = 4
            release_already_posted   = 5
            responsibility_fail      = 6
            no_release_already       = 7
            no_new_release_indicator = 8
            OTHERS                   = 9.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ELSE.
          MESSAGE 'Liberação de pedido(s) cancelada com sucesso.' TYPE 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR gt_frgci.

  ENDLOOP.
*  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gs_bdcdata.
  gs_bdcdata-program  = program.
  gs_bdcdata-dynpro   = dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR gs_bdcdata.
  gs_bdcdata-fnam = fnam.
  gs_bdcdata-fval = fval.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.
