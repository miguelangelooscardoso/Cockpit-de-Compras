*&---------------------------------------------------------------------*
*& Include          ZCOCKPIT_MC_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  IF sy-ucomm EQ '&F03'.
    LEAVE TO SCREEN 0.
  ENDIF.

*    IF sy-ucomm EQ '&F12'.
*    CALL TRANSACTION 'SE80'.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  """ Clear variables!

  CASE sy-ucomm.
    WHEN 'RB'.
      IF p_req EQ 'X'.
        lv_screen = '0003'.
        lv_detalhe = '9999'.
      ELSEIF p_pdd EQ 'X'.
        lv_screen = '0004'.
        lv_detalhe = '9999'.
      ELSEIF p_prc EQ 'X'.
        lv_screen = '0005'.
        lv_detalhe = '9999'.
      ELSEIF p_proreq EQ 'X'.
        lv_screen = '0006'.
        lv_detalhe = '9999'.
      ELSEIF p_nacomp EQ 'X'.
        lv_screen = '0007'.
        lv_detalhe = '9999'.
      ENDIF.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

*  IF gt_tab[] IS INITIAL. " vazia
*    MESSAGE: 'No results found!'TYPE 'I'.
*  ENDIF.

  CASE sy-ucomm.
    WHEN 'GO'.

      CLEAR: lt_eban, r_reqnum[], r_it1[], r_txt1[], r_mat1[].

      IF lv_reqnum IS INITIAL
          AND lv_it1 IS INITIAL
          AND lv_txt1 IS INITIAL
          AND lv_mat1 IS INITIAL.

        MESSAGE 'Introduza pelo menos um parâmetro!' TYPE 'I'.

      ELSE.

        IF lv_reqnum IS NOT INITIAL.
          r_reqnum-sign = 'I'.
          r_reqnum-option = 'EQ'.
          r_reqnum-low = lv_reqnum.
          APPEND r_reqnum.
        ENDIF.

        IF lv_it1 IS NOT INITIAL.
          r_it1-sign = 'I'.
          r_it1-option = 'EQ'.
          r_it1-low = lv_it1.
          APPEND r_it1.
        ENDIF.

        IF lv_txt1 IS NOT INITIAL.
          r_txt1-sign = 'I'.
          r_txt1-option = 'EQ'.
          r_txt1-low = lv_txt1.
          APPEND r_txt1.
        ENDIF.

        IF lv_mat1 IS NOT INITIAL.
          r_mat1-sign = 'I'. "include"
          r_mat1-option = 'EQ'. ""
          r_mat1-low = lv_mat1.
          APPEND r_mat1. "adiciona à range"
        ENDIF.

        "só metia o high se tivesse um intervalo de valores mas como nós só queremos um valor"

        SELECT banfn, bnfpo, txz01, matnr, bednr, lfdat, menge, meins, frggr, frgst
            FROM eban
            WHERE banfn IN @r_reqnum
            AND bnfpo IN @r_it1
            AND upper( txz01 ) IN @r_txt1
            AND matnr IN @r_mat1
            INTO CORRESPONDING FIELDS OF TABLE @lt_eban.

        IF lt_eban IS NOT INITIAL.
          APPEND LINES OF lt_eban TO gt_eban.
        ENDIF.

      ENDIF.

      lv_detalhe = '0008'.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0008 INPUT.

  CASE sy-ucomm.
    WHEN 'A1'.
      PERFORM requisition_release.
    WHEN 'R1'.
      PERFORM requisition_unrelease.
*      LOOP AT gt_inner INTO gs_inner.
*      LOOP AT gt_eban INTO gs_eban.
*        "include bdcrecx1.
*
*        "perform open_group.
*
*        CLEAR gt_bdcdata.
*
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MECHOB'.
*        PERFORM bdc_field       USING 'MEPOTEXT-EDITOR'
*                                      '1'.
*        PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                      '   1'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                      'MEPO_REL_GENERAL-FRGGR'.
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0002'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MEOK'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                      'MEPO_SELECT-BANFN'.
*        PERFORM bdc_field       USING 'MEPO_SELECT-BANFN'
*                                      gs_inner-banfn.
*        PERFORM bdc_field       USING 'MEPO_SELECT-BSTYP_B'
*                                      'X'.
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MERESET'.
*        PERFORM bdc_field       USING 'MEPOTEXT-EDITOR'
*                                      '1'.
*        PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                      '   1'.
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MESAVE'.
*        PERFORM bdc_field       USING 'MEPOTEXT-EDITOR'
*                                      '1'.
*        PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                      '   1'.
*        "perform bdc_transaction using 'ME54N'.
*
*        "perform close_group.
*
*        CALL TRANSACTION 'ME54N' USING gt_bdcdata " Case Sensitive.
*                                 MODE 'A'
*                                 UPDATE 'A'
*                                 MESSAGES INTO lt_messtab.
*
*      ENDLOOP.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0004 INPUT.

  CASE sy-ucomm.
    WHEN 'GO'.

      CLEAR: lt_ekpo, lt_ekko, lt_lfa1, lt_pedidos,
      r_numped[], r_it2[], r_txt2[], r_mat2[], r_forn[], r_nmfrn[].

      IF lv_numped IS INITIAL
          AND lv_it2 IS INITIAL
          AND lv_txt2 IS INITIAL
          AND lv_mat2 IS INITIAL
          AND lv_forn IS INITIAL " número do fornecedor
          AND lv_nmfrn IS INITIAL. " nome do fornecedor

        MESSAGE 'Introduza pelo menos um parâmetro!' TYPE 'I'.

      ELSE.

        IF lv_numped IS NOT INITIAL.
          r_numped-sign = 'I'.
          r_numped-option = 'EQ'.
          r_numped-low = lv_numped.
          APPEND r_numped.
        ENDIF.

        IF lv_it2 IS NOT INITIAL.
          r_it2-sign = 'I'.
          r_it2-option = 'EQ'.
          r_it2-low = lv_it2.
          APPEND r_it2.
        ENDIF.

        IF lv_txt2 IS NOT INITIAL.
          r_txt2-sign = 'I'.
          r_txt2-option = 'EQ'.
          r_txt2-low = lv_txt2.
          APPEND r_txt2.
        ENDIF.

        IF lv_mat2 IS NOT INITIAL.
          r_mat2-sign = 'I'.
          r_mat2-option = 'EQ'.
          r_mat2-low = lv_mat2.
          APPEND r_mat2.
        ENDIF.

        IF lv_forn IS NOT INITIAL.
          r_forn-sign = 'I'.
          r_forn-option = 'EQ'.
          r_forn-low = lv_forn.
          APPEND r_forn.
        ENDIF.

        IF lv_nmfrn IS NOT INITIAL.
          r_nmfrn-sign = 'I'.
          r_nmfrn-option = 'EQ'.
          r_nmfrn-low = lv_nmfrn.
          APPEND r_nmfrn.
        ENDIF.

        SELECT ebeln, ebelp, txz01, matnr, menge, meins, netpr "frggr  frgst name1 lifnr and bedat não estão na ekpo"
            FROM ekpo
            WHERE ebeln IN @r_numped
            AND ebelp IN @r_it2
            AND upper( txz01 ) IN @r_txt2
            AND matnr IN @r_mat2
*            AND name1 IN r_numfrn.
          INTO CORRESPONDING FIELDS OF TABLE @lt_ekpo.


        IF lt_ekpo IS NOT INITIAL. "!!!!!!"
          SELECT ebeln, lifnr, bedat, frggr, frgsx
          FROM ekko
          FOR ALL ENTRIES IN @lt_ekpo
          WHERE lifnr IN @r_forn
          AND ebeln =  @lt_ekpo-ebeln
          INTO CORRESPONDING FIELDS OF TABLE @lt_ekko.
        ENDIF.

        IF lt_ekko IS NOT INITIAL. "!!!!!!!"
          SELECT lifnr, name1
          FROM lfa1
          FOR ALL ENTRIES IN @lt_ekko
          WHERE name1 IN @r_nmfrn
          AND lifnr = @lt_ekko-lifnr
          INTO CORRESPONDING FIELDS OF TABLE @lt_lfa1.
        ENDIF.


        IF lt_lfa1 IS NOT INITIAL. "!!!!!!!"
          LOOP AT lt_ekpo INTO ls_ekpo.
            LOOP AT lt_ekko INTO ls_ekko.
              LOOP AT lt_lfa1 INTO ls_lfa1.

                IF ls_ekpo-ebeln = ls_ekko-ebeln
                  AND ls_ekko-lifnr = ls_lfa1-lifnr. " ter a certeza que pedido e nº fornecedor é o mesmo

                  ls_pedidos-ebeln = ls_ekpo-ebeln.
                  ls_pedidos-ebelp = ls_ekpo-ebelp.
                  ls_pedidos-txz01 = ls_ekpo-txz01.
                  ls_pedidos-matnr = ls_ekpo-matnr.

                  ls_pedidos-lifnr = ls_ekko-lifnr.
                  ls_pedidos-bedat = ls_ekko-bedat.
                  ls_pedidos-frggr = ls_ekko-frggr.
                  ls_pedidos-frgsx = ls_ekko-frgsx.

                  ls_pedidos-menge = ls_ekpo-menge.
                  ls_pedidos-meins = ls_ekpo-meins.
                  ls_pedidos-netpr = ls_ekpo-netpr.


                  ls_pedidos-name1 = ls_lfa1-name1.

                  APPEND ls_pedidos TO lt_pedidos.
                  CLEAR ls_pedidos.

                ENDIF.

              ENDLOOP.
            ENDLOOP.
          ENDLOOP.

          IF lt_pedidos IS INITIAL.
            MESSAGE 'Não foram encontrados registos!' TYPE 'I'.
          ELSE.
            APPEND LINES OF lt_pedidos TO gt_pedidos.
          ENDIF.
        ENDIF.
      ENDIF.

      lv_detalhe = '0009'.
*      lv_detalhe = '8888'.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0009 INPUT.

  CASE sy-ucomm.
    WHEN 'A1'.
      PERFORM request_order.
    WHEN 'R1'.
      PERFORM cancel_order.
*      SELECT a~ebeln, a~frggr, a~frgsx, b~frgc1
*        FROM ekko AS a
*        INNER JOIN t16fs AS b ON ( a~frggr EQ b~frggr AND a~frgsx EQ b~frgsx )
*        WHERE ebeln = @lv_numped
*        APPENDING CORRESPONDING FIELDS
*        OF TABLE @gt_po.
*
*      LOOP AT gt_po INTO gs_po.
*
**     include bdcrecx1.
*
**     perform open_group.
*
*        CLEAR gt_bdcdata.
*
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MECHOB'.
*        PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                      'NB'.
*        PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                      '   1'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                      'MEPO1319-MATKL'.
*        PERFORM bdc_field       USING 'MEPO1319-SPINF'
*                                      'X'.
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0002'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MEOK'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                      'MEPO_SELECT-EBELN'.
*        PERFORM bdc_field       USING 'MEPO_SELECT-EBELN'
*                                      gs_po-ebeln.
*        PERFORM bdc_field       USING 'MEPO_SELECT-BSTYP_F'
*                                      'X'.
*        PERFORM bdc_dynpro      USING 'SAPLMEGUI' '0014'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '=MERESET'.
*        PERFORM bdc_field       USING 'MEPO_TOPLINE-BSART'
*                                      'NB'.
*        PERFORM bdc_field       USING 'DYN_6000-LIST'
*                                      '   1'.
*        PERFORM bdc_field       USING 'MEPO1319-SPINF'
*                                      'X'.
**      PERFORM bdc_transaction USING 'ME29N'.
**
**      PERFORM close_group.
*
*        CALL TRANSACTION 'ME29N' USING gt_bdcdata " Case Sensitive.
*                           MODE 'A'
*                           UPDATE 'A'
*                           MESSAGES INTO lt_messtab.
*
*      ENDLOOP.

    WHEN 'D1'.

      " https://answers.sap.com/questions/6082020/how-to-define-internal-table-in-smartform.html

      DATA: gt_pedidos_new TYPE zcatpedidos,
            gs_pedidos_new TYPE zpedidos.

      CLEAR: gs_pedidos_new.


      LOOP AT gt_pedidos INTO gs_pedidos.
        gs_pedidos_new-ebeln = gs_pedidos-ebeln.
        gs_pedidos_new-ebelp = gs_pedidos-ebelp.
        gs_pedidos_new-txz01 = gs_pedidos-txz01.
        gs_pedidos_new-matnr = gs_pedidos-matnr.
        gs_pedidos_new-lifnr = gs_pedidos-lifnr.
        gs_pedidos_new-name1 = gs_pedidos-name1.
        gs_pedidos_new-bedat = gs_pedidos-bedat.
        gs_pedidos_new-menge = gs_pedidos-menge.
        gs_pedidos_new-meins = gs_pedidos-meins.
        gs_pedidos_new-netpr = gs_pedidos-netpr.
        APPEND gs_pedidos_new TO gt_pedidos_new.
      ENDLOOP.

      DATA c_ok TYPE string.

      SORT gt_pedidos BY lifnr ASCENDING.
      LOOP AT gt_pedidos INTO gs_pedidos.
        IF sy-tabix EQ 1.
          c_ok = gs_pedidos-name1.
        ELSEIF gs_pedidos-name1 NE c_ok.
          MESSAGE 'SELECIONE REGISTOS DE APENAS UM FORNECEDOR!' TYPE 'E'.
        ENDIF.
      ENDLOOP.

      DATA: ls_c_ok TYPE lfa1.

      SELECT SINGLE name1 stras pstlz ort01 land1
        FROM lfa1
        INTO CORRESPONDING FIELDS OF ls_c_ok
        WHERE name1 = c_ok.

      CALL FUNCTION '/1BCDWB/SF00000142'
        EXPORTING
*         ARCHIVE_INDEX    =
*         ARCHIVE_INDEX_TAB          =
*         ARCHIVE_PARAMETERS         =
*         CONTROL_PARAMETERS         =
*         MAIL_APPL_OBJ    =
*         MAIL_RECIPIENT   =
*         MAIL_SENDER      =
*         OUTPUT_OPTIONS   =
*         USER_SETTINGS    = 'X'
          nome             = ls_c_ok-name1
          morada           = ls_c_ok-stras
          codigo_postal    = ls_c_ok-pstlz
          local            = ls_c_ok-ort01
          pais             = ls_c_ok-land1
          gt_pedidos       = gt_pedidos_new
* IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
*         JOB_OUTPUT_INFO  =
*         JOB_OUTPUT_OPTIONS         =
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

*      ENDLOOP.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

  CASE sy-ucomm.
    WHEN 'LD'.
      CALL FUNCTION 'WS_FILENAME_GET'
        IMPORTING
          filename = loc_dir.
      IF sy-subrc <> 0.
        MESSAGE 'FILE .XLSX NOT FOUND!' TYPE 'E'.
      ENDIF.

      DATA: it_raw TYPE truxs_t_text_data.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_field_seperator    = 'X'
          i_line_header        = 'X'
          i_tab_raw_data       = it_raw
          i_filename           = loc_dir
        TABLES
          i_tab_converted_data = lt_preco
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
*        LOOP AT lt_preco INTO ls_preco.
*          WRITE:/ ls_preco-org_compras, ls_preco-centro, ls_preco-nif, ls_preco-mat_forn, ls_preco-prc_unit.
*        ENDLOOP.
      ENDIF.

*lfa1-stceg  NIF Fornecedor -> lfa1-lifnr  Código do Fornecedor
*eina-idnlf Número Material do Fornecedor -> eina-matnr Código do material
*eina-infnr Nº Registo Info

      SELECT stceg lifnr
          INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
          FROM lfa1
          FOR ALL ENTRIES IN lt_preco
          WHERE stceg = lt_preco-nif.

      SELECT infnr matnr
           INTO CORRESPONDING FIELDS OF TABLE lt_eina
           FROM eina
           FOR ALL ENTRIES IN lt_preco
           WHERE infnr = lt_preco-info.


*         nif         TYPE lfa1-stceg, " NIF Fornecedor
*         mat_forn    TYPE eina-idnlf, " Nº material Fornecedor
*         prc_unit    TYPE eine-netpr, " Preço Unitário
*         codfrn      TYPE lfa1-lifnr, " Código do Fornecedor
*         codmat      TYPE eina-matnr, " Código do Material ()

      LOOP AT lt_preco INTO ls_preco.

        LOOP AT lt_lfa1 INTO ls_lfa1.

          IF ls_preco-nif = ls_lfa1-stceg.

            ls_preco-codfrn = ls_lfa1-lifnr.
            MODIFY lt_preco FROM ls_preco.

          ENDIF.
        ENDLOOP.

        LOOP AT lt_eina INTO ls_eina.

*          IF ls_preco-mat_forn = ls_eina-idnlf. "idnlf não existe na tabela
          IF ls_preco-info = ls_eina-infnr.

            ls_preco-codmat = ls_eina-matnr.
            MODIFY lt_preco FROM ls_preco.

          ENDIF.
        ENDLOOP.
      ENDLOOP.

*      APPEND LINES OF lt_preco TO gt_preco.

      DATA: tmp_dir   TYPE string VALUE '/usr/sap/tmp/preco_mc.txt',
            msg       TYPE string,
            lv_string TYPE string,
            lv_netpr  TYPE string.

      OPEN DATASET tmp_dir FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE msg.

      IF sy-subrc <> 0.
* Implement suitable error handling here
        WRITE: msg.
      ELSE.
        LOOP AT lt_preco INTO ls_preco.

          lv_netpr = ls_preco-prc_unit.
          CONCATENATE ls_preco-org_compras ls_preco-centro ls_preco-nif lv_netpr ls_preco-codfrn ls_preco-codmat ls_preco-info INTO lv_string SEPARATED BY ' '.
          TRANSFER lv_string TO tmp_dir.

        ENDLOOP.
        CLOSE DATASET tmp_dir.
      ENDIF.

      DATA: lv_curr_price TYPE eine-netpr,
            lv_new_price  TYPE string.

*Selecionar Nº do Registo e Preço Unitário
* lt_preco contém o valor do .xlsx
* gt_preco contém o valor da tabela transparente eine

      DATA: lv_info     TYPE eine-infnr,
            lv_prc_unit TYPE eine-netpr.

*      DATA: lt_aux TYPE TABLE OF eine.

      LOOP AT lt_preco INTO ls_preco.

        SELECT SINGLE infnr
          FROM eine
          INTO lv_info
          WHERE infnr = ls_preco-info.

        SELECT SINGLE netpr
          FROM eine
          INTO lv_prc_unit
          WHERE infnr = ls_preco-info.

*       lt_aux-infnr = lv_infnr.
*       lt_aux-netpr = lv_netpr.

        IF ( ls_preco-info = lv_info ) AND ( ls_preco-prc_unit <> lv_prc_unit ).

          APPEND ls_preco TO gt_preco. " tabela global a passar para alterar registos

        ENDIF.
        CLEAR: lv_info, lv_prc_unit.
      ENDLOOP.

      CALL FUNCTION 'BDC_OPEN_GROUP'
        EXPORTING
          client              = sy-mandt
*         DEST                = FILLER8
          group               = 'ZCOCKPIT_MC' " folder name
*         HOLDDATE            = FILLER8
          keep                = 'X'
          user                = sy-uname " user
*         RECORD              = FILLER1
*         PROG                = SY-CPROG
*         DCPFM               = '%'
*         DATFM               = '%'
* IMPORTING
*         QID                 =
        EXCEPTIONS
          client_invalid      = 1
          destination_invalid = 2
          group_invalid       = 3
          group_is_locked     = 4
          holddate_invalid    = 5
          internal_error      = 6
          queue_error         = 7
          running             = 8
          system_lock_error   = 9
          user_invalid        = 10
          OTHERS              = 11.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      LOOP AT gt_preco INTO gs_preco.


*        PERFORM open_group.

**         org_compras TYPE eine-ekorg, " Organização de Compras
**         centro      TYPE eine-werks, " Centro
**         nif         TYPE lfa1-stceg, " NIF Fornecedor
***         mat_forn    TYPE eina-idnlf, " Nº material Fornecedor
**         info        TYPE eina-infnr, " Nº Registo Info
**         prc_unit    TYPE eine-netpr, " Preço Unitário
**         codfrn      TYPE lfa1-lifnr, " Código do Fornecedor
**         codmat      TYPE eina-matnr, " Código do Material
*
*        DATA: lv_curr_price TYPE eine-netpr,
*              lv_new_price  TYPE string.

        SELECT SINGLE netpr
          FROM eine
          INTO lv_curr_price
          WHERE infnr = gs_preco-info.

        IF lv_curr_price <> gs_preco-prc_unit.

          MOVE gs_preco-prc_unit TO lv_new_price.

          REPLACE ALL OCCURRENCES OF '.' IN lv_new_price WITH ','.

          PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'EINE-WERKS'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM bdc_field       USING 'EINA-LIFNR'
                                        gs_preco-codfrn.
          PERFORM bdc_field       USING 'EINA-MATNR'
                                        gs_preco-codmat.
          PERFORM bdc_field       USING 'EINE-EKORG'
                                        gs_preco-org_compras.
          PERFORM bdc_field       USING 'EINE-WERKS'
                                        gs_preco-centro.
          PERFORM bdc_field       USING 'RM06I-NORMB'
                                        'X'.
          PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'EINA-MAHN1'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=KO'.
          PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'VAKE-DATAB(01)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PICK'.
          PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'KONP-KBETR(01)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=SICH'.
          PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                        lv_new_price. " gs_preco-prc_unit
*        PERFORM bdc_transaction USING 'ME12'.

*          CALL TRANSACTION 'ME12' USING gt_bdcdata " Case Sensitive.
*                     MODE 'A'
*                     UPDATE 'A'
*                     MESSAGES INTO lt_messtab.

*        PERFORM close_group.

          CALL FUNCTION 'BDC_INSERT'
            EXPORTING
              tcode            = 'ME12'
*             POST_LOCAL       = NOVBLOCAL
*             PRINTING         = NOPRINT
*             SIMUBATCH        = ' '
*             CTUPARAMS        = ' '
            TABLES
              dynprotab        = gt_bdcdata
            EXCEPTIONS
              internal_error   = 1
              not_open         = 2
              queue_error      = 3
              tcode_invalid    = 4
              printing_invalid = 5
              posting_invalid  = 6
              OTHERS           = 7.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

* REFRESH gt_bdcdata.

        ENDIF.

        CLEAR: gt_bdcdata, gs_preco, lv_new_price.

      ENDLOOP.

      CALL FUNCTION 'BDC_CLOSE_GROUP'
        EXCEPTIONS
          not_open    = 1
          queue_error = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0006 INPUT.

  CASE sy-ucomm.
    WHEN 'GO'.

      CLEAR: lt_processo,
      r_reqnum4[], r_it4[], r_txt4[], r_mat4[], r_nacomp4[].

*      SELECT ebeln, banfn, bnfpo, txz01, matnr, bednr, banpr, statu FROM eban
*        WHERE banfn EQ @lv_reqnum4
*        OR bnfpo EQ @lv_it4
*        OR upper( txz01 ) EQ @lv_txt4
*        OR matnr EQ @lv_mat4
*        OR bednr EQ @lv_nacomp4
*      APPENDING CORRESPONDING FIELDS
*      OF TABLE @gt_processo.

      IF lv_reqnum4 IS INITIAL
          AND lv_it4 IS INITIAL
          AND lv_txt4 IS INITIAL
          AND lv_mat4 IS INITIAL
          AND lv_nacomp4 IS INITIAL.

        MESSAGE 'Introduza pelo menos um parâmetro!' TYPE 'I'.

      ELSE.

        IF lv_reqnum4 IS NOT INITIAL.
          r_reqnum4-sign = 'I'.
          r_reqnum4-option = 'EQ'.
          r_reqnum4-low = lv_reqnum4.
          APPEND r_reqnum4.
        ENDIF.

        IF lv_it4 IS NOT INITIAL.
          r_it4-sign = 'I'.
          r_it4-option = 'EQ'.
          r_it4-low = lv_it4.
          APPEND r_it4.
        ENDIF.

        IF lv_txt4 IS NOT INITIAL.
          r_txt4-sign = 'I'.
          r_txt4-option = 'EQ'.
          r_txt4-low = lv_txt4.
          APPEND r_txt4.
        ENDIF.

        IF lv_mat4 IS NOT INITIAL.
          r_mat4-sign = 'I'. "include"
          r_mat4-option = 'EQ'. ""
          r_mat4-low = lv_mat4.
          APPEND r_mat4. "adiciona à range"
        ENDIF.


        SELECT banfn bnfpo txz01 matnr bednr lfdat menge meins frggr frgst ebeln banpr statu " frgsx não existe na EABN
            FROM eban
            INTO CORRESPONDING FIELDS OF TABLE lt_processo
            WHERE banfn IN r_reqnum4
            AND bnfpo IN r_it4
            AND txz01 IN r_txt4
            AND matnr IN r_mat4
            AND bednr IN r_nacomp4.

        """ faltam acrescentar alguns parâmetros ao select

*        DATA: ls_ekbe TYPE ekbe.

        IF lt_processo IS NOT INITIAL.
          LOOP AT lt_processo INTO ls_processo.
            IF ls_processo-banpr = '03' AND ls_processo-statu = 'N'.
              ls_processo-status = 'Aberta'.
            ELSEIF ls_processo-banpr = '05'.
              ls_processo-status = 'Liberada'.
            ELSEIF ls_processo-ebeln IS NOT INITIAL
                AND ls_processo-statu = 'B' AND ls_processo-banpr = '05'.
              ls_processo-status = 'Em Pedido'.
            ELSEIF ls_processo-ebeln IS NOT INITIAL AND ls_processo-statu = 'B'.

*              SELECT SINGLE bewtp, bwart
*                  FROM ekbe
*                  INTO CORRESPONDING FIELDS OF @ls_ekbe
*                  WHERE  ( ebeln = @ls_processo-ebeln AND bewtp = 'E'  AND bwart = '101' ).

              DATA: lv_bewtp TYPE ekbe-bewtp,
                    lv_bwart TYPE ekbe-bwart.

              SELECT SINGLE bewtp
                  FROM ekbe
                  INTO lv_bewtp
                  WHERE  ( ebeln = ls_processo-ebeln AND bewtp = 'E'  AND bwart = '101' ).

              SELECT SINGLE bwart
                  FROM ekbe
                  INTO lv_bwart
                  WHERE  ( ebeln = ls_processo-ebeln AND bewtp = 'E'  AND bwart = '101' ).

              ls_processo-bewtp = lv_bewtp.
              ls_processo-bwart = lv_bwart.

              IF lv_bewtp IS NOT INITIAL AND lv_bwart IS NOT INITIAL.
                ls_processo-status = 'Recepcionada'.
              ENDIF.

            ELSE.
              ls_processo-status = 'Status Desconhecido'.
            ENDIF.

            CLEAR: lv_bewtp, lv_bwart.
            MODIFY lt_processo FROM ls_processo.
          ENDLOOP.

          APPEND LINES OF lt_processo TO gt_processo.

*      TYPES:
*        BEGIN OF ty_recepcionada,
*          ebeln TYPE eban-ebeln,
*          statu TYPE eban-statu,
*          bewtp TYPE ekbe-bewtp,
*          bwart TYPE ekbe-bwart,
*        END OF ty_recepcionada.
*
*      DATA: gt_recepcionada TYPE TABLE OF ty_recepcionada,
*            gs_recepcionada TYPE ty_recepcionada.
*
*      LOOP AT gt_processo INTO gs_processo.
*
*        IF gs_processo-banpr = '03' AND gs_processo-statu ='N'.
*          gs_processo-status = 'Aberta'.
**        MESSAGE 'Aberta' TYPE 'I'.
*        ELSEIF gs_processo-banpr ='05'.
*          gs_processo-status = 'Liberada'.
**        MESSAGE 'Liberada' TYPE 'I'.
*        ELSEIF gs_processo-ebeln IS NOT INITIAL AND gs_processo-statu = 'B' AND gs_processo-banpr = '05'.
*          gs_processo-status = 'Em Pedido'.
**        MESSAGE 'Em Pedido' TYPE 'I'.
*        ELSEIF gs_processo-ebeln IS NOT INITIAL AND gs_processo-statu = 'B'.
*
*          SELECT a~ebeln, a~statu, b~bewtp, b~bwart
*            FROM eban AS a
*            INNER JOIN ekbe AS b
*            ON a~ebeln EQ b~ebeln
*            WHERE a~ebeln EQ @gs_processo-ebeln
*            INTO CORRESPONDING FIELDS
*            OF TABLE @gt_recepcionada.
*
*          LOOP AT gt_recepcionada INTO gs_recepcionada.
*
*            IF gs_recepcionada-bewtp = 'E' AND gs_recepcionada-bwart = '101'.
*              gs_processo-status = 'Recepcionada'.
**          MESSAGE 'Recepcionada' TYPE 'I'.
*            ENDIF.
*
*          ENDLOOP.
*
*        ELSE.
*
*          gs_processo-status = 'Status Desconhecido'.
*
*        ENDIF.
*
*        MODIFY gt_processo FROM gs_processo.
*
** Verificar se tem 'Em Pedido' e ' Recepcionada' como status
*
*      ENDLOOP.
        ENDIF.
      ENDIF.

      lv_detalhe = '0011'.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0007 INPUT.

  CASE sy-ucomm.
    WHEN 'GO'.

*      SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, b~lifnr, c~name1, a~bednr, a~netpr
*        FROM ekpo AS a
*        INNER JOIN ekko AS b ON a~ebeln EQ b~ebeln
*        INNER JOIN lfa1 AS c ON c~lifnr EQ b~lifnr
*        WHERE  a~ebeln EQ @lv_numped5
*        OR a~ebelp EQ @lv_it5
*        OR upper( a~txz01 ) EQ @lv_txt5
*        OR a~matnr EQ @lv_mat5
*        OR b~lifnr EQ @lv_forn5
*        OR c~name1 EQ @lv_nmfrn5
*        OR a~bednr EQ @lv_nacomp5
*        APPENDING CORRESPONDING FIELDS
*        OF TABLE @gt_resumo.

      CLEAR: lt_resumo, r_numped5[], r_it5[], r_txt5[], r_mat5[], r_forn5[], r_nmfrn[], r_nacomp5[].

      IF lv_numped5 IS INITIAL
          AND lv_it5 IS INITIAL
          AND lv_txt5 IS INITIAL
          AND lv_mat5 IS INITIAL
          AND lv_forn5 IS INITIAL
          AND lv_nmfrn5 IS INITIAL
          AND lv_nacomp5 IS INITIAL.

        MESSAGE 'Introduza pelo menos um parâmetro!' TYPE 'I'.

      ELSE.

        IF lv_numped5 IS NOT INITIAL.
          r_numped5-sign = 'I'.
          r_numped5-option = 'EQ'.
          r_numped5-low = lv_numped5.
          APPEND r_numped5.
        ENDIF.

        IF lv_it5 IS NOT INITIAL.
          r_it5-sign = 'I'.
          r_it5-option = 'EQ'.
          r_it5-low = lv_it5.
          APPEND r_it5.
        ENDIF.

        IF lv_txt5 IS NOT INITIAL.
          r_txt5-sign = 'I'.
          r_txt5-option = 'EQ'.
          r_txt5-low = lv_txt5.
          APPEND r_txt5.
        ENDIF.

        IF lv_mat5 IS NOT INITIAL.
          r_mat5-sign = 'I'.
          r_mat5-option = 'EQ'.
          r_mat5-low = lv_mat5.
          APPEND r_mat5.
        ENDIF.

        IF lv_forn5 IS NOT INITIAL.
          r_forn5-sign = 'I'.
          r_forn5-option = 'EQ'.
          r_forn5-low = lv_forn5.
          APPEND r_forn5.
        ENDIF.

        IF lv_nmfrn5 IS NOT INITIAL.
          r_nmfrn5-sign = 'I'.
          r_nmfrn5-option = 'EQ'.
          r_nmfrn5-low = lv_nmfrn5.
          APPEND r_nmfrn5.
        ENDIF.

        IF lv_nacomp5 IS NOT INITIAL.
          r_nacomp5-sign = 'I'.
          r_nacomp5-option = 'EQ'.
          r_nacomp5-low = lv_nacomp5.
          APPEND r_nacomp5.
        ENDIF.

        SELECT ebeln ebelp txz01 matnr menge meins netpr bednr "lifnr, bedat, name1, frggr, frgsx não estão na ekpo
            FROM ekpo
            INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
            WHERE ebeln IN r_numped5
            AND ebelp IN r_it5
            AND txz01 IN r_txt5
            AND matnr IN r_mat5
*            AND name1 IN r_nmfrn5
            AND bednr IN r_nacomp5.

        IF lt_ekpo IS NOT INITIAL. "!!!!!!"
          SELECT ebeln, lifnr, bedat
          FROM ekko
          FOR ALL ENTRIES IN @lt_ekpo
          WHERE lifnr IN @r_nmfrn
          AND ebeln =  @lt_ekpo-ebeln
          INTO CORRESPONDING FIELDS OF TABLE @lt_ekko.
        ENDIF.

        IF lt_ekko IS NOT INITIAL. "!!!!!!!"
          SELECT lifnr, name1
          FROM lfa1
          FOR ALL ENTRIES IN @lt_ekko
          WHERE name1 IN @r_nmfrn5
          AND lifnr = @lt_ekko-lifnr
          INTO CORRESPONDING FIELDS OF TABLE @lt_lfa1.
        ENDIF.
      ENDIF.

      IF lt_lfa1 IS NOT INITIAL. "!!!!!!!"
        LOOP AT lt_ekpo INTO ls_ekpo.
          LOOP AT lt_ekko INTO ls_ekko.
            LOOP AT lt_lfa1 INTO ls_lfa1.

              IF ls_ekpo-ebeln = ls_ekko-ebeln
                 AND ls_ekko-lifnr = ls_lfa1-lifnr.

                ls_resumo-ebeln = ls_ekpo-ebeln.
                ls_resumo-ebelp = ls_ekpo-ebelp.
                ls_resumo-txz01 = ls_ekpo-txz01.
                ls_resumo-matnr = ls_ekpo-matnr.
                ls_resumo-bednr = ls_ekpo-bednr.

                ls_resumo-lifnr = ls_ekko-lifnr.
*              ls_resumo-bedat = ls_ekko-bedat.

*              ls_resumo-menge = ls_ekpo-menge.
*              ls_resumo-meins = ls_ekpo-meins.
                ls_resumo-netpr = ls_ekpo-netpr.
*              ls_resumo-frggr = ls_ekpo-frggr.
*              ls_resumo-frgsx = ls_ekpo-frgsx.

                ls_resumo-name1 = ls_lfa1-name1.

                APPEND ls_resumo TO lt_resumo.
                CLEAR ls_resumo.

              ENDIF.

            ENDLOOP.
          ENDLOOP.
        ENDLOOP.

        IF lt_resumo IS INITIAL.
          MESSAGE 'Não foram encontrados registos!' TYPE 'I'.
        ELSE.
          APPEND LINES OF lt_resumo TO gt_resumo.
        ENDIF.

      ENDIF.

      lv_detalhe = '0012'.

  ENDCASE.

ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE teste_modify INPUT.
  MODIFY gt_pedidos
    FROM gs_pedidos
    INDEX teste-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE teste_mark INPUT.
  DATA: g_teste_wa2 LIKE LINE OF gt_pedidos.
  IF teste-line_sel_mode = 1
  AND gs_pedidos-select = 'X'.
    LOOP AT gt_pedidos INTO g_teste_wa2
      WHERE select = 'X'.
      g_teste_wa2-select = ''.
      MODIFY gt_pedidos
        FROM g_teste_wa2
        TRANSPORTING select.
    ENDLOOP.
  ENDIF.
  MODIFY gt_pedidos
    FROM gs_pedidos
    INDEX teste-current_line
    TRANSPORTING select.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE teste_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TESTE'
                              'GT_PEDIDOS'
                              'SELECT'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'REQCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE reqcontrol_modify INPUT.
  MODIFY gt_eban
    FROM gs_eban
    INDEX reqcontrol-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'REQCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE reqcontrol_mark INPUT.
  DATA: g_reqcontrol_wa2 LIKE LINE OF gt_eban.
  IF reqcontrol-line_sel_mode = 1
  AND gs_eban-select = 'X'.
    LOOP AT gt_eban INTO g_reqcontrol_wa2
      WHERE select = 'X'.
      g_reqcontrol_wa2-select = ''.
      MODIFY gt_eban
        FROM g_reqcontrol_wa2
        TRANSPORTING select.
    ENDLOOP.
  ENDIF.
  MODIFY gt_eban
    FROM gs_eban
    INDEX reqcontrol-current_line
    TRANSPORTING select.
ENDMODULE.
*&SPWIZARD: INPUT MODULE FOR TC 'REQCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE reqcontrol_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'REQCONTROL'
                              'GT_EBAN'
                              'SELECT'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'PEDCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE pedcontrol_modify INPUT.
  MODIFY gt_pedidos
    FROM gs_pedidos
    INDEX pedcontrol-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'PEDCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE pedcontrol_mark INPUT.
  DATA: g_pedcontrol_wa2 LIKE LINE OF gt_pedidos.
  IF pedcontrol-line_sel_mode = 1
  AND gs_pedidos-select = 'X'.
    LOOP AT gt_pedidos INTO g_pedcontrol_wa2
      WHERE select = 'X'.
      g_pedcontrol_wa2-select = ''.
      MODIFY gt_pedidos
        FROM g_pedcontrol_wa2
        TRANSPORTING select.
    ENDLOOP.
  ENDIF.
  MODIFY gt_pedidos
    FROM gs_pedidos
    INDEX pedcontrol-current_line
    TRANSPORTING select.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'PEDCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE pedcontrol_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'PEDCONTROL'
                              'GT_PEDIDOS'
                              'SELECT'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'PROCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE procontrol_modify INPUT.
  MODIFY gt_processo
    FROM gs_processo
    INDEX procontrol-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'PROCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE procontrol_mark INPUT.
  DATA: g_procontrol_wa2 LIKE LINE OF gt_processo.
  IF procontrol-line_sel_mode = 1
  AND gs_processo-select = 'X'.
    LOOP AT gt_processo INTO g_procontrol_wa2
      WHERE select = 'X'.
      g_procontrol_wa2-select = ''.
      MODIFY gt_processo
        FROM g_procontrol_wa2
        TRANSPORTING select.
    ENDLOOP.
  ENDIF.
  MODIFY gt_processo
    FROM gs_processo
    INDEX procontrol-current_line
    TRANSPORTING select.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'PROCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE procontrol_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'PROCONTROL'
                              'GT_PROCESSO'
                              'SELECT'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.
