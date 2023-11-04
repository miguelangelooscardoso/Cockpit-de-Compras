*&---------------------------------------------------------------------*
*& Include          ZCOCKPIT_MC_TOP
*&---------------------------------------------------------------------*

TABLES: eban, ekpo, ekko, t16fc.

*** Minhas Requisições (1.1) ***
DATA: lv_reqnum TYPE eban-banfn, "Variável local screen painter"
      lv_it1    TYPE eban-bnfpo,
      lv_txt1   TYPE eban-txz01,
      lv_mat1   TYPE eban-matnr.

RANGES: r_reqnum FOR eban-banfn, "Variável local screen painter"
        r_it1    FOR eban-bnfpo,
        r_txt1   FOR eban-txz01,
        r_mat1   FOR eban-matnr.

*** Meus Pedidos de Compra (1.2) ***
DATA: lv_numped TYPE ekpo-ebeln,
      lv_it2    TYPE ekpo-ebelp,
      lv_txt2   TYPE ekpo-txz01,
      lv_mat2   TYPE ekpo-matnr,
      lv_forn   TYPE ekko-lifnr, " número conta do fornecedor EKPO-LIFNR não existe!!!
      lv_nmfrn  TYPE lfa1-name1.

RANGES: r_numped FOR ekpo-ebeln,
        r_it2    FOR ekpo-ebelp,
        r_txt2   FOR ekpo-txz01,
        r_mat2   FOR ekpo-matnr,
        r_forn   FOR ekko-lifnr, " número conta do fornecedor EKPO-LIFNR não existe!!!
        r_nmfrn  FOR lfa1-name1.

DATA: gt_ekpo TYPE TABLE OF ekpo,
      gs_ekpo TYPE ekpo.

DATA: gt_ekko TYPE TABLE OF ekko,
      gs_ekko TYPE ekko.

DATA: gt_lfa1 TYPE TABLE OF lfa1,
      gs_lfa1 TYPE lfa1.

DATA: lv_screen(4)  TYPE c VALUE '0003',
      lv_detalhe(4) TYPE c VALUE '9999',
      p_req         TYPE c,
      p_pdd         TYPE c,
      p_prc         TYPE c,
      p_proreq      TYPE c,
      p_nacomp      TYPE c.

TYPES: BEGIN OF ty_eban,
         banfn  LIKE eban-banfn,
         bnfpo  LIKE eban-bnfpo,
         txz01  LIKE eban-txz01,
         matnr  LIKE eban-matnr,
         bednr  LIKE eban-bednr,
         lfdat  LIKE eban-lfdat,
         menge  LIKE eban-menge,
         meins  LIKE eban-meins,
         frggr  LIKE eban-frggr,
         frgst  LIKE eban-frgst,
         select TYPE xfeld,
       END OF ty_eban.

DATA: gt_eban TYPE TABLE OF ty_eban,
      lt_eban TYPE TABLE OF ty_eban,
      gs_eban TYPE ty_eban.

TYPES:
  BEGIN OF ty_pedidos,
    ebeln  TYPE ekpo-ebeln,
    ebelp  TYPE ekpo-ebelp,
    txz01  TYPE ekpo-txz01,
    matnr  TYPE ekpo-matnr,
    lifnr  TYPE ekko-lifnr, " número conta do fornecedor EKPO-LIFNR não existe!!!
    name1  TYPE lfa1-name1,
    bedat  TYPE ekko-bedat,
    menge  TYPE ekpo-menge,
    meins  TYPE ekpo-meins,
    netpr  TYPE ekpo-netpr,
    frggr  LIKE ekko-frggr,
    frgsx  LIKE ekko-frgsx,
    select TYPE xfeld,
  END OF ty_pedidos.

* Verificar se necessário

DATA: gt_pedidos TYPE TABLE OF ty_pedidos,
      lt_pedidos TYPE TABLE OF ty_pedidos,
      gs_pedidos TYPE ty_pedidos,
      ls_pedidos TYPE ty_pedidos.

DATA: lt_ekpo TYPE TABLE OF ekpo,
      ls_ekpo TYPE ekpo,
      lt_ekko TYPE TABLE OF ekko,
      ls_ekko TYPE ekko,
      lt_lfa1 TYPE TABLE OF lfa1,
      ls_lfa1 TYPE lfa1.

*** Preços (1.3) ***
TYPES: BEGIN OF ty_preco,
         org_compras TYPE eine-ekorg, " Organização de Compras
         centro      TYPE eine-werks, " Centro
         nif         TYPE lfa1-stceg, " NIF Fornecedor
*         mat_forn    TYPE eina-idnlf, " Nº material Fornecedor
         info        TYPE eina-infnr, " Nº Registo Info eina-infnr
         prc_unit    TYPE eine-netpr, " Preço Unitário
         codfrn      TYPE lfa1-lifnr, " Código do Fornecedor
         codmat      TYPE eina-matnr, " Código do Material
       END OF ty_preco.

DATA: loc_dir TYPE rlgrap-filename.

*data: gv_file_loc  type RLGRAP-FILENAME,
*      gv_file_name type string.

DATA: gt_preco TYPE TABLE OF ty_preco,
      lt_preco TYPE TABLE OF ty_preco,
      gs_preco TYPE ty_preco,
      ls_preco TYPE ty_preco.

DATA: lt_eina TYPE TABLE OF eina,
      ls_eina TYPE eina.

*** Ver processo por requisição (2.1) ***
DATA: lv_reqnum4 TYPE eban-banfn,
      lv_it4     TYPE eban-bnfpo,
      lv_txt4    TYPE eban-txz01,
      lv_mat4    TYPE eban-matnr,
      lv_nacomp4 TYPE eban-bednr.

RANGES: r_reqnum4 FOR eban-banfn,
        r_it4     FOR eban-bnfpo,
        r_txt4    FOR eban-txz01,
        r_mat4    FOR eban-matnr,
        r_nacomp4 FOR eban-bednr.

TYPES:
  BEGIN OF ty_processo,
    ebeln  TYPE eban-ebeln,
    banfn  TYPE eban-banfn,
    bnfpo  TYPE eban-bnfpo,
    txz01  TYPE eban-txz01,
    matnr  TYPE eban-matnr,
    bednr  TYPE eban-bednr,
    banpr  TYPE eban-banpr,
    statu  TYPE eban-statu,
    bewtp  TYPE ekbe-bewtp,
    bwart  TYPE ekbe-bwart,
    status TYPE string,
    select TYPE xfeld,
  END OF ty_processo.

DATA: gt_processo TYPE TABLE OF ty_processo,
      lt_processo TYPE TABLE OF ty_processo,
      gs_processo TYPE ty_processo,
      ls_processo TYPE ty_processo.

*** Ver resumo por número de acompanhamento (2.2) ***
DATA: lv_numped5 TYPE ekpo-ebeln, " nº pedido
      lv_it5     TYPE ekpo-ebelp, " item
      lv_txt5    TYPE ekpo-txz01, " texto (material)
      lv_mat5    TYPE ekpo-matnr, " material
      lv_forn5   TYPE ekko-lifnr, " fornecedor
      lv_nmfrn5  TYPE lfa1-name1, " nome conta do fornecedor EKPO-LIFNR não existe!!!
      lv_nacomp5 TYPE ekpo-bednr. " número acompanhamento

RANGES: r_numped5 FOR ekpo-ebeln, " nº pedido
        r_it5     FOR ekpo-ebelp, " item
     	  r_txt5    FOR ekpo-txz01, " texto (material)
        r_mat5    FOR ekpo-matnr, " material
        r_forn5   FOR ekko-lifnr, " fornecedor
        r_nmfrn5  FOR lfa1-name1, " número conta do fornecedor EKPO-LIFNR não existe!!!
        r_nacomp5 FOR ekpo-bednr. " número acompanhamento

TYPES:
  BEGIN OF ty_resumo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    txz01 TYPE ekpo-txz01,
    matnr TYPE ekpo-matnr,
    lifnr TYPE ekko-lifnr, " número conta do fornecedor EKPO-LIFNR não existe!!!
    name1 TYPE lfa1-name1,
    bednr TYPE ekpo-bednr,
    netpr TYPE ekpo-netpr,
  END OF ty_resumo.

DATA: gt_resumo TYPE TABLE OF ty_resumo,
      lt_resumo TYPE TABLE OF ty_resumo,
      gs_resumo TYPE ty_resumo,
      ls_resumo TYPE ty_resumo.

*** BAPI ***

DATA: lv_frggr TYPE frggr,
      lv_frgco TYPE frgco,
      lv_frgst TYPE frgst,
      lv_frgab TYPE frgab,
      lv_frgc1 TYPE frgco.

TYPES:
  BEGIN OF ty_inner,
    banfn TYPE eban-banfn,
    bnfpo TYPE eban-bnfpo,
    frggr TYPE eban-frggr,
    frgst TYPE eban-frgst,
    frgsx TYPE t16fs-frgsx,
    frgc1 TYPE t16fs-frgc1,
  END OF ty_inner.

DATA: gt_inner TYPE TABLE OF ty_inner,
      gs_inner TYPE ty_inner.

DATA:
  lt_return TYPE TABLE OF bapireturn,
  ls_return TYPE bapireturn.

TYPES:
  BEGIN OF ty_po,
    ebeln TYPE ekko-ebeln,
    frggr TYPE ekko-frggr,
    frgsx TYPE ekko-frgsx, " estratégia de liberação
    frgc1 TYPE t16fs-frgc1,
  END OF ty_po.

DATA: gt_po TYPE TABLE OF ty_po,
      gs_po TYPE ty_po.

** Usando vários codigos de função. **

DATA:
  frgc  TYPE string,
  index TYPE numc1,
  frgci TYPE string.

TYPES: BEGIN OF ty_frgci,
         frgco TYPE frgco,
       END OF ty_frgci.

DATA:
  gt_frgci TYPE TABLE OF ty_frgci,
  gs_frgci TYPE ty_frgci.

DATA:
*  lv_frggr TYPE frggr,
  lv_frgsx TYPE frgsx. " estratégia de liberação


*** Call Transaction ***

DATA: gt_bdcdata TYPE TABLE OF bdcdata, " OCCURS 0 WITH HEADER LINE,
      gs_bdcdata TYPE bdcdata.

DATA: lt_messtab TYPE TABLE OF bdcmsgcoll,
      ls_messtab TYPE          bdcmsgcoll.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'REQCONTROL' ITSELF
CONTROLS: reqcontrol TYPE TABLEVIEW USING SCREEN 0008.

*&SPWIZARD: LINES OF TABLECONTROL 'REQCONTROL'
DATA:     g_reqcontrol_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'PEDCONTROL' ITSELF
CONTROLS: pedcontrol TYPE TABLEVIEW USING SCREEN 0009.

*&SPWIZARD: LINES OF TABLECONTROL 'PEDCONTROL'
DATA:     g_pedcontrol_lines  LIKE sy-loopc.

*DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'PROCONTROL' ITSELF
CONTROLS: procontrol TYPE TABLEVIEW USING SCREEN 0011.

*&SPWIZARD: LINES OF TABLECONTROL 'PROCONTROL'
DATA:     g_procontrol_lines  LIKE sy-loopc.

*DATA:     OK_CODE LIKE SY-UCOMM.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TESTE' ITSELF
CONTROLS: teste TYPE TABLEVIEW USING SCREEN 8888.

*&SPWIZARD: LINES OF TABLECONTROL 'TESTE'
DATA:     g_teste_lines  LIKE sy-loopc.

*DATA:     OK_CODE LIKE SY-UCOMM.
