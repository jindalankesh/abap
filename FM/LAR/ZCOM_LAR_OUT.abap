*&---------------------------------------------------------------------*
*& Report Z_AJ_ADOBE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcom_lar_out.

TABLES: vbak,mara.

SELECT-OPTIONS : s_event FOR vbak-zz1_eventid_sdh NO-EXTENSION NO INTERVALS,
                 s_booth FOR vbak-zz1_boothso_sdh,
                 s_matnr FOR mara-matnr,
                 s_vbeln FOR vbak-vbeln.

*PARAMETERS : p_dummy TYPE char1 DEFAULT ''.

START-OF-SELECTION.
  DATA(lt_filter_select_options) = VALUE /iwbep/t_mgw_select_option(
                                                              ( property       = 'EventId'
                                                                select_options =
                                                              VALUE /iwbep/t_cod_select_options(
                                                                                                 FOR ls_sel_event IN s_event ( CORRESPONDING #( ls_sel_event ) )
                                                             ) )
                                                              ( property       = 'BoothNumber'
                                                                select_options =
                                                             VALUE /iwbep/t_cod_select_options(
                                                                                                 FOR ls_booth IN s_booth ( CORRESPONDING #( ls_booth ) )
                                                             ) )
                                                              ( property       = 'Material'
                                                                select_options =
                                                             VALUE /iwbep/t_cod_select_options(
                                                                                                 FOR ls_matnr IN s_matnr ( CORRESPONDING #( ls_matnr ) )
                                                             ) )
                                                              ( property       = 'SalesDoc'
                                                                select_options =
                                                             VALUE /iwbep/t_cod_select_options(
                                                                                                 FOR ls_vbeln IN s_vbeln ( CORRESPONDING #( ls_vbeln ) )
                                                             ) )
  ).

*  IF p_dummy EQ space.
  DATA(ls_event) = zcl_com_lar_out=>build_event_tab(
    EXPORTING
      it_filter_select_options = lt_filter_select_options ).
*  ELSE.
*    ls_event = zcl_com_lar_out=>build_event_tab_dummy(  ).
*  ENDIF.

  zcl_com_lar_out=>gen_pdf_out( EXPORTING is_event   = ls_event
                                          iv_preview = abap_true ).
