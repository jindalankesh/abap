class ZCL_ZCOM_LAR_PDF_DPC_EXT definition
  public
  inheriting from ZCL_ZCOM_LAR_PDF_DPC
  create public .

public section.
protected section.

  methods EVENTDETSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZCOM_LAR_PDF_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZCOM_LAR_PDF_DPC_EXT->EVENTDETSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZCOM_LAR_PDF_MPC=>TT_EVENTDET
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD eventdetset_get_entityset.

    DATA(rs_event) =  zcl_com_lar_out=>build_event_tab( it_filter_select_options = it_filter_select_options ).

    IF rs_event-eventid IS NOT INITIAL.
      DATA(lv_pdfxstring) = zcl_com_lar_out=>gen_pdf_out(
        EXPORTING
          is_event   = rs_event
          iv_preview = abap_false
          iv_getpdf  = abap_true
      ).
    ENDIF.
    et_entityset = VALUE #( ( eventid    = rs_event-eventid
                              pdfxstring = lv_pdfxstring ) ).
  ENDMETHOD.
ENDCLASS.
