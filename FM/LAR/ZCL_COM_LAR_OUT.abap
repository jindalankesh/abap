CLASS zcl_com_lar_out DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_string,
        comm TYPE string,
      END OF ty_string,
      tyt_string TYPE TABLE OF ty_string WITH KEY comm.
    TYPES:
      BEGIN OF ty_selcom,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        t_ver TYPE tyt_string,
      END OF ty_selcom .
    TYPES:
      tyt_selcom TYPE TABLE OF ty_selcom WITH KEY vbeln posnr .

    TYPES:
      BEGIN OF ty_soi,
        salesdoc          TYPE vbeln,
        salesdocitem      TYPE posnr,
        material          TYPE  matnr,
        producthierrachy  TYPE  prodh_d,
        materialgroup     TYPE  prodh1,
        materialsubgroup  TYPE  char10,
        targetmoveindate  TYPE  zz1_target_mov_in,
        laborstatedate    TYPE  zz1_lbrast,
        laborstatetime    TYPE  zz1_lbrastt,
        super             TYPE  zz1_lbrsv_d,
        people            TYPE  zz1_lbrppl,
        ahours(9)         TYPE  p DECIMALS 2,
        ticket            TYPE  zz1_tktnum,
        comments          TYPE  zz1_worktobeperformed,
        description       TYPE  arktx,
        quantity(13)      TYPE p DECIMALS 0,
        unitprice         TYPE  netwr_ak,
        unitpricecurrency TYPE  waerk,
        discount          TYPE zboothsqft,
        extendedprice	    TYPE netwr_ak,
        t_ver             TYPE tyt_string,
      END OF ty_soi .
    TYPES:
      tyt_soi TYPE  TABLE OF ty_soi WITH KEY salesdoc salesdocitem .
    TYPES:
      BEGIN OF ty_soh,
        boothnumber    TYPE zz1_boothso,
        salesdoc       TYPE vbeln,
        socreationdate TYPE erdat,
        bootharea(7)   TYPE p DECIMALS 0,
        total_order    TYPE string,
        t_soi          TYPE tyt_soi,
      END OF ty_soh .
    TYPES:
      tyt_soh TYPE  TABLE OF  ty_soh WITH KEY boothnumber salesdoc .
    TYPES:
      BEGIN OF ty_booth,
        eventid      TYPE zz1_eventid,
        boothnumber  TYPE zz1_boothso,
        customer     TYPE kunag,
        customername TYPE md_customer_name,
        total_booth  TYPE string,
        t_soh        TYPE tyt_soh,
      END OF ty_booth .
    TYPES:
      tyt_booth TYPE  TABLE OF  ty_booth WITH KEY eventid boothnumber .
    TYPES:
      BEGIN OF ty_event,
        eventid       TYPE zz1_eventid,
        eventname     TYPE ps_post1,
        showopendate  TYPE ps_plfaz_chg,
        showstarttime TYPE zz1_show_start_time,
        showclosedate TYPE ps_plsez_chg,
        showendtime   TYPE zz1_show_end_time,
        cursysdate    TYPE dats,
        cursystime    TYPE tims,
        lastupddate   TYPE dats,
        lastupdtime   TYPE tims,
        total_show    TYPE  string,
        t_booth       TYPE tyt_booth,
      END OF ty_event .


    CLASS-METHODS build_event_tab_dummy
      RETURNING
        VALUE(rs_event) TYPE ty_event .
    CLASS-METHODS gen_pdf_out
      IMPORTING
        !is_event            TYPE zcl_com_lar_out=>ty_event
        !iv_preview          TYPE char1 OPTIONAL
        !iv_getpdf           TYPE fpgetpdf OPTIONAL
      RETURNING
        VALUE(rv_pdfxstring) TYPE string .
    CLASS-METHODS build_event_tab
      IMPORTING
        !it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING
        VALUE(rs_event)           TYPE ty_event .
    CLASS-METHODS build_sel_comment
      IMPORTING
        !it_vbeln        TYPE tms_t_vbeln_range
      RETURNING
        VALUE(rt_selcom) TYPE tyt_selcom .
*
  PROTECTED SECTION.
private section.

  class-data GT_BOOTH type TYT_BOOTH .
  class-data WA_BOOTH type TY_BOOTH .
  class-data GT_SOH type TYT_SOH .
  class-data WA_SOH type TY_SOH .
  class-data GT_SOI type TYT_SOI .
  class-data WA_SOI type TY_SOI .
  constants GC_FORMNAME type FPNAME value 'ZAF_COM_LAR_OUT' ##NO_TEXT.
  constants GC_AN_LAR type ZAPPNAME value 'LAR' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_COM_LAR_OUT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COM_LAR_OUT=>BUILD_EVENT_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [<-()] RS_EVENT                       TYPE        TY_EVENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_event_tab.

    DATA: lt_select_option TYPE /iwbep/t_mgw_select_option,
          lt_lar_data      TYPE ztt_com_lar_out.

    DATA : lt_soi         TYPE tyt_soi,
           lt_soh         TYPE tyt_soh,
           lt_booth       TYPE tyt_booth,
           lv_total_order TYPE netwr,
           lv_total_booth TYPE netwr,
           lv_total_show  TYPE netwr.

    lt_select_option = it_filter_select_options.

    DATA(lr_eventid) = VALUE #( lt_select_option[ property = 'EventId' ]-select_options OPTIONAL ).
    DATA(lr_boothnumber) = VALUE #( lt_select_option[ property = 'BoothNumber' ]-select_options OPTIONAL ).
    DATA(lr_producthierrachy) = VALUE #( lt_select_option[ property = 'ProductHierrachy' ]-select_options OPTIONAL ).
    DATA(lr_material) = VALUE #( lt_select_option[ property = 'Material' ]-select_options OPTIONAL ).
    DATA(lr_materialgroup) = VALUE #( lt_select_option[ property = 'MaterialGroup' ]-select_options OPTIONAL ).
    DATA(lr_materialsubgroup) = VALUE #( lt_select_option[ property = 'MaterialSubGroup' ]-select_options OPTIONAL ).
    DATA(lr_vbeln)  = VALUE #( lt_select_option[ property = 'SalesDoc' ]-select_options OPTIONAL ).
    DATA(lr_laboractualenddate) = VALUE #( lt_select_option[ property = 'LaborActualEndDate' ]-select_options OPTIONAL ).
    DATA(lr_laboractualendtime) = VALUE #( lt_select_option[ property = 'LaborActualEndTime' ]-select_options OPTIONAL ).


    SELECT *
      FROM zc_com_lar_sdata
      WHERE eventid IN @lr_eventid
        AND boothnumber IN @lr_boothnumber
        AND producthierrachy IN @lr_producthierrachy
        AND material IN @lr_material
        AND materialgroup IN @lr_materialgroup
        AND materialsubgroup IN @lr_materialsubgroup
        AND salesdoc IN @lr_vbeln
        AND laboractualenddate IN @lr_laboractualenddate
        AND laboractualendtime IN @lr_laboractualendtime
        ORDER BY eventid, boothnumber, salesdoc, salesdocitem
       INTO TABLE @DATA(lt_lar_cds).
    IF sy-subrc = 0.

      SELECT SINGLE *
          FROM ztcom_lastupd INTO @DATA(ls_lastupd)
         WHERE appname = @gc_an_lar.
      IF sy-subrc = 0.
        DELETE FROM ztcom_lastupd WHERE appname = gc_an_lar.
        rs_event-lastupddate = ls_lastupd-lastupddt.
        rs_event-lastupdtime = ls_lastupd-lastupdtime.
      ELSE.
        rs_event-lastupddate = sy-datum.
        rs_event-lastupdtime = sy-uzeit.
      ENDIF.
      ls_lastupd = VALUE #( appname = gc_an_lar lastupddt = sy-datum lastupdtime = sy-uzeit lastupdby = sy-uname ).
      MODIFY ztcom_lastupd FROM ls_lastupd.

      DATA(lt_selcom) = zcl_com_lar_out=>build_sel_comment( it_vbeln =
                                                                       VALUE tms_t_vbeln_range( FOR ls_lar IN lt_lar_cds ( sign = 'I' option = 'EQ' low = ls_lar-salesdoc ) )
                                                            ).

      lt_lar_data = CORRESPONDING #( lt_lar_cds ).

      SORT lt_lar_data BY eventid boothnumber salesdoc salesdocitem.

      LOOP AT lt_lar_data ASSIGNING FIELD-SYMBOL(<ls_booth>)
                          GROUP BY (  boothnumber = <ls_booth>-boothnumber  ).


        LOOP AT GROUP  <ls_booth> ASSIGNING FIELD-SYMBOL(<ls_soh>)
                           GROUP BY (   boothnumber = <ls_soh>-boothnumber
                                        salesdoc    = <ls_soh>-salesdoc ).

          LOOP AT GROUP <ls_soh> ASSIGNING FIELD-SYMBOL(<ls_soi>)
                                       GROUP BY (   boothnumber = <ls_soi>-boothnumber
                                                    salesdoc    = <ls_soi>-salesdoc
                                                    salesdocitem  = <ls_soi>-salesdocitem ).

            lt_soi = VALUE #( BASE lt_soi ( VALUE ty_soi(
                                                          salesdoc          = <ls_soi>-salesdoc
                                                          salesdocitem      = <ls_soi>-salesdocitem
                                                          material          = <ls_soi>-material
                                                          producthierrachy  = <ls_soi>-producthierrachy
                                                          materialgroup     = <ls_soi>-materialgroup
                                                          materialsubgroup  = <ls_soi>-materialsubgroup
                                                          targetmoveindate  = <ls_soi>-targetmoveindate
                                                          laborstatedate    = <ls_soi>-laborstatedate
                                                          laborstatetime    = <ls_soi>-laborstatetime
                                                          super             = <ls_soi>-supername
                                                          people            = <ls_soi>-people
                                                          ahours            = <ls_soi>-ahours
                                                          ticket            = <ls_soi>-ticket
                                                          comments          = <ls_soi>-comments
                                                          description       = <ls_soi>-description
                                                          quantity          = <ls_soi>-quantity
                                                          unitprice         = <ls_soi>-unitprice
                                                          unitpricecurrency = <ls_soi>-unitpricecurrency
                                                          discount          = <ls_soi>-discount
                                                          extendedprice     = <ls_soi>-extendedprice
                                                          t_ver             = VALUE #( lt_selcom[ vbeln = <ls_soi>-salesdoc posnr = <ls_soi>-salesdocitem ]-t_ver OPTIONAL )
                                                          ) ) ).


            lv_total_order += <ls_soi>-unitprice.

          ENDLOOP.

          lt_soh = VALUE #( BASE lt_soh ( VALUE ty_soh(                "WA LS_SOH
                                                        boothnumber    = <ls_soh>-boothnumber
                                                        salesdoc       = <ls_soh>-salesdoc
                                                        socreationdate = <ls_soh>-socreationdate
                                                        bootharea      = <ls_soh>-bootharea
                                                        total_order    = |${ lv_total_order } |
                                                        t_soi          = lt_soi ) ) ).

          lv_total_booth += lv_total_order.
          CLEAR: lv_total_order, lt_soi.
        ENDLOOP.

        lt_booth = VALUE #( BASE lt_booth ( VALUE ty_booth(            "WA LS_BOOTH
                                                            eventid      = <ls_booth>-eventid
                                                            boothnumber  = <ls_booth>-boothnumber
                                                            customer     = <ls_booth>-customer
                                                            customername = <ls_booth>-customername
                                                            total_booth  = |${ lv_total_booth } |
                                                            t_soh        = lt_soh ) ) ).
        lv_total_show += lv_total_booth.
        CLEAR: lv_total_booth, lt_soh.
      ENDLOOP.

      rs_event-eventid = <ls_booth>-eventid.
      rs_event-eventname = <ls_booth>-eventname.
      rs_event-showopendate = <ls_booth>-showopendate.
      rs_event-showstarttime = <ls_booth>-showstarttime.
      rs_event-showclosedate =  <ls_booth>-showclosedate.
      rs_event-showendtime = <ls_booth>-showendtime.
      rs_event-total_show  = |${ lv_total_show } |.
      rs_event-t_booth = lt_booth.
    ENDIF.
    rs_event-cursysdate = sy-datum.
    rs_event-cursystime = sy-uzeit.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COM_LAR_OUT=>BUILD_EVENT_TAB_DUMMY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_EVENT                       TYPE        TY_EVENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_event_tab_dummy.


*    wa_soi-salesdoc = '1000000011'.
*    wa_soi-material =  'MATERIAL11-A'.
*    wa_soi-producthierrachy = 'PRODH11-A'.
*    wa_soi-people = 10.
*    wa_soi-unitprice = 100.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000011'.
*    wa_soi-material =  'MATERIAL11-B'.
*    wa_soi-producthierrachy = 'PRODH11-B'.
*    wa_soi-people = 5.
*    wa_soi-unitprice = 150.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000012'.
*    wa_soi-material =  'MATERIAL12-A'.
*    wa_soi-producthierrachy = 'PRODH12-A'.
*    wa_soi-people = 15.
*    wa_soi-unitprice = 100.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000012'.
*    wa_soi-material =  'MATERIAL12-B'.
*    wa_soi-producthierrachy = 'PRODH12-B'.
*    wa_soi-people = 20.
*    wa_soi-unitprice = 250.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000013'.
*    wa_soi-material =  'MATERIAL13-A'.
*    wa_soi-producthierrachy = 'PRODH13-A'.
*    wa_soi-people = 25.
*    wa_soi-unitprice = 50.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000013'.
*    wa_soi-material =  'MATERIAL13-B'.
*    wa_soi-producthierrachy = 'PRODH13-B'.
*    wa_soi-people = 30.
*    wa_soi-unitprice = 100.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soi-salesdoc = '1000000013'.
*    wa_soi-material =  'MATERIAL13-V'.
*    wa_soi-producthierrachy = 'PRODH13-V'.
*    wa_soi-people = 35.
*    wa_soi-unitprice = 150.
*    APPEND wa_soi TO gt_soi.
*
*    wa_soh-boothnumber = 'BOOTH1'.
*    wa_soh-salesdoc = '1000000011'.
*    APPEND wa_soh TO gt_soh.
*
*    wa_soh-boothnumber = 'BOOTH1'.
*    wa_soh-salesdoc = '1000000012'.
*    APPEND wa_soh TO gt_soh.
*
*    wa_soh-boothnumber = 'BOOTH2'.
*    wa_soh-salesdoc = '1000000013'.
*    APPEND wa_soh TO gt_soh.
*
*    wa_booth-eventid = 'EVENT1'.
*    wa_booth-boothnumber =  'BOOTH1'.
*    wa_booth-customer = 'CUSTOMER1'.
*    APPEND wa_booth TO gt_booth.
*
*    wa_booth-eventid = 'EVENT1'.
*    wa_booth-boothnumber =  'BOOTH2'.
*    wa_booth-customer = 'CUSTOMER2'.
*    APPEND wa_booth TO gt_booth.
*
*    LOOP AT gt_booth ASSIGNING FIELD-SYMBOL(<ls_booth>).
*
*      LOOP AT gt_soh ASSIGNING FIELD-SYMBOL(<ls_soh>) WHERE boothnumber EQ <ls_booth>-boothnumber .
*        LOOP AT gt_soi ASSIGNING FIELD-SYMBOL(<ls_soi>) WHERE salesdoc EQ <ls_soh>-salesdoc.
*          APPEND  <ls_soi> TO   <ls_soh>-t_soi.
*          <ls_soh>-total_order =  <ls_soh>-total_order + <ls_soi>-unitprice.
*        ENDLOOP.
*        APPEND <ls_soh> TO <ls_booth>-t_soh.
*        <ls_booth>-total_booth = <ls_booth>-total_booth + <ls_soh>-total_order.
*      ENDLOOP.
*      rs_event-total_show = rs_event-total_show +  <ls_booth>-total_booth.
*    ENDLOOP.
*
*    rs_event-eventid = 'EVENT1'.
*    rs_event-eventname = 'EVENT1NAME'.
*    rs_event-showopendate = sy-datum - 30.
*    rs_event-showstarttime = sy-uzeit - 60.
*    rs_event-showclosedate =  sy-datum.
*    rs_event-showendtime = sy-uzeit.
*    rs_event-t_booth = gt_booth.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COM_LAR_OUT=>GEN_PDF_OUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_EVENT                       TYPE        ZCL_COM_LAR_OUT=>TY_EVENT
* | [--->] IV_PREVIEW                     TYPE        CHAR1(optional)
* | [--->] IV_GETPDF                      TYPE        FPGETPDF(optional)
* | [<-()] RV_PDFXSTRING                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD gen_pdf_out.

    DATA: ls_outputparams  TYPE sfpoutputparams,
          lv_gen_fm_name   TYPE rs38l_fnam,
          ls_formoutput    TYPE fpformoutput,
          lv_base64        TYPE string,
          lt_select_option TYPE /iwbep/t_mgw_select_option,
          ls_docparams     TYPE sfpdocparams,
          ls_exbset        TYPE  zcl_zcom_lar_pdf_mpc=>ts_eventdet,
          lt_lar_data      TYPE ztt_com_lar_out.


*To pass the output type parameters
    ls_outputparams-dest = 'LP01'.
    ls_outputparams-preview = iv_preview.
    ls_outputparams-getpdf = iv_getpdf.
    ls_outputparams-nodialog = 'X'.

* Calling FM to open the form
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*To pass the formname and get the function module
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = gc_formname
      IMPORTING
        e_funcname = lv_gen_fm_name.

    CALL FUNCTION lv_gen_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        is_event           = is_event
      IMPORTING
        /1bcdwb/formoutput = ls_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF iv_getpdf EQ abap_true.
*   Convert Xstring to base64
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = ls_formoutput-pdf   "Xstring
        IMPORTING
          output = rv_pdfxstring.     "Base64 String
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COM_LAR_OUT=>BUILD_SEL_COMMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_VBELN                       TYPE        TMS_T_VBELN_RANGE
* | [<-()] RT_SELCOM                      TYPE        TYT_SELCOM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_sel_comment.
    DATA:
      lt_ver TYPE TABLE OF zscom_sel_comments,
      ls_ver TYPE zscom_sel_comments.

    DATA(lr_vbeln) = it_vbeln.
    DELETE ADJACENT DUPLICATES FROM lr_vbeln COMPARING ALL FIELDS.
    CHECK lr_vbeln IS NOT INITIAL.
    SELECT
          a~vbeln ,
          a~posnr,
          a~matnr,
          b~atnam,
          b~atwrt,
          c~questions,
          c~fieldname
    FROM vbap AS a
    LEFT OUTER JOIN zvcom_characteristic_values AS b
    ON a~matnr = b~objek
    LEFT OUTER JOIN ztcom_fieldgroup AS c
    ON b~atwrt = c~charname
       FOR ALL ENTRIES IN @lr_vbeln
    WHERE a~vbeln = @lr_vbeln-low
      AND a~mvgr4 = 'L'
      AND a~abgru IS INITIAL
      AND b~atnam = 'QNA_TEMPLATE_ID'
      AND b~klart = '001'

    INTO TABLE @DATA(lt_ques).
    IF sy-subrc = 0.
      SORT lt_ques BY posnr.
    ENDIF.

    SELECT * FROM vbap
      WHERE vbeln IN @lr_vbeln
        AND mvgr4 = 'L'
        AND abgru IS INITIAL
      INTO TABLE @DATA(lt_vbap).

    LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<lfs_vbap>).
**** selection and comments
      DATA(lv_desc) = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_GRAPHICORIENTATIO_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_graphicorientatio_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIDES_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }|  && | | && |:| && | | && |{  <lfs_vbap>-zz1_sides_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_BACKINGMATERIAL_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }|  && | | && |:| && | | && |{ <lfs_vbap>-zz1_backingmaterial_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_PRODFILELOC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | &&  |:| && | | && |{ <lfs_vbap>-zz1_prodfileloc_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_GRAPHICTEXT_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | &&  |:| && | | && |{ <lfs_vbap>-zz1_graphictext_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ADDITIONINSTRU_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_additioninstru_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_DIMENSIONS_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| &&  | | && |{ <lfs_vbap>-zz1_dimensions_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNLOCATION_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signlocation_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_PRINTINGMATERIAL_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_printingmaterial_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ADDONS_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_addons_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNTYPE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signtype_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ASSEMBLYSUPER_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_assemblysuper_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_BOOTSIGNLOC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| &&  | | && |{ <lfs_vbap>-zz1_bootsignloc_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHIPMENTS_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shipments_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_FREIGHTPIECES_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_freightpieces_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHIPMENTWEIGHT_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shipmentweight_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHIPMENTDESTI_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shipmentdesti_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[  vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_DISPATCHLOC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_dispatchloc_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ESTIMATEREQ_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_estimatereq_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_FREIGHTPICKUPL_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_freightpickupl_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_COMPANYNAME_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_companyname_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHOWBOOTH_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_showbooth_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ADDRESS_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_address_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHIPPINGCONTAC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shippingcontac_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHIPMENTTIME_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shipmenttime_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_PIECESWEIGHT_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_piecesweight_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.
*
      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SPECIALINSTRUC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_specialinstruc_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_DOCKINGINFO_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_dockinginfo_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_DELIVERYINFO_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_deliveryinfo_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_CARPETCOLOR_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_carpetcolor_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_EXHIDELIVERYLOC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_exhideliveryloc_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHOWOPENDATE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_showopendate_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ITEMLOCATION_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_itemlocation_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SHELFATTACHMENT_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_shelfattachment_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_CONTENTSOURCES_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_contentsources_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_contentsources_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_othercontentsrc_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_DEVICECONNECTION_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_deviceconnection_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_deviceconnection_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_otherdevicecon_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln  fieldname = 'ZZ1_MONITORDISPLAY_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question =  |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_monitordisplay_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_MICROPHONE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_microphone_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_TEXTCOLOR_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_textcolor_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_textcolor_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_othertextcolor_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SUPPORTMETHOD_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_supportmethod_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_supportmethod_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_othersupportmethod_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_TEXTSIDE1_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_textside1_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ELECTRICITYVH_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_electricityvh_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_BANNERFINISHING_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_bannerfinishing_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_bannerfinishing_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_otherbannerfinish_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNROTATIONVH_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signrotationvh_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_FREEMANASSEMBLYVH_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_freemanassemblyvh_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SUPERVISIONFEEMESS_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_supervisionfeemess_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_EQUIPMENTREQUIRED_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_equipmentrequired_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_LIFTHEIGHT_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_liftheight_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_HEAVIEST_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_heaviest_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_WEIGHTTYPE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_weighttype_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_LENGTHOFHEAVIESTPI_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_lengthofheaviestpi_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_HEIGHTOFHEAVIESTPI_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_heightofheaviestpi_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_WIDTHOFHEAVIESTPIE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_widthofheaviestpie_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.


      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln  fieldname = 'ZZ1_MEASUREMENTSTYPE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_measurementstype_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_ASSEMBLY_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_assembly_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNSHAPE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signshape_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_UNITOFMEASURE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_unitofmeasure_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNHANGING_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signhanging_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_BOOTHWORK_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_boothwork_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_boothwork_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_otherboothwork_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_INBOUNDSHIPPINGINF_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_inboundshippinginf_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_EXHIBITINCLUDEFLO_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_exhibitincludeflo_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_EXHIBITGRAPHIC_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_exhibitgraphic_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNDESCRIPTION_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signdescription_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNDESCRIPTIONDE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signdescriptionde_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_GRAPHICHANDLING_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_graphichandling_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_SIGNHEIGHTTOP_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_signheighttop_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_GRAPHICSUPGRADE_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_graphicsupgrade_sdi }|.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_BACKGROUNDCOLOR_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        IF <lfs_vbap>-zz1_backgroundcolor_sdi <> 'Other'.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_backgroundcolor_sdi }|.
        ELSE.
          ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_otherbackgroundcol_sdi }|.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.

      lv_desc = VALUE #( lt_ques[ vbeln = <lfs_vbap>-vbeln fieldname = 'ZZ1_PLUMBINGBOOTHWORK_SDI' posnr = <lfs_vbap>-posnr ]-questions OPTIONAL ).
      IF lv_desc IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF ';;;Other'  IN <lfs_vbap>-zz1_plumbingboothwork_sdi WITH '; '.
        REPLACE ALL OCCURRENCES OF ';;;' IN <lfs_vbap>-zz1_plumbingboothwork_sdi WITH '; '.
        ls_ver-question = |{ lv_desc }| && | | && |:| && | | && |{ <lfs_vbap>-zz1_plumbingboothwork_sdi }|.
        IF <lfs_vbap>-zz1_otherplumbingbooth_sdi IS NOT INITIAL.
          IF <lfs_vbap>-zz1_plumbingboothwork_sdi IS NOT INITIAL.
            ls_ver-question = |{ ls_ver-question }| && | | && | | && |{ <lfs_vbap>-zz1_otherplumbingbooth_sdi }|.
          ENDIF.
        ENDIF.
        APPEND ls_ver TO lt_ver.
      ENDIF.
      APPEND LINES OF lt_ver TO lt_ver.
      rt_selcom = VALUE #( BASE rt_selcom ( vbeln = <lfs_vbap>-vbeln
                                            posnr = <lfs_vbap>-posnr
                                            t_ver = lt_ver
                           ) ).

      REFRESH lt_ver.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
