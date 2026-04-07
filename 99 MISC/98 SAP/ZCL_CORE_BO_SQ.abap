class ZCL_CORE_BO_SQ definition
  public
  inheriting from ZCL_CORE_BO_BASE
  final
  create public .

public section.

  aliases ACCEPT
    for ZIF_CORE_BO~ACCEPT .
  aliases CREATE
    for ZIF_CORE_BO~CREATE .
  aliases DELETE
    for ZIF_CORE_BO~DELETE .
  aliases GET
    for ZIF_CORE_BO~GET .
  aliases UPDATE
    for ZIF_CORE_BO~UPDATE .

  methods CONSTRUCTOR
    importing
      !I_KEY type ZIF_CORE_BO~TY_KEYS optional
      !I_DATA type ZCORE_QUOTE_AIF_SAP optional
    preferred parameter I_KEY .
  methods CHECK_MULTI_CA_FOR_BP
    importing
      !RAW_STRUCT type ZCORE_QUOTE_AIF_RAW_MAIN
      !RAW_LINE type ZCORE_CTR_LINE_AIF_RAW
      !SMAP type /AIF/T_SMAP optional
      !INTREC type /AIF/T_INTREC optional
      !SENDING_SYSTEM type /AIF/AIF_BUSINESS_SYSTEM_KEY optional
    changing
      !OUT_STRUCT type ZCORE_QUOTE_AIF_SAP
      !DEST_LINE type ZCORE_SOLUTION_QU_LINE_AIF_SAP
      !DEST_TABLE type ZCORE_SOLUTI_QU_LINE_AIF_T_SAP
      !APPEND_FLAG type C optional
      !RETURN_TAB type WSTI_TT_BAPIRET2 optional .
  methods CHECK_OVERLAPPING_CA
    importing
      !INPUT type ZCORE_QUOTE_AIF_SAP
      !I_HDR_DEEP type CL_CRMS4_API_ODATA_SRVP_PROXY=>TS_DEEP_BUSSOLNQTAN
    changing
      !RETURN_SUBRC type SY-SUBRC
      !OVERLAP_INFO type string optional .

  methods ZIF_CORE_BO~ACCEPT
    redefinition .
  methods ZIF_CORE_BO~CREATE
    redefinition .
  methods ZIF_CORE_BO~GET
    redefinition .
  methods ZIF_CORE_BO~UPDATE
    redefinition .
protected section.
private section.

  types:
    BEGIN OF ts_deep_subscriptionitem.
      INCLUDE TYPE cl_api_bus_solution_qu_mpc=>ts_a_bussolnqtansubscrpnitemty.
  TYPES: END OF ts_deep_subscriptionitem .
  types:
    BEGIN OF ty_sq_data,

      header         TYPE cl_api_bus_solution_qu_mpc=>ts_a_businesssolutionquotation,
      subsc_item     TYPE TABLE OF ts_deep_subscriptionitem WITH EMPTY KEY,
      price_elem     TYPE STANDARD TABLE OF cl_api_bus_solution_qu_mpc=>ts_a_bussolnqtanitmpriceelemen WITH EMPTY KEY,
      config_item    TYPE STANDARD TABLE OF cl_api_bus_solution_qu_mpc=>ts_a_bussolnqtanitmconfigntype WITH EMPTY KEY,
      fop_subsc_item TYPE STANDARD TABLE OF cl_api_bus_solution_qu_mpc=>ts_a_busoqtanitmfupsubscrpncon WITH EMPTY KEY,

    END OF ty_sq_data .

  data QUOTE_DATA type CL_CRMS4_API_ODATA_SRVP_PROXY=>TS_DEEP_BUSSOLNQTAN .
  data READ_QUOTE_DATA type TY_SQ_DATA .
ENDCLASS.



CLASS ZCL_CORE_BO_SQ IMPLEMENTATION.


  METHOD zif_core_bo~accept.


    DATA: lv_action_name          TYPE string VALUE 'Accept',
          it_parameter            TYPE /iwbep/t_mgw_name_value_pair,
          io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_func_import,
          lr_data_provider        TYPE REF TO lcl_entrprv_sol_qu.

    IF zif_core_bo~keys-businesssolutionquotation IS INITIAL.

      DATA(lv_sol_quote) = zcl_core_aif_solqu_helper=>get_sol_quote_by_concur_id( CONV #( input_data-quote-message_context-crm_account_id ) ).

      IF lv_sol_quote IS INITIAL.

        lv_sol_quote = input_data-quote-message_context-solution_quote_number.

      ENDIF.

      zif_core_bo~keys-businesssolutionquotation = lv_sol_quote.

    ENDIF.

    it_parameter = VALUE #( (
        name = 'BusinessSolutionQuotation'
        value = zif_core_bo~keys-businesssolutionquotation )
        ).

    TRY.

        DATA(lo_context) = NEW /iwbep/cl_mgw_context( ).

        DATA(lo_msg_container) = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

        lo_context->/iwbep/if_mgw_context~set_parameter(
            iv_name  = /iwbep/if_mgw_context=>gc_param_msg_container
            iv_value = lo_msg_container
          ).


        DATA(lo_dp) = NEW cl_api_bus_solution_qu_dpc_ext( ).

        lo_dp->/iwbep/if_mgw_core_srv_runtime~init(
            iv_namespace = '/SAP/'
            iv_version = 0001
            iv_service_document_name = 'API_BUS_SOLUTION_QUOTATION_SRV'
            io_context = lo_context
            ).


        lo_dp->/iwbep/if_mgw_appl_srv_runtime~execute_action(
            iv_action_name = lv_action_name
            it_parameter = it_parameter
         ).

      CATCH /iwbep/cx_mgw_busi_exception INTO DATA(lx_busi_exc).

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lx_tech_exc).

    ENDTRY.

    messages->add_table( CORRESPONDING bapiret2_t( lo_dp->/iwbep/if_mgw_conv_srv_runtime~get_message_container( )->get_messages( ) ) ).

    LOOP AT messages->get( ) ASSIGNING FIELD-SYMBOL(<fs_msg>).
      IF <fs_msg>-type CA 'EAX'.
        DATA(lv_error) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_error = abap_false.

      messages->add_single( VALUE #(
       id = 'ZCORE_QUOTE_AIF'
       number = 4
       type = 'S'
       message_v1 = zif_core_bo~keys-businesssolutionquotation
       message_v2 = input_data-quote-message_context-concur_id  ) ).

    ELSE.

      RAISE EXCEPTION TYPE zcx_core_bo
        EXPORTING
          messages = messages.

    ENDIF.

  ENDMETHOD.


  METHOD zif_core_bo~create.

    DATA lv_i TYPE i.

    DATA: ls_hdr_deep_create  TYPE cl_crms4_api_odata_srvp_proxy=>ts_deep_bussolnqtan,
          lr_data_provider    TYPE REF TO lcl_entrprv_sol_qu,
          io_expand           TYPE REF TO /iwbep/if_mgw_odata_expand,
          lt_subscr_itm_phase TYPE zcore_soluti_qu_line_aif_t_sap,
          lt_sol_qu_items     TYPE zcore_soluti_qu_line_aif_t_sap,
          er_deep_entity      TYPE REF TO data.
    DATA: l_subrc TYPE sy-subrc.  "COREINT-743+

    FIELD-SYMBOLS: <fs_er_deep> TYPE cl_crms4_api_odata_srvp_proxy=>ts_deep_bussolnqtan.

    CREATE DATA er_deep_entity TYPE cl_crms4_api_odata_srvp_proxy=>ts_deep_bussolnqtan.

* Conversion Exit for CA
    LOOP AT input_data-quote-solution_quote-lines ASSIGNING FIELD-SYMBOL(<fs_quote_line>).
      IF <fs_quote_line>-contractaccount IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_quote_line>-contractaccount
          IMPORTING
            output = <fs_quote_line>-contractaccount.
      ENDIF.
    ENDLOOP.
    IF input_data-quote-solution_quote-header-soldtoparty IS INITIAL.

      DATA(lv_bp) = input_data-quote-bp-header_data-partner_number.

      input_data-quote-solution_quote-header-soldtoparty = lv_bp.

    ENDIF.

    ls_hdr_deep_create = CORRESPONDING #( input_data-quote-solution_quote-header ).

    ls_hdr_deep_create-to_subscriptionitem = CORRESPONDING #( input_data-quote-solution_quote-lines MAPPING to_configurations = to_configurations ).

* Begin of COREINT-743+
* Check if there are overlapping active contracts

    DATA(lv_overlap_info) = ''.

    CALL METHOD me->check_overlapping_ca
      EXPORTING
        input        = input_data
        i_hdr_deep   = ls_hdr_deep_create
      CHANGING
        return_subrc = l_subrc
        overlap_info = lv_overlap_info.
    IF l_subrc IS NOT INITIAL.
      messages->add_single( VALUE #(
       id         = 'ZCORE_MSG_BO'
       number     = 013
       type       = 'E'
       message_v1 = input_data-quote-solution_quote-header-soldtoparty
       message_v2 = lv_overlap_info ) ).

      RAISE EXCEPTION TYPE zcx_core_bo
        EXPORTING
          messages = messages.

    ENDIF.
* End of COREINT-743+
    lr_data_provider = NEW #( ).

    lr_data_provider->set_data( ls_hdr_deep_create ).

    TRY.

        DATA(lo_msg_container) = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

        DATA(lo_context) = NEW /iwbep/cl_mgw_context( ).

        lo_context->/iwbep/if_mgw_context~set_parameter(
          iv_name  = /iwbep/if_mgw_context=>gc_param_msg_container
          iv_value = lo_msg_container
        ).

        DATA(lo_dp) = NEW cl_api_bus_solution_qu_dpc_ext( ).

        lo_dp->/iwbep/if_mgw_core_srv_runtime~init(
          iv_namespace             = '/SAP/'
          iv_version               = 0001
          iv_service_document_name = 'API_BUS_SOLUTION_QUOTATION_SRV'
          io_context               = lo_context
        ).


*do .enddo.
        lo_dp->/iwbep/if_mgw_appl_srv_runtime~create_deep_entity(
          EXPORTING
            iv_entity_name     = 'A_BusinessSolutionQuotationType'
            iv_entity_set_name = 'A_BusinessSolutionQuotation'
            io_data_provider   = lr_data_provider
            io_expand          = io_expand
          IMPORTING
            er_deep_entity     = er_deep_entity
        ).


      CATCH /iwbep/cx_mgw_busi_exception INTO DATA(lx_busi_exc).

        " n/a

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lx_tech_exc).

        " n/a

    ENDTRY.


    DATA(lx_msg) =  lo_dp->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    DATA(lt_sq__api_msgs) = lx_msg->get_messages( ).

*    check_if_msg_relevant( CHANGING ct_msgs = lx_msg->get_messages( ) ).
*    later to be changed to above method
*    to enable SQ creation with empty CA
    LOOP AT lt_sq__api_msgs ASSIGNING FIELD-SYMBOL(<fs_msg_rel>).
      IF <fs_msg_rel>-type = 'E' AND <fs_msg_rel>-id = 'CRM_ISX_BTX_PAYMENT' AND <fs_msg_rel>-number = '217' .
        <fs_msg_rel>-type = 'W'.
      ENDIF.
    ENDLOOP.

    messages->add_table( CORRESPONDING #( lt_sq__api_msgs ) ).

    ASSIGN er_deep_entity->* TO <fs_er_deep>.

    IF <fs_er_deep> IS ASSIGNED.

      LOOP AT lt_sq__api_msgs ASSIGNING FIELD-SYMBOL(<fs_msg>).
        IF <fs_msg>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_error  = abap_true.

        messages->add_single( VALUE #(
         id         = 'ZCORE_MSG_BO'
         number     = 5
         type       = 'E'
         message_v1 = <fs_er_deep>-businesssolutionquotation
         message_v2 = input_data-quote-message_context-opportunity_id ) ).

        RAISE EXCEPTION TYPE zcx_core_bo
          EXPORTING
            messages = messages.

      ELSE.

        IF <fs_er_deep>-businesssolutionquotation IS INITIAL.

          messages->add_single( VALUE #(
                  id         = 'ZCORE_MSG_BO'
                  number     = 6
                  type       = 'E'
                  message_v1 = input_data-quote-message_context-opportunity_id ) ).

          RAISE EXCEPTION TYPE zcx_core_bo
            EXPORTING
              messages = messages.

        ENDIF.

        messages->add_single( VALUE #(
              id         = 'ZCORE_MSG_BO'
              number     = 3
              type       = 'S'
              message_v1 = <fs_er_deep>-businesssolutionquotation
              message_v2 = input_data-quote-message_context-opportunity_id ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_core_bo~get.

*    IF read_quote_data IS INITIAL.

      CLEAR zcl_som_sol_quote_util=>gs_sol_quote.

      DATA(lo_som_api) = NEW zcl_som_sol_quote_util( VALUE #( ) ).

      lo_som_api->read_sol_quote_det(
         EXPORTING
           im_solquote         = i_key-businesssolutionquotation
         IMPORTING
           es_solquote_head    = read_quote_data-header
           et_subscrp_item     = read_quote_data-subsc_item
           et_price_elem       = read_quote_data-price_elem
           et_configurations   = read_quote_data-config_item
           et_subscrp_contract = read_quote_data-fop_subsc_item
           ).

      lo_som_api->populate_sol_quote_det(
        im_sq_header     = read_quote_data-header
        imt_subscrp_item = read_quote_data-subsc_item
        imt_price_elem   = read_quote_data-price_elem
        imt_config       = read_quote_data-config_item
        imt_subcontract  = read_quote_data-fop_subsc_item
*        im_effdate       =                  " Effective From Date
      ).
*    ENDIF.

    CREATE DATA rv_data LIKE zcl_som_sol_quote_util=>gs_sol_quote.

    ASSIGN rv_data->* TO FIELD-SYMBOL(<fs_read_data>).

    IF <fs_read_data> IS ASSIGNED.

      <fs_read_data> = zcl_som_sol_quote_util=>gs_sol_quote.

    ENDIF.  .

  ENDMETHOD.


  METHOD zif_core_bo~update.

    DATA:
      ls_data      TYPE cl_crms4_api_odata_srvp_proxy=>ts_deep_bussolnqtan,
      ls_new_quote TYPE cl_crms4_api_odata_srvp_proxy=>ts_deep_bussolnqtan.

   " do. enddo.
    super->zif_core_bo~update( ).

    messages->add_single( VALUE #(  id = 'ZCORE_MSG_BO' number = 10 type = 'I' )  ).

    MESSAGE i010(zcore_msg_bo) INTO DATA(lv_msg).

    ls_data = CORRESPONDING #( input_data-quote-solution_quote-header_modified[ 1 ] ).

    clear ls_data-businesssolutionquotation.

    ls_data-to_subscriptionitem = CORRESPONDING #( input_data-quote-solution_quote-lines_modified EXCEPT to_configurations businesssolutionquotation ).

    IF ls_data-to_subscriptionitem IS INITIAL.

      messages->add_single( VALUE #( id = 'ZCORE_MSG_BO' number = 11 type = 'W' )  ).

      MESSAGE w011(zcore_msg_bo) INTO lv_msg.

    ENDIF.

    DATA(lo_som_api) = NEW zcl_som_sol_quote_util( ls_data ).

    lo_som_api->create_sol_quotation(
    IMPORTING
      ex_output = ls_new_quote
      et_return = DATA(lt_errors)
    ).

    IF ls_new_quote IS INITIAL.

      messages->add_single( VALUE #( id = 'ZCORE_MSG_BO' number = 7 type = 'E' )  ).

      MESSAGE e007(zcore_msg_bo) INTO lv_msg.

      messages->add_table( it_msg = lt_errors ).

      RAISE EXCEPTION TYPE zcx_core_bo
        EXPORTING
          messages = messages.

    ELSE.

      messages->add_single( VALUE #( id = 'ZCORE_MSG_BO' number = 12 type = 'I' message_v1 = ls_new_quote-businesssolutionquotation ) ).

      MESSAGE i012(zcore_msg_bo) INTO lv_msg.

      lo_som_api->trigger_change_quote(
      EXPORTING
        im_new_solquote = ls_new_quote-businesssolutionquotation
        im_action_name  = 'ZSOM_PROCESS_CHANGE_QUOTE'
      IMPORTING
        ex_return       = DATA(lv_return) ).

      IF lv_return IS NOT INITIAL.

        messages->add_single( VALUE #( id = 'ZCORE_MSG_BO' number = 8 type = 'E' message_v1 = ls_new_quote-businesssolutionquotation ) ).

        MESSAGE e008(zcore_msg_bo) INTO lv_msg.

        RAISE EXCEPTION TYPE zcx_core_bo
          EXPORTING
            messages = messages.

      ELSE.

        messages->add_single( VALUE #( id = 'ZCORE_MSG_BO' number = 9 type = 'S' message_v1 = ls_new_quote-businesssolutionquotation )  ).

        MESSAGE s009(zcore_msg_bo) INTO lv_msg.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( i_data = i_data ).

*    CHECK i_key IS NOT INITIAL. "Check RG issue
    zif_core_bo~keys = i_key.


  ENDMETHOD.


    METHOD check_multi_ca_for_bp.
      FIELD-SYMBOLS: <fs_out_struc> TYPE zcore_quote_aif_sap.

      ASSIGN out_struct TO <fs_out_struc>.

      IF <fs_out_struc> IS NOT ASSIGNED.

        RETURN.

      ENDIF.

* Check if there are more than one CA to a BP
      READ TABLE out_struct-quote-contract-lines INTO DATA(ls_ca_lines) INDEX 1.
      IF sy-subrc IS INITIAL AND ls_ca_lines-comp_code IS NOT INITIAL.
*        SELECT COUNT(*)                   COREINT-869 --
*               INTO @DATA(lv_count)       COREINT-869 --
        SELECT vkont                       "COREINT-869 ++
               FROM fkkvkp
               INTO TABLE @DATA(lt_vkont) "COREINT-869 ++
              WHERE gpart = @out_struct-quote-bp-header_data-partner_number
                AND opbuk = @ls_ca_lines-comp_code.
      ELSE.
*        SELECT COUNT(*)                   COREINT-869 --
*               INTO @lv_count             COREINT-869 --
        SELECT vkont                       "COREINT-869 ++
                INTO TABLE @lt_vkont       "COREINT-869 ++
               FROM fkkvkp
               WHERE gpart = @out_struct-quote-bp-header_data-partner_number.
      ENDIF.
      DATA(lv_count) = lines( lt_vkont ).      "COREINT-869 ++
*      IF sy-subrc = 0 AND lv_count > 1.        COREINT-869 --
      IF lv_count > 1.                         "COREINT-869 ++
* Check if the SRC CONTRACT=>LINES=>CA are updated
        READ TABLE raw_struct-contract-lines TRANSPORTING NO FIELDS WITH KEY contract_account = ''.
        IF sy-subrc IS INITIAL.
* To capture the error message once for all the line items.
          DESCRIBE TABLE dest_table LINES DATA(lv_dest_table).
          IF lv_dest_table IS INITIAL.
            DATA lv_symsgv  TYPE symsgv.
            lv_symsgv = out_struct-quote-bp-header_data-partner_number.
            APPEND LINES OF zcl_dci2_aif_utilities=>add_message_with_na(
                        iv_type       = zif_core_aif_constants=>msg_type-error
                        iv_id         = zcl_core_aif_qu_hdr_after_map=>msg_class
                        iv_number     = '029'
                        iv_message_v1 = lv_symsgv
                      ) TO return_tab.
          ENDIF.
          out_struct-quote-solution_quote-header-z4e_bill_excepn_flag_i4p = '29'.
          RETURN.
        ELSE.
* Check the updated CA is correct for the given line item.
          DATA: lv_gpart TYPE gpart_kk,
                lv_vkont TYPE vkont_kk.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = out_struct-quote-bp-header_data-partner_number
            IMPORTING
              output = lv_gpart.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = raw_line-contract_account
            IMPORTING
              output = lv_vkont.

          SELECT COUNT(*)
                 FROM fkkvkp
                 INTO @lv_count
            WHERE ( gpart EQ @lv_gpart ) AND ( vkont EQ @lv_vkont ).
          IF sy-subrc IS NOT INITIAL.
            DATA lv_symsgv1  TYPE symsgv.
            lv_symsgv1 = raw_line-contract_account.
            lv_symsgv = out_struct-quote-bp-header_data-partner_number.
            APPEND LINES OF zcl_dci2_aif_utilities=>add_message_with_na(
                        iv_type       = zif_core_aif_constants=>msg_type-error
                        iv_id         = zcl_core_aif_qu_hdr_after_map=>msg_class
                        iv_number     = '042'
                        iv_message_v1 = lv_symsgv1
                        iv_message_v2 = lv_symsgv
                      ) TO return_tab.
            out_struct-quote-solution_quote-header-z4e_bill_excepn_flag_i4p = '29'.
*          RETURN.
          ELSE.
*        Remove the Billing Exception flag bcoz of this error.
            CLEAR out_struct-quote-solution_quote-header-z4e_bill_excepn_flag_i4p.
          ENDIF.
        ENDIF.
*  BOC COREINT-869
      ELSE.
        IF  dest_line-contractaccount IS INITIAL.
          dest_line-contractaccount = VALUE #( lt_vkont[ 1 ]-vkont OPTIONAL ).
        ENDIF.
      ENDIF.
* EOC COREINT-869
    ENDMETHOD.


  METHOD check_overlapping_ca.

*Action before Solution Quote Creation for new subscription to restrict Quote Creation.
*1.  Get latest PC from table crms4d_serv_i by filtering BP/CA/Product/ objtype_h = 'BUS2000266'.
*2.  If PC found, then check if it is active, crms4d_serv_i-stat_activation having value 'E' and
*input contract start date/time slice is between contract period. If found, then restrict the creation.

    TYPES: BEGIN OF ty_bus2000266,
             z4e_salesforceopptid_srh TYPE crms4d_serv_h-z4e_salesforceopptid_srh,
             object_id                TYPE crms4d_serv_h-object_id,
             contract_account         TYPE crms4d_serv_h-contract_account,
             z4e_crm_accnt_id_srh     TYPE crms4d_serv_h-z4e_crm_accnt_id_srh,
             contstart                TYPE crms4d_serv_i-contstart,
             contend                  TYPE crms4d_serv_i-contend,
             stat_lifecycle           TYPE crms4d_serv_i-stat_lifecycle,
             stat_activation          TYPE crms4d_serv_i-stat_activation,
             ci_contract_id           TYPE crms4d_serv_i-ci_contract_id,
             objtype_h                TYPE crms4d_serv_i-objtype_h,
             product_id               TYPE crms4d_serv_i-product_id,
             itmno                    TYPE crms4d_serv_i-number_int,  "COREINT-895 ++
           END OF ty_bus2000266,

             tt_bus2000266 TYPE STANDARD TABLE OF ty_bus2000266 WITH EMPTY KEY.

    DATA: lt_bus2000266     TYPE tt_bus2000266,
          lt_bus2000266_tmp TYPE tt_bus2000266.
    DATA: lv_now_ts         TYPE crms4_cont_start_ts_date.
    DATA: lv_matnr          TYPE matnr.

    TYPES: BEGIN OF ty_request_item,
             product    TYPE matnr,
             start_date TYPE crms4_cont_start_ts_date,
           END OF ty_request_item.

    DATA lt_request_items TYPE STANDARD TABLE OF ty_request_item WITH EMPTY KEY.

    LOOP AT i_hdr_deep-to_subscriptionitem INTO DATA(ls_subscription).
      APPEND VALUE #( product = ls_subscription-product
                      start_date = CONV crms4_cont_start_ts_date( ls_subscription-subscrpncontritmstartdatetime ) )
        TO lt_request_items.
    ENDLOOP.

    SELECT crms4d_serv_h~z4e_salesforceopptid_srh, crms4d_serv_h~object_id, crms4d_serv_h~contract_account,
      crms4d_serv_h~z4e_crm_accnt_id_srh, crms4d_serv_i~contstart, crms4d_serv_i~contend, crms4d_serv_i~stat_lifecycle, crms4d_serv_i~stat_activation,
      crms4d_serv_i~ci_contract_id, crms4d_serv_i~objtype_h, crms4d_serv_i~product_id,
      crms4d_serv_i~number_int AS itmno "COREINT-895 ++
    FROM ( crms4d_serv_h
           INNER JOIN crms4d_serv_i
           ON  crms4d_serv_i~contract_account = crms4d_serv_h~contract_account
           AND crms4d_serv_i~object_id = crms4d_serv_h~object_id
           AND crms4d_serv_i~objtype_h = crms4d_serv_h~objtype_h
           AND crms4d_serv_i~sold_to_party = crms4d_serv_h~sold_to_party )
*         WHERE crms4d_serv_h~z4e_salesforceopptid_srh = @input-quote-message_context-crm_account_id   "COREINT-895 --
           WHERE crms4d_serv_h~z4e_crm_accnt_id_srh = @input-quote-message_context-crm_account_id  "COREINT-895 ++
            AND crms4d_serv_h~objtype_h = 'BUS2000266'
            AND crms4d_serv_i~stat_activation = 'E'   "COREINT-895 ++
     INTO CORRESPONDING FIELDS OF TABLE @lt_bus2000266 .
    IF sy-subrc IS INITIAL.
      lt_bus2000266_tmp = lt_bus2000266.

      LOOP AT lt_bus2000266 ASSIGNING FIELD-SYMBOL(<fs_bus2000266>).
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input        = <fs_bus2000266>-product_id
          IMPORTING
            output       = lv_matnr
          EXCEPTIONS
            length_error = 1 ##ARG_OK
            OTHERS       = 2.
        IF sy-subrc <> 0.
          ASSERT 1 = 2.
        ENDIF.
        READ TABLE i_hdr_deep-to_subscriptionitem TRANSPORTING NO FIELDS WITH KEY product = lv_matnr.
        IF sy-subrc IS NOT INITIAL.
          CLEAR <fs_bus2000266>-object_id.
        ENDIF.
      ENDLOOP.
      DELETE lt_bus2000266 WHERE object_id IS INITIAL.
      IF lt_bus2000266 IS NOT INITIAL.
        DATA(lt_bus2000266_selected) = VALUE tt_bus2000266( ).
        DATA(lv_prev_product_id)    = ''.
        DATA(lv_prev_contract_id)   = ''.

        " Pick highest ITMNO for each product + CI_CONTRACT_ID
        SORT lt_bus2000266 BY product_id ASCENDING ci_contract_id ASCENDING itmno DESCENDING.
        LOOP AT lt_bus2000266 INTO DATA(ls_bus2000266).
          IF ls_bus2000266-product_id <> lv_prev_product_id
             OR ls_bus2000266-ci_contract_id <> lv_prev_contract_id.
            APPEND ls_bus2000266 TO lt_bus2000266_selected.
            lv_prev_product_id = ls_bus2000266-product_id.
            lv_prev_contract_id = ls_bus2000266-ci_contract_id.
          ENDIF.
        ENDLOOP.

        " Evaluate all selected latest lines; overlap on any CI_CONTRACT_ID for product blocks quote
        LOOP AT lt_bus2000266_selected INTO DATA(ls_selected).
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = ls_selected-product_id
            IMPORTING
              output = lv_matnr
            EXCEPTIONS
              length_error = 1 ##ARG_OK
              OTHERS       = 2.
          IF sy-subrc <> 0.
            ASSERT 1 = 2.
          ENDIF.

          LOOP AT lt_request_items INTO DATA(ls_req_item) WHERE product = lv_matnr.
            IF ls_req_item-start_date BETWEEN ls_selected-contstart AND ls_selected-contend.
              return_subrc = '8'.
              OVERLAP_INFO = |MAT:{ lv_matnr } CA:{ ls_selected-contract_account } CI:{ ls_selected-ci_contract_id } IN:{ ls_req_item-start_date } EXISTING:{ ls_selected-contstart }..{ ls_selected-contend }|.
              RETURN.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        CLEAR return_subrc.
      ENDIF.
    ELSE.
      CLEAR return_subrc.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
