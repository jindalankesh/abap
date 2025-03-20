*&---------------------------------------------------------------------*
*& Report  z_reduce_operator
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_reduce_operator.

CLASS lcl_demo DEFINITION   .
  PUBLIC SECTION  .
    CLASS-METHODS    main.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.

  METHOD main.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
             bookid TYPE string,
             customid TYPE sbook-customid,
             custtype TYPE sbook-custtype,
             smoker TYPE sbook-smoker,
             loccuram TYPE sbook-loccuram,
             loccurkey TYPE sbook-loccurkey,
           END OF helper_type.
    DATA: lt_final TYPE STANDARD TABLE OF helper_type.

    SELECT carrid,
           connid,
           fldate
      FROM sflight
      INTO TABLE @DATA(lt_sflight).
    IF sy-subrc = 0.
      SELECT
        carrid,
        connid,
        fldate,
        bookid,
        customid,
        custtype,
        smoker,
        loccuram,
        loccurkey
       FROM sbook
        FOR ALL ENTRIES IN @lt_sflight
        WHERE carrid = @lt_sflight-carrid
          AND connid = @lt_sflight-connid
          AND fldate = @lt_sflight-fldate
          INTO TABLE @data(lt_sbook).
          If sy-subrc  is INITIAL.
          loop at lt_sflight ASSIGNING FIELD-SYMBOL(<ls_sflight>).
           append initial line to lt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
*            <fs_final>-carrid = <ls_sflight>-carrid.
*            <fs_final>-connid = <ls_sflight>-connid.
*            <fs_final>-fldate = <ls_sflight>-fldate.
            <fs_final>-loccuram = REDUCE #( init lv_amt type S_L_CUR_PR
                                   FOR ls_book in lt_sbook where ( carrid = <ls_sflight>-carrid and
                                                                   connid = <ls_sflight>-connid and
                                                                   fldate =  <ls_sflight>-fldate  )
                                   NEXT lv_amt = lv_amt +  ls_book-loccuram ).

            <fs_final>-bookid = REDUCE #( init lv_bookid type string lv_sep = ' '
                                   FOR ls_book in lt_sbook where ( carrid = <ls_sflight>-carrid and
                                                                   connid = <ls_sflight>-connid and
                                                                   fldate =  <ls_sflight>-fldate  )
                                   NEXT lv_bookid = ls_book-bookid && lv_sep && lv_bookid
                                        lv_sep = ',' ).

            <fs_final>       = REDUCE helper_type( init ls_final type helper_type lv_sep1 type string lv_cnt type i
                                   FOR ls_book in lt_sbook where ( carrid = <ls_sflight>-carrid and
                                                                   connid = <ls_sflight>-connid and
                                                                   fldate =  <ls_sflight>-fldate  )
                                   NEXT ls_final-carrid = ls_book-carrid
                                        ls_final-connid = ls_book-connid
                                        ls_final-fldate = ls_book-fldate
                                        ls_final-loccuram = ls_final-loccuram + ls_book-loccuram
                                        ls_final-bookid = ls_book-bookid && lv_sep1 && ls_final-bookid
                                        lv_cnt = lv_cnt + 1
                                        lv_sep1 =  ',' && lv_cnt &&  ',' ).



          ENDLOOP.

          endif.
    ENDIF.



    cl_demo_output=>display(
      EXPORTING
        data =  lt_final   " Text or Data

    ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION  .
  lcl_demo=>main( ).