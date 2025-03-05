*&---------------------------------------------------------------------*
*& Report  z_filter_operator
*&
*&---------------------------------------------------------------------*
*&  Filter in ABAP
*&
*&---------------------------------------------------------------------*
REPORT z_filter_operator.
CLASS lcl_demo DEFINITION   .
  PUBLIC SECTION  .
    CLASS-METHODS    main.     "Filter
    CLASS-METHODS    main_01.     "Filter

ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
 METHOD main_01.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
             CURRENCY type sflight-CURRENCY,
           END OF helper_type,
           tt_sflight type TABLE OF helper_type with DEFAULT KEY ,
           BEGIN OF helper_type_1,
                    currkey TYPE scurx-currkey,
                  END OF helper_type_1.
    DATA: lt_curr TYPE sorted TABLE OF helper_type_1 with UNIQUE KEY  currkey.
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type with NON-UNIQUE SORTED KEY abc COMPONENTS carrid ,
          lt_sflight_sort type sorted TABLE OF helper_type with NON-UNIQUE key carrid.

    DATA(lr_out) = cl_demo_output=>new( ).


    SELECT carrid,
             connid,
             fldate,
             currency
        FROM sflight
        INTO TABLE @lt_sflight_sort.

        select CURRKEY from
        SCURX into TABLE @lt_curr
        where currkey eq 'USD'.

        cl_demo_output=>write( lt_sflight_sort ).

        data(lt_sflight_01) = FILTER tt_sflight( lt_sflight_sort in  lt_curr where currency = currkey ).



    cl_demo_output=>display( lt_sflight_01 ).


  ENDMETHOD.
  METHOD main.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
           END OF helper_type,
           tt_sflight type TABLE OF helper_type with DEFAULT KEY .
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type with NON-UNIQUE SORTED KEY abc COMPONENTS carrid ,
          lt_sflight_sort type sorted TABLE OF helper_type with NON-UNIQUE key carrid.

    DATA(lr_out) = cl_demo_output=>new( ).


    SELECT carrid,
             connid,
             fldate
        FROM sflight
        INTO TABLE @lt_sflight_sort.

        cl_demo_output=>write( lt_sflight_sort ).

        data(lt_sflight_01) = FILTER tt_sflight( lt_sflight using key abc where CARRID = 'AA ' ).
        data(lt_sflight_02) = FILTER tt_sflight( lt_sflight_sort EXCEPT  where  CARRID = 'AA ' ).


    cl_demo_output=>display( lt_sflight_02 ).


  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION  .
  lcl_demo=>main_01( ).