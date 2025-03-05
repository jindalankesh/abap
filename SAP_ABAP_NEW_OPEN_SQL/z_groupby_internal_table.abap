*&---------------------------------------------------------------------*
*& Report  z_groupby_internal_table
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_groupby_internal_table.

CLASS lcl_demo DEFINITION   .
  PUBLIC SECTION  .
    CLASS-METHODS    main.     "Representative Binding
    CLASS-METHODS    main_01.  "Group key Binding
    CLASS-METHODS    main_02.  "Group key Binding Without Members & Count/Index
    CLASS-METHODS    main_03.  "Group key Binding WITH OR Without Members & Count/Index with For Loop / REDUCE
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD main_03.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
             count  TYPE i,
             index  TYPE i,
           END OF helper_type,
           tt_ht TYPE TABLE OF helper_type WITH DEFAULT KEY.
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type.
    DATA: lt_sflight_grp TYPE STANDARD TABLE OF helper_type.


    DATA(lr_out) = cl_demo_output=>new( ).
    DATA lr_out1 TYPE REF TO if_demo_output.


    SELECT carrid,
             connid,
             fldate
        FROM sflight
        INTO TABLE @lt_sflight.



    lr_out = REDUCE #( INIT lr_dis = cl_demo_output=>new(  )
                     FOR GROUPS <lt_grp> OF <ls_flight> IN lt_sflight
                     GROUP BY ( carrid = <ls_flight>-carrid
                                connid = <ls_flight>-connid
                                count = GROUP SIZE
                                index = GROUP INDEX )

                      LET  lt_sflight1 = VALUE tt_ht(  FOR <ls_grp> IN GROUP <lt_grp> ( <ls_grp> )  )
                      IN NEXT lr_dis = lr_dis->write( lt_sflight1 )
                     ).

    lr_out1 = REDUCE #( INIT lr_dis = cl_demo_output=>new(  )
                     FOR GROUPS <lt_grp> OF <ls_flight> IN lt_sflight
                     GROUP BY ( carrid = <ls_flight>-carrid
                                connid = <ls_flight>-connid
                                count = GROUP SIZE
                                index = GROUP INDEX )
                     WITHOUT MEMBERS
*                      LET  lt_sflight1 = VALUE tt_ht(  FOR <ls_grp> IN GROUP <lt_grp> ( <ls_grp> )  ) IN
                       NEXT lr_dis = lr_dis->write( <lt_grp> )
                     ).

    lr_out1->display( ).
*    LOOP AT lt_sflight ASSIGNING FIELD-SYMBOL(<ls_flight>)
*               GROUP BY ( carrid = <ls_flight>-carrid
*                          connid = <ls_flight>-connid
*                          count = GROUP SIZE
*                          index = GROUP INDEX )
*                    WITHOUT MEMBERS
*                  ASSIGNING FIELD-SYMBOL(<lt_grp>)        .
*
*      DATA(lv_car_con) = | { <lt_grp>-carrid } : { <lt_grp>-connid } : { <lt_grp>-count }: { <lt_grp>-index } |    .
*      cl_demo_output=>write( lv_car_con ).
*
*    ENDLOOP.

    cl_demo_output=>display(  ).


  ENDMETHOD.
  METHOD main_02.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
             count  TYPE i,
             index  TYPE i,
           END OF helper_type.
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type.
    DATA: lt_sflight_grp TYPE STANDARD TABLE OF helper_type.


    DATA(lr_out) = cl_demo_output=>new( ).


    SELECT carrid,
             connid,
             fldate
        FROM sflight
        INTO TABLE @lt_sflight.
    LOOP AT lt_sflight ASSIGNING FIELD-SYMBOL(<ls_flight>)
               GROUP BY ( carrid = <ls_flight>-carrid
                          connid = <ls_flight>-connid
                          count = GROUP SIZE
                          index = GROUP INDEX )
                    WITHOUT MEMBERS
                  ASSIGNING FIELD-SYMBOL(<lt_grp>)        .

      DATA(lv_car_con) = | { <lt_grp>-carrid } : { <lt_grp>-connid } : { <lt_grp>-count }: { <lt_grp>-index } |    .
      cl_demo_output=>write( lv_car_con ).

    ENDLOOP.

    cl_demo_output=>display(  ).


  ENDMETHOD.
  METHOD main_01.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
           END OF helper_type.
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type.
    DATA: lt_sflight_grp TYPE STANDARD TABLE OF helper_type.


    DATA(lr_out) = cl_demo_output=>new( ).


    SELECT carrid,
             connid,
             fldate
        FROM sflight
        INTO TABLE @lt_sflight.
    LOOP AT lt_sflight ASSIGNING FIELD-SYMBOL(<ls_flight>)
               GROUP BY ( carrid = <ls_flight>-carrid
                          connid = <ls_flight>-connid  )

                  ASSIGNING FIELD-SYMBOL(<lt_grp>)        .

      DATA(lv_car_con) = | { <lt_grp>-carrid } : { <lt_grp>-connid } |    .
      cl_demo_output=>write( lv_car_con ).

      lt_sflight_grp = VALUE #( FOR <ls_grp> IN GROUP <lt_grp> ( <ls_grp> ) ).

      cl_demo_output=>write( lt_sflight_grp ).
*      cl_demo_output=>write( <ls_flight> ).
    ENDLOOP.


*     lr_out->write( lt_final ).
    cl_demo_output=>display(  ).

*    cl_demo_output=>display(
*      EXPORTING
*        data =  lt_final   " Text or Data

*    ).
  ENDMETHOD.


  METHOD main.
    TYPES: BEGIN OF helper_type,
             carrid TYPE sbook-carrid,
             connid TYPE sbook-connid,
             fldate TYPE sbook-fldate,
           END OF helper_type.
    DATA: lt_sflight TYPE STANDARD TABLE OF helper_type.

    DATA(lr_out) = cl_demo_output=>new( ).


    SELECT carrid,
             connid,
             fldate
        FROM sflight
        INTO TABLE @lt_sflight.
    LOOP AT lt_sflight ASSIGNING FIELD-SYMBOL(<ls_flight>)
               GROUP BY ( carrid = <ls_flight>-carrid
                          connid = <ls_flight>-connid  ).

      DATA(lv_car_con) = | { <ls_flight>-carrid } : { <ls_flight>-connid } |    .
      cl_demo_output=>write( lv_car_con ).

      LOOP AT GROUP  <ls_flight> ASSIGNING FIELD-SYMBOL(<ls_group>).
        cl_demo_output=>write( <ls_group> ).

      ENDLOOP.




      cl_demo_output=>write( <ls_flight> ).
    ENDLOOP.


*     lr_out->write( lt_final ).
    cl_demo_output=>display(  ).

*    cl_demo_output=>display(
*      EXPORTING
*        data =  lt_final   " Text or Data

*    ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION  .
  lcl_demo=>main_03( ).