*&---------------------------------------------------------------------*
*& Report  z_for_new_abap
*&
*&---------------------------------------------------------------------*
*&  Test Program for FOR expression
*&  
*&---------------------------------------------------------------------*
REPORT z_for_new_abap.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.

  METHOD main.
    TYPES: BEGIN OF ty_sflight,
             carrid TYPE sflight-carrid,
             connid TYPE sflight-connid,
             fldate TYPE sflight-fldate,
             price  TYPE sflight-price,
           END OF ty_sflight.

           TYPES: BEGIN OF ty_sflight1,
             carrid TYPE sflight-carrid,
             connid TYPE sflight-connid,
             fldate TYPE sflight-fldate,
             price  TYPE sflight-price,
             date   type datum,
           END OF ty_sflight1.

    DATA: lt_sflight TYPE STANDARD TABLE OF ty_sflight,
          lt_sflight1 TYPE STANDARD TABLE OF ty_sflight1,
          lt_range_carrid type range of S_CARR_ID.

    SELECT *
    FROM sflight
    INTO TABLE @DATA(lt_sflight_all).

*     lt_sflight = VALUE #( for ls_fl in lt_sflight_all WHERE ( carrid = 'AA' ) (
*                                       carrid = ls_fl-carrid
*                                       connid = ls_fl-connid
*                                       fldate = ls_fl-fldate
*                                       price = ls_fl-price    ) ) .

     lt_sflight = VALUE #( for ls_fl in lt_sflight_all
*                                         WHERE ( carrid = 'AA'  )
                                         ( CORRESPONDING #( ls_fl )    ) ).

    data(lt_sflight_ucarried) = lt_sflight.

    delete ADJACENT DUPLICATES FROM lt_sflight_ucarried comparing carrid.
*  use range
    lt_range_carrid = VALUE #( for ls_fli in lt_sflight_ucarried (
                                          sign = 'I'
                                          option = 'EQ'
                                          low = ls_fli-carrid ) ).

* use of Value, BASE, Corrosponding , Value  in FOR Loop
    lt_sflight1 = VALUE #( for ls_flight1 in lt_sflight
                           let lv_date = VALUE ty_sflight1( date = sy-datum )
                           in ( CORReSPONDING #( base ( lv_date ) ls_flight1  ) ) ).

   cl_demo_output=>display(
     EXPORTING
       data =  lt_sflight1   " Text or Data
*       name =
   ).
  ENDMETHOD.



ENDCLASS.

START-OF-SELECTION.

  demo=>main( ).