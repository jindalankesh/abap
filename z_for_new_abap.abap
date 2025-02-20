*&---------------------------------------------------------------------*
*& Report  z_for_new_abap
*&
*&---------------------------------------------------------------------*
*&    Test Program for FOR expression 
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
    DATA: lt_sflight TYPE STANDARD TABLE OF ty_sflight.

    SELECT *
    FROM sflight
    INTO TABLE @DATA(lt_sflight_all).

*     lt_sflight = VALUE #( for ls_fl in lt_sflight_all WHERE ( carrid = 'AA' ) (
*                                       carrid = ls_fl-carrid
*                                       connid = ls_fl-connid
*                                       fldate = ls_fl-fldate
*                                       price = ls_fl-price    ) ) .

     lt_sflight = VALUE #( for ls_fl in lt_sflight_all WHERE ( carrid = 'AA' ) (
                                       CORRESPONDING #( ls_fl )    ) ).

   cl_demo_output=>display(
     EXPORTING
       data =  lt_sflight   " Text or Data
*       name =
   ).
  ENDMETHOD.



ENDCLASS.

START-OF-SELECTION.

  demo=>main( ).
