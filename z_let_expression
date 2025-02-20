*&---------------------------------------------------------------------*
*& Report  z_let_expression
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_let_expression.

class demo DEFINITION.
PUBLIC SECTION.
class-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.

  METHOD main.

  select single * from SFLIGHT
  into @data(ls_sflight)
  where carrid eq 'AA'.

* if sep is declared in let you can't declare it seperately
*  data sep type c.

  DATA(lv_net_price) = conv string( let disc = 5
                                         sep = '-'
                                         cal_disc = ls_sflight-price * disc
                                      in ls_sflight-price && sep && cal_disc ).

   DATA(lv_net_price1) = conv string( let disc = 5
                                         sep = '-'
                                         cal_disc = ls_sflight-price * disc
                                      in ls_sflight-price && sep && cal_disc ).

   cl_demo_output=>display(
     EXPORTING
       data =  lv_net_price   " Text or Data
*       name =
   ).
  ENDMETHOD.



ENDCLASS.

START-OF-SELECTION.

demo=>main( ).
