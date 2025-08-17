CLASS zcl_hello_world_bam DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HELLO_WORLD_BAM IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    out->write( |Hello world!!!| ).
  ENDMETHOD.
ENDCLASS.
