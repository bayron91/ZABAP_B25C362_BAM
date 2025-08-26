CLASS zcl_work_order_crud_test_bam DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CONSTANTS lcte_create TYPE c LENGTH 1 VALUE 'C'.
    CONSTANTS lcte_read   TYPE c LENGTH 1 VALUE 'R'.
    CONSTANTS lcte_update TYPE c LENGTH 1 VALUE 'U'.
    CONSTANTS lcte_delete TYPE c LENGTH 1 VALUE 'D'.

    DATA lv_msg        TYPE string.
    DATA lo_handler    TYPE REF TO zcl_work_order_crud_handle_bam.
    DATA lt_status     TYPE STANDARD TABLE OF ztstatus_bam.
    DATA lt_prioritys  TYPE STANDARD TABLE OF ztpriority_bam.
    DATA lt_customers  TYPE STANDARD TABLE OF ztcustomer_bam.
    DATA lt_technician TYPE STANDARD TABLE OF zttechnician_bam.

    METHODS:
      check_authority IMPORTING iv_operation    TYPE abap_bool
                      RETURNING VALUE(rv_valid) TYPE abap_bool,
      create_status,
      create_priority,
      create_customers,
      create_technician,
      test_create_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.


CLASS zcl_work_order_crud_test_bam IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "create status.
    create_status( ).

    "create prioritys
    create_priority( ).

    "create customers
    create_customers( ).

    "create technicians
    create_technician( ).

    IF lo_handler IS NOT BOUND.
      lo_handler = NEW #( ).
    ENDIF.

    "Define operation test
    DATA(lv_operation) = lcte_create.  "create work order
*    DATA(lv_operation) = lcte_read.   "read   work order
*    DATA(lv_operation) = lcte_update. "update work order
*    DATA(lv_operation) = lcte_delete. "delete work order

    DATA(lv_valid) = check_authority( lv_operation ).

    IF lv_valid EQ abap_true.
      CASE lv_operation.
        WHEN lcte_create.
          test_create_work_order( out ).
        WHEN lcte_read.
          test_read_work_order( out ).
        WHEN lcte_update.
          test_update_work_order( out ).
        WHEN lcte_delete.
          test_delete_work_order( out ).
      ENDCASE.
    ELSE.
      out->write( |El usuario { cl_abap_context_info=>get_user_alias( ) } no tiene permisos| &&
                  |, para ejecutar la operación { lv_operation }| ).
    ENDIF.

  ENDMETHOD.

  METHOD create_priority.

    "Define status of work orders
    DELETE FROM ztstatus_bam.
    COMMIT WORK AND WAIT.

    lt_status = VALUE #( ( status_code = 'PE' status_description = 'Pending' )
                         ( status_code = 'CO' status_description = 'Completed' ) ).

    MODIFY ztstatus_bam FROM TABLE @lt_status.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD create_status.

    "Define prioritys of work orders
    DELETE FROM ztpriority_bam.
    COMMIT WORK AND WAIT.

    lt_prioritys = VALUE #( ( priority_code = 'A' priority_description = 'High' )
                            ( priority_code = 'B' priority_description = 'Low' ) ).

    MODIFY ztpriority_bam FROM TABLE @lt_prioritys.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD create_customers.

    DELETE FROM ztcustomer_bam.

    lt_customers = VALUE #( FOR i = 1 UNTIL i > 10
                            ( customer_id = i
                              name = |Customer { i }|
                              address = |cll { i } # { i + 10 }|
                              phone = |910600484{ i }| ) ).

    MODIFY ztcustomer_bam FROM TABLE @lt_customers.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD create_technician.

    DELETE FROM zttechnician_bam.

    lt_technician = VALUE #( FOR i = 1 UNTIL i > 5
                            ( technician_id = |TEC{ i }|
                              name = |Technician { i }|
                              specialty = |técnico instalador { i }| ) ).

    MODIFY zttechnician_bam FROM TABLE @lt_technician.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD test_create_work_order.

    DATA(ls_order) = VALUE ztworkorder_bam( work_order_id = 0000000004
                                            customer_id = 00000008
                                            technician_id = 'TEC2'
                                            creation_date = cl_abap_context_info=>get_system_date( )
                                            status = 'PE'
                                            priority = 'A'
                                            description = 'Work order for new installation' ).

    DATA(lv_valid) = lo_handler->create_work_order( ls_order ).

    CASE lv_valid.
      WHEN '1'.
        lv_msg = |The customer { ls_order-customer_id } doesnt'n exist|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
      WHEN '2'.
        lv_msg = |The technician { ls_order-technician_id } doesnt'n exist|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
      WHEN '3'.
        lv_msg = |The priority { ls_order-priority } doesnt'n exist|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
      WHEN '4'.
        lv_msg = |The status { ls_order-status } doesnt'n exist|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
      WHEN '5'.
        lv_msg = |The work order { ls_order-work_order_id } has been created|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
      WHEN '6'.
        lv_msg = |Error with Lock object EZ_WORK_ORDER|.
        ir_out->write( name = 'Create operation:' data = lv_msg ).
    ENDCASE.

  ENDMETHOD.

  METHOD test_delete_work_order.

    DATA(ls_order) = VALUE ztworkorder_bam( work_order_id = 0000000002
                                            status = 'PE' ).

    DATA(lv_valid) = lo_handler->delete_work_order( iv_order = ls_order-work_order_id
                                                    iv_status = ls_order-status ).

    CASE lv_valid.
      WHEN '1'.
        lv_msg = |The work order { ls_order-work_order_id } doesnt'n exist|.
        ir_out->write( name = 'Read operation:' data = lv_msg ).
      WHEN '2'.
        lv_msg = |The status of the order { ls_order-status } doesnt'n exist|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '3'.
        lv_msg = |The order { ls_order-work_order_id } with status { ls_order-status } cannot delete|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '4'.
        lv_msg = |The work order { ls_order-work_order_id } cannot delete, because has history|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '5'.
        lv_msg = |The work order { ls_order-work_order_id } has been delete|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
    ENDCASE.

  ENDMETHOD.

  METHOD test_read_work_order.

    DATA(ls_order) = lo_handler->read_work_order( 0000000001 ).

    IF ls_order IS NOT INITIAL.
      ir_out->write( name = 'Read operation:' data = ls_order ).
    ELSE.
      lv_msg = |The work order { ls_order-work_order_id } doesnt'n exist|.
      ir_out->write( name = 'Read operation:' data = lv_msg ).
    ENDIF.

  ENDMETHOD.

  METHOD test_update_work_order.

    DATA(ls_order) = VALUE ztworkorder_bam( work_order_id = 0000000004
                                            creation_date = cl_abap_context_info=>get_system_date( )
                                            status = 'PE'
                                            priority = 'B'
                                            description = 'Update data of work order' ).

    DATA(lv_valid) = lo_handler->update_work_order( ls_order ).

    CASE lv_valid.
      WHEN '1'.
        lv_msg = |The work order { ls_order-work_order_id } doesnt'n exist|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '2'.
        lv_msg = |The status { ls_order-status } doesnt'n exist|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '3'.
        lv_msg = |The work order with { ls_order-status } status cannot be modified|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
      WHEN '4'.
        lv_msg = |The work order { ls_order-work_order_id } has been update|.
        ir_out->write( name = 'Update operation:' data = lv_msg ).
    ENDCASE.

  ENDMETHOD.

  METHOD check_authority.

    rv_valid = abap_true.
    CASE iv_operation.
      WHEN 'C'.
        AUTHORITY-CHECK OBJECT 'ZAO_WO_BAM'
            ID 'ACTVT' FIELD '01'.

        IF sy-subrc <> 0.
          rv_valid = abap_false.
        ENDIF.

      WHEN 'R'.
        AUTHORITY-CHECK OBJECT 'ZAO_WO_BAM'
            ID 'ACTVT' FIELD '03'.

        IF sy-subrc <> 0.
          rv_valid = abap_false.
        ENDIF.

      WHEN 'U'.
        AUTHORITY-CHECK OBJECT 'ZAO_WO_BAM'
            ID 'ACTVT' FIELD '02'.

        IF sy-subrc <> 0.
          rv_valid = abap_false.
        ENDIF.

      WHEN 'D'.
        AUTHORITY-CHECK OBJECT 'ZAO_WO_BAM'
            ID 'ACTVT' FIELD '06'.

        IF sy-subrc <> 0.
          rv_valid = abap_false.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
