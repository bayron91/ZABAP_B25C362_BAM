CLASS zcl_work_order_crud_handle_bam DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA lo_validator TYPE REF TO zcl_work_order_validator_bam.

    METHODS create_work_order IMPORTING is_order        TYPE ztworkorder_bam
                              RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS update_work_order IMPORTING is_order        TYPE ztworkorder_bam
                              RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS create_work_order_history IMPORTING is_order TYPE ztworkorder_bam.

    METHODS delete_work_order IMPORTING iv_order        TYPE zde_work_order_id_bam
                                        iv_status       TYPE zde_status_code_bam
                              RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS read_work_order IMPORTING iv_order        TYPE zde_work_order_id_bam
                            RETURNING VALUE(rs_order) TYPE ztworkorder_bam.

  PRIVATE SECTION.
    METHODS instance_lock_object RETURNING VALUE(ro_lock_object) TYPE REF TO if_abap_lock_object.
    METHODS enqueue_lock_object IMPORTING lo_lock_object  TYPE REF TO if_abap_lock_object
                                          it_parameter    TYPE if_abap_lock_object=>tt_parameter
                                RETURNING VALUE(rv_valid) TYPE abap_bool.
    METHODS dequeue_lock_object IMPORTING lo_lock_object  TYPE REF TO if_abap_lock_object
                                          it_parameter    TYPE if_abap_lock_object=>tt_parameter
                                RETURNING VALUE(rv_valid) TYPE abap_bool.

ENDCLASS.


CLASS zcl_work_order_crud_handle_bam IMPLEMENTATION.

  METHOD create_work_order.

    IF lo_validator IS NOT BOUND.
      lo_validator = NEW #( ).
    ENDIF.

    rv_valid = lo_validator->validate_creation_work_order( iv_customer_id   = is_order-customer_id
                                                           iv_technician_id = is_order-technician_id
                                                           iv_priority      = is_order-priority
                                                           iv_status        = is_order-status ).

    IF rv_valid EQ '5'.
      TRY.
          "Get instance lock object
          DATA(lo_lock_object) = instance_lock_object( ).

          IF lo_lock_object IS NOT BOUND.
            DATA: lt_parameter TYPE if_abap_lock_object=>tt_parameter.

            lt_parameter = VALUE #( ( name = 'WORK_ORDER_ID' value = REF #( is_order-work_order_id ) )
                                    ( name = 'CUSTOMER_ID' value = REF #( is_order-customer_id ) )
                                    ( name = 'TECHNICIAN_ID' value = REF #( is_order-technician_id ) ) ).

            "Enqueue lock object
            DATA(lv_lock_valid) = enqueue_lock_object( lo_lock_object = lo_lock_object
                                                       it_parameter = lt_parameter ).

            IF lv_lock_valid = abap_true.
              MODIFY ztworkorder_bam FROM @is_order.
              COMMIT WORK AND WAIT.
            ELSE.
              rv_valid = '6'.
            ENDIF.
          ENDIF.

        CATCH cx_sy_open_sql_db INTO DATA(lx_error).
      ENDTRY.

      "Dequeue lock object
      lv_lock_valid = dequeue_lock_object( lo_lock_object = lo_lock_object
                                           it_parameter = lt_parameter ).

      IF lv_lock_valid IS INITIAL.
        rv_valid = '6'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD update_work_order.

    IF lo_validator IS NOT BOUND.
      lo_validator = NEW #( ).
    ENDIF.

    rv_valid = lo_validator->validate_update_work_order( iv_work_order_id = is_order-work_order_id
                                                         iv_status        = is_order-status ).

    IF rv_valid EQ '4'.
      TRY.
          UPDATE ztworkorder_bam SET creation_date = @is_order-creation_date,
                                     status = @is_order-status,
                                     priority = @is_order-priority,
                                     description = @is_order-description
              WHERE work_order_id = @is_order-work_order_id.

          COMMIT WORK AND WAIT.
          create_work_order_history( is_order ).

        CATCH cx_sy_open_sql_db INTO DATA(lx_error).
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD create_work_order_history.

    DATA(ls_order_hist) = VALUE ztwohist_bam( work_order_id = is_order-work_order_id
                                              modification_date = cl_abap_context_info=>get_system_date( )
                                              change_description = |The order has been update| ).

    TRY.
        MODIFY ztwohist_bam FROM @ls_order_hist.
        COMMIT WORK AND WAIT.

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
    ENDTRY.

  ENDMETHOD.

  METHOD delete_work_order.

    IF lo_validator IS NOT BOUND.
      lo_validator = NEW #( ).
    ENDIF.

    rv_valid = lo_validator->validate_delete_work_order( iv_work_order_id = iv_order
                                                         iv_status = iv_status ).

    IF rv_valid EQ '5'.
      TRY.
          DELETE FROM ztworkorder_bam WHERE work_order_id = @iv_order.
          COMMIT WORK AND WAIT.

        CATCH cx_sy_open_sql_db INTO DATA(lx_error).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.

    IF lo_validator IS NOT BOUND.
      lo_validator = NEW #( ).
    ENDIF.

    DATA(lv_valid) = lo_validator->validate_read_work_order( iv_order ).

    IF lv_valid EQ '2'.

      SELECT SINGLE FROM ztworkorder_bam
        FIELDS *
        WHERE work_order_id = @iv_order
        INTO @rs_order.

    ENDIF.

  ENDMETHOD.

  METHOD instance_lock_object.

    TRY.
        ro_lock_object = cl_abap_lock_object_factory=>get_instance( 'EZ_WORK_ORDER' ).
      CATCH cx_abap_lock_failure.
    ENDTRY.

  ENDMETHOD.

  METHOD enqueue_lock_object.

    rv_valid = abap_true.
    TRY.
        lo_lock_object->enqueue( it_parameter = it_parameter ).
      CATCH cx_abap_foreign_lock.
        rv_valid = abap_false.
      CATCH cx_abap_lock_failure.
        rv_valid = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD dequeue_lock_object.

    rv_valid = abap_true.
    TRY.
        lo_lock_object->dequeue( it_parameter = it_parameter ).
      CATCH cx_abap_lock_failure.
        rv_valid = abap_false.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
