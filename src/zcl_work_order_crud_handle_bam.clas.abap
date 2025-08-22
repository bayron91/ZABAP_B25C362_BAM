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
          MODIFY ztworkorder_bam FROM @is_order.
          COMMIT WORK AND WAIT.

        CATCH cx_sy_open_sql_db INTO DATA(lx_error).
      ENDTRY.
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

ENDCLASS.
