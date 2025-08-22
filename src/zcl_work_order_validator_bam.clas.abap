CLASS zcl_work_order_validator_bam DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS validate_creation_work_order IMPORTING iv_customer_id   TYPE zde_customer_id_bam
                                                   iv_technician_id TYPE zde_technician_id_bam
                                                   iv_priority      TYPE zde_priority_code_bm
                                                   iv_status        TYPE zde_status_code_bam
                                         RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_update_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_bam
                                                 iv_status        TYPE zde_status_code_bam
                                       RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_delete_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_bam
                                                 iv_status        TYPE zde_status_code_bam
                                       RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_read_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_bam
                                     RETURNING VALUE(rv_valid)  TYPE abap_bool.

  PRIVATE SECTION.
    METHODS check_customer_exists IMPORTING iv_customer_id   TYPE zde_customer_id_bam
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE zde_technician_id_bam
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_priority_exists IMPORTING iv_priority      TYPE zde_priority_code_bm
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_status_code IMPORTING iv_status        TYPE zde_status_code_bam
                              RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_bam
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE zde_work_order_id_bam
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.


CLASS zcl_work_order_validator_bam IMPLEMENTATION.

  METHOD validate_creation_work_order.

    DATA(lv_customer) = check_customer_exists( iv_customer_id ).
    IF lv_customer IS INITIAL.
      rv_valid = '1'.
      RETURN.
    ENDIF.

    DATA(lv_technician) = check_technician_exists( iv_technician_id ).
    IF lv_technician IS INITIAL.
      rv_valid = '2'.
      RETURN.
    ENDIF.

    DATA(lv_priority) = check_priority_exists( iv_priority ).
    IF lv_priority IS INITIAL.
      rv_valid = '3'.
      RETURN.
    ENDIF.

    DATA(lv_status) = check_status_code( iv_status ).
    IF lv_status IS INITIAL.
      rv_valid = '4'.
      RETURN.
    ENDIF.

    rv_valid = '5'. "Ok

  ENDMETHOD.

  METHOD validate_update_work_order.

    DATA(lv_order) = check_order_exists( iv_work_order_id ).
    IF lv_order IS INITIAL.
      rv_valid = '1'.
      RETURN.
    ENDIF.

    DATA(lv_status) = check_status_code( iv_status ).
    IF lv_status IS INITIAL.
      rv_valid = '2'.
      RETURN.
    ELSEIF iv_status <> 'PE'.
      rv_valid = '3'.
      RETURN.
    ENDIF.

    rv_valid = '4'. "Ok

  ENDMETHOD.

  METHOD validate_delete_work_order.

    DATA(lv_order) = check_order_exists( iv_work_order_id ).
    IF lv_order IS INITIAL.
      rv_valid = '1'.
      RETURN.
    ENDIF.

    DATA(lv_status) = check_status_code( iv_status ).
    IF lv_status IS INITIAL.
      rv_valid = '2'.
      RETURN.
    ELSEIF iv_status <> 'PE'.
      rv_valid = '3'.
      RETURN.
    ENDIF.

    DATA(lv_order_his) = check_order_history( iv_work_order_id ).
    IF lv_order_his IS NOT INITIAL.
      rv_valid = '4'.
      RETURN.
    ENDIF.

    rv_valid = '5'. "Ok

  ENDMETHOD.

  METHOD validate_read_work_order.

    DATA(lv_order) = check_order_exists( iv_work_order_id ).
    IF lv_order IS INITIAL.
      rv_valid = '1'.
      RETURN.
    ENDIF.

    rv_valid = '2'. "Ok

  ENDMETHOD.

  METHOD check_customer_exists.

    IF iv_customer_id IS NOT INITIAL.

      SELECT SINGLE FROM ztcustomer_bam
          FIELDS *
          WHERE customer_id = @iv_customer_id
          INTO @DATA(ls_customer).

      IF sy-subrc EQ 0.
        rv_exists = 'X'.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD check_technician_exists.

    IF iv_technician_id IS NOT INITIAL.
      SELECT SINGLE FROM zttechnician_bam
          FIELDS *
          WHERE technician_id = @iv_technician_id
          INTO @DATA(ls_technician).

      IF sy-subrc EQ 0.
        rv_exists = 'X'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_priority_exists.

    IF iv_priority IS NOT INITIAL.
      SELECT SINGLE FROM ztpriority_bam
          FIELDS *
          WHERE priority_code = @iv_priority
          INTO @DATA(ls_priority).

      IF sy-subrc EQ 0.
        rv_exists = 'X'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_status_code.

    IF iv_status IS NOT INITIAL.
      SELECT SINGLE FROM ztstatus_bam
          FIELDS *
          WHERE status_code = @iv_status
          INTO @DATA(ls_status).

      IF sy-subrc EQ 0.
        rv_exists = abap_true.
      ELSE.
        rv_exists = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_exists.

    IF iv_work_order_id IS NOT INITIAL.
      SELECT SINGLE FROM ztworkorder_bam
          FIELDS *
          WHERE work_order_id = @iv_work_order_id
          INTO @DATA(ls_order).

      IF sy-subrc EQ 0.
        rv_exists = abap_true.
      ELSE.
        rv_exists = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_history.

    IF iv_work_order_id IS NOT INITIAL.
      SELECT SINGLE FROM ztwohist_bam
          FIELDS *
          WHERE work_order_id = @iv_work_order_id
          INTO @DATA(ls_order_history).

      IF sy-subrc EQ 0.
        rv_exists = abap_true.
      ELSE.
        rv_exists = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
