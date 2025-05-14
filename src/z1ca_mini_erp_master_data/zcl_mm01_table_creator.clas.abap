*&---------------------------------------------------------------------*
*& Class ZCL_MM01_TABLE_CREATOR
*&---------------------------------------------------------------------*
*& Description: Global class for creating SAP MM01 tables
*& This is a helper class containing common methods for table creation
*&---------------------------------------------------------------------*
CLASS zcl_mm01_table_creator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_fields_tab TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          iv_package    TYPE devclass
          iv_prefix     TYPE string
          iv_force_flag TYPE c,

      check_and_create_domain
        IMPORTING
          iv_domname  TYPE dd01v-domname
          iv_datatype TYPE dd01v-datatype
          iv_leng     TYPE dd01v-leng
          iv_ddtext   TYPE dd01v-ddtext,

      check_and_create_data_element
        IMPORTING
          iv_rollname  TYPE dd04v-rollname
          iv_domname   TYPE dd04v-domname
          iv_reptext   TYPE dd04v-reptext
          iv_scrtext_s TYPE dd04v-scrtext_s
          iv_scrtext_m TYPE dd04v-scrtext_m
          iv_scrtext_l TYPE dd04v-scrtext_l,

      create_table
        IMPORTING
          iv_tabname      TYPE dd02v-tabname
          iv_ddtext       TYPE dd02v-ddtext
          it_fields       TYPE ty_fields_tab,

      activate_domain
        IMPORTING
          iv_domname TYPE dd01v-domname,

      activate_data_element
        IMPORTING
          iv_rollname TYPE dd04v-rollname,

      activate_table
        IMPORTING
          iv_tabname TYPE dd02v-tabname,

      domain_exists
        IMPORTING
          iv_domname       TYPE dd01v-domname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      data_element_exists
        IMPORTING
          iv_rollname      TYPE dd04v-rollname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      table_exists
        IMPORTING
          iv_tabname       TYPE dd02v-tabname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool.

  PROTECTED SECTION.
    DATA:
      mv_package    TYPE devclass,
      mv_prefix     TYPE string,
      mv_dom_prefix TYPE string,
      mv_elem_prefix TYPE string,
      mv_force      TYPE c.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MM01_TABLE_CREATOR IMPLEMENTATION.


  METHOD activate_data_element.
    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name              = iv_rollname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      WRITE: / |Error: Data element { iv_rollname } activation failed - { sy-subrc }|.
    ELSE.
      WRITE: / |Data element { iv_rollname } activated.|.
    ENDIF.
  ENDMETHOD.


  METHOD activate_domain.
    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
      EXPORTING
        name              = iv_domname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      WRITE: / |Error: Domain { iv_domname } activation failed - { sy-subrc }|.
    ELSE.
      WRITE: / |Domain { iv_domname } activated.|.
    ENDIF.
  ENDMETHOD.


  METHOD activate_table.
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name              = iv_tabname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      WRITE: / |Error: Table { iv_tabname } activation failed - { sy-subrc }|.
    ELSE.
      WRITE: / |Table { iv_tabname } activated.|.
    ENDIF.
  ENDMETHOD.


  METHOD check_and_create_data_element.
    " Check if data element already exists
    DATA(lv_exists) = data_element_exists( iv_rollname ).

    " If data element exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Data element already exists: { iv_rollname }|.
      RETURN.
    ENDIF.

    " If force mode active and data element exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Updating data element: { iv_rollname }...|.
    ENDIF.

    " Create/update data element
    DATA: ls_dd04v TYPE dd04v.

    CLEAR ls_dd04v.
    ls_dd04v-rollname   = iv_rollname.
    ls_dd04v-ddlanguage = sy-langu.
    ls_dd04v-domname    = iv_domname.
    ls_dd04v-reptext    = iv_reptext.
    ls_dd04v-scrtext_s  = iv_scrtext_s.
    ls_dd04v-scrtext_m  = iv_scrtext_m.
    ls_dd04v-scrtext_l  = iv_scrtext_l.
    ls_dd04v-ddtext     = iv_reptext.  " Short Description

    ls_dd04v-scrlen1   = '10'.
    ls_dd04v-scrlen2   = '20'.
    ls_dd04v-scrlen3   = '40'.
    ls_dd04v-headlen   = '55'.

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = iv_rollname
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      WRITE: / |Error: Data element creation failed - { iv_rollname } - { sy-subrc }|.
    ELSE.
      WRITE: / |Data element created: { iv_rollname }|.

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DTEL'
          AND obj_name = @iv_rollname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DTEL'
            AND obj_name = @iv_rollname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'DTEL'
          obj_name  = iv_rollname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate data element
      activate_data_element( iv_rollname ).
    ENDIF.
  ENDMETHOD.


  METHOD check_and_create_domain.
    " Check if domain already exists
    DATA(lv_exists) = domain_exists( iv_domname ).

    " If domain exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Domain already exists: { iv_domname }|.
      RETURN.
    ENDIF.

    " If force mode active and domain exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Updating domain: { iv_domname }...|.
    ENDIF.

    " Create/update domain
    DATA: ls_dd01v TYPE dd01v.

    CLEAR ls_dd01v.
    ls_dd01v-domname    = iv_domname.
    ls_dd01v-ddlanguage = sy-langu.
    ls_dd01v-datatype   = iv_datatype.
    ls_dd01v-leng       = iv_leng.
    ls_dd01v-ddtext     = iv_ddtext.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = iv_domname
        dd01v_wa          = ls_dd01v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      WRITE: / |Error: Domain creation failed - { iv_domname } - { sy-subrc }|.
    ELSE.
      WRITE: / |Domain created: { iv_domname }|.

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DOMA'
          AND obj_name = @iv_domname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DOMA'
            AND obj_name = @iv_domname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'DOMA'
          obj_name  = iv_domname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate domain
      activate_domain( iv_domname ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mv_package    = iv_package.
    mv_prefix     = iv_prefix.
    mv_dom_prefix = |{ iv_prefix }D_|.
    mv_elem_prefix = |{ iv_prefix }E_|.
    mv_force      = iv_force_flag.

    WRITE: / |Package: { mv_package }|.
    WRITE: / |Prefix: { mv_prefix }|.

    IF mv_force = 'X'.
      WRITE: / 'Force Recreation Mode: Active'.
    ENDIF.
  ENDMETHOD.


  METHOD create_table.
    " Check if table already exists
    DATA(lv_exists) = table_exists( iv_tabname ).

    " If table exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Table already exists: { iv_tabname }|.
      RETURN.
    ENDIF.

    " If force mode active and table exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Updating table: { iv_tabname }...|.
    ENDIF.

    " Create table
    DATA: ls_dd02v TYPE dd02v.

    CLEAR ls_dd02v.
    ls_dd02v-tabname     = iv_tabname.
    ls_dd02v-ddlanguage  = sy-langu.
    ls_dd02v-tabclass    = 'TRANSP'.
    ls_dd02v-mainflag    = 'X'.
    ls_dd02v-contflag    = 'A'.
    ls_dd02v-ddtext      = iv_ddtext.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = iv_tabname
        dd02v_wa          = ls_dd02v
      TABLES
        dd03p_tab         = it_fields
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      WRITE: / |Error: Table creation failed - { iv_tabname } - { sy-subrc }|.
    ELSE.
      WRITE: / |Table created: { iv_tabname }|.

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'TABL'
          AND obj_name = @iv_tabname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'TABL'
            AND obj_name = @iv_tabname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'TABL'
          obj_name  = iv_tabname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate table
      activate_table( iv_tabname ).
    ENDIF.
  ENDMETHOD.


  METHOD data_element_exists.
    " 1. Check catalog table
    SELECT SINGLE * FROM dd04l INTO @DATA(ls_dd04l)
      WHERE rollname = @iv_rollname.

    " If not in catalog, definitely doesn't exist
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Check if active
    IF ls_dd04l-as4local <> 'A'. " A = Active
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Get detailed information
    DATA: ls_dd04v TYPE dd04v.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = iv_rollname
      IMPORTING
        dd04v_wa      = ls_dd04v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    " If API couldn't get info or content is empty, doesn't exist
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " If passes all checks, data element exists
    rv_exists = abap_true.
  ENDMETHOD.


  METHOD domain_exists.
    " 1. Check catalog table
    SELECT SINGLE * FROM dd01l INTO @DATA(ls_dd01l)
      WHERE domname = @iv_domname.

    " If not in catalog, definitely doesn't exist
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Check if active
    IF ls_dd01l-as4local <> 'A'. " A = Active
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Get detailed information
    DATA: ls_dd01v TYPE dd01v.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = iv_domname
      IMPORTING
        dd01v_wa      = ls_dd01v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    " If API couldn't get info or content is empty, doesn't exist
    IF sy-subrc <> 0 OR ls_dd01v IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " If passes all checks, domain exists
    rv_exists = abap_true.
  ENDMETHOD.


  METHOD table_exists.
    " 1. Check catalog table
    SELECT SINGLE * FROM dd02l INTO @DATA(ls_dd02l)
      WHERE tabname = @iv_tabname.

    " If not in catalog, definitely doesn't exist
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Check if active
    IF ls_dd02l-as4local <> 'A'. " A = Active
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Get detailed information
    DATA: ls_dd02v TYPE dd02v,
          lt_dd03p TYPE STANDARD TABLE OF dd03p.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = iv_tabname
      IMPORTING
        dd02v_wa      = ls_dd02v
      TABLES
        dd03p_tab     = lt_dd03p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    " If API couldn't get info, content is empty, or no field definitions, doesn't exist
    IF sy-subrc <> 0 OR ls_dd02v IS INITIAL OR lt_dd03p[] IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " If passes all checks, table exists
    rv_exists = abap_true.
  ENDMETHOD.
ENDCLASS.
