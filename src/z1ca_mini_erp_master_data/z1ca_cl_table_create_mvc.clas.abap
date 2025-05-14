*&---------------------------------------------------------------------*
*& Class Z1CA_CL_TABLE_CREATE_MVC
*&---------------------------------------------------------------------*
*& Description: Global class for creating tables for Z1CA Mini ERP system
*& This is a helper class containing common methods for table creation
*& Methods are organized according to MVC pattern:
*& - m_: Methods that deal with Model (database operations)
*& - v_: Methods for View (user interaction/display)
*& - c_: Methods for Controller (coordination)
*& - s_: Methods that provide Service functions
*&---------------------------------------------------------------------*
CLASS z1ca_cl_table_create_mvc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Custom structure for constructor parameters
    TYPES: BEGIN OF ty_constructor_params,
             package    TYPE devclass,
             prefix     TYPE string,
             force_flag TYPE c LENGTH 1,
           END OF ty_constructor_params.

    " Table type definitions for multiple objects creation
    TYPES: ty_domain_tab  TYPE STANDARD TABLE OF dd01v WITH DEFAULT KEY,
           ty_element_tab TYPE STANDARD TABLE OF dd04v WITH DEFAULT KEY,
           ty_fields_tab  TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    " Structure for table creation
    TYPES: BEGIN OF ty_table_creation,
             table_data TYPE dd02v,
             fields     TYPE ty_fields_tab,
           END OF ty_table_creation.

    " Table type for handling multiple tables
    TYPES: ty_table_tab TYPE STANDARD TABLE OF ty_table_creation WITH DEFAULT KEY.

    METHODS:
      " Constructor with structure parameters
      constructor
        IMPORTING
          is_params TYPE ty_constructor_params,

      " Controller methods - coordinate the creation process
      c_process_domains
        IMPORTING
          it_domain_data TYPE ty_domain_tab,

      c_process_elements
        IMPORTING
          it_element_data TYPE ty_element_tab,

      c_process_tables
        IMPORTING
          it_table_data TYPE ty_table_tab,

      " New master controller method that manages entire flow
      c_create_dictionary_objects
        IMPORTING
          it_domain_data  TYPE ty_domain_tab OPTIONAL
          it_element_data TYPE ty_element_tab OPTIONAL
          it_table_data   TYPE ty_table_tab OPTIONAL,

      " Individual controller methods still available for compatibility
      c_create_domain
        IMPORTING
          is_domain_data TYPE dd01v,

      c_create_element
        IMPORTING
          is_element_data TYPE dd04v,

      c_create_table
        IMPORTING
          is_table_data TYPE dd02v
          it_fields     TYPE ty_fields_tab,

      " Model methods - deal with database operations
      m_activate_domain
        IMPORTING
          iv_domname TYPE dd01v-domname,

      m_activate_element
        IMPORTING
          iv_rollname TYPE dd04v-rollname,

      m_activate_table
        IMPORTING
          iv_tabname TYPE dd02v-tabname,

      " Service methods - utility functions for checking existence
      s_domain_exists
        IMPORTING
          iv_domname       TYPE dd01v-domname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      s_element_exists
        IMPORTING
          iv_rollname      TYPE dd04v-rollname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      s_table_exists
        IMPORTING
          iv_tabname       TYPE dd02v-tabname
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      " View methods - handle user output
      v_message
        IMPORTING
          iv_message TYPE string
          iv_type    TYPE c DEFAULT 'I'.

  PROTECTED SECTION.
    DATA:
      mv_package     TYPE devclass,
      mv_prefix      TYPE string,
      mv_dom_prefix  TYPE string,
      mv_elem_prefix TYPE string,
      mv_force       TYPE c.

  PRIVATE SECTION.
ENDCLASS.



CLASS Z1CA_CL_TABLE_CREATE_MVC IMPLEMENTATION.


  METHOD constructor.
    mv_package    = is_params-package.
    mv_prefix     = is_params-prefix.
    mv_dom_prefix = |{ is_params-prefix }D_|.
    mv_elem_prefix = |{ is_params-prefix }E_|.
    mv_force      = is_params-force_flag.

    v_message( |Package: { mv_package }| ).
    v_message( |Prefix: { mv_prefix }| ).

    IF mv_force = 'X'.
      v_message( 'Force Recreation Mode: Active' ).
    ENDIF.
  ENDMETHOD.


  METHOD c_create_dictionary_objects.
    " Master controller method that manages entire DDIC creation flow
    v_message( '============================================' ).
    v_message( |Starting DDIC objects creation process...| ).
    v_message( '============================================' ).

    " Step 1: Create domains (if provided)
    IF it_domain_data IS NOT INITIAL.
      c_process_domains( it_domain_data ).
    ENDIF.

    " Step 2: Create data elements (if provided)
    IF it_element_data IS NOT INITIAL.
      c_process_elements( it_element_data ).
    ENDIF.

    " Step 3: Create tables (if provided)
    IF it_table_data IS NOT INITIAL.
      c_process_tables( it_table_data ).
    ENDIF.

    v_message( '============================================' ).
    v_message( |DDIC objects creation completed.| ).
    v_message( '============================================' ).
  ENDMETHOD.


  METHOD c_create_domain.
    " Check if domain already exists
    DATA(lv_exists) = s_domain_exists( is_domain_data-domname ).

    " If domain exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      v_message( |Domain already exists: { is_domain_data-domname }| ).
      RETURN.
    ENDIF.

    " If force mode active and domain exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      v_message( |Updating domain: { is_domain_data-domname }...| ).
    ENDIF.

    " Create/update domain - Using passed structure directly
    DATA: ls_dd01v TYPE dd01v.

    " Fill in any default values not provided by caller
    ls_dd01v = is_domain_data.

    " Make sure language is set
    IF ls_dd01v-ddlanguage IS INITIAL.
      ls_dd01v-ddlanguage = sy-langu.
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = is_domain_data-domname
        dd01v_wa          = ls_dd01v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      v_message( |Error: Domain creation failed - { is_domain_data-domname } - { sy-subrc }| ).
    ELSE.
      v_message( |Domain created: { is_domain_data-domname }| ).

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DOMA'
          AND obj_name = @is_domain_data-domname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DOMA'
            AND obj_name = @is_domain_data-domname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'DOMA'
          obj_name  = is_domain_data-domname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate domain
      m_activate_domain( is_domain_data-domname ).
    ENDIF.
  ENDMETHOD.


  METHOD c_create_element.
    " Check if data element already exists
    DATA(lv_exists) = s_element_exists( is_element_data-rollname ).

    " If data element exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      v_message( |Data element already exists: { is_element_data-rollname }| ).
      RETURN.
    ENDIF.

    " If force mode active and data element exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      v_message( |Updating data element: { is_element_data-rollname }...| ).
    ENDIF.

    " Create/update data element - Using passed structure directly
    DATA: ls_dd04v TYPE dd04v.

    " Fill in any values not provided
    ls_dd04v = is_element_data.

    " Make sure language is set
    IF ls_dd04v-ddlanguage IS INITIAL.
      ls_dd04v-ddlanguage = sy-langu.
    ENDIF.

    " Set default screen lengths if not provided
    IF ls_dd04v-scrlen1 IS INITIAL.
      ls_dd04v-scrlen1 = '10'.
    ENDIF.

    IF ls_dd04v-scrlen2 IS INITIAL.
      ls_dd04v-scrlen2 = '20'.
    ENDIF.

    IF ls_dd04v-scrlen3 IS INITIAL.
      ls_dd04v-scrlen3 = '40'.
    ENDIF.

    IF ls_dd04v-headlen IS INITIAL.
      ls_dd04v-headlen = '55'.
    ENDIF.

    " If ddtext is initial, use reptext as fallback
    IF ls_dd04v-ddtext IS INITIAL.
      ls_dd04v-ddtext = ls_dd04v-reptext.
    ENDIF.

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = is_element_data-rollname
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      v_message( |Error: Data element creation failed - { is_element_data-rollname } - { sy-subrc }| ).
    ELSE.
      v_message( |Data element created: { is_element_data-rollname }| ).

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DTEL'
          AND obj_name = @is_element_data-rollname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DTEL'
            AND obj_name = @is_element_data-rollname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'DTEL'
          obj_name  = is_element_data-rollname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate data element
      m_activate_element( is_element_data-rollname ).
    ENDIF.
  ENDMETHOD.


  METHOD c_create_table.
    " Check if table already exists
    DATA(lv_exists) = s_table_exists( is_table_data-tabname ).

    " If table exists and force mode not active, display message and exit
    IF lv_exists = abap_true AND mv_force <> 'X'.
      v_message( |Table already exists: { is_table_data-tabname }| ).
      RETURN.
    ENDIF.

    " If force mode active and table exists, show info
    IF lv_exists = abap_true AND mv_force = 'X'.
      v_message( |Updating table: { is_table_data-tabname }...| ).
    ENDIF.

    " Create table - Using passed structure directly
    DATA: ls_dd02v TYPE dd02v.

    " Fill in any values not provided
    ls_dd02v = is_table_data.

    " Make sure language is set
    IF ls_dd02v-ddlanguage IS INITIAL.
      ls_dd02v-ddlanguage = sy-langu.
    ENDIF.

    " Set default table class if not provided
    IF ls_dd02v-tabclass IS INITIAL.
      ls_dd02v-tabclass = 'TRANSP'.
    ENDIF.

    " Set default flags if not provided
    IF ls_dd02v-mainflag IS INITIAL.
      ls_dd02v-mainflag = 'X'.
    ENDIF.

    IF ls_dd02v-contflag IS INITIAL.
      ls_dd02v-contflag = 'A'.
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = is_table_data-tabname
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
      v_message( |Error: Table creation failed - { is_table_data-tabname } - { sy-subrc }| ).
    ELSE.
      v_message( |Table created: { is_table_data-tabname }| ).

      " Add/update package information in TADIR
      " First check if record exists in TADIR
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'TABL'
          AND obj_name = @is_table_data-tabname.

      IF sy-subrc = 0.
        " If record exists, update it
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'TABL'
            AND obj_name = @is_table_data-tabname.
      ELSE.
        " If record doesn't exist, insert it
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'TABL'
          obj_name  = is_table_data-tabname
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      " Activate table
      m_activate_table( is_table_data-tabname ).
    ENDIF.
  ENDMETHOD.


  METHOD c_process_domains.
    " Process all domains from the input table
    DATA(lv_count) = lines( it_domain_data ).
    v_message( |Processing { lv_count } domain(s)...| ).

    LOOP AT it_domain_data INTO DATA(ls_domain).
      c_create_domain( ls_domain ).
    ENDLOOP.

    v_message( |Domain processing completed.| ).
  ENDMETHOD.


  METHOD c_process_elements.
    " Process all elements from the input table
    DATA(lv_count) = lines( it_element_data ).
    v_message( |Processing { lv_count } data element(s)...| ).

    LOOP AT it_element_data INTO DATA(ls_element).
      c_create_element( ls_element ).
    ENDLOOP.

    v_message( |Data element processing completed.| ).
  ENDMETHOD.


  METHOD c_process_tables.
    " Process all tables from the input table
    DATA(lv_count) = lines( it_table_data ).
    v_message( |Processing { lv_count } table(s)...| ).

    LOOP AT it_table_data INTO DATA(ls_table_creation).
      c_create_table(
        is_table_data = ls_table_creation-table_data
        it_fields     = ls_table_creation-fields
      ).
    ENDLOOP.

    v_message( |Table processing completed.| ).
  ENDMETHOD.


  METHOD m_activate_domain.
    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
      EXPORTING
        name              = iv_domname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      v_message( |Error: Domain { iv_domname } activation failed - { sy-subrc }| ).
    ELSE.
      v_message( |Domain { iv_domname } activated.| ).
    ENDIF.
  ENDMETHOD.


  METHOD m_activate_element.
    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name              = iv_rollname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      v_message( |Error: Data element { iv_rollname } activation failed - { sy-subrc }| ).
    ELSE.
      v_message( |Data element { iv_rollname } activated.| ).
    ENDIF.
  ENDMETHOD.


  METHOD m_activate_table.
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name              = iv_tabname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      v_message( |Error: Table { iv_tabname } activation failed - { sy-subrc }| ).
    ELSE.
      v_message( |Table { iv_tabname } activated.| ).
    ENDIF.
  ENDMETHOD.


  METHOD s_domain_exists.
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


  METHOD s_element_exists.
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


  METHOD s_table_exists.
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


  METHOD v_message.
    CASE iv_type.
      WHEN 'E'.
        WRITE: / |Error: { iv_message }|.
      WHEN 'W'.
        WRITE: / |Warning: { iv_message }|.
      WHEN OTHERS.
        WRITE: / iv_message.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
