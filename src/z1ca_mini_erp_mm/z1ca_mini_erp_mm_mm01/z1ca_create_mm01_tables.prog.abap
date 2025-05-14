**&---------------------------------------------------------------------*
**& Report Z1CA_CREATE_MM01_TABLES
**&---------------------------------------------------------------------*
**& Description: This program creates SAP and SPRO tables for MM01
**& with customizable prefix (MVP scope) TEST...TEST
**&---------------------------------------------------------------------*
 REPORT z1ca_create_mm01_tables.

 data gv_a type i.
*
**&---------------------------------------------------------------------*
**& Selection Screen
**&---------------------------------------------------------------------*
*PARAMETERS: p_pack  TYPE devclass DEFAULT 'Z1CA_MINI_ERP_MASTER_DATA' OBLIGATORY,
*            p_pref  TYPE string  DEFAULT 'Z1CA_' OBLIGATORY,          " Customizable prefix
*            p_force TYPE c AS CHECKBOX DEFAULT ' ',                    " Force recreate objects
*            p_sap   RADIOBUTTON GROUP g1,                             " Only SAP tables
*            p_spro  RADIOBUTTON GROUP g1,                             " Only SPRO tables
*            p_all   RADIOBUTTON GROUP g1 DEFAULT 'X'.                 " All tables
*
**&---------------------------------------------------------------------*
**& Main Class Definition
**&---------------------------------------------------------------------*
*CLASS lcl_table_creator DEFINITION FINAL.
*  PUBLIC SECTION.
*    METHODS:
*      constructor IMPORTING iv_package    TYPE devclass
*                            iv_prefix     TYPE string
*                            iv_force_flag TYPE c
*                            iv_table_type TYPE string,
*      create_tables.
*
*  PRIVATE SECTION.
*    CONSTANTS:
*      c_type_sap  TYPE string VALUE 'SAP',    " SAP tables
*      c_type_spro TYPE string VALUE 'SPRO'.   " SPRO tables
*
*    DATA:
*      mv_package     TYPE devclass,
*      mv_prefix      TYPE string,         " User-defined prefix
*      mv_dom_prefix  TYPE string,         " Domain prefix
*      mv_elem_prefix TYPE string,        " Element prefix
*      mv_force       TYPE c,
*      mv_table_type  TYPE string.         " SAP, SPRO or empty (all)
*
*    METHODS:
*      " SAP Material tables
*      create_mara_table,    " Material master
*      create_marc_table,    " Plant data
*      create_mard_table,    " Storage location data
*      create_makt_table,    " Material descriptions
*      create_mbew_table,    " Material valuation
*      create_mvke_table,    " Sales data
*      create_mlgn_table,    " Warehouse data
*      create_eine_table,    " Purchasing info
*      create_marm_table,    " Alternative units
*
*      " SPRO tables
*      create_t001_table,    " Company codes
*      create_t001w_table,   " Plants
*      create_t001l_table,   " Storage locations
*      create_t134_table,    " Material types
*      create_t023_table,    " Material groups
*      create_t006_table,    " Units of measure
*
*      " Helper methods
*      check_and_create_domain
*        IMPORTING
*          iv_domname  TYPE dd01v-domname
*          iv_datatype TYPE dd01v-datatype
*          iv_leng     TYPE dd01v-leng
*          iv_ddtext   TYPE dd01v-ddtext,
*
*      check_and_create_data_element
*        IMPORTING
*          iv_rollname  TYPE dd04v-rollname
*          iv_domname   TYPE dd04v-domname
*          iv_reptext   TYPE dd04v-reptext
*          iv_scrtext_s TYPE dd04v-scrtext_s
*          iv_scrtext_m TYPE dd04v-scrtext_m
*          iv_scrtext_l TYPE dd04v-scrtext_l,
*
*      create_single_table
*        IMPORTING
*          iv_tabname TYPE dd02v-tabname
*          iv_ddtext  TYPE dd02v-ddtext
*          it_fields  TYPE STANDARD TABLE,
*
*      activate_domain
*        IMPORTING
*          iv_domname TYPE dd01v-domname,
*
*      activate_data_element
*        IMPORTING
*          iv_rollname TYPE dd04v-rollname,
*
*      activate_table
*        IMPORTING
*          iv_tabname TYPE dd02v-tabname,
*
*      domain_exists
*        IMPORTING
*          iv_domname       TYPE dd01v-domname
*        RETURNING
*          VALUE(rv_exists) TYPE abap_bool,
*
*      data_element_exists
*        IMPORTING
*          iv_rollname      TYPE dd04v-rollname
*        RETURNING
*          VALUE(rv_exists) TYPE abap_bool,
*
*      table_exists
*        IMPORTING
*          iv_tabname       TYPE dd02v-tabname
*        RETURNING
*          VALUE(rv_exists) TYPE abap_bool,
*
*      is_table_type_matched
*        IMPORTING
*          iv_type           TYPE string
*        RETURNING
*          VALUE(rv_matched) TYPE abap_bool.
*ENDCLASS.
*
**&---------------------------------------------------------------------*
**& Main Class Implementation
**&---------------------------------------------------------------------*
*CLASS lcl_table_creator IMPLEMENTATION.
*  METHOD constructor.
*    mv_package    = iv_package.
*    mv_prefix     = iv_prefix.
*    mv_dom_prefix = |{ iv_prefix }D_|.
*    mv_elem_prefix = |{ iv_prefix }E_|.
*    mv_force      = iv_force_flag.
*    mv_table_type = iv_table_type.
*
*    WRITE: / |Package: { mv_package }|.
*    WRITE: / |Prefix: { mv_prefix }|.
*
*    IF mv_force = 'X'.
*      WRITE: / 'Force Recreation Mode: Active'.
*    ENDIF.
*
*    IF mv_table_type IS NOT INITIAL.
*      WRITE: / |Selected Table Type: { mv_table_type }|.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD create_tables.
*    " Write header
*    WRITE: / '============================================'.
*    WRITE: / |  Creating { mv_prefix } Tables for MM01  |.
*    WRITE: / '============================================'.
*
*    " SAP Tables
*    IF is_table_type_matched( c_type_sap ).
*      WRITE: / '---------- SAP Tables ----------'.
*      create_mara_table( ).
*      create_marc_table( ).
*      create_mard_table( ).
*      create_makt_table( ).
*      create_mbew_table( ).
*      create_mvke_table( ).
*      create_mlgn_table( ).
*      create_eine_table( ).
*      create_marm_table( ).
*    ENDIF.
*
*    " SPRO Tables
*    IF is_table_type_matched( c_type_spro ).
*      WRITE: / '---------- SPRO Tables ----------'.
*      create_t001_table( ).
*      create_t001w_table( ).
*      create_t001l_table( ).
*      create_t134_table( ).
*      create_t023_table( ).
*      create_t006_table( ).
*    ENDIF.
*
*    " Completion message
*    WRITE: / '============================================'.
*    WRITE: / 'Table creation process completed.'.
*    WRITE: / '============================================'.
*  ENDMETHOD.
*
*  METHOD domain_exists.
*    " 1. Check catalog table
*    SELECT SINGLE * FROM dd01l INTO @DATA(ls_dd01l)
*      WHERE domname = @iv_domname.
*
*    " If not in catalog, definitely doesn't exist
*    IF sy-subrc <> 0.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 2. Check if active
*    IF ls_dd01l-as4local <> 'A'. " A = Active
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 3. Get detailed information
*    DATA: ls_dd01v TYPE dd01v.
*
*    CALL FUNCTION 'DDIF_DOMA_GET'
*      EXPORTING
*        name          = iv_domname
*      IMPORTING
*        dd01v_wa      = ls_dd01v
*      EXCEPTIONS
*        illegal_input = 1
*        OTHERS        = 2.
*
*    " If API couldn't get info or content is empty, doesn't exist
*    IF sy-subrc <> 0 OR ls_dd01v IS INITIAL.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " If passes all checks, domain exists
*    rv_exists = abap_true.
*  ENDMETHOD.
*
*  METHOD data_element_exists.
*    " 1. Check catalog table
*    SELECT SINGLE * FROM dd04l INTO @DATA(ls_dd04l)
*      WHERE rollname = @iv_rollname.
*
*    " If not in catalog, definitely doesn't exist
*    IF sy-subrc <> 0.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 2. Check if active
*    IF ls_dd04l-as4local <> 'A'. " A = Active
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 3. Get detailed information
*    DATA: ls_dd04v TYPE dd04v.
*
*    CALL FUNCTION 'DDIF_DTEL_GET'
*      EXPORTING
*        name          = iv_rollname
*      IMPORTING
*        dd04v_wa      = ls_dd04v
*      EXCEPTIONS
*        illegal_input = 1
*        OTHERS        = 2.
*
*    " If API couldn't get info or content is empty, doesn't exist
*    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " If passes all checks, data element exists
*    rv_exists = abap_true.
*  ENDMETHOD.
*
*  METHOD table_exists.
*    " 1. Check catalog table
*    SELECT SINGLE * FROM dd02l INTO @DATA(ls_dd02l)
*      WHERE tabname = @iv_tabname.
*
*    " If not in catalog, definitely doesn't exist
*    IF sy-subrc <> 0.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 2. Check if active
*    IF ls_dd02l-as4local <> 'A'. " A = Active
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " 3. Get detailed information
*    DATA: ls_dd02v TYPE dd02v,
*          lt_dd03p TYPE STANDARD TABLE OF dd03p.
*
*    CALL FUNCTION 'DDIF_TABL_GET'
*      EXPORTING
*        name          = iv_tabname
*      IMPORTING
*        dd02v_wa      = ls_dd02v
*      TABLES
*        dd03p_tab     = lt_dd03p
*      EXCEPTIONS
*        illegal_input = 1
*        OTHERS        = 2.
*
*    " If API couldn't get info, content is empty, or no field definitions, doesn't exist
*    IF sy-subrc <> 0 OR ls_dd02v IS INITIAL OR lt_dd03p[] IS INITIAL.
*      rv_exists = abap_false.
*      RETURN.
*    ENDIF.
*
*    " If passes all checks, table exists
*    rv_exists = abap_true.
*  ENDMETHOD.
*
*  METHOD check_and_create_domain.
*    " Check if domain already exists
*    DATA(lv_exists) = domain_exists( iv_domname ).
*
*    " If domain exists and force mode not active, display message and exit
*    IF lv_exists = abap_true AND mv_force <> 'X'.
*      WRITE: / |Domain already exists: { iv_domname }|.
*      RETURN.
*    ENDIF.
*
*    " If force mode active and domain exists, show info
*    IF lv_exists = abap_true AND mv_force = 'X'.
*      WRITE: / |Updating domain: { iv_domname }...|.
*    ENDIF.
*
*    " Create/update domain
*    DATA: ls_dd01v TYPE dd01v.
*
*    CLEAR ls_dd01v.
*    ls_dd01v-domname    = iv_domname.
*    ls_dd01v-ddlanguage = sy-langu.
*    ls_dd01v-datatype   = iv_datatype.
*    ls_dd01v-leng       = iv_leng.
*    ls_dd01v-ddtext     = iv_ddtext.
*
*    CALL FUNCTION 'DDIF_DOMA_PUT'
*      EXPORTING
*        name              = iv_domname
*        dd01v_wa          = ls_dd01v
*      EXCEPTIONS
*        doma_not_found    = 1
*        name_inconsistent = 2
*        doma_inconsistent = 3
*        put_failure       = 4
*        put_refused       = 5
*        OTHERS            = 6.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Domain creation failed - { iv_domname } - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Domain created: { iv_domname }|.
*
*      " Add/update package information in TADIR
*      " First check if record exists in TADIR
*      SELECT SINGLE * FROM tadir
*        INTO @DATA(ls_tadir)
*        WHERE pgmid    = 'R3TR'
*          AND object   = 'DOMA'
*          AND obj_name = @iv_domname.
*
*      IF sy-subrc = 0.
*        " If record exists, update it
*        UPDATE tadir SET devclass = @mv_package,
*                         author   = @sy-uname,
*                         masterlang = @sy-langu
*          WHERE pgmid    = 'R3TR'
*            AND object   = 'DOMA'
*            AND obj_name = @iv_domname.
*      ELSE.
*        " If record doesn't exist, insert it
*        INSERT INTO tadir VALUES @( VALUE #(
*          pgmid     = 'R3TR'
*          object    = 'DOMA'
*          obj_name  = iv_domname
*          devclass  = mv_package
*          srcsystem = sy-sysid
*          author    = sy-uname
*          masterlang = sy-langu
*        ) ).
*      ENDIF.
*
*      " Activate domain
*      activate_domain( iv_domname ).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD check_and_create_data_element.
*    " Check if data element already exists
*    DATA(lv_exists) = data_element_exists( iv_rollname ).
*
*    " If data element exists and force mode not active, display message and exit
*    IF lv_exists = abap_true AND mv_force <> 'X'.
*      WRITE: / |Data element already exists: { iv_rollname }|.
*      RETURN.
*    ENDIF.
*
*    " If force mode active and data element exists, show info
*    IF lv_exists = abap_true AND mv_force = 'X'.
*      WRITE: / |Updating data element: { iv_rollname }...|.
*    ENDIF.
*
*    " Create/update data element
*    DATA: ls_dd04v TYPE dd04v.
*
*    CLEAR ls_dd04v.
*    ls_dd04v-rollname   = iv_rollname.
*    ls_dd04v-ddlanguage = sy-langu.
*    ls_dd04v-domname    = iv_domname.
*    ls_dd04v-reptext    = iv_reptext.
*    ls_dd04v-scrtext_s  = iv_scrtext_s.
*    ls_dd04v-scrtext_m  = iv_scrtext_m.
*    ls_dd04v-scrtext_l  = iv_scrtext_l.
*    ls_dd04v-ddtext     = iv_reptext.  " Short Description
*
*    ls_dd04v-scrlen1   = '10'.
*    ls_dd04v-scrlen2   = '20'.
*    ls_dd04v-scrlen3   = '40'.
*    ls_dd04v-headlen   = '55'.
*
*    CALL FUNCTION 'DDIF_DTEL_PUT'
*      EXPORTING
*        name              = iv_rollname
*        dd04v_wa          = ls_dd04v
*      EXCEPTIONS
*        dtel_not_found    = 1
*        name_inconsistent = 2
*        dtel_inconsistent = 3
*        put_failure       = 4
*        put_refused       = 5
*        OTHERS            = 6.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Data element creation failed - { iv_rollname } - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Data element created: { iv_rollname }|.
*
*      " Add/update package information in TADIR
*      " First check if record exists in TADIR
*      SELECT SINGLE * FROM tadir
*        INTO @DATA(ls_tadir)
*        WHERE pgmid    = 'R3TR'
*          AND object   = 'DTEL'
*          AND obj_name = @iv_rollname.
*
*      IF sy-subrc = 0.
*        " If record exists, update it
*        UPDATE tadir SET devclass = @mv_package,
*                         author   = @sy-uname,
*                         masterlang = @sy-langu
*          WHERE pgmid    = 'R3TR'
*            AND object   = 'DTEL'
*            AND obj_name = @iv_rollname.
*      ELSE.
*        " If record doesn't exist, insert it
*        INSERT INTO tadir VALUES @( VALUE #(
*          pgmid     = 'R3TR'
*          object    = 'DTEL'
*          obj_name  = iv_rollname
*          devclass  = mv_package
*          srcsystem = sy-sysid
*          author    = sy-uname
*          masterlang = sy-langu
*        ) ).
*      ENDIF.
*
*      " Activate data element
*      activate_data_element( iv_rollname ).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD activate_domain.
*    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
*      EXPORTING
*        name        = iv_domname
*      EXCEPTIONS
*        not_found   = 1
*        put_failure = 2
*        OTHERS      = 3.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Domain { iv_domname } activation failed - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Domain { iv_domname } activated.|.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD activate_data_element.
*    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
*      EXPORTING
*        name        = iv_rollname
*      EXCEPTIONS
*        not_found   = 1
*        put_failure = 2
*        OTHERS      = 3.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Data element { iv_rollname } activation failed - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Data element { iv_rollname } activated.|.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD activate_table.
*    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
*      EXPORTING
*        name        = iv_tabname
*      EXCEPTIONS
*        not_found   = 1
*        put_failure = 2
*        OTHERS      = 3.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Table { iv_tabname } activation failed - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Table { iv_tabname } activated.|.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD create_single_table.
*    " Check if table already exists
*    DATA(lv_exists) = table_exists( iv_tabname ).
*
*    " If table exists and force mode not active, display message and exit
*    IF lv_exists = abap_true AND mv_force <> 'X'.
*      WRITE: / |Table already exists: { iv_tabname }|.
*      RETURN.
*    ENDIF.
*
*    " If force mode active and table exists, show info
*    IF lv_exists = abap_true AND mv_force = 'X'.
*      WRITE: / |Updating table: { iv_tabname }...|.
*    ENDIF.
*
*    " Create table
*    DATA: ls_dd02v TYPE dd02v.
*
*    CLEAR ls_dd02v.
*    ls_dd02v-tabname     = iv_tabname.
*    ls_dd02v-ddlanguage  = sy-langu.
*    ls_dd02v-tabclass    = 'TRANSP'.
*    ls_dd02v-mainflag    = 'X'.
*    ls_dd02v-contflag    = 'A'.
*    ls_dd02v-ddtext      = iv_ddtext.
*
*    CALL FUNCTION 'DDIF_TABL_PUT'
*      EXPORTING
*        name              = iv_tabname
*        dd02v_wa          = ls_dd02v
*      TABLES
*        dd03p_tab         = it_fields
*      EXCEPTIONS
*        tabl_not_found    = 1
*        name_inconsistent = 2
*        tabl_inconsistent = 3
*        put_failure       = 4
*        put_refused       = 5
*        OTHERS            = 6.
*
*    IF sy-subrc <> 0.
*      WRITE: / |Error: Table creation failed - { iv_tabname } - { sy-subrc }|.
*    ELSE.
*      WRITE: / |Table created: { iv_tabname }|.
*
*      " Add/update package information in TADIR
*      " First check if record exists in TADIR
*      SELECT SINGLE * FROM tadir
*        INTO @DATA(ls_tadir)
*        WHERE pgmid    = 'R3TR'
*          AND object   = 'TABL'
*          AND obj_name = @iv_tabname.
*
*      IF sy-subrc = 0.
*        " If record exists, update it
*        UPDATE tadir SET devclass = @mv_package,
*                         author   = @sy-uname,
*                         masterlang = @sy-langu
*          WHERE pgmid    = 'R3TR'
*            AND object   = 'TABL'
*            AND obj_name = @iv_tabname.
*      ELSE.
*        " If record doesn't exist, insert it
*        INSERT INTO tadir VALUES @( VALUE #(
*          pgmid     = 'R3TR'
*          object    = 'TABL'
*          obj_name  = iv_tabname
*          devclass  = mv_package
*          srcsystem = sy-sysid
*          author    = sy-uname
*          masterlang = sy-langu
*        ) ).
*      ENDIF.
*
*      " Activate table
*      activate_table( iv_tabname ).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD is_table_type_matched.
*    " Returns true if no table type specified or if matches requested type
*    rv_matched = abap_false.
*
*    IF mv_table_type IS INITIAL OR mv_table_type = iv_type.
*      rv_matched = abap_true.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD create_mara_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MARA|.
*
*    WRITE: / |Creating MARA table...|.
*
*    " Create necessary domains first
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MATNR|
*      iv_datatype = 'CHAR'
*      iv_leng     = 18
*      iv_ddtext   = 'Material Number'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MTART|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Material Type'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MAKTX|
*      iv_datatype = 'CHAR'
*      iv_leng     = 40
*      iv_ddtext   = 'Material Description'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MEINS|
*      iv_datatype = 'UNIT'
*      iv_leng     = 3
*      iv_ddtext   = 'Base Unit of Measure'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MATKL|
*      iv_datatype = 'CHAR'
*      iv_leng     = 9
*      iv_ddtext   = 'Material Group'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }ERNAM|
*      iv_datatype = 'CHAR'
*      iv_leng     = 12
*      iv_ddtext   = 'Created By'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }ERDAT|
*      iv_datatype = 'DATS'
*      iv_leng     = 8
*      iv_ddtext   = 'Created On'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }AENAM|
*      iv_datatype = 'CHAR'
*      iv_leng     = 12
*      iv_ddtext   = 'Last Changed By'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }AEDAT|
*      iv_datatype = 'DATS'
*      iv_leng     = 8
*      iv_ddtext   = 'Last Changed On'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }BISCUIT_TYPE|
*      iv_datatype = 'CHAR'
*      iv_leng     = 10
*      iv_ddtext   = 'Biscuit Type'
*    ).
*
*    " Then create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MATNR|
*      iv_domname   = |{ mv_dom_prefix }MATNR|
*      iv_reptext   = 'Material Number'
*      iv_scrtext_s = 'Mat.No'
*      iv_scrtext_m = 'Material No.'
*      iv_scrtext_l = 'Material Number'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MTART|
*      iv_domname   = |{ mv_dom_prefix }MTART|
*      iv_reptext   = 'Material Type'
*      iv_scrtext_s = 'Mat.Type'
*      iv_scrtext_m = 'Material Type'
*      iv_scrtext_l = 'Material Type'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MAKTX|
*      iv_domname   = |{ mv_dom_prefix }MAKTX|
*      iv_reptext   = 'Material Description'
*      iv_scrtext_s = 'Mat.Desc'
*      iv_scrtext_m = 'Material Desc.'
*      iv_scrtext_l = 'Material Description'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MEINS|
*      iv_domname   = |{ mv_dom_prefix }MEINS|
*      iv_reptext   = 'Base Unit of Measure'
*      iv_scrtext_s = 'Base UoM'
*      iv_scrtext_m = 'Base UoM'
*      iv_scrtext_l = 'Base Unit of Measure'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MATKL|
*      iv_domname   = |{ mv_dom_prefix }MATKL|
*      iv_reptext   = 'Material Group'
*      iv_scrtext_s = 'Mat.Group'
*      iv_scrtext_m = 'Material Group'
*      iv_scrtext_l = 'Material Group'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }ERNAM|
*      iv_domname   = |{ mv_dom_prefix }ERNAM|
*      iv_reptext   = 'Created By'
*      iv_scrtext_s = 'Created By'
*      iv_scrtext_m = 'Created By'
*      iv_scrtext_l = 'Created By'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }ERDAT|
*      iv_domname   = |{ mv_dom_prefix }ERDAT|
*      iv_reptext   = 'Created On'
*      iv_scrtext_s = 'Cre.Date'
*      iv_scrtext_m = 'Creation Date'
*      iv_scrtext_l = 'Creation Date'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }AENAM|
*      iv_domname   = |{ mv_dom_prefix }AENAM|
*      iv_reptext   = 'Last Changed By'
*      iv_scrtext_s = 'Ch.By'
*      iv_scrtext_m = 'Changed By'
*      iv_scrtext_l = 'Last Changed By'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }AEDAT|
*      iv_domname   = |{ mv_dom_prefix }AEDAT|
*      iv_reptext   = 'Last Changed On'
*      iv_scrtext_s = 'Ch.Date'
*      iv_scrtext_m = 'Changed Date'
*      iv_scrtext_l = 'Last Changed On'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }BISCUIT_TYPE|
*      iv_domname   = |{ mv_dom_prefix }BISCUIT_TYPE|
*      iv_reptext   = 'Biscuit Type'
*      iv_scrtext_s = 'Bisc.Type'
*      iv_scrtext_m = 'Biscuit Type'
*      iv_scrtext_l = 'Biscuit Type'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'   " Standard SAP data element
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MATNR field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATNR'
*      position   = 2
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }MATNR|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 18
*      inttype    = 'C'
*      intlen     = 36
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MTART field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MTART'
*      position   = 3
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MTART|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 4
*      inttype    = 'C'
*      intlen     = 8
*    ) TO lt_fields.
*
*    " MAKTX field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MAKTX'
*      position   = 4
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MAKTX|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 40
*      inttype    = 'C'
*      intlen     = 80
*    ) TO lt_fields.
*
*    " MEINS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MEINS'
*      position   = 5
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MEINS|
*      ddlanguage = sy-langu
*      datatype   = 'UNIT'
*      leng       = 3
*      inttype    = 'C'
*      intlen     = 6
*    ) TO lt_fields.
*
*    " MATKL field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATKL'
*      position   = 6
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MATKL|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 9
*      inttype    = 'C'
*      intlen     = 18
*    ) TO lt_fields.
*
*    " ERNAM field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'ERNAM'
*      position   = 7
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }ERNAM|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 12
*      inttype    = 'C'
*      intlen     = 24
*    ) TO lt_fields.
*
*    " ERDAT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'ERDAT'
*      position   = 8
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }ERDAT|
*      ddlanguage = sy-langu
*      datatype   = 'DATS'
*      leng       = 8
*      inttype    = 'D'
*      intlen     = 16
*    ) TO lt_fields.
*
*    " AENAM field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'AENAM'
*      position   = 9
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }AENAM|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 12
*      inttype    = 'C'
*      intlen     = 24
*    ) TO lt_fields.
*
*    " AEDAT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'AEDAT'
*      position   = 10
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }AEDAT|
*      ddlanguage = sy-langu
*      datatype   = 'DATS'
*      leng       = 8
*      inttype    = 'D'
*      intlen     = 16
*    ) TO lt_fields.
*
*    " Biscuit type specific field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = |{ mv_prefix }BISCUIT_TYPE|
*      position   = 11
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }BISCUIT_TYPE|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 10
*      inttype    = 'C'
*      intlen     = 20
*    ) TO lt_fields.
*
*    " Create the table
*    create_single_table(
*      iv_tabname = lv_tabname
*      iv_ddtext  = 'Material Master Data (MARA)'
*      it_fields  = lt_fields
*    ).
*  ENDMETHOD.
*
*  METHOD create_marc_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MARC|.
*
*    WRITE: / |Creating MARC table...|.
*
*    " Create necessary domains
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }WERKS|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Plant'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }RECIPE_NO|
*      iv_datatype = 'CHAR'
*      iv_leng     = 10
*      iv_ddtext   = 'Biscuit Recipe Number'
*    ).
*
*    " Create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }WERKS|
*      iv_domname   = |{ mv_dom_prefix }WERKS|
*      iv_reptext   = 'Plant'
*      iv_scrtext_s = 'Plant'
*      iv_scrtext_m = 'Plant'
*      iv_scrtext_l = 'Plant'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }RECIPE_NO|
*      iv_domname   = |{ mv_dom_prefix }RECIPE_NO|
*      iv_reptext   = 'Recipe Number'
*      iv_scrtext_s = 'Recipe No'
*      iv_scrtext_m = 'Recipe Number'
*      iv_scrtext_l = 'Biscuit Recipe Number'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MATNR field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATNR'
*      position   = 2
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }MATNR|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 18
*      inttype    = 'C'
*      intlen     = 36
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " WERKS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'WERKS'
*      position   = 3
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }WERKS|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 4
*      inttype    = 'C'
*      intlen     = 8
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " Biscuit recipe number field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = |{ mv_prefix }RECIPE_NO|
*      position   = 4
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }RECIPE_NO|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 10
*      inttype    = 'C'
*      intlen     = 20
*    ) TO lt_fields.
*
*    " Create the table
*    create_single_table(
*      iv_tabname = lv_tabname
*      iv_ddtext  = 'Plant Data for Material (MARC)'
*      it_fields  = lt_fields
*    ).
*  ENDMETHOD.
*
*  METHOD create_mard_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MARD|.
*
*    WRITE: / |Creating MARD table...|.
*
*    " Create necessary domains
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }LGORT|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Storage Location'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }LABST|
*      iv_datatype = 'QUAN'
*      iv_leng     = 13
*      iv_ddtext   = 'Stock Quantity'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }SHELF_LIFE|
*      iv_datatype = 'NUMC'
*      iv_leng     = 3
*      iv_ddtext   = 'Shelf Life in Days'
*    ).
*
*    " Create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }LGORT|
*      iv_domname   = |{ mv_dom_prefix }LGORT|
*      iv_reptext   = 'Storage Location'
*      iv_scrtext_s = 'Stor.Loc'
*      iv_scrtext_m = 'Storage Loc.'
*      iv_scrtext_l = 'Storage Location'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }LABST|
*      iv_domname   = |{ mv_dom_prefix }LABST|
*      iv_reptext   = 'Stock Quantity'
*      iv_scrtext_s = 'Stock Qty'
*      iv_scrtext_m = 'Stock Quantity'
*      iv_scrtext_l = 'Stock Quantity'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }SHELF_LIFE|
*      iv_domname   = |{ mv_dom_prefix }SHELF_LIFE|
*      iv_reptext   = 'Shelf Life'
*      iv_scrtext_s = 'ShelfLife'
*      iv_scrtext_m = 'Shelf Life'
*      iv_scrtext_l = 'Shelf Life in Days'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MATNR field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATNR'
*      position   = 2
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }MATNR|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 18
*      inttype    = 'C'
*      intlen     = 36
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " WERKS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'WERKS'
*      position   = 3
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }WERKS|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 4
*      inttype    = 'C'
*      intlen     = 8
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " LGORT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'LGORT'
*      position   = 4
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }LGORT|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 4
*      inttype    = 'C'
*      intlen     = 8
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " LABST field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'LABST'
*      position   = 5
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }LABST|
*      ddlanguage = sy-langu
*      datatype   = 'QUAN'
*      leng       = 13
*      inttype    = 'P'
*      intlen     = 7
*    ) TO lt_fields.
*
*    " Shelf Life - Biscuit/Restaurant specific
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = |{ mv_prefix }SHELF_LIFE|
*      position   = 6
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }SHELF_LIFE|
*      ddlanguage = sy-langu
*      datatype   = 'NUMC'
*      leng       = 3
*      inttype    = 'N'
*      intlen     = 6
*    ) TO lt_fields.
*
*    " Create the table
*    create_single_table(
*      iv_tabname = lv_tabname
*      iv_ddtext  = 'Storage Location Data for Material (MARD)'
*      it_fields  = lt_fields
*    ).
*  ENDMETHOD.
*
*  METHOD create_makt_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MAKT|.
*
*    WRITE: / |Creating MAKT table...|.
*
*    " Create necessary domains
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }SPRAS|
*      iv_datatype = 'LANG'
*      iv_leng     = 1
*      iv_ddtext   = 'Language Key'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }MENU_NAME|
*      iv_datatype = 'CHAR'
*      iv_leng     = 30
*      iv_ddtext   = 'Menu Name for Restaurant'
*    ).
*
*    " Create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }SPRAS|
*      iv_domname   = |{ mv_dom_prefix }SPRAS|
*      iv_reptext   = 'Language Key'
*      iv_scrtext_s = 'Language'
*      iv_scrtext_m = 'Language Key'
*      iv_scrtext_l = 'Language Key'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }MENU_NAME|
*      iv_domname   = |{ mv_dom_prefix }MENU_NAME|
*      iv_reptext   = 'Menu Name'
*      iv_scrtext_s = 'Menu Name'
*      iv_scrtext_m = 'Menu Name'
*      iv_scrtext_l = 'Menu Name for Restaurant'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MATNR field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATNR'
*      position   = 2
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }MATNR|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 18
*      inttype    = 'C'
*      intlen     = 36
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " SPRAS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'SPRAS'
*      position   = 3
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }SPRAS|
*      ddlanguage = sy-langu
*      datatype   = 'LANG'
*      leng       = 1
*      inttype    = 'C'
*      intlen     = 2
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MAKTX field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MAKTX'
*      position   = 4
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MAKTX|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 40
*      inttype    = 'C'
*      intlen     = 80
*    ) TO lt_fields.
*
*    " Menu name for restaurant
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = |{ mv_prefix }MENU_NAME|
*      position   = 5
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }MENU_NAME|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 30
*      inttype    = 'C'
*      intlen     = 60
*    ) TO lt_fields.
*
*    " Create the table
*    create_single_table(
*      iv_tabname = lv_tabname
*      iv_ddtext  = 'Material Descriptions (MAKT)'
*      it_fields  = lt_fields
*    ).
*  ENDMETHOD.
*
*  METHOD create_mbew_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MBEW|.
*
*    WRITE: / |Creating MBEW table...|.
*
*    " Create necessary domains
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }BWKEY|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Valuation Area'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }STPRS|
*      iv_datatype = 'CURR'
*      iv_leng     = 11
*      iv_ddtext   = 'Standard Price'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }WAERS|
*      iv_datatype = 'CUKY'
*      iv_leng     = 5
*      iv_ddtext   = 'Currency'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }PROD_COST|
*      iv_datatype = 'CURR'
*      iv_leng     = 11
*      iv_ddtext   = 'Production Cost'
*    ).
*
*    " Create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }BWKEY|
*      iv_domname   = |{ mv_dom_prefix }BWKEY|
*      iv_reptext   = 'Valuation Area'
*      iv_scrtext_s = 'Val.Area'
*      iv_scrtext_m = 'Valuation Area'
*      iv_scrtext_l = 'Valuation Area'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }STPRS|
*      iv_domname   = |{ mv_dom_prefix }STPRS|
*      iv_reptext   = 'Standard Price'
*      iv_scrtext_s = 'Std.Price'
*      iv_scrtext_m = 'Standard Price'
*      iv_scrtext_l = 'Standard Price'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }WAERS|
*      iv_domname   = |{ mv_dom_prefix }WAERS|
*      iv_reptext   = 'Currency'
*      iv_scrtext_s = 'Currency'
*      iv_scrtext_m = 'Currency'
*      iv_scrtext_l = 'Currency'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }PROD_COST|
*      iv_domname   = |{ mv_dom_prefix }PROD_COST|
*      iv_reptext   = 'Production Cost'
*      iv_scrtext_s = 'Prod.Cost'
*      iv_scrtext_m = 'Production Cost'
*      iv_scrtext_l = 'Production Cost for Biscuit/Recipe'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " MATNR field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MATNR'
*      position   = 2
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }MATNR|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 18
*      inttype    = 'C'
*      intlen     = 36
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " BWKEY field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'BWKEY'
*      position   = 3
*      keyflag    = 'X'
*      rollname   = |{ mv_elem_prefix }BWKEY|
*      ddlanguage = sy-langu
*      datatype   = 'CHAR'
*      leng       = 4
*      inttype    = 'C'
*      intlen     = 8
*      mandatory  = 'X'
*    ) TO lt_fields.
*
*    " STPRS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'STPRS'
*      position   = 4
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }STPRS|
*      ddlanguage = sy-langu
*      datatype   = 'CURR'
*      leng       = 11
*      inttype    = 'P'
*      intlen     = 6
*      decimals   = 2
*    ) TO lt_fields.
*
*    " WAERS field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'WAERS'
*      position   = 5
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }WAERS|
*      ddlanguage = sy-langu
*      datatype   = 'CUKY'
*      leng       = 5
*      inttype    = 'C'
*      intlen     = 10
*    ) TO lt_fields.
*
*    " Production cost for biscuit/recipe
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = |{ mv_prefix }PROD_COST|
*      position   = 6
*      keyflag    = ''
*      rollname   = |{ mv_elem_prefix }PROD_COST|
*      ddlanguage = sy-langu
*      datatype   = 'CURR'
*      leng       = 11
*      inttype    = 'P'
*      intlen     = 6
*      decimals   = 2
*    ) TO lt_fields.
*
*    " Create the table
*    create_single_table(
*      iv_tabname = lv_tabname
*      iv_ddtext  = 'Material Valuation (MBEW)'
*      it_fields  = lt_fields
*    ).
*  ENDMETHOD.
*
*  METHOD create_mvke_table.
*    DATA: lt_fields  TYPE TABLE OF dd03p,
*          lv_tabname TYPE dd02v-tabname.
*
*    lv_tabname = |{ mv_prefix }MVKE|.
*
*    WRITE: / |Creating MVKE table...|.
*
*    " Create necessary domains
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }VKORG|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Sales Organization'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }VTWEG|
*      iv_datatype = 'CHAR'
*      iv_leng     = 2
*      iv_ddtext   = 'Distribution Channel'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }DWERK|
*      iv_datatype = 'CHAR'
*      iv_leng     = 4
*      iv_ddtext   = 'Delivering Plant'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }VERPR|
*      iv_datatype = 'CURR'
*      iv_leng     = 11
*      iv_ddtext   = 'Sales Price'
*    ).
*
*    check_and_create_domain(
*      iv_domname  = |{ mv_dom_prefix }SPECIAL_OFFER|
*      iv_datatype = 'CHAR'
*      iv_leng     = 1
*      iv_ddtext   = 'Special Offer Indicator'
*    ).
*
*    " Create data elements
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }VKORG|
*      iv_domname   = |{ mv_dom_prefix }VKORG|
*      iv_reptext   = 'Sales Organization'
*      iv_scrtext_s = 'Sales Org'
*      iv_scrtext_m = 'Sales Org.'
*      iv_scrtext_l = 'Sales Organization'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }VTWEG|
*      iv_domname   = |{ mv_dom_prefix }VTWEG|
*      iv_reptext   = 'Distribution Channel'
*      iv_scrtext_s = 'Distr.Ch.'
*      iv_scrtext_m = 'Distr.Channel'
*      iv_scrtext_l = 'Distribution Channel'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }DWERK|
*      iv_domname   = |{ mv_dom_prefix }DWERK|
*      iv_reptext   = 'Delivering Plant'
*      iv_scrtext_s = 'Del.Plant'
*      iv_scrtext_m = 'Deliv.Plant'
*      iv_scrtext_l = 'Delivering Plant'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }VERPR|
*      iv_domname   = |{ mv_dom_prefix }VERPR|
*      iv_reptext   = 'Sales Price'
*      iv_scrtext_s = 'Sales Pr.'
*      iv_scrtext_m = 'Sales Price'
*      iv_scrtext_l = 'Sales Price'
*    ).
*
*    check_and_create_data_element(
*      iv_rollname  = |{ mv_elem_prefix }SPECIAL_OFFER|
*      iv_domname   = |{ mv_dom_prefix }SPECIAL_OFFER|
*      iv_reptext   = 'Special Offer'
*      iv_scrtext_s = 'Spec.Offer'
*      iv_scrtext_m = 'Special Offer'
*      iv_scrtext_l = 'Special Offer Indicator'
*    ).
*
*    " Create table fields
*    " MANDT field
*    APPEND VALUE #(
*      tabname    = lv_tabname
*      fieldname  = 'MANDT'
*      position   = 1
*      keyflag    = 'X'
*      datatype   = 'CLNT'
*      leng       = 3
*      ddlanguage = sy-langu
*      inttype    = 'C'
*      intlen     = 3
*      rollname   = 'MANDT'
*      ddtext     = 'Client'
*      notnull    = 'X'
*      mandatory  = 'X'
*    ).
*
*  ENDMETHOD.
