*&---------------------------------------------------------------------*
*& Report Z1CA_CREATE_MARA_TABLE
*&---------------------------------------------------------------------*
*& Description: This program creates the MARA (Material Master) table
*& with customizable prefix using global Z1CA_CL_TABLE_CREATE class
*& Uses structure-based parameter passing
*&---------------------------------------------------------------------*
REPORT z1ca_create_mara_table.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS: p_pack  TYPE devclass DEFAULT 'Z1CA_MINI_ERP_MASTER_DATA' OBLIGATORY,
            p_pref  TYPE string  DEFAULT 'Z1CA_' OBLIGATORY,          " Customizable prefix
            p_force TYPE c AS CHECKBOX DEFAULT ' '.                    " Force recreate objects

*&---------------------------------------------------------------------*
*& MARA Creator Class Definition
*&---------------------------------------------------------------------*
CLASS lcl_mara_creator DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_package    TYPE devclass
                           iv_prefix      TYPE string
                           iv_force_flag  TYPE c,
      create_mara_table.

  PRIVATE SECTION.
    DATA:
      mo_creator TYPE REF TO z1ca_cl_table_create,  " Reference to global helper class
      mv_prefix  TYPE string,
      mv_tabname TYPE tabname.
ENDCLASS.

*&---------------------------------------------------------------------*
*& MARA Creator Class Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mara_creator IMPLEMENTATION.
  METHOD constructor.
    " Create an instance of the global helper class using structure
    DATA: ls_params TYPE z1ca_cl_table_create=>ty_constructor_params.

    " Fill structure
    ls_params-package = iv_package.
    ls_params-prefix = iv_prefix.
    ls_params-force_flag = iv_force_flag.

    " Create instance with structure
    CREATE OBJECT mo_creator
      EXPORTING
        is_params = ls_params.

    " Store prefix for later use
    mv_prefix  = iv_prefix.
    mv_tabname = |{ iv_prefix }MARA|.

    mo_creator->v_message( |Table name: { mv_tabname }| ).
  ENDMETHOD.

  METHOD create_mara_table.
    " Write header
    mo_creator->v_message( '============================================' ).
    mo_creator->v_message( |    Creating { mv_tabname } Table         | ).
    mo_creator->v_message( '============================================' ).

    DATA: lt_fields TYPE z1ca_cl_table_create=>ty_fields_tab,
          ls_domain TYPE dd01v,
          ls_element TYPE dd04v,
          ls_table TYPE dd02v.

    DATA: lv_dom_prefix  TYPE string,
          lv_elem_prefix TYPE string.

    lv_dom_prefix  = |{ mv_prefix }D_|.
    lv_elem_prefix = |{ mv_prefix }E_|.

    " Create necessary domains using structure-based approach
    " Domain for MATNR
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }MATNR|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 40.
    ls_domain-ddtext = 'Material Number'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for MTART
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }MTART|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 4.
    ls_domain-ddtext = 'Material Type'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for MAKTX
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }MAKTX|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 40.
    ls_domain-ddtext = 'Material Description'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for MEINS
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }MEINS|.
    ls_domain-datatype = 'UNIT'.
    ls_domain-leng = 3.
    ls_domain-ddtext = 'Base Unit of Measure'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for MATKL
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }MATKL|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 9.
    ls_domain-ddtext = 'Material Group'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for ERNAM
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }ERNAM|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 12.
    ls_domain-ddtext = 'Created By'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for ERDAT
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }ERDAT|.
    ls_domain-datatype = 'DATS'.
    ls_domain-leng = 8.
    ls_domain-ddtext = 'Created On'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for AENAM
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }AENAM|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 12.
    ls_domain-ddtext = 'Last Changed By'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for AEDAT
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }AEDAT|.
    ls_domain-datatype = 'DATS'.
    ls_domain-leng = 8.
    ls_domain-ddtext = 'Last Changed On'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Domain for BISCUIT_TYPE
    CLEAR ls_domain.
    ls_domain-domname = |{ lv_dom_prefix }BISCUIT_TYPE|.
    ls_domain-datatype = 'CHAR'.
    ls_domain-leng = 10.
    ls_domain-ddtext = 'Biscuit Type'.
    ls_domain-ddlanguage = sy-langu.
    mo_creator->c_create_domain( ls_domain ).

    " Then create data elements using structure-based approach
    " Element for MATNR
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }MATNR|.
    ls_element-domname = |{ lv_dom_prefix }MATNR|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Material Number'.
    ls_element-scrtext_s = 'Mat.No'.
    ls_element-scrtext_m = 'Material No.'.
    ls_element-scrtext_l = 'Material Number'.
    ls_element-ddtext = 'Material Number'.
    mo_creator->c_create_element( ls_element ).

    " Element for MTART
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }MTART|.
    ls_element-domname = |{ lv_dom_prefix }MTART|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Material Type'.
    ls_element-scrtext_s = 'Mat.Type'.
    ls_element-scrtext_m = 'Material Type'.
    ls_element-scrtext_l = 'Material Type'.
    ls_element-ddtext = 'Material Type'.
    mo_creator->c_create_element( ls_element ).

    " Element for MAKTX
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }MAKTX|.
    ls_element-domname = |{ lv_dom_prefix }MAKTX|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Material Description'.
    ls_element-scrtext_s = 'Mat.Desc'.
    ls_element-scrtext_m = 'Material Desc.'.
    ls_element-scrtext_l = 'Material Description'.
    ls_element-ddtext = 'Material Description'.
    mo_creator->c_create_element( ls_element ).

    " Element for MEINS
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }MEINS|.
    ls_element-domname = |{ lv_dom_prefix }MEINS|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Base Unit of Measure'.
    ls_element-scrtext_s = 'Base UoM'.
    ls_element-scrtext_m = 'Base UoM'.
    ls_element-scrtext_l = 'Base Unit of Measure'.
    ls_element-ddtext = 'Base Unit of Measure'.
    mo_creator->c_create_element( ls_element ).

    " Element for MATKL
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }MATKL|.
    ls_element-domname = |{ lv_dom_prefix }MATKL|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Material Group'.
    ls_element-scrtext_s = 'Mat.Group'.
    ls_element-scrtext_m = 'Material Group'.
    ls_element-scrtext_l = 'Material Group'.
    ls_element-ddtext = 'Material Group'.
    mo_creator->c_create_element( ls_element ).

    " Element for ERNAM
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }ERNAM|.
    ls_element-domname = |{ lv_dom_prefix }ERNAM|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Created By'.
    ls_element-scrtext_s = 'Created By'.
    ls_element-scrtext_m = 'Created By'.
    ls_element-scrtext_l = 'Created By'.
    ls_element-ddtext = 'Created By'.
    mo_creator->c_create_element( ls_element ).

    " Element for ERDAT
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }ERDAT|.
    ls_element-domname = |{ lv_dom_prefix }ERDAT|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Created On'.
    ls_element-scrtext_s = 'Cre.Date'.
    ls_element-scrtext_m = 'Creation Date'.
    ls_element-scrtext_l = 'Creation Date'.
    ls_element-ddtext = 'Created On'.
    mo_creator->c_create_element( ls_element ).

    " Element for AENAM
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }AENAM|.
    ls_element-domname = |{ lv_dom_prefix }AENAM|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Last Changed By'.
    ls_element-scrtext_s = 'Ch.By'.
    ls_element-scrtext_m = 'Changed By'.
    ls_element-scrtext_l = 'Last Changed By'.
    ls_element-ddtext = 'Last Changed By'.
    mo_creator->c_create_element( ls_element ).

    " Element for AEDAT
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }AEDAT|.
    ls_element-domname = |{ lv_dom_prefix }AEDAT|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Last Changed On'.
    ls_element-scrtext_s = 'Ch.Date'.
    ls_element-scrtext_m = 'Changed Date'.
    ls_element-scrtext_l = 'Last Changed On'.
    ls_element-ddtext = 'Last Changed On'.
    mo_creator->c_create_element( ls_element ).

    " Element for BISCUIT_TYPE
    CLEAR ls_element.
    ls_element-rollname = |{ lv_elem_prefix }BISCUIT_TYPE|.
    ls_element-domname = |{ lv_dom_prefix }BISCUIT_TYPE|.
    ls_element-ddlanguage = sy-langu.
    ls_element-reptext = 'Biscuit Type'.
    ls_element-scrtext_s = 'Bisc.Type'.
    ls_element-scrtext_m = 'Biscuit Type'.
    ls_element-scrtext_l = 'Biscuit Type'.
    ls_element-ddtext = 'Biscuit Type'.
    mo_creator->c_create_element( ls_element ).

    " Create table fields
    " MANDT field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MANDT'
      position   = 1
      keyflag    = 'X'
      datatype   = 'CLNT'
      leng       = 3
      ddlanguage = sy-langu
      inttype    = 'C'
      intlen     = 3
      rollname   = 'MANDT'   " Standard SAP data element
      ddtext     = 'Client'
      notnull    = 'X'
      mandatory  = 'X'
    ) TO lt_fields.

    " MATNR field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MATNR'
      position   = 2
      keyflag    = 'X'
      rollname   = |{ lv_elem_prefix }MATNR|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 18
      inttype    = 'C'
      intlen     = 36
      mandatory  = 'X'
    ) TO lt_fields.

    " MTART field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MTART'
      position   = 3
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }MTART|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 4
      inttype    = 'C'
      intlen     = 8
    ) TO lt_fields.

    " MAKTX field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MAKTX'
      position   = 4
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }MAKTX|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 40
      inttype    = 'C'
      intlen     = 80
    ) TO lt_fields.

    " MEINS field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MEINS'
      position   = 5
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }MEINS|
      ddlanguage = sy-langu
      datatype   = 'UNIT'
      leng       = 3
      inttype    = 'C'
      intlen     = 6
    ) TO lt_fields.

    " MATKL field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'MATKL'
      position   = 6
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }MATKL|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 9
      inttype    = 'C'
      intlen     = 18
    ) TO lt_fields.

    " ERNAM field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'ERNAM'
      position   = 7
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }ERNAM|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 12
      inttype    = 'C'
      intlen     = 24
    ) TO lt_fields.

    " ERDAT field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'ERDAT'
      position   = 8
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }ERDAT|
      ddlanguage = sy-langu
      datatype   = 'DATS'
      leng       = 8
      inttype    = 'D'
      intlen     = 16
    ) TO lt_fields.

    " AENAM field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'AENAM'
      position   = 9
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }AENAM|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 12
      inttype    = 'C'
      intlen     = 24
    ) TO lt_fields.

    " AEDAT field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = 'AEDAT'
      position   = 10
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }AEDAT|
      ddlanguage = sy-langu
      datatype   = 'DATS'
      leng       = 8
      inttype    = 'D'
      intlen     = 16
    ) TO lt_fields.

    " Biscuit type specific field
    APPEND VALUE #(
      tabname    = mv_tabname
      fieldname  = |{ mv_prefix }BISCUIT_TYPE|
      position   = 11
      keyflag    = ''
      rollname   = |{ lv_elem_prefix }BISCUIT_TYPE|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 10
      inttype    = 'C'
      intlen     = 20
    ) TO lt_fields.

    " Create the table using structure-based method
    CLEAR ls_table.
    ls_table-tabname = mv_tabname.
    ls_table-ddlanguage = sy-langu.
    ls_table-tabclass = 'TRANSP'.
    ls_table-mainflag = 'X'.
    ls_table-contflag = 'A'.
    ls_table-ddtext = 'Material Master Data (MARA)'.

    " Create the table using the global helper class
    mo_creator->c_create_table(
      is_table_data = ls_table
      it_fields = lt_fields
    ).

    " Completion message
    mo_creator->v_message( '============================================' ).
    mo_creator->v_message( 'MARA table creation process completed.' ).
    mo_creator->v_message( '============================================' ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Program Start
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  " Start table creator
  NEW lcl_mara_creator(
    iv_package    = p_pack
    iv_prefix     = p_pref
    iv_force_flag = p_force
  )->create_mara_table( ).
