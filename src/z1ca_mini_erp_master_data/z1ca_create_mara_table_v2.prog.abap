*&---------------------------------------------------------------------*
*& Report Z1CA_CREATE_MARA_TABLE_V2
*&---------------------------------------------------------------------*
*& This program creates a Material Master (MARA) table and its components
*& using the Z1CA_CL_TABLE_CREATE_MVC_V2 class with MVC pattern
*&---------------------------------------------------------------------*
REPORT z1ca_create_mara_table_v2.

*----------------------------------------------------------------------*
* Selection Screen Parameters
*----------------------------------------------------------------------*
PARAMETERS:
  p_pack   TYPE devclass DEFAULT 'Z1CA_MINI_ERP_MASTER_DATA',  " Package name
  p_prefix TYPE string   DEFAULT 'Z1CA_',         " Prefix for all objects
  p_force  TYPE c        DEFAULT 'X',             " Force recreation if exists
  p_tabnam TYPE tabname  DEFAULT 'Z1CA_MARA',     " Material Master table name
  p_tabdsc TYPE ddtext   DEFAULT 'Z1CA Material Master Table'. " Table description

*----------------------------------------------------------------------*
* Start of main program
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Prepare domains for Material Master
  DATA(lt_domains) = VALUE z1ca_cl_table_create_mvc_v2=>ty_domain_tab(
    " Material Number domain
    ( domname    = |{ p_prefix }D_MATNR|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 40
      ddtext     = 'Material Number'
      entitytab  = p_tabnam )

    " Material Type domain
    ( domname    = |{ p_prefix }D_MTART|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 4
      ddtext     = 'Material Type'
      entitytab  = p_tabnam )

    " Material Description domain
    ( domname    = |{ p_prefix }D_MAKTX|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 40
      ddtext     = 'Material Description'
      entitytab  = p_tabnam )

    " Base Unit of Measure domain
    ( domname    = |{ p_prefix }D_MEINS|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 3
      ddtext     = 'Base Unit of Measure'
      entitytab  = p_tabnam )

    " Material Group domain
    ( domname    = |{ p_prefix }D_MATKL|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 9
      ddtext     = 'Material Group'
      entitytab  = p_tabnam )

    " Date domain (for creation/change dates)
    ( domname    = |{ p_prefix }D_DATE|
      ddlanguage = sy-langu
      datatype   = 'DATS'
      leng       = 8
      ddtext     = 'Date'
      entitytab  = p_tabnam )

    " Time domain
    ( domname    = |{ p_prefix }D_TIME|
      ddlanguage = sy-langu
      datatype   = 'TIMS'
      leng       = 6
      ddtext     = 'Time'
      entitytab  = p_tabnam )

    " Flag domain (for deletion flag etc.)
    ( domname    = |{ p_prefix }D_FLAG|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 1
      ddtext     = 'Flag (X=True)'
      entitytab  = p_tabnam )

    " User domain (for user who created/changed)
    ( domname    = |{ p_prefix }D_USER|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 12
      ddtext     = 'SAP Username'
      entitytab  = p_tabnam )

    " Weight domain
    ( domname    = |{ p_prefix }D_WEIGHT|
      ddlanguage = sy-langu
      datatype   = 'QUAN'
      leng       = 13
      decimals   = 3
      ddtext     = 'Weight'
      entitytab  = p_tabnam )
  ).

  " Prepare data elements for Material Master fields
  DATA(lt_elements) = VALUE z1ca_cl_table_create_mvc_v2=>ty_element_tab(
    " Material Number data element
    ( rollname    = |{ p_prefix }E_MATNR|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_MATNR|
      entitytab   = p_tabnam
      scrtext_s   = 'Mat.Number'
      scrtext_m   = 'Material Number'
      scrtext_l   = 'Material Number'
      reptext     = 'Material Number'
      ddtext      = 'Material Number' )

    " Material Type data element
    ( rollname    = |{ p_prefix }E_MTART|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_MTART|
      entitytab   = p_tabnam
      scrtext_s   = 'Mat.Type'
      scrtext_m   = 'Material Type'
      scrtext_l   = 'Material Type'
      reptext     = 'Material Type'
      ddtext      = 'Material Type' )

    " Material Description data element
    ( rollname    = |{ p_prefix }E_MAKTX|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_MAKTX|
      entitytab   = p_tabnam
      scrtext_s   = 'Description'
      scrtext_m   = 'Material Desc.'
      scrtext_l   = 'Material Description'
      reptext     = 'Material Description'
      ddtext      = 'Material Description' )

    " Base Unit of Measure data element
    ( rollname    = |{ p_prefix }E_MEINS|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_MEINS|
      entitytab   = p_tabnam
      scrtext_s   = 'Base UoM'
      scrtext_m   = 'Base Unit'
      scrtext_l   = 'Base Unit of Measure'
      reptext     = 'Base Unit of Measure'
      ddtext      = 'Base Unit of Measure' )

    " Material Group data element
    ( rollname    = |{ p_prefix }E_MATKL|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_MATKL|
      entitytab   = p_tabnam
      scrtext_s   = 'Mat.Group'
      scrtext_m   = 'Material Group'
      scrtext_l   = 'Material Group'
      reptext     = 'Material Group'
      ddtext      = 'Material Group' )

    " Creation Date data element
    ( rollname    = |{ p_prefix }E_ERDAT|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_DATE|
      entitytab   = p_tabnam
      scrtext_s   = 'Creat.Date'
      scrtext_m   = 'Creation Date'
      scrtext_l   = 'Creation Date'
      reptext     = 'Creation Date'
      ddtext      = 'Date material was created' )

    " Creation Time data element
    ( rollname    = |{ p_prefix }E_ERZET|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_TIME|
      entitytab   = p_tabnam
      scrtext_s   = 'Creat.Time'
      scrtext_m   = 'Creation Time'
      scrtext_l   = 'Creation Time'
      reptext     = 'Creation Time'
      ddtext      = 'Time material was created' )

    " Created By data element
    ( rollname    = |{ p_prefix }E_ERNAM|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_USER|
      entitytab   = p_tabnam
      scrtext_s   = 'Created By'
      scrtext_m   = 'Created By'
      scrtext_l   = 'Created By User'
      reptext     = 'Created By'
      ddtext      = 'User who created the material' )

    " Last Change Date data element
    ( rollname    = |{ p_prefix }E_LAEDA|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_DATE|
      entitytab   = p_tabnam
      scrtext_s   = 'Chng.Date'
      scrtext_m   = 'Change Date'
      scrtext_l   = 'Last Change Date'
      reptext     = 'Last Change Date'
      ddtext      = 'Date material was last changed' )

    " Last Change Time data element
    ( rollname    = |{ p_prefix }E_LAEZT|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_TIME|
      entitytab   = p_tabnam
      scrtext_s   = 'Chng.Time'
      scrtext_m   = 'Change Time'
      scrtext_l   = 'Last Change Time'
      reptext     = 'Last Change Time'
      ddtext      = 'Time material was last changed' )

    " Changed By data element
    ( rollname    = |{ p_prefix }E_AENAM|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_USER|
      entitytab   = p_tabnam
      scrtext_s   = 'Changed By'
      scrtext_m   = 'Changed By'
      scrtext_l   = 'Changed By User'
      reptext     = 'Changed By'
      ddtext      = 'User who last changed the material' )

    " Deletion Flag data element
    ( rollname    = |{ p_prefix }E_LVORM|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_FLAG|
      entitytab   = p_tabnam
      scrtext_s   = 'Del.Flag'
      scrtext_m   = 'Deletion Flag'
      scrtext_l   = 'Deletion Flag'
      reptext     = 'Deletion Flag'
      ddtext      = 'Material Marked for Deletion' )

    " Gross Weight data element
    ( rollname    = |{ p_prefix }E_BRGEW|
      ddlanguage  = sy-langu
      domname     = |{ p_prefix }D_WEIGHT|
      entitytab   = p_tabnam
      scrtext_s   = 'Gross Wt'
      scrtext_m   = 'Gross Weight'
      scrtext_l   = 'Gross Weight'
      reptext     = 'Gross Weight'
      ddtext      = 'Gross Weight of Material' )
  ).

  " Prepare table fields for Material Master
  DATA(lt_fields) = VALUE z1ca_cl_table_create_mvc_v2=>ty_fields_tab(
    " Material Number (Primary Key)
    ( fieldname   = 'MATNR'
      tabname     = p_tabnam
      position    = 1
      keyflag     = 'X'
      rollname    = |{ p_prefix }E_MATNR|
      adminfield  = '0'
      notnull     = 'X'
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 18
      ddtext      = 'Material Number' )

    " Material Type
    ( fieldname   = 'MTART'
      tabname     = p_tabnam
      position    = 2
      rollname    = |{ p_prefix }E_MTART|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 4
      ddtext      = 'Material Type' )

    " Material Description
    ( fieldname   = 'MAKTX'
      tabname     = p_tabnam
      position    = 3
      rollname    = |{ p_prefix }E_MAKTX|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 40
      ddtext      = 'Material Description' )

    " Base Unit of Measure
    ( fieldname   = 'MEINS'
      tabname     = p_tabnam
      position    = 4
      rollname    = |{ p_prefix }E_MEINS|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 3
      ddtext      = 'Base Unit of Measure' )

    " Material Group
    ( fieldname   = 'MATKL'
      tabname     = p_tabnam
      position    = 5
      rollname    = |{ p_prefix }E_MATKL|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 9
      ddtext      = 'Material Group' )

    " Gross Weight
    ( fieldname   = 'BRGEW'
      tabname     = p_tabnam
      position    = 6
      rollname    = |{ p_prefix }E_BRGEW|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'QUAN'
      leng        = 13
      decimals    = 3
      ddtext      = 'Gross Weight of Material' )

    " Deletion Flag
    ( fieldname   = 'LVORM'
      tabname     = p_tabnam
      position    = 7
      rollname    = |{ p_prefix }E_LVORM|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 1
      ddtext      = 'Material Marked for Deletion' )

    " Creation Date
    ( fieldname   = 'ERDAT'
      tabname     = p_tabnam
      position    = 8
      rollname    = |{ p_prefix }E_ERDAT|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'DATS'
      leng        = 8
      ddtext      = 'Date material was created' )

    " Creation Time
    ( fieldname   = 'ERZET'
      tabname     = p_tabnam
      position    = 9
      rollname    = |{ p_prefix }E_ERZET|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'TIMS'
      leng        = 6
      ddtext      = 'Time material was created' )

    " Created By
    ( fieldname   = 'ERNAM'
      tabname     = p_tabnam
      position    = 10
      rollname    = |{ p_prefix }E_ERNAM|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 12
      ddtext      = 'User who created the material' )

    " Last Change Date
    ( fieldname   = 'LAEDA'
      tabname     = p_tabnam
      position    = 11
      rollname    = |{ p_prefix }E_LAEDA|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'DATS'
      leng        = 8
      ddtext      = 'Date material was last changed' )

    " Last Change Time
    ( fieldname   = 'LAEZT'
      tabname     = p_tabnam
      position    = 12
      rollname    = |{ p_prefix }E_LAEZT|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'TIMS'
      leng        = 6
      ddtext      = 'Time material was last changed' )

    " Changed By
    ( fieldname   = 'AENAM'
      tabname     = p_tabnam
      position    = 13
      rollname    = |{ p_prefix }E_AENAM|
      adminfield  = '0'
      notnull     = ' '
      ddlanguage  = sy-langu
      datatype    = 'CHAR'
      leng        = 12
      ddtext      = 'User who last changed the material' )
  ).

  " Create an instance of the table creator class with all parameters in the constructor
  DATA(lo_table_creator) = NEW z1ca_cl_table_create_mvc_v2(
    VALUE #(
      package     = p_pack
      prefix      = p_prefix
      force_flag  = p_force
      table_name  = p_tabnam
      table_descr = p_tabdsc
      domains     = lt_domains
      elements    = lt_elements
      fields      = lt_fields
    )
  ).

  " Execute the table creation process
  lo_table_creator->c_create_table( ).

  " Check if creation was successful and display final message
  IF lo_table_creator->v_get_result( ) = abap_true.
    WRITE: / '==============================================='.
    WRITE: / |{ p_tabnam } table creation completed successfully!|.
    WRITE: / 'Table is now ready for use in Material Management'.
    WRITE: / '==============================================='.
  ELSE.
    WRITE: / '==============================================='.
    WRITE: / |{ p_tabnam } table creation had errors.|.
    WRITE: / 'Please check the messages above for details.'.
    WRITE: / '==============================================='.
  ENDIF.
