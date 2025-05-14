*&---------------------------------------------------------------------*
*& Report Z1CA_CREATE_MATERIAL_MASTER
*&---------------------------------------------------------------------*
*& Açıklama: Bu program bisküvi üretimi için malzeme master tablolarını
*& Z1CA_ ön eki ile oluşturur (MVP kapsamında)
*&---------------------------------------------------------------------*
REPORT z1ca_create_material_master.

*&---------------------------------------------------------------------*
*& Seçim Ekranı
*&---------------------------------------------------------------------*
PARAMETERS: p_pack  TYPE devclass DEFAULT 'Z1CA_MINI_ERP_MASTER_DATA' OBLIGATORY,
            p_force TYPE c AS CHECKBOX DEFAULT ' '. "Nesneleri zorla yeniden oluştur

*&---------------------------------------------------------------------*
*& Ana Sınıf Tanımı
*&---------------------------------------------------------------------*
CLASS lcl_table_creator DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_package    TYPE devclass
                            iv_force_flag TYPE c,
      create_tables.

  PRIVATE SECTION.
    CONSTANTS:
      c_prefix      TYPE string VALUE 'Z1CA_',
      c_dom_prefix  TYPE string VALUE 'Z1CA_D_',
      c_elem_prefix TYPE string VALUE 'Z1CA_E_'.

    DATA:
      mv_table_name TYPE ddobjname,
      mv_package    TYPE devclass,
      mv_force      TYPE c.

    METHODS:
      create_domains,
      create_data_elements,
      create_table,
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
ENDCLASS.

*&---------------------------------------------------------------------*
*& Ana Sınıf Uygulaması
*&---------------------------------------------------------------------*
CLASS lcl_table_creator IMPLEMENTATION.
  METHOD constructor.
    mv_table_name = |{ c_prefix }MARA|.
    mv_package    = iv_package.
    mv_force      = iv_force_flag.
    WRITE: / |Paket: { mv_package }|.
    IF mv_force = 'X'.
      WRITE: / 'Zorla Yeniden Oluşturma Modu: Aktif'.
    ENDIF.
  ENDMETHOD.

  METHOD create_tables.
    " 1. Önce domainleri oluştur ve aktive et
    create_domains( ).
    WRITE: / '---------------------------------------'.

    " 2. Sonra data elementleri oluştur ve aktive et
    create_data_elements( ).
    WRITE: / '---------------------------------------'.

    " 3. En son tabloyu oluştur ve aktive et
    create_table( ).

    WRITE: / |Tablo oluşturma işlemi tamamlandı: { mv_table_name }|.
  ENDMETHOD.

  METHOD domain_exists.
    " 1. Katalog tablosunda kontrol et
    SELECT SINGLE * FROM dd01l INTO @DATA(ls_dd01l)
      WHERE domname = @iv_domname.

    " Katalogda yoksa kesinlikle mevcut değil
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Aktif olup olmadığını kontrol et
    IF ls_dd01l-as4local <> 'A'. " A = Aktif
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Detaylı bilgileri al
    DATA: ls_dd01v TYPE dd01v.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = iv_domname
      IMPORTING
        dd01v_wa      = ls_dd01v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    " API'den bilgi gelemediyse veya içerik boşsa mevcut değil
    IF sy-subrc <> 0 OR ls_dd01v IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " Tüm kontrollerden geçtiyse domain mevcuttur
    rv_exists = abap_true.
  ENDMETHOD.

  METHOD data_element_exists.
    " 1. Katalog tablosunda kontrol et
    SELECT SINGLE * FROM dd04l INTO @DATA(ls_dd04l)
      WHERE rollname = @iv_rollname.

    " Katalogda yoksa kesinlikle mevcut değil
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Aktif olup olmadığını kontrol et
    IF ls_dd04l-as4local <> 'A'. " A = Aktif
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Detaylı bilgileri al
    DATA: ls_dd04v TYPE dd04v.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = iv_rollname
      IMPORTING
        dd04v_wa      = ls_dd04v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    " API'den bilgi gelemediyse veya içerik boşsa mevcut değil
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " Tüm kontrollerden geçtiyse data element mevcuttur
    rv_exists = abap_true.
  ENDMETHOD.

  METHOD table_exists.
    " 1. Katalog tablosunda kontrol et
    SELECT SINGLE * FROM dd02l INTO @DATA(ls_dd02l)
      WHERE tabname = @iv_tabname.

    " Katalogda yoksa kesinlikle mevcut değil
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 2. Aktif olup olmadığını kontrol et
    IF ls_dd02l-as4local <> 'A'. " A = Aktif
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " 3. Detaylı bilgileri al
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

    " API'den bilgi gelemediyse, içerik boşsa veya alan tanımı yoksa mevcut değil
    IF sy-subrc <> 0 OR ls_dd02v IS INITIAL OR lt_dd03p[] IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    " Tüm kontrollerden geçtiyse tablo mevcuttur
    rv_exists = abap_true.
  ENDMETHOD.

  METHOD check_and_create_domain.
    " Domain zaten var mı kontrol et
    DATA(lv_exists) = domain_exists( iv_domname ).

    " Domain varsa ve zorla oluşturma modu aktif değilse mesaj ver ve çık
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Domain zaten mevcut: { iv_domname }|.
      RETURN.
    ENDIF.

    " Zorla oluşturma modu aktif ve domain varsa önce silmeye çalış
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Domain siliniyor: { iv_domname }...|.

      " Alternatif silme yaklaşımı - ABAP Dictionary API kullanılır
      " SAP 7.52'de doğrudan domain silme fonksiyonu olmadığı için
      " bu metodu atla ve yeniden oluşturmaya devam et
      WRITE: / |Not: SAP 7.52'de doğrudan silme fonksiyonu bulunamadı.|.
      WRITE: / |Mevcut domain üzerine yazılacak.|.
    ENDIF.

    " Domain oluştur
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
      WRITE: / |Hata: Domain oluşturulamadı - { iv_domname } - { sy-subrc }|.
    ELSE.
      WRITE: / |Domain oluşturuldu: { iv_domname }|.

      " TADIR tablosuna paket bilgisini ekle/güncelle
      " Önce TADIR'da kayıt var mı kontrol et
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DOMA'
          AND obj_name = @iv_domname.

      IF sy-subrc = 0.
        " Kayıt varsa update et
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DOMA'
            AND obj_name = @iv_domname.
      ELSE.
        " Kayıt yoksa insert et
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

      IF sy-subrc = 0.
        WRITE: / |Domain { iv_domname } için paket ({ mv_package }) ataması yapıldı.|.
      ELSE.
        WRITE: / |Uyarı: Domain { iv_domname } için paket ataması yapılamadı - TADIR güncellenemedi - { sy-subrc }|.
      ENDIF.

      " Domain'i aktive et
      activate_domain( iv_domname ).
    ENDIF.
  ENDMETHOD.

  METHOD check_and_create_data_element.
    " Data element zaten var mı kontrol et
    DATA(lv_exists) = data_element_exists( iv_rollname ).

    " Data element varsa ve zorla oluşturma modu aktif değilse mesaj ver ve çık
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Data element zaten mevcut: { iv_rollname }|.
      RETURN.
    ENDIF.

    " Zorla oluşturma modu aktif ve data element varsa önce silmeye çalış
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Data element siliniyor: { iv_rollname }...|.

      " Alternatif silme yaklaşımı - SAP 7.52'de doğrudan data element silme fonksiyonu olmadığı için
      " bu metodu atla ve yeniden oluşturmaya devam et
      WRITE: / |Not: SAP 7.52'de doğrudan silme fonksiyonu bulunamadı.|.
      WRITE: / |Mevcut data element üzerine yazılacak.|.
    ENDIF.

    " Data element oluştur
    DATA: ls_dd04v TYPE dd04v,
          lv_scrtext_s TYPE dd04v-scrtext_s,
          lv_scrtext_m TYPE dd04v-scrtext_m,
          lv_scrtext_l TYPE dd04v-scrtext_l,
          lv_reptext   TYPE dd04v-reptext.

    lv_scrtext_s = iv_scrtext_s.
    lv_scrtext_m = iv_scrtext_m.
    lv_scrtext_l = iv_scrtext_l.
    lv_reptext   = iv_reptext.

    CLEAR ls_dd04v.
    ls_dd04v-rollname   = iv_rollname.
    ls_dd04v-ddlanguage = sy-langu.
    ls_dd04v-domname    = iv_domname.
    ls_dd04v-reptext    = lv_reptext.
    ls_dd04v-scrtext_s  = lv_scrtext_s.
    ls_dd04v-scrtext_m  = lv_scrtext_m.
    ls_dd04v-scrtext_l  = lv_scrtext_l.
    ls_dd04v-ddtext     = lv_reptext.  " Short Description için DDTEXT alanını ekledik

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
      WRITE: / |Hata: Data element oluşturulamadı - { iv_rollname } - { sy-subrc }|.
    ELSE.
      WRITE: / |Data element oluşturuldu: { iv_rollname }|.

      " TADIR tablosuna paket bilgisini ekle/güncelle
      " Önce TADIR'da kayıt var mı kontrol et
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'DTEL'
          AND obj_name = @iv_rollname.

      IF sy-subrc = 0.
        " Kayıt varsa update et
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'DTEL'
            AND obj_name = @iv_rollname.
      ELSE.
        " Kayıt yoksa insert et
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

      IF sy-subrc = 0.
        WRITE: / |Data element { iv_rollname } için paket ({ mv_package }) ataması yapıldı.|.
      ELSE.
        WRITE: / |Uyarı: Data element { iv_rollname } için paket ataması yapılamadı - TADIR güncellenemedi - { sy-subrc }|.
      ENDIF.

      " Data element'i aktive et
      activate_data_element( iv_rollname ).
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
      WRITE: / |Hata: Domain { iv_domname } aktive edilemedi - { sy-subrc }|.
    ELSE.
      WRITE: / |Domain { iv_domname } aktive edildi.|.
    ENDIF.
  ENDMETHOD.

  METHOD activate_data_element.
    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name              = iv_rollname
      EXCEPTIONS
        not_found         = 1
        put_failure       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      WRITE: / |Hata: Data element { iv_rollname } aktive edilemedi - { sy-subrc }|.
    ELSE.
      WRITE: / |Data element { iv_rollname } aktive edildi.|.
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
      WRITE: / |Hata: Tablo { iv_tabname } aktive edilemedi - { sy-subrc }|.
    ELSE.
      WRITE: / |Tablo { iv_tabname } aktive edildi.|.
    ENDIF.
  ENDMETHOD.

  METHOD create_domains.
    WRITE: / 'Domainler oluşturuluyor...'.

    " Malzeme numarası domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }MATNR|
      iv_datatype = 'CHAR'
      iv_leng     = 18
      iv_ddtext   = 'Malzeme Numarası'
    ).

    " Malzeme türü domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }MTART|
      iv_datatype = 'CHAR'
      iv_leng     = 4
      iv_ddtext   = 'Malzeme Türü'
    ).

    " Malzeme açıklaması domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }MAKTX|
      iv_datatype = 'CHAR'
      iv_leng     = 40
      iv_ddtext   = 'Malzeme Açıklaması'
    ).

    " Ölçü birimi domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }MEINS|
      iv_datatype = 'UNIT'
      iv_leng     = 3
      iv_ddtext   = 'Ölçü Birimi'
    ).

    " Malzeme grubu domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }MATKL|
      iv_datatype = 'CHAR'
      iv_leng     = 9
      iv_ddtext   = 'Malzeme Grubu'
    ).

    " Oluşturan kullanıcı domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }ERNAM|
      iv_datatype = 'CHAR'
      iv_leng     = 12
      iv_ddtext   = 'Oluşturan Kullanıcı'
    ).

    " Oluşturma tarihi domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }ERDAT|
      iv_datatype = 'DATS'
      iv_leng     = 8
      iv_ddtext   = 'Oluşturma Tarihi'
    ).

    " Değiştiren kullanıcı domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }AENAM|
      iv_datatype = 'CHAR'
      iv_leng     = 12
      iv_ddtext   = 'Değiştiren Kullanıcı'
    ).

    " Değiştirme tarihi domain
    check_and_create_domain(
      iv_domname  = |{ c_dom_prefix }AEDAT|
      iv_datatype = 'DATS'
      iv_leng     = 8
      iv_ddtext   = 'Değiştirme Tarihi'
    ).

    " Program tamamlandıktan sonra tüm data element uzunluklarını güncelle
    WRITE: / 'Tüm data element uzunlukları güncelleniyor...'.

    DATA: lv_pattern TYPE string.
    lv_pattern = c_elem_prefix && '%'.

    " Eski SQL sözdizimi kullanılıyor
    UPDATE dd04l SET scrlen1 = '10',
                    scrlen2 = '20',
                    scrlen3 = '40',
                    headlen = '55'
     WHERE rollname LIKE @lv_pattern.
       " AND as4local = 'A'.

    IF sy-subrc = 0.
      WRITE: / 'Data element uzunlukları güncellendi.'.
      WRITE: / sy-dbcnt, ' kayıt güncellendi.'.
    ELSE.
      WRITE: / 'Uyarı: Data element uzunlukları güncellenemedi.'.
    ENDIF.
  ENDMETHOD.

  METHOD create_data_elements.
    WRITE: / 'Data elementler oluşturuluyor...'.

    DATA: lv_scrtext_s TYPE dd04v-scrtext_s,
          lv_scrtext_m TYPE dd04v-scrtext_m,
          lv_scrtext_l TYPE dd04v-scrtext_l,
          lv_reptext   TYPE dd04v-reptext.

    " Malzeme numarası data element
    lv_reptext   = 'Malzeme No'.         " Kısaltıldı (maksimum 55)
    lv_scrtext_s = 'Mal.No'.             " Kısaltıldı (maksimum 10)
    lv_scrtext_m = 'Malzeme No'.         " Kısaltıldı (maksimum 20)
    lv_scrtext_l = 'Malzeme Numarası'.   " Kısaltıldı (maksimum 40)
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }MATNR|
      iv_domname   = |{ c_dom_prefix }MATNR|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Malzeme türü data element
    lv_reptext   = 'Malzeme Türü'.
    lv_scrtext_s = 'M.Tür'.
    lv_scrtext_m = 'Malzeme Türü'.
    lv_scrtext_l = 'Malzeme Türü'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }MTART|
      iv_domname   = |{ c_dom_prefix }MTART|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Malzeme açıklaması data element
    lv_reptext   = 'Malzeme Açıklaması'.
    lv_scrtext_s = 'Mal.Açk'.
    lv_scrtext_m = 'Malz.Açıklama'.
    lv_scrtext_l = 'Malzeme Açıklaması'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }MAKTX|
      iv_domname   = |{ c_dom_prefix }MAKTX|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Ölçü birimi data element
    lv_reptext   = 'Ölçü Birimi'.
    lv_scrtext_s = 'Ölç.Bir'.
    lv_scrtext_m = 'Ölçü Birimi'.
    lv_scrtext_l = 'Ölçü Birimi'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }MEINS|
      iv_domname   = |{ c_dom_prefix }MEINS|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Malzeme grubu data element
    lv_reptext   = 'Malzeme Grubu'.
    lv_scrtext_s = 'Malz.Grp'.
    lv_scrtext_m = 'Malzeme Grubu'.
    lv_scrtext_l = 'Malzeme Grubu'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }MATKL|
      iv_domname   = |{ c_dom_prefix }MATKL|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Oluşturan kullanıcı data element
    lv_reptext   = 'Oluşturan Kullanıcı'.
    lv_scrtext_s = 'Oluşt.K'.
    lv_scrtext_m = 'Oluşturan K.'.
    lv_scrtext_l = 'Oluşturan Kullanıcı'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }ERNAM|
      iv_domname   = |{ c_dom_prefix }ERNAM|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Oluşturma tarihi data element
    lv_reptext   = 'Oluşturma Tarihi'.
    lv_scrtext_s = 'Olş.Tar'.
    lv_scrtext_m = 'Oluşturma Tar.'.
    lv_scrtext_l = 'Oluşturma Tarihi'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }ERDAT|
      iv_domname   = |{ c_dom_prefix }ERDAT|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Değiştiren kullanıcı data element
    lv_reptext   = 'Değiştiren Kullanıcı'.
    lv_scrtext_s = 'Değ.K'.
    lv_scrtext_m = 'Değiştiren K.'.
    lv_scrtext_l = 'Değiştiren Kullanıcı'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }AENAM|
      iv_domname   = |{ c_dom_prefix }AENAM|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).

    " Değiştirme tarihi data element
    lv_reptext   = 'Değiştirme Tarihi'.
    lv_scrtext_s = 'Değ.Tar'.
    lv_scrtext_m = 'Değiştirme Tar.'.
    lv_scrtext_l = 'Değiştirme Tarihi'.
    check_and_create_data_element(
      iv_rollname  = |{ c_elem_prefix }AEDAT|
      iv_domname   = |{ c_dom_prefix }AEDAT|
      iv_reptext   = lv_reptext
      iv_scrtext_s = lv_scrtext_s
      iv_scrtext_m = lv_scrtext_m
      iv_scrtext_l = lv_scrtext_l
    ).
  ENDMETHOD.

  METHOD create_table.
    WRITE: / 'Tablo oluşturuluyor...'.

    " Tablo zaten var mı kontrol et
    DATA(lv_exists) = table_exists( mv_table_name ).

    " Tablo varsa ve zorla oluşturma modu aktif değilse mesaj ver ve çık
    IF lv_exists = abap_true AND mv_force <> 'X'.
      WRITE: / |Tablo zaten mevcut: { mv_table_name }|.
      RETURN.
    ENDIF.

    " Zorla oluşturma modu aktif ve tablo varsa önce silmeye çalış
    IF lv_exists = abap_true AND mv_force = 'X'.
      WRITE: / |Tablo siliniyor: { mv_table_name }...|.

      " Alternatif silme yaklaşımı - SAP 7.52'de doğrudan tablo silme fonksiyonu olmadığı için
      " bu metodu atla ve yeniden oluşturmaya devam et
      WRITE: / |Not: SAP 7.52'de doğrudan silme fonksiyonu bulunamadı.|.
      WRITE: / |Mevcut tablo üzerine yazılacak.|.
    ENDIF.

    " Tablo alanlarını tanımla
    DATA: lt_dd03p TYPE TABLE OF dd03p.

    " Client alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MANDT'
      position   = 1
      keyflag    = 'X'
      datatype   = 'CLNT'
      leng       = 3
      ddlanguage = sy-langu
      inttype    = 'C'
      intlen     = 3
      rollname   = 'MANDT'   " SAP standardı MANDT data elementi kullanıldı
      ddtext     = 'Client'
      notnull    = 'X'       " Boş değer kabul etmez - Initial value yerine kullanıldı
      mandatory  = 'X'
    ) TO lt_dd03p.

    " Malzeme numarası alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MATNR'
      position   = 2
      keyflag    = 'X'
      rollname   = |{ c_elem_prefix }MATNR|
      ddlanguage = sy-langu
      datatype   = 'CHAR'    " Veri tipi açıkça belirtildi
      leng       = 18        " Uzunluk açıkça belirtildi
      inttype    = 'C'       " İç tip açıkça belirtildi
      intlen     = 36        " İç uzunluk açıkça belirtildi
      mandatory  = 'X'
    ) TO lt_dd03p.

    " Malzeme türü alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MTART'
      position   = 3
      keyflag    = ''
      rollname   = |{ c_elem_prefix }MTART|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 4
      inttype    = 'C'
      intlen     = 8
    ) TO lt_dd03p.

    " Malzeme açıklaması alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MAKTX'
      position   = 4
      keyflag    = ''
      rollname   = |{ c_elem_prefix }MAKTX|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 40
      inttype    = 'C'
      intlen     = 80
    ) TO lt_dd03p.

    " Ölçü birimi alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MEINS'
      position   = 5
      keyflag    = ''
      rollname   = |{ c_elem_prefix }MEINS|
      ddlanguage = sy-langu
      datatype   = 'UNIT'
      leng       = 3
      inttype    = 'C'
      intlen     = 6
    ) TO lt_dd03p.

    " Malzeme grubu alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'MATKL'
      position   = 6
      keyflag    = ''
      rollname   = |{ c_elem_prefix }MATKL|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 9
      inttype    = 'C'
      intlen     = 18
    ) TO lt_dd03p.

    " Oluşturan kullanıcı alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'ERNAM'
      position   = 7
      keyflag    = ''
      rollname   = |{ c_elem_prefix }ERNAM|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 12
      inttype    = 'C'
      intlen     = 24
    ) TO lt_dd03p.

    " Oluşturma tarihi alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'ERDAT'
      position   = 8
      keyflag    = ''
      rollname   = |{ c_elem_prefix }ERDAT|
      ddlanguage = sy-langu
      datatype   = 'DATS'
      leng       = 8
      inttype    = 'D'
      intlen     = 16
    ) TO lt_dd03p.

    " Değiştiren kullanıcı alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'AENAM'
      position   = 9
      keyflag    = ''
      rollname   = |{ c_elem_prefix }AENAM|
      ddlanguage = sy-langu
      datatype   = 'CHAR'
      leng       = 12
      inttype    = 'C'
      intlen     = 24
    ) TO lt_dd03p.

    " Değiştirme tarihi alanı
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'AEDAT'
      position   = 10
      keyflag    = ''
      rollname   = |{ c_elem_prefix }AEDAT|
      ddlanguage = sy-langu
      datatype   = 'DATS'
      leng       = 8
      inttype    = 'D'
      intlen     = 16
    ) TO lt_dd03p.

    " Bisküvi tipini belirten özel alan
    APPEND VALUE #(
      tabname    = mv_table_name
      fieldname  = 'Z1CA_BISKUVI_TIP'
      position   = 11
      keyflag    = ''
      datatype   = 'CHAR'
      leng       = 10
      ddlanguage = sy-langu
      inttype    = 'C'
      intlen     = 20
      ddtext     = 'Bisküvi Tipi'
    ) TO lt_dd03p.

    " Tabloyu oluştur
    DATA: ls_dd02v TYPE dd02v.

    CLEAR ls_dd02v.
    ls_dd02v-tabname     = mv_table_name.
    ls_dd02v-ddlanguage  = sy-langu.
    ls_dd02v-tabclass    = 'TRANSP'.
    ls_dd02v-mainflag    = 'X'.
    ls_dd02v-contflag    = 'A'.
    ls_dd02v-ddtext      = 'Bisküvi Üretimi Malzeme Ana Verileri_MARA'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = mv_table_name
        dd02v_wa          = ls_dd02v
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      WRITE: / |Hata: Tablo oluşturulamadı - { mv_table_name } - { sy-subrc }|.
    ELSE.
      WRITE: / |Tablo oluşturuldu: { mv_table_name }|.

      " TADIR tablosuna paket bilgisini ekle/güncelle
      " Önce TADIR'da kayıt var mı kontrol et
      SELECT SINGLE * FROM tadir
        INTO @DATA(ls_tadir)
        WHERE pgmid    = 'R3TR'
          AND object   = 'TABL'
          AND obj_name = @mv_table_name.

      IF sy-subrc = 0.
        " Kayıt varsa update et
        UPDATE tadir SET devclass = @mv_package,
                         author   = @sy-uname,
                         masterlang = @sy-langu
          WHERE pgmid    = 'R3TR'
            AND object   = 'TABL'
            AND obj_name = @mv_table_name.
      ELSE.
        " Kayıt yoksa insert et
        INSERT INTO tadir VALUES @( VALUE #(
          pgmid     = 'R3TR'
          object    = 'TABL'
          obj_name  = mv_table_name
          devclass  = mv_package
          srcsystem = sy-sysid
          author    = sy-uname
          masterlang = sy-langu
        ) ).
      ENDIF.

      IF sy-subrc = 0.
        WRITE: / |Tablo { mv_table_name } için paket ({ mv_package }) ataması yapıldı.|.
      ELSE.
        WRITE: / |Uyarı: Tablo { mv_table_name } için paket ataması yapılamadı - TADIR güncellenemedi - { sy-subrc }|.
      ENDIF.

      " Tabloyu aktive et
      activate_table( mv_table_name ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Programın başlangıcı
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  " Tablo oluşturucuyu oluştur ve çalıştır
  NEW lcl_table_creator( iv_package = p_pack
                         iv_force_flag = p_force )->create_tables( ).
