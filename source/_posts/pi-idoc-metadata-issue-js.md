---
title: PI IDOC metadata issue in Java Stack
tags: [SAP,PI,IDOC,Issue Resolution]
date: 2018-03-15 17:23:14
categories: PI
---
### Background

The PI channel using adapter module `IDOCXmlToFlatConvertor` with `SAPRelease 46C` is failed with error message `Unable to get meta data for IDOC type <IDOC_TYPE>: SEGMENT_UNKNOWN`. 

### Analysis

The adapter module `IDOCXmlToFlatConvertor` is using IDOC metadata in Java stack to transform IDOC from XML format to flat file format. We tried to load the IDOC metadata in PI NWA (Java stack) manually with the `Connection Factory` and `SAPRelease` parameters maintained in the PI channel, and the load is failed with error message `Service node <node> is not accessible; metadata cannot be shown or manipulated`. 

After RFC trace, we find that the ECC RFC function `IDOCTYPE_READ_COMPLETE` is called by PI when loading IDOC metadata in Java stack. The RFC function is changed in the recent upgrade. In the new change, if the return result of `CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU( )` is true, it will add a new IDOC segment `E1IDOCENHANCEMENT`, which is valid since release 700 (t-code `WE31`). Since the new IDOC segment is not valid in release `46C`, the RFC function will be failed, which causes the failure of PI channel.

``` abap
  IF CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU( ) = abap_true.
    READ TABLE LT_SYNTAX WITH KEY SEGTYP = CL_IDOC_PORT_DEF=>ENH_SEGMENT TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DESCRIBE TABLE LT_SYNTAX LINES COUNT.
      READ TABLE lt_syntax INDEX COUNT INTO l_LAST.
      L_LAST-nr = L_LAST-nr + 1.
      l_LAST-segtyp = CL_IDOC_PORT_DEF=>enh_segment.
      CLEAR: L_LAST-parseg, L_LAST-refsegtyp, L_LAST-mustfl, l_last-parpno.
      L_LAST-occmin = 1.
      L_LAST-hlevel = 01.
      L_LAST-occmax = 999999.
      APPEND L_LAST TO lt_syntax.
    ENDIF.
  ENDIF.
```

### Resolution

To fix the issue, we can maintain a new entry in table EDICONFIG, so that the return result of `CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU( )` is false. With this change, the new IDOC segment will not be added, and it will fix the issue. A table entry in `EDICONFIG` for the service user with EDI parameter `NO_ENHSEND` is added by running program `RBDENHANCEDDOCUSET`. The service user to load IDOC metadata from PI Java stack could be found in the `Connection Factory` in NWA.

```abap
  METHOD send_enhanced_docu.

    DATA: wa TYPE ediconfig,
          uname like sy-uname.

    IF cl_rfc=>is_external_direct( ) = abap_true. " external call only
      uname = cl_abap_syst=>get_user_name( ).
      SELECT  SINGLE * FROM ediconfig
        INTO wa
         WHERE uname = uname
          AND edi_global  = ' '
          AND edi_param = 'NO_ENHSEND'
          AND edi_parval = 'X'.

      IF sy-subrc = 0.
        send_docu = abap_false.
      ELSE.
        send_docu = abap_true.
      ENDIF.

    ELSE.
      send_docu = abap_true. "
    ENDIF.

  ENDMETHOD.
```
