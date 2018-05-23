---
title: PI IDOC metadata issue with error message IDOC_ADAPTER/155
tags: [SAP,PI,IDOC,Issue Resolution]
date: 2018-03-15 14:19:32
categories: PI
---
### Background

In ECC tRFC monitor (t-code: `SM58`), user finds that there are failed tRFC calls to PI system with error message `IDOC_ADAPTER/155: EDISDEF: Port &1 segment defn &2 in IDoc type &3 CIM type &4 do not exist`. 

> ECC version: 7.50 SP9
> PI version: 7.40

### Analysis

When a ECC IDOC with PI tRFC port is dispatched, the RFC function  `IDOC_INBOUND_ASYNCHRONOUS` is called in PI. In PI, it will run the function module with the below call stack. In the below last step, it will call RFC function  `IDX_META_SYNTAX_READ` to read IDOC metadata from ECC.

	FUNCTION IDOC_INBOUND_ASYNCHRONOUS
	FUNCTION IDX_INBOUND_XMB
	FORM idx_inbound
	FORM idx_inbound_idoc
	FUNCTION IDX_IDOC_TO_XML
	FUNCTION IDX_SYNTAX_CHECK
	FUNCTION IDX_STRUCTURE_GET	
	FUNCTION IDX_META_SYNTAX_READ

In the RFC function `IDX_META_SYNTAX_READ`, it will read the IDOC metadata from database table `EDISYN` into internal table `I_EDISYN`, and then add them to internal table `I_IDOCSYN`. If `CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU( )` returns true, it will read the last record of `I_IDOCSYN`, use the `nr` field value of the last record, add `1`, use it as the `nr` for the new segment `E1IDOCENHANCEMENT`, and add the new segment to `I_IDOCSYN`.

``` abap
    select * from edisyn into table i_edisyn
                         where idoctyp eq doctyp
                         and   cimtyp  eq cimtyp.
    if sy-subrc ne 0. "muß edisyn noch gemerged werden?
      ....
    endif."EDISYN
* nun kann man aus der i_edisyn i_idocsyn füllen
    loop at i_edisyn.
      clear i_idocsyn.
      move-corresponding i_edisyn to i_idocsyn.
      move i_edisyn-posno to i_idocsyn-nr.
      append i_idocsyn.
    endloop.
  endif."cimtyp

  IF CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU( ) = abap_true.
    READ TABLE i_idocsyn WITH KEY SEGTYP = CL_IDOC_PORT_DEF=>ENH_SEGMENT TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DESCRIBE TABLE i_idocsyn LINES COUNT.
      READ TABLE i_idocsyn INDEX COUNT INTO l_LAST.
      L_LAST-nr = L_LAST-nr + 1.
      l_LAST-segtyp = CL_IDOC_PORT_DEF=>enh_segment.
      CLEAR: L_LAST-parseg,  L_LAST-mustfl, l_last-parpno.
      L_LAST-occmin = 1.
      L_LAST-hlevel = 01.
      L_LAST-occmax = 999999.
      APPEND L_LAST TO i_idocsyn.
      move-corresponding L_LAST to i_edisyn.
      move L_LAST to i_edisyn-posno.
      append i_edisyn.
    ENDIF.
  ENDIF.

```

Since there is no `ORDER BY` in the select statement, and neither `sort` statement after the selection, the order in table `I_EDISYN` and `I_IDOCSYN` could be changed in each selection, and the `nr` field value from the last record may or may not be the maximum `nr` in the table. If the `nr` field value from the last record is not the maximum `nr` in the table as below example, the IDOC segments `STANDARD_SEGMENT6` and `STANDARD_SEGMENT5` will not be loaded into PI IDOC metadata correctly (t-code `IDX2`).  

	0001	STANDARD_SEGMENT1
	0006	STANDARD_SEGMENT6
	0005	STANDARD_SEGMENT5
	0002	STANDARD_SEGMENT2
	0003	STANDARD_SEGMENT3
	0004	STANDARD_SEGMENT4
	0005	E1IDOCENHANCEMENT

In the above case, if the IDOC contains a data record with segment `STANDARD_SEGMENT6` or `STANDARD_SEGMENT5`, the IDOC metadata will not be loaded completely in PI, those data record can not be recognized, and the PI RFC function `IDOC_INBOUND_ASYNCHRONOUS` will be failed with error message `IDOC_ADAPTER/155: EDISDEF: Port &1 segment defn &2 in IDoc type &3 CIM type &4 do not exist`. So the tRFC call will be failed and can be found in t-code `SM58` in ECC.

### Resolution

To fix the issue, there are two options.

1. Contact SAP to fix the issue. I did a search for SAP note with function `IDX_META_SYNTAX_READ`, and didn't find any relevant SAP noes. Client may need to submit a OSS message to SAP, and work with SAP to fix the issue.

2. Add a table entry in table `EDICONFIG` to make sure the IDOC segment `E1IDOCENHANCEMENT` will not be added. In the method `CL_IDOC_PORT_DEF=>SEND_ENHANCED_DOCU`, it will check table `EDICONFIG` for a table entry with the current user and EDI parameter `NO_ENHSEND`. 

We can add a table entry in `EDICONFIG` for the user with EDI parameter `NO_ENHSEND` by running program `RBDENHANCEDDOCUSET`. The service user to load ECC IDOC metadata from PI could be found in the RFC destination of the PI port `SAP<ECC>`, which could be found via t-code `IDX1` in PI.

``` abap 
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

