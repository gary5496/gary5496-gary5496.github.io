---
title: Highlight ABAP Language in Hexo
tags: [Website,Hexo,SAP,ABAP]
date: 2018-03-21 12:34:49
categories: ABAP
---

### Background

The code highlighting functionality `highlight.js` is not enabled for ABAP language for Hexo by default. You can follow the below guide to enable it.

### Setup

#### 1. Update the file `.\node_modules\highlight.js\lib\index.js` under Hexo base folder, and add the below code

``` javascript
hljs.registerLanguage('abap', require('./languages/abap'));
```

#### 2. Create a file `.\node_modules\highlight.js\lib\languages\abap.js` under Hexo base folder, and add the below code

The ABAP language syntax file is downloaded from [GitHub](https://raw.githubusercontent.com/cassioiks/highlight.js/2db174f25da1153f0ca2da12e742526909ad6b3a/src/languages/abap.js), adjusted as below.
- Add `module.exports = ` at the beginning
- Adjust the comment end tag from `'\n'` to `'$'`
- Correct some typos in the keywords

``` javascript
module.exports = function(hljs) {
  return {
    case_insensitive: true,
    keywords: {
    keyword: 'ABBREVIATED ABS ABSTRACT FINAL ACCEPT ACCEPTING ACCORDING ACOS ACTUAL ADD ADD-CORRESPONDING ADDITIONS ADJACENT AFTER '  
        +'ALIASES ALL ALLOCATE ANALYZER AND APPEND APPENDING AS ASCENDING DESCENDING ASIN ASSIGN ASSIGNING ATAN ATTRIBUTE AUTHORITY-CHECK '
        +'AVG BACK BACKGOUND BEFORE BETWEEN BIG LITTLE BINARY BIT BLANK BLOCK BREAK-POINT BUFFER BY BYPASSING BYTE BYTECHARACTER CALL CASE RESPECTING '
        +'CASTING CEIL CENTERED CHANGE CHANGING CHARACTER CHECK CHECKBOX SYMBOL ICONLINE CLASS-DATA CLASS-EVENTS CLASS-METHODS CLEANUP CLEAR '
        +'CLIENT CLOCK CLOSE CODE COL_BACKGROUND COL_HEADING COL_NORMAL COL_TOTAL COLLECT COLOR COLUMN COMMENT COMMIT COMMON COMMUNICATION COMPARING '
        +'COMPONENT COMPONENTS COMPUTE CONCATENATE CONDENSE CONSTANTS CONTEXT CONTEXTS CONTINUE CONTROL CONTROLS CONVERSION CONVERT COS COSH COUNT COUNTRY '
        +'COUNTY CREATE CURRENCY CURRENT CURSOR CUSTOMER-FUNCTION DATA DATABASE DATASET DATE DEALLOCATE DECIMALS DEFAULT UTF-8 NON-UNICODE DEFERRED '
        +'DEFINE DEFINING DEFINITION DELETE DELETING DEMAND DESCENDING DESCRIBE DESTINATION DIALOG DIRECTORY DISTANCE DISTINCT DIVIDE DIVIDE-CORRESPONDING '
        +'DUPLICATE DUPLICATES DURING DYNAMIC EDIT EDITOR-CALL ELSE ELSEIF ENCODING ENDING ENDON ENTRIES ERRORS EVENT EVENTS EXCEPTION EXCEPTIONS EXCEPTION-TABLE '
        +'EXCLUDE EXCLUDING EXIT EXIT-COMMAND EXP EXPORT EXPORTING EXTENDED EXTENSION EXTRACT FETCH FIELD FIELD-GROUPS FIELDS NO FIELD-SYMBOLS FILTER FINAL FIND '
        +'FIRST FLOOR FOR FORMAT FORWARD BACKWARD FOUND FRAC FRAME FREE FRIENDS FROM FUNCTION-POOL GET GIVING GROUP HANDLER HASHED HAVING HEADER HEADING '
        +'HELP-ID HIDE HIGH LOW HOLD HOTSPOT ICON ID IGNORING IMMEDIATELY IMPLEMENTATION IMPORT IMPORTING IN INCLUDE INCREMENT INDEX INDEX-LINE INHERITING '
        +'INIT INITIAL INITIALIZATION INNER INSERT INSTANCES INTENSIFIED INTERFACES INTERVALS INTO APPENDING INVERTED-DATE IS ITAB JOIN KEEPING '
        +'KEY KEYS KIND LANGUAGE LAST LEADING LEAVE LEFT LEFT-JUSTIFIED CIRCULAR LEGACY LENGTH LIKE LINE LINE-COUNT LINES LINE-SELECTION '
        +'LINE-SIZE LIST LIST-PROCESSING LOAD LOAD-OF-PROGRAM LOCAL LOCALE LOG LOG10 LOWER '
        +'MARGIN MARK MASK MATCH MAX MAXIMUM MEMORY MESSAGE MESSAGE-ID MESSAGES METHODS MIN MOD MODE IF MODIFIER MODIFY MOVE MOVE-CORRESPONDING '
        +'MULTIPLY MULTIPLY-CORRESPONDING NEW NEW-LINE NEW-PAGE NEXT NO- NODES NODETABLE NO-DISPLAY NO-GAP NO-GAPS NO-HEADING WITH-HEADING NON NO-SCROLLING '
        +'SCROLLING NOT NO-TITLE WITH-TITLE NO-ZERO NP NS NUMBER OBJECT OBLIGATORY OCCURENCE OCCURENCES OCCURS OF OFF OFFSET ON ONLY ONOFF OPEN '
        +'OPTION OPTIONAL OR ORDER OTHERS OUTER OUTPUT-LENGTH OVERLAY PACK PACKAGE PAGE LAST PAGES PARAMETER PARAMETERS PARAMETER-TABLE '
        +'PART PERFORM PERFORMING PFN PF-STATUS PLACES POS_HIGH POS_LOW POSITION POSITIONS PRIMARY PRINT PRINT-CONTROL PRIVATE PROCESS PROGRAM PROPERTY '
        +'PUBLIC PROTECTED PRIVATE PUSHBUTTON PUT QUICKINFO RADIOBUTTON RAISE RAISING RANGE RANGES READ RECEIVE RECEIVING REDEFINITION '
        +'REF REFERENCE REFRESH REJECT RENAMING REPLACE REPLACEMENT REPORT RESERVE RESET RESOLUTION RESULTS RETURN RETURNING RIGHT RIGHT-JUSTIFIED '
        +'ROLLBACK ROWS RUN SCAN SCREEN SCREEN-GROUP1 SCREEN-GROUP2 SCREEN-GROUP3 SCREEN-GROUP4 SCREEN-GROUP5 SCREEN-INPUT SCREEN-INTENSIFIED SCROLL '
        +'SCROLL-BOUNDARY SEARCH SECTION SELECT SELECTION SELECTIONS SELECTION-SCREEN SELECTION-SET SELECTION-TABLE SELECT-OPTIONS SEND SEPARATED SET '
        +'SHARED SHIFT SIGN SIN SINGLE DISTINCT SINH SIZE SKIP SORT SORTABLE SPECIFIED SPLIT SQL SQRT STABLE STAMP STANDARD START STARTING '
        +'STATICS STEP-LOOP STOP STRLEN STRUCTURE SUBMIT SUBTRACT SUBTRACT-CORRESPONDING SUFFIX SUM SUPPLY SUPPRESS SYMBOLS SYSTEM-EXCEPTIONS TABLE TABLENAME '
        +'TABLES TABLEVIEW TAN TANH TASK TEXT THEN TIME TIMES TITLE TITLEBAR TO TOPIC TOP-OF-PAGE TRAILING TRANSACTION TRANSFER TRANSLATE TRUNC TYPE '
        +'TYPELIKE TYPE-POOL TYPE-POOLS TYPES ULINE UNION UNIQUE UNIT UNTIL UP UPDATE UPPER USER-COMMAND USING VALUE VALUES VARY VARYING '
        +'VERSION VIA WAIT WHEN WHERE WINDOW WITH WORK WRITE XSTRLEN ZONE TRANSPORTING '
        +'CA CN CO CP CS EQ GE GT LE LT NA NE'
        +'START-OF-SELECTION START-OF-PAGE END-OF-PAGE END-OF-SELECTION AT ENDAT',
      literal: 'abap_true abap_false',
      built_in: 'DO FORM IF LOOP MODULE START-OF_FILE DEFINE WHILE BEGIN ENDDO ENDFORM ENDIF ENDLOOP ENDMODULE END-OF_FILE END-OF-DEFINITION ENDWHILE END'
        +' METHOD ENDMETHOD CHAIN ENDCHAIN CASE ENDCASE FUNCTION ENDFUNCTION ELSEIF ELSE TRY ENDTRY CATCH '
    },
    contains: [ 
      hljs.APOS_STRING_MODE,
      hljs.NUMBER_MODE,
      {
        className: 'comment',
        begin: '^[*]',
        end: '$'
      },
      {
        className: 'comment',
        begin: '\b*"',
        end: '$'
      },
      {
        className: 'built_in',
        begin: 'CALL +([A-Z])',
        end:' +',
        relevance: 10
      }
    ]
  }
};
```

#### 3. Create a file `.\node_modules\highlight.js\styles\abap.css` under Hexo base folder, and add the below code

The ABAP language css file is downloaded from [GitHub](https://raw.githubusercontent.com/cassioiks/highlight.js/f1b511817f2fda61304dead6725daac47704f29b/src/styles/abap.css).

``` css
/*
ABAP styles by Cassio Binkowski <cassioiks@gmail.com>
*/


.hljs {
  display: block;
  overflow-x: auto;
  padding: 0.5em;
}
.hljs-keyword{
  color: #0005fb;
  }
.hljs-string{
  color: #4da31e;
  }
.hljs-comment{
  color: #7c7879;
  font-style: italic;
  }
.hljs-built_in{
  color: #000007;
  font-weight: bold;
}
.hljs-literal{
  color: #000007;
}
.hljs-subst {
  color: black;
}
.hljs-number{
  color: #5aceff;
}
```

#### 4. Update the file `\node_modules\hexo-util\highlight_alias.json` under Hexo base folder

Add the below field-value mappings to alias object

``` javascript
        "abap": "abap",
        "ABAP": "abap",
```

Add the below value to languages array

``` javascript
        "abap",
```

### Result

Run `hexo clean`, `hexo generate` to generate the files. Below is the result with ABAP code highlighting.

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

### Reference

[Add ABAP Language to highlight.js - GitHub](https://github.com/isagalaev/highlight.js/pull/1259)
