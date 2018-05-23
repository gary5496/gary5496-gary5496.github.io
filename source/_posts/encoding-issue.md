---
title: Encoding in SAP
tags: [SAP]
date: 2018-04-12 12:30:23
categories: SAP
---
Since different applications are using different encodings to process data, there may be encoding issue when the source application and destination application are using different encodings.

### ECC Encoding
- SAP ECC is saving data in database with Unicode encoding. You can check if your SAP application server is using Little Indian (4103) or Big Indian (4102) via `t-code I18N -> I18N Customizing -> I18N System Configuration -> Display Current NLS config -> "Code Page of Application Server"`. 
- When SAP ECC is sending a XML file to PI, UTF-8 encoding (4110) will be used.
- When SAP ECC is writing a flat file to application server folder, the encoding will depend on the ABAP statement `OPEN DATASET ... ENCODING ...`. If `ENCODING DEFAULT` or `ENCODING UTF-8` is used, UTF-8 will be used.
- When SAP ECC is using file port to write an IDOC flat file, the encoding will be UTF-8 if the `Unicode format` checkbox is selected in port (t-code WE21).
- When SAP ECC is using file port to write an IDOC flat file, the encoding will be using the relevant code page if `Unicode format` is not selected and `Char. Set` is specified in port (t-code WE21).
- When SAP ECC is using file port to write an IDOC flat file, if `Unicode format` and `Char. Set` are not specified, the encoding will be using the same encoding as ABAP statement `ENCODING NON-UNICODE`. In an Unicode SAP system, the encoding in the table `TCP0C` by platform, language and country will be used. In the case of Linux, English, and US, the encoding will be ISO-8859-1 (1100). You can use ABAP statements `SET COUNTRY` and `SET LOCALE LANGUAGE` to change the language and country in a ABAP session.

### PI Encoding
- PI sender file channel will use the specified file encoding to read the file, if file type is set to `Text` instead of `Binary`. 
- You can convert a flat file to XML file via adapter module `AF_Modules/MessageTransformBean`, and specify the encoding of the XML file by setting the parameter `Transform.ContentType` to `text/plain;charset=utf-8`.
- You can convert the encoding of an output file from UTF-8 to ISO-8859-1 via adapter module `AF_Modules/TextCodepageConversionBean` with parameter `Conversion.charset` equal to `iso-8859-1`.

### Utilities
You can check the hex code and UTF-8 bytes for one character in this [site](http://www.ltg.ed.ac.uk/~richard/utf-8.cgi?input=0421&mode=hex). Please note:
- The hex code is using Unicode big indian, such as `0421` for `CYRILLIC CAPITAL LETTER ES` - `C`.
- The UTF-8 bytes should contain space if there are multiple bytes for one character, such as `D0 A1`.
- In some cases, `UTF-8 bytes as Latin-1 characters bytes` will be showing the same invalid characters as destination application, if source application is using `UTF-8` encoding, and the destination application is using encoding like `ISO-8859-1` to process data.
You can use Notepad++ to open a file with different encodings, and you can also convert it to use another encoding.
You can use some application like `WinHex` or `Hex_edit` to see the hex code for one text file.

### Common Encoding/Code Pages
- 1100 for ISO-8859-1, which is similar to WINDOWS-1252/ANSI
- 4110 for UTF-8, the optional BOM (Byte Order Mask) will be EFBBBF for UTF-8
- 4102 for UTF-16be
- 4103 for UTF-16le
- 8400 for GB2312

### One encoding issue in my work
- Issue: Below is one encoding issue I came accross with in my work. In SAP ECC system, the character is showing as `C`, but in destination application, the character is showing as `Ð ¡`, and can not be recoginzed.
- Analysis: The special character `CYRILLIC CAPITAL LETTER ES` which is showing exactly the same as `LATIN LETTER C` is used in SAP ECC. In debugging mode, we will see that the hex code is `2104` since SAP ECC system is using Unicode little indian. In the [site](http://www.ltg.ed.ac.uk/~richard/utf-8.cgi?input=0421&mode=hex), we checked with the hex code `0421` which is Unicode big indian, and we can find that the UTF-8 bytes for this character is `D0 A1`, and `UTF-8 bytes as Latin-1 characters bytes` is showing as `Ð ¡`.

### References
[Check hex code and UTF-8 bytes for one character](http://www.ltg.ed.ac.uk/~richard/utf-8.cgi?input=0421&mode=hex)
[SAP note 552464 - What is Big Endian / Little Endian? What Endian do I have?](https://launchpad.support.sap.com/#/notes/552464)