---
title: How to send material master delta updates to external system by change pointer
tags: [SAP,Interface,IDOC]
date: 2018-03-12 16:17:32
categories: SAP
---
In this guide, I will share the steps about how to use change pointer to send the material master delta updates to external system. For demo purpose, I will write the IDOC to SAP application server file system with XML file format. 

>System: ECC
>
>Release: 740

### Create message type (t-code: WE81)

Create message type `ZMATMAS_DIST` via t-code `WE81`. Use any short text you like

### Link message type with IDOC type (t-code: WE82)

Link the message type `ZMATMAS_DIST` with standard IDOC type `MATMAS05` and release `740`

### Make sure the change pointer is activated globally (t-code: BD61)

Run t-code `BD61` to make sure the change pointer is activated globally

### Make sure the change pointer is activated for the message type (t-code: BD50)

Run t-code `BD50`, and add a new table entry for `ZMATMAS_DIST`. Select the `active` checkbox

### Maintain the change pointer table fields for the message type (t-code: BD52)

Run t-code `BD52`, input change pointer `ZMATMAS_DIST`, and input the table fields for which change pointer will be created. If one table field is specified here, the change pointer with the message type `ZMATMAS_DIST` will be created when the field value is changed.

- Object: `MATERIAL`
- Table name: `MARA, MARC, DMAKT, etc`
- Field name: `MEINS, MATKL, NTGEW, KEY`

> When `KEY` is used as field name, the change pointer will be created when a new table record is created/deleted.

### Create a logical system if necessary (t-code: BD54)

Create a new logical system `LS_DIST`. You can reuse your existing logical system as well.

### Add the message type to distribution model (t-code: BD64)

Run t-code `BD64`. Switch to `Edit` mode. Choose `Create Model View` to create a new model view `E4D`. You can reuse your existing model view as well. 

Choose `Add Message Type` to add one distribution record with message type `ZMATMAS_DIST`, sender `E4DCLNT100`, which is the logical system of current SAP system specified in t-code `SCC4`, receiver `LS_DIST` 

### Create a XML file port (t-code: WE21)

Create a XML file port `XMLPORT` via t-code `WE21`. Choose `Physical directory`, choose the directory like `\usr\sap\tmp\`, and choose `EDI_PATH_CREATE_MESTYP_DOCNUM` as the function to generate file name


### Maintain the partner profile (t-code: WE20)

Run t-code `WE20`, create the partner profile for the logical system `LS_DIST`. Save the partner profile, click `Create outbound parameters` button to add the outbound parameters for message type `ZMATMAS_DIST`. Use receiver port `XMLPORT`, basic type `MATMAS05`, output mode `Transfer IDoc immediately` 

### Maintain the additional data for message type (t-code: BD60)

Run t-code `BD60`. Copy the record `MATMAS` to a new record `ZMATMAS_DIST`, and deselect the checkbox `Reducable Message Type`. The format function module `MASTERIDOC_CREATE_SMD_MATMAS` is used to create material IDOC from change pointer.

### Update material master data (t-code: MM02)

Run t-code `MM02`, input material `P-100`, choose view `Basic Data 1`, change the gross weight and save the change

### Check the change pointer in BDCP2 (t-code: SE16)

Run t-code `SE16`, input table `BDCP2`, input message type `ZMATMAS_DIST` and execute the program. You will see that a new change pointer with message type `ZMATMAS_DIST` and blank processing status is generated.

### Create IDOC from change pointer (t-code: BD21)

Run t-code `BD21` or run program `RBDMIDOC` manually. You can schedule the program as periodical batch job as well. Input message type `ZMATMAS_DIST` and execute the program. You will get the information message saying that `# IDOC created and generated for the message type ZMATMAS_DIST`.

Run t-code `SE16`, and check the previous change pointer. You will see that the change pointer processing status is changed to `X`.

### Check the new generated IDOC (t-code: WE02)

Run t-code `WE02`, input the message type `ZMATMAS_DIST` and execute. You will see the new generated IDOC.

### Check the XML file generated (t-code: AL11) 

Run t-code `AL11`, open the directory `\usr\sap\tmp\`. You will see that a XML file `ZMATMAS_DI_02496857.xml` was created. Open the file, and you will see the IDOC XML file details.

