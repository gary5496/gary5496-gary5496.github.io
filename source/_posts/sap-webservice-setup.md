---
title: SOAP technical setup in SAP for Webservice
tags: [SAP,Webservice]
date: 2018-03-22 17:04:11
categories: SAP
---

To enable a webservice in SAP system, we have to perform SOAP technical setup if it was not completed before.

### SOAP technical setup in SAP
1. Run t-code `SRT_ADMIN` to perform automatic setup for the below technical settings
   - bgRFC inbound destination
   - bgRFC supervisor destination
   - Service RFC destination and user
   - ICF nodes
2. Run t-code `SRM_ADMIN` to check the technical settings
3. Make sure the Webservice SICF service `/sap/bc/srt/xip/sap/<webservice_name>` is activated in t-code `SICF`

### Errors and Resolutions
- Error:
 > The webservice message is hanging in webservice monitor `SRT_MONI` with status *wait for scheduler*.

- Resolution:
> Based on SAP note `2278161`, the probable reason is the incomplete setup of bgRFC supervisor destination. Complete the technical settings setup in `SRT_ADMIN` to fix the issue.
