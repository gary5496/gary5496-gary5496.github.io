---
title: Enhance ABAP Code provided by Vendor in SAP
tags: [SAP,ABAP]
date: 2018-03-23 11:49:43
categories: ABAP
---

### Background
In SAP, sometimes we may need to add an enhancement implementation for an implicit enhancement spot of Vendor provided ABAP code. In that case, when you are trying to enhance the vendor code, you may receive an error message `Object <object_type> <object_name> cannot be enhanced; software component Unknown cannot be enhanced`.

### Analysis
After the analysis, we find that in the function `TR_GET_DLVUNIT_CHANGEABILITY`, it will check table `CVERS` and `DLV_SYSTC` to see if the software component is changeable or not. If not, the error message will display. The software component is assigned to the package (development class) of the ABAP code.

To fix the issue, we tried to find a t-code or program to update table entries in `CVERS` and `DLV_SYSTC` to set the changeable flag to *Yes* for the software component, but with no luck.

Later, we tried to update the software component for the package, and below is the solution.

### Solution
Run t-code `SE03`, and choose `Administration` -> `Display/Change Namespaces`. Switch to change mode, double click on the namespace `/<vendor_ns>/` of the vendor package, change `Namespace role` to `C: Recipient` and save the entry.

Run t-code `SE80`, choose `Package`, input the package name, double click on the package, switch to change mode, change `Software Component` to `HOME`, and save the package.
