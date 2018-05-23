---
title: ABAP development for Status Management in SAP
tags: [SAP,ABAP]
date: 2018-03-28 15:57:53
categories: SAP
---

Below you will find some objects which is useful when you are performing ABAP development for status management in SAP.

### Function Modules
1. `STATUS_READ`: With this function, you can get the system status and user status internal codes, such as `I0001`, `E0001`.
2. `STATUS_TEXT_EDIT`: With this function, you can get the external status code `REL` `APPR` for one object. In an update transaction, if you set the input parameter `BYPASS_BUFFER` to `X`, it will get the old status code from database. If you set the input parameter `BYPASS_BUFFER` to blank, it will get the new status code after the change.
3. `STATUS_CHANGE_INTERN`: With this function, you can update the system status.
4. `STATUS_CHANGE_EXTERN`: With this function, you can update the user status.
5. `STATUS_CHANGES_GET`: With this function, you can get the status changes in the current update transaction.

### Tables
1. `JSTO`: Status object information. It contains the status profile and object category for one object.
2. `JEST`: Individual Object Status. It contains the object status information. Field `INACT` indicates whether the status is active or not.
3. `TJ02T`: System status texts. It contains the external status code and description for one system status, which is `INNNN`.
4. `TJ30T`: Texts for User Status. It contains the external status code and description for one user status, which is `ENNNN`. 
5. `JCDS`: Change Documents for system/user status. It contains the change documents for status management.

### T-codes
1. `BS23`: Display system status
2. `BS03`: Display status profile (user status)

### Tips
- You can use `Authorization key` to control the authorization to update user status. If the auth key is assigned to one user status in status profile, the authorization with auth object `B_USERSTAT` and auth key should be assigned to the user, so that one is able to update the user status. 