---
title: Lessons Learnt during creating SCP CF Node.js application
tags: [SAP, SCP, Cloud]
date: 2018-06-19 15:47:49
categories: SAP
---
### How to install SAP modules

Run the below command before running `npm install`
`npm config set @sap:registry https://npm.sap.com`

---
### How to create a SCP CF node.js application

Check the below link for detail:
https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/772b45ce6c46492b908d4c985add932a.html


---
### How to install Cloud Foundry client tools

Download the Cloud Foundry client tool from the below link and install it
https://github.com/cloudfoundry/cli#downloads

### Reference:
https://www.sap.com/developer/tutorials/hcp-cf-getting-started.html
https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/75125ef1e60e490e91eb58fe48c0f9e7.html#loio4ef907afb1254e8286882a2bdef0edf4

---
### How to connect to CF region

`cf api https://api.cf.us10.hana.ondemand.com`
`cf login`
`cf target -o ORG -s SPACE`

---
### How to deploy an application to SCP CF environment

`cf push` is executed in the same directory to deploy an application where the manifest.yml is located.
`cf app myapp` is used to check the status of application myapp.
`cf apps` is used to check the status of all deployed applications.

---
### How to access a deployed application

Access the application via link `<app_host>.cfapps.us10.hana.ondemand.com`

