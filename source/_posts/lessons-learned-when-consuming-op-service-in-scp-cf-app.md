---
title: Lessons Learned When Consuming On-Premise Service in SCP Cloud Foundry Application
tags: [SAP, SCP, Cloud]
date: 2018-06-19 14:17:18
categories: SAP
---
I am trying to follow the below links to build a SCP (SAP Cloud Platform) Cloud Foundry application which is using XSUAA (User account and authentication) service,  SCP Connectivity service and SCC (SAP Cloud Connector) to consume On-Premise OData service. The below lessons are learned during the prototype.

https://blogs.sap.com/2017/07/09/how-to-use-the-sap-cloud-platform-connectivity-and-the-cloud-connector-in-the-cloud-foundry-environment-part-1/
https://blogs.sap.com/2017/07/13/part-2-how-to-use-the-sap-cloud-platform-connectivity-and-the-cloud-connector-in-the-cloud-foundry-environment/


### Maven installation

1. Install JDK and set environment variable `JAVA_HOME`
2. Download Apache Maven and unzip the file
3. Add environment variable `M2_HOME` and `MAVEN_HOME`
4. Update environment variable `PATH` for Maven bin folder
5. Run command `mvn -version` to verify the installation

### Reference:
https://www.mkyong.com/maven/how-to-install-maven-in-windows/
http://maven.apache.org/download.cgi


---
### Maven eclipse plugin installation

Please check the below reference links for details. Update site to install Maven plugin in Eclipse: 
http://download.eclipse.org/technology/m2e/releases/

### Reference:
https://stackoverflow.com/questions/8620127/maven-in-eclipse-step-by-step-installation
http://toolsqa.com/java/maven/how-to-install-maven-eclipse-ide/


---
### Error:
Failed to read artifact descriptor for org.apache.maven.plugins:maven-resources-plugin:jar:2.6
Could not transfer artifact org.springframework.boot:spring-boot-starter-parent:pom:1.5.7.RELEASE from/to Central (http://repo1.maven.org/maven2): Connection refused: connect and 'parent.relativePath' points at wrong local POM

### Resolution:

1. Run the below command in Linux:
`find ~/.m2  -name "*.lastUpdated" -exec grep -q "Could not transfer" {} \; -print -exec rm {} \;`
Run the below commands in Windows
`cd %userprofile%\.m2\repository`
`for /r %i in (*.lastUpdated) do del %i`
Then right click on your project in eclipse and choose `Maven->Update Project ...`, make sure "Update Dependencies" is checked in the resulting dialog and click OK.
2. You can always try `mvn -U clean install`
-U Forces a check for updated releases and snapshots on remote repositories
3. There may be different versions of Maven running between command prompt and Eclipse. Check the Maven installations and Maven user settings in `Eclipse -> Windows -> Preferences`

### Reference:
https://stackoverflow.com/questions/5074063/maven-error-failure-to-transfer


---
### Error:
No compiler is provided in this environment. Perhaps you are running on a JRE rather than a JDK?

### Resolution:
1. Install JDK instead of JRE
2. Set environment variables `JAVA_HOME, classpath, PATH` to link them to JDK, instead of JRE
3. Check the settings `Windows -> Preferences -> Java ->Installed JREs` in Eclipse. Make sure there is at least one JDK there. Link the Run configuration of Maven build to the JDK

### Reference:
https://stackoverflow.com/questions/19655184/no-compiler-is-provided-in-this-environment-perhaps-you-are-running-on-a-jre-ra


---
### Error:
The following artifacts could not be resolved: com.sap.xs2.security:java-container-security-api:jar:0.26.4, com.sap.xs2.security:java-container-security:jar:0.26.4: Failure to find com.sap.xs2.security:java-container-security-api:jar:0.26.4 in https://repo.maven.apache.org/maven2 was cached in the local repository, resolution will not be reattempted until the update interval of central has elapsed or updates are forced

### Resolution:
Install XS Security libs to your local Maven repository:
1. The first step is to get some additional Java libs from Service Marketplace.
Download additional XS security libs from service marketplace:https://launchpad.support.sap.com/#/softwarecenter/search/XS_JAVA 
2. Unzip the file
3. Install XS Security Libs to your local maven repo using:
`cd <destLocation>`
`mvn clean install`

### Reference:
https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/


---
### Error:
The import org.json cannot be resolved
The import org.apache.commons.io cannot be resolved

### Resolution:
1. Download the ZIP file from the below URL and extract it to get the Jar. 

2. Add the Jar to your build path. To Add this Jar to your build path `Right click the Project > Build Path > Configure build path > Select Libraries tab > Click Add External Libraries > Select the Jar file downloaded`

http://www.java2s.com/Code/JarDownload/java/java-json.jar.zip
http://commons.apache.org/proper/commons-io/download_io.cgi

### Reference
https://stackoverflow.com/questions/8997598/importing-json-into-an-eclipse-project


---
### Error
package json.java does not exist
package org.apache.commons.io does not exist

### Resolution
1. Run the below Maven command to install the Maven repository
`mvn install:install-file -Dfile=java-json.jar -DgroupId=json.java -DartifactId=java-json -Dversion=1.0 -Dpackaging=jar`
`mvn install:install-file -Dfile=commons-io-2.6.jar -DgroupId=commons-io -DartifactId=commons-io -Dversion=2.6 -Dpackaging=jar`
2. Add the below dependencies to the pom.xml file
```
		<dependency>
			<groupId>json.java</groupId>
			<artifactId>java-json</artifactId>
			<version>1.0</version>
			<scope>compile</scope>
		</dependency>
		<dependency>
			<groupId>commons-io</groupId>
			<artifactId>commons-io</artifactId>
			<version>2.6</version>
			<scope>compile</scope>
		</dependency>
```

### Reference
https://stackoverflow.com/questions/41779195/maven-compilation-error-even-if-dependency-path-is-correct


---
### Error
Cannot instantiate the type XSTokenRequest

### Resolution
1. Run the below command to install the Jar file to local Maven repository
`mvn install:install-file -Dfile=java-container-security-api-0.26.4.jar -DgroupId=com.sap.xs2.security -DartifactId=java-container-security-api -Dversion=0.26.4 -Dpackaging=jar`
`mvn install:install-file -Dfile=java-container-security-0.26.4.jar -DgroupId=com.sap.xs2.security -DartifactId=java-container-security -Dversion=0.26.4 -Dpackaging=jar`
2. Change the pom.xml with the below:
```
		<dependency>
  			<groupId>com.sap.xs2.security</groupId>
  			<artifactId>java-container-security-api</artifactId>
  			<version>0.26.4</version>
		</dependency>
		<dependency>
			<groupId>com.sap.xs2.security</groupId>
			<artifactId>java-container-security</artifactId>
 			<version>0.26.4</version> 
		</dependency>
```		
3. Run `Maven update` for the project
4. Check the build path for the Java file to see if multiple libraries are used


---
### Error
The import javax.servlet can't be resolved [duplicate]

### Resolution
You need to add the Servlet API to your classpath. In Tomcat 6.0, this is in a JAR called servlet-api.jar in Tomcat's lib folder. You can either add a reference to that JAR to the project's classpath, or put a copy of the JAR in your Eclipse project and add it to the classpath from there.

### Reference
https://stackoverflow.com/questions/4119448/the-import-javax-servlet-cant-be-resolved


---
### Error:
Unable to extract request URI: URI must contain a host
http://<virtual_host>:80/sap/opu/odata/sap/<odata_service>//$metadata

### Resolution:
Update the SCC (SAP Cloud Connector) and SCP destination to make sure underscore is not used in the hostname


---
### How to change the font size in Eclipse editor

Windows -> Preferences -> General -> Appearance -> Color and Fonts -> Basic -> Text Font -> Edit -> Apply


---
### How to search for a string in a project in Eclipse

Ctrl + H


---
### How to upload a project over 20M to SCP CF environment

Use Eclipse Cloud Foundry plugin
