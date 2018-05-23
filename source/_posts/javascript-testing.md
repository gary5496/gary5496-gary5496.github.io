---
title: Testing in JavaScript
tags: [JavaScript, Testing]
date: 2018-03-21 15:21:52
categories: JavaScript
---

### Introduction

There are many ways to test and debug JavaScript code. Here I will note down some methods that I am using.

### Methods

#### 1. Create a HTML file with JavaScript code

Create a HTML file such as `test.html` with JavaScript code in local directory. Open the file directly
``` html
<!DOCTYPE html>
<html>
<head>
<meta charset="ISO-8859-1">
<title>Insert title here</title>
<script>
    var test = 'Test string';
    console.log(test);
    alert("Hello, World!");
</script>
</head>
<body>
This is a test page.
</body>
</html>
```

#### 2. Test with jsfiddle

- Open site [jsfiddle](https://jsfiddle.net)

- Add the below code to HTML section
``` html
<div id="console-log"></div>
```

- Add the below code to CSS section
``` css
.console-line
{
    font-family: monospace;
    margin: 2px;
}
```

- Add the below code to JavaScript section and choose a `jQuery` library as the `FRAMEWORK & EXTENSIONS`
``` javascript
var consoleLine = "<p class=\"console-line\"></p>";
 
console = {
    log: function (text) {
        $("#console-log").append($(consoleLine).html(text));
    }
};
```

- Choose `Save` button to save new fiddle, and bookmark the new URL

- Now, you can add your code for testing. 
``` javascript
var test = "Test String";
console.log(test);
alert("Hello, World!");
```

#### 3. Test with Visual Studio Code and local web server

- Install Node.js
- Create a new folder for your project
- Add a package.json file to the project folder
``` json
{ 
   "name": "Demo", 
   "version": "1.0.0", 
   "description": "demo project.", 
   "scripts": { 
     "lite": "lite-server --port 10001", 
     "start": "npm run lite" 
   }, 
   "author": "", 
   "license": "ISC", 
   "devDependencies": { 
     "lite-server": "^1.3.1" 
   } 
}
```
- Install the web server

  In a terminal window (command prompt in Windows) opened on your project folder, run this command:
``` bash
npm install
```

  This will install lite-server (defined in package.json), a static server that loads index.html in your default browser and auto refreshes it when application files change.
- Start the local web server

  Assuming you have an index.html file in your project folder. In the same terminal window (command prompt in Windows) run this command:
``` bash
npm start
```

  Wait a second and index.html is loaded and displayed in your default browser served by your local web server!
  lite-server is watching your files and refreshes the page as soon as you make changes to any html, js or css files.
  And if you have VS Code configured to auto save (menu File / Auto Save), you see changes in the browser as you type!

  Notes:
> Do not close the command line prompt until you’re done coding in your app for the day
> It opens on http://localhost:10001 but you can change the port by editing the package.json file.

  That’s it. Now before any coding session just type npm start and you are good to go!

### Reference

[Visual Studio Code and local web server](https://blogs.msdn.microsoft.com/cdndevs/2016/01/24/visual-studio-code-and-local-web-server/)


