---
title: Async and Await Keywords in JavaScript
tags: [JavaScript]
date: 2018-03-27 14:31:49
categories: JavaScript
---

### Async/Await
To better understand the behavior of async and await keywords in JavaScript, I have written the below code and check the result.

``` javascript
const fs = require('fs');
const util = require('util');
// Convert fs.readFile into Promise version of same    
const readFile = util.promisify(fs.readFile);

async function asyncReadfile1() {
  console.log("The await async function to read file 1 is called");
  const text = await readFile("C:/Download/129.xps");
  console.log('The file 1 contains: ' + text.length + ' bytes');
}

async function asyncReadfile2() {
  console.log("The await async function to read file 2 is called");
  const text =await readFile("C:/Download/141.xps");
  console.log('The file 2 contains: ' + text.length + ' bytes');
}

async function asyncMain() {
  await asyncReadfile1();
  console.log("Processing between reading 2 files");  
  await asyncReadfile2();
  return "successful";
}

console.log("Start processing...");
asyncMain().then((result) => { 
  console.log("The async main function is called " + result); 
});
console.log("Go to the next statement after the async main function");
```

Result:

```
$ node app.js
Start processing...
The await async function to read file 1 is called
Go to the next statement after the async main function
The file 1 contains: 251610 bytes
Processing between reading 2 files
The await async function to read file 2 is called
The file 2 contains: 549337 bytes
The async main function is called successful
```

Here are the processing steps for the above example.
1. The runtime engine will call main function `asyncMain`, which will in turn call the first `await` async function `asyncReadfile1` , which will output console log `The await async function to read file 1 is called`
2. When the function `asyncReadfile1` runs to the statement `const text = await readFile("C:/Download/129.xps");`, the Promise object returned from `readFile` is *not resolved* yet, so the execution stack will go back to upper function `asyncMain`. In the main function, the Promise object returned from `asyncReadfile1` is *not resolved* yet, so the executation stack will go back to upper level. Therefore, the next console log is `Go to the next statement after the async main function`.
3. When the sync processing logic in the upmost level is completed, the call stack goes back to deepest unresolved Promise function `asyncReadfile1`. In the async function `asyncReadfile1`, the statement `const text = await readFile("C:/Download/129.xps");` is completed and the Promise object is *resolved*, so it will continue with the next statement, and the next console log is `The file 1 contains: 251610 bytes`. After the console log, the Promise object of async function `asyncReadfile1` is *resolved*.
4. Since the promise object of async function `asyncReadfile1` is *resolved*, the main function will continue with next statement, which will output console log `Processing between reading 2 files`.
5. In the next statement of main function, async function `asyncReadfile2` wil be called, and the console log will be `The await async function to read file 2 is called` and `The file 2 contains: 549337 bytes`
6. After the return statement of main function, the Promise object of the main function is *resolved*, so the callback function in `Promise.prototype.then` method will be executed to output console log `The async main function is called successful`

### Understanding for async/await:
1. When the Promise object returned from `await` child function is *not resolved* yet, the execution stack will go back to parent function.
2. When the Promise object returned from `await` child function is *resolved*, the execution stack will continue with next statement in the current function.
3. When the sync processing logic in the upmost level is completed, the call stack goes back to deepest unresolved Promise function.

### Promise

To achieve the same functionality with Promise object, we would need to more complex code with nested `Promise.prototype.then` method.

```javascript
const fs = require('fs');
const util = require('util');
// Convert fs.readFile into Promise version of same    
const readFile = util.promisify(fs.readFile);

function asyncReadfile1() {
  console.log("The await async function to read file 1 is called");
  return new Promise(function (resolve, reject) { 
    readFile("C:/Download/129.xps").then( (text) => {
      console.log('The file 1 contains: ' + text.length + ' bytes');
      resolve();
    } );
  } );
}

function asyncReadfile2() {
  console.log("The await async function to read file 2 is called");
  return new Promise(function (resolve, reject) { 
    readFile("C:/Download/141.xps").then( (text) => {
      console.log('The file 2 contains: ' + text.length + ' bytes');
      resolve();
    } );
  } );
}

function asyncMain() {
   return new Promise(function (resolve, reject) { 
     asyncReadfile1().then( () => {
      console.log("Processing between reading 2 files");
      asyncReadfile2().then( () => {
        resolve("successful");
      });
    });
  });
}

console.log("Start processing...");
asyncMain().then((result) => { 
  console.log("The async main function is called " + result); 
});
console.log("Go to the next statement after the async main function");
```

### Understanding for Promise
1. The resolved immediately Promise object will be effective in next *Event loop*, instead of current *Event loop*.
```javascript
setTimeout(function () {
  console.log('three');
}, 0);

Promise.resolve().then(function () {
  console.log('two');
});

console.log('one');

// one
// two
// three
```
2. The `Promise.prototype.then` method will return another Promise object, which is not the original Promise object.
3. The new Promise object will be executed immediately after creation.
```javascript
var promise = new Promise(function(resolve, reject){
    console.log("New Promise object");
    resolve();
  }).then( () => {
    console.log("Callback method");
});

console.log("After creating new Promise object");

// New Promise object
// After creating new Promise object
// Callback method
```

### Reference

[Promise object](http://es6.ruanyifeng.com/#docs/promise#%E5%BA%94%E7%94%A8)
[async function](http://es6.ruanyifeng.com/?search=await&x=0&y=0#docs/async)