---
title: this keyword in JavaScript
tags: [JavaScript]
date: 2018-03-19 14:31:01
categories: JavaScript
---
### Background

After reading the below few posts about `this` variable in JavaScript. I think I have to write a post to note down my understanding about it.

<http://www.ruanyifeng.com/blog/2010/04/using_this_keyword_in_javascript.html>
<https://www.liaoxuefeng.com/wiki/001434446689867b27157e896e74d51a89c25cc8b43bdb3000>

### Function call

In the normal function call, `this` is the global variable, which is `window` when the page is loaded in web browser. `test()` is the same as `window.test()`.

``` javascript
var x = 1;
alert(this.x);    //1

function test(){
    alert(this.x);
}
test();    //1
```

### Method call

When the function is used as a method of an object, `this` is `reference to the object` when the method (function) is called.

``` javascript
var x = 1;
var o = {
    x: 2,
    method: function () {
        alert(this.x);
    }
};
o.method();    //2  
```

### Constructor function

When the function is used as a constructor function, `this` is `reference to the object` in the construction function. 

``` javascript
var x = 1;
function test(){
    alert(this.x);
}

function o(){
    this.x = 2;
    this.method = test;
}

var o = new o();
o.method();    //2
test();    //1
```

### Method apply

Developer can use the method `object.apply` to change the binding of `this`. 

``` javascript
var x = 1;
var test = function () {
    alert(this.x);
};
var o = {
    x: 2,
    m: test
};

test();    //1
test.apply(o);    //2
```

### Function call in method call

If `this` variable is used in a function call in an object method call, it will be the reference to `window`, instead of the `reference to the object`.

``` javascript
var x = 1;
var Foo = {
    x: 2,
    method: function(){ 
        var test = function(){
            console.log(this.x);
        }
        test();
    }
};
Foo.method();    //1, which is the global variable
```

To set `this` to the `reference to the object` in the above case, you can use a variable like `that` to store `this` variable as below.

``` javascript
var x = 1;
var Foo = {
    x: 2,
    method: function(){ 
        var that = this;
        var test = function(){
            console.log(that.x);
        }
        test();
    }
};
Foo.method();    //2, which is the global variable
```
