---
title: Closure in JavaScript
tags: [JavaScript]
date: 2018-03-19 15:50:49
categories: JavaScript
---

### Introduction

In a function, the variables may come from global variable like `window`, local variable, or the variable of its ancestor functions. If the variable comes from its ancestor functions, it is called `Closure` in JavaScript. 

There are three variable scopes: `Local`, `Closure` and `Global`. If the variable is coming from its ancestor function and the child function is still being used, the variable in the ancestor function will be saved and it will not be recycled by JavaScript garbage collection.

In JavaScript garbage collection, if the where-used list for a variable/function/object is 0, it will be recycled. Otherwise, it will be kept. If `Closure` is used and the child function is still in used, the ancestor function and its variable will be kept since the where-used list is not 0.

### Understand variable scope

In the below case, the output of `f1, f2, f3` will be all `16`, because the variable scope is **finalized** in the `return arr;` statement. When the statement `return arr;` is executed, the variable `i` is `4`.

``` javascript
function count() {
    var arr = [];
    var i;
    for (i=1; i<=3; i++) {
        arr.push(function () {
            return i * i;
        });
    }
    return arr;
}

var results = count();
var f1 = results[0];
var f2 = results[1];
var f3 = results[2];
f1();    //16
f2();    //16
f3();    //16
```

To fix the issue in the above case, we can change the code as below. In the below case, the variable `i` is **finalized** and passed to `n` when the statement `(function(n){...})(i);` is executed. When the statement is executed, the variable `i` is `1, 2, 3`.

``` javascript
function count() {
    var arr = [];
    for (var i=1; i<=3; i++) {
        arr.push((function (n) {
            return function () {
                return n * n;
            }
        })(i));
    }
    return arr;
}

var results = count();
var f1 = results[0];
var f2 = results[1];
var f3 = results[2];

f1(); // 1
f2(); // 4
f3(); // 9
```

### Reference

<http://www.ruanyifeng.com/blog/2009/08/learning_javascript_closures.html>
[Closure by Liao Xue Feng](https://www.liaoxuefeng.com/wiki/001434446689867b27157e896e74d51a89c25cc8b43bdb3000/00143449934543461c9d5dfeeb848f5b72bd012e1113d15000)



