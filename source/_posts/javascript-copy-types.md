---
title: Shadow copy and Deep copy in JavaScript
tags: [JavaScript]
date: 2018-03-20 15:57:50
categories: JavaScript
---


### Shadow Copy

In a shadow copy, the `simple value` properties of the source object is copied to the target object. However, for those `object` properties and methods of source object, the reference is assigned to the target object, instead of copy.

```javascript
var Chinese = {
    nation: "Chinese",
    birthPlace: ["Beijing", "Shanghai"]
};
var Doctor = {
    career: "Doctor"
};
function extendCopy(p) {
    var c = {};
    for (var i in p) { 
        c[i] = p[i];
    }    
    c.uber = p;    
    return c;
}

var Doctor = extendCopy(Chinese);

Doctor.birthPlace.push("Hongkong");
console.log(Doctor.birthPlace);     //["Beijing", "Shanghai", "Hongkong"]
console.log(Chinese.birthPlace);    //["Beijing", "Shanghai", "Hongkong"]
```

### Deep Copy

In a deep copy, all the properties and methods of the source object is copied to the target object.

``` javascript
var Chinese = {
    nation: "Chinese",
    birthPlace: ["Beijing", "Shanghai"]
};
var Doctor = {
    career: "Doctor"
};
function deepCopy(p, c) {
    var c = c || {};
    for (var i in p) {
        if (typeof p[i] === 'object') {
            c[i] = (p[i].constructor === Array) ? [] : {};
            deepCopy(p[i], c[i]);
        } else {
            c[i] = p[i];
        }
    }
    return c;
}

var Doctor = deepCopy(Chinese);

Doctor.birthPlace.push("Hongkong");
console.log(Doctor.birthPlace);     //["Beijing", "Shanghai", "Hongkong"]
console.log(Chinese.birthPlace);    //["Beijing", "Shanghai"]
```

### Reference

<http://www.ruanyifeng.com/blog/2010/05/object-oriented_javascript_inheritance_continued.html>