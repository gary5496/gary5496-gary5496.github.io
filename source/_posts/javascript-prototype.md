---
title: Prototype in JavaScript
tags: [JavaScript]
date: 2018-03-20 11:26:49
categories: JavaScript
---

### Introduction

To simulate the OO class feature and implement the inheritance functionality in JavaScript, we have the below methods.

### Methods

#### 1. Inherit by calling parent constructor function

Using this method, all the properties and methods of parent function will be copied to the child objects. 

##### Cons
1. It will cost more memory if the parent function has lots of properties and methods or the inheritance chain is long.
2. The prototype chain is not correct.

``` javascript
function Animal(species){
    this.species = species;
}

function Cat(name, color){
    Animal.call(this, "Cat");
    this.name = name;
    this.color = color;
}
var cat1 = new Cat("Kitty","Yellow");

console.log(cat1.species);    //Cat
console.log(cat1.__proto__ === Cat.prototype);  //true
console.log(cat1.__proto__.__proto__ === Object.prototype);  //true
```

In this case, the prototype chain will be:

	new Cat() ----> Cat.prototype ----> Object.prototype ----> null

#### 2. Inherit with one parent object 

Using this method, **one parent object** will be created to save all the properties and methods of parent function, and the parent object is linked to `__proto__` property of the child objects. 

##### Pros:
1. It will cost less memory when **creating** new child objects if the parent function has lots of properties and methods or the inheritance chain is long.
2. The prototype chain is correct.

##### Cons:
1. It will cost more memory when **defining** the child function.
2. Since one parent object is linked to multiple child objects, the change to the parent object will affect multiple child objects.

``` javascript
function Animal(species){
    this.species = species;
}

function Cat(name, color){
    this.name = name;
    this.color = color;
}

Cat.prototype = new Animal({species: "Cat"});
Cat.prototype.constructor = Cat;

var cat1 = new Cat("Kitty","Yellow");
var cat2 = new Cat("Miao","White");

function Dog(name, color){
    this.name = name;
    this.color = color;
}

Dog.prototype = new Animal({species: "Dog"});
Dog.prototype.constructor = Dog;

var dog1 = new Dog("Wan","Yellow");

console.log(cat1.species.species);    //Cat
console.log(cat1.__proto__ === cat2.__proto__);   //true
console.log(cat1.__proto__ === Cat.prototype);  //true
console.log(cat1.__proto__.__proto__ === Animal.prototype);  //true
console.log(dog1.species.species);    //Dog
console.log(Dog.prototype === Cat.prototype);   //false

var sp = cat1.species;
sp.species = "New Cat";
console.log(cat1.species.species);    //New Cat  
console.log(cat2.species.species);    //New Cat  
```

In this case, the prototype chain will be:

	new Cat() ----> Cat.prototype ----> Animal.prototype ----> Object.prototype ----> null

#### 3. Inherit with new function object - Recommended

Using this method, all the properties and methods of parent function will be copied to the child objects.

##### Pros: 
1. It will cost less memory when **defining** the child function, since only one blank function object is created.
2. The prototype chain is correct.
3. The change to one child object will not affect other child objects.

##### Cons:
1. It will cost more memory when **creating** new child objects, since all the properties and methods of parent function will be copied to the child objects, instead of linking one parent object to the child objects.
 

```
function Animal(species){
    this.species = species;
}

function Cat(name, color){
    Animal.call(this, {species: "Cat"});
    this.name = name;
    this.color = color;
}

var F = function(){};
F.prototype = Animal.prototype;
Cat.prototype = new F();
Cat.prototype.constructor = Cat;

var cat1 = new Cat("Kitty","Yellow");
var cat2 = new Cat("Miao","White");

function Dog(name, color){
    Animal.call(this, {species: "Dog"});
    this.name = name;
    this.color = color;
}

var F = function(){};
F.prototype = Animal.prototype;
Dog.prototype = new F();
Dog.prototype.constructor = Dog;

var dog1 = new Dog("Wan","Yellow");

console.log(cat1.species.species);    //Cat
console.log(cat1.__proto__ === cat2.__proto__);   //true
console.log(cat1.__proto__ === Cat.prototype);  //true
console.log(cat1.__proto__.__proto__ === Animal.prototype);  //true
console.log(dog1.species.species);    //Dog
console.log(Dog.prototype === Cat.prototype);   //false

var sp = cat1.species;
sp.species = "New Cat";
console.log(cat1.species.species);    //New Cat  
console.log(cat2.species.species);    //Cat 
```

In this case, the prototype chain will be:

	new Cat() ----> Cat.prototype ----> Animal.prototype ----> Object.prototype ----> null

### Notes

- When create a new object using constructor function, the `__proto__` of the new created object will point to the same object of `ConstructorFunction.prototype`.

``` javascript
function Animal(species){
    this.species = species;
}

var animal = new Animal("Cat");
console.log(animal.__proto__ === Animal.prototype);  //true
```

- We can create a common function for the inheritance in the third method. 

``` javascript
function Animal(species){
    this.species = species;
}

function Cat(name, color){
    Animal.call(this, {species: "Cat"});
    this.name = name;
    this.color = color;
}

function inherits(Child, Parent) {
    var F = function () {};
    F.prototype = Parent.prototype;
    Child.prototype = new F();
    Child.prototype.constructor = Child;
    Child.uber = Parent.prototype;	
}

inherits(Cat, Animal);

var cat1 = new Cat("Kitty","Yellow");
var cat2 = new Cat("Miao","White");

function Dog(name, color){
    Animal.call(this, {species: "Dog"});
    this.name = name;
    this.color = color;
}

inherits(Dog, Animal);

var dog1 = new Dog("Wan","Yellow");
```

- We can define the new methods for the parent function and child function, in the prototype of the constructor function.

```javascript
Animal.prototype.getspecies = function(){
    return this.species.species;
};

Dog.prototype.getname = function(){
    return this.name;
};

console.log(dog1.getspecies()); //Dog
console.log(dog1.getname());    //Wan
```


### Reference

<http://www.ruanyifeng.com/blog/2010/05/object-oriented_javascript_inheritance.html>
[Prototype Inheritance by Liao Xue Feng](https://www.liaoxuefeng.com/wiki/001434446689867b27157e896e74d51a89c25cc8b43bdb3000/0014344997013405abfb7f0e1904a04ba6898a384b1e925000)