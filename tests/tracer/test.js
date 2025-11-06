// from test/built-ins/Object/create/15.2.3.5-4-271.js

var data1 = "data";
var data2 = "data";

var proto = {};
Object.defineProperty(proto, "set", {
  get: function() {
    return function(value) {
      data2 = value;
    };
  }
});

var ConstructFun = function() {};
ConstructFun.prototype = proto;
var child = new ConstructFun();
Object.defineProperty(child, "set", {
  value: function(value) {
    data1 = value;
  }
});

var newObj = Object.create({}, {
  prop: child
});

var hasProperty = newObj.hasOwnProperty("prop");

newObj.prop = "overrideData";