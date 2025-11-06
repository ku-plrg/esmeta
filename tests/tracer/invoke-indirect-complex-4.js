function callbackfn() { }

var proto = {};

Object.defineProperty(proto, "1", {
  get: function() {
    return 11;
  }
});

var child = Object.create(proto);
child.length = 20;
var newArr = Array.prototype.filter.call(child, callbackfn);
