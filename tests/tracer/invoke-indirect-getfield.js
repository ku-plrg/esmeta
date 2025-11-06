var child = {}

Object.defineProperty(child, "set", {
  get: function() {
    return function(value) { };
  }
});

var newObj = Object.create({}, {
  prop: child
});
