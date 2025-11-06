

var base = {};
var internal = 0;

Object.defineProperty(base, 'foo', {
  get: function() {
    return internal;
  },
  set: function(val) {
    internal = val;
  },
  configurable: true
});

base.foo += 42;
