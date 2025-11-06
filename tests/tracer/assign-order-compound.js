var order = [];

var base = null;
var prop = {
  toString: function() {
    // throw new Test262Error("property key evaluated");
    order.push('prop toString');
    return 'prop';
  }
};
var expr = function() {
  // throw new Test262Error("right-hand side expression evaluated");
  order.push('expr evaluated');
  return 42;
};
  
try {
    base[prop] += expr();
  } catch (e) { }
  
print(order.toString());