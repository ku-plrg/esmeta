var obj = {}
obj[Symbol.toPrimitive] = function(hint) {
  // print('toPrimitive called with hint:', hint);
  return hint === 'number' ? 42 : 'default';
}

function identity(x) { return x; }

identity(++obj);