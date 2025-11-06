var obj = {};

obj[Symbol.toPrimitive] = function () { return 42; };

var x = obj[Symbol.toPrimitive];

x();