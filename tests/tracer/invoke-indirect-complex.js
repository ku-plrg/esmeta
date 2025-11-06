var myobj2 = {
  toNumber: function() {
    return 12345;
  },
  toString: function() {
    return {}
  },
  valueOf: function() {
    return "[object MyObj]";
  }
};

String(myobj2);