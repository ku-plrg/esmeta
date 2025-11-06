var newObj = Object.create({}, {
  prop: {
    get: function() {
      return "present";
    }
  }
});

function identity(x) { return x; }

identity(newObj.prop);