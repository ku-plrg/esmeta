var newObj = Object.create({}, {
  prop: {
    get: function() {
      return "present";
    }
  }
});

newObj.prop;