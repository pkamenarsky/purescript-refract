exports.mapI = function(x) {
  return function(f) {
    var as = new Array(x);
    var i;

    for (i = 0; i < x; i++) {
      as[i] = f(i);
    }

    return as;
  };
};

exports.memo = function(f) {
  var old_a = null;
  var old_b = null;

  return function(a) {
    if (old_a !== a) {
      old_a = a;
      old_b = f(a);

      return old_b;
    }
    else {
      return old_b;
    }
  };
};

exports.refEq = function(a) {
  return function(b) {
    console.log("EQ", a, b);
    return a === b;
  };
};

exports.logAny = function(a) {
  return function() {
    console.log(a);
  };
};
