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

exports.refEq = function(a) {
  return function(b) {
    console.log("EQ", a, b, a === b);
    return a === b;
  };
};

exports.logAny = function(a) {
  return function() {
    console.log(a);
  };
};

exports.showAny = function(a) {
  return a.toString;
};

exports.trace = function(str) {
  return function(a) {
    console.log(str);
    return a;
  };
};
