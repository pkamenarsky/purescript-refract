var id = 0;

exports.genId = function() {
  id++;
  return "RefractComponent_" + id;
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
