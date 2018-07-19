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

exports.memo = function(f) {
  var old_a = null;
  var r;

  return function(a) {
    console.log("memo", old_a, a, old_a === a);

    if (old_a !== a) {
      old_a = a;
      r = f(a);
    }

    return r;
  };
};

exports.memo2 = function(f) {
  var old_a = null;
  var old_b = null;
  var r;

  return function(a) {
    return function(b) {
      if (old_a !== a || old_b !== b) {
        old_a = a;
        old_b = b;
        r = f(a)(b);
      }

      return r;
    };
  };
};

exports.memo3 = function(f) {
  var old_a = null;
  var old_b = null;
  var old_c = null;
  var r;

  return function(a) {
    return function(b) {
      return function(c) {
        if (old_a !== a || old_b !== b || old_c !== c) {
          old_a = a;
          old_b = b;
          old_c = c;
          r = f(a)(b)(c);
        }

        return r;
      };
    };
  };
};
