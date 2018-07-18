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

exports.memo_ = function(f) {
  var old_a = null;
  var old_b = null;

  return function(a) {
    console.log("OLD", old_a);
    console.log("NEW", a);

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

exports.memo = function(f) {
  var m = new Map();

  return function(a) {
    if (m[a]) {
      return m[a];
    }
    else {
      m[a] = f(a);
      return m[a];
    }
  };
};


exports.memo2_ = function(f) {
  var old_a = null;
  var old_b = null;
  var old_c = null;

  return function(a) {
    return function(b) {
      console.log("OLD A", old_a);
      console.log("NEW A", a, old_a === a);

      console.log("OLD B", old_b);
      console.log("NEW B", b, old_b === b);

      if (/*old_a !== a || */ old_b !== b) {
        old_a = a;
        old_b = b;
        old_c = f(a)(b);

        return old_c;
      }
      else {
        return old_c;
      }
    };
  };
};

exports.memo2 = function(f) {
  var m = new Map();

  return function(a) {
    return function(b) {
      if (m[a]) {
        if (m[a][b]) {
          return m[a][b];
        }
        else {
          m[a][b] = f(a)(b);
          return m[a][b];
        }
      }
      else {
        m[a] = new Map();
        m[a][b] = f(a)(b);

        return m[a][b];
      }
    };
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
