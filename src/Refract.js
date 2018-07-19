var id = 0;

exports.genId = function() {
  id++;
  return "RefractComponent_" + id;
};

exports.refEq_ = function(a) {
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

/**
 * inlined Object.is polyfill to avoid requiring consumers ship their own
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is
 */
function is(x, y) {
  // SameValue algorithm
  if (x === y) {
    // Steps 1-5, 7-10
    // Steps 6.b-6.e: +0 != -0
    // Added the nonzero y check to make Flow happy, but it is redundant
    return x !== 0 || y !== 0 || 1 / x === 1 / y;
  } else {
    // Step 6.a: NaN == NaN
    return x !== x && y !== y;
  }
}

/**
 * Performs equality by iterating through keys on an object and returning false
 * when any key has values which are not strictly equal between the arguments.
 * Returns true when the values of all keys are strictly equal.
 */
exports.refEq = function(objA) {
  return function(objB) {
    console.log("EQ", objA, objB, objA === objB);

    if (is(objA, objB)) {
      return true;
    }

    if (typeof objA !== 'object' || objA === null || typeof objB !== 'object' || objB === null) {
      return false;
    }

    var keysA = Object.keys(objA);
    var keysB = Object.keys(objB);

    if (keysA.length !== keysB.length) {
      return false;
    }

    // Test for A's keys different from B.
    for (var i = 0; i < keysA.length; i++) {
      if (!hasOwnProperty.call(objB, keysA[i]) || !is(objA[keysA[i]], objB[keysA[i]])) {
        return false;
      }
    }

    return true;
  };
};
