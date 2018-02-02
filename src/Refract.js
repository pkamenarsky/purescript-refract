exports.mapI = function(x) {
  return function(f) {
    var as = new Array(x);
    var i;

    for (i = 0; i < x; i++) {
      as[i] = f(i);
    }

    return as;
  }
}
