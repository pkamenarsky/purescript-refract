exports.lensget =
  function(dictRecordToLens) {
    return function(view) {
      return function(r) {
        return function(st) {
          var obj = {};

          Object.keys(r).forEach(function(k) {
            obj[k] = view(r[k])(st);
          });

          return obj;
        }
      }
    }
  };

exports.lensset =
  function(dictRecordToLens) {
    return function(set) {
      return function(r) {
        return function(st) {
          return function(a) {
            var obj = st;

            // TODO: only set changed values?
            Object.keys(r).forEach(function(k) {
              obj = set(r[k])(a[k])(obj);
            });

            return obj;
          }
        }
      }
    }
  };
