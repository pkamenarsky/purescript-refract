'use strict';

var css = require('../static/style.css');
var entry = require('../test/examples/Counters.purs');

var subscribe = function(st) {
  return function() {
    // localStorage.setItem('__refract_state', JSON.stringify(st));
  }
}

// entry.main(JSON.parse(localStorage.getItem('__refract_state')) || entry.initialState)(subscribe)();
// entry.main(entry.initialState)(subscribe)();
entry.main();

if (module.hot) {
  module.hot.accept();
}
