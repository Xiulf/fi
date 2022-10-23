function toString(x) {
  return x.toString();
}

function push(a, x) {
  a.push(x);
  return a;
}

function concatString(a, b) {
  return a + b.toString();
}

function pureEffect(x) {
  return function() {
    return x;
  };
}

function bindEffect(a) {
  return function(f) {
    return function() {
      return f(a())();
    };
  };
}

function reportEffect(r) {
  return function(e) {
    return r.report(e());
  };
}
