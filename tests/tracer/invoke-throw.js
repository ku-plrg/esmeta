function f() {
  throw 42;
}

try {
  f();
} catch (e) {}