globalThis.__level = globalThis.__level ?? 0;
globalThis.print = function print() {
  var str = [...arguments].join(" ");
  console.log(`{ "type" : "print", "text" : ${JSON.stringify(str)}, "level" : ${__level} }`);
};