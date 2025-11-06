function aux() {}

function callbackfn() {  aux(); }

Array.prototype.filter.call([1,2,3], callbackfn);