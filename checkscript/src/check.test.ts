import { describe, it } from 'node:test';
import assert from 'node:assert';
import { parseES5 } from './check.js';
import { checkTopLevelThisAndThrow } from './check.this.js';
import * as walk from 'acorn-walk';

describe('ES5 Parser', () => {
  it('', () => {
    const code = `var test = (function() { return this })();`;
    const result = parseES5(code);
    console.log(result);

    walk.ancestor<unknown>(result, {
      ThisExpression(node, state, ancestors) {
        console.log("Found 'this' expression:", node, "Ancestors:", ancestors);
        },
      });

    var d = undefined;
    try {
      checkTopLevelThisAndThrow(result);
    } catch (e) {
      d = e;
    }
    assert.strictEqual(d, undefined);
  });
}); 