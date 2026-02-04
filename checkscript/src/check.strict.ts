import type { Program } from "acorn";
import * as walk from "acorn-walk";

export function checkUseStrictDirectiveAndThrow(ast: Program): void {
  if (checkUseStrictDirective(ast)) {
    throw new Error('use strict is not allowed (ESMeta not supported).');
  }
}

function checkUseStrictDirective(ast: Program): boolean {
  let has = false;

  walk.ancestor<unknown>(ast, {
    Literal(node, state, ancestors) {
      // No-op for literals
      if (node.value === 'use strict') {
        // 'use strict' directive found
        has = true;
      }

      let nv = node.value;
      if (typeof nv === 'string') nv = nv.trimStart();
      if (typeof nv === 'string' && (nv.startsWith('"use strict"') || nv.startsWith("'use strict'"))) {
        has = true;
      }
    }
  });

  return has;
}