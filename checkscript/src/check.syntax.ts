import type { Program } from "acorn";
import * as walk from "acorn-walk";

export function checkSyntaxErrorAndThrow(ast: Program): void {
  if (checkSyntaxErrorRefer(ast)) {
    throw new Error('early error (syntax error) ESMeta not supported.');
  }
}

function checkSyntaxErrorRefer(ast: Program): boolean {
  let has = false;

  walk.ancestor<unknown>(ast, {
    Identifier(node, state, ancestors) {
      // maybe there are better ways to detect SyntaxError tests, but for now this works
      if (node.name === 'SyntaxError') {
        has = true;
      }
    }
  });

  return has;
}