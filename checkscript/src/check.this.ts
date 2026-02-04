import type { Program } from "acorn";
import * as walk from "acorn-walk";

export function checkTopLevelThisAndThrow(ast: Program): void {
  if (checkTopLevelThis(ast)) {
    throw new Error('Top-level "this" is not allowed, (script-cjs diff).');
  }
}

function checkTopLevelThis(ast: Program): boolean {
  let has = false;
  
  walk.ancestor<unknown>(ast, {
    ThisExpression(node, state, ancestors) {

      const containsAtLeastOneScope = ancestors.some((node, index) => {
        return node.type === 'FunctionDeclaration' ||
          node.type === 'FunctionExpression' ||
          node.type === 'ClassDeclaration' ||
          node.type === 'ClassExpression';
        // 'ArrowFunctionExpression' does NOT create own `this`
      })

      if (!containsAtLeastOneScope) {
        has = true;
      }
    },
  });

  return has;
}