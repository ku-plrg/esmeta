use nodejs 18.14.1 cause 'nodeprof' use that version, 
both checkscripts and externel deps - 'Jalangi2'.

---

Node.js에서 실패하는 test262를 제외하도록 했더니 실패 케이스가 확 줄어드네요. 특히 Promise 관련 테스트가 엄청나게 빠지네요. (동적분석기 테스팅 연구랑 직접적인 관련은 없는 듯 하지만) 왜 그런지 이유를 알아두면 흥미로울 것 같네요… -> 조사해보니 print가 없다거나 하는 이유인 거 같은데.. ESMeta가 harness를 자기 자신에만 맞게 달아줘서 생기는 문제일지도

```json
{
  "Test failed for Jalangi" : [
    "built-ins/Error/prototype/S15.11.3.1_A2_T1.js",
    "built-ins/Object/create/15.2.3.5-4-14.js",
    "built-ins/Object/create/15.2.3.5-4-37.js",
    "built-ins/Object/defineProperty/15.2.3.6-3-228-1.js",
    "built-ins/Object/defineProperty/15.2.3.6-3-258-1.js",
    "built-ins/Object/getOwnPropertyNames/15.2.3.4-2-4.js",
    "built-ins/Object/getOwnPropertyNames/S15.2.3.4_A1_T1.js",
    "built-ins/Object/prototype/valueOf/S15.2.4.4_A14.js",
    "built-ins/eval/no-construct.js",
    "language/block-scope/syntax/redeclaration-global/allowed-to-redeclare-var-with-function-declaration.js",
    "language/block-scope/syntax/redeclaration-global/allowed-to-redeclare-function-declaration-with-var.js",
    "language/expressions/addition/S11.6.1_A3.2_T1.2.js",
    "language/expressions/postfix-decrement/S11.3.2_A4_T2.js",
    "language/expressions/postfix-increment/S11.3.1_A4_T2.js",
    "language/function-code/S10.2.1_A4_T2.js",
    "language/reserved-words/unreserved-words.js",
    "language/statements/for-in/scope-body-var-none.js",
    "language/statements/function/S13.2.1_A6_T3.js",
    "language/statements/function/S13.2.2_A8_T3.js",
    "language/statements/function/S13_A19_T1.js",
    "language/statements/function/S13_A19_T2.js",
    "language/types/object/S8.6_A2_T1.js",
    "language/types/object/S8.6_A3_T1.js"
  ]
}
```

별개로 nodeprof를 환경 설정에 신경 덜 쓰고 돌릴 수 있도록 docker image를 만들었는데 ( https://github.com/ku-plrg/nodeprof-docker ) 실행 시간이 엄청나게 느려지네요 ㅋㅋ 원래 전체 테스트 돌리는게 10분 내외로 끝났는데 nodeprof 넣으니까 3시간 넘게 걸려서 좀 시간을 들이더라도 도커 없이 로컬에서 돌릴 수 있게 엔지니어링을 좀 해야할 것 같습니다..

---

# 20251201

지금까지 read 훅만으로 테스트했던거 정리:

```jsonc
{
  "Test failed for Jalangi" : [
    "built-ins/Error/prototype/S15.11.3.1_A2_T1.js", // (Error objects).stack
    "built-ins/Object/create/15.2.3.5-4-14.js", // (Error objects).stack
    "built-ins/Object/create/15.2.3.5-4-37.js", // (Error objects).stack

    // (???) TypeError: Invalid property descriptor. Cannot both specify accessors and a value or writable attribute,
    "built-ins/Object/defineProperty/15.2.3.6-3-228-1.js",
    "built-ins/Object/defineProperty/15.2.3.6-3-258-1.js",

    "built-ins/Object/getOwnPropertyNames/15.2.3.4-2-4.js", // maximum call stack size exceeded (overriding(?) built-in Object) (???)

    "built-ins/Object/getOwnPropertyNames/S15.2.3.4_A1_T1.js", // Object.prototype.J$IID ... `SPECIAL_PROP_IID` in Jalangi (manually)

    "built-ins/Object/prototype/valueOf/S15.2.4.4_A14.js", // (진짜 잘못된듯???)

    "built-ins/eval/no-construct.js", // `new eval` is not properly handled? J$.instrumentEvalCode call, and no 'constructor' flag for eval (though it is meaningless to call new eval)
    "language/block-scope/syntax/redeclaration-global/allowed-to-redeclare-function-declaration-with-var.js", // syntax error
    "language/block-scope/syntax/redeclaration-global/allowed-to-redeclare-var-with-function-declaration.js", // syntax error
    "language/expressions/addition/S11.6.1_A3.2_T1.2.js", // .toString of function 
    "language/expressions/postfix-decrement/S11.3.2_A4_T2.js", // ++(precision) : (new Number(-0.1)) - 1 + 1
    "language/expressions/postfix-increment/S11.3.1_A4_T2.js", // ++(precision) : (same)
    "language/function-code/S10.2.1_A4_T2.js", // syntax error
    "language/reserved-words/unreserved-words.js", // (kinda) syntax error (assert is already defined)
    "language/statements/for-in/scope-body-var-none.js", // not ES5;
    "language/statements/function/S13.2.1_A6_T3.js", // syntax error
    "language/statements/function/S13.2.2_A8_T3.js", // ???
    "language/statements/function/S13_A19_T2.js", // syntax error
    "language/statements/function/S13_A19_T1.js", // syntax error
    "language/types/object/S8.6_A2_T1.js", // ++ ('string' + 1과 'string' ++ 는 다르다)
    "language/types/object/S8.6_A3_T1.js" // ++
    // syntax error 빼두기
  ]
  // maybe we should "not support" tests calling Object.getOwnPropertyNames or Object.getOwnPropertyName


  // other resolved issues:
  // calling Function() with arguments which have side effects for .toString
  // Function.prototype - .call, .apply

  // Fix test262 harness not to use string templates

  // 훅은 잘란기가 부르는거라 ++가 read에 대해 side effect를 부를 수 있으면 그거를 확인하는게 필요하고
  // 델타디버깅은 데이터를 더 보고 확신이 들면 하고 (데이터가 더 나와야)
  // nodeprof는 빌트인에 대해서 getField를 정확(internal)하게 뽑아주나?
}
```