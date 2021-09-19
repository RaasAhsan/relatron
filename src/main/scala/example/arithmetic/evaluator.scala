package example
package arithmetic

import Core._

// one-step evaluation
def eval(t1: Term[Node], t2: Term[Node]): Goal =
  (
    (t1 === isZero(zero) && t2 === trueBool) ||
    fresh[Node] { k => t1 === isZero(succ(k)) && t2 === falseBool && numeric(k) } ||
    fresh[Node] { k => t1 === pred(succ(k)) && t2 === k && numeric(k) } ||
    fresh[Node, Node] { (k, t) => t1 === isZero(k) && t2 === isZero(t) && eval(k, t) && node(k) && node(t) } ||
    fresh[Node, Node] { (s2, s3) => t1 === test(trueBool, s2, s3) && t2 === s2 && node(s2) && node(s3) } ||
    fresh[Node, Node] { (s2, s3) => t1 === test(falseBool, s2, s3) && t2 === s3 && node(s2) && node(s3) } ||
    fresh[Node, Node, Node, Node] { (s1, s2, s3, s1e) => t1 === test(s1, s2, s3) && t2 === test(s1e, s2, s3) && node(s1) && node(s2) && node(s3) && node(s1e) && eval(s1, s1e) }
  )

/*
||
    fresh[Node, Node, Node, Node] { (s1, s2, s3, s1e) => t1 === test(s1, s2, s3) && t2 === test(s1e, s2, s3) && node(s1) && node(s2) && node(s3) && node(s1e) && eval(s1, s1e) }
*/

// multi-step evaluation
def multiEval(t1: Term[Node], t2: Term[Node]): Goal = 
  t1 === t2 || fresh[Node] { tk =>
    eval(t1, tk) && multiEval(tk, t2)
  }
