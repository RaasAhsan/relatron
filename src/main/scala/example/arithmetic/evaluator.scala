package example
package arithmetic

import Core._

// one-step evaluation
def eval(t1: Term[Node], t2: Term[Node]): Goal =
  (fresh[Node] { k => t1 === isZero(k) && k === zero && t2 === trueBool } ||
  fresh[Node, Node] { (k, t) => t1 === isZero(k) && k === succ(t) && t2 === falseBool && node(t) } ||
  fresh[Node, Node] { (s2, s3) => t1 === test(trueBool, s2, s3) && t2 === s2 } ||
  fresh[Node, Node] { (s2, s3) => t1 === test(falseBool, s2, s3) && t2 === s3 } ||
  fresh[Node, Node, Node, Node] { (s1, s2, s3, s1e) => t1 === test(s1, s2, s3) && t2 === test(s1e, s2, s3) && eval(s1, s1e) }) && node(t1) && node(t2)

// multi-step evaluation
def multiEval(t1: Term[Node], t2: Term[Node]): Goal = 
  t1 === t2 || fresh[Node] { tk =>
    eval(t1, tk) && multiEval(tk, t2)
  }
