package example
package arithmetic

import Core._

// typing relation
def typing(t: Term[Node], tty: Term[Type]): Goal =
  ((t === zero && tty === typeNat) ||
  fresh[Node] { (k) => t === succ(k) && tty === typeNat && typing(k, typeNat) } ||
  (t === trueBool && tty === typeBool) ||
  (t === falseBool && tty === typeBool) ||
  fresh[Node] { k => t === isZero(k) && tty === typeBool && typing(k, typeNat) } ||
  fresh[Node, Type, Node, Type, Node, Type] { (a, aty, b, bty, c, cty) => t === test(a, b, c) && aty === typeBool && bty === cty && tty === bty && typing(a, aty) && typing(b, bty) && typing(c, cty) }) && node(t) && typeNode(tty)
