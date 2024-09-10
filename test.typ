#import "iterino.typ" as iter

#let i = iter.from-array((1, 2, 3))
#let i = iter.map(i, fn: x => x + 1)
#assert.eq(iter.to-array(i), (2, 3, 4))

#let i = iter.chain(
  iter.range(100),
  iter.map.with(fn: x => x + 1),
  iter.filter.with(pred: n => n != 3),
  iter.inspect.with(fn: v => if v == 3 { panic("don't") }),
  iter.to-array
)
#assert.eq(i, (2, 4))
