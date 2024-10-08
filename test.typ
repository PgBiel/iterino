#import "src/lib.typ" as iter

#let i = iter.from-array((1, 2, 3))
#let i = iter.map(i, fn: x => x + 1)
#assert.eq(iter.to-array(i), (2, 3, 4))

#let i = iter.chain(
  iter.range(10),
  iter.map.with(fn: x => x + 1),
  iter.filter.with(pred: n => n != 3),
  iter.inspect.with(fn: v => if v == 3 { panic("don't") }),
  iter.to-array
)
#assert.eq(i, (1, 2, 4, 5, 6, 7, 8, 9, 10, 11))
