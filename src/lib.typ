#let yield(x, state) = (x, state, false)
#let done = (none, none, true)

#let new(state, fn) = (state, fn)

#let next(iter) = {
  let (state, fn) = iter
  let (value, next-state, is-done) = fn(state)
  (value, (next-state, fn), is-done)
}

#let from-fn(fn) = {
  assert.eq(type(fn), function)
  (none, fn)
}

#let from-array(arr) = {
  assert.eq(type(arr), array)
  (0, i => if i < arr.len() { yield(arr.at(i), i + 1) } else { done } )
}

#let range(..args) = {
  assert.eq(args.named(), (:), message: "Unexpected named args for 'range'")
  let args = args.pos()
  assert.ne(args, (), message: "Expected at least one bound for 'range'")
  assert(args.len() <= 3, message: "Expected up to 3 params for 'range'")

  let start = 0
  let stop = 0
  let step = 1

  if args.len() == 1 {
    stop = args.first()
  } else if args.len() == 2 {
    (start, stop) = args
  } else if args.len() == 3 {
    (start, stop, step) = args
  }

  (start, i => if i <= stop { yield(i, i + step) } else { done })
}

#let map(iterator, fn: none) = {
  assert.eq(type(fn), function)
  let (initial-state, next) = iterator
  (
    initial-state,
    state => {
      let (value, next-state, is-done) = next(state)
      if is-done {
        done
      } else {
        yield(fn(value), next-state)
      }
    }
  )
}

#let filter(iterator, pred: none) = {
  assert.eq(type(pred), function)
  let (initial-state, next) = iterator
  (
    initial-state,
    state => {
      let state = state
      while true {
        let (value, next-state, is-done) = next(state)
        if is-done {
          break
        } else if pred(value) {
          return yield(value, next-state)
        }

        state = next-state
      }

      // Searched all remaining items, nothing found
      done
    }
  )
}

#let inspect(iterator, fn: none) = {
  assert.eq(type(fn), function)
  let (initial-state, next) = iterator
  (
    initial-state,
    state => {
      let (value, next-state, is-done) = next(state)
      if is-done {
        done
      } else {
        fn(value)
        yield(value, next-state)
      }
    }
  )
}

#let to-array(iterator) = {
  let (state, next) = iterator
  let arr = ()
  while true {
    let (value, next-state, is-done) = next(state)
    if is-done {
      break
    }

    state = next-state
    arr.push(value)
  }

  arr
}

#let chain(iter, ..fn) = {
  let fns = fn.pos().rev()
  let current = iter
  while fns.len() > 0 {
    current = (fns.pop())(current)
  }
  current
}
