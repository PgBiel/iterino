#let yield(x, state) = (x, state, false)
#let done = (none, none, true)

#let new(state, fn, ..props) = (state, fn, props.named())

#let next(iter) = {
  let (state, fn, props) = iter
  let (value, next-state, is-done) = fn(state)
  (value, (next-state, fn, props), is-done)
}

#let from-fn(fn) = {
  assert.eq(type(fn), function)
  (none, fn, (:))
}

#let from-array(arr) = {
  assert.eq(type(arr), array)
  (0, i => if i < arr.len() { yield(arr.at(i), i + 1) } else { done }, (:))
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

  (start, i => if i <= stop { yield(i, i + step) } else { done }, (:))
}

// Apply a transformation to an iterator
// Returns a combined iterator which applies all previous
// consecutive transformations more efficiently than through recursion
// Uses a for loop which takes the value from the previous step and
// provides to the next directly
#let transform(iter, initial-state, fn, recursive: false) = {
  let (input-state, next, props) = iter
  if recursive {
    // Keeping the original version because why not
    return (
      (input-state, initial-state),

      ((input-state, state)) => {
        let (value, input-state, is-done) = next(input-state)
        if is-done {
          return done
        }

        let (new-value, new-state, is-done) = fn(value, state)
        if is-done {
          done
        } else {
          yield(new-value, (input-state, new-state))
        }
      },

      (:)
    )
  }

  let combined = props.at("_combined", default: none)
  let (input-state, next, initial-states, fns, props) = if combined != none {
    // Merge with previous transformations
    // Get their initial states, functions and combined properties
    combined
  } else {
    // Initialize combined data
    (input-state, next, (), (), (:))
  }

  // Append our own initial state and function
  // so it is used and the function is run after the others
  initial-states.push(initial-state)
  fns.push(fn)

  // Create combined iterator
  (
    // The state is the combined state of each transformation,
    // plus the initial iterator's state
    (input-state, initial-states),

    ((input-state, states)) => {
      let (value, input-state, is-done) = next(input-state)
      if is-done {
        return done
      }

      let i = 0
      for transformation in fns {
        let (new-value, new-state, is-done) = transformation(value, states.at(i))
        if is-done {
          return done
        } else {
          value = new-value
          states.at(i) = new-state
        }
      }

      yield(value, (input-state, states))
    },
    (
      _combined: (
        input-state,
        next,
        initial-states,
        fns,
        (:)
      )
    )
  )
}

#let map(iterator, fn: none, recursive: false) = {
  assert.eq(type(fn), function)
  transform(iterator, none, (v, s) => {
    yield(fn(v), s)
  }, recursive: recursive)
}

#let filter(iterator, pred: none, recursive: false) = {
  assert.eq(type(pred), function)
  let (initial-state, next, props) = iterator
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
    },
    (:)
  )
}

#let inspect(iterator, fn: none) = {
  assert.eq(type(fn), function)
  let (initial-state, next, props) = iterator
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
    },
    (:)
  )
}

#let to-array(iterator) = {
  let (state, next, props) = iterator
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
