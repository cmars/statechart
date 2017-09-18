#[cfg(test)]

extern crate statechart;
use statechart::*;

#[test]
fn common_ancestor() {
    let l = vec![0, 1, 1, 3, 4];
    let r = vec![0, 1, 2, 3, 1];
    let a = Interpreter::common_ancestor(&l, &r);
    assert_eq!(a, vec![0, 1]);
    let a = Interpreter::common_ancestor(&r, &l);
    assert_eq!(a, vec![0, 1]);

    let l = vec![0, 1, 1];
    let r = vec![0, 1, 1, 2, 0];
    let a = Interpreter::common_ancestor(&l, &r);
    assert_eq!(a, vec![0, 1, 1]);
    let a = Interpreter::common_ancestor(&r, &l);
    assert_eq!(a, vec![0, 1, 1]);

    let l = vec![];
    let r = vec![0, 1];
    let a = Interpreter::common_ancestor(&l, &r);
    assert_eq!(a, vec![]);
    let a = Interpreter::common_ancestor(&r, &l);
    assert_eq!(a, vec![]);
}

#[test]
fn exit_states() {
    let l = vec![0, 1, 1, 3, 4];
    let r = vec![0, 1, 1];
    let exits = Interpreter::exit_states(&l, &r);
    assert_eq!(exits, vec![vec![0, 1, 1, 3, 4], vec![0, 1, 1, 3]]);
    let exits = Interpreter::exit_states(&r, &l);
    assert!(exits.is_empty());
}

#[test]
fn entry_states() {
    let l = vec![0, 1, 1];
    let r = vec![0, 1, 1, 3, 4];
    let entries = Interpreter::entry_states(&l, &r);
    assert_eq!(entries, vec![vec![0, 1, 1, 3], vec![0, 1, 1, 3, 4]]);
    let entries = Interpreter::entry_states(&r, &l);
    assert!(entries.is_empty());
}
