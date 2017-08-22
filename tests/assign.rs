#[cfg(test)]

use std::cell::RefCell;
use std::rc::Rc;

extern crate statechart;
use statechart::*;

#[test]
fn assign_string() {
    let sc = states!("S", substates: substates!(
        state!("S1",
            transitions: vec![goto!(target_label: Some("S2".to_string()))],
            on_entry: vec![action_assign!(key: "hello", value: Value::String("assign".to_string()))]),
        final_state!("S2",
            result: Output::ValueOf(ValueOfBuilder::default().key("hello").build().unwrap()))));
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("assign".to_string()));
}