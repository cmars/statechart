#[cfg(test)]

use std::cell::RefCell;
use std::rc::Rc;

extern crate statechart;
use statechart::*;

#[test]
fn assign_string() {
    let sc = State::Compound(CompoundBuilder::default()
        .label("S")
        .substates(RefCell::new(vec![Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S1")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("S2".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_entry(vec![Action::Assign(AssignBuilder::default()
                                                            .key("hello")
                                                            .value(Value::String("assign".to_string()))
                                                            .build()
                                                            .unwrap())])
                                         .build()
                                         .unwrap())),
                                     Rc::new(State::Final(FinalBuilder::default()
                                         .label("S2")
                                         .result(Output::ValueOf(ValueOfBuilder::default().key("hello").build().unwrap()))
                                         .build()
                                         .unwrap()))]))
        .build()
        .unwrap());
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("assign".to_string()));
}
