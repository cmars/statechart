#[cfg(test)]

use std::cell::RefCell;
use std::rc::Rc;

extern crate statechart;
use statechart::*;

fn a_eq_x(x: i32) -> Rc<Fn(&Context) -> bool> {
    Rc::new(move |ctx: &Context| -> bool {
        match ctx.get_var("a") {
            Some(&Value::Int(n)) => n == x,
            _ => false,
        }
    })
}

#[test]
fn first_choice_only_match() {
    let sc = State::Compound(CompoundBuilder::default()
        .label("S")
        .substates(RefCell::new(vec![Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S0")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("SF".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_entry(vec![Action::Assign(AssignBuilder::default()
                                                            .key("a")
                                                            .value(Value::Int(1))
                                                            .build()
                                                            .unwrap())])
                                         .on_exit(vec![Action::Choose(ChooseBuilder::default().when(vec![
                                            (Condition::Fn(CondFn::new(a_eq_x(1))), Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("matched".to_string())).build().unwrap())))
                                            ]).build().unwrap())]).build().unwrap())),
                                    Rc::new(State::Final(FinalBuilder::default()
                                         .label("SF")
                                         .result(Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))
                                         .build()
                                         .unwrap()))]))
        .build()
        .unwrap());
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn last_match() {
    let sc = State::Compound(CompoundBuilder::default()
        .label("S")
        .substates(RefCell::new(vec![Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S0")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("SF".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_entry(vec![Action::Assign(AssignBuilder::default()
                                                            .key("a")
                                                            .value(Value::Int(2))
                                                            .build()
                                                            .unwrap())])
                                         .on_exit(vec![
                                         Action::Choose(ChooseBuilder::default().when(vec![
                                            (Condition::Fn(CondFn::new(a_eq_x(1))), Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("not matched".to_string())).build().unwrap()))),
                                            (Condition::Fn(CondFn::new(a_eq_x(2))), Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("matched".to_string())).build().unwrap()))),
                                            ]).build().unwrap()),
                                        ]).build().unwrap())),
                                    Rc::new(State::Final(FinalBuilder::default()
                                         .label("SF")
                                         .result(Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))
                                         .build()
                                         .unwrap())),
                                         ]))
        .build()
        .unwrap());
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn otherwise() {
    let sc = State::Compound(CompoundBuilder::default()
        .label("S")
        .substates(RefCell::new(vec![Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S0")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("SF".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_exit(vec![
                                         Action::Choose(ChooseBuilder::default().when(vec![
                                            (Condition::Fn(CondFn::new(a_eq_x(1))), Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("not matched".to_string())).build().unwrap()))),
                                            (Condition::Fn(CondFn::new(a_eq_x(2))), Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("not matched".to_string())).build().unwrap()))),
                                            ]).otherwise(Some(Box::new(Action::Assign(
                                                    AssignBuilder::default().key("b").value(
                                                        Value::String("matched".to_string())).build().unwrap()))))
                                            .build().unwrap()),
                                        ]).build().unwrap())),
                                    Rc::new(State::Final(FinalBuilder::default()
                                         .label("SF")
                                         .result(Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))
                                         .build()
                                         .unwrap())),
                                         ]))
        .build()
        .unwrap());
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}
