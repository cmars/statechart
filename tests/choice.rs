#[cfg(test)]

use std::rc::Rc;

extern crate statechart;
use statechart::*;

fn a_eq_x(x: i32) -> Condition {
    cond_fn!(move |ctx: &Context| -> bool {
        match ctx.get_var("a") {
            Some(&Value::Int(n)) => n == x,
            _ => false,
        }
    })
}

#[test]
fn first_choice_only_match() {
    let sc = states!{ S {
        substates: [
            state!{ S0 {
                transitions: [goto!(target: SF)],
                on_entry: [action_assign!(key: "a", value: Value::Int(1))],
                on_exit: [action_choose!(when: vec![
                    (a_eq_x(1),
                        Box::new(action_assign!(key: "b",
                            value: Value::String("matched".to_string()))))])],
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn last_match() {
    let sc = states!{ S {
        substates: [
            state!{ S0 {
                transitions: [goto!(target: SF)],
                on_entry: [action_assign!(key: "a", value: Value::Int(2))],
                on_exit: [action_choose!(when: vec![
                    (a_eq_x(1), Box::new(action_assign!(
                        key: "b", value: Value::String("not matched".to_string())))),
                    (a_eq_x(2), Box::new(action_assign!(
                        key: "b", value: Value::String("matched".to_string())))),
                ])],
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn otherwise() {
    let sc = states!{ S {
        substates: [
            state!{ S0 {
                transitions: vec![goto!(target: SF)],
                on_exit: vec![action_choose!(
                    when: vec![
                        (a_eq_x(1), Box::new(action_assign!(
                            key: "b", value: Value::String("not matched".to_string())))),
                        (a_eq_x(2), Box::new(action_assign!(
                            key: "b", value: Value::String("not matched".to_string())))),
                    ],
                    otherwise: Some(Box::new(action_assign!(
                        key: "b", value: Value::String("matched".to_string())))))],
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}
