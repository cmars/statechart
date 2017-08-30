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
                on_exit: [choose!(when: [
                    (a_eq_x(1),
                        action_assign!(key: "b", value: Value::from_str("matched")))])]
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::from_str("matched"));
}

#[test]
fn last_match() {
    let sc = states!{ S {
        substates: [
            state!{ S0 {
                transitions: [goto!(target: SF)],
                on_entry: [action_assign!(key: "a", value: Value::Int(2))],
                on_exit: [choose!(when: [
                    (a_eq_x(1),
                        action_assign!(key: "b", value: Value::from_str("not matched"))),
                    (a_eq_x(2),
                        action_assign!(key: "b", value: Value::from_str("matched")))])],
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::from_str("matched"));
}

#[test]
fn otherwise() {
    let sc = states!{ S {
        substates: [
            state!{ S0 {
                transitions: [goto!(target: SF)],
                on_exit: [choose!(when: [
                    (a_eq_x(1),
                        action_assign!(key: "b", value: Value::from_str("not matched"))),
                    (a_eq_x(2),
                        action_assign!(key: "b", value: Value::from_str("not matched")))],
                    otherwise: action_assign!(key: "b", value: Value::from_str("matched")))],
            }},
            final_state!{ SF {
                result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::from_str("matched"));
}
